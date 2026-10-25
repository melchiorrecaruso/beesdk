{
  Copyright (c) 2026 Melchiorre Caruso

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

  EXE-X86 executable preprocessor: recognition, filter and inverse.

  Supported images: PE and ELF for x86, x86-64 and ARM64, recognized from
  their content. Managed (.NET) PE images, ELF big-endian images and code
  ranges with near-random content (packed or encrypted) are left alone.

  x86/x64: instructions are decoded (legacy prefixes, REX, 0F/0F38/0F3A,
  VEX, EVEX, XOP). The rel32 operands of CALL, JMP and Jcc and the
  RIP-relative displacements become absolute RVAs, written big-endian into
  three streams of their own; every other byte stays in file order.

  ARM64: aligned words; BL targets and ADRP pages become absolute (ADRP
  with the +/-512 MiB rule of the xz ARM64 filter) and every word is
  written big-endian.

  Code ranges are stored explicitly, so the decoder never parses headers
  from filtered bytes and range selection can improve without a format
  change. Each filtered image is decoded and compared with the original
  before it is accepted.

  Format version 1 (little-endian fields):
    'X' 1 Arch(32/64/$AA) Flags StreamCount
    OriginalSize RangeCount RangeCount * (Offset Length Parsed Rva)
    StreamCount * StreamLength streams

  The opcode tables, conversions and stream layout below belong to
  version 1. Changing any of them changes the format: add a new version
  and keep decoding version 1 unchanged.

  Modifyed:

  v0.7.9 build 0602 - 2026.10.25 by Melchiorre Caruso.
}

unit BeeExeFilter;

{$MODE OBJFPC}{$H+}
{$R-}{$Q-}{$S-}{$I-}

interface

uses
  Classes, SysUtils;

const
  BeeExePreprocessorName = 'EXE-X86';

  { Files outside these sizes are never filtered. }
  BeeExeMinSize = 1024;
  BeeExeMaxSize = 512 * 1024 * 1024;

  { Bytes inspected before a file is loaded into memory. }
  BeeExeProbeSize = 4096;

type
  TBeeExeLoad = (
    elStream,   // not an executable: Buffer holds the first bytes, the rest
                // is still in the source stream
    elRaw,      // executable left unfiltered: Buffer holds the whole file
    elFiltered  // Buffer holds the filtered image
  );

{ Quick check on the first bytes of a file. }
function BeeExeProbe(const Prefix: TBytes; FileSize: Int64): Boolean;
function BeeExeProbeFile(const FileName: string): Boolean;

{ Filters a whole file in memory. Returns False when the content is not a
  supported executable or the image would not be reversible. }
function BeeExeEncode(const Data: TBytes; out Filtered: TBytes): Boolean;
function BeeExeDecode(const Filtered: TBytes; out Data: TBytes): Boolean;

{ Largest filtered image a file of Size bytes can produce. }
function BeeExeFilteredSizeLimit(Size: Cardinal): Cardinal;

{ Reads Size bytes from Source, updating Crc, and filters them when they
  hold an executable. Source is read forward only, so encrypted streams
  work as well. }
function BeeExeLoad(Source: TStream; Size: Integer; var Crc: Cardinal;
  out Buffer: TBytes): TBeeExeLoad;

implementation

uses
  BeeCrc;

const
  FormatMarker = Ord('X');
  FormatVersion = 1;
  ArchX86 = 32;
  ArchX64 = 64;
  ArchArm64 = $AA;

  { Version 1 layouts }
  FlagsX86 = 6;     // targets in their own streams, big-endian
  FlagsArm64 = 4;   // in place, big-endian
  StreamsX86 = 4;
  StreamsArm64 = 1;

  HeaderSize = 13;
  RangeRecordSize = 16;
  MaxRanges = 4096;
  MaxPrefixes = 14;
  MaxOperands = 4;

  { Ranges below this size, or above this order-0 entropy (bits per byte
    scaled by 1000), are not worth filtering. }
  MinCodeBytes = 4096;
  MaxRangeEntropy = 7500;

type
  EBeeExeFormat = class(Exception);

  TArch = (arX86, arX64, arArm64);

  { Stream of a field. Only call, jump and RIP targets leave the main
    stream; the other kinds document the instruction walker. }
  TFieldKind = (fkOp, fkModRM, fkSIB, fkDisp8, fkDisp, fkRip, fkImm8, fkImm,
    fkCall, fkJump, fkRel8);

  TCodeRange = record
    Offset, Length, Parsed, Rva: Cardinal;
  end;
  TCodeRanges = array of TCodeRange;

  TImmKind = (ikNone, ikIb, ikIw, ikIz, ikIv, ikIwIb, ikJb, ikJzCall,
    ikJzJump, ikMoffs, ikAp, ikGroup3);

  TOpAttr = record
    ModRM: Boolean;
    Imm: TImmKind;
  end;
  TOpTable = array[Byte] of TOpAttr;

  TOperand = record
    Kind: TFieldKind;
    Size: Cardinal;
    Offset: Cardinal;
  end;

  TByteStream = record
    Data: TBytes;
    Count: Cardinal;
    ReadPos: Cardinal;
  end;
  TByteStreams = array of TByteStream;

  { Instruction walker shared by probe, encoder and decoder. Structural
    bytes (prefixes, opcodes, ModRM, SIB, VEX payload) drive the parse and
    are never changed by the filter, so both sides find the same
    instruction boundaries. }
  TX86Walker = class
  protected
    Is64: Boolean;
    Pos: Cardinal;
    OperandCount: Integer;
    Operands: array[0..MaxOperands - 1] of TOperand;
    function Fetch(Kind: TFieldKind): Byte; virtual; abstract;
    procedure Operand(Kind: TFieldKind; Size: Cardinal);
    procedure DoModRM(ModRM: Byte; Addr16: Boolean);
    procedure DoImmediate(Imm: TImmKind; ModRM: Byte; OpSize16, AddrOverride,
      RexW: Boolean);
  public
    procedure ParseInstruction;
  end;

  { Length probe: checks that an instruction ends before Limit. }
  TX86Probe = class(TX86Walker)
  protected
    Src: PByte;
    Limit: Cardinal;
    function Fetch(Kind: TFieldKind): Byte; override;
  end;

  TX86Encoder = class(TX86Walker)
  protected
    Src: PByte;
    Streams: ^TByteStreams;
    function Fetch(Kind: TFieldKind): Byte; override;
  public
    procedure Finish(NextRva: Cardinal);
  end;

  TX86Decoder = class(TX86Walker)
  protected
    Dst: PByte;
    Limit: Cardinal;
    Streams: ^TByteStreams;
    function Fetch(Kind: TFieldKind): Byte; override;
  public
    procedure Finish(NextRva: Cardinal);
  end;

var
  { Read-only after initialization. }
  Map0, Map1, Map2, Map3: TOpTable;

{ Streams of version 1 }

function StreamOf(Kind: TFieldKind): Integer; inline;
begin
  case Kind of
    fkCall: Result := 1;
    fkJump: Result := 2;
    fkRip: Result := 3;
  else
    Result := 0;
  end;
end;

procedure StreamPush(var S: TByteStream; Value: Byte); inline;
begin
  if S.Count = Cardinal(Length(S.Data)) then
    if S.Count < 4096 then SetLength(S.Data, 4096)
    else SetLength(S.Data, S.Count * 2);
  S.Data[S.Count] := Value;
  Inc(S.Count);
end;

function StreamPop(var S: TByteStream): Byte; inline;
begin
  if S.ReadPos >= S.Count then
    raise EBeeExeFormat.Create('EXE-X86 stream underflow');
  Result := S.Data[S.ReadPos];
  Inc(S.ReadPos);
end;

{ x86 instruction walker }

procedure TX86Walker.Operand(Kind: TFieldKind; Size: Cardinal);
begin
  if OperandCount = MaxOperands then
    raise EBeeExeFormat.Create('EXE-X86 operand overflow');
  Operands[OperandCount].Kind := Kind;
  Operands[OperandCount].Size := Size;
  Operands[OperandCount].Offset := Pos;
  Inc(OperandCount);
  Inc(Pos, Size);
end;

procedure TX86Walker.DoModRM(ModRM: Byte; Addr16: Boolean);
var
  Mode, RM, SIB: Byte;
begin
  Mode := ModRM shr 6;
  RM := ModRM and 7;
  if Mode = 3 then Exit;
  if Addr16 then
  begin
    if (Mode = 0) and (RM = 6) then Operand(fkDisp, 2)
    else if Mode = 1 then Operand(fkDisp8, 1)
    else if Mode = 2 then Operand(fkDisp, 2);
    Exit;
  end;
  if RM = 4 then
  begin
    SIB := Fetch(fkSIB);
    if (Mode = 0) and ((SIB and 7) = 5) then
    begin
      Operand(fkDisp, 4);
      Exit;
    end;
  end
  else if (Mode = 0) and (RM = 5) then
  begin
    if Is64 then Operand(fkRip, 4) else Operand(fkDisp, 4);
    Exit;
  end;
  if Mode = 1 then Operand(fkDisp8, 1)
  else if Mode = 2 then Operand(fkDisp, 4);
end;

procedure TX86Walker.DoImmediate(Imm: TImmKind; ModRM: Byte; OpSize16,
  AddrOverride, RexW: Boolean);

  function ZSize: Cardinal; inline;
  begin
    if OpSize16 then Result := 2 else Result := 4;
  end;

begin
  case Imm of
    ikIb: Operand(fkImm8, 1);
    ikIw: Operand(fkImm, 2);
    ikIz: Operand(fkImm, ZSize);
    ikIv:
      if RexW then Operand(fkImm, 8) else Operand(fkImm, ZSize);
    ikIwIb:
      begin
        Operand(fkImm, 2);
        Operand(fkImm8, 1);
      end;
    ikJb: Operand(fkRel8, 1);
    ikJzCall, ikJzJump:
      if (not Is64) and OpSize16 then Operand(fkImm, 2)
      else if Imm = ikJzCall then Operand(fkCall, 4)
      else Operand(fkJump, 4);
    ikMoffs:
      if Is64 then
      begin
        if AddrOverride then Operand(fkImm, 4) else Operand(fkImm, 8);
      end
      else if AddrOverride then Operand(fkImm, 2)
      else Operand(fkImm, 4);
    ikAp:
      if not Is64 then
        if OpSize16 then Operand(fkImm, 4) else Operand(fkImm, 6);
    ikGroup3:
      if ((ModRM shr 3) and 7) < 2 then Operand(fkImm, ZSize);
  else
    ;
  end;
end;

procedure TX86Walker.ParseInstruction;
var
  B, Op, ModRM, P0, MapId: Byte;
  OpSize16, AddrOverride, RexW, HaveModRM: Boolean;
  Prefixes: Integer;
  Attr: TOpAttr;
  Table: ^TOpTable;
begin
  OperandCount := 0;
  OpSize16 := False;
  AddrOverride := False;
  RexW := False;
  Prefixes := 0;
  repeat
    B := Fetch(fkOp);
    if Prefixes >= MaxPrefixes then Break;
    case B of
      $26, $2E, $36, $3E, $64, $65, $F0, $F2, $F3: RexW := False;
      $66:
        begin
          OpSize16 := True;
          RexW := False;
        end;
      $67:
        begin
          AddrOverride := True;
          RexW := False;
        end;
      $40..$4F:
        if Is64 then RexW := (B and 8) <> 0 else Break;
    else
      Break;
    end;
    Inc(Prefixes);
  until False;

  ModRM := 0;
  HaveModRM := False;
  Table := @Map0;
  Op := B;

  if B = $0F then
  begin
    Op := Fetch(fkOp);
    if Op = $38 then
    begin
      Op := Fetch(fkOp);
      Table := @Map2;
    end
    else if Op = $3A then
    begin
      Op := Fetch(fkOp);
      Table := @Map3;
    end
    else
      Table := @Map1;
  end
  else if B in [$C4, $C5, $62, $8F] then
  begin
    { VEX, EVEX and XOP share opcodes with LES, LDS, BOUND and POP r/m.
      Where the meaning depends on the next byte, that byte is read as a
      ModRM so that its stream does not depend on its value. }
    if Is64 and (B <> $8F) then
      P0 := Fetch(fkOp)
    else
    begin
      P0 := Fetch(fkModRM);
      HaveModRM := True;
    end;
    if ((B = $8F) and ((P0 and $1F) >= 8)) or
      ((B <> $8F) and (Is64 or (P0 shr 6 = 3))) then
    begin
      case B of
        $C5:
          MapId := 1;
        $C4:
          begin
            MapId := P0 and $1F;
            Fetch(fkOp);
          end;
        $62:
          begin
            MapId := P0 and $07;
            Fetch(fkOp);
            Fetch(fkOp);
          end;
      else { XOP }
        MapId := P0 and $1F;
        Fetch(fkOp);
      end;
      Op := Fetch(fkOp);
      case MapId of
        1: Table := @Map1;
        3: Table := @Map3;
      else
        Table := @Map2;
      end;
      Attr := Table^[Op];
      if B = $8F then
      begin
        ModRM := Fetch(fkModRM);
        DoModRM(ModRM, False);
        case MapId of
          8: Operand(fkImm8, 1);
          10: Operand(fkImm, 4);
        end;
        Exit;
      end;
      if Attr.ModRM then
      begin
        ModRM := Fetch(fkModRM);
        DoModRM(ModRM, False);
      end;
      if Attr.Imm = ikIb then Operand(fkImm8, 1);
      Exit;
    end;
    ModRM := P0;
  end;

  Attr := Table^[Op];
  if Attr.ModRM then
  begin
    if not HaveModRM then ModRM := Fetch(fkModRM);
    DoModRM(ModRM, (not Is64) and AddrOverride);
  end;
  if (Attr.Imm = ikGroup3) and (Op = $F6) then
  begin
    if ((ModRM shr 3) and 7) < 2 then Operand(fkImm8, 1);
  end
  else
    DoImmediate(Attr.Imm, ModRM, OpSize16, AddrOverride, RexW);
end;

function TX86Probe.Fetch(Kind: TFieldKind): Byte;
begin
  if Pos < Limit then Result := Src[Pos] else Result := 0;
  Inc(Pos);
end;

function TX86Encoder.Fetch(Kind: TFieldKind): Byte;
begin
  Result := Src[Pos];
  Inc(Pos);
  StreamPush(Streams^[StreamOf(Kind)], Result);
end;

procedure TX86Encoder.Finish(NextRva: Cardinal);
var
  I, S: Integer;
  J, V: Cardinal;
begin
  for I := 0 to OperandCount - 1 do
    with Operands[I] do
    begin
      S := StreamOf(Kind);
      if (Kind in [fkCall, fkJump, fkRip]) and (Size = 4) then
      begin
        V := Src[Offset] or (Cardinal(Src[Offset + 1]) shl 8) or
          (Cardinal(Src[Offset + 2]) shl 16) or
          (Cardinal(Src[Offset + 3]) shl 24);
        V := V + NextRva;
        for J := 3 downto 0 do StreamPush(Streams^[S], Byte(V shr (J * 8)));
      end
      else
        for J := 0 to Size - 1 do StreamPush(Streams^[S], Src[Offset + J]);
    end;
end;

function TX86Decoder.Fetch(Kind: TFieldKind): Byte;
begin
  if Pos >= Limit then
    raise EBeeExeFormat.Create('EXE-X86 instruction overruns its range');
  Result := StreamPop(Streams^[StreamOf(Kind)]);
  Dst[Pos] := Result;
  Inc(Pos);
end;

procedure TX86Decoder.Finish(NextRva: Cardinal);
var
  I, S: Integer;
  J, V: Cardinal;
begin
  if Pos > Limit then
    raise EBeeExeFormat.Create('EXE-X86 instruction overruns its range');
  for I := 0 to OperandCount - 1 do
    with Operands[I] do
    begin
      S := StreamOf(Kind);
      if (Kind in [fkCall, fkJump, fkRip]) and (Size = 4) then
      begin
        V := 0;
        for J := 0 to 3 do V := (V shl 8) or StreamPop(Streams^[S]);
        V := V - NextRva;
        for J := 0 to 3 do Dst[Offset + J] := Byte(V shr (J * 8));
      end
      else
        for J := 0 to Size - 1 do Dst[Offset + J] := StreamPop(Streams^[S]);
    end;
end;

{ Opcode tables (format version 1) }

procedure SetAttr(var T: TOpTable; First, Last: Byte; HasModRM: Boolean;
  Imm: TImmKind);
var
  I: Integer;
begin
  for I := First to Last do
  begin
    T[I].ModRM := HasModRM;
    T[I].Imm := Imm;
  end;
end;

procedure InitTables;
var
  I: Integer;
begin
  SetAttr(Map0, 0, 255, False, ikNone);
  for I := 0 to $3F do
    case I and 7 of
      0..3: SetAttr(Map0, I, I, True, ikNone);
      4: SetAttr(Map0, I, I, False, ikIb);
      5: SetAttr(Map0, I, I, False, ikIz);
    end;
  SetAttr(Map0, $62, $63, True, ikNone);
  SetAttr(Map0, $68, $68, False, ikIz);
  SetAttr(Map0, $69, $69, True, ikIz);
  SetAttr(Map0, $6A, $6A, False, ikIb);
  SetAttr(Map0, $6B, $6B, True, ikIb);
  SetAttr(Map0, $70, $7F, False, ikJb);
  SetAttr(Map0, $80, $80, True, ikIb);
  SetAttr(Map0, $81, $81, True, ikIz);
  SetAttr(Map0, $82, $83, True, ikIb);
  SetAttr(Map0, $84, $8F, True, ikNone);
  SetAttr(Map0, $9A, $9A, False, ikAp);
  SetAttr(Map0, $A0, $A3, False, ikMoffs);
  SetAttr(Map0, $A8, $A8, False, ikIb);
  SetAttr(Map0, $A9, $A9, False, ikIz);
  SetAttr(Map0, $B0, $B7, False, ikIb);
  SetAttr(Map0, $B8, $BF, False, ikIv);
  SetAttr(Map0, $C0, $C1, True, ikIb);
  SetAttr(Map0, $C2, $C2, False, ikIw);
  SetAttr(Map0, $C4, $C5, True, ikNone);
  SetAttr(Map0, $C6, $C6, True, ikIb);
  SetAttr(Map0, $C7, $C7, True, ikIz);
  SetAttr(Map0, $C8, $C8, False, ikIwIb);
  SetAttr(Map0, $CA, $CA, False, ikIw);
  SetAttr(Map0, $CD, $CD, False, ikIb);
  SetAttr(Map0, $D0, $D3, True, ikNone);
  SetAttr(Map0, $D4, $D5, False, ikIb);
  SetAttr(Map0, $D8, $DF, True, ikNone);
  SetAttr(Map0, $E0, $E3, False, ikJb);
  SetAttr(Map0, $E4, $E7, False, ikIb);
  SetAttr(Map0, $E8, $E8, False, ikJzCall);
  SetAttr(Map0, $E9, $E9, False, ikJzJump);
  SetAttr(Map0, $EA, $EA, False, ikAp);
  SetAttr(Map0, $EB, $EB, False, ikJb);
  SetAttr(Map0, $F6, $F7, True, ikGroup3);
  SetAttr(Map0, $FE, $FF, True, ikNone);

  SetAttr(Map1, 0, 255, True, ikNone);
  for I in [$04..$0C, $0E, $30..$37, $39, $3B..$3F, $77, $A0..$A2,
    $A8..$AA, $C8..$CF] do
    SetAttr(Map1, I, I, False, ikNone);
  SetAttr(Map1, $0F, $0F, True, ikIb); // 3DNow! suffix
  SetAttr(Map1, $0D, $0D, True, ikNone);
  SetAttr(Map1, $70, $73, True, ikIb);
  SetAttr(Map1, $80, $8F, False, ikJzJump);
  SetAttr(Map1, $A4, $A4, True, ikIb);
  SetAttr(Map1, $AC, $AC, True, ikIb);
  SetAttr(Map1, $BA, $BA, True, ikIb);
  SetAttr(Map1, $C2, $C2, True, ikIb);
  SetAttr(Map1, $C4, $C6, True, ikIb);

  SetAttr(Map2, 0, 255, True, ikNone);
  SetAttr(Map3, 0, 255, True, ikIb);
end;

{ ARM64 (format version 1) }

function ArmKind(HighByte: Byte): TFieldKind; inline;
begin
  if HighByte shr 2 = $25 then Result := fkCall          // BL
  else if HighByte and $9F = $90 then Result := fkRip    // ADRP
  else Result := fkOp;
end;

{ The kind of a word depends on its high byte only, which the conversion
  keeps, so the decoder classifies converted words like the encoder. }
procedure ArmConvert(var W: Cardinal; Pc: Cardinal; Encoding: Boolean);
var
  Src, Dest, Delta: Cardinal;
begin
  case ArmKind(W shr 24) of
    fkCall:
      begin
        Delta := Pc shr 2;
        if not Encoding then Delta := 0 - Delta;
        W := $94000000 or ((W + Delta) and $03FFFFFF);
      end;
    fkRip:
      begin
        Src := ((W shr 29) and 3) or ((W shr 3) and $001FFFFC);
        if (Src + $00020000) and $001C0000 <> 0 then Exit;
        Delta := Pc shr 12;
        if not Encoding then Delta := 0 - Delta;
        Dest := Src + Delta;
        W := (W and $9000001F) or ((Dest and 3) shl 29) or
          ((Dest and $0003FFFC) shl 3) or
          ((0 - (Dest and $00020000)) and $00E00000);
      end;
  else
    ;
  end;
end;

procedure ArmEncodeRange(var Streams: TByteStreams; const D: TBytes;
  var Range: TCodeRange);
var
  Pos, Last, W: Cardinal;
begin
  Range.Parsed := Range.Length and not Cardinal(3);
  Pos := Range.Offset;
  Last := Range.Offset + Range.Parsed;
  while Pos < Last do
  begin
    W := D[Pos] or (Cardinal(D[Pos + 1]) shl 8) or
      (Cardinal(D[Pos + 2]) shl 16) or (Cardinal(D[Pos + 3]) shl 24);
    ArmConvert(W, Range.Rva + (Pos - Range.Offset), True);
    StreamPush(Streams[0], Byte(W shr 24));
    StreamPush(Streams[0], Byte(W shr 16));
    StreamPush(Streams[0], Byte(W shr 8));
    StreamPush(Streams[0], Byte(W));
    Inc(Pos, 4);
  end;
end;

procedure ArmDecodeRange(var Streams: TByteStreams; var D: TBytes;
  const Range: TCodeRange);
var
  Pos, Last, W: Cardinal;
begin
  if Range.Parsed and 3 <> 0 then
    raise EBeeExeFormat.Create('EXE-X86 misaligned ARM64 range');
  Pos := Range.Offset;
  Last := Range.Offset + Range.Parsed;
  while Pos < Last do
  begin
    W := Cardinal(StreamPop(Streams[0])) shl 24;
    W := W or (Cardinal(StreamPop(Streams[0])) shl 16);
    W := W or (Cardinal(StreamPop(Streams[0])) shl 8);
    W := W or StreamPop(Streams[0]);
    ArmConvert(W, Range.Rva + (Pos - Range.Offset), False);
    D[Pos] := Byte(W);
    D[Pos + 1] := Byte(W shr 8);
    D[Pos + 2] := Byte(W shr 16);
    D[Pos + 3] := Byte(W shr 24);
    Inc(Pos, 4);
  end;
end;

{ Recognition }

function ReadU16(const D: TBytes; Ofs: QWord; out V: Cardinal): Boolean;
begin
  Result := Ofs + 2 <= QWord(Length(D));
  if Result then V := D[Ofs] or (Cardinal(D[Ofs + 1]) shl 8);
end;

function ReadU32(const D: TBytes; Ofs: QWord; out V: Cardinal): Boolean;
begin
  Result := Ofs + 4 <= QWord(Length(D));
  if Result then
    V := D[Ofs] or (Cardinal(D[Ofs + 1]) shl 8) or
      (Cardinal(D[Ofs + 2]) shl 16) or (Cardinal(D[Ofs + 3]) shl 24);
end;

function ReadU64(const D: TBytes; Ofs: QWord; out V: QWord): Boolean;
var
  Lo, Hi: Cardinal;
begin
  Result := ReadU32(D, Ofs, Lo) and ReadU32(D, Ofs + 4, Hi);
  if Result then V := QWord(Lo) or (QWord(Hi) shl 32);
end;

function PeArch(Machine: Cardinal; out Arch: TArch): Boolean;
begin
  Result := True;
  case Machine of
    $014C: Arch := arX86;
    $8664: Arch := arX64;
    $AA64: Arch := arArm64;
  else
    Result := False;
  end;
end;

function ElfArch(ElfClass, Machine: Cardinal; out Arch: TArch): Boolean;
begin
  Result := True;
  if (ElfClass = 1) and (Machine = 3) then Arch := arX86
  else if (ElfClass = 2) and (Machine = 62) then Arch := arX64
  else if (ElfClass = 2) and (Machine = 183) then Arch := arArm64
  else Result := False;
end;

function BeeExeProbe(const Prefix: TBytes; FileSize: Int64): Boolean;
var
  Pe, Signature, Machine: Cardinal;
  Arch: TArch;
begin
  Result := False;
  if (FileSize < BeeExeMinSize) or (FileSize > BeeExeMaxSize) or
    (Length(Prefix) < 64) then Exit;
  if (Prefix[0] = Ord('M')) and (Prefix[1] = Ord('Z')) then
  begin
    if not ReadU32(Prefix, $3C, Pe) or (QWord(Pe) + 24 > QWord(FileSize)) then
      Exit;
    { A PE header beyond the probe is accepted; full validation follows. }
    if not ReadU32(Prefix, Pe, Signature) then Exit(True);
    Result := (Signature = $00004550) and ReadU16(Prefix, QWord(Pe) + 4,
      Machine) and PeArch(Machine, Arch);
  end
  else if (Prefix[0] = $7F) and (Prefix[1] = Ord('E')) and
    (Prefix[2] = Ord('L')) and (Prefix[3] = Ord('F')) and (Prefix[5] = 1) then
    Result := ReadU16(Prefix, 18, Machine) and ElfArch(Prefix[4], Machine, Arch);
end;

function BeeExeProbeFile(const FileName: string): Boolean;
var
  Stream: TFileStream;
  Prefix: TBytes;
  Size: Int64;
begin
  Result := False;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Size := Stream.Size;
      if (Size < BeeExeMinSize) or (Size > BeeExeMaxSize) then Exit;
      SetLength(Prefix, BeeExeProbeSize);
      SetLength(Prefix, Stream.Read(Prefix[0], BeeExeProbeSize));
      Result := BeeExeProbe(Prefix, Size);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

procedure AddRange(var Ranges: TCodeRanges; const D: TBytes;
  Offset, Size: QWord; Rva: Cardinal);
var
  N: Integer;
begin
  if (Size = 0) or (Offset >= QWord(Length(D))) or
    (Length(Ranges) >= MaxRanges) then Exit;
  if Size > QWord(Length(D)) - Offset then Size := QWord(Length(D)) - Offset;
  N := Length(Ranges);
  SetLength(Ranges, N + 1);
  Ranges[N].Offset := Offset;
  Ranges[N].Length := Size;
  Ranges[N].Rva := Rva;
  Ranges[N].Parsed := 0;
end;

function FindPeRanges(const D: TBytes; out Arch: TArch;
  out Ranges: TCodeRanges): Boolean;
var
  Pe, Machine, Sections, OptSize, Magic, Count, I: Cardinal;
  RawSize, RawOffset, Va, Flags, ClrRva: Cardinal;
  DirBase, Table, Ofs: QWord;
begin
  Result := False;
  Ranges := nil;
  Arch := arX86;
  if (Length(D) < 64) or (D[0] <> Ord('M')) or (D[1] <> Ord('Z')) then Exit;
  if not ReadU32(D, $3C, Pe) or not ReadU32(D, Pe, Magic) or
    (Magic <> $00004550) then Exit;
  if not ReadU16(D, QWord(Pe) + 4, Machine) or
    not ReadU16(D, QWord(Pe) + 6, Sections) or
    not ReadU16(D, QWord(Pe) + 20, OptSize) or
    not ReadU16(D, QWord(Pe) + 24, Magic) or
    not PeArch(Machine, Arch) then Exit;
  { Managed (.NET) images contain CIL, not machine code. }
  if Magic = $20B then DirBase := QWord(Pe) + 24 + 108
  else DirBase := QWord(Pe) + 24 + 92;
  if ReadU32(D, DirBase, Count) and (Count > 14) and
    ReadU32(D, DirBase + 4 + 14 * 8, ClrRva) and (ClrRva <> 0) then Exit;
  if (Sections = 0) or (Sections > 96) then Exit;
  Table := QWord(Pe) + 24 + OptSize;
  for I := 1 to Sections do
  begin
    Ofs := Table + QWord(I - 1) * 40;
    if not ReadU32(D, Ofs + 12, Va) or
      not ReadU32(D, Ofs + 16, RawSize) or
      not ReadU32(D, Ofs + 20, RawOffset) or
      not ReadU32(D, Ofs + 36, Flags) then Exit;
    if Flags and $20000020 <> 0 then // CNT_CODE or MEM_EXECUTE
      AddRange(Ranges, D, RawOffset, RawSize, Va);
  end;
  Result := Length(Ranges) > 0;
end;

function FindElfRanges(const D: TBytes; out Arch: TArch;
  out Ranges: TCodeRanges): Boolean;
var
  Machine, EntSize, Count, I, Flags32, SecType, V32: Cardinal;
  ShOff, Ofs, Flags, Addr, SecOffset, SecSize: QWord;
  Is64: Boolean;
begin
  Result := False;
  Ranges := nil;
  Arch := arX86;
  if (Length(D) < 64) or (D[0] <> $7F) or (D[1] <> Ord('E')) or
    (D[2] <> Ord('L')) or (D[3] <> Ord('F')) or (D[5] <> 1) then Exit;
  if not ReadU16(D, 18, Machine) or not ElfArch(D[4], Machine, Arch) then Exit;
  Is64 := D[4] = 2;
  if Is64 then
  begin
    if not ReadU64(D, 40, ShOff) or not ReadU16(D, 58, EntSize) or
      not ReadU16(D, 60, Count) then Exit;
  end
  else
  begin
    if not ReadU32(D, 32, V32) or not ReadU16(D, 46, EntSize) or
      not ReadU16(D, 48, Count) then Exit;
    ShOff := V32;
  end;
  if (Count = 0) or (ShOff = 0) then Exit;
  for I := 1 to Count do
  begin
    Ofs := ShOff + QWord(I - 1) * EntSize;
    if not ReadU32(D, Ofs + 4, SecType) then Exit;
    if Is64 then
    begin
      if not ReadU64(D, Ofs + 8, Flags) or not ReadU64(D, Ofs + 16, Addr) or
        not ReadU64(D, Ofs + 24, SecOffset) or
        not ReadU64(D, Ofs + 32, SecSize) then Exit;
    end
    else
    begin
      if not ReadU32(D, Ofs + 8, Flags32) then Exit;
      Flags := Flags32;
      if not ReadU32(D, Ofs + 12, V32) then Exit;
      Addr := V32;
      if not ReadU32(D, Ofs + 16, V32) then Exit;
      SecOffset := V32;
      if not ReadU32(D, Ofs + 20, V32) then Exit;
      SecSize := V32;
    end;
    if (SecType <> 8) and (Flags and 4 <> 0) then // not NOBITS, EXECINSTR
      AddRange(Ranges, D, SecOffset, SecSize, Cardinal(Addr));
  end;
  Result := Length(Ranges) > 0;
end;

{ Sorts ranges by offset and clips overlaps. }
procedure NormalizeRanges(var Ranges: TCodeRanges);
var
  I, J, N: Integer;
  T: TCodeRange;
  Last: QWord;
begin
  for I := 1 to High(Ranges) do
  begin
    T := Ranges[I];
    J := I - 1;
    while (J >= 0) and (Ranges[J].Offset > T.Offset) do
    begin
      Ranges[J + 1] := Ranges[J];
      Dec(J);
    end;
    Ranges[J + 1] := T;
  end;
  N := 0;
  Last := 0;
  for I := 0 to High(Ranges) do
  begin
    T := Ranges[I];
    if T.Offset < Last then
    begin
      if QWord(T.Offset) + T.Length <= Last then Continue;
      Inc(T.Rva, Last - T.Offset);
      Dec(T.Length, Last - T.Offset);
      T.Offset := Last;
    end;
    Ranges[N] := T;
    Inc(N);
    Last := QWord(T.Offset) + T.Length;
  end;
  SetLength(Ranges, N);
end;

{ Order-0 entropy in millibits per byte. }
function RangeEntropy(const D: TBytes; const Range: TCodeRange): Cardinal;
var
  Counts: array[Byte] of Cardinal;
  I: Cardinal;
  P, Sum: Double;
begin
  FillChar(Counts, SizeOf(Counts), 0);
  for I := Range.Offset to Range.Offset + Range.Length - 1 do
    Inc(Counts[D[I]]);
  Sum := 0;
  for I := 0 to 255 do
    if Counts[I] > 0 then
    begin
      P := Counts[I] / Range.Length;
      Sum := Sum - P * Ln(P);
    end;
  Result := Round(Sum / Ln(2) * 1000);
end;

{ Drops ranges not worth filtering and aligns ARM64 ranges on their
  virtual address. }
procedure SelectRanges(const D: TBytes; Arch: TArch; var Ranges: TCodeRanges);
var
  I, N: Integer;
  Lead: Cardinal;
begin
  N := 0;
  for I := 0 to High(Ranges) do
  begin
    if Arch = arArm64 then
    begin
      Lead := (4 - (Ranges[I].Rva and 3)) and 3;
      if Ranges[I].Length < Lead + 4 then Continue;
      Inc(Ranges[I].Offset, Lead);
      Inc(Ranges[I].Rva, Lead);
      Dec(Ranges[I].Length, Lead);
    end;
    if RangeEntropy(D, Ranges[I]) > MaxRangeEntropy then Continue;
    Ranges[N] := Ranges[I];
    Inc(N);
  end;
  SetLength(Ranges, N);
end;

{ Container }

procedure PutU32(var B: TBytes; var P: Cardinal; V: Cardinal);
begin
  B[P] := Byte(V);
  B[P + 1] := Byte(V shr 8);
  B[P + 2] := Byte(V shr 16);
  B[P + 3] := Byte(V shr 24);
  Inc(P, 4);
end;

function GetU32(const B: TBytes; var P: QWord): Cardinal;
begin
  if P + 4 > QWord(Length(B)) then
    raise EBeeExeFormat.Create('EXE-X86 header truncated');
  Result := B[P] or (Cardinal(B[P + 1]) shl 8) or
    (Cardinal(B[P + 2]) shl 16) or (Cardinal(B[P + 3]) shl 24);
  Inc(P, 4);
end;

procedure PushRaw(var S: TByteStream; const D: TBytes; First, Last: Cardinal);
var
  I: Cardinal;
begin
  if Last > First then
    for I := First to Last - 1 do StreamPush(S, D[I]);
end;

procedure PopRaw(var S: TByteStream; var D: TBytes; First, Last: Cardinal);
var
  I: Cardinal;
begin
  if Last > First then
    for I := First to Last - 1 do D[I] := StreamPop(S);
end;

function BeeExeFilteredSizeLimit(Size: Cardinal): Cardinal;
begin
  Result := Size + HeaderSize + 8 + MaxRanges * RangeRecordSize +
    StreamsX86 * 4;
end;

{ Filters Data without verification; False if nothing to filter. }
function EncodeImage(const Data: TBytes; out Filtered: TBytes): Boolean;
var
  Ranges: TCodeRanges;
  Arch: TArch;
  Streams: TByteStreams;
  Enc: TX86Encoder;
  Probe: TX86Probe;
  R, I: Integer;
  Cursor, RangeEnd, P, Total: Cardinal;
  CodeBytes: QWord;
begin
  Result := False;
  Filtered := nil;
  if (Length(Data) < BeeExeMinSize) or (Length(Data) > BeeExeMaxSize) then Exit;
  if not FindPeRanges(Data, Arch, Ranges) and
    not FindElfRanges(Data, Arch, Ranges) then Exit;
  NormalizeRanges(Ranges);
  SelectRanges(Data, Arch, Ranges);
  CodeBytes := 0;
  for R := 0 to High(Ranges) do Inc(CodeBytes, Ranges[R].Length);
  if CodeBytes < MinCodeBytes then Exit;

  if Arch = arArm64 then SetLength(Streams, StreamsArm64)
  else SetLength(Streams, StreamsX86);
  Enc := TX86Encoder.Create;
  Probe := TX86Probe.Create;
  try
    Enc.Is64 := Arch = arX64;
    Enc.Src := @Data[0];
    Enc.Streams := @Streams;
    Probe.Is64 := Enc.Is64;
    Probe.Src := @Data[0];

    Cursor := 0;
    for R := 0 to High(Ranges) do
    begin
      PushRaw(Streams[0], Data, Cursor, Ranges[R].Offset);
      if Arch = arArm64 then
        ArmEncodeRange(Streams, Data, Ranges[R])
      else
      begin
        RangeEnd := Ranges[R].Offset + Ranges[R].Length;
        Enc.Pos := Ranges[R].Offset;
        while Enc.Pos < RangeEnd do
        begin
          Probe.Pos := Enc.Pos;
          Probe.Limit := RangeEnd;
          Probe.ParseInstruction;
          if Probe.Pos > RangeEnd then Break;
          Enc.ParseInstruction;
          Enc.Finish(Ranges[R].Rva + (Enc.Pos - Ranges[R].Offset));
        end;
        Ranges[R].Parsed := Enc.Pos - Ranges[R].Offset;
      end;
      Cursor := Ranges[R].Offset + Ranges[R].Parsed;
    end;
    PushRaw(Streams[0], Data, Cursor, Length(Data));

    Total := HeaderSize + Cardinal(Length(Ranges)) * RangeRecordSize +
      Cardinal(Length(Streams)) * 4;
    for I := 0 to High(Streams) do Inc(Total, Streams[I].Count);
    SetLength(Filtered, Total);
    Filtered[0] := FormatMarker;
    Filtered[1] := FormatVersion;
    case Arch of
      arX86: Filtered[2] := ArchX86;
      arX64: Filtered[2] := ArchX64;
      arArm64: Filtered[2] := ArchArm64;
    end;
    if Arch = arArm64 then Filtered[3] := FlagsArm64
    else Filtered[3] := FlagsX86;
    Filtered[4] := Length(Streams);
    P := 5;
    PutU32(Filtered, P, Length(Data));
    PutU32(Filtered, P, Length(Ranges));
    for R := 0 to High(Ranges) do
    begin
      PutU32(Filtered, P, Ranges[R].Offset);
      PutU32(Filtered, P, Ranges[R].Length);
      PutU32(Filtered, P, Ranges[R].Parsed);
      PutU32(Filtered, P, Ranges[R].Rva);
    end;
    for I := 0 to High(Streams) do PutU32(Filtered, P, Streams[I].Count);
    for I := 0 to High(Streams) do
      if Streams[I].Count > 0 then
      begin
        Move(Streams[I].Data[0], Filtered[P], Streams[I].Count);
        Inc(P, Streams[I].Count);
      end;
    Result := True;
  finally
    Probe.Free;
    Enc.Free;
  end;
end;

function DecodeVersion1(const Filtered: TBytes; out Data: TBytes): Boolean;
var
  Streams: TByteStreams;
  Dec_: TX86Decoder;
  Arch: TArch;
  P, Cursor, ParsedEnd: QWord;
  Size, Count, StreamCount, Len, I: Cardinal;
  Ranges: TCodeRanges;
  R: Integer;
begin
  Result := False;
  case Filtered[2] of
    ArchX86: Arch := arX86;
    ArchX64: Arch := arX64;
    ArchArm64: Arch := arArm64;
  else
    Exit;
  end;
  { Unknown flags or layouts are rejected, never guessed. }
  if Arch = arArm64 then
  begin
    if (Filtered[3] <> FlagsArm64) or (Filtered[4] <> StreamsArm64) then Exit;
  end
  else if (Filtered[3] <> FlagsX86) or (Filtered[4] <> StreamsX86) then Exit;
  StreamCount := Filtered[4];
  SetLength(Streams, StreamCount);

  P := 5;
  Size := GetU32(Filtered, P);
  Count := GetU32(Filtered, P);
  if (Size = 0) or (Size > BeeExeMaxSize) or (Count > MaxRanges) then Exit;
  SetLength(Ranges, Count);
  Cursor := 0;
  for R := 0 to Integer(Count) - 1 do
  begin
    Ranges[R].Offset := GetU32(Filtered, P);
    Ranges[R].Length := GetU32(Filtered, P);
    Ranges[R].Parsed := GetU32(Filtered, P);
    Ranges[R].Rva := GetU32(Filtered, P);
    if (Ranges[R].Offset < Cursor) or (Ranges[R].Parsed > Ranges[R].Length) or
      (QWord(Ranges[R].Offset) + Ranges[R].Length > Size) then Exit;
    Cursor := QWord(Ranges[R].Offset) + Ranges[R].Length;
  end;
  for I := 0 to StreamCount - 1 do Streams[I].Count := GetU32(Filtered, P);
  for I := 0 to StreamCount - 1 do
  begin
    Len := Streams[I].Count;
    if P + Len > QWord(Length(Filtered)) then Exit;
    SetLength(Streams[I].Data, Len);
    if Len > 0 then Move(Filtered[P], Streams[I].Data[0], Len);
    Streams[I].ReadPos := 0;
    Inc(P, Len);
  end;
  if P <> QWord(Length(Filtered)) then Exit;

  SetLength(Data, Size);
  Dec_ := TX86Decoder.Create;
  try
    Dec_.Is64 := Arch = arX64;
    Dec_.Dst := @Data[0];
    Dec_.Streams := @Streams;
    Cursor := 0;
    for R := 0 to High(Ranges) do
    begin
      PopRaw(Streams[0], Data, Cursor, Ranges[R].Offset);
      ParsedEnd := QWord(Ranges[R].Offset) + Ranges[R].Parsed;
      if Arch = arArm64 then
        ArmDecodeRange(Streams, Data, Ranges[R])
      else
      begin
        Dec_.Pos := Ranges[R].Offset;
        Dec_.Limit := ParsedEnd;
        while Dec_.Pos < ParsedEnd do
        begin
          Dec_.ParseInstruction;
          Dec_.Finish(Ranges[R].Rva + (Dec_.Pos - Ranges[R].Offset));
        end;
      end;
      Cursor := ParsedEnd;
    end;
    PopRaw(Streams[0], Data, Cursor, Size);
  finally
    Dec_.Free;
  end;
  for I := 0 to StreamCount - 1 do
    if Streams[I].ReadPos <> Streams[I].Count then Exit;
  Result := True;
end;

function BeeExeDecode(const Filtered: TBytes; out Data: TBytes): Boolean;
begin
  Result := False;
  Data := nil;
  if (Length(Filtered) < HeaderSize) or (Filtered[0] <> FormatMarker) then Exit;
  try
    case Filtered[1] of
      1: Result := DecodeVersion1(Filtered, Data);
    end;
  except
    on EBeeExeFormat do Result := False;
  end;
  if not Result then Data := nil;
end;

function BeeExeEncode(const Data: TBytes; out Filtered: TBytes): Boolean;
var
  Back: TBytes;
begin
  Result := EncodeImage(Data, Filtered) and BeeExeDecode(Filtered, Back) and
    (Length(Back) = Length(Data)) and
    CompareMem(@Back[0], @Data[0], Length(Data));
  if not Result then Filtered := nil;
end;

{ Stream interface }

function ReadUpdating(Source: TStream; var Buffer: TBytes; First,
  Count: Integer; var Crc: Cardinal): Integer;
var
  I, N: Integer;
begin
  Result := 0;
  while Result < Count do
  begin
    N := Source.Read(Buffer[First + Result], Count - Result);
    if N <= 0 then Break;
    Inc(Result, N);
  end;
  { A file that shrinks while it is archived reads as zeros, as the
    unfiltered path would store whatever Read left in its buffer. }
  if Result < Count then
    FillChar(Buffer[First + Result], Count - Result, 0);
  for I := First to First + Count - 1 do UpdCrc32(Crc, Buffer[I]);
end;

function BeeExeLoad(Source: TStream; Size: Integer; var Crc: Cardinal;
  out Buffer: TBytes): TBeeExeLoad;
var
  Head: Integer;
  Filtered: TBytes;
begin
  Result := elStream;
  Buffer := nil;
  if Size <= 0 then Exit;
  if Size < BeeExeProbeSize then Head := Size else Head := BeeExeProbeSize;
  SetLength(Buffer, Head);
  ReadUpdating(Source, Buffer, 0, Head, Crc);
  if not BeeExeProbe(Buffer, Size) then Exit;

  SetLength(Buffer, Size);
  ReadUpdating(Source, Buffer, Head, Size - Head, Crc);
  if BeeExeEncode(Buffer, Filtered) then
  begin
    Buffer := Filtered;
    Result := elFiltered;
  end
  else
    Result := elRaw;
end;

initialization
  InitTables;
end.
