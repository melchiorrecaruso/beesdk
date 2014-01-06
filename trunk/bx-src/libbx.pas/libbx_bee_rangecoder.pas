{
  Copyright (c) 2003 Evgeny Shelwien;
  Copyright (c) 2003-2012 Andrew Filinsky;
  Copyright (c) 2013 Melchiorre Caruso.

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

  TRangeCoder class, 
  
    kind of RangeCoder 
      (based on Eugeny Shelwien's shindler_var1 
        (based on Shindler's rangecoder), MaxFreq = 2^24); 
    It uses MulDiv opcode extension.

  (C) 2003 Evgeny Shelwien;
  (C) 2003-2012 Andrew Filinsky;
  (C) 2013 Melchiorre Caruso.

  Created:

    v0.1.0 build 0001 - 2003.02.01 by Evgeny Shelwien.

  Translated:

    v0.1.1 build 0002 - 2003.03.01 by Andrew Filinsky.

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
    v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;
  
    v1.0.0 build 2176 - 2014.01.06 by Melchiorre Caruso.
}

unit libbx_bee_rangecoder;

{$I bx_compiler.inc}

interface

uses
  libbx_Stream;

type
  { Array of Frequencyes }

  TFreq = array of longword;

type
  { TBeeRangeEnc }

  PBeeRangeEnc = ^TBeeRangeEnc;

  TBeeRangeEnc = packed record
    FStream: PWriteStream;
    FRange:  longword;
    FLow:    longword;
    FCode:   longword;
    FCarry:  longword;
    FCache:  longword;
    FFNum:   longword;
  end;

  function  BeeRangeEnc_Create      (aStream: pointer; aStreamWrite: PStreamWrite): PBeeRangeEnc;
  procedure BeeRangeEnc_Destroy     (Self: PBeeRangeEnc);
  procedure BeeRangeEnc_StartEncode (Self: PBeeRangeEnc);
  procedure BeeRangeEnc_FinishEncode(Self: PBeeRangeEnc);
  function  BeeRangeEnc_Update      (Self: PBeeRangeEnc; const Freq: TFreq; aSymbol: longword): longword;

type
  { TBeeRangeDec }

  PBeeRangeDec = ^TBeeRangeDec;

  TBeeRangeDec = packed record
    FStream: PReadStream;
    FRange:  longword;
    FLow:    longword;
    FCode:   longword;
    FCarry:  longword;
    FCache:  longword;
    FFNum:   longword;
  end;

  function  BeeRangeDec_Create      (aStream: pointer; aStreamRead:  PStreamRead): PBeeRangeDec;
  procedure BeeRangeDec_Destroy     (Self: PBeeRangeDec);
  procedure BeeRangeDec_StartDecode (Self: PBeeRangeDec);
  procedure BeeRangeDec_FinishDecode(Self: PBeeRangeDec);
  function  BeeRangeEnc_Update      (Self: PBeeRangeDec; const Freq: TFreq; aSymbol: longword): longword;

implementation

uses
  libbx_bee_common;

const
  TOP     = 1 shl 24;
  NUM     = 4;
  Thres   = 255 * longword(TOP);
  MaxFreq = TOP - 1;

{ TBeeRangeEnc }

function BeeRangeEnc_Create(aStream: pointer; aStreamWrite: PStreamWrite): PBeeRangeEnc;
begin
  Result := GetMem(SizeOf(TBeeRangeEnc));
  Result^.FStream := WriteStream_Create(aStream, aStreamWrite);
end;

procedure BeeRangeEnc_Destroy(Self: PBeeRangeEnc);
begin
  WriteStream_Destroy(Self^.FStream);
  FreeMem(Self);
end;

procedure BeeRangeEnc_StartEncode(Self: PBeeRangeEnc);
begin
  Self^.FRange := $FFFFFFFF;
  Self^.FLow   := 0;
  Self^.FFNum  := 0;
  Self^.FCarry := 0;
end;

procedure BeeRangeEnc_ShiftLow(Self: PBeeRangeEnc);
begin
  if ((Self^.FLow < THRES) or (Self^.FCarry <> 0)) then
  begin
    WriteStream_Write(Self^.FStream, Self^.FCache + Self^.FCarry);

    while Self^.FFNum <> 0 do
    begin
      WriteStream_Write(Self^.FStream, Self^.FCarry - 1);
      Dec(Self^.FFNum);
    end;
    Self^.FCache := Self^.FLow shr 24;
    Self^.FCarry := 0;
  end else
    Inc(Self^.FFNum);

  Self^.FLow := Self^.FLow shl 8;
end;

procedure BeeRangeEnc_Encode(Self: PBeeRangeEnc; CumFreq: longword; Freq: longword; TotFreq: longword);
var
  Tmp: longword;
begin
  Tmp   := Self^.FLow;
  Low   := Low + MulDiv(Range, CumFreq, TotFreq);
  Carry := Carry + longword(Low < Tmp);
  Range := MulDiv(Range, Freq, TotFreq);
  while Range < TOP do
  begin
    Range := Range shl 8;
    ShiftLow;
  end;
end;


procedure BeeRangeEnc_FinishEncode(Self: PBeeRangeEnc);
var
  I: longword;
begin
  with TRangeCoder(Self^) do
  begin
    for I := 0 to NUM do ShiftLow;
  end;
end;


procedure BeeRangeDec_StartDecode(Self: pointer);
var
  I: longword;
begin
  TRangeCoder_StartEncode(Self);
  with TRangeCoder(Self^) do
  begin
    // for I := 0 to NUM do Code := Code shl 8 + FStream.Read;
  end;
end;

procedure TRangeCoder_FinishDecode(Self: pointer);
begin
  { nothing to do }
end;






















function TRangeCoder.UpdateSymbol(Self: TRangeCoder; const Freq: TFreq; aSymbol: longword): longword;
var
  CumFreq, TotFreq, I: longword;
begin
  // Count CumFreq...
  CumFreq := 0;
  I := CumFreq;
  while I < aSymbol do
  begin
    Inc(CumFreq, Freq[I]);
    Inc(I);
  end;
  // Count TotFreq...
  TotFreq := CumFreq;
  I := Length(Freq);
  repeat
    Dec(I);
    Inc(TotFreq, Freq[I]);
  until I = aSymbol;
  // Encode...
  Encode(CumFreq, Freq[aSymbol], TotFreq);
  // Return Result...
  Result := aSymbol;
end;

procedure TRangeCoder.Decode(CumFreq, Freq, TotFreq: longword);
begin
  Code  := Code - MulDiv(Range, CumFreq, TotFreq);
  Range := MulDiv(Range, Freq, TotFreq);
  while Range < TOP do
  begin
    Code  := Code  shl 8 + FStream.Read;
    Range := Range shl 8;
  end;
end;

function TRangeCoder.GetFreq(TotFreq: longword): longword;
begin
  Result := MulDecDiv(Code + 1, TotFreq, Range);
end;

function TSecondaryDecoder.UpdateSymbol(const Freq: TFreq; aSymbol: longword): longword;
var
  CumFreq, TotFreq, SumFreq: longword;
begin
  // Count TotFreq...
  TotFreq := 0;
  aSymbol := Length(Freq);
  repeat
    Dec(aSymbol);
    Inc(TotFreq, Freq[aSymbol]);
  until aSymbol = 0;
  // Count CumFreq...
  CumFreq := GetFreq(TotFreq);
  // Search aSymbol...
  SumFreq := 0;
  aSymbol := SumFreq;
  while SumFreq + Freq[aSymbol] <= CumFreq do
  begin
    Inc(SumFreq, Freq[aSymbol]);
    Inc(aSymbol);
  end;
  // Finish Decode...
  Decode(SumFreq, Freq[aSymbol], TotFreq);
  // Return Result...
  Result := aSymbol;
end;

function TRangeCoder.InputByte: Cardinal;
var
  Value: Byte;
begin
  FStream.Read (Value, 1);
  Result := Value;
end;

procedure TRangeCoder.OutputByte (aValue: Cardinal);
begin
  FStream.Write (aValue, 1);
end;

end.