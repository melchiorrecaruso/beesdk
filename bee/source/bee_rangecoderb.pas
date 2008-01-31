{
  Copyright (c) 2002-2007 Andrew Filinsky

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

{ Unit contains classes:

  TRangeCoder, range coder
    (based on BeeAri
      (based on arithmetic coder), MaxFreq = 2^29);

  Created: Andrew Filinsky, 2002;
}

unit Bee_RangeCoderB;

{$I compiler.inc}

interface

uses
  Classes; // TStream

const
  ValueBits = 26; // 31;
  FirstQtr = 1 shl (ValueBits - 2);
  Half = 2 * FirstQtr;
  ThirdQtr = 3 * FirstQtr;
  TopValue = Half - 1 + Half;
  MaxFreq = FirstQtr - 1;

// TRangeCoder...

type
  TRangeCoder = class
    constructor Create(aStream: TStream);

    procedure StartEncode;
    procedure FinishEncode;
    procedure Encode(CumFreq, Freq, TotFreq: cardinal);

    procedure StartDecode;
    procedure FinishDecode;
    function GetFreq(TotFreq: cardinal): cardinal;
    procedure Decode(CumFreq, Freq, TotFreq: cardinal);
  private
    // For encoding...
    procedure OutBit(Bit: cardinal);
    procedure BitPlusFollow(Bit: cardinal);
    // For decoding...
    function InputBit: cardinal;
  private
    Stream: TStream;

    Buffer, Bits,        // For bytes output/input...
    BitsToFollow,        // For encoding...
    Value,               // For decoding...
    Low, High: cardinal; // For both encoding and decoding...
  end;

// Opcode extension functions...

function MulDiv(A, B, C: cardinal): cardinal; forward;
function MulDecDiv(A, B, C: cardinal): cardinal; forward;

implementation

// TRangeCoder...

constructor TRangeCoder.Create(aStream: TStream);
begin
  inherited Create;
  Stream := aStream;
end;

procedure TRangeCoder.StartEncode;
begin
  Low  := 0;
  High := TopValue;
  BitsToFollow := 0;
  Buffer := 0;
  Bits := 1;
end;

procedure TRangeCoder.FinishEncode;
begin
  if Bits = 0 then
    Exit;
  Inc(BitsToFollow);
  if Low < FirstQtr then
    BitPlusFollow(0)
  else
    BitPlusFollow(cardinal(-1));
  if Bits > $00800000 then
    Stream.Write(Buffer, 4)
  else
  if Bits > $00008000 then
    Stream.Write(Buffer, 3)
  else
  if Bits > $00000080 then
    Stream.Write(Buffer, 2)
  else
  if Bits > $00000001 then
    Stream.Write(Buffer, 1);
  Bits := 0;
end;

procedure TRangeCoder.Encode(CumFreq, Freq, TotFreq: cardinal);
var
  Range: cardinal;
begin
  // Recount bounds...
  Range := High - Low + 1;
  High := Low + MulDiv(Range, CumFreq + Freq, TotFreq) - 1;
  Low := Low + MulDiv(Range, CumFreq, TotFreq);
  // Emit bites...
  while True do
  begin
    if High < Half then
      BitPlusFollow(0)
    else if Low >= Half then
    begin
      BitPlusFollow(cardinal(-1));
      Dec(Low, Half);
      Dec(High, Half);
    end else if (Low >= FirstQtr) and (High < ThirdQtr) then
    begin
      Inc(BitsToFollow);
      Dec(Low, FirstQtr);
      Dec(High, FirstQtr);
    end else
      break;
    Low  := Low shl 1;
    High := High shl 1 + 1;
  end;
end;

// For encoding...

procedure TRangeCoder.OutBit(Bit: cardinal);
begin
  Inc(Buffer, Bits and Bit);
  Bits := Bits shl 1;
  if Bits = 0 then
  begin
    Stream.Write(Buffer, SizeOf(Buffer));
    Buffer := 0;
    Inc(Bits);
  end;
end;

procedure TRangeCoder.BitPlusFollow(Bit: cardinal);
begin
  OutBit(Bit);
  while BitsToFollow > 0 do
  begin
    OutBit(not Bit);
    Dec(BitsToFollow);
  end;
end;

procedure TRangeCoder.StartDecode;
var
  I: cardinal;
begin
  Bits := 0;
  Value := 0;
  Low  := 0;
  High := TopValue;
  for I := 1 to ValueBits do
    Value := Value shl 1 + InputBit;
end;

procedure TRangeCoder.FinishDecode;
begin
  // nothing to do...
end;

function TRangeCoder.GetFreq(TotFreq: cardinal): cardinal;
var
  Range: cardinal;
begin
  Range  := High - Low + 1;
  Result := MulDecDiv(Value - Low + 1, TotFreq, Range);
end;

procedure TRangeCoder.Decode(CumFreq, Freq, TotFreq: cardinal);
var
  Range: cardinal;
begin
  // Recount bounds...
  Range := High - Low + 1;
  High := Low + MulDiv(Range, CumFreq + Freq, TotFreq) - 1;
  Low := Low + MulDiv(Range, CumFreq, TotFreq);
  // Emit bites...
  while True do
  begin
    if High < Half then
    // nothing
    else if Low >= Half then
    begin
      Dec(Value, Half);
      Dec(Low, Half);
      Dec(High, Half);
    end else if (Low >= FirstQtr) and (High < ThirdQtr) then
    begin
      Dec(Value, FirstQtr);
      Dec(Low, FirstQtr);
      Dec(High, FirstQtr);
    end else
      break;
    Low  := Low shl 1;
    High := High shl 1 + 1;
    Value := Value shl 1 + InputBit;
  end;
end;

function TRangeCoder.InputBit: cardinal;
begin
  if Bits = 0 then
  begin
    Stream.Read(Buffer, 1);
    Bits := 8;
  end;
  Result := Buffer and 1;
  Buffer := Buffer shr 1;
  Dec(Bits);
end;

// Opcode extension functions...

function MulDiv(A, B, C: cardinal): cardinal; assembler;
asm
  MUL B
  div C
end;

function MulDecDiv(A, B, C: cardinal): cardinal; assembler;
asm
  MUL B
  SUB EAX, 1
  SBB EDX, 0
  div C
end;

end.
