{
  Copyright (c) 2003 Evgeny Shelwien;
  Copyright (c) 2003-2008 Andrew Filinsky

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

  Created:

  v0.1.0 build 0001 - 2003.02.01 by Evgeny Shelwien.

  Translated:

  v0.1.1 build 0002 - 2003.03.01 by Andrew Filinsky.

  Modifyed:

  v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
  v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
  v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;
  
  v0.7.9 build 0906 - 2008.10.25 by Melchiorre Caruso.
}

unit Bee_RangeCoder;

{$I compiler.inc}

interface

uses
  Classes, // TStream, ...
  Bee_Assembler; // Low-level routines ...

const
  TOP     = 1 shl 24;
  NUM     = 4;
  Thres   = 255 * cardinal(TOP);
  MaxFreq = TOP - 1;

// TRangeCoder...

type
  TRangeCoder = class
    constructor Create(aStream: TStream);
    procedure StartEncode;
    procedure StartDecode;
    procedure FinishEncode;
    procedure FinishDecode;
    procedure Encode(CumFreq, Freq, TotFreq: cardinal);
    function GetFreq(TotFreq: cardinal): cardinal;
    procedure Decode(CumFreq, Freq, TotFreq: cardinal);
  private
    procedure ShiftLow;
    function InputByte: cardinal;
    procedure OutputByte(aValue: cardinal);
  private
    FStream: TStream;
    Range:   cardinal;
    Low:     cardinal;
    Code:    cardinal;
    Carry:   cardinal;
    Cache:   cardinal;
    FFNum:   cardinal;
  end;

implementation

// TRangeCoder...

constructor TRangeCoder.Create(aStream: TStream);
begin
  inherited Create;
  FStream := aStream;
end;

procedure TRangeCoder.StartEncode;
begin
  Range := $FFFFFFFF;
  Low   := 0;
  FFNum := 0;
  Carry := 0;
end;

procedure TRangeCoder.StartDecode;
var
  I: cardinal;
begin
  StartEncode;
  for I := 0 to NUM do
    Code := Code shl 8 + InputByte;
end;

procedure TRangeCoder.FinishEncode;
var
  I: cardinal;
begin
  for I := 0 to NUM do
    ShiftLow;
end;

procedure TRangeCoder.FinishDecode;
begin
  // nothing to do...
end;

procedure TRangeCoder.Encode(CumFreq, Freq, TotFreq: cardinal);
var
  Tmp: cardinal;
begin
  Tmp   := Low;
  Low   := Low + MulDiv(Range, CumFreq, TotFreq);
  Carry := Carry + cardinal(Low < Tmp);
  Range := MulDiv(Range, Freq, TotFreq);
  while Range < TOP do
  begin
    Range := Range shl 8;
    ShiftLow;
  end;
end;

procedure TRangeCoder.Decode(CumFreq, Freq, TotFreq: cardinal);
begin
  Code  := Code - MulDiv(Range, CumFreq, TotFreq);
  Range := MulDiv(Range, Freq, TotFreq);
  while Range < TOP do
  begin
    Code  := Code shl 8 + InputByte;
    Range := Range shl 8;
  end;
end;

function TRangeCoder.GetFreq(TotFreq: cardinal): cardinal;
begin
  Result := MulDecDiv(Code + 1, TotFreq, Range);
end;

procedure TRangeCoder.ShiftLow;
begin
  if (Low < Thres) or (Carry <> 0) then
  begin
    OutputByte(Cache + Carry);
    while FFNum <> 0 do
    begin
      OutputByte(Carry - 1);
      Dec(FFNum);
    end;
    Cache := Low shr 24;
    Carry := 0;
  end else
    Inc(FFNum);
  Low := Low shl 8;
end;

function TRangeCoder.InputByte: cardinal;
var
  Value: byte;
begin
  FStream.Read(Value, 1);
  Result := Value;
end;

procedure TRangeCoder.OutputByte(aValue: cardinal);
begin
  FStream.Write(aValue, 1);
end;

end.
