{
  Copyright (c) 2003-2007 Andrew Filinsky

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

  TRangeCoder, range coder (based on Eugeny Shelwien's Shindlet
  (based on Shindler's rangecoder), MaxFreq = 2^24);

  Created: Evgeny Shelwien, 2002 (?);

  Translated: Andrew Filinsky, Dec 2002;
}

unit Bee_RangeCoder0;

{$I compiler.inc}

interface

uses
  Classes; // TStream, ...

const
  Top     = 1 shl 24;
  MaxFreq = Top - 1; // Top - 1;

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
    procedure OutTgtByte(Value: byte);
    function InpSrcByte: byte;
  private
    Stream: TStream;
    Code, Range, FFNum, Cache: cardinal;
    Low:    int64;
  end;

implementation

// TRangeCoder...

constructor TRangeCoder.Create(aStream: TStream);
begin
  inherited Create;
  Stream := aStream;
end;

procedure TRangeCoder.StartEncode;
begin
  Range := $FFFFFFFF;
  Low   := 0;
  FFNum := 0;
  Cache := 0;
end;

procedure TRangeCoder.StartDecode;
var
  I: cardinal;
begin
  Code  := 0;
  Range := $FFFFFFFF;
  for I := 0 to 4 do
    Code := (Code shl 8) or InpSrcByte;
end;

procedure TRangeCoder.FinishEncode;
var
  I: cardinal;
begin
  for I := 0 to 4 do
    ShiftLow;
end;

procedure TRangeCoder.FinishDecode;
begin
  // Nothing to do...
end;

procedure TRangeCoder.Encode(CumFreq, Freq, TotFreq: cardinal);
begin
  Range := Range div TotFreq;
  Low   := Low + CumFreq * Range;
  Range := Range * Freq;
  while Range < Top do
  begin
    ShiftLow;
    Range := Range shl 8;
  end;
end;

function TRangeCoder.GetFreq(TotFreq: cardinal): cardinal;
begin
  Range  := Range div TotFreq;
  Result := Code div Range;
end;

procedure TRangeCoder.Decode(CumFreq, Freq, TotFreq: cardinal);
begin
  Code  := Code - CumFreq * Range;
  Range := Range * Freq;
  while Range < Top do
  begin
    Code  := (Code shl 8) or InpSrcByte;
    Range := Range shl 8;
  end;
end;

procedure TRangeCoder.ShiftLow;
var
  C: cardinal;
begin
  if ((Low xor $FF000000) > $FFFFFF) then
  begin
    OutTgtByte(Cache + Low shr 32);
    C := $FF + (Low shr 32);
    while FFNum <> 0 do
    begin
      OutTgtByte(C);
      Dec(FFNum);
    end;
    Cache := cardinal(Low) shr 24;
  end else
    Inc(FFNum);
  Low := cardinal(Low) shl 8;
end;

procedure TRangeCoder.OutTgtByte(Value: byte);
begin
  Stream.Write(Value, 1);
end;

function TRangeCoder.InpSrcByte: byte;
begin
  Stream.Read(Result, 1);
end;

end.
