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

  TRangeCoder, range coder (based on Eugeny Shelwien's shindler_var1
  (based on Shindler's rangecoder), MaxFreq = 2^24 (?));

  Created: Evgeny Shelwien, Feb 2003.

  Translated: Andrew Filinsky, Mar 2003.
}

unit Bee_RangeCoder1;

{$I compiler.inc}

interface

uses
  Classes; // TStream, ...

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
    procedure OutTgtByte(Value: byte);
    function InpSrcByte: byte;
  private
    Stream: TStream;
    Range:  cardinal;
    Low:    cardinal;
    Code:   cardinal;
    Carry:  cardinal;
    Cache:  cardinal;
    FFNum:  cardinal;
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
  Carry := 0;
end;

procedure TRangeCoder.StartDecode;
var
  I: integer;
begin
  StartEncode;
  for I := 0 to NUM do
    code := code shl 8 + InpSrcByte;
end;

procedure TRangeCoder.FinishEncode;
var
  I: integer;
begin
  for I := 0 to NUM do
    ShiftLow;
end;

procedure TRangeCoder.FinishDecode;
begin
  // Nothing to do...
end;

procedure TRangeCoder.Encode(cumFreq, freq, totFreq: cardinal);
var
  tmp: cardinal;
begin
  tmp   := low;
  range := range div totFreq;
  low   := low + cumFreq * range;
  Carry := Carry + cardinal(low < tmp);
  range := range * freq;
  while Range < TOP do
  begin
    Range := Range shl 8;
    ShiftLow;
  end;
end;

function TRangeCoder.GetFreq(totFreq: cardinal): cardinal;
begin
  range  := range div totFreq;
  Result := code div range;
end;

procedure TRangeCoder.Decode(CumFreq, Freq, TotFreq: cardinal);
begin
  Code  := Code - CumFreq * Range;
  Range := Range * Freq;
  while Range < TOP do
  begin
    Code  := Code shl 8 + InpSrcByte;
    Range := Range shl 8;
  end;
end;

procedure TRangeCoder.ShiftLow;
begin
  if ((Low < Thres) or (Carry <> 0)) then
  begin
    OutTgtByte(Cache + Carry);
    while FFNum <> 0 do
    begin
      OutTgtByte(Carry - 1);
      Dec(FFNum);
    end;
    Cache := Low shr 24;
    Carry := 0;
  end else
    Inc(FFNum);
  Low := Low shl 8;
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
