{
  Copyright (c) 2003 Evgeny Shelwien;
  Copyright (c) 2003-2010 Andrew Filinsky

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
  (C) 2003-2010 Andrew Filinsky.

  Created:

    v0.1.0 build 0001 - 2003.02.01 by Evgeny Shelwien.

  Translated:

    v0.1.1 build 0002 - 2003.03.01 by Andrew Filinsky.

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
    v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;
  
    v0.8.0 build 1100 - 2011.08.02 by Melchiorre Caruso.
}

unit Bee_RangeCoder;

{$I compiler.inc}

interface

uses
  Classes;

type
  { Array of Frequencyes }

  TFreq = array of longword;

function  RangeEncoder_Create      (aStream: TStream): pointer;
procedure RangeEncoder_Destroy     (Self: pointer);
procedure RangeEncoder_StartEncode (Self: pointer);
procedure RangeEncoder_FinishEncode(Self: pointer);
function  RangeEncoder_Update      (Self: pointer, const TFreq Freq, uint32 aSymbol);

function  RangeDecoder_Create      (aStream: TStream): pointer;
procedure RangeDecoder_Destroy     (Self: pointer);
procedure RangeDecoder_StartDecode (Self: pointer);
procedure RangeDecoder_FinishDecode(Self: pointer);

implementation

uses
  BeeLib_Assembler;

const
  TOP     = 1 shl 24;
  NUM     = 4;
  Thres   = 255 * longword(TOP);
  MaxFreq = TOP - 1;

type
  { TRangeCoder }

  TRangeCoder = packed record
    Stream: TStream;
    Range:   longword;
    Low:     longword;
    Code:    longword;
    Carry:   longword;
    Cache:   longword;
    FFNum:   longword;
  end;

{ TRangeCoder }

function TRangeCoder_Create(aStream: TStream): pointer;
begin
  Result := GetMem(SizeOf(TRangeCoder), 0);
  with Result^ do
  begin
    Stream := aStream;
  end;
end;

procedure TRangeCoder_Destroy(Self: pointer);
begin
  FreeMem(Self);
end;

procedure TRangeCoder_StartEncode(Self: pointer);
begin
  with Result^ do
  begin
    Range := $FFFFFFFF;
    Low   := 0;
    FFNum := 0;
    Carry := 0;
  end;
end;

procedure TRangeCoder_StartDecode;
var
  I: longword;
begin
  TRangeCoder_StartEncode(Self);
  with Result^ do
  begin
    for I := 0 to NUM do
      Code := Code shl 8 + FStream.Read;
  end;
end;

procedure TRangeCoder.FinishEncode;
var
  I: longword;
begin
  for I := 0 to NUM do ShiftLow;
end;

procedure TRangeCoder.FinishDecode;
begin
  { nothing to do }
end;

procedure TRangeCoder.Encode(Self: TRangeCoder; CumFreq, Freq, TotFreq: longword);
var
  Tmp: longword;
begin
  Tmp   := Low;
  Low   := Low + MulDiv(Range, CumFreq, TotFreq);
  Carry := Carry + longword(Low < Tmp);
  Range := MulDiv(Range, Freq, TotFreq);
  while Range < TOP do
  begin
    Range := Range shl 8;
    ShiftLow;
  end;
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

procedure TRangeCoder.ShiftLow;
begin
  if (Low < Thres) or (Carry <> 0) then
  begin
    FStream.Write(Cache + Carry);
    while FFNum <> 0 do
    begin
      FStream.Write(Carry - 1);
      Dec(FFNum);
    end;
    Cache := Low shr 24;
    Carry := 0;
  end else
    Inc(FFNum);

  Low := Low shl 8;
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

end.
