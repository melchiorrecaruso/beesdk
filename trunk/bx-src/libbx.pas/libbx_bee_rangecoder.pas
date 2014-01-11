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
  libbx_stream;

const
  TOP     = 1 shl 24;
  THRES   = 255 * longword(TOP);
  MAXFREQ = TOP - 1;

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
  function  BeeRangeDec_Update      (Self: PBeeRangeDec; const Freq: TFreq; aSymbol: longword): longword;

type
  { PBeeRangeCodUpdate definition }

  PBeeRangeCodUpdate = function(Self: pointer; const Freq: TFreq; aSymbol: longword): longword;

implementation

uses
  libbx_bee_common;

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
  Tmp := Self^.FLow;
  Self^.FLow   := Self^.FLow + MulDiv(Self^.FRange, CumFreq, TotFreq);
  Self^.FCarry := Self^.FCarry + longword(Self^.FLow < Tmp);
  Self^.FRange := MulDiv(Self^.FRange, Freq, TotFreq);
  while Self^.FRange < TOP do
  begin
    Self^.FRange := Self^.FRange shl 8;
    BeeRangeEnc_ShiftLow(Self);
  end;
end;

procedure BeeRangeEnc_FinishEncode(Self: PBeeRangeEnc);
begin
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
end;

function BeeRangeEnc_Update(Self: PBeeRangeEnc; const Freq: TFreq; aSymbol: longword): longword;
var
  CumFreq, TotFreq, I: longword;
begin
  // Count CumFreq...
  CumFreq := 0; I := 0;
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
  BeeRangeEnc_Encode(Self, CumFreq, Freq[aSymbol], TotFreq);
  // Return Result...
  Result := aSymbol;
end;

{ TBeeRangeDec }

function BeeRangeDec_Create(aStream: pointer; aStreamRead: PStreamRead): PBeeRangeDec;
begin
  Result := GetMem(SizeOf(TBeeRangeDec));
  Result^.FStream := ReadStream_Create(aStream, aStreamRead);
end;

procedure BeeRangeDec_Destroy(Self: PBeeRangeDec);
begin
  ReadStream_Destroy(Self^.FStream);
  FreeMem(Self);
end;

procedure BeeRangeDec_StartDecode(Self: PBeeRangeDec);
begin
  Self^.FRange := $FFFFFFFF;
  Self^.FLow   := 0;
  Self^.FFNum  := 0;
  Self^.FCarry := 0;

  Self^.FCode := Self^.FCode shl 8 + ReadStream_Read(Self^.FStream);
  Self^.FCode := Self^.FCode shl 8 + ReadStream_Read(Self^.FStream);
  Self^.FCode := Self^.FCode shl 8 + ReadStream_Read(Self^.FStream);
  Self^.FCode := Self^.FCode shl 8 + ReadStream_Read(Self^.FStream);
  Self^.FCode := Self^.FCode shl 8 + ReadStream_Read(Self^.FStream);
end;

procedure BeeRangeDec_FinishDecode(Self: PBeeRangeDec);
begin
  ReadStream_ClearBuffer(Self^.FStream);
end;

function BeeRangeDec_GetFreq(Self: PBeeRangeDec; TotFreq: longword): longword;
begin
  Result := MulDecDiv(Self^.FCode + 1, TotFreq, Self^.FRange);
end;

procedure BeeRangeDec_Decode(Self: PBeeRangeDec; CumFreq, Freq, TotFreq: longword);
begin
  Self^.FCode  := Self^.FCode - MulDiv(Self^.FRange, CumFreq, TotFreq);
  Self^.FRange := MulDiv(Self^.FRange, Freq, TotFreq);
  while Self^.FRange < TOP do
  begin
    Self^.FCode  := Self^.FCode  shl 8 + ReadStream_Read(Self^.FStream);
    Self^.FRange := Self^.FRange shl 8;
  end;
end;

function BeeRangeDec_Update(Self: PBeeRangeDec; const Freq: TFreq; aSymbol: longword): longword;
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
  CumFreq := BeeRangeDec_GetFreq(Self, TotFreq);
  // Search aSymbol...
  SumFreq := 0;
  aSymbol := 0;
  while SumFreq + Freq[aSymbol] <= CumFreq do
  begin
    Inc(SumFreq, Freq[aSymbol]);
    Inc(aSymbol);
  end;
  // Finish Decode...
  BeeRangeDec_Decode(Self, SumFreq, Freq[aSymbol], TotFreq);
  // Return Result...
  Result := aSymbol;
end;

end.
