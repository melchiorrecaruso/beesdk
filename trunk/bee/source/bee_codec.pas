{
  Copyright (c) 1999-2008 Andrew Filinsky

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

  TSecondaryCodec class,   abstract secondary codec, similar to RangeCoder or Arithmetic Coder;
  TSecondaryEncoder class, implementation of secondary encoder;
  TSecondaryDecoder class, implementation of secondary decoder;

  Modifyed:

  v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
  v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
  
  v0.7.9 build 0960 - 2009.02.27 by Melchiorre Caruso.
}

unit Bee_Codec;

{$I compiler.inc}

interface

uses
  Classes,        // TStream
  Bee_RangeCoder; // TRangeCoder, ...

const
  MaxFreq = Bee_RangeCoder.MaxFreq;

// Array of Frequencyes...

type
  TFreq = array of cardinal;

// Abstract secondary codec, like a RangeCoder or Arithmetic Coder...

type
  TSecondaryCodec = class(TRangeCoder)
    procedure Start; virtual; abstract;
    procedure Flush; virtual; abstract;
    function UpdateSymbol(Freq0, Freq1, aSymbol: cardinal): cardinal; overload; virtual; abstract;
    function UpdateSymbol(const Freq: TFreq; aSymbol: cardinal): cardinal; overload; virtual; abstract;
  end;

// Range Encoder...

type
  TSecondaryEncoder = class(TSecondaryCodec)
    procedure Start; override;
    procedure Flush; override;
    function UpdateSymbol(Freq0, Freq1, aSymbol: cardinal): cardinal; override;
    function UpdateSymbol(const Freq: TFreq; aSymbol: cardinal): cardinal; override;
  end;

// Range Decoder...

type
  TSecondaryDecoder = class(TSecondaryCodec)
    procedure Start; override;
    procedure Flush; override;
    function UpdateSymbol(Freq0, Freq1, aSymbol: cardinal): cardinal; override;
    function UpdateSymbol(const Freq: TFreq; aSymbol: cardinal): cardinal; override;
  end;

implementation

// TSecondaryEncoder...

procedure TSecondaryEncoder.Start;
begin
  StartEncode;
end;

procedure TSecondaryEncoder.Flush;
begin
  FinishEncode;
end;

function TSecondaryEncoder.UpdateSymbol(Freq0, Freq1, aSymbol: cardinal): cardinal;
begin
  if aSymbol = 0 then
    Encode(0, Freq0, Freq0 + Freq1)
  else
    Encode(Freq0, Freq1, Freq0 + Freq1);
  Result := aSymbol;
end;

function TSecondaryEncoder.UpdateSymbol(const Freq: TFreq; aSymbol: cardinal): cardinal;
var
  CumFreq, TotFreq, I: cardinal;
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

// TSecondaryDecoder...

procedure TSecondaryDecoder.Start;
begin
  StartDecode;
end;

procedure TSecondaryDecoder.Flush;
begin
  FinishDecode;
end;

function TSecondaryDecoder.UpdateSymbol(Freq0, Freq1, aSymbol: cardinal): cardinal;
begin
  if GetFreq(Freq0 + Freq1) < Freq0 then
  begin
    Decode(0, Freq0, Freq0 + Freq1);
    Result := 0;
  end else
  begin
    Decode(Freq0, Freq1, Freq0 + Freq1);
    Result := 1;
  end;
end;

function TSecondaryDecoder.UpdateSymbol(const Freq: TFreq; aSymbol: cardinal): cardinal;
var
  CumFreq, TotFreq, SumFreq: cardinal;
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
