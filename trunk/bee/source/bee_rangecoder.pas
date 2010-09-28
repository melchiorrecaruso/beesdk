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
  
    v0.8.0 build 1100 - 2010.03.21 by Melchiorre Caruso.
}

unit Bee_RangeCoder;

{$I compiler.inc}

interface

uses
  Classes, Bee_Assembler;

const
  TOP     = 1 shl 24;
  NUM     = 4;
  Thres   = 255 * longword(TOP);
  MaxFreq = TOP - 1;

type
  { TRangeCoder }

  TRangeCoder = class
  private
    FStream: TStream;
    Range:   longword;
    Low:     longword;
    Code:    longword;
    Carry:   longword;
    Cache:   longword;
    FFNum:   longword;
    procedure ShiftLow;
    function InputByte: byte;
    procedure OutputByte(aValue: byte);
  public
    constructor Create(aStream: TStream);
    procedure StartEncode;
    procedure StartDecode;
    procedure FinishEncode;
    procedure FinishDecode;
    procedure Encode(CumFreq, Freq, TotFreq: longword);
    function GetFreq(TotFreq: longword): longword;
    procedure Decode(CumFreq, Freq, TotFreq: longword);
  end;

implementation

{ TRangeCoder }

constructor TRangeCoder.Create(aStream: TStream);
begin
  inherited Create;
  FStream := aStream;
end;

procedure TRangeCoder.StartEncode; {$IFDEF FPC} inline; {$ENDIF}
begin
  Range := $FFFFFFFF;
  Low   := 0;
  FFNum := 0;
  Carry := 0;
end;

procedure TRangeCoder.StartDecode; {$IFDEF FPC} inline; {$ENDIF}
var
  I: longword;
begin
  StartEncode;
  for I := 0 to NUM do
    Code := Code shl 8 + InputByte;
end;

procedure TRangeCoder.FinishEncode; {$IFDEF FPC} inline; {$ENDIF}
var
  I: longword;
begin
  for I := 0 to NUM do ShiftLow;
end;

procedure TRangeCoder.FinishDecode; {$IFDEF FPC} inline; {$ENDIF}
begin
  { nothing to do }
end;

procedure TRangeCoder.Encode(CumFreq, Freq, TotFreq: longword); {$IFDEF FPC} inline; {$ENDIF}
var
  Tmp: longword;
begin

  WRITELN(' ENCODE - ERROR');


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

procedure TRangeCoder.Decode(CumFreq, Freq, TotFreq: longword); {$IFDEF FPC} inline; {$ENDIF}
begin
  Code  := Code - MulDiv(Range, CumFreq, TotFreq);
  Range := MulDiv(Range, Freq, TotFreq);
  while Range < TOP do
  begin
    Code  := Code  shl 8 + InputByte;
    Range := Range shl 8;
  end;
end;

function TRangeCoder.GetFreq(TotFreq: longword): longword; {$IFDEF FPC} inline; {$ENDIF}
begin
  Result := MulDecDiv(Code + 1, TotFreq, Range);
end;

procedure TRangeCoder.ShiftLow; {$IFDEF FPC} inline; {$ENDIF}
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

function TRangeCoder.InputByte: byte; {$IFDEF FPC} inline; {$ENDIF}
begin
  FStream.Read(Result, 1);
end;

procedure TRangeCoder.OutputByte(aValue: byte); {$IFDEF FPC} inline; {$ENDIF}
begin
  WRITELN('DEBUG-5');
  FStream.Write(aValue, 1);
  WRITELN('DEBUG-6');
end;

end.
