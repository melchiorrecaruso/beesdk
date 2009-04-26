unit Bee_RangeCoder;

{ Contains:

  TRangeCoder class, 
  
    kind of RangeCoder 
      (based on Eugeny Shelwien's shindler_var1 
        (based on Shindler's rangecoder), MaxFreq = 2^24); 
    It uses MulDiv opcode extension.

  (C) 2003 Evgeny Shelwien;
  (C) 2003-2007 Andrew Filinsky.

  Created:

  v0.1.0 build 0001 - 2003/02/01 by Evgeny Shelwien.

  Translated:

  v0.1.1 build 0002 - 2003/03/01 by Andrew Filinsky.

  Modifyed:

  v0.7.8 build 0153 - 2005/07/08 by Andrew Filinsky;
  v0.7.9 build 0301 - 2007/01/23 by Andrew Filinsky.
  v0.7.9 build 0316 - 2007/02/16 by Andrew Filinsky.
}

{$R-,Q-,S-}

interface

uses
  Classes,          /// TStream, ...
  Bee_Assembler;    /// Low-level routines ...

const
  TOP     = 1 shl 24;
  NUM     = 4;
  Thres   = 255 * Cardinal (TOP);
  MaxFreq = TOP - 1;

type
  /// TRangeCoder...

  TRangeCoder = class
    constructor Create (aStream: TStream);

    procedure   StartEncode;
    procedure   StartDecode;
    procedure   FinishEncode;
    procedure   FinishDecode;
    procedure   Encode (CumFreq, Freq, TotFreq: Cardinal);
    function    GetFreq (TotFreq: Cardinal): Cardinal;
    procedure   Decode (CumFreq, Freq, TotFreq: Cardinal);

  private
    procedure   ShiftLow;
    function    InputByte: Cardinal;
    procedure   OutputByte (aValue: Cardinal);

  private
    FStream: TStream;

    Range:  Cardinal;
    Low:    Cardinal;
    Code:   Cardinal;
    Carry:  Cardinal;
    Cache:  Cardinal;
    FFNum:  Cardinal;
  end;

implementation

/// TRangeCoder...

  constructor TRangeCoder.Create (aStream: TStream);
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
    I: Integer;
  begin
    StartEncode;
    for I := 0 to NUM do
      Code := Code shl 8 + InputByte;
  end;

  procedure TRangeCoder.FinishEncode;
  var
    I: Integer;
  begin
    for I := 0 to NUM do ShiftLow;
  end;

  procedure TRangeCoder.FinishDecode;
  begin
    /// Nothing to do...
  end;

  procedure TRangeCoder.Encode (CumFreq, Freq, TotFreq: Cardinal);
  var
    Tmp: Cardinal;
  begin
    Tmp   := Low;
    Low   := Low + MulDiv (Range, CumFreq, TotFreq);
    Carry := Carry + Cardinal (Low < Tmp);
    Range := MulDiv (Range, Freq, TotFreq);
    while Range < TOP do begin Range := Range shl 8; ShiftLow; end;
  end;

  procedure TRangeCoder.Decode (CumFreq, Freq, TotFreq: Cardinal);
  begin
    Code  := Code - MulDiv (Range, CumFreq, TotFreq);
    Range := MulDiv (Range, Freq, TotFreq);
    while Range < TOP do
    begin
      Code := Code shl 8 + InputByte;
      Range := Range shl 8;
    end;
  end;

  function  TRangeCoder.GetFreq (TotFreq: Cardinal): Cardinal;
  begin
    Result := MulDecDiv (Code + 1, TotFreq, Range);
  end;

  procedure TRangeCoder.ShiftLow;
  begin
    if (Low < Thres) or (Carry <> 0) then
    begin
      OutputByte (Cache + Carry);
      while FFNum <> 0 do
      begin
        OutputByte (Carry - 1);
        Dec (FFNum);
      end;
      Cache := Low shr 24;
      Carry := 0;
    end else
      Inc (FFNum);
    Low := Low shl 8;
  end;

  function  TRangeCoder.InputByte: Cardinal;
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
