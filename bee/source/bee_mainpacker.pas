{
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

    TStreamCoder class, stream encoder/decoder;

  Modifyed:

   v0.8.0 build 1864 - 2013.02.16 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

type
  { TBaseCoder abstract class }

  TBaseCoder = class(TObject)
  private
    FHandle: pointer;
  public
    constructor Create(Handle: pointer);
    procedure SetCompressionLevel(Value: longint); virtual abstract;
    procedure SetCompressionLevelAux(Value: longint); virtual abstract;
    procedure SetCompressionFilter(const Value: string); virtual abstract;
    procedure SetCompressionFilterAux(const Value: string); virtual abstract;
    procedure SetCompressionBlock(const Value: int64); virtual abstract;

    procedure Start; virtual abstract;
    procedure Finish; virtual abstract;
    function  Encode(Data: PByte; Count: longint): longint; virtual abstract;
    function  Decode(Data: PByte; Count: longint): longint; virtual abstract;
  end;

  { TStoreCoder class }

  TStoreCoder = class(TBaseCoder)
  public
    procedure SetCompressionLevel(Value: longint); override;
    procedure SetCompressionLevelAux(Value: longint); override;
    procedure SetCompressionFilter(const Value: string); override;
    procedure SetCompressionFilterAux(const Value: string); override;
    procedure SetCompressionBlock(const Value: int64); override;

    procedure Start; override;
    procedure Finish; override;
    function Encode(Data: PByte; Count: longint): longint; override;
    function Decode(Data: PByte; Count: longint): longint; override;
  end;

  { TBeeCoder classes }

  TBeeCoder = class(TBaseCoder)
  private
    FCoder: pointer;
    FModeller: pointer;
  public
    procedure SetCompressionLevel(Value: longint); override;
    procedure SetCompressionLevelAux(Value: longint); override;
    procedure SetCompressionFilter(const Filter: string); override;
    procedure SetCompressionFilterAux(const Filter: string); override;
    procedure SetCompressionBlock(const Value: int64); override;
  end;

  TBeeEncoder = class(TBeeCoder)
  public
    constructor Create(Handle: pointer);
    destructor Destroy; override;

    procedure Start; override;
    procedure Finish; override;
    function Encode(Data: PByte; Count: longint): longint; override;
  end;

  TBeeDecoder = class(TBeeCoder)
  public
    constructor Create(Handle: pointer);
    destructor Destroy; override;

    procedure Start; override;
    procedure Finish; override;
    function Decode(Data: PByte; Count: longint): longint; override;
  end;

  TCoderAlgorithm = (caStore, caBee);

implementation

uses
  SysUtils,
  Bee_Common,
  Bee_Configuration,
  Bee_Interface,
  {$IFDEF cLib}
    Bee_LibLink;
  {$ELSE}
    Bee_Modeller;
  {$ENDIF}

procedure DoFill(Handle: pointer; Data: PByte; Size: longint);
{$IFDEF cLIB} cdecl; {$ENDIF}
begin
  FileRead(THandle(Handle^), Data, Size);
end;

procedure DoFlush(Handle: pointer; Data: PByte; Size: longint);
{$IFDEF cLIB} cdecl; {$ENDIF}
begin
  FileWrite(THandle(Handle^), Data, Size);
end;

/// TBaseCoder abstract class

constructor TBaseCoder.Create(Handle: pointer);
begin
  inherited Create;
  FHandle := Handle;
end;

/// TStoreCoder class

procedure TStoreCoder.SetCompressionLevel(Value: longint);
begin
  // nothing to do
end;

procedure TStoreCoder.SetCompressionLevelAux(Value: longint);
begin
  // nothing to do
end;

procedure TStoreCoder.SetCompressionFilter(const Value: string);
begin
  // nothing to do
end;

procedure TStoreCoder.SetCompressionFilterAux(const Value: string);
begin
  // nothing to do
end;

procedure TStoreCoder.SetCompressionBlock(const Value: int64);
begin
  // nothing to do
end;

procedure TStoreCoder.Start;
begin
  // nothing to do
end;

procedure TStoreCoder.Finish;
begin
  // nothing to do
end;

function TStoreCoder.Encode(Data: PByte; Count: longint): longint;
begin
  Result := FileWrite(THandle(FHandle^), Data, Count);
end;

function TStoreCoder.Decode(Data: PByte; Count: longint): longint;
begin
  Result := FileRead(THandle(FHandle^), Data, Count);
end;

/// TBeeCoder class

procedure TBeeCoder.SetCompressionLevel(Value: longint);
begin
  // nothing to do
end;

procedure TBeeCoder.SetCompressionLevelAux(Value: longint);
begin
  BaseCoder_SetDictionary(FModeller, Value);
end;

procedure TBeeCoder.SetCompressionFilter(const Filter: string);
var
  Table: TTableParameters;
begin
  if HexToData(Filter, Table[1], SizeOf(Table)) then
  begin
    BaseCoder_SetTable(FModeller, @Table[1]);
  end;
end;

procedure TBeeCoder.SetCompressionFilterAux(const Filter: string);
begin
  // nothing to do
end;

procedure TBeeCoder.SetCompressionBlock(const Value: int64);
begin
  if Value = 0 then
    BaseCoder_FreshFlexible(FModeller)
  else
    BaseCoder_FreshSolid(FModeller);
end;

/// TBeeEncoder class

constructor TBeeEncoder.Create(Handle: pointer);
begin
  inherited Create(Handle);
  FCoder    := RangeEncoder_Create(Handle, @DoFlush);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor TBeeEncoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeEncoder_Destroy(FCoder);
  inherited Destroy;
end;

procedure TBeeEncoder.Start;
begin
  RangeEncoder_StartEncode(FCoder);
end;

procedure TBeeEncoder.Finish;
begin
  RangeEncoder_FinishEncode(FCoder);
end;

function TBeeEncoder.Encode(Data: PByte; Count: longint): longint;
begin
  Result := BaseCoder_Encode(FModeller, Data, Count);
end;

/// TBeeDecoder class

constructor TBeeDecoder.Create(Handle: pointer);
begin
  inherited Create(Handle);
  FCoder    := RangeDecoder_Create(Handle, @DoFill);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor TBeeDecoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeDecoder_Destroy(FCoder);
  inherited Destroy;
end;

procedure TBeeDecoder.Start;
begin
  RangeDecoder_StartDecode(FCoder);
end;

procedure TBeeDecoder.Finish;
begin
  RangeDecoder_FinishDecode(FCoder);
end;

function TBeeDecoder.Decode(Data: PByte; Count: longint): longint;
begin
  Result := BaseCoder_Decode(FModeller, Data, Count);
end;

end.
