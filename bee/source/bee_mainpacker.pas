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

{$I bee_compiler.inc}

interface

type
  { TBaseCoder abstract class }

  TBaseCoder = class(TObject)
  private
    FStream: pointer;
    FLevel: longword;
    FLevelAux: longword;
    FFilter: string;
    FFilterAux: string;
    FBlock: int64;
  public
    constructor Create(Stream: pointer);
    procedure SetCompressionLevel(Value: longword); virtual;
    procedure SetCompressionLevelAux(Value: longword); virtual;
    procedure SetCompressionFilter(const Value: string); virtual;
    procedure SetCompressionFilterAux(const Value: string); virtual;
    procedure SetCompressionBlock(const Value: int64); virtual;

    procedure Init; virtual abstract;
    procedure Start; virtual abstract;
    procedure Finish; virtual abstract;
    function  Encode(Data: PByte; Count: longint): longint; virtual abstract;
    function  Decode(Data: PByte; Count: longint): longint; virtual abstract;
  end;

  { TStoreCoder class }

  TStoreCoder = class(TBaseCoder)
  public
    procedure Init; override;
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
    procedure Init; override;
  end;

  TBeeEncoder = class(TBeeCoder)
  public
    constructor Create(Stream: pointer);
    destructor Destroy; override;

    procedure Start; override;
    procedure Finish; override;
    function Encode(Data: PByte; Count: longint): longint; override;
  end;

  TBeeDecoder = class(TBeeCoder)
  public
    constructor Create(Stream: pointer);
    destructor Destroy; override;

    procedure Start; override;
    procedure Finish; override;
    function Decode(Data: PByte; Count: longint): longint; override;
  end;

  { TPpmdCoder classes }

  TPpmdCoder = class(TBaseCoder)
  private
    FCoder: pointer;
    FModeller: pointer;
  public
    procedure Init; override;
  end;

  TPpmdEncoder = class(TPpmdCoder)
  public
    constructor Create(Stream: pointer);
    destructor Destroy; override;

    procedure Start; override;
    procedure Finish; override;
    function Encode(Data: PByte; Count: longint): longint; override;
  end;

  TPpmdDecoder = class(TPpmdCoder)
  public
    constructor Create(Stream: pointer);
    destructor Destroy; override;

    procedure Start; override;
    procedure Finish; override;
    function Decode(Data: PByte; Count: longint): longint; override;
  end;

  TCoderAlgorithm = (caStore, caBee, caPpmd);

implementation

uses
  SysUtils,
  Bee_BufStream,
  Bee_Common,
  Bee_Configuration,
  Bee_Interface,
  {$IFDEF LIBBX}
  Bee_LibBx;
  {$ELSE}
  Bee_Modeller;
  {$ENDIF}

/// TBaseCoder abstract class

constructor TBaseCoder.Create(Stream: pointer);
begin
  inherited Create;
  FStream    := Stream;
  FLevel     :=  0;
  FLevelAux  :=  0;
  FFilter    := '';
  FFilterAux := '';
  FBlock     :=  0;
end;

procedure TBaseCoder.SetCompressionLevel(Value: longword);
begin
  FLevel := Value;
end;

procedure TBaseCoder.SetCompressionLevelAux(Value: longword);
begin
  FLevelAux := Value;
end;

procedure TBaseCoder.SetCompressionFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TBaseCoder.SetCompressionFilterAux(const Value: string);
begin
  FFilterAux := Value;
end;

procedure TBaseCoder.SetCompressionBlock(const Value: int64);
begin
  FBlock := Value;
end;

/// TStoreCoder class

procedure TStoreCoder.Init;
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
  Result := TBufStream(FStream).Write(Data, Count);
end;

function TStoreCoder.Decode(Data: PByte; Count: longint): longint;
begin
  Result := TBufStream(FStream).Read(Data, Count);
end;

/// TBeeCoder class

procedure TBeeCoder.Init;
var
  Table: TTableParameters;
begin
  if (FLevel < 1) or (FLevel > 3) then
    SetExitStatus(esCmdLineError);

  if (FLevelAux < 0) or (FLevelAux > 9) then
    SetExitStatus(esCmdLineError);

  if ExitStatus = esNoError then
  begin
    if not HexToData(FFilter, Table[1], SizeOf(Table)) then
      Table := DefaultTableParameters;
    BeeModeller_Init(FModeller, FLevelAux, @Table[1]);
  end;
end;

/// TBeeEncoder class

constructor TBeeEncoder.Create(Stream: pointer);
begin
  inherited Create(Stream);
  FCoder    := BeeRangeEnc_Create(Stream, @DoFlush);
  FModeller := BeeModeller_Create(FCoder);
end;

destructor TBeeEncoder.Destroy;
begin
  BeeModeller_Destroy(FModeller);
  BeeRangeEnc_Destroy(FCoder);
  inherited Destroy;
end;

procedure TBeeEncoder.Start;
begin
  BeeRangeEnc_StartEncode(FCoder);
end;

procedure TBeeEncoder.Finish;
begin
  BeeRangeEnc_FinishEncode(FCoder);
end;

function TBeeEncoder.Encode(Data: PByte; Count: longint): longint;
begin
  Result := BeeModeller_Encode(FModeller, Data, Count);
end;

/// TBeeDecoder class

constructor TBeeDecoder.Create(Stream: pointer);
begin
  inherited Create(Stream);
  FCoder    := BeeRangeDec_Create(Stream, @DoFill);
  FModeller := BeeModeller_Create(FCoder);
end;

destructor TBeeDecoder.Destroy;
begin
  BeeModeller_Destroy(FModeller);
  BeeRangeDec_Destroy(FCoder);
  inherited Destroy;
end;

procedure TBeeDecoder.Start;
begin
  BeeRangeDec_StartDecode(FCoder);
end;

procedure TBeeDecoder.Finish;
begin
  BeeRangeDec_FinishDecode(FCoder);
end;

function TBeeDecoder.Decode(Data: PByte; Count: longint): longint;
begin
  Result := BeeModeller_Decode(FModeller, Data, Count);
end;

{ TPpmdCoder class }

procedure TPpmdCoder.Init;
begin
  if (FLevel < 2) or (FLevel > 64) then
    SetExitStatus(esCmdLineError);

  if (FLevelAux < 2048) or (FLevelAux > 4294967296) then
    SetExitStatus(esCmdLineError);

  if ExitStatus = esNoError then
    PpmdModeller_Start(FModeller, FLevelAux, FLevel);
end;

{ TPpmdEncoder class }

constructor TPpmdEncoder.Create(Stream: pointer);
begin
  inherited Create(Stream);
  FCoder    := PpmdRangeEnc_Create(Stream, @DoFlush);
  FModeller := PpmdModeller_Create;
end;

destructor TPpmdEncoder.Destroy;
begin
  PpmdModeller_Destroy(FModeller);
  PpmdRangeEnc_Destroy(FCoder);
  inherited Destroy;
end;

procedure TPpmdEncoder.Start;
begin
  PpmdRangeEnc_StartEncode(FCoder);
end;

procedure TPpmdEncoder.Finish;
begin
  PpmdRangeEnc_FinishEncode(FCoder);
end;

function TPpmdEncoder.Encode(Data: PByte; Count: longint): longint;
begin
  Result := PpmdModeller_Encode(FModeller, FCoder, Data, Count);
end;

{ TPpmdDecoder class }

constructor TPpmdDecoder.Create(Stream: pointer);
begin
  inherited Create(Stream);
  FCoder    := PpmdRangeDec_Create(Stream, @DoFill);
  FModeller := PpmdModeller_Create;
end;

destructor TPpmdDecoder.Destroy;
begin
  PpmdModeller_Destroy(FModeller);
  PpmdRangeDec_Destroy(FCoder);
  inherited Destroy;
end;

procedure TPpmdDecoder.Start;
begin
  PpmdRangeDec_StartDecode(FCoder);
end;

procedure TPpmdDecoder.Finish;
begin
  PpmdRangeDec_FinishDecode(FCoder);
end;

function TPpmdDecoder.Decode(Data: PByte; Count: longint): longint;
begin
  Result := PpmdModeller_Decode(FModeller, FCoder, Data, Count);
end;

end.
