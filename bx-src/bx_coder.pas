{
  Copyright (c) 2010-2013 Melchiorre Caruso.

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

{
  Contains:

    TCoder classes;

  Fist release:

    v1.0 build 2165 - 2013.12.26 by Melchiorre Caruso.

  Modifyed:

}

unit bx_Coder;

{$I bx_compiler.inc}

interface

type
  { TCoder abstract class }

  TCoder = class(TObject)
  private
    FStream: pointer;
    FLevel: longword;
    FLevelAux: longword;
    FFilter: string;
    FFilterAux: string;
    FBlock: int64;
  public
    constructor Create(Stream: pointer);
    procedure Start; virtual abstract;
    procedure Finish; virtual abstract;
    function Encode(Data: PByte; Count: longint): longint; virtual abstract;
    function Decode(Data: PByte; Count: longint): longint; virtual abstract;
  public
    property Level: longword read FLevel write FLevel;
    property LevelAux: longword read FLevelAux write FLevelAux;
    property Filter: string read FFilter write FFilter;
    property FilterAux: string read FFilterAux write FFilterAux;
    property Block: int64 read FBlock write FBlock;
  end;

  { TStoreCoder classes }

  TStoreCoder = class(TCoder)
  public
    procedure Start; override;
    procedure Finish; override;
    function Encode(Data: PByte; Count: longint): longint; override;
    function Decode(Data: PByte; Count: longint): longint; override;
  end;

  { TBeeCoder classes }

  TBeeCoder = class(TCoder)
  private
    FCoder: pointer;
    FModeller: pointer;
  public
    procedure Start; override;
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

  TPpmdCoder = class(TCoder)
  private
    FCoder: pointer;
    FModeller: pointer;
  public
    procedure Start; override;
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

implementation

uses
  bx_Common,
  bx_Configuration,
  {$IFDEF LIBBX}
  bx_LibLink,
  {$ELSE}
  libbx_Stream,


  {$ENDIF}
  bx_Messages,
  bx_Stream,
  SysUtils;

/// TCoder abstract class

constructor TCoder.Create(Stream: pointer);
begin
  inherited Create;
  FStream    := Stream;
  FLevel     :=  0;
  FLevelAux  :=  0;
  FFilter    := '';
  FFilterAux := '';
  FBlock     :=  0;
end;

/// TStoreCoder class

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

procedure TBeeCoder.Start;
var
  Table: TTableParameters;
begin
  if FBlock = 0 then
  begin
    if HexToData(FFilter, Table[1], SizeOf(Table)) = FALSE then
      SetExitStatus(esBeeFilterError);

    BeeModeller_SetTableParameters(FModeller, @Table[1]);
    BeeModeller_SetDictionaryLevel(FModeller, FLevelAux);
    BeeModeller_FreshFlexible(FModeller);
  end else
    BeeModeller_FreshSolid(FModeller);
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
  inherited Start;
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
  inherited Start;
  BeeRangeDec_StartDecode(FCoder);
end;

procedure TBeeDecoder.Finish;
begin
  BeeRangeDec_FinishDecode(FCoder);
end;

function TBeeDecoder.Decode(Data: PByte; Count: longint): longint;
begin
  try
    Result := BeeModeller_Decode(FModeller, Data, Count);
  except
    SetExitStatus(esHashError);
  end;
end;

{ TPpmdCoder class }

procedure TPpmdCoder.Start;
begin
  if FBlock = 0 then
  begin
    PpmdModeller_SetMemSize (FModeller, FLevelAux);
    PpmdModeller_SetModelOrd(FModeller, FLevel);
  end;
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
  inherited Start;
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
  inherited Start;
  PpmdRangeDec_StartDecode(FCoder);
end;

procedure TPpmdDecoder.Finish;
begin
  PpmdRangeDec_FinishDecode(FCoder);
end;

function TPpmdDecoder.Decode(Data: PByte; Count: longint): longint;
begin
  try
    Result := PpmdModeller_Decode(FModeller, FCoder, Data, Count);
  except
    SetExitStatus(esHashError);
  end;
end;

end.