{
  Copyright (c) 2012-2014 Melchiorre Caruso.

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

    Coder class.

  Modifyed:

    v1.0.0 build 2210 - 2014.01.15 by Melchiorre Caruso.

}

unit bx_coder;

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
  bx_common,
  bx_messages,
  {$IFDEF BEEPAS} libbx_bee_modeller, {$ENDIF}
  {$IFDEF BEEPAS} libbx_bee_rangecoder, {$ENDIF}
  bx_stream,
  libbx_bee_common;

{$IFDEF MSWINDOWS}
  {$linklib libmsvcrt}
{$ENDIF}

{$IFDEF UNIX}
  {$linklib libc}
  {$linklib libm}
{$ENDIF}

{$IFDEF MAC}
  TODO...
{$ENDIF}

{$link libbx_stream.o}
// bee coder link
{$IFNDEF BEEPAS}
  {$link libbx_bee_common.o}
  {$link libbx_bee_rangecoder.o}
  {$link libbx_bee_modeller.o}
{$ENDIF}
// ppmd coder link
{$link libbx_ppmd_common.o}
{$link libbx_ppmd_rangecoder.o}
{$link libbx_ppmd_modeller.o}

type
  TStreamRead  = function(Stream: pointer; Data: PByte; Count: longint): longint;
  TStreamWrite = function(Stream: pointer; Data: PByte; Count: longint): longint;

{$IFNDEF BEEPAS}
// bee interface
function  BeeRangeEnc_Create       (aStream: pointer; aStreamWrite: TStreamWrite): pointer; cdecl; external;
procedure BeeRangeEnc_Destroy      (Self: pointer); cdecl; external;
procedure BeeRangeEnc_StartEncode  (Self: pointer); cdecl; external;
procedure BeeRangeEnc_FinishEncode (Self: pointer); cdecl; external;
function  BeeRangeEnc_Update       (Self: pointer; Freq: pointer; aSymbol: longword): longword; cdecl; external;

function  BeeRangeDec_Create       (aStream: pointer; aStreamRead: TStreamRead): pointer; cdecl; external;
procedure BeeRangeDec_Destroy      (Self: pointer); cdecl; external;
procedure BeeRangeDec_StartDecode  (Self: pointer); cdecl; external;
procedure BeeRangeDec_FinishDecode (Self: pointer); cdecl; external;

function  BeeModeller_Create       (aCoder: pointer): pointer; cdecl; external;
procedure BeeModeller_Destroy      (Self :pointer); cdecl; external;

procedure BeeModeller_SetTableParameters(Self: pointer; TableParapeters: pointer); cdecl; external;
procedure BeeModeller_SetDictionaryLevel(Self: pointer; DictLevel: longword); cdecl; external;
procedure BeeModeller_FreshFlexible     (Self: pointer); cdecl; external;
procedure BeeModeller_FreshSolid        (Self: pointer); cdecl; external;

function  BeeModeller_Encode       (Self: pointer; Buffer: pointer; BufSize: longint): longint; cdecl; external;
function  BeeModeller_Decode       (Self: pointer; Buffer: pointer; BufSize: longint): longint; cdecl; external;
{$ENDIF}

// ppmd interface
function  PpmdRangeEnc_Create      (aStream: pointer; aStreamWrite: TStreamWrite): pointer; cdecl; external;
procedure PpmdRangeEnc_Destroy     (Self: pointer); cdecl; external;
procedure PpmdRangeEnc_StartEncode (Self: pointer); cdecl; external;
procedure PpmdRangeEnc_FinishEncode(Self: pointer); cdecl; external;

function  PpmdRangeDec_Create      (aStream: pointer; aStreamRead: TStreamRead): pointer; cdecl; external;
procedure PpmdRangeDec_Destroy     (Self: pointer); cdecl; external;
procedure PpmdRangeDec_StartDecode (Self: pointer); cdecl; external;
procedure PpmdRangeDec_FinishDecode(Self: pointer); cdecl; external;

function  PpmdModeller_Create: pointer; cdecl; external;
procedure PpmdModeller_Destroy     (Self: pointer); cdecl; external;

procedure PpmdModeller_SetMemSize  (Self: pointer; MemSize: longword); cdecl; external;
procedure PpmdModeller_SetModelOrd (Self: pointer; ModelOrd: longword); cdecl; external;

function  PpmdModeller_Encode      (Self: pointer; RangeEnc: pointer; Buffer: pointer; BufSize: longint): longint; cdecl; external;
function  PpmdModeller_Decode      (Self: pointer; RangeEnc: pointer; Buffer: pointer; BufSize: longint): longint; cdecl; external;

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
