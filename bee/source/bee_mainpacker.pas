{
  Copyright (c) 1999-2010 Andrew Filinsky and Melchiorre Caruso

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

}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Codec,
  Bee_Headers,
  Bee_Modeller,
  Bee_Interface,
  Bee_Configuration;

type

  TTickerMethod = function: boolean of object;

  { TStreamCoder class }

  TStreamCoder = class
  protected
    FStream: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
  public
    constructor Create(Stream: TStream; Decoder: boolean);
    destructor Destroy; override;
    procedure SetTable(const Value: TTableParameters);
    procedure SetDictionary(Value: byte);
    procedure FreshFlexible;
    procedure FreshSolid;
  end;

  { TStreamEncoder class }

  TStreamEncoder = class(TStreamCoder)
  private
    FTicker: TTickerMethod;
    FTick: boolean;
  public
    constructor Create(Stream: TStream; Ticker: TTickerMethod);
    destructor Destroy; override;
    function CopyFrom(Strm: TStream; const Size: int64): int64; overload; virtual;
    function CopyFrom(Strm: TStream; const Size: int64; var CRC: longword): int64; overload; virtual;
    function EncodeFrom(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
  end;

  { TStreamDecoder class }

  TStreamDecoder = class(TStreamCoder)
  private
    FTicker: TTickerMethod;
    FTick: boolean;
  public
    constructor Create(Stream: TStream; Ticker: TTickerMethod);
    destructor Destroy; override;
    function CopyTo(Strm: TStream; const Size: int64): int64; overload; virtual;
    function CopyTo(Strm: TStream; const Size: int64; var CRC: longword): int64; overload; virtual;
    function DecodeTo(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
  end;

  { TFileStreamEncoder class }

  TFileStreamEncoder = class(TStreamEncoder)
  public
    function CopyFrom(const FileName: string; var CRC: longword): int64; overload;
    function EncodeFrom(const FileName: string; var CRC: longword): int64; overload;
  end;

  { TFileStreamDecoder class }

  TFileStreamDecoder = class(TStreamDecoder)
  public
    function CopyTo(const FileName: string; const Size: int64; var CRC: longword): int64; overload;
    function DecodeTo(const FileName: string; const Size: int64; var CRC: longword): int64; overload;
  end;

  { THeaderStreamEncoder class }

  THeaderStreamEncoder = class(TFileStreamEncoder)
  public
    function CopyFrom(Strm: TStream; const Size: int64; Item: THeader): boolean; overload;
    function EncodeFrom(Strm: TStream; const Size: int64; Item: THeader): boolean; overload;
    function EncodeFrom(Item: THeader): boolean; overload;
    procedure InitializeCoder(Item: THeader);
  end;

  { THeaderStreamDecoder class }

  THeaderStreamDecoder = class(TFileStreamDecoder)
  public
    function DecodeTo(Item: THeader): boolean; overload;
    function DecodeToNul(Item: THeader): boolean;
    procedure InitializeCoder(Item: THeader);
  end;

implementation

uses
  SysUtils,
  Bee_Files,
  Bee_Crc;

{ TStreamCoder class }

constructor TStreamCoder.Create(Stream: TStream; Decoder: boolean);
begin
  FStream := Stream;
  if Decoder then
    FSecondaryCodec := TSecondaryDecoder.Create(FStream)
  else
    FSecondaryCodec := TSecondaryEncoder.Create(FStream);
  FPPM := TBaseCoder.Create(FSecondaryCodec);
end;

destructor TStreamCoder.Destroy;
begin
  FPPM.Free;
  FSecondaryCodec.Free;
  FStream := nil;
end;

procedure TStreamCoder.SetDictionary(Value: byte);
begin
  FPPM.SetDictionary(Value);
end;

procedure TStreamCoder.SetTable(const Value: TTableParameters);
begin
  FPPM.SetTable(Value);
end;

procedure TStreamCoder.FreshFlexible;
begin
  FPPM.FreshFlexible;
end;

procedure TStreamCoder.FreshSolid;
begin
  FPPM.FreshSolid;
end;

{ TStreamEncoder class }

constructor TStreamEncoder.Create(Stream: TStream; Ticker: TTickerMethod);
begin
  inherited Create(Stream, False);
  FTicker := Ticker;
  FTick := Assigned(FTicker);
end;

destructor TStreamEncoder.Destroy;
begin
  FTicker := nil;
  FTick := False;
  inherited Destroy;
end;

function TStreamEncoder.CopyFrom(Strm: TStream; const Size: int64): int64;
var
  Symbol: byte;
begin
  Result := 0;
  while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
  begin
    if FTick and (not FTicker) then Break;
    FStream.Write(Symbol, 1);
    Inc(Result);
  end;
end;

function TStreamEncoder.CopyFrom(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC    := longword(-1);
  while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
  begin
    if FTick and (not FTicker) then Break;
    FStream.Write(Symbol, 1);
    UpdCrc32(CRC, Symbol);
    Inc(Result);
  end;
end;

function TStreamEncoder.EncodeFrom(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC    := longword(-1);
  FSecondaryCodec.Start;
  while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
  begin
    if FTick and (not FTicker) then Break;
    FPPM.UpdateModel(Symbol);
    UpdCrc32(CRC, Symbol);
    Inc(Result);
  end;
  FSecondaryCodec.Flush;
end;

{ TStreamDecoder class }

constructor TStreamDecoder.Create(Stream: TStream; Ticker: TTickerMethod);
begin
  inherited Create(Stream, True);
  FTicker := Ticker;
  FTick := Assigned(FTicker);
end;

destructor TStreamDecoder.Destroy;
begin
  FTicker := nil;
  FTick := False;
  inherited Destroy;
end;

function TStreamDecoder.CopyTo(Strm: TStream; const Size: int64): int64;
var
  Symbol: byte;
begin
  Result := 0;
  while (Result < Size) and (FStream.Read(Symbol, 1) = 1) do
  begin
    if FTick and (not FTicker) then Break;
    Strm.Write(Symbol, 1);
    Inc(Result);
  end;
end;

function TStreamDecoder.CopyTo(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC    := longword(-1);
  while (Result < Size) and (FStream.Read(Symbol, 1) = 1) do
  begin
    if FTick and (not FTicker) then Break;
    Strm.Write(Symbol, 1);
    UpdCrc32(CRC, Symbol);
    Inc(Result);
  end;
end;

function TStreamDecoder.DecodeTo(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC    := longword(-1);
  FSecondaryCodec.Start;
  while (Result < Size) do
  begin
    if FTick and (not FTicker) then Break;
    Symbol := FPPM.UpdateModel(0);
    Strm.Write(Symbol, 1);
    UpdCrc32(CRC, Symbol);
    Inc(Result);
  end;
  FSecondaryCodec.Flush;
end;

{ TFileStreamEncoder class }

function TFileStreamEncoder.CopyFrom(const FileName: string; var CRC: longword): int64;
var
  Strm: TFileReader;
begin
  Result := 0;
  CRC    := longword(-1);
  Strm   := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := CopyFrom(Strm, Strm.Size, CRC);
    begin
      Strm.Free;
    end;
  end;
end;

function TFileStreamEncoder.EncodeFrom(const FileName: string; var CRC: longword): int64;
var
  Strm: TFileReader;
begin
  Result := 0;
  CRC    := longword(-1);
  Strm   := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := EncodeFrom(Strm, Strm.Size, CRC);
    begin
      Strm.Free;
    end;
  end;
end;

{ TFileStreamDecoder class }

function TFileStreamDecoder.CopyTo(const FileName: string; const Size: int64; var CRC: longword): int64;
var
  Strm: TFileWriter;
begin
  Result := 0;
  CRC    := longword(-1);
  Strm   := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := CopyTo(Strm, Size, CRC);
    begin
      Strm.Free;
    end;
  end;
end;

function TFileStreamDecoder.DecodeTo(const FileName: string; const Size: int64; var CRC: longword): int64;
var
  Strm: TFileWriter;
begin
  Result := 0;
  CRC    := longword(-1);
  Strm   := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := DecodeTo(Strm, Size, CRC);
    begin
      Strm.Free;
    end;
  end;
end;

{ THeaderStreamEncoder class }

function THeaderStreamEncoder.CopyFrom(Strm: TStream; const Size: int64; Item: THeader): boolean;
begin
  Strm.Seek(Item.StartPos, soBeginning);
  Item.StartPos := FStream.Seek(0, soCurrent);
  begin
    Result := CopyFrom(Strm, Size) = Size;
  end;
  Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;
end;

function THeaderStreamEncoder.EncodeFrom(Strm: TStream; const Size: int64; Item: THeader): boolean;
begin
  Strm.Seek(Item.StartPos, soBeginning);
  Item.StartPos := FStream.Seek(0, soCurrent);
  if foMoved in Item.Flags then
    Item.Size := CopyFrom(Strm, Size, Item.Crc)
  else
    Item.Size := EncodeFrom(Strm, Size, Item.Crc);
  Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;

  // optimize compression ...
  if Item.PackedSize > Item.Size then
  begin
    FStream.Size := Item.StartPos;
    Include(Item.Flags, foMoved);
    Include(Item.Flags, foTear);
    InitializeCoder(Item);

    FTick := False;
    Item.Size := CopyFrom(Strm, Size, Item.Crc);
    Item.PackedSize := Item.Size;
    FTick := Assigned(FTicker);
  end;
  Result := Item.Size <> -1;
end;

function THeaderStreamEncoder.EncodeFrom(Item: THeader): boolean;
begin
  Item.StartPos := FStream.Seek(0, soCurrent);
  if foMoved in Item.Flags then
    Item.Size := CopyFrom(Item.ExtName, Item.Crc)
  else
    Item.Size := EncodeFrom(Item.ExtName, Item.Crc);
  Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;

  // optimize compression ...
  if Item.PackedSize > Item.Size then
  begin
    FStream.Size := Item.StartPos;
    Include(Item.Flags, foMoved);
    Include(Item.Flags, foTear);
    InitializeCoder(Item);

    FTick := False;
    Item.Size := CopyFrom(Item.ExtName, Item.Crc);
    Item.PackedSize := Item.Size;
    FTick := Assigned(FTicker);
  end;
  Result := Item.Size <> -1;
end;

procedure THeaderStreamEncoder.InitializeCoder(Item: THeader);
begin
  if foDictionary in Item.Flags then FPPM.SetDictionary(Item.Dictionary);
  if foTable      in Item.Flags then FPPM.SetTable     (Item.Table);
  if foTear       in Item.Flags then
    FPPM.FreshFlexible
  else
    FPPM.FreshSolid;
end;

{ THeaderStreamDecoder class }

function THeaderStreamDecoder.DecodeTo(Item: THeader): boolean;
var
  CRC: longword;
begin
  FStream.Seek(Item.StartPos, soBeginning);
  case foMoved in Item.Flags of
    False: Result := DecodeTo(Item.ExtName, Item.Size, CRC) = Item.Size;
    True:  Result := CopyTo  (Item.ExtName, Item.Size, CRC) = Item.Size;
  end;

  if Result then
  begin
    Result := Item.Crc = CRC;
    if Result then
    begin
      FileSetAttr(Item.ExtName, Item.Attr);
      FileSetDate(Item.ExtName, Item.Time);
    end;
  end;
end;

function THeaderStreamDecoder.DecodeToNul(Item: THeader): boolean;
var
  CRC: longword;
  Strm: TNulWriter;
begin
  Strm := TNulWriter.Create;
  FStream.Seek(Item.StartPos, soBeginning);
  if foMoved in Item.Flags then
    Result := CopyTo(Strm, Item.Size, CRC) = Item.Size
  else
    Result := DecodeTo(Strm, Item.Size, CRC) = Item.Size;

  if Result then
  begin
    Result := CRC = Item.Crc;
  end;
  Strm.Free;
end;

procedure THeaderStreamDecoder.InitializeCoder(Item: THeader);
begin
  if foDictionary in Item.Flags then FPPM.SetDictionary(Item.Dictionary);
  if foTable      in Item.Flags then FPPM.SetTable     (Item.Table);
  if foTear       in Item.Flags then
    FPPM.FreshFlexible
  else
    FPPM.FreshSolid;
end;

end.
