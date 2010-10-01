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

  { TStreamEncoder class }

  TStreamEncoder = class
  protected
    FStream: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
    FTicker: TTickerMethod;
    FTick: boolean;
  public
    constructor Create(Stream: TStream; Ticker: TTickerMethod);
    destructor Destroy; override;
    procedure SetTable(const Value: TTableParameters);
    procedure SetDictionary(Value: byte);
    procedure FreshFlexible;
    procedure FreshSolid;

    function CopyFrom(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
    function EncodeFrom(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
  end;

  { TStreamDecoder class }

  TStreamDecoder = class
  protected
    FStream: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
    FTicker: TTickerMethod;
    FTick: boolean;
  public
    constructor Create(Stream: TStream; Ticker: TTickerMethod);
    destructor Destroy; override;
    procedure SetTable(const Value: TTableParameters);
    procedure SetDictionary(Value: byte);
    procedure FreshFlexible;
    procedure FreshSolid;


    function DecodeTo(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
    function CopyTo(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
  end;

  { TFileStreamEncoder class }

  TFileStreamEncoder = class(TStreamEncoder)
  public
    function CopyFrom(const FileName: string; var CRC: longword): boolean; overload;
    function EncodeFrom(const FileName: string; var CRC: longword): boolean; overload;
  end;

  { TFileStreamDecoder class }

  TFileStreamDecoder = class(TStreamDecoder)
  public
    function CopyTo(const FileName: string; var CRC: longword): boolean; overload;
    function DecodeTo(const FileName: string; var CRC: longword): boolean; overload;
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

{ TStreamEncoder class }

constructor TStreamEncoder.Create(Stream: TStream; Ticker: TTickerMethod);
begin
  FStream := Stream;
  FSecondaryCodec := TSecondaryEncoder.Create(FStream);
  FPPM := TBaseCoder.Create(FSecondaryCodec);

  FTicker := Ticker;
  FTick   := Assigned(FTicker);
end;

destructor TStreamEncoder.Destroy;
begin
  FPPM.Free;
  FSecondaryCodec.Free;
  FStream := nil;
  FTicker := nil;
end;

procedure TStreamEncoder.SetDictionary(Value: byte);
begin
  FPPM.SetDictionary(Value);
end;

procedure TStreamEncoder.SetTable(const Value: TTableParameters);
begin
  FPPM.SetTable(Value);
end;

procedure TStreamEncoder.FreshFlexible;
begin
  FPPM.FreshFlexible;
end;

procedure TStreamEncoder.FreshSolid;
begin
  FPPM.FreshSolid;
end;

function TStreamEncoder.CopyFrom(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC := longword(-1);
  while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
  begin
    FStream.Write(Symbol, 1);
    UpdCrc32(CRC, Symbol);
    if FTick and (not FTicker) then Break;
    Inc(Result);
  end;
end;

function TStreamEncoder.EncodeFrom(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC := longword(-1);
  FSecondaryCodec.Start;
  while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
  begin
    FPPM.UpdateModel(Symbol);
    UpdCrc32(CRC, Symbol);
    if FTick and (not FTicker) then Break;
    Inc(Result);
  end;
  FSecondaryCodec.Flush;
end;

{ TStreamDecoder class }


constructor TStreamDecoder.Create(Stream: TStream; Ticker: TTickerMethod);
begin
  FStream := Stream;
  FSecondaryCodec := TSecondaryDecoder.Create(FStream);
  FPPM := TBaseCoder.Create(FSecondaryCodec);

  FTicker := Ticker;
  FTick   := Assigned(FTicker);
end;

destructor TStreamDecoder.Destroy;
begin
  FPPM.Free;
  FSecondaryCodec.Free;
  FStream := nil;
  FTicker := nil;
end;

procedure TStreamDecoder.SetDictionary(Value: byte);
begin
  FPPM.SetDictionary(Value);
end;

procedure TStreamDecoder.SetTable(const Value: TTableParameters);
begin
  FPPM.SetTable(Value);
end;

procedure TStreamDecoder.FreshFlexible;
begin
  FPPM.FreshFlexible;
end;

procedure TStreamDecoder.FreshSolid;
begin
  FPPM.FreshSolid;
end;


function TStreamDecoder.CopyTo(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC := longword(-1);
  while (Result < Size) and (FStream.Read(Symbol, 1) = 1) do
  begin
    Strm.Write(Symbol, 1);
    UpdCrc32(CRC, Symbol);
    if FTick and (not FTicker) then Break;
    Inc(Result);
  end;
end;

function TStreamDecoder.DecodeTo(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC := longword(-1);
  FSecondaryCodec.Start;
  while (Result < Size) do
  begin
    Symbol := FPPM.UpdateModel(0);
    Strm.Write(Symbol, 1);
    UpdCrc32(CRC, Symbol);
    if FTick and (not FTicker) then Break;
    Inc(Result);
  end;
  FSecondaryCodec.Flush;
end;

{ TFileStreamEncoder class }

function TFileStreamEncoder.CopyFrom(const FileName: string; var CRC: longword): boolean;
var
  Strm: TFileReader;
begin
  CRC  := longword(-1);
  Strm := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := CopyFrom(Strm, Strm.Size, CRC) = Strm.Size;
    Strm.Free;
  end else
    Result := False;
end;

function TFileStreamEncoder.EncodeFrom(const FileName: string; var CRC: longword): boolean;
var
  Strm: TFileReader;
begin
  CRC  := longword(-1);
  Strm := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := EncodeFrom(Strm, Strm.Size, CRC) = Strm.Size;
    Strm.Free;
  end else
    Result := False;
end;

{ TFileStreamDecoder class }

function TFileStreamDecoder.CopyTo(const FileName: string; var CRC: longword): boolean;
var
  Strm: TFileWriter;
begin
  CRC  := longword(-1);
  Strm := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := CopyTo(Strm, Strm.Size, CRC) = Strm.Size;
    Strm.Free;
  end else
    Result := False;
end;

function TFileStreamDecoder.DecodeTo(const FileName: string; var CRC: longword): boolean;
var
  Strm: TFileWriter;
begin
  CRC  := longword(-1);
  Strm := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := DecodeTo(Strm, Strm.Size, CRC) = Strm.Size;
    Strm.Free;
  end else
    Result := False;
end;

{ THeaderStreamEncoder class }

function THeaderStreamEncoder.CopyFrom(Strm: TStream; const Size: int64; Item: THeader): boolean;
var
  CRC: longword;
begin
  Strm.Seek(Item.StartPos, soBeginning);
  Item.StartPos := FStream.Seek(0, soCurrent);
  begin
    Result := CopyFrom(Strm, Size, CRC) = Size;
  end;
  Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;
end;

function THeaderStreamEncoder.EncodeFrom(Strm: TStream; const Size: int64; Item: THeader): boolean;
var
  FStreamPos: int64;
begin
  Strm.Seek(Item.StartPos, soBeginning);
  FStreamPos := FStream.Seek(0, soCurrent);
  if foMoved in Item.Flags then
    Result := CopyFrom(Strm, Size, Item.Crc) = Size
  else
    Result := EncodeFrom(Strm, Size, Item.Crc) = Size;
  Item.PackedSize := FStream.Seek(0, soCurrent) - FStreamPos;

  if Item.PackedSize <= Item.Size then
  begin
    Item.StartPos := FStreamPos;
  end else
  begin
    Include(Item.Flags, foMoved);
    Include(Item.Flags, foTear);
    FStream.Size := FStreamPos;

    FTick  := False;
    Result := CopyFrom(Strm, Size, Item.Crc) = Size;
    FTick  := Assigned(FTicker);

    Item.PackedSize := Item.Size;
  end;
end;

function THeaderStreamEncoder.EncodeFrom(Item: THeader): boolean;
var
  FStreamPos: int64;
begin
  FStreamPos := FStream.Seek(0, soCurrent);
  if foMoved in Item.Flags then
    Result := CopyFrom(Item.Link, Item.Crc)
  else
    Result := EncodeFrom(Item.Link, Item.Crc);
  Item.PackedSize := FStream.Seek(0, soCurrent) - FStreamPos;

  if Item.PackedSize <= Item.Size then
  begin
    Item.StartPos := FStreamPos;
  end else
  begin
    Include(Item.Flags, foMoved);
    Include(Item.Flags, foTear);
    FStream.Size := FStreamPos;

    FTick  := False;
    Result := CopyFrom(Item.Link, Item.Crc);
    FTick  := Assigned(FTicker);

    Item.PackedSize := Item.Size;
  end;
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
  if foMoved in Item.Flags then
    Result := CopyTo(Item.Link, CRC)
  else
    Result := DecodeTo(Item.Link, CRC);

  if Result then
  begin
    Result := Item.Crc = CRC;
    if Result then
    begin
      FileSetAttr(Item.Link, Item.Attr);
      FileSetDate(Item.Link, Item.Attr);
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
