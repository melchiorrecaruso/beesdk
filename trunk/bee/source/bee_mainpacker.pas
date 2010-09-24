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
  private
    FStream: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
    FTicker: TTickerMethod;
    FTick: boolean;
  public
    constructor Create(Stream: TStream; Ticker: TTickerMethod);
    destructor Destroy; override;
    function CopyFrom(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
    function EncodeFrom(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
    function DecodeTo(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
    function CopyTo(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual;
    procedure SetTable(const Value: TTableParameters);
    procedure SetDictionary(Value: byte);
    procedure FreshFlexible;
    procedure FreshSolid;
  end;

  { TFileStreamCoder class }

  TFileStreamCoder = class(TStreamCoder)
  public
    function CopyFrom(const FileName: string; var CRC: longword): boolean; overload;
    function EncodeFrom(const FileName: string; var CRC: longword): boolean; overload;
    function CopyTo(const FileName: string; var CRC: longword): boolean; overload;
    function DecodeTo(const FileName: string; var CRC: longword): boolean; overload;
  end;

  { THeaderStreamCoder class }

  THeaderStreamCoder = class(TFileStreamCoder)
  public
    function CopyFrom(Strm: TStream; const Size: int64; Item: THeader): boolean; overload;
    function EncodeFrom(Strm: TStream; const Size: int64; Item: THeader): boolean; overload;
    function EncodeFrom(Item: THeader): boolean; overload;
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

constructor TStreamCoder.Create(Stream: TStream; Ticker: TTickerMethod);
begin
  FStream := Stream;
  FSecondaryCodec := TSecondaryEncoder.Create(FStream);
  FPPM := TBaseCoder.Create(FSecondaryCodec);

  FTicker := Ticker;
  FTick   := Assigned(FTicker);
end;

destructor TStreamCoder.Destroy;
begin
  FPPM.Free;
  FSecondaryCodec.Free;
  FStream := nil;
  FTicker := nil;
end;

function TStreamCoder.CopyFrom(Strm: TStream; const Size: int64; var CRC: longword): int64;
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

function TStreamCoder.EncodeFrom(Strm: TStream; const Size: int64; var CRC: longword): int64;
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

function TStreamCoder.DecodeTo(Strm: TStream; const Size: int64; var CRC: longword): int64;
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

function TStreamCoder.CopyTo(Strm: TStream; const Size: int64; var CRC: longword): int64;
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

{ TFileStreamCoder class }

function TFileStreamCoder.CopyFrom(const FileName: string; var CRC: longword): boolean;
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

function TFileStreamCoder.EncodeFrom(const FileName: string; var CRC: longword): boolean;
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

function TFileStreamCoder.DecodeTo(const FileName: string; var CRC: longword): boolean;
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

function TFileStreamCoder.CopyTo(const FileName: string; var CRC: longword): boolean;
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

{ THeaderStreamCoder class }

function THeaderStreamCoder.CopyFrom(Strm: TStream; const Size: int64; Item: THeader): boolean;
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

function THeaderStreamCoder.EncodeFrom(Strm: TStream; const Size: int64; Item: THeader): boolean;
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
  end;
end;

function THeaderStreamCoder.EncodeFrom(Item: THeader): boolean;
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
  end;
end;

function THeaderStreamCoder.DecodeTo(Item: THeader): boolean;
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

function THeaderStreamCoder.DecodeToNul(Item: THeader): boolean;
var
  Strm: TNulWriter;
  CRC: longword;
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

procedure THeaderStreamCoder.InitializeCoder(Item: THeader);
begin
  if foDictionary in Item.Flags then FPPM.SetDictionary(Item.Dictionary);
  if foTable      in Item.Flags then FPPM.SetTable     (Item.Table);
  if foTear       in Item.Flags then
    FPPM.FreshFlexible
  else
    FPPM.FreshSolid;
end;

end.
