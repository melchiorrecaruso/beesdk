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

unit Bee_MainPacker2;

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

  { TStreamCoder class }

  TStreamCoder = class
  private
    FStream: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
    FTicker: TThreadMethod;
    FTick: boolean;
  public
    constructor Create(Stream: TStream; Ticker: TThreadMethod);
    destructor Destroy; override;
    function CopyFrom(Strm: TStream; Size: int64; var CRC: longword): boolean; virtual;
    function EncodeFrom(Strm: TStream; Size: int64; var CRC: longword): boolean; virtual;
    function DecodeTo(Strm: TStream; Size: int64; var CRC: longword): boolean; virtual;
    function CopyTo(Strm: TStream; Size: int64; var CRC: longword): boolean; virtual;
    procedure SetTable(const Value: TTableParameters);
    procedure SetDictionary(Value: byte);
    procedure FreshFlexible;
    procedure FreshSolid;
  end;

  { TFileStreamCoder class }

  TFileStreamCoder = class(TStreamCoder)
  public
    function CopyFrom(const FileName: string): longword; overload;
    function EncodeFrom(const FileName: string): longword; overload;
    function CopyTo(const FileName: string): longword; overload;
    function DecodeTo(const FileName: string): longword; overload;
  end;

  { THeaderStreamCoder class }

  THeaderStreamCoder = class(TFileStreamCoder)
  public
    procedure CopyFrom(Strm: TStream; const Size: int64; Item: THeader); overload;
    procedure EncodeFrom(Strm: TStream; const Size: int64; Item: THeader); overload;
    procedure EncodeFrom(Item: THeader); overload;
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

constructor TStreamCoder.Create(Stream: TStream; Ticker: TThreadMethod);
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

function TStreamCoder.CopyFrom(Strm: TStream; Size: int64; var CRC: longword): boolean;
var
  Symbol: byte;
begin
  CRC := longword(-1);
  while (Size > 0) and (Strm.Read(Symbol, 1) = 1) do
  begin
    FStream.Write(Symbol, 1);
    UpdCrc32(CRC, Symbol);
    if FTick then FTicker;
    Dec(Size);
  end;
  Result := Size = 0;
end;

function TStreamCoder.EncodeFrom(Strm: TStream; Size: int64; var CRC: longword): boolean;
var
  Symbol: byte;
begin
  CRC := longword(-1);
  FSecondaryCodec.Start;
  while (Size > 0) and (Strm.Read(Symbol, 1) = 1) do
  begin
    FPPM.UpdateModel(Symbol);
    UpdCrc32(Result, Symbol);
    if FTick then FTicker;
    Dec(Size);
  end;
  FSecondaryCodec.Flush;
  Result := Size = 0;
end;

function TStreamCoder.DecodeTo(Strm: TStream; Size: int64): longword;
var
  Symbol: byte;
begin
  Result := longword(-1);
  FSecondaryCodec.Start;
  while Size > 0 do
  begin
    Symbol := FPPM.UpdateModel(0);
    Strm.Write(Symbol, 1);
    UpdCrc32(Result, Symbol);
    if FTick then FTicker;
    Dec(Size);
  end;
  FSecondaryCodec.Flush;
end;

function TStreamCoder.CopyTo(Strm: TStream; Size: int64): longword;
var
  Symbol: byte;
begin
  Result := longword(-1);
  while Size > 0 do
  begin
    FStream.Read(Symbol, 1);
    Strm.Write(Symbol, 1);
    UpdCrc32(Result, Symbol);
    if FTick then FTicker;
    Dec(Size);
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

function TFileStreamCoder.CopyFrom(const FileName: string): longword;
var
  Strm: TFileReader;
begin
  Result := longword(-1);
  Strm   := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := CopyFrom(Strm, Strm.Size);
    Strm.Free;
  end;
end;

function TFileStreamCoder.EncodeFrom(const FileName: string): longword;
var
  Strm: TFileReader;
begin
  Result := longword(-1);
  Strm   := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := EncodeFrom(Strm, Strm.Size);
    Strm.Free;
  end;
end;

function TFileStreamCoder.DecodeTo(const FileName: string): longword;
var
  Strm: TFileWriter;
begin
  Result := longword(-1);
  Strm   := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := DecodeTo(Strm, Strm.Size);
    Strm.Free;
  end;
end;

function TFileStreamCoder.CopyTo(const FileName: string): longword;
var
  Strm: TFileWriter;
begin
  Result := longword(-1);
  Strm   := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := CopyTo(Strm, Strm.Size);
    Strm.Free;
  end;
end;

{ THeaderStreamCoder class }

procedure THeaderStreamCoder.CopyFrom(Strm: TStream; const Size: int64; Item: THeader);
begin
  Strm.Seek(Item.StartPos, soBeginning);
  Item.StartPos := FStream.Seek(0, soCurrent);
  begin
    CopyFrom(Strm, Size);
  end;
  Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;
end;

procedure THeaderStreamCoder.EncodeFrom(Strm: TStream; const Size: int64; Item: THeader);
var
  FStreamPos: int64;
begin
  Strm.Seek(Item.StartPos, soBeginning);
  FStreamPos := FStream.Seek(0, soCurrent);
  if foMoved in Item.Flags then
    Item.Crc := CopyFrom(Strm, Size)
  else
    Item.Crc := EncodeFrom(Strm, Size);
  Item.PackedSize := FStream.Seek(0, soCurrent) - FStreamPos;

  if Item.PackedSize <= Item.Size then
  begin
    Item.StartPos := FStreamPos;
  end else
  begin
    Include(Item.Flags, foMoved);
    Include(Item.Flags, foTear);



    FStream.Size := FStreamPos;

    FTick    := False;
    Item.Crc := CopyFrom(Strm, Size);
    FTick    := Assigned(FTicker);
  end;
end;

function THeaderStreamCoder.EncodeFrom(Item: THeader): boolean;
var
  FStreamPos: int64;
begin
  FStreamPos := FStream.Seek(0, soCurrent);
  if foMoved in Item.Flags then
    Item.Crc := CopyFrom(Item.Link)
  else
    Item.Crc := EncodeFrom(Item.Link);
  Item.PackedSize := FStream.Seek(0, soCurrent) - FStreamPos;

  if Item.PackedSize <= Item.Size then
  begin
    Item.StartPos := FStreamPos;
  end else
  begin
    Include(Item.Flags, foMoved);
    Include(Item.Flags, foTear);
    FStream.Size := FStreamPos;

    FTick := False;
    CopyFrom(Strm, Size, Item);
    FTick := Assigned(FTicker);
  end;
end;

function THeaderStreamCoder.DecodeTo(Item: THeader): boolean;
begin
  FStream.Seek(Item.StartPos, soBeginning);
  if foMoved in Item.Flags then
    Result := CopyTo(Item.Link) = Item.Crc
  else
    Result := DecodeTo(Item.Link) = Item.Crc;

  if Result then
  begin
    FileSetAttr(Item.Link, Item.Attr);
    FileSetDate(Item.Link, Item.Attr);
  end;
end;

function THeaderStreamCoder.DecodeToNul(Item: THeader): boolean;
var
  Strm: TNulWriter;
begin
  Strm := TNulWriter.Create;

  FStream.Seek(Item.StartPos, soBeginning);
  if foMoved in Item.Flags then
    Result := CopyTo(Strm, Item.Size) = Item.Crc
  else
    Result := DecodeTo(Strm, Item.Size) = Item.Crc;

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
