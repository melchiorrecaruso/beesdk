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
  Bee_Headers,
  Bee_Interface,
  Bee_StreamCoder;

type

  { THeaderEncoder class }

  THeaderEncoder = class(TFileStreamEncoder)
  public
    procedure Initialize(Item: THeader);
    function Copy(Strm: TStream; const Size: int64; Item: THeader): boolean; overload;
    function Write(Strm: TStream; const Size: int64; Item: THeader): boolean; overload;
    function Write(Item: THeader): boolean; overload;
  end;

  { THeaderDecoder class }

  THeaderDecoder = class(TFileStreamDecoder)
  public
    procedure Initialize(Item: THeader);
    function Write(Item: THeader): boolean;
    function WriteToNul(Item: THeader): boolean;
  end;

implementation

uses
  SysUtils;

{ THeaderEncoder class }

function THeaderEncoder.Copy(Strm: TStream; const Size: int64; Item: THeader): boolean;
begin
  Strm.Seek(Item.StartPos, soBeginning);
  Item.StartPos := FStream.Seek(0, soCurrent);
  begin
    Result := Copy(Strm, Size) = Size;
  end;
  Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;
end;

function THeaderEncoder.Write(Strm: TStream; const Size: int64; Item: THeader): boolean;
begin
  Strm.Seek(Item.StartPos, soBeginning);
  Item.StartPos := FStream.Seek(0, soCurrent);
  if foMoved in Item.Flags then
    Item.Size := Copy(Strm, Size, Item.Crc)
  else
    Item.Size := Write(Strm, Size, Item.Crc);
  Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;

  // optimize compression ...
  if Item.PackedSize > Item.Size then
  begin
    FStream.Size := Item.StartPos;
    Include(Item.Flags, foMoved);
    Include(Item.Flags, foTear);
    Initialize(Item);

    FTick := False;
    Item.Size := CopyFrom(Strm, Size, Item.Crc);
    Item.PackedSize := Item.Size;
    FTick := Assigned(FTicker);
  end;
  Result := Item.Size <> -1;
end;

function THeaderEncoder.Encode(Item: THeader): boolean;
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

procedure THeaderEncoder.Initialize(Item: THeader);
begin
  if foDictionary in Item.Flags then FPPM.SetDictionary(Item.Dictionary);
  if foTable      in Item.Flags then FPPM.SetTable     (Item.Table);
  if foTear       in Item.Flags then
    FPPM.FreshFlexible
  else
    FPPM.FreshSolid;
end;

{ THeaderDecoder class }

function THeaderDecoder.Decode(Item: THeader): boolean;
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

function THeaderDecoder.Test(Item: THeader): boolean;
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

procedure THeaderDecoder.InitializeCoder(Item: THeader);
begin
  if foDictionary in Item.Flags then FPPM.SetDictionary(Item.Dictionary);
  if foTable      in Item.Flags then FPPM.SetTable     (Item.Table);
  if foTear       in Item.Flags then
    FPPM.FreshFlexible
  else
    FPPM.FreshSolid;
end;

end.
