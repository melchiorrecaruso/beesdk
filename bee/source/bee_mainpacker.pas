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

  THeaderEncoder = class(TStreamEncoder)
  private
    function CopySilent(Strm: TStream; const Size: int64): int64;
  public
    procedure Initialize(Item: THeader);
    function Copy (Item: THeader; Strm: TStream; const Size: int64): boolean; overload;
    function Write(Item: THeader; Strm: TStream; const Size: int64): boolean; overload;
    function Write(Item: THeader): boolean; overload;
  end;

  { THeaderDecoder class }

  THeaderDecoder = class(TStreamDecoder)
  public
    procedure Initialize(Item: THeader);
    function Read(Item: THeader; Strm: TStream; const Size: int64): boolean; overload;
    function Read(Item: THeader): boolean;
  end;

implementation

uses
  SysUtils,
  Bee_Files,
  Bee_Crc;

  { THeaderEncoder class }

  procedure THeaderEncoder.Initialize(Item: THeader);
  begin
    if foDictionary in Item.Flags then DictionaryLevel := Item.Dictionary;
    if foTable      in Item.Flags then TableParameters := Item.Table;
    if foTear       in Item.Flags then FreshFlexible else FreshSolid;
  end;

  function THeaderEncoder.CopySilent(Strm: TStream; const Size: int64): int64;
  var
    Symbol: byte;
  begin
    Result := 0;
    while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
    begin
      FStrm.Write(Symbol, 1);
      Inc(Result);
    end;
  end;

  function THeaderEncoder.Write(Item: THeader; Strm: TStream; const Size: int64): boolean;
  var
    StartPos: int64;
  begin
         StartPos :=  Strm.Seek(0, soCurrent);
    Item.StartPos := FStrm.Seek(0, soCurrent);
    if foMoved in Item.Flags then
      Item.Size := Copy(Strm, Size, Item.Crc)
    else
      Item.Size := Write(Strm, Size, Item.Crc);
    Item.PackedSize := FStrm.Seek(0, soCurrent) - Item.StartPos;

    // optimize compression ...
    if Item.PackedSize > Item.Size then
    begin
      Strm.Seek(StartPos, soBeginning);
      FStrm.Size := Item.StartPos;

      Include(Item.Flags, foMoved);
      Include(Item.Flags, foTear);
      Initialize(Item);

      Item.PackedSize := CopySilent(Strm, Size);
    end;
    Result := Item.Size = Size;
  end;

  function THeaderEncoder.Write(Item: THeader): boolean;
  var
    Strm: TFileReader;
  begin
    Result := False;
    Strm   := CreateTFileReader(Item.ExtName, fmOpenRead);
    if Assigned(Strm) then
    begin
      Result := Write(Item, Strm, Strm.Size);
      Strm.Free;
    end;
  end;

  { TheaderDecoder class }

  procedure THeaderDecoder.Initialize(Item: THeader);
  begin
    if foDictionary in Item.Flags then DictionaryLevel := Item.Dictionary;
    if foTable      in Item.Flags then TableParameters := Item.Table;
    if foTear       in Item.Flags then FreshFlexible else FreshSolid;
  end;




















function TStreamDecoder.Copy(Strm: TStream; const Size: int64): int64;
var
  Symbol: byte;
begin
  Result := 0;
  while (Result < Size) and (FStream.Read(Symbol, 1) = 1) do
  begin
    Strm.Write(Symbol, 1);
    Inc(Result);
    if (Result and $FFFF = 0)
      and Assigned(FOnUserAbortEvent)
        and FOnUserAbortEvent then Break;
  end;
end;




{ TFileStreamEncoder class }

function TFileStreamEncoder.Copy(const FileName: string; var CRC: longword): int64;
var
  Strm: TFileReader;
begin
  Strm := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := Copy(Strm, Strm.Size, CRC);
    Strm.Free;
  end else
    Result := 0;
end;



{ TFileStreamDecoder class }

function TFileStreamDecoder.Copy(const FileName: string; const Size: int64; var CRC: longword): int64;
var
  Strm: TFileWriter;
begin
  Strm := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := Copy(Strm, Size, CRC);
    Strm.Free;
  end else
    Result := 0;
end;

function TFileStreamDecoder.Write(const FileName: string; const Size: int64; var CRC: longword): int64;
var
  Strm: TFileWriter;
begin
  Strm := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := Write(Strm, Size, CRC);
    Strm.Free;
  end else
    Result := 0;
end;












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
