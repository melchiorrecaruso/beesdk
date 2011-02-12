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
    function Copy(Strm: TStream; const Size: int64): int64; overload;
    function Copy(Strm: TStream; const Size: int64; var CRC: longword): int64; overload;
    function Write(Item: THeader; Strm: TStream; const Size: int64): boolean; overload;
  public
    procedure Initialize(Item: THeader);
    function CopyFrom(Item: THeader; Strm: TStream): boolean; // from Archive
    function WriteFrom(Item: THeader; Strm: TStream): boolean;// from Swap
    function Write(Item: THeader): boolean; overload;         // from File
  end;

  { THeaderDecoder class }

  THeaderDecoder = class(TStreamDecoder)
  private
    function Copy(Strm: TStream; const Size: int64; var CRC: longword): int64;
  public
    procedure Initialize(Item: THeader);
    function ReadTo(Item: THeader; Strm: TStream): boolean; // to Swap
    function ReadToNul(Item: THeader): boolean;             // to Nul
    function Read(Item: THeader): boolean; overload;        // to File
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

  function THeaderEncoder.Copy(Strm: TStream; const Size: int64): int64;
   var
     Symbol: byte;
   begin
     Result := 0;
     while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
     begin
       FStrm.Write(Symbol, 1);
       Inc(Result);
       if (Result and DefaultUserAbortEventStepSize = 0)
         and Assigned(FOnUserAbortEvent)
           and FOnUserAbortEvent then Break;
     end;
   end;

  function THeaderEncoder.Copy(Strm: TStream; const Size: int64; var CRC: longword): int64;
  var
    Symbol: byte;
  begin
    Result := 0;
    CRC    := longword(-1);
    while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
    begin
      FStrm.Write(Symbol, 1);
      UpdCrc32(CRC, Symbol);
      Inc(Result);
      if (Result and DefaultUserAbortEventStepSize = 0)
        and Assigned(FOnUserAbortEvent)
          and FOnUserAbortEvent then Break;
    end;
  end;

  function THeaderEncoder.Write(Item: THeader; Strm: TStream; const Size: int64): boolean;
  var
    StartPos: int64;
  begin
         StartPos :=  Strm.Seek(0, soCurrent);
    Item.StartPos := FStrm.Seek(0, soCurrent);
    case foMoved in Item.Flags of
      True:  Item.Size := Copy (Strm, Size, Item.Crc);
      False: Item.Size := Write(Strm, Size, Item.Crc);
    end;
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

  function THeaderEncoder.WriteFrom(Item: THeader; Strm: TStream): boolean;
  begin
    Strm.Seek(Item.StartPos, soBeginning);
    Result := Write(Item, Strm, Item.Size);
  end;

  function THeaderEncoder.CopyFrom(Item: THeader; Strm: TStream): boolean;
  var
   Symbol: byte;
   begin
      Strm.Seek(Item.StartPos, soBeginning);
      Item.StartPos := FStrm.Seek(0, soCurrent);
      Result := Copy(Strm, Item.PackedSize) = Item.PackedSize;
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

  function THeaderDecoder.Copy(Strm: TStream; const Size: int64; var CRC: longword): int64;
  var
    Symbol: byte;
  begin
    Result := 0;
    CRC := longword(-1);
    while (Result < Size) and (FStrm.Read(Symbol, 1) = 1) do
    begin
      Strm.Write(Symbol, 1);
      UpdCrc32(CRC, Symbol);
      Inc(Result);
      if (Result and $FFFF = 0)
        and Assigned(FOnUserAbortEvent)
          and FOnUserAbortEvent then Break;
    end;
  end;

  function THeaderDecoder.ReadTo(Item: THeader; Strm: TStream): boolean;
  var
    CRC: longword;
  begin
    FStrm.Seek(Item.StartPos, soBeginning);
    Item.StartPos := Strm.Seek(0, soCurrent);
    case foMoved in Item.Flags of
      True:  Result := Copy(Strm, Item.Size, CRC) = Item.Size;
      False: Result := Read(Strm, Item.Size, CRC) = Item.Size;
    end;
    if Result then Result := Item.Crc = CRC;
  end;

  function THeaderDecoder.ReadToNul(Item: THeader): boolean;
  var
    CRC: longword;
    Strm: TNulWriter;
  begin
    Strm := TNulWriter.Create;
    FStrm.Seek(Item.StartPos, soBeginning);
    case foMoved in Item.Flags of
      True:  Result := Copy(Strm, Item.Size, CRC) = Item.Size;
      False: Result := Read(Strm, Item.Size, CRC) = Item.Size;
    end;
    Strm.Free;

    if Result then Result := Item.Crc = CRC;
  end;

  function THeaderDecoder.Read(Item: THeader): boolean;
  var
    CRC: longword;
    Strm: TFileWriter;
  begin
    Result := False;
    Strm   := CreateTFileWriter(Item.ExtName, fmCreate);
    if Assigned(Strm) then
    begin
      FStrm.Seek(Item.StartPos, soBeginning);
      case foMoved in Item.Flags of
        True:  Result := Copy(Strm, Item.Size, CRC) = Item.Size;
        False: Result := Read(Strm, Item.Size, CRC) = Item.Size;
      end;
      if Result then Result := Item.Crc = CRC;

      if Result then
      begin
        FileSetAttr(Item.ExtName, Item.Attr);
        FileSetDate(Item.ExtName, Item.Time);
      end;
      Strm.Free;
    end;
  end;

end.
