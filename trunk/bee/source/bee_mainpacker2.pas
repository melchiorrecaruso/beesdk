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
  Bee_Configuration;

type

  { TStreamCoder class }

  TStreamCoder = class
  private
    FStream: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Copy  (Strm: TStream; const Size: int64): longword; virtual; abstract;
    function Encode(Strm: TStream; const Size: int64): longword; virtual; abstract;
    function Decode(Strm: TStream; const Size: int64): longword; virtual; abstract;
    procedure SetTable(const Value: TTableParameters);
    procedure SetDictionary(Value: byte);
    procedure FreshFlexible;
    procedure FreshSolid;
  end;

  { TStreamEncoder class }

  TStreamEncoder = class(TStreamCoder)
  public
    function Copy  (Strm: TStream; const Size: int64): longword; override;
    function Encode(Strm: TStream; const Size: int64): longword; override;
  end;

  { TStreamDecoder class }

  TStreamDecoder = class(TStreamCoder)
  public
    function Copy  (Strm: TStream; const Size: int64): longword; override;
    function Decode(Strm: TStream; const Size: int64): longword; override;
  end;

  TFileStreamCoder = class(TStreamCoder)
  public
    function Copy  (const FileName: string): longword; overload;
    function Encode(const FileName: string): longword; overload;
    function Decode(const FileName: string): longword; overload;
  end;

  THeaderStreamCoder = class(TFileStreamCoder)
  public
    procedure Copy  (const Item: THeader); overload;
    procedure Encode(const Item: THeader); overload;
    procedure Decode(const Item: THeader); overload;
    procedure SetTDF(const Item: THeader);
  end;


implementation

uses
  Bee_Crc,
  Bee_Files;

{ TStreamCoder class }

constructor TStreamCoder.Create(Stream: TStream);
begin
  FStream := Stream;
  FSecondaryCodec := TSecondaryEncoder.Create(FStream);
  FPPM := TBaseCoder.Create(FSecondaryCodec);
end;

destructor TStreamCoder.Destroy;
begin
  FPPM.Free;
  FSecondaryCodec.Free;
  FStream := nil;
end;

function TStreamCoder.Copy(SrcStrm: TStream; Size: int64): longword;
var
  Symbol: byte;
begin
  Result := longword(-1);
  while Size > 0 do
  begin
    SrcStrm.Read(Symbol, 1);
    FStream.Write(Symbol, 1);
    UpdCrc32(Result, Symbol);
    Dec(Size);
  end;
end;

function TStreamCoder.Encode(SrcStrm: TStream; Size: int64): longword;
var
  Symbol: byte;
begin
  Result := longword(-1);
  FSecondaryCodec.Start;
  while Size > 0 do
  begin
    SrcStrm.Read(Symbol, 1);
    FPPM.UpdateModel(Symbol);
    UpdCrc32(Result, Symbol);
    Dec(Size);
  end;
  FSecondaryCodec.Flush;
end;

function TStreamCoder.Decode(DstStrm: TStream; Size: int64): longword;
var
  Symbol: byte;
begin
  Result := longword(-1);
  FSecondaryCodec.Start;
  while Size > 0 do
  begin
    Symbol := FPPM.UpdateModel(0);
    DstStrm.Write(Symbol, 1);
    UpdCrc32(Result, Symbol);
    Dec(Size);
  end;
  FSecondaryCodec.Flush;
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

function TFileStreamCoder.Copy(const FileName: string): longword;
var
  SrcStrm: TFileReader;
begin
  SrcStrm := CreateTFileReader(FileName, fmOpenRead);
  if SrcStrm <> nil then
  begin
    Result := Copy(SrcStrm, SrcStrm.Size);
    SrcStrm.Free;
  end else
    Result := longword(-1);
end;

function TFileStreamCoder.Encode(const FileName: string): longword;
var
  SrcStrm: TFileReader;
begin
  SrcStrm := CreateTFileReader(FileName, fmOpenRead);
  if SrcStrm <> nil then
  begin
    Result := Encode(SrcStrm, SrcStrm.Size);
    SrcStrm.Free;
  end else
    Result := longword(-1);
end;

function TFileStreamCoder.Decode(const FileName: string): longword;
var
  DstStrm: TFileWriter;
begin
  DstStrm := CreateTFileWriter(FileName, fmCreate);
  if DstStrm <> nil then
  begin
    Result := Decode(DstStrm, DstStrm.Size);
    DstStrm.Free;
  end else
    Result := longword(-1);
end;

{ THeaderStreamCoder class }

procedure THeaderStreamCoder.Encode(const Item: THeader);
begin
  Item.StartPos   := FStream.Seek(0, soCurrent);
  if foMoved in Item.Flags then
    Item.Crc := Copy(Item.Link)
  else
    Item.Crc := Encode(Item.Link);
  Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;
end;

function THeaderStreamCoder.Decode(const Item: THeader): longword;
begin
  FStream.Seek(Item.StartPos, soFromBeginning);
  if foMoved in Item.Flags then

    Result := Copy
  else
    Result := Decode(Item.Link);


  ;
end;

procedure THeaderStreamCoder.SetTDF(const Item: THeader);
begin
  if foDictionary in Item.FileFlags then PPM.SetDictionary(Item.FileDictionary);
  if foTable      in Item.FileFlags then PPM.SetTable(Item.FileTable);
  if foTear       in Item.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;
end;

end.
