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
  SysUtils,
  // ---
  Bee_Files,
  BeeLib_Configuration,
  {$IFDEF cppDLL}
    Bee_LibLink;
  {$ELSE}
    Bee_Modeller;
  {$ENDIF}

type
  TProgressEvent = function(Value: longint): boolean of object;

type
  { THeaderCoder class }

  THeaderCoder = class
  private
    FCoder: pointer;
    FModeller: pointer;
    FDictionaryLevel: longint;
    FCompressionTable: TTableParameters;
    FOnProgressEvent: TProgressEvent;
    procedure SetDictionaryLevel(Value: longint);
    procedure SetCompressionTable(const Value: TTableParameters);
    function DoProgress(Value: longint): boolean;
  public
    procedure FreshModeller(SolidCompression: boolean);
  public
    property DictionaryLevel: longint read FDictionaryLevel write SetDictionaryLevel;
    property CompressionTable: TTableParameters read FCompressionTable write SetCompressionTable;
    property OnProgress: TProgressEvent read FOnProgressEvent write FOnProgressEvent;
  end;

  { THeaderEncoder class }

  THeaderEncoder = class(THeaderCoder)
  private
    FStream: TFileWriter;
  public
    constructor Create(Stream: TFileWriter);
    destructor Destroy; override;
    function Copy  (Stream: TFileReader; const Size: int64; var CRC: longword): int64;
    function Encode(Stream: TFileReader; const Size: int64; var CRC: longword): int64;
  end;

  { THeaderDecoder class }

  THeaderDecoder = class(THeaderCoder)
  private
    FStream: TFileReader;
  public
    constructor Create(Stream: TFileReader);
    destructor Destroy; override;
    function Copy  (Stream: TFileWriter; const Size: int64; var CRC: longword): int64;
    function Decode(Stream: TFileWriter; const Size: int64; var CRC: longword): int64;
  end;

implementation

uses
  Bee_Crc;

/// THeaderCoder class

function THeaderCoder.DoProgress(Value: longint): boolean;
begin
  Result := TRUE;
  if Assigned(FOnProgressEvent) then
    Result := FOnProgressEvent(Value);
end;

procedure THeaderCoder.SetDictionaryLevel(Value: longint);
begin
  FDictionaryLevel := Value;
  BaseCoder_SetDictionary(FModeller, FDictionaryLevel);
end;

procedure THeaderCoder.SetCompressionTable(const Value: TTableParameters);
begin
  FCompressionTable := Value;
  BaseCoder_SetTable(FModeller, @FCompressionTable);
end;

procedure THeaderCoder.FreshModeller(SolidCompression: boolean);
begin
  if SolidCompression then
    BaseCoder_FreshSolid(FModeller)
  else
    BaseCoder_FreshFlexible(FModeller);
end;

/// THeaderEncoder class

constructor THeaderEncoder.Create(Stream: TFileWriter);
begin
  inherited Create;
  FStream   := Stream;
  FCoder    := RangeEncoder_Create(FStream, @DoFlush);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor THeaderEncoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeEncoder_Destroy(FCoder);
  inherited Destroy;
end;

function THeaderEncoder.Copy(Stream: TFileReader; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Readed: longint;
  Writed: longint;
  Buffer: array[0..$FFFF] of byte;
begin
  Result := 0;
  CRC    := longword(-1);
  Count  := Size div SizeOf(Buffer);
  while Count <> 0 do
  begin
    Readed :=  Stream.Read (Buffer, SizeOf(Buffer));
    Writed := FStream.Write(Buffer, Readed);
    UpdateCrc32(CRC, Buffer, Writed);
    Inc(Result, Writed);
    Dec(Count);

    if DoProgress(Writed) = FALSE then Exit;
  end;
  Readed :=  Stream.Read (Buffer, Size mod SizeOf(Buffer));
  Writed := FStream.Write(Buffer, Readed);
  UpdateCRC32(CRC, Buffer, Writed);
  Inc(Result, Writed);

  DoProgress(Writed);
end;

function THeaderEncoder.Encode(Stream: TFileReader; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Readed: longint;
  Buffer: array[0..$FFFF] of byte;
begin
  RangeEncoder_StartEncode(FCoder);

  Result := 0;
  CRC    := longword(-1);
  Count  := Size div SizeOf(Buffer);
  while Count <> 0 do
  begin
    Readed := Stream.Read(Buffer, SizeOf(Buffer));
    BaseCoder_Encode(FModeller, @Buffer, Readed);
    UpdateCrc32(CRC, Buffer, Readed);
    Inc(Result, Readed);
    Dec(Count);

    if DoProgress(Readed) = FALSE then Exit;
  end;
  Readed := Stream.Read(Buffer, Size mod SizeOf(Buffer));
  BaseCoder_Encode(FModeller, @Buffer, Readed);
  UpdateCRC32(CRC, Buffer, Readed);
  Inc(Result, Readed);
  DoProgress(Readed);

  RangeEncoder_FinishEncode(FCoder);
end;

  { TheaderDecoder class }

constructor THeaderDecoder.Create(Stream: TFileReader);
begin
  inherited Create;
  FStream   := Stream;
  FCoder    := RangeDecoder_Create(FStream, @DoFill);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor THeaderDecoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeDecoder_Destroy(FCoder);
  inherited Destroy;
end;

function THeaderDecoder.Copy(Stream: TFileWriter; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Readed: longint;
  Writed: longint;
  Buffer: array[0..$FFFF] of byte;
begin
  Result := 0;
  CRC    := longword(-1);
  Count  := Size div SizeOf(Buffer);
  while Count <> 0 do
  begin
    Readed := FStream.Read(Buffer, SizeOf(Buffer));
    Writed := Stream.Write(Buffer, Readed);
    UpdateCrc32(CRC, Buffer, Writed);
    Inc(Result, Writed);
    Dec(Count);

    if DoProgress(Writed) = FALSE then Break;
  end;
  Readed := FStream.Read(Buffer, Size mod SizeOf(Buffer));
  Writed := Stream.Write(Buffer, Readed);
  UpdateCRC32(CRC, Buffer, Writed);
  Inc(Result, Writed);

  DoProgress(Writed);
end;

function THeaderDecoder.Decode(Stream: TFileWriter; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Writed: longint;
  Buffer: array[0..$FFFF] of byte;
begin
  RangeDecoder_StartDecode(FCoder);

  Result := 0;
  CRC    := longword(-1);
  Count  := Size div SizeOf(Buffer);
  while Count <> 0 do
  begin
    BaseCoder_Decode(FModeller, @Buffer, SizeOf(Buffer));
    Writed := Stream.Write(Buffer, SizeOf(Buffer));
    UpdateCrc32(CRC, Buffer, Writed);
    Inc(Result, Writed);
    Dec(Count);

    if DoProgress(Writed) = FALSE then Exit;
  end;
  BaseCoder_Decode(FModeller, @Buffer, Size mod SizeOf(Buffer));
  Writed := Stream.Write(Buffer, Size mod SizeOf(Buffer));
  UpdateCRC32(CRC, Buffer, Writed);
  Inc(Result, Writed);
  DoProgress(Writed);

  RangeDecoder_FinishDecode(FCoder);
end;

end.
