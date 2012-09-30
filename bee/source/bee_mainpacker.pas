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
    FStream: TStream;
    FOnProgressEvent: TProgressEvent;
    FCoder: pointer;
    FModeller: pointer;
    FDictionaryLevel: longint;
    FTableParameters: TTableParameters;
    FTear: boolean;
  private
    procedure SetDictionaryLevel(Value: longint);
    procedure SetTableParameters(const Value: TTableParameters);
    procedure SetTear(Value: boolean);
  private
    function DoProgress(Value: longint): boolean;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
  public
    property DictionaryLevel: longint
      read FDictionaryLevel write SetDictionaryLevel;
    property TableParameters: TTableParameters
      read FTableParameters write SetTableParameters;
    property Tear: boolean read FTear write FTear;
    property OnProgress: TProgressEvent
      read FOnProgressEvent write FOnProgressEvent;
    property Stream: TStream read FStream write FStream;
  end;

  { THeaderEncoder class }

  THeaderEncoder = class(THeaderCoder)
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function Copy  (Stream: TStream; const Size: int64):                    int64; overload;
    function Copy  (Stream: TStream; const Size: int64; var CRC: longword): int64; overload;
    function Encode(Stream: TStream; const Size: int64; var CRC: longword): int64;
  end;

  { THeaderDecoder class }

  THeaderDecoder = class(THeaderCoder)
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function Copy  (Stream: TStream; const Size: int64; var CRC: longword): int64;
    function Decode(Stream: TStream; const Size: int64; var CRC: longword): int64;
  end;

implementation

uses
  Bee_Crc,
  Bee_Files;

/// THeaderCoder class

constructor THeaderCoder.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
end;

destructor THeaderCoder.Destroy;
begin
  inherited Destroy;
end;

function THeaderCoder.DoProgress(Value: longint): boolean;
begin
  Result := TRUE;
  if Assigned(FOnProgressEvent) then
  begin
    Result := FOnProgressEvent(Value);
  end;
end;

procedure THeaderCoder.SetDictionaryLevel(Value: longint);
begin
  FDictionaryLevel := Value;
  BaseCoder_SetDictionary(FModeller, FDictionaryLevel);
end;

procedure THeaderCoder.SetTableParameters(const Value: TTableParameters);
begin
  FTableParameters := Value;
  BaseCoder_SetTable(FModeller, @FTableParameters);
end;

procedure THeaderCoder.SetTear(Value: boolean);
begin
  FTear := Value;
  if FTear then
    BaseCoder_FreshFlexible(FModeller)
  else
    BaseCoder_FreshSolid(FModeller);
end;

/// THeaderEncoder class

constructor THeaderEncoder.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FCoder    := RangeEncoder_Create(FStream, @DoFlush);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor THeaderEncoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeEncoder_Destroy(FCoder);
  inherited Destroy;
end;

function THeaderEncoder.Copy(Stream: TStream; const Size: int64): int64;
var
  Count:  int64;
  Readed: longint;
  Writed: longint;
  Buffer: array[0..$FFFF] of byte;
begin
  Result := 0;
  Count  := Size div SizeOf(Buffer);
  while Count <> 0 do
  begin
    Readed :=  Stream.Read (Buffer, SizeOf(Buffer));
    Writed := FStream.Write(Buffer, Readed);
    Inc(Result, Writed);
    Dec(Count);

    if DoProgress(Writed) = FALSE then Exit;
  end;
  Readed :=  Stream.Read (Buffer, Size mod SizeOf(Buffer));
  Writed := FStream.Write(Buffer, Readed);
  Inc(Result, Writed);

  DoProgress(Writed);
end;

function THeaderEncoder.Copy(Stream: TStream; const Size: int64; var CRC: longword): int64;
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

function THeaderEncoder.Encode(Stream: TStream; const Size: int64; var CRC: longword): int64;
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

constructor THeaderDecoder.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FCoder    := RangeDecoder_Create(FStream, @DoFill);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor THeaderDecoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeDecoder_Destroy(FCoder);
  inherited Destroy;
end;

function THeaderDecoder.Copy(Stream: TStream; const Size: int64; var CRC: longword): int64;
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

function THeaderDecoder.Decode(Stream: TStream; const Size: int64; var CRC: longword): int64;
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
