{
  Copyright (c) 2013 Melchiorre Caruso.

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

   v0.8.0 build 1864 - 2013.02.16 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  // ---
  Bee_Files,
  Bee_Interface,
  Bee_Configuration,
  {$IFDEF cLib}
    Bee_LibLink;
  {$ELSE}
    Bee_Modeller;
  {$ENDIF}

type
  TArchiveProgressEvent = procedure(Value: longint) of object;

type
  { TStreamCoder class }

  TStreamCoder = class
  private
    FBuffer: array of byte;
    FCoder: pointer;
    FModeller: pointer;
    FOnProgressEvent: TArchiveProgressEvent;
    procedure DoProgress(Value: longint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetDictionaryLevel(Value: longint);
    procedure SetCompressionTable(const Table: string);
    procedure FreshModeller(SolidCompression: boolean);
  public
    property OnProgress: TArchiveProgressEvent read FOnProgressEvent write FOnProgressEvent;
  end;

  { TStreamEncoder class }

  TStreamEncoder = class(TStreamCoder)
  private
    FStream: TFileWriter;
  public
    constructor Create(Stream: TFileWriter);
    destructor Destroy; override;
    function Copy  (Stream: TFileReader; const Size: int64): int64;
    function Encode(Stream: TFileReader; const Size: int64): int64;
  end;

  { TStreamDecoder class }

  TStreamDecoder = class(TStreamCoder)
  private
    FStream: TFileReader;
  public
    constructor Create(Stream: TFileReader);
    destructor Destroy; override;
    function Copy  (Stream: TFileWriter; const Size: int64): int64;
    function Decode(Stream: TFileWriter; const Size: int64): int64;
  end;

implementation

uses
  Bee_Crc, Bee_BufStream;

/// TStreamCoder class

constructor TStreamCoder.Create;
begin
  inherited Create;
  SetLength(FBuffer, 0);
end;

destructor TStreamCoder.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

procedure TStreamCoder.DoProgress(Value: longint);
begin
  if Assigned(FOnProgressEvent) then
    FOnProgressEvent(Value);
end;

procedure TStreamCoder.SetDictionaryLevel(Value: longint);
begin
  BaseCoder_SetDictionary(FModeller, Value);
end;

procedure TStreamCoder.SetCompressionTable(const Table: string);
begin
  if Length(Table) = High(TTableParameters) then
  begin
    BaseCoder_SetTable(FModeller, @Table[1]);
  end;
end;

procedure TStreamCoder.FreshModeller(SolidCompression: boolean);
begin
  if SolidCompression = FALSE then
    BaseCoder_FreshFlexible(FModeller)
  else
    BaseCoder_FreshSolid(FModeller);
end;

/// TStreamEncoder class

constructor TStreamEncoder.Create(Stream: TFileWriter);
begin
  inherited Create;
  FStream   := Stream;
  FCoder    := RangeEncoder_Create(FStream, @DoFlush);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor TStreamEncoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeEncoder_Destroy(FCoder);
  inherited Destroy;
end;

function TStreamEncoder.Copy(Stream: TFileReader; const Size: int64): int64;
var
  Count:  int64;
  Readed: longint;
  Writed: longint;
begin
  Result := 0;
  CRC    := 0;
  if Size > 0 then
  begin
    CRC := longword(-1);
    SetLength(FBuffer, GetCapacity(Size));
    Count  := Size div Length(FBuffer);
    while (Count <> 0) and (ExitStatus = esNoError) do
    begin
      Readed :=  Stream.Read (@FBuffer[0], Length(FBuffer));
      Writed := FStream.Write(@FBuffer[0], Readed);
      UpdateCrc32(CRC,        @FBuffer[0], Writed);
      Inc(Result, Writed);
      DoProgress(Writed);
      Dec(Count);
    end;
    Readed :=  Stream.Read (@FBuffer[0], Size mod Length(FBuffer));
    Writed := FStream.Write(@FBuffer[0], Readed);
    UpdateCRC32(CRC,        @FBuffer[0], Writed);
    Inc(Result, Writed);
    DoProgress(Writed);
  end;
end;

function TStreamEncoder.Encode(Stream: TFileReader; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Readed: longint;
begin
  Result := 0;
  CRC    := 0;
  if Size > 0 then
  begin
    CRC := longword(-1);
    RangeEncoder_StartEncode(FCoder);
    SetLength(FBuffer, GetCapacity(Size));
    Count  := Size div Length(FBuffer);
    while (Count <> 0) and (ExitStatus = esNoError) do
    begin
      Readed := Stream.Read      (@FBuffer[0], Length(FBuffer));
      BaseCoder_Encode(FModeller, @FBuffer[0], Readed);
      UpdateCrc32(CRC,            @FBuffer[0], Readed);
      Inc(Result, Readed);
      DoProgress(Readed);
      Dec(Count);
    end;
    Readed := Stream.Read      (@FBuffer[0], Size mod Length(FBuffer));
    BaseCoder_Encode(FModeller, @FBuffer[0], Readed);
    UpdateCRC32(CRC,            @FBuffer[0], Readed);
    Inc(Result, Readed);
    DoProgress(Readed);
    RangeEncoder_FinishEncode(FCoder);
  end;
end;

  { TStreamDecoder class }

constructor TStreamDecoder.Create(Stream: TFileReader);
begin
  inherited Create;
  FStream   := Stream;
  FCoder    := RangeDecoder_Create(FStream, @DoFill);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor TStreamDecoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeDecoder_Destroy(FCoder);
  inherited Destroy;
end;

function TStreamDecoder.Copy(Stream: TFileWriter; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Readed: longint;
  Writed: longint;
begin
  Result := 0;
  CRC    := 0;
  if Size > 0 then
  begin
    CRC := longword(-1);
    SetLength(FBuffer, GetCapacity(Size));
    Count  := Size div Length(FBuffer);
    while (Count <> 0) and (ExitStatus = esNoError) do
    begin
      Readed := FStream.Read (@FBuffer[0], Length(FBuffer));
      Writed :=  Stream.Write(@FBuffer[0], Readed);
      UpdateCrc32(CRC,        @FBuffer[0], Writed);
      Inc(Result, Writed);
      DoProgress(Writed);
      Dec(Count);
    end;
    Readed := FStream.Read (@FBuffer[0], Size mod Length(FBuffer));
    Writed :=  Stream.Write(@FBuffer[0], Readed);
    UpdateCRC32(CRC,        @FBuffer[0], Writed);
    Inc(Result, Writed);
    DoProgress(Writed);
  end;
end;

function TStreamDecoder.Decode(Stream: TFileWriter; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Writed: longint;
begin
  Result := 0;
  CRC    := 0;
  if Size > 0 then
  begin
    CRC := longword(-1);
    RangeDecoder_StartDecode(FCoder);
    SetLength(FBuffer, GetCapacity(Size));
    Count  := Size div Length(FBuffer);
    while (Count <> 0) and (ExitStatus = esNoError) do
    begin
      BaseCoder_Decode(FModeller, @FBuffer[0], Length(FBuffer));
      Writed := Stream.Write(     @FBuffer[0], Length(FBuffer));
      UpdateCrc32(CRC,            @FBuffer[0], Writed);
      Inc(Result, Writed);
      DoProgress(Writed);
      Dec(Count);
    end;
    BaseCoder_Decode(FModeller, @FBuffer[0], Size mod Length(FBuffer));
    Writed := Stream.Write(     @FBuffer[0], Size mod Length(FBuffer));
    UpdateCRC32(CRC,            @FBuffer[0], Writed);
    Inc(Result, Writed);
    DoProgress(Writed);
    RangeDecoder_FinishDecode(FCoder);
  end;
end;

end.
