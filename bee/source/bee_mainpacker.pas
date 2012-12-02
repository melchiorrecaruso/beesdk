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
  Bee_Interface,
  BeeLib_Configuration,
  {$IFDEF cppDLL}
    Bee_LibLink;
  {$ELSE}
    Bee_Modeller;
  {$ENDIF}

type
  TArchiveProgressEvent = procedure(Value: longint) of object;

type
  { THeaderCoder class }

  THeaderCoder = class
  private
    FBuffer: array of byte;
    FCoder: pointer;
    FModeller: pointer;
    FDictionaryLevel: longint;
    FCompressionTable: TTableParameters;
    FOnProgressEvent: TArchiveProgressEvent;
    procedure SetDictionaryLevel(Value: longint);
    procedure SetCompressionTable(const Value: TTableParameters);
    procedure DoProgress(Value: longint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreshModeller(SolidCompression: boolean);
  public
    property DictionaryLevel: longint read FDictionaryLevel write SetDictionaryLevel;
    property CompressionTable: TTableParameters read FCompressionTable write SetCompressionTable;
    property OnProgress: TArchiveProgressEvent read FOnProgressEvent write FOnProgressEvent;
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
  Bee_Crc, Bee_BufStream;

/// THeaderCoder class

constructor THeaderCoder.Create;
begin
  inherited Create;
  FBuffer := nil;
end;

destructor THeaderCoder.Destroy;
begin
  FBuffer := nil;
  inherited Destroy;
end;

procedure THeaderCoder.DoProgress(Value: longint);
begin
  if Assigned(FOnProgressEvent) then
    FOnProgressEvent(Value);
end;

procedure THeaderCoder.SetDictionaryLevel(Value: longint);
begin
  if Value <> FDictionaryLevel then
  begin
    FDictionaryLevel := Value;
    BaseCoder_SetDictionary(FModeller, FDictionaryLevel);
  end;
end;

procedure THeaderCoder.SetCompressionTable(const Value: TTableParameters);
begin
  FCompressionTable := Value;
  BaseCoder_SetTable(FModeller, @FCompressionTable);
end;

procedure THeaderCoder.FreshModeller(SolidCompression: boolean);
begin
  if SolidCompression = FALSE then
    BaseCoder_FreshFlexible(FModeller)
  else
    BaseCoder_FreshSolid(FModeller);
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
begin
  Result := 0;
  CRC    := longword(-1);
  if Size > 0 then
  begin
    SetLength(FBuffer, GetCapacity(Size));
    Count  := Size div Length(FBuffer);
    while (Count <> 0) and (ExitCode = 0) do
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
    FBuffer := nil;
  end;
end;

function THeaderEncoder.Encode(Stream: TFileReader; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Readed: longint;
begin
  Result := 0;
  CRC    := longword(-1);
  if Size > 0 then
  begin
    RangeEncoder_StartEncode(FCoder);
    SetLength(FBuffer, GetCapacity(Size));
    Count  := Size div Length(FBuffer);
    while (Count <> 0) and (ExitCode = 0) do
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
    FBuffer := nil;
    RangeEncoder_FinishEncode(FCoder);
  end;
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
begin
  Result := 0;
  CRC    := longword(-1);
  if Size > 0 then
  begin
    SetLength(FBuffer, GetCapacity(Size));
    Count  := Size div Length(FBuffer);
    while (Count <> 0) and (ExitCode = 0) do
    begin
      Readed := FStream.Read(@FBuffer[0], Length(FBuffer));
      Writed := Stream.Write(@FBuffer[0], Readed);
      UpdateCrc32(CRC,       @FBuffer[0], Writed);
      Inc(Result, Writed);
      DoProgress(Writed);
      Dec(Count);
    end;
    Readed := FStream.Read(@FBuffer[0], Size mod Length(FBuffer));
    Writed := Stream.Write(@FBuffer[0], Readed);
    UpdateCRC32(CRC,       @FBuffer[0], Writed);
    Inc(Result, Writed);
    DoProgress(Writed);
    FBuffer := nil;
  end;
end;

function THeaderDecoder.Decode(Stream: TFileWriter; const Size: int64; var CRC: longword): int64;
var
  Count:  int64;
  Writed: longint;
begin
  Result := 0;
  CRC    := longword(-1);
  if Size > 0 then
  begin
    RangeDecoder_StartDecode(FCoder);
    SetLength(FBuffer, GetCapacity(Size));
    Count  := Size div Length(FBuffer);
    while (Count <> 0) and (ExitCode = 0) do
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
    FBuffer := nil;
    RangeDecoder_FinishDecode(FCoder);
  end;
end;

end.
