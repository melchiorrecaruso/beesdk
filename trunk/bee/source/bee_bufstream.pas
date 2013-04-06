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

    Buffered stream classes.

  Modifyed:

    v0.8.0 build 1913 - 2013.03.23 by Melchiorre Caruso.
}

unit Bee_BufStream;

{$I compiler.inc}

interface

uses
  Bee_Crc,
  Bee_BlowFish,
  Bee_Interface;

type
  { TBufStream abstract class }

  TBufStream = class(TObject)
  protected
    FHandle: THandle;
  public
    constructor Create(Handle: THandle);
    destructor Destroy; override;
    function Read(Data: PByte; Count: longint): longint; virtual; abstract;
    function Write(Data: PByte; Count: longint): longint; virtual; abstract;
    function Seek(const Offset: int64; Origin: longint): int64; virtual; abstract;
  end;

  { TBaseCoder abstract class }

  TBaseCoder = class
  private
    FStream: TBufStream;
  public
    constructor Create(Stream: TBufStream);
    procedure SetCompressionLevel(Value: longint); virtual abstract;
    procedure SetCompressionLevelAux(Value: longint); virtual abstract;
    procedure SetCompressionFilter(const Filter: string); virtual abstract;
    procedure SetCompressionFilterAux(const Filter: string); virtual abstract;
    procedure SetCompressionBlock(const Value: int64); virtual abstract;

    procedure Initialize; virtual abstract;
    procedure Finalize; virtual abstract;
    function  Encode(Data: PByte; Count: longint): longint; virtual abstract;
    function  Decode(Data: PByte; Count: longint): longint; virtual abstract;
  end;

  { TNulCoder classes }

  TNulCoder = class(TBaseCoder)
  public
    procedure SetCompressionLevel(Value: longint); override;
    procedure SetCompressionLevelAux(Value: longint); override;
    procedure SetCompressionFilter(const Filter: string); override;
    procedure SetCompressionFilterAux(const Filter: string); override;
    procedure SetCompressionBlock(const Value: int64); override;

    procedure Initialize; override;
    procedure Finalize; override;
  end;

  TNulEncoder = class(TNulCoder)
  public
    function Encode(Data: PByte; Count: longint): longint; override;
  end;

  TNulDecoder = class(TNulCoder)
  public
    function Decode(Data: PByte; Count: longint): longint; override;
  end;

  { TBeeCoder classes }

  TBeeCoder = class(TBaseCoder)
  private
    FCoder: pointer;
    FModeller: pointer;
  public
    procedure SetCompressionLevel(Value: longint); override;
    procedure SetCompressionLevelAux(Value: longint); override;
    procedure SetCompressionFilter(const Filter: string); override;
    procedure SetCompressionFilterAux(const Filter: string); override;
    procedure SetCompressionBlock(const Value: int64); override;
  end;

  TBeeEncoder = class(TBeeCoder)
  public
    constructor Create(Stream: TBufStream);
    destructor Destroy; override;

    procedure Initialize; override;
    procedure Finalize; override;

    function Encode(Data: PByte; Count: longint): longint; override;
  end;

  TBeeDecoder = class(TBeeCoder)
  public
    constructor Create(Stream: TBufStream);
    destructor Destroy; override;

    procedure Initialize; override;
    procedure Finalize; override;

    function Decode(Data: PByte; Count: longint): longint; override;
  end;

  TCoderAlgorithm = (caCopy, caBee);

  { TCustomBufStream class }

  TCustomBufStream = class(TBufStream)
  private
    FBuffer: TBuffer;
    FHash: TBaseHash;
    FCipher: TBaseCipher;
    FCoder: TBaseCoder;
  public
    constructor Create(Handle: THandle);
    destructor Destroy; override;

    procedure SetCoder(Algorithm: TCoderAlgorithm); virtual abstract;
    procedure SetCompressionLevel(Value: longint);
    procedure SetCompressionLevelAux(Value: longint);
    procedure SetCompressionFilter(const Filter: string);
    procedure SetCompressionFilterAux(const Filter: string);
    procedure SetCompressionBlock(const Value: int64);
    procedure InitializeCoder;
    procedure FinalizeCoder;

    procedure SetCipher(Algorithm: TCipherAlgorithm; const Key: string);
    procedure SetHash(Algorithm: THashAlgorithm);
    function GetHashDigest: string;
  end;

  { TReadBufStream class }

  TReadBufStream = class(TCustomBufStream)
  private
    FBufferIndex: longint;
    FBufferSize: longint;
  protected
    procedure ClearBuffer;
    procedure FillBuffer;
  public
    constructor Create(Handle: THandle);
    function Read(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64; override;

    procedure SetCoder(Algorithm: TCoderAlgorithm); override;
    function Decode(Data: PByte; Count: longint): longint;
  end;

  { TWriteBufStream class }

  TWriteBufStream = class(TCustomBufStream)
  private
    FBufferIndex: longint;
  protected
    procedure ClearBuffer;
    procedure FlushBuffer;
  public
    constructor Create(Handle: THandle);
    function Write(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64; override;

    procedure SetCoder(Algorithm: TCoderAlgorithm); override;
    function Encode(Data: PByte; Count: longint): longint;
  end;

  procedure DoFill (Stream: pointer; Data: pointer; Size: longint);
    {$IFDEF cLIB} cdecl; {$ENDIF}
  procedure DoFlush(Stream: pointer; Data: pointer; Size: longint);
    {$IFDEF cLIB} cdecl; {$ENDIF}

implementation

uses
  Math,
  SysUtils,
  Bee_Common,
  {$IFDEF cLib}
    Bee_LibLink;
  {$ELSE}
    Bee_Modeller;
  {$ENDIF}

procedure DoFill(Stream: pointer; Data: pointer; Size: longint);
begin
  TBufStream(Stream).Read(Data, Size);
end;

procedure DoFlush(Stream: pointer; Data: pointer; Size: longint);
begin
  TBufStream(Stream).Write(Data, Size);
end;

/// TBufStream abstract class

constructor TBufStream.Create(Handle: THandle);
begin
  inherited Create;
  FHandle := Handle;
end;

destructor TBufStream.Destroy;
begin
  inherited Destroy;
end;

/// TBaseCoder abstract class

constructor TBaseCoder.Create(Stream: TBufStream);
begin
  inherited Create;
  FStream := Stream;
end;

/// TNulCoder class

procedure TNulCoder.SetCompressionLevel(Value: longint);
begin
  // nothing to do
end;

procedure TNulCoder.SetCompressionLevelAux(Value: longint);
begin
  // nothing to do
end;

procedure TNulCoder.SetCompressionFilter(const Filter: string);
begin
  // nothing to do
end;

procedure TNulCoder.SetCompressionFilterAux(const Filter: string);
begin
  // nothing to do
end;

procedure TNulCoder.SetCompressionBlock(const Value: int64);
begin
  // nothing to do
end;

procedure TNulCoder.Initialize;
begin
  // nothing to do
end;

procedure TNulCoder.Finalize;
begin
  // nothing to do
end;

/// TNulEncoder class

function TNulEncoder.Encode(Data: PByte; Count: longint): longint;
begin
  Result := FStream.Write(Data, Count);
end;

/// TNulEncoder class

function TNulDecoder.Decode(Data: PByte; Count: longint): longint;
begin
  Result := FStream.Read(Data, Count);
end;

/// TBeeCoder class

procedure TBeeCoder.SetCompressionLevel(Value: longint);
begin
  // nothing to do
end;

procedure TBeeCoder.SetCompressionLevelAux(Value: longint);
begin
  BaseCoder_SetDictionary(FModeller, Value);
end;

procedure TBeeCoder.SetCompressionFilter(const Filter: string);
var
  Table: array of byte;
begin
  SetLength(Table, Length(Filter) div 2);
  if HexToData(Filter, Table[0], Length(Table)) then
  begin
    BaseCoder_SetTable(FModeller, @Table[0]);
  end;
  SetLength(Table, 0);
end;

procedure TBeeCoder.SetCompressionFilterAux(const Filter: string);
begin
  // nothing to do
end;

procedure TBeeCoder.SetCompressionBlock(const Value: int64);
begin
  if Value = 0 then
    BaseCoder_FreshFlexible(FModeller)
  else
    BaseCoder_FreshSolid(FModeller);
end;

/// TBeeEncoder class

constructor TBeeEncoder.Create(Stream: TBufStream);
begin
  inherited Create(Stream);
  FCoder    := RangeEncoder_Create(FStream, @DoFlush);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor TBeeEncoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeEncoder_Destroy(FCoder);
  inherited Destroy;
end;

procedure TBeeEncoder.Initialize;
begin
  RangeEncoder_StartEncode(FCoder);
end;

procedure TBeeEncoder.Finalize;
begin
  RangeEncoder_FinishEncode(FCoder);
end;

function TBeeEncoder.Encode(Data: PByte; Count: longint): longint;
begin
  Result := BaseCoder_Encode(FModeller, Data, Count);
end;

/// TBeeDecoder class

constructor TBeeDecoder.Create(Stream: TBufStream);
begin
  inherited Create(Stream);
  FCoder    := RangeDecoder_Create(FStream, @DoFill);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor TBeeDecoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeDecoder_Destroy(FCoder);
  inherited Destroy;
end;

procedure TBeeDecoder.Initialize;
begin
  RangeDecoder_StartDecode(FCoder);
end;

procedure TBeeDecoder.Finalize;
begin
  RangeDecoder_FinishDecode(FCoder);
end;

function TBeeDecoder.Decode(Data: PByte; Count: longint): longint;
begin
  Result := BaseCoder_Decode(FModeller, Data, Count);
end;

/// TCustomBufCoder class

constructor TCustomBufStream.Create(Handle: THandle);
begin
  inherited Create(Handle);
  FHash   := nil;
  FCipher := nil;
  FCoder  := TBaseCoder.Create(Self);
end;

destructor TCustomBufStream.Destroy;
begin
  if Assigned(FHash  ) then FreeAndNil(FHash  );
  if Assigned(FCipher) then FreeAndNil(FCipher);
  if Assigned(FCoder ) then FreeAndNil(FCoder );
  inherited Destroy;
end;

procedure TCustomBufStream.SetCompressionLevel(Value: longint);
begin
  FCoder.SetCompressionLevel(Value);
end;

procedure TCustomBufStream.SetCompressionLevelAux(Value: longint);
begin
  FCoder.SetCompressionLevelAux(Value);
end;

procedure TCustomBufStream.SetCompressionFilter(const Filter: string);
begin
  FCoder.SetCompressionFilter(Filter);
end;

procedure TCustomBufStream.SetCompressionFilterAux(const Filter: string);
begin
  FCoder.SetCompressionFilterAux(Filter);
end;

procedure TCustomBufStream.SetCompressionBlock(const Value: int64);
begin
  FCoder.SetCompressionBlock(Value);
end;

procedure TCustomBufStream.InitializeCoder;
begin
  FCoder.Initialize;
end;

procedure TCustomBufStream.FinalizeCoder;
begin
  FCoder.Finalize;
end;

procedure TCustomBufStream.SetHash(Algorithm: THashAlgorithm);
begin
  if Assigned(FHash) then
    FreeAndNil(FHash);
  case Algorithm of
    haCRC32: FHash := TCRC32Hash.Create;
    haCRC64: FHash := TCRC64Hash.Create;
    haSHA1:  FHash := TSHA1Hash.Create;
    else     FHash := nil;
  end;
  if Assigned(FHash) then
    FHash.Initialize;
end;

procedure TCustomBufStream.SetCipher(Algorithm: TCipherAlgorithm; const Key: string);
begin
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  case Algorithm of
    caBlowFish: FCipher := TBlowFishCipher.Create(Key);
    else        FCipher := nil;
  end;
end;

function TCustomBufStream.GetHashDigest: string;
begin
  if Assigned(FHash) then
    Result := FHash.Finalize
  else
    Result := '';
end;

/// TReadBufStream class

constructor TReadBufStream.Create(Handle: THandle);
begin
  inherited Create(Handle);
  ClearBuffer;
end;

procedure TReadBufStream.ClearBuffer;
begin
  FBufferIndex := 0;
  FBufferSize  := 0;
end;

procedure TReadBufStream.FillBuffer;
begin
  FBufferIndex := 0;
  FBufferSize  := FileRead(FHandle, FBuffer[0], SizeOf(FBuffer));
  if FBufferSize > -1 then
  begin
    if Assigned(FCipher) then
      FCipher.Decrypt(FBuffer, FBufferSize);
  end else
  begin
    SetExitStatus(esFillStreamError);
    FBufferSize := 0;
  end;
end;

function TReadBufStream.Read(Data: PByte; Count: longint): longint;
var
  I: longint;
begin
  Result := 0;
  repeat
    if FBufferIndex = FBufferSize then
    begin
      FillBuffer;
      if FBufferSize = 0 then Break;
    end;
    I := Min(Count - Result, FBufferSize - FBufferIndex);

    Move(FBuffer[FBufferIndex], Data[Result], I);
    Inc(FBufferIndex, I);
    Inc(Result, I);
  until Result = Count;

  if Assigned(FHash) then
    FHash.Update(Data, Result);
end;

function TReadBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  FBufferIndex := 0;
  FBufferSize  := 0;
  Result       := FileSeek(FHandle, Offset, Origin);
end;

procedure TReadBufStream.SetCoder(Algorithm: TCoderAlgorithm);
begin
  if Assigned(FCoder) then
    case Algorithm of
      caBee:  if not(FCoder is TBeeDecoder) then FreeAndNil(FCoder);
      caCopy: if not(FCoder is TNulDecoder) then FreeAndNil(FCoder);
    end;

  if Assigned(FCoder) = FALSE then
    case Algorithm of
      caBee:  FCoder := TBeeDecoder.Create(Self);
      caCopy: FCoder := TNulDecoder.Create(Self);
    end;
end;

function TReadBufStream.Decode(Data: PByte; Count: longint): longint;
begin
  Result := FCoder.Decode(Data, Count);
end;

/// TWriteBufStream class

constructor TWriteBufStream.Create(Handle: THandle);
begin
  inherited Create(Handle);
  ClearBuffer;
end;

procedure TWriteBufStream.ClearBuffer;
begin
  FBufferIndex := 0;
end;

procedure TWriteBufStream.FlushBuffer;
begin
  if FBufferIndex > 0 then
  begin
    if Assigned(FCipher) then
      FBufferIndex := FCipher.Encrypt(FBuffer, FBufferIndex);

    if FBufferIndex <> FileWrite(FHandle, FBuffer[0], FBufferIndex)  then
      SetExitStatus(esFlushStreamError);
  end;
  FBufferIndex := 0;
end;

function TWriteBufStream.Write(Data: PByte; Count: longint): longint;
var
  I: longint;
begin
  if Assigned(FHash) then
    FHash.Update(Data, Count);

  Result := 0;
  repeat
    if FBufferIndex = SizeOf(FBuffer) then
    begin
      FlushBuffer;
      if ExitStatus <> esNoError then Break;
    end;
    I := Min(Count - Result, SizeOf(FBuffer) - FBufferIndex);

    Move(Data[Result], FBuffer[FBufferIndex], I);
    Inc(FBufferIndex, I);
    Inc(Result, I);
  until Result = Count;
end;

function TWriteBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  FBufferIndex := 0;
  Result       := FileSeek(FHandle, Offset, Origin);
end;

procedure TWriteBufStream.SetCoder(Algorithm: TCoderAlgorithm);
begin
  if Assigned(FCoder) then
    case Algorithm of
      caBee:  if not(FCoder is TBeeEncoder) then FreeAndNil(FCoder);
      caCopy: if not(FCoder is TNulEncoder) then FreeAndNil(FCoder);
    end;

  if Assigned(FCoder) = FALSE then
    case Algorithm of
      caBee:  FCoder := TBeeEncoder.Create(Self);
      caCopy: FCoder := TNulEncoder.Create(Self);
    end;
end;

function TWriteBufStream.Encode(Data: PByte; Count: longint): longint;
begin
  Result := FCoder.Encode(Data, Count);
end;

end.

