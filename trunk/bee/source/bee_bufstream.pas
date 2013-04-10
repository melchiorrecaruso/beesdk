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
  Bee_MainPacker,
  Bee_Interface;

type
  { TBufStream abstract class }

  TBufStream = class(TObject)
  protected
    FHandle: THandle;
  private
    FHash: TBaseHash;
    FHashStarted: boolean;
    FCipher: TBaseCipher;
    FCipherStarted: boolean;
    FCoder: TBaseCoder;
  public
    constructor Create(Handle: THandle);
    destructor Destroy; override;
    function Read(Data: PByte; Count: longint): longint; virtual; abstract;
    function Write(Data: PByte; Count: longint): longint; virtual; abstract;
    function Seek(const Offset: int64; Origin: longint): int64; virtual; abstract;

    procedure StartHash(Algorithm: THashAlgorithm);
    function  FinishHash: string;

    procedure StartCipher(Algorithm: TCipherAlgorithm; const Key: string); virtual;
    procedure FinishCipher;

    procedure StartCoder(Algorithm: TCoderAlgorithm); virtual abstract;
    procedure FinishCoder; virtual abstract;
    procedure SetCompressionLevel(Value: longint);
    procedure SetCompressionLevelAux(Value: longint);
    procedure SetCompressionFilter(const Value: string);
    procedure SetCompressionFilterAux(const Value: string);
    procedure SetCompressionBlock(const Value: int64);
  end;

  { TReadBufStream class }

  TReadBufStream = class(TBufStream)
  private
    FBuffer: TBuffer;
    FBufferIndex: longint;
    FBufferSize: longint;
  protected
    procedure ClearBuffer;
    procedure FillBuffer;
  public
    constructor Create(Handle: THandle);
    function Read(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64; override;

    procedure StartCipher(Algorithm: TCipherAlgorithm; const Key: string); override;
    procedure StartCoder(Algorithm: TCoderAlgorithm); override;
    procedure FinishCoder; override;

    function Decode(Data: PByte; Count: longint): longint;
  end;

  { TWriteBufStream class }

  TWriteBufStream = class(TBufStream)
  private
    FBuffer: TBuffer;
    FBufferIndex: longint;
  protected
    procedure ClearBuffer;
    procedure FlushBuffer;
  public
    constructor Create(Handle: THandle);
    function Write(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64; override;

    procedure StartCipher(Algorithm: TCipherAlgorithm; const Key: string); override;
    procedure StartCoder(Algorithm: TCoderAlgorithm); override;
    procedure FinishCoder; override;

    function Encode(Data: PByte; Count: longint): longint;
  end;

  { TNulBufStream }

  TNulBufStream = class(TWriteBufStream)
  public
    constructor Create;
    function Write(Data: PByte; Count: longint): longint;  override;
    function Seek(const Offset: int64; Origin: longint): int64; override;
  end;

  function DoFill (Stream: pointer; Data: PByte; Size: longint): longint; {$IFDEF cLIB} cdecl; {$ENDIF}
  function DoFlush(Stream: pointer; Data: PByte; Size: longint): longint; {$IFDEF cLIB} cdecl; {$ENDIF}

implementation

uses
  Math,
  SysUtils;

function DoFill(Stream: pointer; Data: PByte; Size: longint): longint; inline;
begin
  Result := TBufStream(Stream).Read(Data, Size);
end;

function DoFlush(Stream: pointer; Data: PByte; Size: longint): longint; inline;
begin
  Result := TBufStream(Stream).Write(Data, Size);
end;

/// TBufStream abstract class

constructor TBufStream.Create(Handle: THandle);
begin
  inherited Create;
  FHandle        := Handle;
  FHash          := TNulHash.Create;
  FHashStarted   := FALSE;
  FCipher        := TNulCipher.Create;
  FCipherStarted := FALSE;
  FCoder         := TStoreCoder.Create(Self);
end;

destructor TBufStream.Destroy;
begin
  FreeAndNil(FHash);
  FreeAndNil(FCipher);
  FreeAndNil(FCoder);
  inherited Destroy;
end;

procedure TBufStream.SetCompressionLevel(Value: longint);
begin
  FCoder.SetCompressionLevel(Value);
end;

procedure TBufStream.SetCompressionLevelAux(Value: longint);
begin
  FCoder.SetCompressionLevelAux(Value);
end;

procedure TBufStream.SetCompressionFilter(const Value: string);
begin
  FCoder.SetCompressionFilter(Value);
end;

procedure TBufStream.SetCompressionFilterAux(const Value: string);
begin
  FCoder.SetCompressionFilterAux(Value);
end;

procedure TBufStream.SetCompressionBlock(const Value: int64);
begin
  FCoder.SetCompressionBlock(Value);
end;

procedure TBufStream.StartHash(Algorithm: THashAlgorithm);
begin
  FreeAndNil(FHash);
  case Algorithm of
    haNul:   FHash := TNulHash.Create;
    haCRC32: FHash := TCRC32Hash.Create;
    haCRC64: FHash := TCRC64Hash.Create;
    haSHA1:  FHash := TSHA1Hash.Create;
    haMD5:   FHash := TMD5Hash.Create;
  end;
  FHash.Start;
  FHashStarted := Algorithm <> haNul;
end;

function TBufStream.FinishHash: string;
begin
  if FHashStarted then
    Result := FHash.Finish
  else
    Result := '';
  FHashStarted := FALSE;
end;

procedure TBufStream.StartCipher(Algorithm: TCipherAlgorithm; const Key: string);
begin
  FreeAndNil(FCipher);
  case Algorithm of
    caNul:      FCipher := TNulCipher.Create;
    caBlowFish: FCipher := TBlowFishCipher.Create(Key);
  end;
  FCipherStarted := Algorithm <> caNul;
end;

procedure TBufStream.FinishCipher;
begin
  FCipherStarted := FALSE;
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
     if FCipherStarted then
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

  if FHashStarted then
    FHash.Update(Data, Result);
end;

function TReadBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  FBufferIndex := 0;
  FBufferSize  := 0;
  Result       := FileSeek(FHandle, Offset, Origin);
end;

procedure TReadBufStream.StartCipher(Algorithm: TCipherAlgorithm; const Key: string);
begin
  ClearBuffer;
  inherited StartCipher(Algorithm, Key);
end;

procedure TReadBufStream.StartCoder(Algorithm: TCoderAlgorithm);
begin
  case Algorithm of
    caStore: if not(FCoder is TStoreCoder) then FreeAndNil(FCoder);
    caBee:   if not(FCoder is TBeeDecoder) then FreeAndNil(FCoder);
  end;

  if FCoder = nil then
    case Algorithm of
      caStore: FCoder := TStoreCoder.Create(Self);
      caBee:   FCoder := TBeeDecoder.Create(Self);
    end;
  FCoder.Start;
end;

procedure TReadBufStream.FinishCoder;
begin
  FCoder.Finish;
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
    if FCipherStarted then
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

  if FHashStarted then
    FHash.Update(Data, Result);
end;

function TWriteBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  FlushBuffer;
  Result := FileSeek(FHandle, Offset, Origin);
end;

procedure TWriteBufStream.StartCipher(Algorithm: TCipherAlgorithm; const Key: string);
begin
  FlushBuffer;
  inherited StartCipher(Algorithm, Key);
end;

procedure TWriteBufStream.StartCoder(Algorithm: TCoderAlgorithm);
begin
  case Algorithm of
    caBee:   if not(FCoder is TBeeEncoder) then FreeAndNil(FCoder);
    caStore: if not(FCoder is TStoreCoder) then FreeAndNil(FCoder);
  end;

  if FCoder = nil then
    case Algorithm of
      caBee:   FCoder := TBeeEncoder.Create(Self);
      caStore: FCoder := TStoreCoder.Create(Self);
    end;

  FCoder.Start;
end;

procedure TWriteBufStream.FinishCoder;
begin
  FCoder.Finish;
end;

function TWriteBufStream.Encode(Data: PByte; Count: longint): longint;
begin
  Result := FCoder.Encode(Data, Count);
end;

{ TNulBufStream class }

constructor TNulBufStream.Create;
begin
  inherited Create(THandle(-1));
end;

function TNulBufStream.Write(Data: PByte; Count: longint): longint;
begin
  Result := Count;
  if FHashStarted then
    FHash.Update(Data, Count);
end;

function TNulBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  Result := 0;
end;

end.

