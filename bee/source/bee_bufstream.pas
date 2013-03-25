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
  Crc,
  Sha1,
  BlowFish;

const
  DefaultBufferSize = 4096;

type
  TBuffer = array [0..DefaultBufferSize -1] of byte;

  { TBaseHash class }

  TBaseHash = class(TObject)
  public
    procedure Initialize; virtual; abstract;
    procedure Update(Data: PByte; Count: longint); virtual; abstract;
    function Finalize: string; virtual; abstract;
  end;

  { TCRC32Hash class }

  TCRC32Hash = class(TBaseHash)
  private
    FCRC: longword;
  public
    procedure Initialize; override;
    procedure Update(Data: PByte; Count: longint); override;
    function Finalize: string; override;
  end;

  { TCRC64Hash class }

  TCRC64Hash = class(TBaseHash)
  private
    FCRC: qword;
  public
    procedure Initialize; override;
    procedure Update(Data: PByte; Count: longint); override;
    function Finalize: string; override;
  end;

  { TSHA1Hash class }

  TSHA1Hash = class(TBaseHash)
  private
    FCTX: TSHA1Context;
    FDigest: TSHA1Digest;
  public
    procedure Initialize; override;
    procedure Update(Data: PByte; Count: longint); override;
    function Finalize: string; override;
  end;

  THashAlgorithm = (haNone, haCRC32, haCRC64, haSHA1);

  { TBaseCipher class }

  TBaseCipher = class(TObject)
  public
    function Encrypt(const Data: TBuffer; Count: longint): longint; virtual; abstract;
    function Decrypt(const Data: TBuffer; Count: longint): longint; virtual; abstract;
  end;

  { TBlowFishCipher class }

  TBlowFishCipher = class(TBaseCipher)
  private
    FBlowFish: TBlowFish;
  public
    constructor Create(const Key: string);
    destructor Destroy; override;
    function Encrypt(const Data: TBuffer; Count: longint): longint; override;
    function Decrypt(const Data: TBuffer; Count: longint): longint; override;
  end;

  TCipherAlgorithm = (caNone, caBlowFish);

  { TBufStream class }

  TBufStream = class(TObject)
  private
    FBuffer: TBuffer;
    FHash: TBaseHash;
    FCipher: TBaseCipher;
  protected
    FHandle: THandle;
  public
    constructor Create(Handle: THandle);
    destructor Destroy; override;
    function Read(Data: PByte; Count: longint): longint; virtual; abstract;
    function Write(Data: PByte; Count: longint): longint; virtual; abstract;
    function Seek(const Offset: int64; Origin: longint): int64; virtual; abstract;

    procedure SetCipher(Algorithm: TCipherAlgorithm; const Key: string);
    procedure SetHash(Algorithm: THashAlgorithm);
    function  GetHashDigest: string;
  end;

  { TReadBufStream class }

  TReadBufStream = class(TBufStream)
  private
    FBufferIndex: longint;
    FBufferSize: longint;
    FPosition: int64;
  protected
    procedure ClearBuffer;
    procedure FillBuffer;
  public
    constructor Create(Handle: THandle);
    function Read(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64; override;
  end;

  { TWriteBufStream class }

  TWriteBufStream = class(TBufStream)
  private
    FBufferIndex: longint;
    FPosition: int64;
  protected
    procedure ClearBuffer;
    procedure FlushBuffer;
    procedure SetSize(const NewSize: int64); virtual;
  public
    constructor Create(Handle: THandle);
    function Write(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64;  override;
  end;

implementation

uses
  Math,
  SysUtils,
  Bee_Common,
  Bee_Interface;

/// TCRC32Hash class

procedure TCRC32Hash.Initialize;
begin
  FCRC := crc32(0, nil, 0);
end;

procedure TCRC32Hash.Update(Data: PByte; Count: longint);
begin
  FCRC := crc32(FCRC, Data, Count);
end;

function TCRC32Hash.Finalize: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

/// TCRC64Hash class

procedure TCRC64Hash.Initialize;
begin
  FCRC := crc64(0, nil, 0);
end;

procedure TCRC64Hash.Update(Data: PByte; Count: longint);
begin
  FCRC := crc64(FCRC, Data, Count);
end;

function TCRC64Hash.Finalize: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

/// TSHA1Hash class

procedure TSHA1Hash.Initialize;
begin
  SHA1Init(FCTX);
end;

procedure TSHA1Hash.Update(Data: PByte; Count: longint);
begin
  SHA1Update(FCTX, Data, Count);
end;

function TSHA1Hash.Finalize: string;
var
  Digest: TSHA1Digest;
begin
  SHA1Final(FCTX, Digest);
  Result := SHA1Print(Digest);
end;

/// TBlowFishCipher class

constructor TBlowFishCipher.Create(const Key: string);
var
  I: longint;
  K: TBlowFishKey;
  KLEn: longint;
begin
  inherited Create;
  KLen := Min(Length(Key), Length(K));
  for I := 1 to KLen do
  begin
    K[I -1] := byte(Key[I]);
  end;
  FBlowFish := TBlowFish.Create(K, KLen);
end;

destructor TBlowFishCipher.Destroy;
begin
  FBlowFish.Destroy;
  inherited Destroy;
end;

function TBlowFishCipher.Encrypt(const Data: TBuffer; Count: longint): longint;
var
  Block: ^TBFBlock;
begin
  Result := 0;
  while Result < Count do
  begin
    Block := @Data[Result];
    FBlowFish.Encrypt(Block^);
    Inc(Result, 8);
  end;
end;

function TBlowFishCipher.Decrypt(const Data: TBuffer; Count: longint): longint;
var
  Block : ^TBFBlock;
begin
  Result := 0;
  while Result < Count do
  begin
    Block := @Data[Result];
    FBlowFish.Decrypt(Block^);
    Inc(Result, 8);
  end;
end;

/// TBufStream class

constructor TBufStream.Create(Handle: THandle);
begin
  inherited Create;
  FHandle := Handle;
  FHash   := nil;
  FCipher := nil;
end;

destructor TBufStream.Destroy;
begin
  if Assigned(FHash) then
    FreeAndNil(FHash);
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  inherited Destroy;
end;

procedure TBufStream.SetHash(Algorithm: THashAlgorithm);
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

procedure TBufStream.SetCipher(Algorithm: TCipherAlgorithm; const Key: string);
begin
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  case Algorithm of
    caBlowFish: FCipher := TBlowFishCipher.Create(Key);
    else        FCipher := nil;
  end;
end;

function TBufStream.GetHashDigest: string;
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
  FPosition    := 0;
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
  Inc(FPosition, Result);

  if Assigned(FHash) then
    FHash.Update(Data, Result);
end;

function TReadBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  FBufferIndex := 0;
  FBufferSize  := 0;
  FPosition    := FileSeek(FHandle, Offset, Origin);
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

/// TWriteBufStream class

constructor TWriteBufStream.Create(Handle: THandle);
begin
  inherited Create(Handle);
  ClearBuffer;
end;

procedure TWriteBufStream.ClearBuffer;
begin
  FBufferIndex := 0;
  FPosition    := 0;
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
  Inc(FPosition, Result);

  if Assigned(FHash) then
    FHash.Update(Data, Count);
end;

function TWriteBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  Result := FPosition;
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

procedure TWriteBufStream.SetSize(const NewSize: int64);
begin
  FlushBuffer;
  if FileTruncate(FHandle, NewSize) then
  begin
    FBufferIndex := 0;
    FPosition    := NewSize;
  end else
    SetExitStatus(esResizeStreamError);
end;

end.

