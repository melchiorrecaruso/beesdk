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
    procedure Update(Data: PByte; Count: longint); virtual;
    function GetHash: string; virtual;
  end;

  { TCRC32Hash class }

  TCRC32Hash = class(TBaseHash)
  private
    FCRC: longword;
  public
    constructor Create;
    procedure Update(Data: PByte; Count: longint); override;
    function GetHash: string; override;
  end;

  { TCRC64Hash class }

  TCRC64Hash = class(TBaseHash)
  private
    FCRC: qword;
  public
    constructor Create;
    procedure Update(Data: PByte; Count: longint); override;
    function GetHash: string; override;
  end;

  { TSHA1Hash class }

  TSHA1Hash = class(TBaseHash)
  private
    FCTX: TSHA1Context;
    FDigest: TSHA1Digest;
  public
    constructor Create;
    procedure Update(Data: PByte; Count: longint); override;
    function GetHash: string; override;
  end;

  { TBaseCipher class }

  TBaseCipher = class(TObject)
  public
    function Encrypt(const Data: TBuffer; Count: longint): longint; virtual;
    function Decrypt(const Data: TBuffer; Count: longint): longint; virtual;
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

  { TBufStream class }

  TBufStream = class(TObject)
  private
    FHash: TBaseHash;
    FCipher: TBaseCipher;
    FBuffer: TBuffer;
  protected
    FHandle: THandle;
  public
    constructor Create(Handle: THandle);
    function Read(Data: PByte; Count: longint): longint; virtual; abstract;
    function Write(Data: PByte; Count: longint): longint; virtual; abstract;
    function SeekFromBeginning(const Offset: int64): int64; virtual; abstract;
    function SeekFromEnd(const Offset: int64): int64; virtual; abstract;
    function SeekFromCurrent: int64; virtual; abstract;
  public
    property Hash: TBaseHash read FHash write FHash;
    property Cipher: TBaseCipher read FCipher write FCipher;
  end;

  { TReadBufStream class }

  TReadBufStream = class(TBufStream)
  private
    FBufferIndex: longint;
    FBufferSize: longint;
    FPosition: int64;
  protected
    procedure FillBuffer;
    procedure ClearBuffer;
  public
    constructor Create(Handle: THandle);
    function Read(Data: PByte; Count: longint): longint; override;
    function SeekFromBeginning(const Offset: int64): int64; override;
    function SeekFromEnd(const Offset: int64): int64;  override;
  end;

  { TWriteBufStream class }

  TWriteBufStream = class(TBufStream)
  private
    FBufferIndex: longint;
    FPosition: int64;
  protected
    procedure FlushBuffer;
    procedure ClearBuffer;
    procedure SetSize(const NewSize: int64); virtual;
  public
    constructor Create(Handle: THandle);
    function Write(Data: PByte; Count: longint): longint; override;
    function SeekFromCurrent: int64;  override;
  end;

implementation

uses
  Math,
  SysUtils,
  Bee_Common,
  Bee_Interface;

/// TBaseHash class

procedure TBaseHash.Update(Data: PByte; Count: longint);
begin
  // nothing to do
end;

function TBaseHash.GetHash: string;
begin
  Result := '';
end;

/// TCRC32Hash class

constructor TCRC32Hash.Create;
begin
  FCRC := crc32(0, nil, 0);
end;

procedure TCRC32Hash.Update(Data: PByte; Count: longint);
begin
  FCRC := crc32(FCRC, Data, Count);
end;

function TCRC32Hash.GetHash: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

/// TCRC64Hash class

constructor TCRC64Hash.Create;
begin
  FCRC := crc64(0, nil, 0);
end;

procedure TCRC64Hash.Update(Data: PByte; Count: longint);
begin
  FCRC := crc64(FCRC, Data, Count);
end;

function TCRC64Hash.GetHash: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

/// TSHA1Hash class

constructor TSHA1Hash.Create;
begin
  SHA1Init(FCTX);
end;

procedure TSHA1Hash.Update(Data: PByte; Count: longint);
begin
  SHA1Update(FCTX, Data, Count);
end;

function TSHA1Hash.GetHash: string;
var
  Digest: TSHA1Digest;
begin
  SHA1Final(FCTX, Digest);
  Result := SHA1Print(Digest);
end;

/// TBaseCipher class

function TBaseCipher.Encrypt(const Data: TBuffer; Count: longint): longint;
begin
  // nothing to do
end;

function TBaseCipher.Decrypt(const Data: TBuffer; Count: longint): longint;
begin
  // nothing to do
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

function TReadBufStream.SeekFromBeginning(const Offset: int64): int64;
begin
  if FPosition <> Offset then
  begin
    FBufferIndex := 0;
    FBufferSize  := 0;
    FPosition    := FileSeek(FHandle, Offset, fsFromBeginning);
  end;
end;

function TReadBufStream.SeekFromEnd(const Offset: int64): int64;
begin
  FBufferIndex := 0;
  FBufferSize  := 0;
  FPosition    := FileSeek(FHandle, Offset, fsFromBeginning);
end;

procedure TReadBufStream.FillBuffer;
begin
  FBufferIndex := 0;
  FBufferSize  := FileRead(FHandle, FBuffer[0], SizeOf(FBuffer));
  if FBufferSize > -1 then
  begin
    if Assigned(FCipher) then
      Cipher.Decrypt(FBuffer, FBufferSize);
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

function TWriteBufStream.SeekFromCurrent: int64;
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

