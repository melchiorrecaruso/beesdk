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

    v0.8.0 build 1980 - 2013.04.29 by Melchiorre Caruso.
}

unit Bee_BufStream;

{$I bee_compiler.inc}

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
    FHashMethod: THashAlgorithm;
    FHashDigest: string;
    FCipher: TBaseCipher;
    FCipherMethod: TCipherAlgorithm;
    FCipherKey: string;
    FCoder: TBaseCoder;
    FCoderMethod: TCoderAlgorithm;
    FCoderLevel: longword;
    FCoderLevelAux: longword;
    FCoderFilter: string;
    FCoderFilterAux: string;
    FCoderBlock: int64;
  public
    constructor Create(Handle: THandle);
    destructor Destroy; override;
    function Read(Data: PByte; Count: longint): longint; virtual; abstract;
    function Write(Data: PByte; Count: longint): longint; virtual; abstract;
    function Decode(Data: PByte; Count: longint): longint; virtual; abstract;
    function Encode(Data: PByte; Count: longint): longint; virtual; abstract;
    function Seek(const Offset: int64; Origin: longint): int64; virtual; abstract;
    procedure StartSession; virtual;
    procedure EndSession; virtual;
  public
    property HashMethod: THashAlgorithm read FHashMethod write FHashMethod;
    property HashDigest: string read FHashDigest;
    property CipherMethod: TCipherAlgorithm read FCipherMethod write FCipherMethod;
    property CipherKey: string read FCipherKey write FCipherKey;
    property CoderMethod: TCoderAlgorithm read FCoderMethod write FCoderMethod;
    property CoderLevel: longword read FCoderLevel write FCoderLevel;
    property CoderLevelAux: longword read FCoderLevelAux write FCoderLevelAux;
    property CoderFilter: string read FCoderFilter write FCoderFilter;
    property CoderFilterAux: string read FCoderFilterAux write FCoderFilterAux;
    property CoderBlock: int64 read FCoderBlock write FCoderBlock;
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
    function Decode(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64; override;
    procedure StartSession; override;
    procedure EndSession; override;
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
    function Encode(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64; override;
    procedure StartSession; override;
    procedure EndSession; override;
  end;

  { TNulBufStream }

  TNulBufStream = class(TWriteBufStream)
  public
    constructor Create;
    function Write(Data: PByte; Count: longint): longint;  override;
    function Seek(const Offset: int64; Origin: longint): int64; override;
  end;

  function DoFill (Stream: pointer; Data: PByte; Size: longint): longint; {$IFDEF LIBBX} cdecl; {$ENDIF}
  function DoFlush(Stream: pointer; Data: PByte; Size: longint): longint; {$IFDEF LIBBX} cdecl; {$ENDIF}

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
  FHash          := nil;
  FHashMethod    := haNul;
  FCipher        := nil;
  FCipherMethod  := caNul;
  FCoder         := nil;
  FCoderMethod   := caStore;
end;

destructor TBufStream.Destroy;
begin
  if Assigned(FHash) then
    FreeAndNil(FHash);
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  if Assigned(FCoder)then
    FreeAndNil(FCoder);
  inherited Destroy;
end;

procedure TBufStream.StartSession;
begin
  FHashDigest := '';
  if Assigned(FHash) then
    FreeAndNil(FHash);
  case FHashMethod of
    haCRC32: FHash := TCRC32Hash.Create;
    haCRC64: FHash := TCRC64Hash.Create;
    haSHA1:  FHash := TSHA1Hash .Create;
    haMD5:   FHash := TMD5Hash  .Create;
  end;
  if Assigned(FHash) then
    FHash.Start;
end;

procedure TBufStream.EndSession;
begin
  if Assigned(FHash) then
  begin
    FHashDigest := FHash.Finish;
    FreeAndNil(FHash);
  end;
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
  if FBufferSize = -1 then
  begin
    FBufferSize := 0;
    SetExitStatus(esFillStreamError);
  end;
  if Assigned(FCipher) then
    FCipher.Decrypt(FBuffer, FBufferSize);
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

procedure TReadBufStream.StartSession;
begin
  ClearBuffer;
  inherited StartSession;
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  case FCipherMethod of
    caBlowFish: FCipher := TBlowFishCipher.Create(FCipherKey);
    caIdea:     FCipher := TIdeaCipher.CreateDec(FCipherKey);
  end;

  if Assigned(FCoder) then
    case FCoderMethod of
      caStore: if not (FCoder is TStoreCoder ) then FreeAndNil(FCoder);
      caBee:   if not (FCoder is TBeeDecoder ) then FreeAndNil(FCoder);
      caPpmd:  if not (FCoder is TPpmdDeCoder) then FreeAndNil(FCoder);
    end;

  if not Assigned(FCoder) then
    case FCoderMethod of
      caStore: FCoder := TStoreCoder .Create(Self);
      caBee:   FCoder := TBeeDecoder .Create(Self);
      caPpmd:  FCoder := TPpmdDecoder.Create(Self);
    end;

  if FCoderBlock = 0 then
  begin
    FCoder.Level     := FCoderLevel;
    FCoder.LevelAux  := FCoderLevelAux;
    FCoder.Filter    := FCoderFilter;
    FCoder.FilterAux := FCoderFilterAux;
    FCoder.Block     := FCoderBlock;
  end;
  FCoder.Start;
end;

procedure TReadBufStream.Endsession;
begin
  if Assigned(FCoder) then
    FCoder.Finish;
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  inherited EndSession;
  ClearBuffer;
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
    begin
      SetExitStatus(esFlushStreamError);
    end;
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

  if Assigned(FHash) then
    FHash.Update(Data, Result);
end;

function TWriteBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  FlushBuffer;
  Result := FileSeek(FHandle, Offset, Origin);
end;

procedure TWriteBufStream.StartSession;
begin
  FlushBuffer;
  inherited StartSession;
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  case FCipherMethod of
    caBlowFish: FCipher := TBlowFishCipher.Create(FCipherKey);
    caIdea:     FCipher := TIdeaCipher.CreateEnc(FCipherKey);
  end;

  if Assigned(FCoder) then
    case FCoderMethod of
      caStore: if not (FCoder is TStoreCoder ) then FreeAndNil(FCoder);
      caBee:   if not (FCoder is TBeeEncoder ) then FreeAndNil(FCoder);
      caPpmd:  if not (FCoder is TPpmdEnCoder) then FreeAndNil(FCoder);
    end;

  if not Assigned(FCoder) then
    case FCoderMethod of
      caStore: FCoder := TStoreCoder .Create(Self);
      caBee:   FCoder := TBeeEncoder .Create(Self);
      caPpmd:  FCoder := TPpmdEncoder.Create(Self);
    end;

  if FCoderBlock = 0 then
  begin
    FCoder.Level     := FCoderLevel;
    FCoder.LevelAux  := FCoderLevelAux;
    FCoder.Filter    := FCoderFilter;
    FCoder.FilterAux := FCoderFilterAux;
    FCoder.Block     := FCoderBlock;
  end;
  FCoder.Start;
end;

procedure TWriteBufStream.EndSession;
begin
  if Assigned(FCoder) then
    FCoder.Finish;
  FlushBuffer;
  if Assigned(FCipher) then
    FreeAndNil(FCipher);
  inherited EndSession;
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
  if Assigned(FHash) then
    FHash.Update(Data, Count);
end;

function TNulBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  Result := 0;
end;

end.

