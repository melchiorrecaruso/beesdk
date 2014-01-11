{
  Copyright (c) 2010-2013 Melchiorre Caruso.

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

{
  Contains:

    TBufStream classes.

  Fist release:

    v1.0 build 2165 - 2013.12.26 by Melchiorre Caruso.

  Modifyed:

}

unit bx_Stream;

{$I bx_compiler.inc}

interface

uses
  bx_Cipher,
  bx_Coder,
  bx_HashGen,
  bx_Messages;

type
  { TBufStream abstract class }

  TBufStream = class(TObject)
  protected
    FHandle: THandle;
  private
    FHash: THashGen;
    FHashMethod: longword;
    FHashDigest: string;
    FCipher: TCipher;
    FCipherMethod: longword;
    FCipherPassword: string;
    FCoder: TCoder;
    FCoderMethod: longword;
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
    property HashMethod: longword read FHashMethod write FHashMethod;
    property HashDigest: string read FHashDigest;
    property CipherMethod: longword read FCipherMethod write FCipherMethod;
    property CipherPassword: string read FCipherPassword write FCipherPassword;
    property CoderMethod: longword read FCoderMethod write FCoderMethod;
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
    function Encode(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint): int64; override;
  end;

  function DoFill (Stream: pointer; Data: PByte; Size: longint): longint; {$IFNDEF BEEPAS} cdecl; {$ENDIF} inline;
  function DoFlush(Stream: pointer; Data: PByte; Size: longint): longint; {$IFNDEF BEEPAS} cdecl; {$ENDIF} inline;

implementation

uses
  Math,
  SysUtils;

function DoFill(Stream: pointer; Data: PByte; Size: longint): longint; {$IFNDEF BEEPAS} cdecl; {$ENDIF} inline;
begin
  Result := TBufStream(Stream).Read(Data, Size);
end;

function DoFlush(Stream: pointer; Data: PByte; Size: longint): longint; {$IFNDEF BEEPAS} cdecl; {$ENDIF} inline;
begin
  Result := TBufStream(Stream).Write(Data, Size);
end;

/// TBufStream abstract class

constructor TBufStream.Create(Handle: THandle);
begin
  inherited Create;
  FHandle        := Handle;
  // HashGen ...
  FHash          := THashGen.Create;
  FHashMethod    := 0;
  // Cipher ...
  FCipher        := TCipher.Create;
  FCipherMethod  := 0;
  // Coder ...
  FCoder         := TStoreCoder.Create(Self);
  FCoderMethod   := 0;
end;

destructor TBufStream.Destroy;
begin
  FreeAndNil(FHash);
  FreeAndNil(FCipher);
  FreeAndNil(FCoder);
  inherited Destroy;
end;

procedure TBufStream.StartSession;
begin
  case FHashMethod of
    0: if (FHash is THashGen     ) = FALSE then FreeAndNil(FHash);
    1: if (FHash is TCRC32HashGen) = FALSE then FreeAndNil(FHash);
    2: if (FHash is TCRC64HashGen) = FALSE then FreeAndNil(FHash);
    3: if (FHash is TSHA1HashGen ) = FALSE then FreeAndNil(FHash);
    4: if (FHash is TMD5HashGen  ) = FALSE then FreeAndNil(FHash);
    else SetExitStatus(esCaseError);
  end;

  if Assigned(FHash) = FALSE then
    case FHashMethod of
      0: FHash := THashGen     .Create;
      1: FHash := TCRC32HashGen.Create;
      2: FHash := TCRC64HashGen.Create;
      3: FHash := TSHA1HashGen .Create;
      4: FHash := TMD5HashGen  .Create;
      else SetExitStatus(esCaseError);
    end;
  FHash.Start;
end;

procedure TBufStream.EndSession;
begin
  FHashDigest := FHash.Finish;
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

  FCipher.Update(FBuffer, FBufferSize);
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

  // Cipher initialization ...
  case FCipherMethod of
    0: if (FCipher is TCipher          ) = FALSE then FreeAndNil(FCipher);
    1: if (FCipher is TBlowFishDeCipher) = FALSE then FreeAndNil(FCipher);
    2: if (FCipher is TIdeaDeCipher    ) = FALSE then FreeAndNil(FCipher);
    else SetExitStatus(esCaseError);
  end;

  if Assigned(FCipher)= FALSE then
    case FCipherMethod of
      0: FCipher := TCipher          .Create;
      1: FCipher := TBlowFishDeCipher.Create;
      2: FCipher := TIdeaDeCipher    .Create;
      else SetExitStatus(esCaseError);
    end;

  FCipher.Start(FCipherPassword);

  // Coder initialization ...
  case FCoderMethod of
    0: if (FCoder is TStoreCoder ) = FALSE then FreeAndNil(FCoder);
    1: if (FCoder is TBeeDecoder ) = FALSE then FreeAndNil(FCoder);
    2: if (FCoder is TPpmdDeCoder) = FALSE then FreeAndNil(FCoder);
    else SetExitStatus(esCaseError);
  end;

  if Assigned(FCoder) = FALSE then
    case FCoderMethod of
      0: FCoder := TStoreCoder .Create(Self);
      1: FCoder := TBeeDecoder .Create(Self);
      2: FCoder := TPpmdDecoder.Create(Self);
      else SetExitStatus(esCaseError);
    end;

  FCoder.Level     := FCoderLevel;
  FCoder.LevelAux  := FCoderLevelAux;
  FCoder.Filter    := FCoderFilter;
  FCoder.FilterAux := FCoderFilterAux;
  FCoder.Block     := FCoderBlock;
  FCoder.Start;
end;

procedure TReadBufStream.EndSession;
begin
  FCoder.Finish;
  FCipher.Finish;
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
    FBufferIndex := FCipher.Update(FBuffer, FBufferIndex);

    if FBufferIndex <> FileWrite(FHandle, FBuffer[0], FBufferIndex) then
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

  // Cipher initialization ...
  case FCipherMethod of
    0: if not (FCipher is TCipher          ) then FreeAndNil(FCipher);
    1: if not (FCipher is TBlowFishEnCipher) then FreeAndNil(FCipher);
    2: if not (FCipher is TIdeaEnCipher    ) then FreeAndNil(FCipher);
    else SetExitStatus(esCaseError);
  end;

  if Assigned(FCipher) = FALSE then
    case FCipherMethod of
      0: FCipher := TCipher          .Create;
      1: FCipher := TBlowFishEnCipher.Create;
      2: FCipher := TIdeaEnCipher    .Create;
    else SetExitStatus(esCaseError);
    end;

  FCipher.Start(FCipherPassword);

  // Coder initialization ..
  case FCoderMethod of
    0: if not (FCoder is TStoreCoder ) then FreeAndNil(FCoder);
    1: if not (FCoder is TBeeEncoder ) then FreeAndNil(FCoder);
    2: if not (FCoder is TPpmdEnCoder) then FreeAndNil(FCoder);
    else SetExitStatus(esCaseError);
  end;

  if Assigned(FCoder) = FALSE then
    case FCoderMethod of
      0: FCoder := TStoreCoder .Create(Self);
      1: FCoder := TBeeEncoder .Create(Self);
      2: FCoder := TPpmdEncoder.Create(Self);
      else SetExitStatus(esCaseError);
    end;

  FCoder.Level     := FCoderLevel;
  FCoder.LevelAux  := FCoderLevelAux;
  FCoder.Filter    := FCoderFilter;
  FCoder.FilterAux := FCoderFilterAux;
  FCoder.Block     := FCoderBlock;
  FCoder.Start;
end;

procedure TWriteBufStream.EndSession;
begin
  FCoder.Finish;
  FlushBuffer;
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
  FHash.Update(Data, Result);
end;

function TNulBufStream.Encode(Data: PByte; Count: longint): longint;
begin
  Result := Write(Data, Count);
end;

function TNulBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  Result := 0;
end;

end.

