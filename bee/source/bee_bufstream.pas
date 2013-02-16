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

    v0.8.0 build 1864 - 2013.02.15 by Melchiorre Caruso.
}

unit Bee_BufStream;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_BlowFish;

type
  { TBufStream }
  TBufStream = class(TObject)
  private
    FBlowFish: TBlowFish;
  protected
    FHandle: THandle;
    FBuffer: array of byte;
  public
    constructor Create(aHandle: THandle);
    destructor Destroy; override;
  property
    BlowFish: TBlowFish read FBlowFish write FBlowFish;
  end;

  { TReadBufStream }
  TReadBufStream = class(TBufStream)
  private
    FPosition: int64;
    FBufferSize: longint;
    FBufferIndex: longint;
  protected
    procedure FillBuffer;
    procedure ClearBuffer;
  public
    constructor Create(aHandle: THandle);
    function Read(Data: PByte; Count: longint): longint; virtual;
    procedure SeekFromBeginning(const Offset: int64); virtual;
    procedure SeekFromEnd      (const Offset: int64); virtual;
  end;

  { TWriteBufStream }
  TWriteBufStream = class(TBufStream)
  private
    FPosition: int64;
    FBufferIndex: longint;
  protected
    procedure FlushBuffer;
    procedure ClearBuffer;
    procedure SetSize(const NewSize: int64); virtual;
  public
    constructor Create(aHandle: THandle);
    function Write(Data: PByte; Count: longint): longint; virtual;
    function SeekFromCurrent: int64; virtual;
  end;

  function GetCapacity(const Size: int64): longint;

implementation

uses
  Math,
  SysUtils,
  Bee_Interface;

const
  MinBufferCapacity = 1024;
  MaxBufferCapacity = 1024*1024;

function GetCapacity(const Size: int64): longint;
begin
  Result := MinBufferCapacity;
  if Size > Result then
  begin
    Result := MaxBufferCapacity;
    while (Size div Result) = 0 do
      Result := Result div 2;
  end;
end;

{ TBufStream class }

constructor TBufStream.Create(aHandle: THandle);
begin
  inherited Create;
  FBlowFish := nil;
  FHandle   := aHandle;
  SetLength(FBuffer, 0);
end;

destructor TBufStream.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

{ TReadBufStream class }

constructor TReadBufStream.Create(aHandle: THandle);
begin
  inherited Create(aHandle);

  ClearBuffer;
  SetLength(FBuffer, 4*MinBufferCapacity);
end;

procedure TReadBufStream.ClearBuffer;
begin
  FPosition    := 0;
  FBufferSize  := 0;
  FBufferIndex := 0;
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
end;

procedure TReadBufStream.SeekFromBeginning(const Offset: int64);
begin
  if FPosition <> OffSet then
  begin
    FPosition    := FileSeek(FHandle, Offset, fsFromBeginning);
    FBufferSize  := 0;
    FBufferIndex := 0;
  end;
end;

procedure TReadBufStream.SeekFromEnd(const Offset: int64);
begin
  FPosition    := FileSeek(FHandle, Offset, fsFromEnd);
  FBufferSize  := 0;
  FBufferIndex := 0;
end;

procedure TReadBufStream.FillBuffer;
begin
  FBufferIndex := 0;
  FBufferSize  := FileRead(FHandle, FBuffer[0], Length(FBuffer));

  if FBufferSize = -1 then
  begin
    FBufferSize := 0;
    SetExitStatus(esFillStreamError);
  end;

  if Assigned(FBlowFish) then
    FBlowFish.Decode(@FBuffer[0], Length(FBuffer));
end;

{ TWriteBufStream class }

constructor TWriteBufStream.Create(aHandle: THandle);
begin
  inherited Create(aHandle);

  ClearBuffer;
  SetLength(FBuffer, MaxBufferCapacity);
end;

procedure TWriteBufStream.ClearBuffer;
begin
  FPosition    := 0;
  FBufferIndex := 0;
end;

function TWriteBufStream.Write(Data: PByte; Count: longint): longint;
var
  I: longint;
begin
  Result := 0;
  repeat
    if FBufferIndex = Length(FBuffer) then
    begin
      FlushBuffer;
      if ExitStatus <> esNoError then Break;
    end;
    I := Min(Count - Result, Length(FBuffer) - FBufferIndex);

    Move(Data[Result], FBuffer[FBufferIndex], I);
    Inc(FBufferIndex, I);
    Inc(Result, I);
  until Result = Count;
  Inc(FPosition, Result);
end;

function TWriteBufStream.SeekFromCurrent: int64;
begin
  Result := FPosition;
end;

procedure TWriteBufStream.FlushBuffer;
begin
  if Assigned(FBlowFish) then
    FBlowFish.Encode(@FBuffer[0], Length(FBuffer));

  if FBufferIndex > 0 then
  begin
    if FBufferIndex <> FileWrite(FHandle, FBuffer[0], FBufferIndex)  then
      SetExitStatus(esFlushStreamError);

    FBufferIndex := 0;
  end;
end;

procedure TWriteBufStream.SetSize(const NewSize: int64);
begin
  FlushBuffer;
  if ExitStatus = esNoError then
  begin
    if FileTruncate(FHandle, NewSize) then
    begin
      FPosition    := NewSize;
      FBufferIndex := 0;
    end else
      SetExitStatus(esResizeStreamError);
  end;
end;

end.

