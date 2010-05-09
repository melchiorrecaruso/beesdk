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

    Buffered stream classes.

  Modifyed:

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
}

unit Bee_BufStream;

{$I compiler.inc}

interface

uses
  Classes, Bee_BlowFish;

const
  DefaultBufferCapacity: longint = 128 * 1024;

type
  { TBufStream }

  TBufStream = class(TStream)
  private
    FSource: TStream;
    FCapacity: longint;
    FBufferSize: longint;
    FBufferReaded: longint;
    FBuffer: array of byte;
    FBlowFishKey: boolean;
    FBlowFish: TBlowFish;
    procedure FillBuffer;
    procedure FlushBuffer;
    procedure SetCapacity(const AValue: longint);
  public
    constructor Create(ASource: TStream; ACapacity: longint); overload;
    constructor Create(ASource: TStream); overload;
    destructor Destroy; override;
    procedure ClearBlowFishKey;
    procedure SetBlowFishKey(const Key: string);
    property Capacity: longint read FCapacity write SetCapacity;
  end;

  { TReadBufStream }

  TReadBufStream = class(TBufStream)
  public
    function Read(var Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64;override;
  end;

  { TWriteBufStream }

  TWriteBufStream = class(TBufStream)
  public
    destructor Destroy; override;
    function Write(const Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  end;

implementation

uses
  {$IFDEF FPC} Math, {$ENDIF} Bee_Assembler;

{ TBufStream class }

constructor TBufStream.Create(ASource: TStream; ACapacity: longint);
begin
  inherited Create;
  FSource := ASource;
  FBlowFishKey := False;
  FBlowFish := TBlowFish.Create;
  SetCapacity(ACapacity);
end;

constructor TBufStream.Create(ASource: TStream);
begin
  inherited Create;
  FSource := ASource;
  FBlowFishKey := False;
  FBlowFish := TBlowFish.Create;
  SetCapacity(DefaultBufferCapacity);
end;

destructor TBufStream.Destroy;
begin
  SetCapacity(0);
  FSource := nil;
  FBlowFish.Destroy;
  inherited Destroy;
end;

procedure TBufStream.SetCapacity(const AValue: longint);
begin
  if (AValue mod 8) = 0 then
  begin
    FBufferSize := 0;
    FBufferReaded := 0;
    FCapacity := AValue;
    SetLength(FBuffer, FCapacity);
  end;
end;

procedure TBufStream.SetBlowFishKey(const Key: string);
begin
  FBlowFishKey := Length(Key) >= MinKeyLength;
  if FBlowFishKey then
  begin
    FBlowFish.Initialize(Key);
  end;
end;

procedure TBufStream.ClearBlowFishKey;
begin
  FBlowFishKey := False;
end;

procedure TBufStream.FillBuffer;
var
  I: longint;
begin
  FBufferSize := FSource.Read(FBuffer[0], FCapacity);
  if FBlowFishKey then
  begin
    I := 0;
    while I < FBufferSize do
    begin
      FBlowFish.Decode(@FBuffer[I], @FBuffer[I + 4]);
      Inc(I, 8);
    end;
  end;
  FBufferReaded := 0;
end;

procedure TBufStream.FlushBuffer;
var
  I: longint;
begin
  if FBlowFishKey then
  begin
    I := 0;
    while I < FBufferSize do
    begin
      FBlowFish.Encode(@FBuffer[I], @FBuffer[I + 4]);
      Inc(I, 8);
    end;
  end;
  FSource.Write(FBuffer[0], FBufferSize);
  FBufferSize := 0;
end;

{ TReadBufStream class }

function TReadBufStream.Read(var Data; Count: longint): longint;
var
  Bytes: array [0..$FFFFFFF] of byte absolute Data;
  S: longint;
begin
  if (Count = SizeOf(Byte)) and (FBufferReaded < FBufferSize) then
  begin
    Byte(Data) := FBuffer[FBufferReaded];
    Result := SizeOf(Byte);
    Inc(FBufferReaded, Result);
  end else
  begin
    Result := 0;
    repeat
      if FBufferReaded = FBufferSize then
      begin
        FillBuffer;
        if FBufferSize = 0 then Exit;
      end;
      S := Min(Count - Result, FBufferSize - FBufferReaded);

      Move(FBuffer[FBufferReaded], Bytes[Result], S);

      Inc(Result, S);
      Inc(FBufferReaded, S);
    until Result = Count;
  end;
end;

function TReadBufStream.Seek(Offset: longint; Origin: word): longint;
begin
  if (Origin = soFromCurrent) and (OffSet = 0) then
    Result := FSource.Seek(Offset - (FBufferSize - FBufferReaded), Origin)
  else
    Result := FSource.Seek(Offset, Origin);

  FBufferSize   := 0;
  FBufferReaded := 0;
end;

function TReadBufStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if (Origin = soCurrent) and (OffSet = 0) then
    Result := FSource.Seek(Offset + FBufferSize - FBufferReaded, Origin)
  else
    Result := FSource.Seek(Offset, Origin);

  FBufferSize   := 0;
  FBufferReaded := 0;
end;

{ TWriteBufStream class }

destructor TWriteBufStream.Destroy;
begin
  FlushBuffer;
  inherited Destroy;
end;

function TWriteBufStream.Write(const Data; Count: longint): longint;
var
  Bytes: array [0..$FFFFFFF] of byte absolute Data;
  S: longint;
begin
  if (Count = SizeOf(Byte)) and (FCapacity > FBufferSize) then
  begin
    FBuffer[FBufferSize] := Byte(Data);
    Inc(FBufferSize);
    Result := SizeOf(Byte);
  end else
  if Count > (FCapacity - FBufferSize) then
    begin
      Result := 0;
      repeat
        S := FCapacity - FBufferSize;
        CopyBytes(Bytes[Result], FBuffer[FBufferSize], S);
        Inc(Result, S);
        Inc(FBufferSize, S);
        FlushBuffer;
      until ((Count - Result) <= FCapacity);

      CopyBytes(Bytes[Result], FBuffer[FBufferSize], Count - Result);
      Inc(FBufferSize, Count - Result);
      Inc(Result, Count - Result);
    end else
    begin
      CopyBytes(Data, FBuffer[FBufferSize], Count);
      Inc(FBufferSize, Count);
      Result := Count;
    end;
end;

function TWriteBufStream.Seek(Offset: longint; Origin: word): longint;
begin
  if FBufferSize > 0 then
  begin
    FlushBuffer;
  end;
  Result := FSource.Seek(Offset, Origin);
end;

function TWriteBufStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if FBufferSize > 0 then
  begin
    FlushBuffer;
  end;
  Result := FSource.Seek(Offset, Origin);
end;

end.

