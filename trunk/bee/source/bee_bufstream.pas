{
  Copyright (c) 1999-2009 Andrew Filinsky and Melchiorre Caruso

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



  Modifyed:

    v0.8.0 build 1110 - 2010.04.07 by Melchiorre Caruso.
}

unit Bee_BufStream;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_BlowFish;

const
  DefaultBufferCapacity: longint = 128 * 1024;

type
  { TBufStream }

  TBufStream2 = class(TStream)
  private
    FSource: TStream;
    FCapacity: longint;
    FBufferSize: longint;
    FBufferReaded: longint;
    FBuffer: array of byte;
    FBlowFish: TBlowFish;
    procedure FillBuffer;
    procedure FlushBuffer;
    procedure SetCapacity(const AValue: longint);
  public
    constructor Create(ASource: TStream; ACapacity: longint); overload;
    constructor Create(ASource: TStream); overload;
    destructor Destroy; override;
    procedure BlowFish(const AKey: string); overload;
    procedure BlowFish; overload;
    property Capacity: longint read FCapacity write SetCapacity;
  end;

  { TReadBufStream }

  TReadBufStream2 = class(TBufStream2)
  public
    function Read(var Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64;override;
  end;

  { TWriteBufStream }

  TWriteBufStream2 = class(TBufStream2)
  public
    destructor Destroy; override;
    function Write(const Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  end;

implementation

uses
  Math,
  Bee_Assembler;

{ TBufStream class }

constructor TBufStream2.Create(ASource: TStream; ACapacity: longint);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FBlowFish := TBlowFish.Create;
  FSource := ASource;
end;

constructor TBufStream2.Create(ASource: TStream);
begin
  Create(ASource, DefaultBufferCapacity);
end;

destructor TBufStream2.Destroy;
begin
  SetCapacity(0);
  FSource := nil;
  FBlowFish.Destroy;
  inherited Destroy;
end;

procedure TBufStream2.SetCapacity(const AValue: longint);
begin
  FBufferSize := 0;
  FBufferReaded := 0;
  FCapacity := AValue;
  SetLength(FBuffer, FCapacity);
end;

procedure TBufStream2.FillBuffer;
begin
  FBufferSize := FSource.Read(FBuffer[0], FCapacity);
  if FBlowFish.Started then
  begin
    FBlowFish.Decode(FBuffer[0], FBufferSize);
  end;
  FBufferReaded := 0;
end;

procedure TBufStream2.FlushBuffer;
begin
  FSource.Write(FBuffer[0], FBufferSize);
  if FBlowFish.Started then
  begin
    FBlowFish.Encode(FBuffer[0], FBufferSize);
  end;
  FBufferSize := 0;
end;

procedure TBufStream2.BlowFish(const AKey: string);
begin
  FBlowFish.Start(AKey);
end;

procedure TBufStream2.BlowFish;
begin
  FBlowFish.Finish;
end;

{ TReadBufStream class }

function TReadBufStream2.Read(var Data; Count: longint): longint;
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

function TReadBufStream2.Seek(Offset: longint; Origin: word): longint;
begin
  if (Origin = soFromCurrent) and (OffSet = 0) then
    Result := FSource.Seek(Offset - (FBufferSize - FBufferReaded), Origin)
  else
    Result := FSource.Seek(Offset, Origin);

  FBufferSize   := 0;
  FBufferReaded := 0;
end;

function TReadBufStream2.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if (Origin = soCurrent) and (OffSet = 0) then
    Result := FSource.Seek(Offset + FBufferSize - FBufferReaded, Origin)
  else
    Result := FSource.Seek(Offset, Origin);

  FBufferSize   := 0;
  FBufferReaded := 0;
end;

{ TWriteBufStream class }

destructor TWriteBufStream2.Destroy;
begin
  FlushBuffer;
  inherited Destroy;
end;

function TWriteBufStream2.Write(const Data; Count: longint): longint;
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

function TWriteBufStream2.Seek(Offset: longint; Origin: word): longint;
begin
  if FBufferSize > 0 then
  begin
    FlushBuffer;
  end;
  Result := FSource.Seek(Offset, Origin);
end;

function TWriteBufStream2.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if FBufferSize > 0 then
  begin
    FlushBuffer;
  end;
  Result := FSource.Seek(Offset, Origin);
end;

end.

