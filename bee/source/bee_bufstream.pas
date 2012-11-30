{
  Copyright (c) 1999-2012 Andrew Filinsky and Melchiorre Caruso

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

}

unit Bee_BufStream;

{$I compiler.inc}

interface

uses
  Classes;

const
  DefaultBufferCapacity: longint = $100000;

type
  { TBufStream }

  TBufStream = class(TObject)
  protected
    FSource: THandle;
    FBufferSize: longint;
    FBufferReaded: longint;
    FBuffer: array of byte;
    procedure ClearBuffer;
  public
    constructor Create(aSource: THandle);
    destructor Destroy; override;
  end;

  { TReadBufStream }

  TReadBufStream = class(TBufStream)
  protected
    procedure FillBuffer;
  public
    function Read(Data: PByte; Count: longint): longint; virtual;
    function Seek(Offset: longint; Origin: longint): longint; virtual; overload;
    function Seek(const Offset: int64; Origin: longint): int64; virtual; overload;
  end;

  { TWriteBufStream }

  TWriteBufStream = class(TBufStream)
  protected
    procedure FlushBuffer;
    procedure SetSize(NewSize: longint); virtual; overload;
    procedure SetSize(const NewSize: int64); virtual; overload;
  public
    function Write(Data: PByte; Count: longint): longint; virtual;
    function Seek(Offset: longint; Origin: longint): longint; virtual; overload;
    function Seek(const Offset: int64; Origin: longint): int64; virtual; overload;
  end;

implementation

uses
  Math,
  SysUtils,
  Bee_Assembler,
  Bee_Interface;

{ TBufStream class }

constructor TBufStream.Create(aSource: THandle);
begin
  inherited Create;
  FSource := aSource;
  FBufferSize   := 0;
  FBufferReaded := 0;
  SetLength(FBuffer, DefaultBufferCapacity);
end;

destructor TBufStream.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

procedure TBufStream.ClearBuffer;
begin
  FBufferSize   := 0;
  FBufferReaded := 0;
end;

{ TReadBufStream class }

function TReadBufStream.Read(Data: PByte; Count: longint): longint;
var
  I: longint;
begin
  Result := 0;
  repeat
    if FBufferReaded = FBufferSize then
    begin
      FillBuffer;
      if FBufferSize = 0 then Break;
    end;
    I := Min(Count - Result, FBufferSize - FBufferReaded);

    CopyBytes(FBuffer[FBufferReaded], Data[Result], I);
    Inc(FBufferReaded, I);
    Inc(Result, I);
  until Result = Count;
end;

function TReadBufStream.Seek(Offset: longint; Origin: longint): longint;
begin
  if (Origin = fsFromCurrent) and (OffSet = 0) then
    OffSet := FBufferReaded - FBufferSize;

  Result        := FileSeek(FSource, Offset, Origin);
  FBufferSize   := 0;
  FBufferReaded := 0;
end;

function TReadBufStream.Seek(const Offset: int64; Origin: longint): int64;
var
  NewOffset: int64;
begin
  if (Origin = fsFromCurrent) and (OffSet = 0) then
    NewOffset := FBufferReaded - FBufferSize
  else
    NewOffset := Offset;

  Result        := FileSeek(FSource, NewOffset, Origin);
  FBufferSize   := 0;
  FBufferReaded := 0;
end;

procedure TReadBufStream.FillBuffer;
begin
  FBufferReaded := 0;
  FBufferSize   := FileRead(FSource, FBuffer[0], DefaultBufferCapacity);

  Writeln('Fill BufferSize =', FBufferSize);

  if FBufferSize = -1 then
  begin
    FBufferSize := 0;
    ExitCode  := 103;
  end;
end;

{ TWriteBufStream class }

function TWriteBufStream.Write(Data: PByte; Count: longint): longint;
var
  I: longint;
begin
  Result := 0;
  repeat
    if FBufferSize = DefaultBufferCapacity then
    begin
      FlushBuffer;
      // if...
    end;
    I := Min(Count - Result, DefaultBufferCapacity - FBufferSize);

    CopyBytes(Data[Result], FBuffer[FBufferSize], I);
    Inc(FBufferSize, I);
    Inc(Result, I);
  until Result = Count;
end;

function TWriteBufStream.Seek(Offset: longint; Origin: longint): longint;
begin
  FlushBuffer;
  Result := FileSeek(FSource, Offset, Origin);
end;

function TWriteBufStream.Seek(const Offset: int64; Origin: longint): int64;
begin
  FlushBuffer;
  Result := FileSeek(FSource, Offset, Origin);
end;

procedure TWriteBufStream.FlushBuffer;
begin
  if FileWrite(FSource, FBuffer[0], FBufferSize) <> FBufferSize then
  begin
    ExitCode := 160;
  end;
  FBufferSize := 0;
end;

procedure TWriteBufStream.SetSize(NewSize: longint);
begin
  FileTruncate(FSource, NewSize);
end;

procedure TWriteBufStream.SetSize(const NewSize: int64);
begin
  FileTruncate(FSource, NewSize);
end;

(*
{ TReadBlowFishBufStream class }

constructor TReadBlowFishBufStream.Create(ASource: TStream; ACapacity: longint);
begin
  FBFK := False;
  FBF  := TBlowFish.Create;
  inherited Create(ASource, ACapacity);
end;

constructor TReadBlowFishBufStream.Create(ASource: TStream);
begin
  FBFK := False;
  FBF  := TBlowFish.Create;
  inherited Create(ASource);
end;

destructor TReadBlowFishBufStream.Destroy;
begin
  inherited Destroy;
  FBF.Destroy;
end;

procedure TReadBlowFishBufStream.FillBuffer;
var
  I: longint;
begin
  inherited FillBuffer;
  if FBFK then
  begin
    I := 0;
    while I < FBufferSize do
    begin
      FBF.Decode(@FBuffer[I], @FBuffer[I + 4]);
      Inc(I, 8);
    end;
  end;
end;

procedure TReadBlowFishBufStream.SetCapacity(const AValue: longint);
begin
  if (AValue mod 8) = 0 then
  begin
    inherited SetCapacity(AValue);
  end;
end;

procedure TReadBlowFishBufStream.StartDecode(const Value: string);
begin
  FBFK := Length(Value) >= MinBlowFishKeyLength;
  if FBFK then
  begin
    FBufferSize   := 0;
    FBufferReaded := 0;
    FBF.Initialize(Value);
  end;
end;

procedure TReadBlowFishBufStream.FinishDecode;
begin
  FBufferSize   := 0;
  FBufferReaded := 0;
  FBFK := False;
end;

{ TWriteBlowFishBufStream class }

constructor TWriteBlowFishBufStream.Create(ASource: TStream; ACapacity: longint);
begin
  FBFK := False;
  FBF  := TBlowFish.Create;
  inherited Create(ASource, ACapacity);
end;

constructor TWriteBlowFishBufStream.Create(ASource: TStream);
begin
  FBFK := False;
  FBF  := TBlowFish.Create;
  inherited Create(ASource);
end;

destructor TWriteBlowFishBufStream.Destroy;
begin
  inherited Destroy;
  FBF.Destroy;
end;

procedure TWriteBlowFishBufStream.FlushBuffer;
var
  I: longint;
begin
  if FBFK then
  begin
    I := 0;
    while I < FBufferSize do
    begin
      FBF.Encode(@FBuffer[I], @FBuffer[I + 4]);
      Inc(I, 8);
    end;
    FBufferSize := I;
  end;
  inherited FlushBuffer;
end;

procedure TWriteBlowFishBufStream.SetCapacity(const AValue: longint);
begin
  if (AValue mod 8) = 0 then
  begin
    inherited SetCapacity(AValue);
  end;
end;

procedure TWriteBlowFishBufStream.StartEncode(const Value: string);
begin
  FlushBuffer;
  // set key ...
  FBFK := Length(Value) >= MinBlowFishKeyLength;
  if FBFK then
  begin
    FBF.Initialize(Value);
  end;
end;

procedure TWriteBlowFishBufStream.FinishEncode;
begin
  FlushBuffer;
  FBFK := False;
end;
*)

end.

