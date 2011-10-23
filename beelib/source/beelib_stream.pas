{
  Copyright (c) 2010-2011 Melchiorre Caruso

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
}

unit BeeLib_Stream;

interface

uses
  BeeLib_Interface;

const
  { Default TStream buffer capacity }

  DefaultBufferCapacity = 128 * 1024;

type
  { TStream abstract class }

  TStream = class
  private
    FHandle: pointer;
    FBufferSize: longint;
    FBuffer: array[0.. DefaultBufferCapacity - 1] of byte;
  public
    constructor Create(Handle: pointer);
    destructor Destroy; override;
    procedure FlushBuffer; virtual; abstract;
    function Read: byte; virtual; abstract;
    procedure Write(Data: byte); virtual; abstract;
  end;

  { TReadStream class }

  TReadStream = class(TStream)
  private
    FBufferReaded: longint;
    FOnFillEvent: TFillEvent;
    procedure FillBuffer;
  public
    constructor Create(Handle: pointer; OnFillEvent: TFillEvent);
    destructor Destroy; override;
    procedure FlushBuffer; override;
    function Read: byte; override;
  end;

  { TWriteStream class }

  TWriteStream = class(TStream)
  private
    FOnFlushEvent: TFlushEvent;
  public
    constructor Create(Handle: pointer; OnFlushEvent: TFlushEvent);
    destructor Destroy; override;
    procedure FlushBuffer; override;
    procedure Write(Data: byte); override;
  end;

implementation

{ TStream class}

constructor TStream.Create(Handle: pointer);
begin
  inherited Create;
  FHandle     := Handle;
  FBufferSize := 0;
end;

destructor TStream.Destroy;
begin
  FHandle := nil;
  inherited Destroy;
end;

{ TReadStream class}

constructor TReadStream.Create(Handle: pointer; OnFillEvent: TFillEvent);
begin
  inherited Create(Handle);
  FBufferReaded := 0;
  FOnFillEvent  := OnFillEvent;
end;

destructor TReadStream.Destroy;
begin
  FOnFillEvent := nil;
  inherited Destroy;
end;

procedure TReadStream.FillBuffer;
begin
  FBufferSize   := FOnFillEvent(FHandle, FBuffer[0], DefaultBufferCapacity);
  FBufferReaded := 0;
end;

procedure TReadStream.FlushBuffer;
begin
  FBufferSize   := 0;
  FBufferReaded := 0;
end;

function TReadStream.Read: byte;
begin
  if FBufferReaded < FBufferSize then
  begin
    Result := FBuffer[FBufferReaded];
    Inc(FBufferReaded);
  end else
  begin
    FillBuffer;
    if FBufferReaded < FBufferSize then
    begin
      Result := FBuffer[FBufferReaded];
      Inc(FBufferReaded);
    end;
  end;
end;

{ TWriteStream class}

constructor TWriteStream.Create(Handle: pointer; OnFlushEvent: TFlushEvent);
begin
  inherited Create(Handle);
  FOnFlushEvent := OnFlushEvent;
end;

destructor TWriteStream.Destroy;
begin
  FlushBuffer;
  FOnFlushEvent := nil;
  inherited Destroy;
end;

procedure TWriteStream.FlushBuffer;
begin
  if FBufferSize <> 0 then
  begin
    FOnFlushEvent(FHandle, FBuffer[0], FBufferSize);
    FBufferSize := 0;
  end;
end;

procedure TWriteStream.Write(Data: byte);
begin
  if FBufferSize = DefaultBufferCapacity then
  begin
    FlushBuffer;
  end;
  FBuffer[FBufferSize] := Data;
  Inc(FBufferSize);
end;

end.

