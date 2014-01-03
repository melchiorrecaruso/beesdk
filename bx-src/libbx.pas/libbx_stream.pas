{
  Copyright (c) 2013 Melchiorre Caruso

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

unit libbx_Stream;

{$I bx_compiler.inc}

interface

const
  { Default buffer capacity }

  DEFAULT_BUFFER_CAPACITY = 4096;

type
  /* TReadStream struct/methods implemetation */

  TReadStream = packed record
          Stream: pointer;
      StreamRead: PStreamRead;    
      BufferSize: longint;
    BufferReaded: longint;
          Buffer: array[0.. DefaultBufferCapacity - 1] of byte;
  end;
  PReadStream =^TReadStream;

  function ReadStream_Create(aStream: pointer; aStreamRead: PStreamRead): PReadStream;
  procedure ReadStream_Destroy(Self: PReadStream);
  procedure ReadStream_ClearBuffer(Self: PReadStream);
  procedure ReadStream_FillBuffer(Self: PReadStream);
  function ReadStream_Read(Self: PReadStream);

/* TWriteStream struct/methods implemetation */

  TWriteStream = packed record
         Stream: pointer;
    StreamWrite: PStreamWrite;    
     BufferSize: longint;   
         Buffer: array[0.. DefaultBufferCapacity - 1] of byte;
  end;
  PWriteStream = ^TWriteStream;

  function WriteStream_Create(aStream: pointer; aStreamWrite: PStreamWrite) PWriteStream;
  procedure WriteStream_Destroy(Self PWriteStream);
  procedure WriteStream_ClearBuffer(Self PWriteStream);
  procedure WriteStream_FlushBuffer(Self PWriteStream);
  procedure WriteStream_Write(Self PWriteStream, Data: byte)

implementation

{ TReadStream methods }

function TReadStream_Create(aStream: pointer; aStreamRead: PStreamRead): PReadStream;
begin
  Result       := GetMem(sizeof(TReadStream));
  Result^.Stream       := aStream;
  Result^.StreamRead   := aStreamRead;

  Result^.BufferSize   := 0;
  Result^.BufferReaded := 0;
end;

procedure TReadStream_Destroy(Self: PReadStream);
begin
  FreeMem(Self);
end;

procedure ReadStream_ClearBuffer(Self: PReadStream);
begin
  Self

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

