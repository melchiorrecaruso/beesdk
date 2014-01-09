{
  Copyright (C) 2013 Melchiorre Caruso

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

  Modifyed:
}

unit libbx_Stream;

interface

const
  { Default buffer capacity }

  DEFAULT_BUFFER_CAPACITY = 4096;

type
  { I/O functions }

  PStreamRead  = function(Stream: Pointer; Data: PByte; Count: longint): longint;
  PStreamWrite = function(Stream: Pointer; Data: PByte; Count: longint): longint;

type
  { TReadStream struct/methods }

  PReadStream = ^TReadStream;

  TReadStream = packed record
    Stream: Pointer;
    StreamRead: PStreamRead;
    BufferSize: longint;
    BufferReaded: longint;
    Buffer: array[0.. DEFAULT_BUFFER_CAPACITY - 1] of byte;
  end;

  function  ReadStream_Create(aStream: Pointer; aStreamRead: PStreamRead): PReadStream;
  procedure ReadStream_Destroy    (Self: PReadStream);
  procedure ReadStream_ClearBuffer(Self: PReadStream);
  procedure ReadStream_FillBuffer (Self: PReadStream);
  function  ReadStream_Read       (Self: PReadStream): byte;

type
  { TWriteStream struct/methods }

  PWriteStream = ^TWriteStream;

  TWriteStream = packed record
    Stream: Pointer;
    StreamWrite: PStreamWrite;    
    BufferSize: longint;
    Buffer: array[0.. DEFAULT_BUFFER_CAPACITY - 1] of byte;
  end;

  function  WriteStream_Create(aStream: Pointer; aStreamWrite: PStreamWrite): PWriteStream;
  procedure WriteStream_Destroy    (Self: PWriteStream);
  procedure WriteStream_ClearBuffer(Self: PWriteStream);
  procedure WriteStream_FlushBuffer(Self: PWriteStream);
  procedure WriteStream_Write      (Self: PWriteStream; Data: byte);

implementation

{ TReadStream methods }

function ReadStream_Create(aStream: Pointer; aStreamRead: PStreamRead): PReadStream;
begin
  ReadStream_Create := GetMem(sizeof(TReadStream));
  ReadStream_Create^.Stream       := aStream;
  ReadStream_Create^.StreamRead   := aStreamRead;
  ReadStream_Create^.BufferSize   := 0;
  ReadStream_Create^.BufferReaded := 0;
end;

procedure ReadStream_Destroy(Self: PReadStream);
begin
  FreeMem(Self);
end;

procedure ReadStream_ClearBuffer(Self: PReadStream);
begin
  Self^.BufferReaded := 0;
  Self^.BufferSize   := 0;
end;

procedure ReadStream_FillBuffer(Self: PReadStream);
begin
  Self^.BufferSize   := Self^.StreamRead(Self^.Stream, @Self^.Buffer[0], DEFAULT_BUFFER_CAPACITY);
  Self^.BufferReaded := 0;
end;

function ReadStream_Read(Self: PReadStream): byte;
begin
  if Self^.BufferReaded < Self^.BufferSize then
  begin
    ReadStream_Read := Self^.Buffer[Self^.BufferReaded];
    Inc(Self^.BufferReaded);
  end else
  begin
    ReadStream_FillBuffer(Self);
    if Self^.BufferReaded < Self^.BufferSize then
    begin
      ReadStream_Read := Self^.Buffer[Self^.BufferReaded];
      Inc(Self^.BufferReaded);
    end;
  end;
end;

{ TWriteStream methods}

function WriteStream_Create(aStream: Pointer; aStreamWrite: PStreamWrite): PWriteStream;
begin
  WriteStream_Create := GetMem(sizeof(TWriteStream));
  WriteStream_Create^.Stream      := aStream;
  WriteStream_Create^.StreamWrite := aStreamWrite;
  WriteStream_Create^.BufferSize  := 0;
end;

procedure WriteStream_Destroy(Self: PWriteStream);
begin
  WriteStream_FlushBuffer(Self);
  FreeMem(Self);
end;

procedure WriteStream_ClearBuffer(Self: PWriteStream);
begin
  Self^.BufferSize := 0;
end;

procedure WriteStream_FlushBuffer(Self: PWriteStream);
begin
  if Self^.BufferSize <> 0 then
  begin
    Self^.StreamWrite(Self^.Stream, @Self^.Buffer[0], Self^.BufferSize);
    Self^.BufferSize := 0;
  end;
end;

procedure WriteStream_Write(Self: PWriteStream; Data: byte);
begin
  if Self^.BufferSize = DEFAULT_BUFFER_CAPACITY then
  begin
    WriteStream_FlushBuffer(Self);
  end;
  Self^.Buffer[Self^.BufferSize] := Data;
  Inc(Self^.BufferSize);
end;

end.

