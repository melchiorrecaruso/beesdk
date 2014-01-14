/*
  Copyright (c) 2012-2014 Melchiorre Caruso

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the free software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT any WARRANTY; WITHOUT even the implied WARRANTY of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License FOR more details.

  you should have received A copy of the GNU General Public License
  along with This program; if not, write to the free software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
  Contains:

    Common buffered stream.

  Modifyed:

    v1.0.0 build 2202 - 2014.01.13 by Melchiorre Caruso.

*/

#include "libbx_stream.h"
#include <stdlib.h>

/* Default buffer capacity */

#define DEFAULT_BUFFER_CAPACITY 4096L

/* TReadStream struct/methods implemetation */

struct TReadStream {
         void *Stream;
    PStreamRead StreamRead;
       int32_t BufferSize;
       int32_t BufferReaded;
       uint8_t Buffer[DEFAULT_BUFFER_CAPACITY - 1];
};

PReadStream ReadStream_Create(void *aStream, PStreamRead aStreamRead)
{
  PReadStream Self   = malloc(sizeof(struct TReadStream));
  Self->Stream       = aStream;
  Self->StreamRead   = aStreamRead;

  Self->BufferReaded = 0;
  Self->BufferSize   = 0;
  return Self;
}

void ReadStream_Destroy(PReadStream Self)
{
  free(Self);
}

inline void ReadStream_ClearBuffer(PReadStream Self)
{
  Self->BufferReaded = 0;
  Self->BufferSize   = 0;
}

inline void ReadStream_FillBuffer(PReadStream Self)
{
  Self->BufferSize   = Self->StreamRead(Self->Stream, &(Self->Buffer[0]), DEFAULT_BUFFER_CAPACITY);
  Self->BufferReaded = 0;
}

inline uint8_t ReadStream_Read(PReadStream Self)
{
  uint8_t result = 0;
  if (Self->BufferReaded < Self->BufferSize)
  {
    result = Self->Buffer[Self->BufferReaded];
    Self->BufferReaded++;
  }
  else
  {
    ReadStream_FillBuffer(Self);
    if (Self->BufferReaded < Self->BufferSize)
    {
      result = Self->Buffer[Self->BufferReaded];
      Self->BufferReaded++;
    }
  }
  return result;
}

/* TWriteStream struct/methods implemetation */

struct TWriteStream {
         void *Stream;
  PStreamWrite StreamWrite;
         int32_t BufferSize;
         uint8_t Buffer[DEFAULT_BUFFER_CAPACITY - 1];
};

PWriteStream WriteStream_Create(void *aStream, PStreamWrite aStreamWrite)
{
  PWriteStream Self = malloc(sizeof(struct TWriteStream));
  Self->Stream      = aStream;
  Self->StreamWrite = aStreamWrite;
  Self->BufferSize  = 0;
  return Self;
}

void WriteStream_Destroy(PWriteStream Self)
{
  free(Self);
}

inline void WriteStream_ClearBuffer(PWriteStream Self)
{
  Self->BufferSize = 0;
}

inline void WriteStream_FlushBuffer(PWriteStream Self)
{
  if (Self->BufferSize != 0)
  {
    Self->StreamWrite(Self->Stream, &(Self->Buffer[0]), Self->BufferSize);
    Self->BufferSize = 0;
  }
}

inline void WriteStream_Write(PWriteStream Self, uint8_t Data)
{
  if (Self->BufferSize == DEFAULT_BUFFER_CAPACITY)
  {
    WriteStream_FlushBuffer(Self);
  }
  Self->Buffer[Self->BufferSize] = Data;
  Self->BufferSize++;
}

