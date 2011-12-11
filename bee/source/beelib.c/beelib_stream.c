#include "beelib_stream.h"
#include <stdlib.h>

/* TReadStream struct/methods implemetation */

struct TReadStream {
  PFillBuffer FillBuffer;
      PStream Stream;
          int BufferSize;
          int BufferReaded;
         char Buffer[DEFAULT_BUFFER_CAPACITY];
};

PReadStream ReadStream_Malloc(PStream aStream, PFillBuffer aFillBuffer)
{
  PReadStream Self = malloc(sizeof(struct TReadStream));

  Self->Stream       = aStream;
  Self->FillBuffer   = aFillBuffer;
  Self->BufferSize   = 0;
  Self->BufferReaded = 0;

  return Self;
}

void ReadStream_ClearBuffer(PReadStream Self)
{
  Self->BufferSize   = 0;
  Self->BufferReaded = 0;
}

void ReadStream_FillBuffer(PReadStream Self)
{
  Self->BufferSize   = Self->FillBuffer(Self->Stream, &(Self->Buffer[0]), DEFAULT_BUFFER_CAPACITY);
  Self->BufferReaded = 0;
}

char ReadStream_Read(PReadStream Self)
{
  char result = 0;
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
  PFlushBuffer FlushBuffer;
       PStream Stream;
           int BufferSize;
          char Buffer[DEFAULT_BUFFER_CAPACITY];
};

void WriteStream_Initialize(PWriteStream Self, PStream aStream, PFlushBuffer aFlushBuffer)
{
  Self->Stream      = aStream;
  Self->FlushBuffer = aFlushBuffer;
  Self->BufferSize  = 0;
}

void WriteStream_ClearBuffer(PWriteStream Self)
{
  Self->BufferSize = 0;
}

void WriteStream_FlushBuffer(PWriteStream Self)
{
  if (Self->BufferSize != 0)
  {
    Self->FlushBuffer(Self->Stream, &(Self->Buffer[0]), Self->BufferSize);
    Self->BufferSize = 0;
  }
}

void WriteStream_Write(PWriteStream Self, char Data)
{
  if (Self->BufferSize == DEFAULT_BUFFER_CAPACITY)
  {
    WriteStream_FlushBuffer(Self);
  }
  Self->Buffer[Self->BufferSize] = Data;
  Self->BufferSize++;
}
