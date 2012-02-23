#include <stdlib.h>
#include "beelib_stream.h"

/* Default buffer capacity */

#define DEFAULT_BUFFER_CAPACITY 65536

/* TReadStream struct/methods implemetation */

struct TReadStream {
         void *Stream;
   PStreamRead StreamRead;
         int32 BufferSize;
         int32 BufferReaded;
         uint8 Buffer[DEFAULT_BUFFER_CAPACITY];
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

inline uint8 ReadStream_Read(PReadStream Self)
{
  uint8 result = 0;
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
         int32 BufferSize;
         uint8 Buffer[DEFAULT_BUFFER_CAPACITY];
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

inline void WriteStream_Write(PWriteStream Self, uint8 Data)
{
  if (Self->BufferSize == DEFAULT_BUFFER_CAPACITY)
  {
    WriteStream_FlushBuffer(Self);
  }
  Self->Buffer[Self->BufferSize] = Data;
  Self->BufferSize++;
}

