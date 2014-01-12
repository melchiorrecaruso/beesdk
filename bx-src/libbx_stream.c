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
       uint8_t Buffer[DEFAULT_BUFFER_CAPACITY];
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
  uint8_t result;
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
         uint8_t Buffer[DEFAULT_BUFFER_CAPACITY];
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

