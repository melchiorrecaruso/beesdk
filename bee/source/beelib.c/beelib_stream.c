#include "beelib_stream.h"
#include <stdlib.h>

#define DEFAULT_BUFFER_CAPACITY 65536

/* TReadStream struct/methods implemetation */

struct TReadStream {
        void *StrmHandle;
    PStrmRead StrmRead;
          int BufferSize;
          int BufferReaded;
         char Buffer[DEFAULT_BUFFER_CAPACITY];
};

PReadStream ReadStream_Create(void *aStrmHandle, PStrmRead aStrmRead)
{
  PReadStream Self = malloc(sizeof(struct TReadStream));
  Self->StrmHandle = aStrmHandle;
  Self->StrmRead   = aStrmRead;

  Self->BufferReaded = 0;
  Self->BufferSize   = 0;
  return Self;
}

void ReadStream_Destroy(PReadStream Self)
{
  free(Self);
}

void ReadStream_ClearBuffer(PReadStream Self)
{
  Self->BufferReaded = 0;
  Self->BufferSize   = 0;
}

void ReadStream_FillBuffer(PReadStream Self)
{
  Self->BufferSize   = Self->StrmRead(Self->StrmHandle, &(Self->Buffer[0]), DEFAULT_BUFFER_CAPACITY);
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
       void *StrmHandle;
  PStrmWrite StrmWrite;
         int BufferSize;
        char Buffer[DEFAULT_BUFFER_CAPACITY];
};

PWriteStream WriteStream_Create(void *aStrmHandle, PStrmWrite aStrmWrite)
{
  PWriteStream Self = malloc(sizeof(struct TWriteStream));
  Self->StrmHandle  = aStrmHandle;
  Self->StrmWrite   = aStrmWrite;

  Self->BufferSize  = 0;
  return Self;
}

void WriteStream_Destroy(PWriteStream Self)
{
  free(Self);
}

void WriteStream_ClearBuffer(PWriteStream Self)
{
  Self->BufferSize = 0;
}

void WriteStream_FlushBuffer(PWriteStream Self)
{
  if (Self->BufferSize != 0)
  {
    Self->StrmWrite(Self->StrmHandle, &(Self->Buffer[0]), Self->BufferSize);
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

