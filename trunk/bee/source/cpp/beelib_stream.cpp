#include "beelib_stream.h"


TStream::TStream(void* Handle)
{
  FHandle     = Handle;
  FBufferSize = 0;
}

TStream::~TStream()
{
  FHandle = NULL;
}

void TStream::ClearBuffer()
{
  FBufferSize = 0;
}

TReadStream::TReadStream(void* Handle, TFillEvent OnFillEvent): TStream(Handle)
{
  FBufferReaded = 0;
  FOnFillEvent  = OnFillEvent;
}

TReadStream::~TReadStream()
{
  FOnFillEvent = NULL;
}

void TReadStream::FillBuffer()
{
  FBufferSize   = FOnFillEvent(FHandle, &FBuffer[0], DefaultBufferCapacity);
  FBufferReaded = 0;
}

void TReadStream::FlushBuffer()
{

}

unsigned char TReadStream::Read()
{
  unsigned char result = 0;
  if (FBufferReaded < FBufferSize)
  {
    result = FBuffer[FBufferReaded];
    FBufferReaded++;
  }
  else
  {
    FillBuffer();
    if (FBufferReaded < FBufferSize)
    {
      result = FBuffer[FBufferReaded];
      FBufferReaded++;
    }
  }
  return result;
}

void TReadStream::Write(unsigned char Data)
{

}

TWriteStream::TWriteStream(void* Handle, TFlushEvent OnFlushEvent): TStream(Handle)
{
  FOnFlushEvent = OnFlushEvent;
}

TWriteStream::~TWriteStream()
{
  FlushBuffer();
  FOnFlushEvent = NULL;
}

void TWriteStream::FlushBuffer()
{
  if (FBufferSize != 0)
  {
    FOnFlushEvent(FHandle, &FBuffer[0], FBufferSize);
    FBufferSize = 0;
  }
}

void TWriteStream::FillBuffer()
{

}

unsigned char TWriteStream::Read()
{
  return 0;
}

void TWriteStream::Write(unsigned char Data)
{
  if (FBufferSize == DefaultBufferCapacity)
  {
    FlushBuffer();
  }
  FBuffer[FBufferSize] = Data;
  FBufferSize++;
}
