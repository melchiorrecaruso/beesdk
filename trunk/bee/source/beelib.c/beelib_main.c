#include <stdlib.h>

#include "beelib_crc.h"
#include "beelib_main.h"
#include "beelib_types.h"
#include "beelib_stream.h"
#include "beelib_modeller.h"
#include "beelib_assembler.h"
#include "beelib_rangecoder.h"

unsigned int DllVersion()
{
  return 111;
};

/* TStreamEncoder struct/methods implementation */

struct TStreamEncoder {
       PWriteStream Stream;
      PRangeEncoder RangeEncoder;
         PBaseCoder BaseCoder;
};

PStreamEncoder StreamEncoder_Malloc(PStream aStream, PFlushBuffer aFlushBuffer)
{
  PStreamEncoder Self = malloc(sizeof(struct TStreamEncoder));

  Self->Stream       = WriteStream_Malloc(aStream, aFlushBuffer);
  Self->RangeEncoder = RangeEncoder_Malloc(Self->Stream);
  Self->BaseCoder    = BaseCoder_Malloc(Self->RangeEncoder, (PUpdateSymbol)RangeEncoder_UpdateSymbol);

  return Self;
};

void StreamEncoder_Free(PStreamEncoder Self)
{
  BaseCoder_Free(Self->BaseCoder);
  RangeEncoder_Free(Self->RangeEncoder);
  WriteStream_Free(Self->Stream);
  free(Self);
};

void StreamEncoder_SetDictionaryLevel(PStreamEncoder Self, unsigned int Value)
{
  BaseCoder_SetDictionary(Self->BaseCoder, Value);
};

void StreamEncoder_SetTableParameters(PStreamEncoder Self, const TTableParameters *Value)
{
  BaseCoder_SetTable(Self->BaseCoder, Value);
};

void StreamEncoder_FreshFlexible(PStreamEncoder Self)
{
  BaseCoder_FreshFlexible(Self->BaseCoder);
};

void StreamEncoder_FreshSolid(PStreamEncoder Self)
{
  BaseCoder_FreshSolid(Self->BaseCoder);
};

long long int StreamEncoder_Encode(PStreamEncoder Self, PStream aStream, PFillBuffer aFillBuffer, long long int Size, unsigned int *CRC)
{
                  *CRC = (unsigned int)-1;
      long long result = 0;
  unsigned char Symbol = 0;

  PReadStream Source = ReadStream_Malloc(aStream, aFillBuffer);

  RangeEncoder_StartEncode(Self->RangeEncoder);
  while (result < Size)
  {
    Symbol = ReadStream_Read(Source);
    BaseCoder_UpdateSymbol(Self->BaseCoder, Symbol);
    *CRC = UpdateCrc32(*CRC, Symbol);
    result++;

    // if ((result & DefaultTickStepSize) == 0)
      // if (StreamCoder->OnTick != 0)
	    // if (StreamCoder->OnTick(StreamCoder->Tick)) break;
  }
  RangeEncoder_FinishEncode(Self->RangeEncoder);
  WriteStream_FlushBuffer(Self->Stream);

  ReadStream_Free(Source);
  return result;
};

/* TStreamDecoder struct/methods implementation */

struct TStreamDecoder {
        PReadStream Stream;
      PRangeDecoder RangeDecoder;
         PBaseCoder BaseCoder;
};

PStreamDecoder StreamDecoder_Malloc(PStream aStream, PFillBuffer aFillBuffer)
{
  PStreamDecoder Self = malloc(sizeof(struct TStreamDecoder));

  Self->Stream       = ReadStream_Malloc(aStream, aFillBuffer);
  Self->RangeDecoder = RangeDecoder_Malloc(Self->Stream);
  Self->BaseCoder    = BaseCoder_Malloc(Self->RangeDecoder, (PUpdateSymbol)RangeDecoder_UpdateSymbol);

  return Self;
};

void StreamDecoder_Free(PStreamDecoder Self)
{
  BaseCoder_Free(Self->BaseCoder);
  RangeDecoder_Free(Self->RangeDecoder);
  ReadStream_Free(Self->Stream);
  free(Self);
};

void StreamDecoder_SetDictionaryLevel(PStreamDecoder Self, unsigned int Value)
{
  BaseCoder_SetDictionary(Self->BaseCoder, Value);
};

void StreamDecoder_SetTableParameters(PStreamDecoder Self, const TTableParameters *Value)
{
  BaseCoder_SetTable(Self->BaseCoder, Value);
};

void StreamDecoder_FreshFlexible(PStreamDecoder Self)
{
  BaseCoder_FreshFlexible(Self->BaseCoder);
};

void StreamDecoder_FreshSolid(PStreamDecoder Self)
{
  BaseCoder_FreshSolid(Self->BaseCoder);
};

long long int StreamDecoder_Decode(PStreamDecoder Self, PStream aStream, PFlushBuffer aFlushBuffer, long long int Size, unsigned int *CRC)
{
  /*
                   CRC = (unsigned int)-1;
          int64 result = 0;
  unsigned char Symbol = 0;

  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;

  TWriteStream* Dest = new TWriteStream(StrmPtr, StreamCoder->OnFlush);

  StreamCoder->Stream->ClearBuffer();
  StreamCoder->SecondaryCodec->Start();
  while (result < Size)
  {
    Symbol = StreamCoder->PPM->UpdateModel(0);
	Dest->Write(Symbol);
    UpdCrc32(CRC, Symbol);
    result++;

    if ((result & DefaultTickStepSize) == 0)
	  if (StreamCoder->OnTick != 0)
	    if (StreamCoder->OnTick(StreamCoder->Tick)) break;
  }
  StreamCoder->SecondaryCodec->Flush();
  (*Dest).~TWriteStream();
  return result;

  */
  return 0;
};

