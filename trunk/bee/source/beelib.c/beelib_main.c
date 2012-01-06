#include <stdlib.h>
#include "beelib_main.h"
#include "beelib_modeller.h"
#include "beelib_rangecoder.h"

#define DefaultTickStepSize 0xFFFF

unsigned int LibVersion()
{
  return 102;
};

/* TStreamEncoder struct/methods implementation */

struct TStreamEncoder {
  PRangeEncoder RangeEncoder;
     PBaseCoder BaseCoder;
};

PStreamEncoder StreamEncoder_Create(void *aStream, PWrite aWrite)
{
  PStreamEncoder Self = malloc(sizeof(struct TStreamEncoder));
  Self->RangeEncoder  = RangeEncoder_Create(aStream, aWrite);
  Self->BaseCoder     = BaseCoder_Create(Self->RangeEncoder, (PUpdate)RangeEncoder_Update);
  return Self;
};

void* StreamEncoder_Destroy(PStreamEncoder Self)
{
  BaseCoder_Destroy(Self->BaseCoder);
  RangeEncoder_Destroy(Self->RangeEncoder);
  free(Self);
  return 0;
};

void StreamEncoder_SetDictionaryLevel(PStreamEncoder Self, uint32 Value)
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

uint64 StreamEncoder_Encode(PStreamEncoder Self, void *aStream, PRead aRead, uint64 Size, uint32 *CRC)
{
           *CRC = 0xFFFFFFFF;
   uint8 Symbol = 0;
  uint64 result = 0;

  RangeEncoder_StartEncode(Self->RangeEncoder);
  while ((result < Size) && (aRead(aStream, &Symbol) == 1))
  {
    BaseCoder_Update(Self->BaseCoder, Symbol);
    *CRC = UpdateCRC32(*CRC, Symbol);
    result++;

    if ((result & DefaultTickStepSize) == 0)
     if (Self->OnTick != 0)
	   if (StreamCoder->OnTick(StreamCoder->Tick) == 0) break;
  }
  RangeEncoder_FinishEncode(Self->RangeEncoder);
  return result;
};

/* TStreamDecoder struct/methods implementation */

struct TStreamDecoder {
      PRangeDecoder RangeDecoder;
         PBaseCoder BaseCoder;
};

PStreamDecoder StreamDecoder_Create(void *aStream, PRead aRead)
{
  PStreamDecoder Self = malloc(sizeof(struct TStreamDecoder));
  Self->RangeDecoder = RangeDecoder_Create(aStream, aRead);
  Self->BaseCoder    = BaseCoder_Create(Self->RangeDecoder, (PUpdate)RangeDecoder_Update);
  return Self;
};

void* StreamDecoder_Destroy(PStreamDecoder Self)
{
  BaseCoder_Destroy(Self->BaseCoder);
  RangeDecoder_Destroy(Self->RangeDecoder);
  free(Self);
  return 0;
};

void StreamDecoder_SetDictionaryLevel(PStreamDecoder Self, uint32 Value)
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

uint64 StreamDecoder_Decode(PStreamDecoder Self, void *aStream, PWrite aWrite, uint64 Size, uint32 *CRC)
{
  /*
                   CRC = 0xFFFFFFFF;
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

