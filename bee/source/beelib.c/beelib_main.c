#include <stdlib.h>
#include "beelib_main.h"
#include "beelib_modeller.h"
#include "beelib_rangecoder.h"

#define DefaultTickStepSize 0xFFFF

unsigned int BeeVersion()
{
  return 102;
};

/* TBeeEncoder struct/methods implementation */

struct TBeeEncoder {
        PTicker Ticker;
           void *TickHandle;
  PRangeEncoder RangeEncoder;
     PBaseCoder BaseCoder;
};

PBeeEncoder BeeEncoder_Create(void *aStream, PWriter aWriter)
{
  PBeeEncoder Self    = malloc(sizeof(struct TBeeEncoder));
  Self->RangeEncoder  = RangeEncoder_Create(aStream, aWriter);
  Self->BaseCoder     = BaseCoder_Create(Self->RangeEncoder, (PUpdate)RangeEncoder_Update);

  Self->TickHandle    = 0;
  Self->Ticker        = 0;

  return Self;
};

void* BeeEncoder_Destroy(PBeeEncoder Self)
{
  BaseCoder_Destroy(Self->BaseCoder);
  RangeEncoder_Destroy(Self->RangeEncoder);
  free(Self);
  return 0;
};

void BeeEncoder_SetDictionaryLevel(PBeeEncoder Self, uint32 Value)
{
  BaseCoder_SetDictionary(Self->BaseCoder, Value);
};

void BeeEncoder_SetTableParameters(PBeeEncoder Self, const TTableParameters *Value)
{
  BaseCoder_SetTable(Self->BaseCoder, Value);
};

void BeeEncoder_SetTicker(PBeeEncoder Self, void *aTickHandle, PTicker aTicker)
{
  Self->TickHandle = aTickHandle;
  Self->Ticker     = aTicker;
};

void BeeEncoder_FreshFlexible(PBeeEncoder Self)
{
  BaseCoder_FreshFlexible(Self->BaseCoder);
};

void BeeEncoder_FreshSolid(PBeeEncoder Self)
{
  BaseCoder_FreshSolid(Self->BaseCoder);
};

uint64 BeeEncoder_Encode(PBeeEncoder Self, void *aStream, PReader aReader, uint64 Size, uint32 *CRC)
{
           *CRC = 0xFFFFFFFF;
   uint8 Symbol = 0;
  uint64 result = 0;

  RangeEncoder_StartEncode(Self->RangeEncoder);
  while ((result < Size) && (aReader(aStream, &Symbol) == 1))
  {
    BaseCoder_Update(Self->BaseCoder, Symbol);
    *CRC = UpdateCRC32(*CRC, Symbol);
    result++;

    if ((result & DefaultTickStepSize) == 0)
      if (Self->Ticker != 0)
	    if (Self->Ticker(Self->TickHandle) != 0) break;
  }
  RangeEncoder_FinishEncode(Self->RangeEncoder);
  return result;
};

/* TBeeDecoder struct/methods implementation */

struct TBeeDecoder {
        PTicker Ticker;
           void *TickHandle;
  PRangeDecoder RangeDecoder;
     PBaseCoder BaseCoder;
};

PBeeDecoder BeeDecoder_Create(void *aStream, PReader aReader)
{
  PBeeDecoder Self   = malloc(sizeof(struct TBeeDecoder));
  Self->RangeDecoder = RangeDecoder_Create(aStream, aReader);
  Self->BaseCoder    = BaseCoder_Create(Self->RangeDecoder, (PUpdate)RangeDecoder_Update);

  Self->TickHandle   = 0;
  Self->Ticker       = 0;

  return Self;
};

void* BeeDecoder_Destroy(PBeeDecoder Self)
{
  BaseCoder_Destroy(Self->BaseCoder);
  RangeDecoder_Destroy(Self->RangeDecoder);
  free(Self);
  return 0;
};

void BeeDecoder_SetDictionaryLevel(PBeeDecoder Self, uint32 Value)
{
  BaseCoder_SetDictionary(Self->BaseCoder, Value);
};

void BeeDecoder_SetTableParameters(PBeeDecoder Self, const TTableParameters *Value)
{
  BaseCoder_SetTable(Self->BaseCoder, Value);
};

void BeeDecoder_SetTicker(PBeeDecoder Self, void *aTickHandle, PTicker aTicker)
{
  Self->TickHandle = aTickHandle;
  Self->Ticker     = aTicker;
};

void BeeDecoder_FreshFlexible(PBeeDecoder Self)
{
  BaseCoder_FreshFlexible(Self->BaseCoder);
};

void BeeDecoder_FreshSolid(PBeeDecoder Self)
{
  BaseCoder_FreshSolid(Self->BaseCoder);
};

uint64 BeeDecoder_Decode(PBeeDecoder Self, void *aStream, PWriter aWriter, uint64 Size, uint32 *CRC)
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

