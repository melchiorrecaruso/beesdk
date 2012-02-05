#include <stdlib.h>
#include "beelib_main.h"
#include "beelib_modeller.h"
#include "beelib_rangecoder.h"

#define DefaultTickStepSize 0xFFFF

unsigned int BeeVersion()
{
  return 103;
};

/* TBeeEncoder struct/methods implementation */

struct TBeeEncoder {
          void *TickHandle;
      PTickSend TickSend;
   PWriteStream WriteStream;
  PRangeEncoder RangeEncoder;
     PBaseCoder BaseCoder;
};

PBeeEncoder BeeEncoder_Create(void *aStrmHandle, PStrmWrite aStrmWrite)
{
  PBeeEncoder Self   = malloc(sizeof(struct TBeeEncoder));
  Self->WriteStream  =  WriteStream_Create(aStrmHandle, aStrmWrite);
  Self->RangeEncoder = RangeEncoder_Create(Self->WriteStream);
  Self->BaseCoder    =    BaseCoder_Create(Self->RangeEncoder, (PUpdate)RangeEncoder_Update);

  Self->TickHandle   = 0;
  Self->TickSend     = 0;

  return Self;
};

void* BeeEncoder_Destroy(PBeeEncoder Self)
{
     BaseCoder_Destroy(Self->BaseCoder);
  RangeEncoder_Destroy(Self->RangeEncoder);
   WriteStream_Destroy(Self->WriteStream);
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

void BeeEncoder_SetTicker(PBeeEncoder Self, void *aTickHandle, PTickSend aTickSend)
{
  Self->TickHandle = aTickHandle;
  Self->TickSend   = aTickSend;
};

void BeeEncoder_FreshFlexible(PBeeEncoder Self)
{
  BaseCoder_FreshFlexible(Self->BaseCoder);
};

void BeeEncoder_FreshSolid(PBeeEncoder Self)
{
  BaseCoder_FreshSolid(Self->BaseCoder);
};

uint64 BeeEncoder_Encode(PBeeEncoder Self, void *aStrmHandle, PStrmRead aStrmRead, uint64 Size, uint32 *CRC)
{
           *CRC = 0xFFFFFFFF;
   uint8 Symbol = 0;
  uint64 result = 0;

  PReadStream Source = ReadStream_Create(aStrmHandle, aStrmRead);
  RangeEncoder_StartEncode(Self->RangeEncoder);
  while (result < Size)
  {
    Symbol = ReadStream_Read(Source);
    BaseCoder_Update(Self->BaseCoder, Symbol);

    *CRC = UpdateCRC32(*CRC, Symbol);
    result++;

    if ((result & DefaultTickStepSize) == 0)
      if (Self->TickSend != 0)
	    if (Self->TickSend(Self->TickHandle) != 0) break;
  }
  RangeEncoder_FinishEncode(Self->RangeEncoder);
  ReadStream_Destroy(Source);
  return result;
};

/* TBeeDecoder struct/methods implementation */

struct TBeeDecoder {
          void *TickHandle;
      PTickSend TickSend;
    PReadStream ReadStream;
  PRangeDecoder RangeDecoder;
     PBaseCoder BaseCoder;
};

PBeeDecoder BeeDecoder_Create(void *aStrmHandle, PStrmRead aStrmRead)
{
  PBeeDecoder Self   = malloc(sizeof(struct TBeeDecoder));
  Self->ReadStream   =   ReadStream_Create(aStrmHandle, aStrmRead);
  Self->RangeDecoder = RangeDecoder_Create(Self->ReadStream);
  Self->BaseCoder    =    BaseCoder_Create(Self->RangeDecoder, (PUpdate)RangeDecoder_Update);

  Self->TickHandle   = 0;
  Self->TickSend     = 0;

  return Self;
};

void* BeeDecoder_Destroy(PBeeDecoder Self)
{
     BaseCoder_Destroy(Self->BaseCoder);
  RangeDecoder_Destroy(Self->RangeDecoder);
    ReadStream_Destroy(Self->ReadStream);
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

void BeeDecoder_SetTicker(PBeeDecoder Self, void *aTickHandle, PTickSend aTickSend)
{
  Self->TickHandle = aTickHandle;
  Self->TickSend   = aTickSend;
};

void BeeDecoder_FreshFlexible(PBeeDecoder Self)
{
  BaseCoder_FreshFlexible(Self->BaseCoder);
};

void BeeDecoder_FreshSolid(PBeeDecoder Self)
{
  BaseCoder_FreshSolid(Self->BaseCoder);
};

uint64 BeeDecoder_Decode(PBeeDecoder Self, void *aStrmHandle, PStrmWrite aStrmWrite, uint64 Size, uint32 *CRC)
{
           *CRC = 0xFFFFFFFF;
   uint8 Symbol = 0;
  uint64 result = 0;

  PWriteStream Dest = WriteStream_Create(aStrmHandle, aStrmWrite);
  RangeDecoder_StartDecode(Self->RangeDecoder);
  while (result < Size)
  {
    Symbol = BaseCoder_Update(Self->BaseCoder, 0);
    WriteStream_Write(Dest, Symbol);

	*CRC = UpdateCRC32(*CRC, Symbol);
    result++;

    if ((result & DefaultTickStepSize) == 0)
      if (Self->TickSend != 0)
	    if (Self->TickSend(Self->TickHandle) != 0) break;
  }
  RangeDecoder_FinishDecode(Self->RangeDecoder);
  WriteStream_Destroy(Dest);
  return result;
};

