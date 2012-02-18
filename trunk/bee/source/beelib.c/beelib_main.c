#include <stdlib.h>
#include "beelib_main.h"
#include "beelib_modeller.h"
#include "beelib_rangecoder.h"

unsigned int BeeVersion()
{
  return 103;
};

/* TBeeEncoder struct/methods implementation */

struct TBeeEncoder {
  PRangeEncoder RangeEncoder;
     PBaseCoder BaseCoder;
};

PBeeEncoder BeeEncoder_Create(void *aStream, PStreamWrite aStreamWrite)
{
  PBeeEncoder Self   = malloc(sizeof(struct TBeeEncoder));


  Self->RangeEncoder = RangeEncoder_Create(aStream, aStreamWrite);
  Self->BaseCoder    =    BaseCoder_Create(Self->RangeEncoder, (PUpdate)RangeEncoder_Update);
  return Self;
};

void BeeEncoder_Destroy(PBeeEncoder Self)
{
     BaseCoder_Destroy(Self->BaseCoder);
  RangeEncoder_Destroy(Self->RangeEncoder);
  free(Self);
};

void BeeEncoder_SetDictionary(PBeeEncoder Self, uint32 Level)
{
  BaseCoder_SetDictionary(Self->BaseCoder, Level);
};

void BeeEncoder_SetTable(PBeeEncoder Self, const TTableParameters *Table)
{
  BaseCoder_SetTable(Self->BaseCoder, Table);
};

void BeeEncoder_FreshFlexible(PBeeEncoder Self)
{
  BaseCoder_FreshFlexible(Self->BaseCoder);
};

void BeeEncoder_FreshSolid(PBeeEncoder Self)
{
  BaseCoder_FreshSolid(Self->BaseCoder);
};

void BeeEncoder_EncodeBegin(PBeeEncoder Self)
{
  RangeEncoder_StartEncode(Self->RangeEncoder);
};

void BeeEncoder_Encode(PBeeEncoder Self, char *Buffer, int32 BufSize)
{
  int32 I;
  for (I = 0; I < BufSize; I++)
  {
    BaseCoder_Update(Self->BaseCoder, Buffer[I]);
  }
};

void BeeEncoder_EncodeEnd(PBeeEncoder Self)
{
  RangeEncoder_FinishEncode(Self->RangeEncoder);
};

/* TBeeDecoder struct/methods implementation */

struct TBeeDecoder {
  PRangeDecoder RangeDecoder;
     PBaseCoder BaseCoder;
};

PBeeDecoder BeeDecoder_Create(void *aStream, PStreamRead aStreamRead)
{
  PBeeDecoder Self   = malloc(sizeof(struct TBeeDecoder));
  Self->RangeDecoder = RangeDecoder_Create(aStream, aStreamRead);
  Self->BaseCoder    =    BaseCoder_Create(Self->RangeDecoder, (PUpdate)RangeDecoder_Update);
  return Self;
};

void BeeDecoder_Destroy(PBeeDecoder Self)
{
     BaseCoder_Destroy(Self->BaseCoder);
  RangeDecoder_Destroy(Self->RangeDecoder);
  free(Self);
};

void BeeDecoder_SetDictionary(PBeeDecoder Self, uint32 Level)
{
  BaseCoder_SetDictionary(Self->BaseCoder, Level);
};

void BeeDecoder_SetTableParameters(PBeeDecoder Self, const TTableParameters *Table)
{
  BaseCoder_SetTable(Self->BaseCoder, Table);
};

void BeeDecoder_FreshFlexible(PBeeDecoder Self)
{
  BaseCoder_FreshFlexible(Self->BaseCoder);
};

void BeeDecoder_FreshSolid(PBeeDecoder Self)
{
  BaseCoder_FreshSolid(Self->BaseCoder);
};

void BeeDecoder_DecodeBegin(PBeeDecoder Self)
{
  RangeDecoder_StartDecode(Self->RangeDecoder);
};

void BeeDecoder_Decode(PBeeDecoder Self, char *Buffer, int32 BufSize)
{
  int32 I;
  for (I = 0; I < BufSize; I++)
  {
    Buffer[I] = BaseCoder_Update(Self->BaseCoder, 0);
  }
};

void BeeDecoder_DecodeEnd(PBeeDecoder Self)
{
  RangeDecoder_FinishDecode(Self->RangeDecoder);
};

