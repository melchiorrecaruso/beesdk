#include "libbx_ppmd_rangecoder.h"
#include "libbx_ppmd_common.h"
#include <stdlib.h>
#include <memory.h>

struct TPpmdRangeEnc {
  uint64_t Low;
  uint32_t Range;
   uint8_t Cache;
  uint64_t CacheSize;
  PWriteStream Stream;
};

struct TPpmdRangeDec {
  uint32_t Range;
  uint32_t Code;
  PReadStream Stream;
};

#define kTopValue (1 << 24)

void PpmdRangeEnc_Init(PPpmdRangeEnc Self)
{
  Self->Low       = 0;
  Self->Range     = 0xFFFFFFFF;
  Self->Cache     = 0;
  Self->CacheSize = 1;
}

static void PpmdRangeEnc_ShiftLow(PPpmdRangeEnc Self)
{
  if ((uint32_t)Self->Low < (uint32_t)0xFF000000 || (unsigned)(Self->Low >> 32) != 0)
  {
    uint8_t temp = Self->Cache;
    do
    {
      WriteStream_Write(Self->Stream, (uint8_t)(temp + (uint8_t)(Self->Low >> 32)));
      temp = 0xFF;
    }
    while(--Self->CacheSize != 0);
    Self->Cache = (uint8_t)((uint32_t)Self->Low >> 24);
  }
  Self->CacheSize++;
  Self->Low = (uint32_t)Self->Low << 8;
}

void PpmdRangeEnc_Encode(PPpmdRangeEnc Self, uint32_t start, uint32_t size, uint32_t total)
{
  Self->Low += start * (Self->Range /= total);
  Self->Range *= size;
  while (Self->Range < kTopValue)
  {
    Self->Range <<= 8;
    PpmdRangeEnc_ShiftLow(Self);
  }
}

void PpmdRangeEnc_EncodeBit_0(PPpmdRangeEnc Self, uint32_t size0)
{
  Self->Range = (Self->Range >> 14) * size0;
  while (Self->Range < kTopValue)
  {
    Self->Range <<= 8;
    PpmdRangeEnc_ShiftLow(Self);
  }
}

void PpmdRangeEnc_EncodeBit_1(PPpmdRangeEnc Self, uint32_t size0)
{
  uint32_t newBound = (Self->Range >> 14) * size0;
  Self->Low += newBound;
  Self->Range -= newBound;
  while (Self->Range < kTopValue)
  {
    Self->Range <<= 8;
    PpmdRangeEnc_ShiftLow(Self);
  }
}

void PpmdRangeEnc_FlushData(PPpmdRangeEnc Self)
{
  unsigned i;
  for (i = 0; i < 5; i++)
    PpmdRangeEnc_ShiftLow(Self);
}

PPpmdRangeEnc PpmdRangeEnc_Create (void *aStream, PStreamWrite aStreamWrite)
{
  PPpmdRangeEnc Self = malloc(sizeof(struct TPpmdRangeEnc));
  Self->Stream = WriteStream_Create(aStream, aStreamWrite);
  return Self;
};

void PpmdRangeEnc_Destroy(PPpmdRangeEnc Self)
{
  WriteStream_Destroy(Self->Stream);
  free(Self);
};

void PpmdRangeEnc_StartEncode(PPpmdRangeEnc Self)
{
  PpmdRangeEnc_Init(Self);
};

void PpmdRangeEnc_FinishEncode(PPpmdRangeEnc Self)
{
  PpmdRangeEnc_FlushData(Self);
  WriteStream_FlushBuffer(Self->Stream);
};




#define PpmdRangeDec_IsFinishedOK(Self) ((Self)->Code == 0)

int PpmdRangeDec_Init(PPpmdRangeDec Self)
{
  unsigned i;
  Self->Code  = 0;
  Self->Range = 0xFFFFFFFF;
  if (ReadStream_Read(Self->Stream) != 0)
    return FALSE;
  for (i = 0; i < 4; i++)
    Self->Code = (Self->Code << 8) | ReadStream_Read(Self->Stream);
  return (Self->Code < 0xFFFFFFFF);
}

uint32_t PpmdRangeDec_GetThreshold(PPpmdRangeDec Self, uint32_t total)
{
  return (Self->Code) / (Self->Range /= total);
}

static void PPmdRangeDec_Normalize(PPpmdRangeDec Self)
{
  if (Self->Range < kTopValue)
  {
    Self->Code = (Self->Code << 8) | ReadStream_Read(Self->Stream);
    Self->Range <<= 8;
    if (Self->Range < kTopValue)
    {
      Self->Code = (Self->Code << 8) | ReadStream_Read(Self->Stream);
      Self->Range <<= 8;
    }
  }
}

void PpmdRangeDec_Decode(PPpmdRangeDec Self, uint32_t start, uint32_t size)
{
  Self->Code  -= start * Self->Range;
  Self->Range *= size;
  PPmdRangeDec_Normalize(Self);
}

uint32_t PpmdRangeDec_DecodeBit(PPpmdRangeDec Self, uint32_t size0)
{
  uint32_t newBound = (Self->Range >> 14) * size0;
  uint32_t symbol;
  if (Self->Code < newBound)
  {
    symbol = 0;
    Self->Range = newBound;
  }
  else
  {
    symbol = 1;
    Self->Code -= newBound;
    Self->Range -= newBound;
  }
  PPmdRangeDec_Normalize(Self);
  return symbol;
}

PPpmdRangeDec PpmdRangeDec_Create (void *aStream, PStreamRead aStreamRead)
{
  PPpmdRangeDec Self = malloc(sizeof(struct TPpmdRangeDec));
  Self->Stream = ReadStream_Create(aStream, aStreamRead);
  return Self;
};

void PpmdRangeDec_Destroy(PPpmdRangeDec Self)
{
  ReadStream_Destroy(Self->Stream);
  free(Self);
};

void PpmdRangeDec_StartDecode(PPpmdRangeDec Self)
{
  PpmdRangeDec_Init(Self);
};

void PpmdRangeDec_FinishDecode(PPpmdRangeDec Self)
{
  ReadStream_ClearBuffer(Self->Stream);
};
