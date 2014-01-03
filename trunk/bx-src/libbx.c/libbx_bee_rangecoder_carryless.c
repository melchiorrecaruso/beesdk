#include "libbx_bee_rangecoder.h"
#include <stdlib.h>

/* Range coder const definitions */

#define TOP     16777216L
#define THRES   4278190080U

/* TRangeEncoder struct/methods implementation */

struct TBeeRangeEnc {
  PWriteStream FStream;
     uint32_t FCode;
     uint32_t FRange;
     uint32_t FFNum;
     uint32_t FCache;
     uint64_t FLow;
};

PBeeRangeEnc BeeRangeEnc_Create(void *aStream, PStreamWrite aStreamWrite)
{
  PBeeRangeEnc Self = malloc(sizeof(struct TBeeRangeEnc));
  Self->FStream     = WriteStream_Create(aStream, aStreamWrite);
  return Self;
}

void BeeRangeEnc_Destroy(PBeeRangeEnc Self)
{
  WriteStream_Destroy(Self->FStream);
  free(Self);
  return;
}

void BeeRangeEnc_StartEncode(PBeeRangeEnc Self)
{
  Self->FLow   = 0;
  Self->FFNum  = 0;
  Self->FCache = 0;
  Self->FRange = 0xFFFFFFFF;
  return;
}

static inline void BeeRangeEnc_ShiftLow(PBeeRangeEnc Self)
{
  if ((Self->FLow >> 24) != 0xFF)
  {
    WriteStream_Write(Self->FStream, Self->FCache + (Self->FLow >> 32));

    int32_t C = 0xFF + (Self->FLow >> 32);
    while (Self->FFNum)
    {
      WriteStream_Write(Self->FStream, C);
      Self->FFNum--;
    }
    Self->FCache = (uint32_t)Self->FLow >> 24;
  } else
    Self->FFNum++;

  Self->FLow = (uint32_t)Self->FLow << 8;
  return;
}

static inline void BeeRangeEnc_Encode(PBeeRangeEnc Self, uint32_t CumFreq,  uint32_t Freq,  uint32_t TotFreq)
{
  Self->FLow   += CumFreq * (Self->FRange /= TotFreq);
  Self->FRange *= Freq;
  while (Self->FRange < TOP)
  {
    BeeRangeEnc_ShiftLow(Self);
    Self->FRange <<= 8;
  }
}

void BeeRangeEnc_FinishEncode(PBeeRangeEnc Self)
{
  Self->FLow += 1;
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);

  WriteStream_FlushBuffer(Self->FStream);
  return;
}

inline uint32_t BeeRangeEnc_Update(PBeeRangeEnc Self, TFreq Freq, uint32_t aSymbol)
{
  // Count CumFreq...
  uint32_t CumFreq = 0, I = 0;
  while (I < aSymbol)
  {
    CumFreq += Freq[I];
    I++;
  }
  // Count TotFreq...
  uint32_t TotFreq = CumFreq;
  I = TFREQSIZE;
  do
  {
    I--;
    TotFreq += Freq[I];
  }
  while (!(I == aSymbol));
  // Encode...
  BeeRangeEnc_Encode(Self, CumFreq, Freq[aSymbol], TotFreq);
  // Return result...
  return aSymbol;
}

/* TRangeDecoder struct/methods implementation */

struct TBeeRangeDec {
  PReadStream FStream;
    uint32_t FCode;
    uint32_t FRange;
    uint32_t FFNum;
    uint32_t FCache;
    uint64_t FLow;
};

PBeeRangeDec BeeRangeDec_Create(void *aStream, PStreamRead aStreamRead)
{
  PBeeRangeDec Self = malloc(sizeof(struct TBeeRangeDec));
  Self->FStream     = ReadStream_Create(aStream, aStreamRead);
  return Self;
}

void BeeRangeDec_Destroy(PBeeRangeDec Self)
{
  ReadStream_Destroy(Self->FStream);
  free(Self);
  return;
}

void BeeRangeDec_StartDecode(PBeeRangeDec Self)
{
  Self->FCode  = 0;
  Self->FRange = 0xFFFFFFFF;

  Self->FCode = (Self->FCode << 8) | ReadStream_Read(Self->FStream);
  Self->FCode = (Self->FCode << 8) | ReadStream_Read(Self->FStream);
  Self->FCode = (Self->FCode << 8) | ReadStream_Read(Self->FStream);
  Self->FCode = (Self->FCode << 8) | ReadStream_Read(Self->FStream);
  Self->FCode = (Self->FCode << 8) | ReadStream_Read(Self->FStream);
  return;
}

void BeeRangeDec_FinishDecode(PBeeRangeDec Self)
{
  ReadStream_ClearBuffer(Self->FStream);
  return;
}

inline uint32_t BeeRangeDec_GetFreq(PBeeRangeDec Self, uint32_t TotFreq)
{
  return Self->FCode / (Self->FRange /= TotFreq);
}

static inline void BeeRangeDec_Decode(PBeeRangeDec Self, uint32_t CumFreq, uint32_t Freq, uint32_t TotFreq)
{
  Self->FCode  -= CumFreq * Self->FRange;
  Self->FRange *= Freq;
  while (Self->FRange < TOP)
  {
    Self->FCode = (Self->FCode << 8) | ReadStream_Read(Self->FStream);
    Self->FRange <<= 8;
  }
  return;
}

inline uint32_t BeeRangeDec_Update(PBeeRangeDec Self, TFreq Freq, uint32_t aSymbol)
{
  uint32_t CumFreq = 0, TotFreq = 0, SumFreq = 0;
  // Count TotFreq...
  aSymbol = TFREQSIZE;
  do
  {
    aSymbol--;
    TotFreq += Freq[aSymbol];
  }
  while (!(aSymbol == 0));
  // Count CumFreq...
  CumFreq = BeeRangeDec_GetFreq(Self, TotFreq);
  // Search aSymbol...
  SumFreq = 0;
  aSymbol = 0;
  while (SumFreq + Freq[aSymbol] <= CumFreq)
  {
    SumFreq += Freq[aSymbol];
    aSymbol++;
  }

  // Finish Decode...
  BeeRangeDec_Decode(Self, SumFreq, Freq[aSymbol], TotFreq);

  // Return Result...
  return aSymbol;
}
