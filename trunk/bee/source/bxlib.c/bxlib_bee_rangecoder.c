#include <stdlib.h>
#include "bxlib_bee_rangecoder.h"

/* Range coder const definitions */

#define TOP   16777216L
#define NUM   4L
#define THRES 4278190080U

/* TRangeEncoder struct/methods implementation */

struct TBeeRangeEnc {
  PWriteStream FStream;
      uint32_t FRange;
      uint32_t FLow;
      uint32_t FCode;
      uint32_t FCarry;
      uint32_t FCache;
      uint32_t FFNum;
};

PBeeRangeEnc BeeRangeEnc_Create(void *aStream, PStreamWrite aStreamWrite)
{
  PBeeRangeEnc Self = malloc(sizeof(struct TBeeRangeEnc));
  Self->FStream      = WriteStream_Create(aStream, aStreamWrite);
  return Self;
}

void BeeRangeEnc_Destroy(PBeeRangeEnc Self)
{
  WriteStream_Destroy(Self->FStream);
  free(Self);
}

void BeeRangeEnc_StartEncode(PBeeRangeEnc Self)
{
  Self->FRange = 0xFFFFFFFF;
  Self->FLow   = 0;
  Self->FFNum  = 0;
  Self->FCarry = 0;
}

static inline void RangeEncoder_ShiftLow(PBeeRangeEnc Self)
{
  if ((Self->FLow < THRES) || (Self->FCarry != 0))
  {
    WriteStream_Write(Self->FStream, Self->FCache + Self->FCarry);

    while (Self->FFNum != 0)
    {
      WriteStream_Write(Self->FStream, Self->FCarry - 1);
      Self->FFNum--;
    }
    Self->FCache = Self->FLow >> 24;
    Self->FCarry = 0;
  } else
    Self->FFNum++;

  Self->FLow <<= 8;
}

static inline void RangeEncoder_Encode(PBeeRangeEnc Self, uint32_t CumFreq,  uint32_t Freq,  uint32_t TotFreq)
{
  uint32_t Temp = Self->FLow;
  Self->FLow   += _MulDiv(Self->FRange, CumFreq, TotFreq);
  Self->FCarry += (uint32_t)(Self->FLow < Temp);
  Self->FRange  = _MulDiv(Self->FRange, Freq, TotFreq);
  while (Self->FRange < TOP)
  {
    Self->FRange <<= 8;
    RangeEncoder_ShiftLow(Self);
  }
}

void BeeRangeEnc_FinishEncode(PBeeRangeEnc Self)
{
  int32_t I;
  for (I = 0; I <= NUM; I++)
  {
    RangeEncoder_ShiftLow(Self);
  }
  WriteStream_FlushBuffer(Self->FStream);
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
  RangeEncoder_Encode(Self, CumFreq, Freq[aSymbol], TotFreq);
  // Return result...
  return aSymbol;
}

/* TRangeDecoder struct/methods implementation */

struct TBeeRangeDec {
  PReadStream FStream;
     uint32_t FRange;
     uint32_t FLow;
     uint32_t FCode;
     uint32_t FCarry;
     uint32_t FCache;
     uint32_t FFNum;
};

PBeeRangeDec BeeRangeDec_Create(void *aStream, PStreamRead aStreamRead)
{
  PBeeRangeDec Self = malloc(sizeof(struct TBeeRangeDec));
  Self->FStream      = ReadStream_Create(aStream, aStreamRead);
  return Self;
}

void BeeRangeDec_Destroy(PBeeRangeDec Self)
{
  ReadStream_Destroy(Self->FStream);
  free(Self);
}

void BeeRangeDec_StartDecode(PBeeRangeDec Self)
{
  Self->FRange = 0xFFFFFFFF;
  Self->FLow   = 0;
  Self->FFNum  = 0;
  Self->FCarry = 0;

  int32_t I;
  for (I = 0; I <= NUM; I++)
  {
    Self->FCode = (Self->FCode << 8) + ReadStream_Read(Self->FStream);
  }
}

void BeeRangeDec_FinishDecode(PBeeRangeDec Self)
{
  ReadStream_ClearBuffer(Self->FStream);
}

inline uint32_t BeeRangeDec_GetFreq(PBeeRangeDec Self, uint32_t TotFreq)
{
  return _MulDecDiv(Self->FCode + 1, TotFreq, Self->FRange);
}

static inline void RangeDecoder_Decode(PBeeRangeDec Self, uint32_t CumFreq, uint32_t Freq, uint32_t TotFreq)
{
  Self->FCode -= _MulDiv(Self->FRange, CumFreq, TotFreq);
  Self->FRange = _MulDiv(Self->FRange,    Freq, TotFreq);

  while (Self->FRange < TOP)
  {
    Self->FCode = (Self->FCode << 8) + ReadStream_Read(Self->FStream);
    Self->FRange <<= 8;
  }
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
  aSymbol = SumFreq;
  while (SumFreq + Freq[aSymbol] <= CumFreq)
  {
    SumFreq += Freq[aSymbol];
    aSymbol++;
  }

  // Finish Decode...
  RangeDecoder_Decode(Self, SumFreq, Freq[aSymbol], TotFreq);

  // Return Result...
  return aSymbol;
}
