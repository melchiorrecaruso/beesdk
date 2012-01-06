#include <stdlib.h>
#include "beelib_common.h"
#include "beelib_rangecoder.h"

/* Range coder const definitions */

#define TOP      16777216
#define NUM      4
#define THRES    4278190080

/* TRangeEncoder struct/methods implementation */

struct TRangeEncoder {
    void *FStream;
  PWrite FWrite;
  uint32 FRange;
  uint32 FLow;
  uint32 FCode;
  uint32 FCarry;
  uint32 FCache;
  uint32 FFNum;
};

PRangeEncoder RangeEncoder_Create(void *aStream, PWrite aWrite)
{
  PRangeEncoder Self = malloc(sizeof(struct TRangeEncoder));
  Self->FStream      = aStream;
  Self->FWrite       = aWrite;
  return Self;
}

void* RangeEncoder_Destroy(PRangeEncoder Self)
{
  free(Self);
  return 0;
}

void RangeEncoder_StartEncode(PRangeEncoder Self)
{
  Self->FRange = 0xFFFFFFFF;
  Self->FLow   = 0;
  Self->FFNum  = 0;
  Self->FCarry = 0;
}

static inline void RangeEncoder_ShiftLow(PRangeEncoder Self)
{
  if ((Self->FLow < THRES) || (Self->FCarry != 0))
  {
    Self->FWrite(Self->FStream, Self->FCache + Self->FCarry);

    while (Self->FFNum != 0)
    {
      Self->FWrite(Self->FStream, Self->FCarry - 1);
      Self->FFNum--;
    }
    Self->FCache = Self->FLow >> 24;
    Self->FCarry = 0;
  } else
    Self->FFNum++;

  Self->FLow <<= 8;
}

static inline void RangeEncoder_Encode(PRangeEncoder Self, uint32 CumFreq,  uint32 Freq,  uint32 TotFreq)
{
  uint32 Temp   = Self->FLow;
  Self->FLow   += MulDiv(Self->FRange, CumFreq, TotFreq);
  Self->FCarry += (uint32)(Self->FLow < Temp);
  Self->FRange  = MulDiv(Self->FRange, Freq, TotFreq);
  while (Self->FRange < TOP)
  {
    Self->FRange <<= 8;
    RangeEncoder_ShiftLow(Self);
  }
}

void RangeEncoder_FinishEncode(PRangeEncoder Self)
{
  int32 I;
  for (I = 0; I <= NUM; I++)
    RangeEncoder_ShiftLow(Self);
}

uint32 RangeEncoder_Update(PRangeEncoder Self, TFreq Freq, uint32 aSymbol)
{
  // Count CumFreq...
  uint32 CumFreq = 0, I = 0;
  while (I < aSymbol)
  {
    CumFreq += Freq[I];
    I++;
  }
  // Count TotFreq...
  uint32 TotFreq = CumFreq;
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

struct TRangeDecoder {
    void *FStream;
   PRead FRead;
  uint32 FRange;
  uint32 FLow;
  uint32 FCode;
  uint32 FCarry;
  uint32 FCache;
  uint32 FFNum;
};

PRangeDecoder RangeDecoder_Create(void *aStream, PRead aRead)
{
  PRangeDecoder Self = malloc(sizeof(struct TRangeDecoder));
  Self->FStream      = aStream;
  Self->FRead        = aRead;
  return Self;
}

void* RangeDecoder_Destroy(PRangeDecoder Self)
{
  free(Self);
  return 0;
}

void RangeDecoder_StartDecode(PRangeDecoder Self)
{
  Self->FRange = 0xFFFFFFFF;
  Self->FLow   = 0;
  Self->FFNum = 0;
  Self->FCarry = 0;

  uint8 Symbol;
  int32 I;
  for (I = 0; I <= NUM; I++)
  {
    Self->FRead(Self->FStream, &Symbol);
    Self->FCode = (Self->FCode << 8) + Symbol;
  }
}

void RangeDecoder_FinishDecode(PRangeDecoder Self)
{
  // nothing to do
}

uint32 RangeDecoder_GetFreq(PRangeDecoder Self, uint32 TotFreq)
{
  return MulDecDiv(Self->FCode + 1, TotFreq, Self->FRange);
}

void RangeDecoder_Decode(PRangeDecoder Self, uint32 CumFreq, uint32 Freq, uint32 TotFreq)
{
  Self->FCode -= MulDiv(Self->FRange, CumFreq, TotFreq);
  Self->FRange = MulDiv(Self->FRange,    Freq, TotFreq);

  uint8 Symbol;
  while (Self->FRange < TOP)
  {
    Self->FRead(Self->FStream, &Symbol);
    Self->FCode  <<= 8;
    Self->FCode   += Symbol;
    Self->FRange <<= 8;
  }
}

uint32 RangeDecoder_Update(PRangeDecoder Self, TFreq Freq, uint32 aSymbol)
{
  uint32 CumFreq = 0, TotFreq = 0, SumFreq = 0;

  // Count TotFreq...
  TotFreq = 0;
  aSymbol = TFREQSIZE;
  do
  {
    aSymbol--;
    TotFreq += Freq[aSymbol];
  }

  while (!(aSymbol == 0));
  // Count CumFreq...
  CumFreq = RangeDecoder_GetFreq(Self, TotFreq);
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