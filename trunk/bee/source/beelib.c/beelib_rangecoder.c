#include <stdlib.h>
#include "beelib_common.h"
#include "beelib_rangecoder.h"

/* Range coder const definitions */

#define TOP      16777216
#define NUM      4
#define THRES    4278190080

/* TRangeEncoder struct/methods implementation */

struct TRangeEncoder {
  PWriteStream FStream;
  unsigned int FRange;
  unsigned int FLow;
  unsigned int FCode;
  unsigned int FCarry;
  unsigned int FCache;
  unsigned int FFNum;
};

PRangeEncoder RangeEncoder_Create(PWriteStream aStream)
{
  PRangeEncoder Self = malloc(sizeof(struct TRangeEncoder));
  Self->FStream = aStream;
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

static inline void RangeEncoder_Encode(PRangeEncoder Self, unsigned int CumFreq,  unsigned int Freq,  unsigned int TotFreq)
{
  unsigned int Temp = Self->FLow;
  Self->FLow       += MulDiv(Self->FRange, CumFreq, TotFreq);
  Self->FCarry     += (unsigned int)(Self->FLow < Temp);
  Self->FRange      = MulDiv(Self->FRange, Freq, TotFreq);
  while (Self->FRange < TOP)
  {
    Self->FRange <<= 8;
    RangeEncoder_ShiftLow(Self);
  }
}

void RangeEncoder_FinishEncode(PRangeEncoder Self)
{
  int I;
  for (I = 0; I <= NUM; I++)
    RangeEncoder_ShiftLow(Self);
}

unsigned int RangeEncoder_UpdateSymbol(PRangeEncoder Self, TFreq Freq, unsigned int aSymbol)
{
  // Count CumFreq...
  unsigned int CumFreq = 0, I = 0;
  while (I < aSymbol)
  {
    CumFreq += Freq[I];
    I++;
  }

  // Count TotFreq...
  unsigned int TotFreq = CumFreq;
  I = FREQSIZE;
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
   PReadStream Stream;
  unsigned int Range;
  unsigned int Low;
  unsigned int Code;
  unsigned int Carry;
  unsigned int Cache;
  unsigned int FFNum;
};

PRangeDecoder RangeDecoder_Create(PReadStream aStream)
{
  PRangeDecoder Self = malloc(sizeof(struct TRangeDecoder));

  Self->Stream = aStream;
  return Self;
}

void* RangeDecoder_Destroy(PRangeDecoder Self)
{
  free(Self);
  return 0;
}

void RangeDecoder_StartDecode(PRangeDecoder Self)
{
  Self->Range = 0xFFFFFFFF;
  Self->Low   = 0;
  Self->FFNum = 0;
  Self->Carry = 0;

  int I;
  for (I = 0; I <= NUM; I++)
    Self->Code = (Self->Code << 8)
      + ReadStream_Read(Self->Stream);
}

void RangeDecoder_FinishDecode(PRangeDecoder Self)
{
  // nothing to do
}

unsigned int RangeDecoder_GetFreq(PRangeDecoder Self, unsigned int TotFreq)
{
  return MulDecDiv(Self->Code + 1, TotFreq, Self->Range);
}

void RangeDecoder_Decode(PRangeDecoder Self, unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq)
{
  Self->Code -= MulDiv(Self->Range, CumFreq, TotFreq);
  Self->Range = MulDiv(Self->Range,    Freq, TotFreq);

  while (Self->Range < TOP)
  {
    Self->Code  = (Self->Code  << 8) + ReadStream_Read((PReadStream)Self->Stream);
    Self->Range = (Self->Range << 8);
  }
}

unsigned int RangeDecoder_UpdateSymbol(PRangeDecoder Self, TFreq Freq, unsigned int aSymbol)
{
  unsigned int CumFreq = 0, TotFreq = 0, SumFreq = 0;

  // Count TotFreq...
  TotFreq = 0;

  aSymbol = FREQSIZE;
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
