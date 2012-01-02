#include <stdlib.h>
#include "beelib_rangecoder.h"

static inline unsigned int MulDiv(unsigned int A, unsigned int B, unsigned int C)
{
  return (unsigned int)(((long long unsigned int)A *
                         (long long unsigned int)B)/
                         (long long unsigned int)C);
}

static inline unsigned int MulDecDiv(unsigned int A, unsigned int B, unsigned int C)
{
    return (unsigned int)((((long long unsigned int)A *
                            (long long unsigned int)B) - 1) /
                            (long long unsigned int)C);
}

/* TRangeEncoder struct/methods implementation */

struct TRangeEncoder {
  PWriteStream Stream;
  unsigned int Range;
  unsigned int Low;
  unsigned int Code;
  unsigned int Carry;
  unsigned int Cache;
  unsigned int FFNum;
};

PRangeEncoder RangeEncoder_Malloc(PWriteStream aStream)
{
  PRangeEncoder Self = malloc(sizeof(struct TRangeEncoder));

  Self->Stream = aStream;
  return Self;
}

void RangeEncoder_Free(PRangeEncoder Self)
{
  free(Self);
}

void RangeEncoder_StartEncode(PRangeEncoder Self)
{
  Self->Range = 0xFFFFFFFF;
  Self->Low   = 0;
  Self->FFNum = 0;
  Self->Carry = 0;
}

void RangeEncoder_ShiftLow(PRangeEncoder Self)
{
  if ((Self->Low < THRES) || (Self->Carry != 0))
  {
    WriteStream_Write(Self->Stream, Self->Cache + Self->Carry);
    while (Self->FFNum != 0)
    {
      WriteStream_Write(Self->Stream, Self->Carry - 1);
      Self->FFNum--;
    }
    Self->Cache = Self->Low >> 24;
    Self->Carry = 0;
  }
  else
    Self->FFNum++;

  Self->Low <<= 8;
  return;
}

void RangeEncoder_FinishEncode(PRangeEncoder Self)
{
  int I;
  for (I = 0; I <= NUM; I++)
    RangeEncoder_ShiftLow(Self);
  return;
}

void RangeEncoder_Encode(PRangeEncoder Self, unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq)
{
  unsigned int Tmp = Self->Low;
  Self->Low       += MulDiv(Self->Range, CumFreq, TotFreq);
   Self->Carry    += (unsigned int) (Self->Low < Tmp);
  Self->Range      = MulDiv(Self->Range, Freq, TotFreq);
  while (Self->Range < TOP)
  {
    Self->Range <<= 8;
    RangeEncoder_ShiftLow(Self);
  }
  return;
}

unsigned int RangeEncoder_UpdateSymbol(PRangeEncoder Self, TFreq Freq, unsigned int aSymbol)
{
  unsigned int CumFreq = 0, TotFreq = 0, I = 0;

  // Count CumFreq...
  I = CumFreq;
  while (I < aSymbol)
  {
    CumFreq += Freq[I];
    I++;
  }

  // Count TotFreq...
  TotFreq = CumFreq;

  I = FREQSIZE;
  do
  {
    I--;
    TotFreq += Freq[I];
  }
  while (!(I == aSymbol));

  // Encode...
  RangeEncoder_Encode(Self, CumFreq, Freq[aSymbol], TotFreq);

  // Return Result...
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

PRangeDecoder RangeDecoder_Malloc(PReadStream aStream)
{
  PRangeDecoder Self = malloc(sizeof(struct TRangeDecoder));

  Self->Stream = aStream;
  return Self;
}

void RangeDecoder_Free(PRangeDecoder Self)
{
  free(Self);
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
