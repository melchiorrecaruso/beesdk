#include "beelib_assembler.h"
#include "beelib_rangecoder.h"
#include <stdlib.h>

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
  Self->Stream = 0;
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

  Self->Low = Self->Low << 8;
  return;
}

void RangeEncoder_Encode(PRangeEncoder Self, unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq)
{
  unsigned int Tmp = Self->Low;
  Self->Low       += MulDiv(Self->Range, CumFreq, TotFreq);
  Self->Carry     += (unsigned int) (Self->Low < Tmp);
  Self->Range      = MulDiv(Self->Range, Freq, TotFreq);
  while (Self->Range < TOP)
  {
    Self->Range = Self->Range << 8;
    RangeEncoder_ShiftLow(Self);
  }
  return;
}

void RangeEncoder_FinishEncode(PRangeEncoder Self)
{
  int I;
  for (I = 0; I <= NUM; I++)
    RangeEncoder_ShiftLow(Self);
  return;
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
  Self->Stream = 0;
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

unsigned int RangeDecoder_GetFreq(PRangeDecoder Self, unsigned int TotFreq)
{
  // return MulDecDiv(Code + 1, TotFreq, Range);
  return
    (unsigned int) ((unsigned long long)(Self->Code + 1) *
    (unsigned long long)TotFreq / (unsigned long long)Self->Range);
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

void RangeDecoder_FinishDecode(PRangeDecoder Self)
{

}
