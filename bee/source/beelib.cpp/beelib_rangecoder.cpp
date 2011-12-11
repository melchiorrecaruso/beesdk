#include "beelib_rangecoder.hpp"

TRangeCoder::TRangeCoder(TStream* Stream)
{
  FStream = Stream;
}

TRangeCoder::~TRangeCoder()
{
  FStream = 0;
}

void TRangeCoder::StartEncode()
{
  Range = 0xFFFFFFFF;
  Low   = 0;
  FFNum = 0;
  Carry = 0;
}

void TRangeCoder::StartDecode()
{
  StartEncode();
  for (unsigned int I = 0; I <= NUM; I++)
    Code = (Code << 8) + FStream->Read();
}


void TRangeCoder::FinishEncode()
{
  for (unsigned int I = 0; I <= NUM; I++)
    ShiftLow();
}

void TRangeCoder::FinishDecode()
{
  /* nothing to do */
}

void TRangeCoder::Encode(unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq)
{
  unsigned int Tmp = Low;
  Low   += MulDiv(Range, CumFreq, TotFreq);
  Carry += (unsigned int) (Low < Tmp);
  Range = MulDiv(Range, Freq, TotFreq);
  while (Range < TOP)
  {
    Range = Range << 8;
    ShiftLow();
  }
}


void TRangeCoder::Decode(unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq)
{
  // Code -= MulDiv(Range, CumFreq, TotFreq);
  Code -= (unsigned int) ((uint64)Range * (uint64)CumFreq / (uint64)TotFreq);

  // Range = MulDiv(Range, Freq, TotFreq);
  Range = (unsigned int) ((uint64)Range * (uint64)Freq / (uint64)TotFreq);

  while (Range < TOP)
  {
    Code  = (Code  << 8) + FStream->Read();
    Range = (Range << 8);
  }
}


unsigned int TRangeCoder::GetFreq(unsigned int TotFreq)
{
  // return MulDecDiv(Code + 1, TotFreq, Range);
  return (unsigned int) ((uint64)(Code + 1) * (uint64)TotFreq / (uint64)Range);
}

void TRangeCoder::ShiftLow()
{
  if ((Low < Thres) || (Carry != 0))
  {
    FStream->Write(Cache + Carry);
    while (FFNum != 0)
    {
      FStream->Write(Carry - 1);
      FFNum--;
    }
    Cache = Low >> 24;
    Carry = 0;
  }
  else
    FFNum++;
  Low = Low << 8;
}
