#include "beelib_codec.h"

/* TSecondaryCodec */

TSecondaryCodec::TSecondaryCodec(TStream* Stream): TRangeCoder(Stream)
{

}

TSecondaryCodec::~TSecondaryCodec()
{

}

/* TSecondaryEncoder */

TSecondaryEncoder::TSecondaryEncoder(TStream* Stream): TSecondaryCodec(Stream)
{

}

TSecondaryEncoder::~TSecondaryEncoder()
{

}

/* TSecondaryDecoder */

TSecondaryDecoder::TSecondaryDecoder(TStream* Stream): TSecondaryCodec(Stream)
{

}

TSecondaryDecoder::~TSecondaryDecoder()
{

}


void TSecondaryEncoder::Start()
{
  StartEncode();
}

void TSecondaryEncoder::Flush()
{
  FinishEncode();
}

unsigned int TSecondaryEncoder::UpdateSymbol(unsigned int Freq0, unsigned int Freq1, unsigned int aSymbol)
{
  unsigned int result = 0;
  if (aSymbol == 0)
    Encode(0, Freq0, Freq0 + Freq1);
  else
    Encode(Freq0, Freq1, Freq0 + Freq1);
  result = aSymbol;
  return result;
}

unsigned int TSecondaryEncoder::UpdateSymbol(const TFreq& Freq, unsigned int aSymbol)
{
  unsigned int result  = 0;
  unsigned int CumFreq = 0, TotFreq = 0, I = 0;

  // Count CumFreq...
  CumFreq = 0;
  I = CumFreq;
  while (I < aSymbol)
  {
    CumFreq += Freq[I];
    I++;
  }
  // Count TotFreq...
  TotFreq = CumFreq;

  I = TFreqSize; // DA SISTEMARE

  do
  {
    I--;
    TotFreq += Freq[I];
  }
  while (!(I == aSymbol));
  // Encode...
  Encode(CumFreq, Freq[aSymbol], TotFreq);
  // Return Result...
  result = aSymbol;
  return result;
}

/* TSecondaryDecoder */


void TSecondaryDecoder::Start()
{
  StartDecode();
}

void TSecondaryDecoder::Flush()
{
  FinishDecode();
}

unsigned int TSecondaryDecoder::UpdateSymbol(unsigned int Freq0, unsigned int Freq1, unsigned int aSymbol)
{
  unsigned int result = 0;
  if (GetFreq(Freq0 + Freq1) < Freq0)
  {
    Decode(0, Freq0, Freq0 + Freq1);
    result = 0;
  }
  else
  {
    Decode(Freq0, Freq1, Freq0 + Freq1);
    result = 1;
  }
  return result;
}

unsigned int TSecondaryDecoder::UpdateSymbol(const TFreq& Freq, unsigned int aSymbol)
{
  unsigned int result = 0;
  unsigned int CumFreq = 0, TotFreq = 0, SumFreq = 0;
  // Count TotFreq...
  TotFreq = 0;

  aSymbol = TFreqSize; // DA SISTEMARE

  do
  {
    aSymbol--;
    TotFreq += Freq[aSymbol];
  }
  while (!(aSymbol == 0));
  // Count CumFreq...
  CumFreq = GetFreq(TotFreq);
  // Search aSymbol...
  SumFreq = 0;
  aSymbol = SumFreq;
  while (SumFreq + Freq[aSymbol] <= CumFreq)
  {
    SumFreq += Freq[aSymbol];
    aSymbol++;
  }
  // Finish Decode...
  Decode(SumFreq, Freq[aSymbol], TotFreq);
  // Return Result...
  result = aSymbol;
  return result;
}
