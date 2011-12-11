#include "beelib_codec.h"
#include <stdlib.h>

/* TSecondaryEncoder struct/methods implementation */

PSecondaryEncoder SecondaryEncoder_Mallac(PWriteStream aStream)
{
  return RangeEncoder_Malloc(aStream);
}

void SecondaryEncoder_Free(PSecondaryEncoder Self)
{
  RangeEncoder_Free(Self);
}

void SecondaryEncoder_StartEncode(PSecondaryEncoder Self)
{
  RangeEncoder_StartEncode(Self);
}

void SecondaryEncoder_FinishEncode(PSecondaryEncoder Self)
{
  RangeEncoder_FinishEncode(Self);
}

unsigned int SecondaryEncoder_UpdateSymbol(PSecondaryEncoder Self, TFreq Freq, unsigned int aSymbol)
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

/* TSecondaryDecoder */

PSecondaryDecoder SecondaryDecoder_Malloc(PReadStream aStream)
{
  return RangeDecoder_Malloc(aStream);
}

void SecondaryDecoder_Free(PSecondaryDecoder Self)
{
  RangeDecoder_Free(Self);
}

void SecondaryDecoder_StartDecode(PSecondaryDecoder Self)
{
  RangeDecoder_StartDecode(Self);
}

void SecondaryDecoder_FinishDecode(PSecondaryDecoder Self)
{
  RangeDecoder_FinishDecode(Self);
}

unsigned int SecondaryDecoder_UpdateSymbol(PSecondaryDecoder Self, TFreq Freq, unsigned int aSymbol)
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
