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


