/*
  Copyright (c) 1999-2011 Andrew Filinsky

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the free software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT any WARRANTY; WITHOUT even the implied WARRANTY of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License FOR more details.

  you should have received A copy of the GNU General Public License
  along with This program; if not, write to the free software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
  Contains:

    TSecondaryCodec class,   abstract secondary codec,
      similar to RangeCoder or Arithmetic Coder;
    TSecondaryEncoder class, implementation of secondary encoder;
    TSecondaryDecoder class, implementation of secondary decoder;

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;

    v0.8.0 build 1400 - 2011.08.04 by Melchiorre Caruso.
*/

#ifndef BEELIB_CODEC_H
#define BEELIB_CODEC_H

#include "beelib_rangecoder.h"

/* Array of Frequencyes */

#define FREQSIZE 16
typedef unsigned int *TFreq;

/* TSecondaryEncoder struct/methods */

typedef struct TRangeEncoder *PSecondaryEncoder;

  PSecondaryEncoder SecondaryEncoder_Malloc      (PWriteStream aStream);
               void SecondaryEncoder_Free        (PSecondaryEncoder Self);
               void SecondaryEncoder_StartEncode (PSecondaryEncoder Self);
               void SecondaryEncoder_FinishEncode(PSecondaryEncoder Self);
       unsigned int SecondaryEncoder_UpdateSymbol(PSecondaryEncoder Self, TFreq Freq, unsigned int aSymbol);

/* TSecondaryDecoder struct methods */

typedef struct TRangeDecoder *PSecondaryDecoder;

  PSecondaryDecoder SecondaryDecoder_Malloc      (PReadStream aStream);
               void SecondaryDecoder_Free        (PSecondaryDecoder Self);
               void SecondaryDecoder_StartDecode (PSecondaryDecoder Self);
               void SecondaryDecoder_FinishDecode(PSecondaryDecoder Self);
       unsigned int SecondaryDecoder_UpdateSymbol(PSecondaryDecoder Self, TFreq Freq, unsigned int aSymbol);

#endif //  BEELIB_CODEC_H
