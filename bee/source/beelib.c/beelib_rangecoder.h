/*
  Copyright (c) 2003 Evgeny Shelwien;
  Copyright (c) 2003-2011 Andrew Filinsky

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

/* Contains:

  TRangeCoder class,

    kind of RangeCoder
      (based on Eugeny Shelwien's shindler_var1
        (based on Shindler's RangeCoder), MaxFreq = 2^24);
    it uses MulDiv opcode extension.

  (c) 2003 Evgeny Shelwien;
  (c) 2003-2010 Andrew Filinsky.
  (c) 2010-2011 Melchiorre Caruso.

  Created:

    v0.1.0 build 0001 - 2003.02.01 by Evgeny Shelwien.

  Translated:

    v0.1.1 build 0002 - 2003.03.01 by Andrew Filinsky.

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
    v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;

    v0.8.0 build 1100 - 2011.08.02 by Melchiorre Caruso.
*/

#ifndef BEELIB_RANGECODER_H
#define BEELIB_RANGECODER_H

#define TOP     16777216
#define NUM     4
#define THRES   4278190080
#define MAXFREQ 16777215

#include "beelib_stream.h"

 /* TRangeEncoder struct/methods */

typedef struct TRangeEncoder* PRangeEncoder;

  PRangeEncoder RangeEncoder_Malloc      (PWriteStream aStream);
           void RangeEncoder_Free        (PRangeEncoder Self);
           void RangeEncoder_StartEncode (PRangeEncoder Self);
           void RangeEncoder_FinishEncode(PRangeEncoder Self);
           void RangeEncoder_Encode      (PRangeEncoder Self, unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq);

 /* TRangeDecoder struct/methods */

typedef struct TRangeDecoder* PRangeDecoder;

  PRangeDecoder RangeDecoder_Malloc      (PReadStream aStream);
           void RangeDecoder_Free        (PRangeDecoder Self);
           void RangeDecoder_StartDecode (PRangeDecoder Self);
           void RangeDecoder_FinishDecode(PRangeDecoder Self);
   unsigned int RangeDecoder_GetFreq     (PRangeDecoder Self, unsigned int TotFreq);
           void RangeDecoder_Decode      (PRangeDecoder Self, unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq);

#endif //  BEELIB_RANGECODER_H