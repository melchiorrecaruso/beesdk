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

*/

#ifndef BEELIB_RANGECODER_H
#define BEELIB_RANGECODER_H

#include "beelib_stream.h"

/* Array of Frequencyes */

typedef unsigned int *TFreq;
#define FREQSIZE 16

#define TOP      16777216
#define NUM      4
#define THRES    4278190080
#define MAXFREQ  16777215

typedef int (*PUpdateSymbol) (void*, void*, unsigned char);

 /* TRangeEncoder struct/methods */

typedef struct TRangeEncoder *PRangeEncoder;

  PRangeEncoder RangeEncoder_Malloc      (PWriteStream aStream);
           void RangeEncoder_Free        (PRangeEncoder Self);
           void RangeEncoder_StartEncode (PRangeEncoder Self);
           void RangeEncoder_FinishEncode(PRangeEncoder Self);
   unsigned int RangeEncoder_UpdateSymbol(PRangeEncoder Self, TFreq Freq, unsigned int aSymbol);

 /* TRangeDecoder struct/methods */

typedef struct TRangeDecoder *PRangeDecoder;

  PRangeDecoder RangeDecoder_Malloc      (PReadStream aStream);
           void RangeDecoder_Free        (PRangeDecoder Self);
           void RangeDecoder_StartDecode (PRangeDecoder Self);
           void RangeDecoder_FinishDecode(PRangeDecoder Self);
   unsigned int RangeDecoder_GetFreq     (PRangeDecoder Self, unsigned int TotFreq);
   unsigned int RangeDecoder_UpdateSymbol(PRangeDecoder Self, TFreq Freq, unsigned int aSymbol);

#endif //  BEELIB_RANGECODER_H
