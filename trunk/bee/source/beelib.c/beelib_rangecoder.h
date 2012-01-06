/*
  Copyright (c) 2010-2012 Melchiorre Caruso

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

#include "beelib_common.h"

#define MAXFREQ   16777215
#define TFREQSIZE 16

/* Array of Frequencyes */

typedef uint32 *TFreq;

 /* TRangeEncoder struct/methods */

typedef struct TRangeEncoder *PRangeEncoder;

  PRangeEncoder  RangeEncoder_Create      (void *aStream, PWrite aWrite);
           void *RangeEncoder_Destroy     (PRangeEncoder Self);
           void  RangeEncoder_StartEncode (PRangeEncoder Self);
           void  RangeEncoder_FinishEncode(PRangeEncoder Self);
         uint32  RangeEncoder_Update      (PRangeEncoder Self, TFreq Freq, uint32 aSymbol);

 /* TRangeDecoder struct/methods */

typedef struct TRangeDecoder *PRangeDecoder;

  PRangeDecoder  RangeDecoder_Create      (void *aStream, PRead aRead);
           void *RangeDecoder_Destroy     (PRangeDecoder Self);
           void  RangeDecoder_StartDecode (PRangeDecoder Self);
           void  RangeDecoder_FinishDecode(PRangeDecoder Self);
         uint32  RangeDecoder_GetFreq     (PRangeDecoder Self, uint32 TotFreq);
         uint32  RangeDecoder_Update      (PRangeDecoder Self, TFreq Freq, uint32 aSymbol);

#endif //  BEELIB_RANGECODER_H
