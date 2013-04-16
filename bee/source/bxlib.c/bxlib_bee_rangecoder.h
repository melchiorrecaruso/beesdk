/*
  Copyright (c) 2010-2013 Melchiorre Caruso

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

#include "bxlib_bee_common.h"
#include "bxlib_stream.h"

#define MAXFREQ   16777215
#define TFREQSIZE 16

/* Array of Frequencyes */

typedef uint32_t *TFreq;

/* TBeeRangeEnc struct/methods */

typedef struct TBeeRangeEnc *PBeeRangeEnc;

  PBeeRangeEnc BeeRangeEnc_Create      (void *aStream, PStreamWrite aStreamWrite);
          void BeeRangeEnc_Destroy     (PBeeRangeEnc Self);
          void BeeRangeEnc_StartEncode (PBeeRangeEnc Self);
          void BeeRangeEnc_FinishEncode(PBeeRangeEnc Self);
      uint32_t BeeRangeEnc_Update      (PBeeRangeEnc Self, TFreq Freq, uint32_t aSymbol);

 /* TBeeRangeDec struct/methods */

typedef struct TBeeRangeDec *PBeeRangeDec;

  PBeeRangeDec BeeRangeDec_Create      (void *aStream, PStreamRead aStreamRead);
          void BeeRangeDec_Destroy     (PBeeRangeDec Self);
          void BeeRangeDec_StartDecode (PBeeRangeDec Self);
          void BeeRangeDec_FinishDecode(PBeeRangeDec Self);
      uint32_t BeeRangeDec_GetFreq     (PBeeRangeDec Self, uint32_t TotFreq);
      uint32_t BeeRangeDec_Update      (PBeeRangeDec Self, TFreq Freq, uint32_t aSymbol);

/* BeeRangeCod_Update definition */

typedef uint32_t (*PRangeCod_Update) (void*, uint32_t*, uint32_t);

#endif //  BEELIB_RANGECODER_H
