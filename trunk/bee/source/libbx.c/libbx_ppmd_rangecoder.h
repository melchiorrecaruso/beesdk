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

#ifndef LIBBX_PPMD_RANGECODER_H
#define LIBBX_PPMD_RANGECODER_H

#include "libbx_stream.h"
#include "stdint.h"

/* TPpmdRangeEncoder struct/methods */

typedef struct TPpmdRangeEnc *PPpmdRangeEnc;

  PPpmdRangeEnc PpmdRangeEnc_Create      (void *aStream, PStreamWrite aStreamWrite);
           void PpmdRangeEnc_Destroy     (PPpmdRangeEnc Self);
           void PpmdRangeEnc_StartEncode (PPpmdRangeEnc Self);
           void PpmdRangeEnc_FinishEncode(PPpmdRangeEnc Self);

           void PpmdRangeEnc_Encode      (PPpmdRangeEnc Self, uint32_t start, uint32_t size, uint32_t total);
           void PpmdRangeEnc_EncodeBit_0 (PPpmdRangeEnc Self, uint32_t size0);
           void PpmdRangeEnc_EncodeBit_1 (PPpmdRangeEnc Self, uint32_t size0);

/* TPpmdRangeDecoder struct/methods */

typedef struct TPpmdRangeDec *PPpmdRangeDec;

  PPpmdRangeDec PpmdRangeDec_Create      (void *aStream, PStreamRead aStreamRead);
           void PpmdRangeDec_Destroy     (PPpmdRangeDec Self);
           void PpmdRangeDec_StartDecode (PPpmdRangeDec Self);
           void PpmdRangeDec_FinishDecode(PPpmdRangeDec Self);

          void PpmdRangeDec_Decode      (PPpmdRangeDec Self, uint32_t start, uint32_t size);
      uint32_t PpmdRangeDec_GetThreshold(PPpmdRangeDec Self, uint32_t total);
      uint32_t PpmdRangeDec_DecodeBit(PPpmdRangeDec Self, uint32_t size0);

#endif /* LIBBX_PPMD_RANGECODER_H */

