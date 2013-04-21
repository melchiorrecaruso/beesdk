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

/*
  Contains:

  Modifyed:

*/

#ifndef LIBBX_PPMD_MODELLER_H
#define LIBBX_PPMD_MODELLER_H

#include "libbx_ppmd_rangecoder.h"
#include "libbx_stream.h"
#include "stdint.h"

typedef struct TPpmdModeller *PPpmdModeller;

  PPpmdModeller PpmdModeller_Create ();
           void PpmdModeller_Destroy(PPpmdModeller Self);
           void PpmdModeller_Start  (PPpmdModeller Self, uint32_t MemLev, uint32_t ModOrd);

        int32_t PpmdModeller_Encode (PPpmdModeller Self, PPpmdRangeEnc RangeEnc, uint8_t *Buffer, int32_t BufSize);
        int32_t PpmdModeller_Decode (PPpmdModeller Self, PPpmdRangeDec RangeDec, uint8_t *Buffer, int32_t BufSize);

#endif /* LIBBX_PPMD_MODELLER_H */
