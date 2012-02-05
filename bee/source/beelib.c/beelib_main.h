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

/*
  Contains:

  Modifyed:

*/

#ifndef BEELIB_MAIN_H
#define BEELIB_MAIN_H

#include "beelib_common.h"
#include "beelib_stream.h"

unsigned int BeeVersion();

/* TBeeEncoder struct/methods */

typedef struct TBeeEncoder *PBeeEncoder;

  PBeeEncoder BeeEncoder_Create            (void *aStrmHandle, PStrmWrite aStrmWrite);
        void* BeeEncoder_Destroy           (PBeeEncoder Self);
         void BeeEncoder_SetDictionaryLevel(PBeeEncoder Self, uint32 Value);
         void BeeEncoder_SetTableParameters(PBeeEncoder Self, const TTableParameters *Value);
         void BeeEncoder_SetTicker         (PBeeEncoder Self, void *aTickHandle, PTickSend aTickSend);
         void BeeEncoder_FreshFlexible     (PBeeEncoder Self);
         void BeeEncoder_FreshSolid        (PBeeEncoder Self);
       uint64 BeeEncoder_Encode            (PBeeEncoder Self, void *aStrmHandle, PStrmRead aStrmRead, uint64 Size, uint32 *CRC);

/* TStreamDecoder struct/methods */

typedef struct TBeeDecoder *PBeeDecoder;

  PBeeDecoder BeeDecoder_Create            (void* aStrmHandle, PStrmRead aStrmRead);
        void* BeeDecoder_Destroy           (PBeeDecoder Self);
         void BeeDecoder_SetDictionaryLevel(PBeeDecoder Self, uint32 Value);
         void BeeDecoder_SetTableParameters(PBeeDecoder Self, const TTableParameters *Value);
         void BeeDecoder_SetTicker         (PBeeDecoder Self, void *aTickHandle, PTickSend aTickSend);
         void BeeDecoder_FreshFlexible     (PBeeDecoder Self);
         void BeeDecoder_FreshSolid        (PBeeDecoder Self);
       uint64 BeeDecoder_Decode            (PBeeDecoder Self, void *aStrmHandle, PStrmWrite aStrmWrite, uint64 Size, uint32 *CRC);

#endif //  BEELIB_MAIN_H
