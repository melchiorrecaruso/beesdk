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

unsigned int BeeVersion();

typedef struct TBStream





/* TBeeEncoder struct/methods */

typedef struct TBeeEnc *PStreamEncoder;

  PStreamEncoder StreamEncoder_Create            (void *aStream, PWrite aWrite, void *aPtr, PTick aTick);
           void* StreamEncoder_Destroy           (PStreamEncoder Self);
            void StreamEncoder_SetDictionaryLevel(PStreamEncoder Self, uint32 Value);
            void StreamEncoder_SetTableParameters(PStreamEncoder Self, const TTableParameters *Value);
            void StreamEncoder_FreshFlexible     (PStreamEncoder Self);
            void StreamEncoder_FreshSolid        (PStreamEncoder Self);


          uint64 StreamEncoder_Encode            (PStreamEncoder Self, void *aStream, uint32 Size, uint32 *CRC);

/* TStreamDecoder struct/methods */

typedef struct TStreamDecoder *PStreamDecoder;

  PStreamDecoder StreamDecoder_Create            (void* aStream, PRead aRead);
           void* StreamDecoder_Destroy           (PStreamDecoder Self);
            void StreamDecoder_SetDictionaryLevel(PStreamDecoder Self, uint32 Value);
            void StreamDecoder_SetTableParameters(PStreamDecoder Self, const TTableParameters *Value);
            void StreamDecoder_FreshFlexible     (PStreamDecoder Self);
            void StreamDecoder_FreshSolid        (PStreamDecoder Self);
          uint64 StreamDecoder_Decode            (PStreamDecoder Self, void *aStream, PWrite aWrite, uint64 Size, uint32 *CRC);

#endif //  BEELIB_MAIN_H
