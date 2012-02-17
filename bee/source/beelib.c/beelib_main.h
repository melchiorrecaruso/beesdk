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

  PBeeEncoder BeeEncoder_Create       (void *aStream, PStreamWrite aStreamWrite);
         void BeeEncoder_Destroy      (PBeeEncoder Self);

         void BeeEncoder_SetTable     (PBeeEncoder Self, const TTableParameters *Table);
         void BeeEncoder_SetDictionary(PBeeEncoder Self, uint32 Level);

         void BeeEncoder_FreshFlexible(PBeeEncoder Self);
         void BeeEncoder_FreshSolid   (PBeeEncoder Self);

         void BeeEncoder_EncodeBegin  (PBeeEncoder Self);
         void BeeEncoder_Encode       (PBeeEncoder Self, char *Buffer, int32 BufSize);
         void BeeEncoder_EncodeEnd    (PBeeEncoder Self);

/* TStreamDecoder struct/methods */

typedef struct TBeeDecoder *PBeeDecoder;

  PBeeDecoder BeeDecoder_Create       (void* aStream, PStreamRead aStreamRead);
         void BeeDecoder_Destroy      (PBeeDecoder Self);

         void BeeDecoder_SetTable     (PBeeDecoder Self, const TTableParameters *Table);
         void BeeDecoder_SetDictionary(PBeeDecoder Self, uint32 Level);

         void BeeDecoder_FreshFlexible(PBeeDecoder Self);
         void BeeDecoder_FreshSolid   (PBeeDecoder Self);

         void BeeDecoder_DecodeBegin  (PBeeDecoder Self);
         void BeeDecoder_Decode       (PBeeDecoder Self, char *Buffer, int32 BufSize);
         void BeeDecoder_DecodeEnd    (PBeeDecoder Self);

#endif //  BEELIB_MAIN_H
