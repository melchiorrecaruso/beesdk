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

#ifndef BEELIB_MODELLER_H
#define BEELIB_MODELLER_H

#include "bxlib_bee_common.h"

/* TBaseCoder struct/methods */

typedef struct TBeeModeller *PBeeModeller;

  PBeeModeller BeeModeller_Create (void *aCodec);
          void BeeModeller_Destroy(PBeeModeller Self);
          void BeeModeller_Init   (PBeeModeller Self, uint32_t DictLevel, const TTableParameters *T);
      uint32_t BeeModeller_Encode (PBeeModeller Self, uint8_t *Buffer, uint32_t BufSize);
      uint32_t BeeModeller_Decode (PBeeModeller Self, uint8_t *Buffer, uint32_t BufSize);

#endif /* BEELIB_MODELLER_H */
