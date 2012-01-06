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

#ifndef BEELIB_MODELLER_H
#define BEELIB_MODELLER_H

#include "beelib_common.h"

/* UpdateSymbol routine definition */

typedef uint32 (*PUpdate) (void*, uint32*, uint32);

/* TBaseCoder struct/methods */

typedef struct TBaseCoder *PBaseCoder;

  PBaseCoder  BaseCoder_Create       (void *aCodec, PUpdate aUpdate);
        void *BaseCoder_Destroy      (PBaseCoder Self);
        void  BaseCoder_SetTable     (PBaseCoder Self, const TTableParameters *T);
        void  BaseCoder_SetDictionary(PBaseCoder Self, uint32 aDictLevel);
        void  BaseCoder_FreshFlexible(PBaseCoder Self);
        void  BaseCoder_FreshSolid   (PBaseCoder Self);
        void  BaseCoder_Free         (PBaseCoder Self);
      uint32  BaseCoder_Update       (PBaseCoder Self, uint32 aSymbol);

#endif //  BEELIB_MODELLER_H
