/*
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

/*
  Contains:

    TBaseCoder class, PPM modeller;

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
    v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;

    v0.8.0 build 1400 - 2011.08.04 by Melchiorre Caruso.
*/

#ifndef BEELIB_MODELLER_H
#define BEELIB_MODELLER_H

#include "beelib_types.h"      // TTable, TTableCol, ...

typedef unsigned int (*PUpdateSymbol) (void*, unsigned int*, unsigned int);

/* TBaseCoder struct/methods */

typedef struct TBaseCoder *PBaseCoder;

    PBaseCoder BaseCoder_Malloc       (void *aCodec, PUpdateSymbol aUpdateSymbol);
          void BaseCoder_Free         (PBaseCoder Self);
          void BaseCoder_SetTable     (PBaseCoder Self, TTableParameters *T);
          void BaseCoder_SetDictionary(PBaseCoder Self, unsigned int aDictLevel);
          void BaseCoder_FreshFlexible(PBaseCoder Self);
          void BaseCoder_FreshSolid   (PBaseCoder Self);
          void BaseCoder_Free         (PBaseCoder Self);
  unsigned int BaseCoder_UpdateSymbol (PBaseCoder Self, unsigned int aSymbol);

#endif //  BEELIB_MODELLER_H
