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

/* Contains:

    Assembly routines.

  Modifyed:

    v0.7.9 build 0383 - 2007.06.27 by Andrew Filinsky;

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
*/

#ifndef BEELIB_ASSEMBLER_H
#define BEELIB_ASSEMBLER_H

#include "beelib_types.h"

// void FillLongword (void* Data, unsigned int Count, unsigned int Value);
// void AddLongword  (void* Data, unsigned int Count, unsigned int Value);
// void ClearLongword(void* Data, unsigned int Count );
// void MoveLongwordUnchecked(void* Source, void* Dest, unsigned int Count);

unsigned int MulDiv(unsigned int A, unsigned int B, unsigned int C);
unsigned int MulDecDiv(unsigned int A, unsigned int B, unsigned int C);

#endif //  BEELIB_ASSEMBLER_H
