/*
  Copyright (c) 2003-2011 Andrew Filinsky and Melchiorre Caruso

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

    Bee library exports.

  Modifyed:

    v1.0.0 build 0106 - 2009.08.12 by Melchiorre Caruso.
*/

#ifndef BEELIB_MAIN_H
#define BEELIB_MAIN_H

#include "beelib_types.h"

#define LIB_API extern "C" __declspec(dllexport)

LIB_API unsigned int DllVersion();

LIB_API void* CreateEncoder(void* StrmPtr, TFillEvent OnFillEv, TFlushEvent OnFlushEv, void* TickPtr, TTickEvent OnTickEv);
LIB_API void* CreateDecoder(void* StrmPtr, TFillEvent OnFillEv, TFlushEvent OnFlushEv, void* TickPtr, TTickEvent OnTickEv);
LIB_API void  DestroyCoder (void* Handle);

LIB_API void SetDictionaryLevel(void* Handle, signed int Value);
LIB_API void SetTableParameters(void* Handle, const TTableParameters& Value);

LIB_API void FreshFlexible(void* Handle);
LIB_API void FreshSolid   (void* Handle);

LIB_API int64 Encode(void* Handle, void* StrmPtr, const int64 Size, unsigned int& CRC);
LIB_API int64 Decode(void* Handle, void* StrmPtr, const int64 Size, unsigned int& CRC);

#endif //  BEELIB_MAIN_H