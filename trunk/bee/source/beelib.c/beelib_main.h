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
#include "beelib_stream.h"

unsigned int DllVersion();

/* TStreamEncoder struct/methods */

typedef struct TStreamEncoder *PStreamEncoder;

 PStreamEncoder StreamEncoder_Malloc(PStream aStream, PFillBuffer aFillBuffer, PFlushBuffer aFlushBuffer);

           void StreamEncoder_Free              (PStreamEncoder Self);
           void StreamEncoder_SetDictionaryLevel(PStreamEncoder Self, unsigned int Value);
           void StreamEncoder_SetTableParameters(PStreamEncoder Self, TTableParameters *Value);
           void StreamEncoder_FreshFlexible     (PStreamEncoder Self);
           void StreamEncoder_FreshSolid        (PStreamEncoder Self);
  long long int StreamEncoder_Encode            (PStreamEncoder Self, PStream aStream, long long int Size, unsigned int *CRC);

/* TStreamDecoder struct/methods */

typedef struct TStreamDecoder *PStreamDecoder;

 PStreamDecoder StreamDecoder_Malloc(PStream aStream, PFillBuffer aStreamFill, PFlushBuffer aFlushBuffer);

           void StreamDecoder_Free              (PStreamDecoder Self);
           void StreamDecoder_SetDictionaryLevel(PStreamDecoder Self, unsigned int Value);
           void StreamDecoder_SetTableParameters(PStreamDecoder Self, TTableParameters *Value);
           void StreamDecoder_FreshFlexible     (PStreamDecoder Self);
           void StreamDecoder_FreshSolid        (PStreamDecoder Self);
  long long int StreamEncoder_Decode            (PStreamDecoder Self, PStream aStream, long long int Size, unsigned int *CRC);

#endif //  BEELIB_MAIN_H
