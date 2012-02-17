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

/* Contains:

*/

#ifndef BEELIB_STREAM_H
#define BEELIB_STREAM_H

#include "beelib_common.h"

/* TReadStream struct/methods implementation */

typedef struct TReadStream *PReadStream;

  PReadStream ReadStream_Create     (void *aStream, PStreamRead aStreamRead);
         void ReadStream_Destroy    (PReadStream Self);
         void ReadStream_ClearBuffer(PReadStream Self);
         void ReadStream_FillBuffer (PReadStream Self);
         char ReadStream_Read       (PReadStream Self);

/* TWriteStream struct/methods implementation */

typedef struct TWriteStream *PWriteStream;

  PWriteStream WriteStream_Create     (void *aStream, PStreamWrite aStreamWrite);
          void WriteStream_Destroy    (PWriteStream Self);
          void WriteStream_ClearBuffer(PWriteStream Self);
          void WriteStream_FlushBuffer(PWriteStream Self);
          void WriteStream_Write      (PWriteStream Self, char Data);

#endif // BEELIB_STREAM_H
