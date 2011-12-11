/*
  Copyright (c) 2005-2011 Andrew Filinsky and Melchiorre Caruso

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

    ReadStream/WriteStream struct/methods implementation.

  Modifyed:
*/

#ifndef BEELIB_STREAM_H
#define BEELIB_STREAM_H

#define DEFAULT_BUFFER_CAPACITY 65536

typedef void *PStream;
typedef int (*PFillBuffer ) (void*, void*, int);
typedef int (*PFlushBuffer) (void*, void*, int);

/* TReadStream struct/methods implementation */

typedef struct TReadStream *PReadStream;

  PReadStream ReadStream_Malloc     (PStream aStream, PFillBuffer aFillBuffer);
         void ReadStream_Free       (PReadStream Self);
         void ReadStream_ClearBuffer(PReadStream Self);
         void ReadStream_FillBuffer (PReadStream Self);
         char ReadStream_Read       (PReadStream Self);

/* TWriteStream struct/methods implementation */

typedef struct TWriteStream *PWriteStream;

  PWriteStream WriteStream_Malloc     (PStream aStream, PFillBuffer aFillBuffer);
          void WriteStream_Free       (PWriteStream Self);
          void WriteStream_ClearBuffer(PWriteStream Self);
          void WriteStream_FlushBuffer(PWriteStream Self);
          void WriteStream_Write      (PWriteStream Self, char Data);

#endif // BEELIB_STREAM_H


