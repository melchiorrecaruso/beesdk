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

    TStream class.

  Modifyed:
*/

#ifndef BEELIB_STREAM_H
#define BEELIB_STREAM_H

#include "beelib_types.hpp"


/* Default TStream buffer capacity */

const unsigned int DefaultBufferCapacity = 128 * 1024;

/* TStream class */

class TStream
{
protected:
          void* FHandle;
            int FBufferSize;
  unsigned char FBuffer [131072];
public:
           TStream(void* Handle);
  virtual ~TStream();
  virtual unsigned char Read()           = 0;
  virtual void Write(unsigned char Data) = 0;
  virtual void FillBuffer () = 0;
  virtual void FlushBuffer() = 0;
          void ClearBuffer();
};

/* TReadStream class */

class TReadStream: public TStream
{
private:
         int FBufferReaded;
  TFillEvent FOnFillEvent;
public:
           TReadStream(void* Handle, TFillEvent OnFillEvent);
  virtual ~TReadStream();
  virtual unsigned char Read();
  virtual void Write(unsigned char Data);
  virtual void FillBuffer ();
  virtual void FlushBuffer();
};

/* TWriteStream class */

class TWriteStream: public TStream
{
private:
  TFlushEvent FOnFlushEvent;
public:
           TWriteStream(void* Handle, TFlushEvent OnFlushEvent);
  virtual ~TWriteStream();
  virtual unsigned char Read();
  virtual void Write(unsigned char Data);
  virtual void FillBuffer ();
  virtual void FlushBuffer();
};

#endif // BEELIB_STREAM_H
