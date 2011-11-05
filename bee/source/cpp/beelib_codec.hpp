/*
  Copyright (c) 1999-2011 Andrew Filinsky

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

    TSecondaryCodec class,   abstract secondary codec,
      similar to RangeCoder or Arithmetic Coder;
    TSecondaryEncoder class, implementation of secondary encoder;
    TSecondaryDecoder class, implementation of secondary decoder;

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;

    v0.8.0 build 1400 - 2011.08.04 by Melchiorre Caruso.
*/

#ifndef BEELIB_CODEC_H
#define BEELIB_CODEC_H

#include "beelib_rangecoder.h"

/* Array of Frequencyes */

typedef unsigned int* TFreq;

static const unsigned int TFreqSize = 16;

/* Abstract secondary codec, like A RangeCoder or Arithmetic Coder */

class TSecondaryCodec: public TRangeCoder
{
public:
            TSecondaryCodec(TStream* Stream);
  virtual ~TSecondaryCodec();
  virtual void Start() = 0;
  virtual void Flush() = 0;
  virtual unsigned int UpdateSymbol(unsigned int Freq0, unsigned int Freq1, unsigned int aSymbol) = 0;
  virtual unsigned int UpdateSymbol(const TFreq& Freq, unsigned int aSymbol) = 0;
};

 /* Range encoder */

class TSecondaryEncoder: public TSecondaryCodec
{
public:
            TSecondaryEncoder(TStream* Stream);
  virtual ~TSecondaryEncoder();
  virtual void Start();
  virtual void Flush();
  virtual unsigned int UpdateSymbol( unsigned int Freq0, unsigned int Freq1, unsigned int aSymbol);
  virtual unsigned int UpdateSymbol( const TFreq& Freq, unsigned int aSymbol);
};

 /* Range decoder */

class TSecondaryDecoder: public TSecondaryCodec
{
public:
            TSecondaryDecoder(TStream* Stream);
  virtual ~TSecondaryDecoder();
  virtual void Start();
  virtual void Flush();
  virtual unsigned int UpdateSymbol(unsigned int Freq0, unsigned int Freq1, unsigned int aSymbol);
  virtual unsigned int UpdateSymbol(const TFreq& Freq, unsigned int aSymbol);
};

#endif //  BEELIB_CODEC_H
