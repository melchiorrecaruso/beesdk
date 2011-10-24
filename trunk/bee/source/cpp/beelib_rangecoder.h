/*
  Copyright (c) 2003 Evgeny Shelwien;
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

  TRangeCoder class,

    kind of RangeCoder
      (based on Eugeny Shelwien's shindler_var1
        (based on Shindler's RangeCoder), MaxFreq = 2^24);
    it uses MulDiv opcode extension.

  (c) 2003 Evgeny Shelwien;
  (c) 2003-2010 Andrew Filinsky.
  (c) 2010-2011 Melchiorre Caruso.

  Created:

    v0.1.0 build 0001 - 2003.02.01 by Evgeny Shelwien.

  Translated:

    v0.1.1 build 0002 - 2003.03.01 by Andrew Filinsky.

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
    v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;

    v0.8.0 build 1100 - 2011.08.02 by Melchiorre Caruso.
*/

#ifndef BEELIB_RANGECODER_H
#define BEELIB_RANGECODER_H

#include "beelib_types.h"
#include "beelib_stream.h"
#include "beelib_assembler.h"


static const int TOP     = 1 << 24;
static const int NUM     = 4;
static const int Thres   = 255 * ((unsigned int) TOP);
static const int MaxFreq = TOP - 1;

 /* TRangeCoder */

class TRangeCoder
{
private:
      TStream* FStream;
  unsigned int Range;
  unsigned int Low;
  unsigned int Code;
  unsigned int Carry;
  unsigned int Cache;
  unsigned int FFNum;
  void ShiftLow();
 public:
           TRangeCoder(TStream* Stream);
  virtual ~TRangeCoder();
          void StartEncode();
          void StartDecode();
          void FinishEncode();
          void FinishDecode();
          void Encode (unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq);
  unsigned int GetFreq(unsigned int TotFreq);
          void Decode (unsigned int CumFreq, unsigned int Freq, unsigned int TotFreq);
};


#endif //  BEELIB_RANGECODER_H
