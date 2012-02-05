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

    Crc32 routine:

    Polynomial = x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 +
                 x^10 + x^8  + x^7  + x^5  + x^4  + x^2  + x + 1;
  Modifyed:

*/

#ifndef BEELIB_COMMON_H
#define BEELIB_COMMON_H

// ------------------------------------------------------------------ //
//  Configuration table types                                         //
// ------------------------------------------------------------------ //

#define uint64 long long unsigned int
#define  int64 long long   signed int

#define uint32      long unsigned int
#define  int32      long   signed int

#define uint8            unsigned char
#define  int8              signed char

// ------------------------------------------------------------------ //
//  Interface definitions                                             //
// ------------------------------------------------------------------ //

typedef int32 (*PTickSend)  (void*);
typedef int32 (*PStrmRead)  (void*, void*, int32);
typedef int32 (*PStrmWrite) (void*, void*, int32);

// ------------------------------------------------------------------ //
//  Configuration table types                                         //
// ------------------------------------------------------------------ //

#define TABLESIZE       20  // array [0..20]
#define TABLECOLS        1  // array [0.. 1]
#define TABLEPARAMETERS 42  // array [0..42]

typedef uint32 TTableCol[TABLESIZE + 1];

struct TTable{
  int32 Level;
  TTableCol T[TABLECOLS + 1];
};

typedef uint8 TTableParameters[TABLEPARAMETERS + 1];

// ------------------------------------------------------------------ //
//  Default table parameters                                          //
// ------------------------------------------------------------------ //

static const TTableParameters DefaultTableParameters = {
    3, 163, 157,  65,  93, 117, 135, 109, 126, 252, 172, 252, 152,
  227, 249, 249, 253, 196,  27,  82,  93,  74, 182, 245,  40,  67,
   77, 143, 133, 135, 128, 155, 207, 177, 225, 251, 253, 248,  73,
   35,  15, 107, 143};

// ------------------------------------------------------------------ //
//  Default dictionary level                                          //
// ------------------------------------------------------------------ //

static const int32 DefaultDictionaryLevel = 2;

// ------------------------------------------------------------------ //
//  CRC calculating routine                                           //
// ------------------------------------------------------------------ //

inline uint32 UpdateCRC32(uint32 aCRC, uint8 aSymbol);

// ------------------------------------------------------------------ //
//  Common routine                                                    //
// ------------------------------------------------------------------ //

inline uint32 MulDiv   (uint32 A, uint32 B, uint32 C);
inline uint32 MulDecDiv(uint32 A, uint32 B, uint32 C);

#endif // BEELIB_COMMON_H
