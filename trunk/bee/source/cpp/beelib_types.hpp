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

    Types definitions.

  Modifyed:
*/

#ifndef BEELIB_TYPES_H
#define BEELIB_TYPES_H

// ------------------------------------------------------------------ //
//  Configuration table types                                         //
// ------------------------------------------------------------------ //

const unsigned int DefaultDictionaryLevel = 0x0002;

// enum Table {TableSize = 20, TableCols = 2, TableParameters = 43};

static const int TableSize       = 21;
static const int TableCols       = 2;
static const int TableParameters = 43;

typedef   signed long long  int64;
typedef unsigned long long uint64;

typedef unsigned int TTableCol [TableSize];

typedef struct TTable {
  unsigned int Level;
  TTableCol T [TableCols];
} TTable;

typedef unsigned char TTableParameters [TableParameters];

const TTableParameters DefaultTableParameters = {3, 163, 157,  65,  93,
  117, 135, 109, 126, 252, 172, 252, 152, 227, 249, 249, 253, 196,  27,
   82,  93,  74, 182, 245,  40,  67,  77, 143, 133, 135, 128, 155, 207,
  177, 225, 251, 253, 248,  73,  35,  15, 107, 143};

// ------------------------------------------------------------------ //
//  Library interface type                                            //
// ------------------------------------------------------------------ //

static const int64 DefaultTickStepSize = 0xFFFF;

typedef int ( * TTickEvent  )( void* );
typedef int ( * TFillEvent  )( void*, void*, int );
typedef int ( * TFlushEvent )( void*, void*, int );

#endif // BEELIB_TYPES_H
