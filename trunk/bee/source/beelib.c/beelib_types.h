/*
  Copyright (c) 2010-2011 Melchiorre Caruso

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

  Modifyed:

*/

#ifndef BEELIB_TYPES_H
#define BEELIB_TYPES_H

// ------------------------------------------------------------------ //
//  Configuration table types                                         //
// ------------------------------------------------------------------ //

#define TABLESIZE       21
#define TABLECOLS        2
#define TABLEPARAMETERS 43

typedef unsigned int TTableCol[TABLESIZE]; /* [0..20] */

struct TTable{
  int Level;
  TTableCol T[TABLECOLS]; /* [0..1] */
};

typedef unsigned char TTableParameters[TABLEPARAMETERS]; /* [0..42] */

// ------------------------------------------------------------------ //
//  Default table parameters                                          //
// ------------------------------------------------------------------ //

const TTableParameters DefaultTableParameters = {3, 163, 157,  65,  93,
  117, 135, 109, 126, 252, 172, 252, 152, 227, 249, 249, 253, 196,  27,
   82,  93,  74, 182, 245,  40,  67,  77, 143, 133, 135, 128, 155, 207,
  177, 225, 251, 253, 248,  73,  35,  15, 107, 143};

// ------------------------------------------------------------------ //
//  Default dictionary level                                          //
// ------------------------------------------------------------------ //

static const int DefaultDictionaryLevel = 2;

// ------------------------------------------------------------------ //
//  Default tick step size                                            //
// ------------------------------------------------------------------ //

static const int DefaultTickStepSize    = 0xFFFF;

#endif // BEELIB_TYPES_H
