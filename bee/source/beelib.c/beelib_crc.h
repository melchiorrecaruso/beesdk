/*
  Copyright (c) 2010-2012 Melchiorre Caruso.

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

    Routines for Crc32 calculating.

  Modifyed:

*/
#ifndef BEELIB_CRC_H
#define BEELIB_CRC_H

/* Crc32 calculating routine */

inline unsigned int UpdateCrc32(unsigned int aCrc32, unsigned char aSymbol);

#endif //  BEELIB_CRC_H
