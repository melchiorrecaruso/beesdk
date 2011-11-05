/*
  Copyright (c) 1998 Dmitry Auzhin;
            (c) 1999-2010 Andrew Filinsky.

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

    Routines for CRC32 calculating (c) 1998 Dmitry Auzhin;
    Table is calculated by Sasha Leshinsky.

    (c) 1998 Dmitry Auzhin;
    (c) 1999-2005 Andrew Filinsky.

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
*/
#ifndef BEELIB_CRC_H
#define BEELIB_CRC_H

/* Crc32 calculating Routines */

void UpdCrc32(unsigned int& aCrc32, unsigned char aData );
unsigned int UpdateCrc32(const unsigned int aCrc32, unsigned char aData);

#endif //  BEELIB_CRC_H
