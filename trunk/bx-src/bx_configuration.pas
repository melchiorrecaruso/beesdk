{
  Copyright (c) 2013 Melchiorre Caruso.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

    Configuration support.

  Fist release:

    v1.0 build 0010 - 2013.09.11 by Melchiorre Caruso.

  Modifyed:

}

unit bx_Configuration;

{$I bx_compiler.inc}

interface

uses
  Classes,
  IniFiles,
  SysUtils;

// -------------------------------------------------------------------------- //
//  Configuration tables type                                                 //
// -------------------------------------------------------------------------- //

const
  TableSize = 20;
  TableCols = 2;

type
  TTableCol = array [0..TableSize] of longword;

  TTable = packed record
    Level: longword;
    T: array [0..TableCols - 1] of TTableCol;
  end;

  TTableParameters = array [1..SizeOf(TTable) div 4] of byte;

// -------------------------------------------------------------------------- //
//  Default table parameters                                                  //
// -------------------------------------------------------------------------- //

const
  DefaultDictionaryLevel: longword = $0003;
  DefaultTableParameters: TTableParameters =
    (  3, 163, 157,  65,  93, 117, 135, 109, 126, 252, 172, 252, 152, 227, 249,
     249, 253, 196,  27,  82,  93,  74, 182, 245,  40,  67,  77, 143, 133, 135,
     128, 155, 207, 177, 225, 251, 253, 248,  73,  35,  15, 107, 143);

  EmptyTableParameters: TTableParameters =
    (  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0);

// -------------------------------------------------------------------------- //
//  Configuration class                                                       //
// -------------------------------------------------------------------------- //

type
  TConfiguration = class(TIniFile);

implementation

end.