{
  Copyright (c) 2013-2014 Melchiorre Caruso.

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

{
  Contains:

    TConfiguration class.

  Fist release:

    v1.0 build 2185 - 2014.01.12 by Melchiorre Caruso.

  Modifyed:

}

unit bx_configuration;

{$I bx_compiler.inc}

interface

uses
  IniFiles;

// -------------------------------------------------------------------------- //
//  Configuration tables type                                                 //
// -------------------------------------------------------------------------- //

const
  TABLESIZE       = 20; // array [0..20]
  TABLECOLS       =  1; // array [0.. 1]
  TABLEPARAMETERS = 42; // array [0..42]

type
  TTableCol = array [0..TABLESIZE] of longword;

  TTable = packed record
    Level: longword;
    T: array [0..TABLECOLS] of TTableCol;
  end;

  TTableParameters = array [0..TABLEPARAMETERS] of byte;

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