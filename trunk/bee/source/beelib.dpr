{
  Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

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

    Bee library exports.

  Modifyed:

    v1.0.0 build 0106 - 2009.08.12 by Melchiorre Caruso.
}

library BeeLib;

{$I compiler.inc}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} cThreads, {$ENDIF}
  Bee_Library;

 // -------------------------------------------------------------------------- //
 //                                                                            //
 //  Library core routines exported                                            //
 //                                                                            //
 // -------------------------------------------------------------------------- //

exports
  LibVersion,
  CoreCreate,
  CoreSend,
  CoreGetI32,
  CoreSetI32,
  CoreGetI64,
  CoreGetPtr,
  CoreSetPtr;

  // -------------------------------------------------------------------------- //
  //                                                                            //
  //  Library main block                                                        //
  //                                                                            //
  // -------------------------------------------------------------------------- //

begin
  // initialization code
end.
