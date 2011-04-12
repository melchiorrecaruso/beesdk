{
  Copyright (c) 2003-2010 Andrew Filinsky and Melchiorre Caruso

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

    Bee archiver library link.

  Modifyed:

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit BeeLib_Link;

{$I compiler.inc}

interface

const
  {$IFDEF MSWINDOWS}
    cApplicationLib = 'beelib.dll';
  {$ENDIF}
  {$IFDEF UNIX}
    cApplicationLib = 'beelib.so';
  {$ENDIF}

function LibVersion: longint; external cApplicationLib;
function CoreCreate(P: pchar): pointer; external cApplicationLib;
function CoreSend  (ID: pointer; VALUE: longint): boolean; external cApplicationLib;
function CoreGetI32(ID: pointer; VALUE: longint): longint; external cApplicationLib;
function CoreSetI32(ID: pointer; VALUE: longint): longint; external cApplicationLib;
function CoreGetI64(ID: pointer; VALUE: longint): int64; external cApplicationLib;
function CoreGetPtr(ID: pointer; VALUE, INDEX: longint): pointer; external cApplicationLib;
function CoreSetPtr(ID: pointer; VALUE: longint; P: pointer): boolean; external cApplicationLib;



implementation

end.
