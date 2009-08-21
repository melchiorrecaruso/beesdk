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

    Bee archiver library link.

  Modifyed:

    v1.0.5 build 1032 - 2009.04.25 by Melchiorre Caruso.
}

unit BeeLib_Link;

{$I compiler.inc}

interface

uses
  Classes,
  // ---
  Bee_Types;

const
  {$IFDEF MSWINDOWS}
  cApplicationLib = 'beelib.dll';
  {$ENDIF}
  {$IFDEF UNIX}
  cApplicationLib = 'beelib.so';
  {$ENDIF}

  function CoreLibVersion: integer; external cApplicationLib;

  // ---

  procedure CoreFreePChar(P: PChar);                   external cApplicationLib;
  procedure CoreFreePFileInfo(P: PFileInfo);           external cApplicationLib;
  procedure CoreFreePFileInfoExtra(P: PFileInfoExtra); external cApplicationLib;

  // ---

  function CoreCreate(aCommandLine: PChar): boolean; external cApplicationLib;
  function CoreDestroy: boolean;                     external cApplicationLib;
  function CoreExecute: boolean;                     external cApplicationLib;
  function CoreSuspended(aValue: boolean): boolean;  external cApplicationLib;
  function CoreTerminate: boolean;                   external cApplicationLib;

  // ---

  function CoreGetPriority: integer;                  external cApplicationLib;
  function CoreSetPriority(aValue: integer): boolean; external cApplicationLib;

  // ---

  function CoreGetSpeed: integer;     external cApplicationLib;
  function CoreGetMessage: PChar;     external cApplicationLib;
  function CoreGetMessages: PChar;    external cApplicationLib;
  function CoreGetPercentes: integer; external cApplicationLib;
  function CoreGetTotalTime: integer; external cApplicationLib;
  function CoreGetTotalSize: int64;   external cApplicationLib;
  function CoreGetTime: integer;      external cApplicationLib;
  function CoreGetSize: int64;        external cApplicationLib;
  function CoreGetCode: integer;      external cApplicationLib;
  function CoreGetStatus: integer;    external cApplicationLib;

  // ---

  function CoreGetRequest: PChar;                  external cApplicationLib;
  function CoreGetRequestItem: PFileInfo;          external cApplicationLib;
  function CoreSetRequest(aValue: PChar): boolean; external cApplicationLib;

  // ---

  function CoreGetItemsCount: integer;                    external cApplicationLib;
  function CoreGetItems(aIndex: integer): PFileInfoExtra; external cApplicationLib;

implementation

end.
