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
function CoreExecute(HANDLE: pointer): boolean; external cApplicationLib;
function CoreSuspend(HANDLE: pointer; VALUE: boolean): boolean; external cApplicationLib;
function CoreTerminate(HANDLE: pointer): boolean; external cApplicationLib;
function CoreDestroy(HANDLE: pointer): boolean; external cApplicationLib;

function CoreGetStatus(HANDLE: pointer): longint; external cApplicationLib;
function CoreGetCode(HANDLE: pointer): longint; external cApplicationLib;
function CoreGetSpeed(HANDLE: pointer): longint; external cApplicationLib;
function CoreGetPercentage(HANDLE: pointer): longint; external cApplicationLib;
function CoreGetElapsedTime(HANDLE: pointer): longint; external cApplicationLib;
function CoreGetRemainingTime(HANDLE: pointer): longint; external cApplicationLib;

procedure CoreSetPriority(HANDLE: pointer; VALUE: longint); external cApplicationLib;
function CoreGetPriority(HANDLE: pointer): longint; external cApplicationLib;

function CoreGetSize(HANDLE: pointer): int64; external cApplicationLib;
function CoreGetProcessedSize(HANDLE: pointer): int64; external cApplicationLib;

function CoreGetMessage(HANDLE: pointer): pchar; external cApplicationLib;
function CoreGetMessages(HANDLE: pointer; INDEX: longint): pchar; external cApplicationLib;
function CoreGetMessageCount(HANDLE: pointer): longint; external cApplicationLib;

function CoreGetItem(HANDLE: pointer): pointer; external cApplicationLib;
function CoreGetItems(HANDLE: pointer; INDEX: longint): pointer; external cApplicationLib;
function CoreGetItemCount(HANDLE: pointer): longint; external cApplicationLib;

function CoreGetItemPending(HANDLE: pointer; STATUS: longint): pointer; external cApplicationLib;
procedure CoreSetItemPending(HANDLE: pointer; STATUS: longint; VALUE: pointer); external cApplicationLib;

implementation

end.
