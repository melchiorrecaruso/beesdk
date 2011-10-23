{
  Copyright (c) 2010-2011 Melchiorre Caruso

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

interface

uses
  BeeLib_Interface,
  BeeLib_Configuration;

const
  {$IFDEF MSWINDOWS}
    cApplicationLib = 'beelib.dll';
  {$ENDIF}
  {$IFDEF UNIX}
    cApplicationLib = 'beelib.so';
  {$ENDIF}

function  DllVersion: longword; {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;

function  CreateEncoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv: TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer; {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;
function  CreateDecoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv: TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer; {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;

procedure DestroyCoder(Handle: pointer); {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;

procedure SetDictionaryLevel(Handle: pointer; Value: longword);               {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;
procedure SetTableParameters(Handle: pointer; const Value: TTableParameters); {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;

procedure FreshFlexible(Handle: pointer); {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;
procedure FreshSolid(Handle: pointer);    {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;

function  Encode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64; {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;
function  Decode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64; {$IFDEF USECDLL} cdecl; {$ENDIF}  external cApplicationLib;

implementation

end.
