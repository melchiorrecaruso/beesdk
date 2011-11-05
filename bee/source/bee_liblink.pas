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

unit Bee_LibLink;

{$link    \cpp\obj\release\beelib_main.o}
{$linklib \cpp\bin\release\beelib.dll}

{$linklib libmsvcrt}

interface

uses
  BeeLib_Interface,
  BeeLib_Configuration;

function  DllVersion: longword; cdecl; external;

function  CreateEncoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv:
            TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer; cdecl; external;

function  CreateDecoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv:
            TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer; cdecl; external;

procedure DestroyCoder(Handle: pointer); cdecl; external;

procedure SetDictionaryLevel(Handle: pointer; Value: longint); cdecl; external;
procedure SetTableParameters(Handle: pointer; const Value: TTableParameters); cdecl; external;

procedure FreshFlexible(Handle: pointer); cdecl; external;
procedure FreshSolid(Handle: pointer); cdecl; external;

function  Encode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64; cdecl; external;
function  Decode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64; cdecl; external;

implementation

end.
