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

{$I compiler.inc}

interface

uses
  BeeLib_Configuration;

{$IFDEF MSWINDOWS}
  {$linklib libmsvcrt}
{$ENDIF}

{$IFDEF UNIX}
// {$linklib libm}
// {$linklib libgcc}
{$ENDIF}

{$link beelib.c\obj\release\beelib_common.o}
{$link beelib.c\obj\release\beelib_modeller.o}
{$link beelib.c\obj\release\beelib_rangecoder.o}
{$link beelib.c\obj\release\beelib_stream.o}

type
  TStreamRead  = function(Stream: pointer; Data: pointer; DataSize: longint): longint;
  TStreamWrite = function(Stream: pointer; Data: pointer; DataSize: longint): longint;

function  RangeEncoder_Create
  (aStream: pointer; aStreamWrite: TStreamWrite): pointer; cdecl; external;

procedure RangeEncoder_Destroy     (Self: pointer); cdecl; external;
procedure RangeEncoder_StartEncode (Self: pointer); cdecl; external;
procedure RangeEncoder_FinishEncode(Self: pointer); cdecl; external;

function  RangeDecoder_Create
   (aStream: pointer; aStreamRead: TStreamRead): pointer; cdecl; external;

procedure RangeDecoder_Destroy     (Self: pointer); cdecl; external;
procedure RangeDecoder_StartDecode (Self: pointer); cdecl; external;
procedure RangeDecoder_FinishDecode(Self: pointer); cdecl; external;

function  BaseCoder_Create       (aCoder: pointer): pointer; cdecl; external;
procedure BaseCoder_Destroy      (Self :pointer); cdecl; external;
procedure BaseCoder_SetTable     (Self: pointer; Table: pointer); cdecl; external;
procedure BaseCoder_SetDictionary(Self: pointer; Level: longint); cdecl; external;
procedure BaseCoder_FreshFlexible(Self: pointer); cdecl; external;
procedure BaseCoder_FreshSolid   (Self: pointer); cdecl; external;
procedure BaseCoder_Encode       (Self: pointer; Buffer: pointer; BufSize: longint); cdecl; external;
procedure BaseCoder_Decode       (Self: pointer; Buffer: pointer; BufSize: longint); cdecl; external;

implementation

end.
