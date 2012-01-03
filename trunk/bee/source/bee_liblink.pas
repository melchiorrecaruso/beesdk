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
  cTypes,
  BeeLib_Interface,
  BeeLib_Configuration;

{$IFDEF cppDLL}
  {$IFDEF MSWINDOWS}
    {$linklib libgcc}
    {$linklib libmsvcrt}
    {$linklib libm}
  {$ENDIF}

  {$IFDEF UNIX}
  {$ENDIF}

  {$link beelib.c\obj\release\beelib_common.o}
  {$link beelib.c\obj\release\beelib_stream.o}
  {$link beelib.c\obj\release\beelib_rangecoder.o}
  {$link beelib.c\obj\release\beelib_modeller.o}
  {$link beelib.c\obj\release\beelib_main.o}

  function DllVersion:cuint; cdecl; external;

  function  StreamEncoder_Malloc
    (aStream: pointer; aFlushBuffer: TFlushBuffer): pointer; cdecl; external;

  procedure StreamEncoder_Free
    (Self: pointer); cdecl; external;

  procedure StreamEncoder_SetDictionaryLevel
    (Self: pointer; Value: cuint); cdecl; external;

  procedure StreamEncoder_SetTableParameters
    (Self: pointer; Value: pointer); cdecl; external;

  procedure StreamEncoder_FreshFlexible
    (Self: pointer); cdecl; external;

  procedure StreamEncoder_FreshSolid
    (Self: pointer); cdecl; external;

  function  StreamEncoder_Encode
    (Self: pointer; aStream: pointer; aFillBuffer: TFillBuffer;
     Size: clonglong; CRC: pcuint): clonglong; cdecl; external;

  function StreamDecoder_Malloc
    (aStream:pointer; aFillBuffer:TFillBuffer):pointer; cdecl; external;

  procedure StreamDecoder_Free
    (Self: pointer); cdecl; external;

  procedure StreamDecoder_SetDictionaryLevel
    (Self: pointer; Value:cuint); cdecl; external;

  procedure StreamDecoder_SetTableParameters
    (Self: pointer; Value: pointer); cdecl; external;

  procedure StreamDecoder_FreshFlexible
    (Self: pointer); cdecl; external;

  procedure StreamDecoder_FreshSolid
    (Self: pointer); cdecl; external;

  function StreamDecoder_Decode
    (Self: pointer; aStream: pointer; aFlushBuffer: TFlushBuffer;
     Size: clonglong; CRC: pcuint): clonglong; cdecl; external;

{$ENDIF}

{$IFDEF fpcDLL}


const
  {$IFDEF MSWINDOWS}
   cApplicationLib = 'beelib.dll';
  {$ENDIF}
  {$IFDEF UNIX}
   cApplicationLib = 'beelib.so';
  {$ENDIF}

function  DllVersion: longword; external cApplicationLib;

function  CreateEncoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv:
            TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer;
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

function  CreateDecoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv:
            TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer;
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

procedure DestroyCoder(Handle: pointer);
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

procedure SetDictionaryLevel(Handle: pointer; Value: longint);
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

procedure SetTableParameters(Handle: pointer; const Value: TTableParameters);
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

procedure FreshFlexible(Handle: pointer);
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

procedure FreshSolid(Handle: pointer);
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

function  Encode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64;
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

function  Decode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64;
            {$IFDEF cppDLL} cdecl; {$ENDIF} external cApplicationLib;

{$ENDIF}

implementation

end.
