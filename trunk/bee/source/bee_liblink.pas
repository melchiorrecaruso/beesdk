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
  {$link beelib.c\obj\release\beelib_main.o}
  {$link beelib.c\obj\release\beelib_modeller.o}
  {$link beelib.c\obj\release\beelib_rangecoder.o}
  {$link beelib.c\obj\release\beelib_stream.o}

  function BeeVersion:cuint; cdecl; external;

  function  BeeEncoder_Create
    (aStrmHandle: pointer; aStrmWrite: TWriter): pointer; cdecl; external;

  function BeeEncoder_Destroy
    (Self: pointer): pointer; cdecl; external;

  procedure BeeEncoder_SetDictionaryLevel
    (Self: pointer; Value: cuint); cdecl; external;

  procedure BeeEncoder_SetTableParameters
    (Self: pointer; Value: pointer); cdecl; external;

  procedure BeeEncoder_SetTicker
    (Self: pointer; aTickHandle: pointer; aTickSend: TTicker); cdecl; external;

  procedure BeeEncoder_FreshFlexible
    (Self: pointer); cdecl; external;

  procedure BeeEncoder_FreshSolid
    (Self: pointer); cdecl; external;

  function BeeEncoder_Encode
    (Self: pointer; aStrmHandle: pointer; aStrmRead: TReader;
     Size: clonglong; CRC: pcuint): cint64; cdecl; external;

  function BeeDecoder_Create
    (aStrmHandle:pointer; aStrmRead:TReader):pointer; cdecl; external;

  function BeeDecoder_Destroy
    (Self: pointer): pointer; cdecl; external;

  procedure BeeDecoder_SetDictionaryLevel
    (Self: pointer; Value:cuint); cdecl; external;

  procedure BeeDecoder_SetTableParameters
    (Self: pointer; Value: pointer); cdecl; external;

  procedure BeeDecoder_SetTicker
    (Self: pointer; aTickHeandle: pointer; aTickSend: TTicker); cdecl; external;

  procedure BeeDecoder_FreshFlexible
    (Self: pointer); cdecl; external;

  procedure BeeDecoder_FreshSolid
    (Self: pointer); cdecl; external;

  function BeeDecoder_Decode
    (Self: pointer; aStrmHandle: pointer; aStrmWrite: TWriter;
     Size: clonglong; CRC: pcuint): cint64; cdecl; external;

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
