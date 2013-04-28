{
  Copyright (c) 2013 Melchiorre Caruso.

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

    LibBx archiver library link.

  Modifyed:

    v0.8.0 build 1970 - 2013.04.26 by Melchiorre Caruso.
}

unit Bee_LibBx;

{$I bee_compiler.inc}

interface

{$IFDEF MSWINDOWS}
  {$linklib libmsvcrt}
{$ENDIF}

{$IFDEF UNIX}
//{$linklib libmath}
//{$linklib libgcc}
{$ENDIF}

{$link libbx.c\libbx_stream.o}
// bee coder link
{$link libbx.c\libbx_bee_common.o}
{$link libbx.c\libbx_bee_modeller.o}
{$link libbx.c\libbx_bee_rangecoder.o}
// ppmd coder link
{$link libbx.c\libbx_ppmd_common.o}
{$link libbx.c\libbx_ppmd_modeller.o}
{$link libbx.c\libbx_ppmd_rangecoder.o}

type
  TStreamRead  = function(Stream: pointer; Data: PByte; Count: longint): longint;
  TStreamWrite = function(Stream: pointer; Data: PByte; Count: longint): longint;

// beelib interface
function  BeeRangeEnc_Create       (aStream: pointer; aStreamWrite: TStreamWrite): pointer; cdecl; external;
procedure BeeRangeEnc_Destroy      (Self: pointer); cdecl; external;
procedure BeeRangeEnc_StartEncode  (Self: pointer); cdecl; external;
procedure BeeRangeEnc_FinishEncode (Self: pointer); cdecl; external;
function  BeeRangeEnc_Update       (Self: pointer; Freq: pointer; aSymbol: longword): longword; cdecl; external;

function  BeeRangeDec_Create       (aStream: pointer; aStreamRead: TStreamRead): pointer; cdecl; external;
procedure BeeRangeDec_Destroy      (Self: pointer); cdecl; external;
procedure BeeRangeDec_StartDecode  (Self: pointer); cdecl; external;
procedure BeeRangeDec_FinishDecode (Self: pointer); cdecl; external;

function  BeeModeller_Create       (aCoder: pointer): pointer; cdecl; external;
procedure BeeModeller_Destroy      (Self :pointer); cdecl; external;

procedure BeeModeller_SetTableParameters(Self: pointer; TableParapeters: pointer); cdecl; external;
procedure BeeModeller_SetDictionaryLevel(Self: pointer; DictLevel: longword); cdecl; external;
procedure BeeModeller_FreshFlexible     (Self: pointer); cdecl; external;
procedure BeeModeller_FreshSolid        (Self: pointer); cdecl; external;

function  BeeModeller_Encode       (Self: pointer; Buffer: pointer; BufSize: longint): longint; cdecl; external;
function  BeeModeller_Decode       (Self: pointer; Buffer: pointer; BufSize: longint): longint; cdecl; external;

// ppmdlib interface
function  PpmdRangeEnc_Create      (aStream: pointer; aStreamWrite: TStreamWrite): pointer; cdecl; external;
procedure PpmdRangeEnc_Destroy     (Self: pointer); cdecl; external;
procedure PpmdRangeEnc_StartEncode (Self: pointer); cdecl; external;
procedure PpmdRangeEnc_FinishEncode(Self: pointer); cdecl; external;

function  PpmdRangeDec_Create      (aStream: pointer; aStreamRead: TStreamRead): pointer; cdecl; external;
procedure PpmdRangeDec_Destroy     (Self: pointer); cdecl; external;
procedure PpmdRangeDec_StartDecode (Self: pointer); cdecl; external;
procedure PpmdRangeDec_FinishDecode(Self: pointer); cdecl; external;

function  PpmdModeller_Create: pointer; cdecl; external;
procedure PpmdModeller_Destroy     (Self: pointer); cdecl; external;

procedure PpmdModeller_SetMemSize  (Self: pointer; MemSize: longword); cdecl; external;
procedure PpmdModeller_SetModelOrd (Self: pointer; ModelOrd: longword); cdecl; external;

function  PpmdModeller_Encode      (Self: pointer; RangeEnc: pointer; Buffer: pointer; BufSize: longint): longint; cdecl; external;
function  PpmdModeller_Decode      (Self: pointer; RangeEnc: pointer; Buffer: pointer; BufSize: longint): longint; cdecl; external;

implementation

end.
