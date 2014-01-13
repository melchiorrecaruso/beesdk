{
  Copyright (c) 2003-2011 Andrew Filinsky;
  Copyright (c) 2012-2014 Melchiorre Caruso.

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

    Various helper routines.

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0312 - 2007.02.16 by Andrew Filinsky;

    v1.0.0 build 2202 - 2014.01.13 by Melchiorre Caruso.
}

unit libbx_bee_common;

{$I bx_compiler.inc}

interface

uses
  IniFiles;

// -------------------------------------------------------------------------- //
//  Configuration tables type                                                 //
// -------------------------------------------------------------------------- //

const
  TABLESIZE       = 20; // array [0..20]
  TABLECOLS       =  1; // array [0.. 1]
  TABLEPARAMETERS = 42; // array [0..42]

type
  TTableCol = array [0..TABLESIZE] of longword;

  TTable = packed record
    Level: longword;
    T: array [0..TABLECOLS] of TTableCol;
  end;

  TTableParameters = array [0..TABLEPARAMETERS] of byte;

// -------------------------------------------------------------------------- //
//  Default table parameters                                                  //
// -------------------------------------------------------------------------- //

const
  DefaultDictionaryLevel: longword = $0003;
  DefaultTableParameters: TTableParameters =
    (  3, 163, 157,  65,  93, 117, 135, 109, 126, 252, 172, 252, 152, 227, 249,
     249, 253, 196,  27,  82,  93,  74, 182, 245,  40,  67,  77, 143, 133, 135,
     128, 155, 207, 177, 225, 251, 253, 248,  73,  35,  15, 107, 143);

  EmptyTableParameters: TTableParameters =
    (  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0);


procedure CopyBytes(const Source, Dest; Count: longword);
procedure FillLongword(const Data; const Count, Value: longword);
procedure AddLongword(const Data; const Count, Value: longword);
procedure ClearLongword(const Data; const Count: longword);
procedure MoveLongwordUnchecked(const Source, Dest; Count: longword);

function MulDiv(A, B, C: longword): longword;
function MulDecDiv(A, B, C: longword): longword;

// -------------------------------------------------------------------------- //
//  Configuration class                                                       //
// -------------------------------------------------------------------------- //

type
  TConfiguration = class(TIniFile);

implementation

procedure CopyBytes(const Source, Dest; Count: longword);
asm
  XCHG    ESI, Source
  XCHG    EDI, Dest
  PUSH    Count
  SHR     Count, 2
  REP     movsd
  POP     Count
  AND     Count, $03
  REP     movsb
  MOV     ESI, Source
  MOV     EDI, Dest
end;

procedure FillLongword(const Data; const Count, Value: longword);
asm
  PUSH    EDI
  MOV     EDI, Data
  MOV     EAX, Value
  MOV     ECX, Count
  REP     stosd
  POP     EDI
end;

procedure AddLongword(const Data; const Count, Value: longword);
asm
  @1:
  ADD     [Data], Value
  ADD     Data, 4
  DEC     Count
  JNE     @1
end;

procedure ClearLongword(const Data; const Count: longword);
asm
  MOV     ECX, Count
  MOV     EDX, EDI
  MOV     EDI, Data
  XOR     EAX, EAX
  REP     stosd
  MOV     EDI, EDX
end;

procedure MoveLongwordUnchecked(const Source, Dest; Count: longword);
asm
  XCHG    ESI, Source
  XCHG    EDI, Dest
  REP     movsd
  MOV     ESI, Source
  MOV     EDI, Dest
end;

function MulDiv(A, B, C: longword): longword;
asm
  MUL     B
  DIV     C
end;

function MulDecDiv(A, B, C: longword): longword;
asm
  MUL     B
  SUB     EAX, 1
  SBB     EDX, 0
  DIV     C
end;

end.
