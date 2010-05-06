{
  Copyright (c) 2003-2010 Andrew Filinsky

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

    Assembly routines.

  Modifyed:

    v0.7.9 build 0383 - 2007.06.27 by Andrew Filinsky;

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
}

unit Bee_Assembler;

{$I compiler.inc}

interface

procedure CopyBytes(const Source, Dest; Count: longword);

procedure FillLongword(const Data; const Count, Value: longword);
procedure AddLongword(const Data; const Count, Value: longword);
procedure ClearLongword(const Data; const Count: longword);
procedure MoveLongwordUnchecked(const Source, Dest; Count: longword);

function MulDiv(A, B, C: longword): longword;
function MulDecDiv(A, B, C: longword): longword;

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
