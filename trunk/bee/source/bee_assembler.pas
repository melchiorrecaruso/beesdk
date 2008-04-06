{
  Copyright (c) 2003-2008 Andrew Filinsky

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

  v0.7.9 build 0707 - 2008.04.06 by Melchiorre Caruso.
}

unit Bee_Assembler;

{$i compiler.inc}

interface

procedure CopyBytes(const Source, Dest; Count: cardinal);

procedure FillCardinal(const Data; const Count, Value: cardinal);
procedure AddCardinal(const Data; const Count, Value: cardinal);
procedure ClearCardinal(const Data; const Count: cardinal);
procedure MoveCardinalUnchecked(const Source, Dest; Count: cardinal);

function MulDiv(A, B, C: cardinal): cardinal;
function MulDecDiv(A, B, C: cardinal): cardinal;

implementation

procedure CopyBytes(const Source, Dest; Count: cardinal);
asm
  xchg esi, Source
  xchg edi, Dest
  push Count
  shr  Count, 2
  rep  movsd
  pop  Count
  and  Count, $03
  rep  movsb
  mov  esi, Source
  mov  edi, Dest
end;

procedure FillCardinal(const Data; const Count, Value: cardinal);
asm
  push edi
  mov  edi, Data
  mov  eax, Value
  mov  ecx, Count
  rep  stosd
  pop  edi
end;

procedure AddCardinal(const Data; const Count, Value: cardinal);
asm
  @1:
  add [Data], Value
  add Data, 4
  dec Count
  jne @1
end;

procedure ClearCardinal(const Data; const Count: cardinal);
asm
  mov  ecx, Count
  mov  edx, edi
  mov  edi, Data
  xor  eax, eax
  rep  stosd
  mov  edi, edx
end;

procedure MoveCardinalUnchecked(const Source, Dest; Count: cardinal);
asm
  xchg esi, Source
  xchg edi, Dest
  rep  movsd
  mov  esi, Source
  mov  edi, Dest
end;

function MulDiv(A, B, C: cardinal): cardinal;
asm
  mul  B
  div  C
end;

function MulDecDiv(A, B, C: cardinal): cardinal;
asm
  mul  B
  sub  eax, 1
  sbb  edx, 0
  div  C
end;

end.
