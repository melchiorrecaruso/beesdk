{
  Copyright (c) 2003-2007 Andrew Filinsky

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

  v0.7.9 build 0515 - 2007.12.02 by Melchiorre Caruso.
}

unit Bee_Assembler;

{$MODE DELPHI}
{$I compiler.inc}
{$IFDEF FPC}
  {$ASMMODE INTEL}
{$ENDIF}

interface

procedure CopyBytes(const Source, Dest; Count: cardinal);

procedure FillCardinal(const Data; const Count, Value: cardinal);
procedure AddCardinal(const Data; const Count, Value: cardinal);
procedure ClearCardinal(const Data; const Count: cardinal);
procedure MovePointerUnchecked(const Source, Dest; Count: cardinal);

function MulDiv(A, B, C: cardinal): cardinal;
function MulDecDiv(A, B, C: cardinal): cardinal;

implementation

procedure CopyBytes(const Source, Dest; Count: cardinal);
{$IFDEF CPUI386}
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
{$ELSE}
{$IFDEF CPUX86_64}
asm
  push rsi
  push rdi
  mov  rsi, Source
  mov  rdi, Dest
  mov  eax, Count
  mov  ecx, eax
  shr  ecx, 3
  rep  movsq
  mov  ecx, eax
  and  ecx, $07
  rep  movsb
  pop  rdi
  pop  rsi
end;
{$ELSE}
begin
  System.Move(Source, PByte(@Dest)^, Count);
end;
{$ENDIF}
{$ENDIF}

procedure FillCardinal(const Data; const Count, Value: cardinal);
{$IFDEF CPUI386}
asm
  push edi
  mov  edi, Data
  mov  eax, Value
  mov  ecx, Count
  rep  stosd
  pop  edi
end;
{$ELSE}
{$IFDEF CPUX86_64}
asm
  push rdi
  mov  rdi, Data
  mov  eax, Value
  mov  ecx, Count
  rep  stosd
  pop  rdi
end;
{$ELSE}
var
  P: ^cardinal;
  I: cardinal;
begin
  P := @Data;
  for I := 1 to Count do
  begin
    P^ := Value;
    Inc(P);
  end;
end;
{$ENDIF}
{$ENDIF}

procedure AddCardinal(const Data; const Count, Value: cardinal);
{$IFDEF CPUI386}
asm
  test Count, Count
  jz   @done
@next:
  add  [Data], Value
  add  Data, 4
  dec  Count
  jne  @next
@done:
end;
{$ELSE}
{$IFDEF CPUX86_64}
asm
  mov  rax, Data
  mov  ecx, Count
  test ecx, ecx
  jz   @done
@next:
  add  dword ptr [rax], Value
  add  rax, 4
  dec  ecx
  jne  @next
@done:
end;
{$ELSE}
var
  P: ^cardinal;
  I: cardinal;
begin
  P := @Data;
  for I := 1 to Count do
  begin
    Inc(P^, Value);
    Inc(P);
  end;
end;
{$ENDIF}
{$ENDIF}

procedure ClearCardinal(const Data; const Count: cardinal);
{$IFDEF CPUI386}
asm
  mov  ecx, Count
  mov  edx, edi
  mov  edi, Data
  xor  eax, eax
  rep  stosd
  mov  edi, edx
end;
{$ELSE}
{$IFDEF CPUX86_64}
asm
  push rdi
  mov  rdi, Data
  mov  ecx, Count
  xor  eax, eax
  rep  stosd
  pop  rdi
end;
{$ELSE}
begin
  FillChar(PByte(@Data)^, Count * SizeOf(cardinal), 0);
end;
{$ENDIF}
{$ENDIF}

procedure MovePointerUnchecked(const Source, Dest; Count: cardinal);
{$IFDEF CPUI386}
asm
  xchg esi, Source
  xchg edi, Dest
  rep  movsd
  mov  esi, Source
  mov  edi, Dest
end;
{$ELSE}
{$IFDEF CPUX86_64}
asm
  push rsi
  push rdi
  mov  rsi, Source
  mov  rdi, Dest
  mov  ecx, Count
  rep  movsq
  pop  rdi
  pop  rsi
end;
{$ELSE}
begin
  System.Move(Source, PByte(@Dest)^, Count * SizeOf(Pointer));
end;
{$ENDIF}
{$ENDIF}

function MulDiv(A, B, C: cardinal): cardinal;
{$IFDEF CPUI386}
asm
  mul B
  div C
end;
{$ELSE}
{$IFDEF CPUX86_64}
asm
  mov eax, A
  mul B
  div C
end;
{$ELSE}
begin
  Result := (uint64(A) * B) div C;
end;
{$ENDIF}
{$ENDIF}

function MulDecDiv(A, B, C: cardinal): cardinal;
{$IFDEF CPUI386}
asm
  mul B
  sub eax, 1
  sbb edx, 0
  div C
end;
{$ELSE}
{$IFDEF CPUX86_64}
asm
  mov eax, A
  mul B
  sub eax, 1
  sbb edx, 0
  div C
end;
{$ELSE}
begin
  Result := (uint64(A) * B - 1) div C;
end;
{$ENDIF}
{$ENDIF}

end.
