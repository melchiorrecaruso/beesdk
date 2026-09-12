{
  Copyright (c) 2003-2026 Andrew Filinsky and Melchiorre Caruso


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
  v0.7.9 build 0601 - 2026.09.12 by Melchiorre Caruso.
}

unit Bee_Assembler;

{$MODE OBJFPC}
{$ASMMODE INTEL}

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
assembler; nostackframe;
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
assembler; nostackframe;
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
assembler; nostackframe;
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
assembler; nostackframe;
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
assembler; nostackframe;
asm
  cmp    Count, 16
  jne    @generic
  movd   xmm0, Value
  pshufd xmm0, xmm0, 0
  movdqu xmm1, [eax +  0]
  paddd  xmm1, xmm0
  movdqu [eax +  0], xmm1
  movdqu xmm1, [eax + 16]
  paddd  xmm1, xmm0
  movdqu [eax + 16], xmm1
  movdqu xmm1, [eax + 32]
  paddd  xmm1, xmm0
  movdqu [eax + 32], xmm1
  movdqu xmm1, [eax + 48]
  paddd  xmm1, xmm0
  movdqu [eax + 48], xmm1
  jmp    @done
@generic:
  add    dword ptr [eax], Value
  add    eax, 4
  dec    Count
  jne    @generic
@done:
end;
{$ELSE}
{$IFDEF CPUX86_64}
assembler; nostackframe;
asm
  mov    rax, Data
  cmp    Count, 16
  jne    @generic
  movd   xmm0, Value
  pshufd xmm0, xmm0, 0
  movdqu xmm1, [rax +  0]
  paddd  xmm1, xmm0
  movdqu [rax +  0], xmm1
  movdqu xmm1, [rax + 16]
  paddd  xmm1, xmm0
  movdqu [rax + 16], xmm1
  movdqu xmm1, [rax + 32]
  paddd  xmm1, xmm0
  movdqu [rax + 32], xmm1
  movdqu xmm1, [rax + 48]
  paddd  xmm1, xmm0
  movdqu [rax + 48], xmm1
  jmp    @done
@generic:
  add    dword ptr [rax], Value
  add    rax, 4
  dec    Count
  jne    @generic
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
assembler; nostackframe;
asm
  cmp    Count, 16
  jne    @generic
  pxor   xmm0, xmm0
  movdqu [eax +  0], xmm0
  movdqu [eax + 16], xmm0
  movdqu [eax + 32], xmm0
  movdqu [eax + 48], xmm0
  jmp    @done
@generic:
  push   edi
  mov    edi, Data
  mov    ecx, Count
  xor    eax, eax
  rep    stosd
  pop    edi
@done:
end;
{$ELSE}
{$IFDEF CPUX86_64}
assembler; nostackframe;
asm
  mov    rax, Data
  cmp    Count, 16
  jne    @generic
  pxor   xmm0, xmm0
  movdqu [rax +  0], xmm0
  movdqu [rax + 16], xmm0
  movdqu [rax + 32], xmm0
  movdqu [rax + 48], xmm0
  jmp    @done
@generic:
  push   rdi
  mov    rdi, Data
  mov    ecx, Count
  xor    eax, eax
  rep    stosd
  pop    rdi
@done:
end;
{$ELSE}
begin
  FillChar(PByte(@Data)^, Count * SizeOf(cardinal), 0);
end;
{$ENDIF}
{$ENDIF}

procedure MovePointerUnchecked(const Source, Dest; Count: cardinal);
{$IFDEF CPUI386}
assembler; nostackframe;
asm
  push esi
  test Count, Count
  jz   @done
@next:
  mov  esi, dword ptr [Source]
  mov  dword ptr [Dest], esi
  add  Source, 4
  add  Dest, 4
  dec  Count
  jne  @next
@done:
  pop  esi
end;
{$ELSE}
{$IFDEF CPUX86_64}
assembler; nostackframe;
asm
  test Count, Count
  jz   @done
@next:
  mov  r10, qword ptr [Source]
  mov  qword ptr [Dest], r10
  add  Source, 8
  add  Dest, 8
  dec  Count
  jne  @next
@done:
end;
{$ELSE}
begin
  System.Move(Source, PByte(@Dest)^, Count * SizeOf(Pointer));
end;
{$ENDIF}
{$ENDIF}

function MulDiv(A, B, C: cardinal): cardinal;
{$IFDEF CPUI386}
assembler; nostackframe;
asm
  mul  B
  div  C
end;
{$ELSE}
{$IFDEF CPUX86_64}
assembler; nostackframe;
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
assembler; nostackframe;
asm
  mul  B
  sub  eax, 1
  sbb  edx, 0
  div  C
end;
{$ELSE}
{$IFDEF CPUX86_64}
assembler; nostackframe;
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
