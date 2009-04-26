unit Bee_Assembler;

interface

procedure CopyBytes (const Source, Dest; Count: Cardinal);

procedure FillCardinal (const Data; const Count, Value: Cardinal);
procedure AddCardinal (const Data; const Count, Value: Cardinal);
procedure ClearCardinal (const Data; const Count: Cardinal);
procedure MoveCardinalUnchecked (const Source, Dest; Count: Cardinal);

function MulDiv (A, B, C: Cardinal): Cardinal;
function MulDecDiv (A, B, C: Cardinal): Cardinal;

implementation

procedure CopyBytes (const Source, Dest; Count: Cardinal);
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

procedure FillCardinal (const Data; const Count, Value: Cardinal);
asm
  push edi
  mov  edi, Data
  mov  eax, Value
  mov  ecx, Count
  rep  stosd
  pop  edi
end;

procedure AddCardinal (const Data; const Count, Value: Cardinal);
asm
  sub  Data, 4
@1:
  add  [Data + Count * 4], Value
  dec  Count
  jne  @1
end;

procedure ClearCardinal (const Data; const Count: Cardinal);
asm
  mov  ecx, Count
  mov  edx, edi
  mov  edi, Data
  xor  eax, eax
  rep  stosd
  mov  edi, edx
end;

procedure MoveCardinalUnchecked (const Source, Dest; Count: Cardinal);
asm
  xchg esi, Source
  xchg edi, Dest
  rep  movsd
  mov  esi, Source
  mov  edi, Dest
end;

function MulDiv (A, B, C: Cardinal): Cardinal;
asm
  mul  B
  div  C
end;

function MulDecDiv (A, B, C: Cardinal): Cardinal;
asm
  mul  B
  sub  eax, 1
  sbb  edx, 0
  div  C
end;

end.
