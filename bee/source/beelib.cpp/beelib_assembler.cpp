#include "beelib_assembler.hpp"

/*
void FillLongword(void* Data, unsigned int Count, unsigned int Value )
{
  _asm
  {
  PUSH    EDI
  MOV     EDI, Data
  MOV     EAX, Value
  MOV     ECX, Count
  REP     stosd
  POP     EDI
  }
}

void AddLongword(void* Data, unsigned int Count, unsigned int Value)
{
  _asm
  {
    // $L0:
    // ADD     [Data], Value
    // ADD     Data, 4
    // DEC     Count
    // JNE     L0
  }
}

void ClearLongword(void* Data, unsigned int Count)
{
  _asm
  {
    MOV     ECX, Count
    MOV     EDX, EDI
    MOV     EDI, Data
    XOR     EAX, EAX
    REP     stosd
    MOV     EDI, EDX
  }
}

void MoveLongwordUnchecked(void* Source, void* Dest, unsigned int Count)
{
  _asm xchg  esi,  [Source]
  _asm xchg  edi,  [Dest]
  _asm rep   movsd
  _asm mov   esi,  [Source]
  _asm mov   edi,  [Dest]

}
*/

unsigned int MulDiv(unsigned int A, unsigned int B, unsigned int C)
{
  // __asm  mov  eax, A
  // __asm  mul       B
  // __asm  div       C

  //asm("movl  'A', %eax");
  //asm("mull  'B'");
  //asm("divl  'C'");






  // return (((long long)A * (long long)B) / (long long)C);
  return (((   uint64)A * (   uint64)B) / (   uint64)C);
}

unsigned int MulDecDiv(unsigned int A, unsigned int B, unsigned int C)
{
  // __asm  mov  eax, A
  // __asm  mul       B
  // __asm  sub  eax, 1
  // __asm  sbb  edx, 0
  // __asm  div       C

  // return ((((long long)A * (long long)B) - 1) / (long long)C);
  return ((((   uint64)A * (   uint64)B) - 1) / (   uint64)C);
}
