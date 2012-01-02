#include "beelib_assembler.h"

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




