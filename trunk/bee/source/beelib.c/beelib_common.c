#include "beelib_common.h"

// ------------------------------------------------------------------ //
//  Common routine                                                    //
// ------------------------------------------------------------------ //

inline uint32 MulDiv(uint32 A, uint32 B, uint32 C)
{
  // return (uint32)(((uint64)A * (uint64)B) / (uint64)C);
  asm volatile (
    "movl %1, %%eax;"
    "mul  %2;"
    "div  %3;"
    "movl %%eax, %0;"
    : "=r"(A)
    :  "0"(A), "r"(B), "r"(C)
    : "%eax"
  );
  return A;
}

inline uint32 MulDecDiv(uint32 A, uint32 B, uint32 C)
{
  return (uint32)((((uint64)A * (uint64)B)-1)/(uint64)C);
}
