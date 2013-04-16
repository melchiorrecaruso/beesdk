#include "beelib_common.h"

// ------------------------------------------------------------------ //
//  Common routine                                                    //
// ------------------------------------------------------------------ //

inline uint32_t _MulDiv (uint32_t A, uint32_t B, uint32_t C)
{
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

inline uint32_t _MulDecDiv(uint32_t A, uint32_t B, uint32_t C)
{
  asm volatile (
    "movl %1, %%eax;"
    "mul  %2;"
    "dec  %%eax;"
    "div  %3;"
    "movl %%eax, %0;"
    : "=r"(A)
    :  "0"(A), "r"(B), "r"(C)
    : "%eax"
  );
  return A;
}
