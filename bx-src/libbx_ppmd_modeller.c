#include "libbx_ppmd_modeller.h"
#include "libbx_ppmd_common.h"
#include <stdlib.h>
#include <memory.h>

#define PPMD_32BIT

#define PPMD_INT_BITS 7
#define PPMD_PERIOD_BITS 7
#define PPMD_BIN_SCALE (1 << (PPMD_INT_BITS + PPMD_PERIOD_BITS))

#define PPMD_GET_MEAN_SPEC(summ, shift, round) (((summ) + (1 << ((shift) - (round)))) >> (shift))
#define PPMD_GET_MEAN(summ) PPMD_GET_MEAN_SPEC((summ), PPMD_PERIOD_BITS, 2)
#define PPMD_UPDATE_PROB_0(prob) ((prob) + (1 << PPMD_INT_BITS) - PPMD_GET_MEAN(prob))
#define PPMD_UPDATE_PROB_1(prob) ((prob) - PPMD_GET_MEAN(prob))

#define PPMD_N1 4
#define PPMD_N2 4
#define PPMD_N3 4
#define PPMD_N4 ((128 + 3 - 1 * PPMD_N1 - 2 * PPMD_N2 - 3 * PPMD_N3) / 4)
#define PPMD_NUM_INDEXES (PPMD_N1 + PPMD_N2 + PPMD_N3 + PPMD_N4)

#define PPMD_MIN_ORDER 2
#define PPMD_MAX_ORDER 64

#define PPMD_MIN_MEM_SIZE (1 << 11)
#define PPMD_MAX_MEM_SIZE (0xFFFFFFFF - 12 * 3)

       const  uint8_t PPMD_kExpEscape[16] = { 25, 14, 9, 7, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2 };
static const uint16_t kInitBinEsc[] = { 0x3CDD, 0x1F3F, 0x59BF, 0x48F3, 0x64A1, 0x5ABC, 0x6632, 0x6051};

typedef
  #ifdef PPMD_32BIT
    uint8_t *
  #else
    uint32_t
  #endif
  TPpmdByteRef;

#define PPMD_SetAllBitsIn256Bytes(p) \
  { unsigned i; for (i = 0; i < 256 / sizeof(p[0]); i += 8) { \
  p[i+7] = p[i+6] = p[i+5] = p[i+4] = p[i+3] = p[i+2] = p[i+1] = p[i+0] = ~(size_t)0; }}

typedef
  #ifdef PPMD_32BIT
    struct TPpmdNode_ *
  #else
    uint32_t
  #endif
  TPpmdNodeRef;

typedef struct TPpmdNode_ {
  uint16_t Stamp;
  uint16_t NU;
  TPpmdNodeRef Next;
  TPpmdNodeRef Prev;
} TPpmdNode;

typedef struct TPpmdState_ {
   uint8_t Symbol;
   uint8_t Freq;
  uint16_t SuccessorLow;
  uint16_t SuccessorHigh;
} TPpmdState;

typedef
  #ifdef PPMD_32BIT
    struct TPpmdState_ *
  #else
    uint32_t
  #endif
  TPpmdStateRef;

typedef
  #ifdef PPMD_32BIT
    struct TPpmdContext_ *
  #else
    uint32_t
  #endif
  TPpmdContextRef;

typedef struct TPpmdContext_ {
  uint16_t NumStats;
  uint16_t SummFreq;
  TPpmdStateRef   Stats;
  TPpmdContextRef Suffix;
} TPpmdContext;

#define PpmdContext_OneState(Self) ((TPpmdState *)&(Self)->SummFreq)

typedef TPpmdContext * CTX_PTR;

typedef
  #ifdef PPMD_32BIT
    void *
  #else
    uint32_t
  #endif
  TPpmdVoidRef;

/* SEE-contexts for PPM-contexts with masked symbols */

typedef struct {
  uint16_t Summ;   /* Freq */
   uint8_t Shift;  /* Speed of Freq change; low Shift is for fast change */
   uint8_t Count;  /* Count to next change of Shift */
} TPpmdSee;

#define PpmdSee_Update(Self)  if ((Self)->Shift < PPMD_PERIOD_BITS && --(Self)->Count == 0) \
    { (Self)->Summ <<= 1; (Self)->Count = (uint8_t)(3 << (Self)->Shift++); }

struct TPpmdModeller {
  TPpmdContext *MinContext, *MaxContext;
  TPpmdState *FoundState;
  unsigned OrderFall, InitEsc, PrevSuccess, MaxOrder, HiBitsFlag;
   int32_t RunLength, InitRL; /* must be 32-bit at least */

  uint32_t Size;
  uint32_t GlueCount;
   uint8_t *Base, *LoUnit, *HiUnit, *Text, *UnitsStart;
  uint32_t AlignOffset;

   uint8_t Indx2Units[PPMD_NUM_INDEXES];
   uint8_t Units2Indx[128];
  TPpmdVoidRef FreeList[PPMD_NUM_INDEXES];
   uint8_t NS2Indx[256], NS2BSIndx[256], HB2Flag[256];
  TPpmdSee DummySee, See[25][16];
  uint16_t BinSumm[128][64];
};

#ifdef PPMD_32BIT
  #define Ppmd_GetPtr(Self, ptr)(ptr)
  #define Ppmd_GetContext(Self, ptr)(ptr)
  #define Ppmd_GetStats(Self, ctx)((ctx)->Stats)
#else
  #define Ppmd_GetPtr(Self, offs)((void *)((Self)->Base + (offs)))
  #define Ppmd_GetContext(Self, offs)((TPpmdContext *)Ppmd_GetPtr((Self), (offs)))
  #define Ppmd_GetStats(Self, ctx)((TPpmdState *)Ppmd_GetPtr((Self), ((ctx)->Stats)))
#endif

#define Ppmd_GetBinSumm(Self) \
    &Self->BinSumm[PpmdContext_OneState(Self->MinContext)->Freq - 1][Self->PrevSuccess + \
    Self->NS2BSIndx[Ppmd_GetContext(Self, Self->MinContext->Suffix)->NumStats - 1] + \
    (Self->HiBitsFlag = Self->HB2Flag[Self->FoundState->Symbol]) + \
    2 * Self->HB2Flag[PpmdContext_OneState(Self->MinContext)->Symbol] + \
    ((Self->RunLength >> 26) & 0x20)]

#define MAX_FREQ 124
#define UNIT_SIZE 12

#define U2B(nu)((uint32_t)(nu) * UNIT_SIZE)
#define U2I(nu)(Self->Units2Indx[(nu) - 1])
#define I2U(indx)(Self->Indx2Units[indx])

#ifdef PPMD_32BIT
  #define REF(ptr)(ptr)
#else
  #define REF(ptr)((uint32_t)((uint8_t *)(ptr) - (Self)->Base))
#endif

#define STATS_REF(ptr) ((TPpmdStateRef)REF(ptr))

#define CTX(ref)((TPpmdContext *)Ppmd_GetContext(Self, ref))
#define STATS(ctx)Ppmd_GetStats(Self, ctx)
#define ONE_STATE(ctx)PpmdContext_OneState(ctx)
#define SUFFIX(ctx)CTX((ctx)->Suffix)

#ifdef PPMD_32BIT
  #define NODE(ptr)(ptr)
#else
  #define NODE(offs)((TPpmdNode *)(Self->Base + (offs)))
#endif

void PpmdModeller_Construct(PPpmdModeller Self)
{
  unsigned i, k, m;

  Self->Base = 0;
  for (i = 0, k = 0; i < PPMD_NUM_INDEXES; i++)
  {
    unsigned step = (i >= 12 ? 4 : (i >> 2) + 1);
    do { Self->Units2Indx[k++] = (uint8_t)i; } while(--step);
    Self->Indx2Units[i] = (uint8_t)k;
  }

  Self->NS2BSIndx[0] = (0 << 1);
  Self->NS2BSIndx[1] = (1 << 1);
  memset(Self->NS2BSIndx + 2, (2 << 1), 9);
  memset(Self->NS2BSIndx + 11, (3 << 1), 256 - 11);

  for (i = 0; i < 3; i++)
    Self->NS2Indx[i] = (uint8_t)i;
  for (m = i, k = 1; i < 256; i++)
  {
    Self->NS2Indx[i] = (uint8_t)m;
    if (--k == 0)
      k = (++m) - 2;
  }
  memset(Self->HB2Flag, 0, 0x40);
  memset(Self->HB2Flag + 0x40, 8, 0x100 - 0x40);
}

void PpmdModeller_Free(PPpmdModeller Self)
{
  free(Self->Base);
  Self->Size = 0;
  Self->Base = 0;
}

int PpmdModeller_Alloc(PPpmdModeller Self, uint32_t size)
{
  if (Self->Base == 0 || Self->Size != size)
  {
    PpmdModeller_Free(Self);
    Self->AlignOffset =
      #ifdef PPMD_32BIT
        (4 - size) & 3;
      #else
         4 - (size & 3);
      #endif
    if ((Self->Base = (uint8_t *)malloc(Self->AlignOffset + size
        #ifndef PPMD_32BIT
        + UNIT_SIZE
        #endif
        )) == 0)
      return FALSE;
    Self->Size = size;
  }
  return TRUE;
}

static void PpmdModeller_InsertNode(PPpmdModeller Self, void *node, unsigned indx)
{
  *((TPpmdVoidRef *)node) = Self->FreeList[indx];
  Self->FreeList[indx] = REF(node);
}

static void *PpmdModeller_RemoveNode(PPpmdModeller Self, unsigned indx)
{
  TPpmdVoidRef *node = (TPpmdVoidRef *)Ppmd_GetPtr(Self, Self->FreeList[indx]);
  Self->FreeList[indx] = *node;
  return node;
}

static void PpmdModeller_SplitBlock(PPpmdModeller Self, void *ptr, unsigned oldIndx, unsigned newIndx)
{
  unsigned i, nu = I2U(oldIndx) - I2U(newIndx);
  ptr = (uint8_t *)ptr + U2B(I2U(newIndx));
  if (I2U(i = U2I(nu)) != nu)
  {
    unsigned k = I2U(--i);
    PpmdModeller_InsertNode(Self, ((uint8_t *)ptr) + U2B(k), nu - k - 1);
  }
  PpmdModeller_InsertNode(Self, ptr, i);
}

static void PpmdModeller_GlueFreeBlocks(PPpmdModeller Self)
{
  #ifdef PPMD_32BIT
  TPpmdNode headItem;
  TPpmdNodeRef head = &headItem;
  #else
  TPpmdNodeRef head = Self->AlignOffset + Self->Size;
  #endif

  TPpmdNodeRef n = head;
  unsigned i;

  Self->GlueCount = 255;

  /* create doubly-linked list of free blocks */
  for (i = 0; i < PPMD_NUM_INDEXES; i++)
  {
    uint16_t nu = I2U(i);
    TPpmdNodeRef next = (TPpmdNodeRef)Self->FreeList[i];
    Self->FreeList[i] = 0;
    while (next != 0)
    {
      TPpmdNode *node = NODE(next);
      node->Next = n;
      n = NODE(n)->Prev = next;
      next = *(const TPpmdNodeRef *)node;
      node->Stamp = 0;
      node->NU = (uint16_t)nu;
    }
  }
  NODE(head)->Stamp = 1;
  NODE(head)->Next = n;
  NODE(n)->Prev = head;
  if (Self->LoUnit != Self->HiUnit)
    ((TPpmdNode *)Self->LoUnit)->Stamp = 1;

  /* Glue free blocks */
  while (n != head)
  {
    TPpmdNode *node = NODE(n);
    uint32_t nu = (uint32_t)node->NU;
    for (;;)
    {
      TPpmdNode *node2 = NODE(n) + nu;
      nu += node2->NU;
      if (node2->Stamp != 0 || nu >= 0x10000)
        break;
      NODE(node2->Prev)->Next = node2->Next;
      NODE(node2->Next)->Prev = node2->Prev;
      node->NU = (uint16_t)nu;
    }
    n = node->Next;
  }

  /* Fill lists of free blocks */
  for (n = NODE(head)->Next; n != head;)
  {
    TPpmdNode *node = NODE(n);
    unsigned nu;
    TPpmdNodeRef next = node->Next;
    for (nu = node->NU; nu > 128; nu -= 128, node += 128)
      PpmdModeller_InsertNode(Self, node, PPMD_NUM_INDEXES - 1);
    if (I2U(i = U2I(nu)) != nu)
    {
      unsigned k = I2U(--i);
      PpmdModeller_InsertNode(Self, node + k, nu - k - 1);
    }
    PpmdModeller_InsertNode(Self, node, i);
    n = next;
  }
}

static void *PpmdModeller_AllocUnitsRare(PPpmdModeller Self, unsigned indx)
{
  unsigned i;
  void *retVal;
  if (Self->GlueCount == 0)
  {
    PpmdModeller_GlueFreeBlocks(Self);
    if (Self->FreeList[indx] != 0)
      return PpmdModeller_RemoveNode(Self, indx);
  }
  i = indx;
  do
  {
    if (++i == PPMD_NUM_INDEXES)
    {
      uint32_t numuint8_ts = U2B(I2U(indx));
      Self->GlueCount--;
      return ((uint32_t)(Self->UnitsStart - Self->Text) > numuint8_ts) ? (Self->UnitsStart -= numuint8_ts) : (NULL);
    }
  }
  while (Self->FreeList[i] == 0);
  retVal = PpmdModeller_RemoveNode(Self, i);
  PpmdModeller_SplitBlock(Self, retVal, i, indx);
  return retVal;
}

static void *PpmdModeller_AllocUnits(PPpmdModeller Self, unsigned indx)
{
  uint32_t numuint8_ts;
  if (Self->FreeList[indx] != 0)
    return PpmdModeller_RemoveNode(Self, indx);
  numuint8_ts = U2B(I2U(indx));
  if (numuint8_ts <= (uint32_t)(Self->HiUnit - Self->LoUnit))
  {
    void *retVal = Self->LoUnit;
    Self->LoUnit += numuint8_ts;
    return retVal;
  }
  return PpmdModeller_AllocUnitsRare(Self, indx);
}

#define MyMem12Cpy(dest, src, num) \
  { uint32_t *d = (uint32_t *)dest; const uint32_t *s = (const uint32_t *)src; uint32_t n = num; \
    do { d[0] = s[0]; d[1] = s[1]; d[2] = s[2]; s += 3; d += 3; } while(--n); }

static void *PpmdModeller_ShrinkUnits(PPpmdModeller Self, void *oldPtr, unsigned oldNU, unsigned newNU)
{
  unsigned i0 = U2I(oldNU);
  unsigned i1 = U2I(newNU);
  if (i0 == i1)
    return oldPtr;
  if (Self->FreeList[i1] != 0)
  {
    void *ptr = PpmdModeller_RemoveNode(Self, i1);
    MyMem12Cpy(ptr, oldPtr, newNU);
    PpmdModeller_InsertNode(Self, oldPtr, i0);
    return ptr;
  }
  PpmdModeller_SplitBlock(Self, oldPtr, i0, i1);
  return oldPtr;
}

#define SUCCESSOR(Self) ((TPpmdVoidRef)((Self)->SuccessorLow | ((uint32_t)(Self)->SuccessorHigh << 16)))

static void PpmdModeller_SetSuccessor(TPpmdState *Self, TPpmdVoidRef v)
{
  (Self)->SuccessorLow  = (uint16_t) ((uint32_t)(v)        & 0xFFFF);
  (Self)->SuccessorHigh = (uint16_t)(((uint32_t)(v) >> 16) & 0xFFFF);
}

static void PpmdModeller_RestartModel(PPpmdModeller Self)
{
  unsigned i, k, m;

  memset(Self->FreeList, 0, sizeof(Self->FreeList));
  Self->Text   = Self->Base + Self->AlignOffset;
  Self->HiUnit = Self->Text + Self->Size;
  Self->LoUnit = Self->UnitsStart = Self->HiUnit - Self->Size / 8 / UNIT_SIZE * 7 * UNIT_SIZE;
  Self->GlueCount = 0;

  Self->OrderFall = Self->MaxOrder;
  Self->RunLength = Self->InitRL = -(int32_t)((Self->MaxOrder < 12) ? Self->MaxOrder : 12) - 1;
  Self->PrevSuccess = 0;

  Self->MinContext = Self->MaxContext = (CTX_PTR)(Self->HiUnit -= UNIT_SIZE); /* PPmdModeller_AllocContext(p); */
  Self->MinContext->Suffix   = 0;
  Self->MinContext->NumStats = 256;
  Self->MinContext->SummFreq = 256 + 1;
  Self->FoundState = (TPpmdState *)Self->LoUnit; /* PPmdModeller_AllocUnits(p, PPMD_NUM_INDEXES - 1); */
  Self->LoUnit += U2B(256 / 2);
  Self->MinContext->Stats = REF(Self->FoundState);
  for (i = 0; i < 256; i++)
  {
    TPpmdState *s = &Self->FoundState[i];
    s->Symbol = (uint8_t)i;
    s->Freq = 1;
    PpmdModeller_SetSuccessor(s, 0);
  }

  for (i = 0; i < 128; i++)
    for (k = 0; k < 8; k++)
    {
      uint16_t *dest = Self->BinSumm[i] + k;
      uint16_t val = (uint16_t)(PPMD_BIN_SCALE - kInitBinEsc[k] / (i + 2));
      for (m = 0; m < 64; m += 8)
        dest[m] = val;
    }

  for (i = 0; i < 25; i++)
    for (k = 0; k < 16; k++)
    {
      TPpmdSee *s = &Self->See[i][k];
      s->Summ  = (uint16_t)((5 * i + 10) << (s->Shift = PPMD_PERIOD_BITS - 4));
      s->Count = 4;
    }
}

void PpmdModeller_Init(PPpmdModeller Self, unsigned maxOrder)
{
  Self->MaxOrder = maxOrder;
  PpmdModeller_RestartModel(Self);
  Self->DummySee.Shift = PPMD_PERIOD_BITS;
  Self->DummySee.Summ  =  0; /* unused */
  Self->DummySee.Count = 64; /* unused */
}

static CTX_PTR PpmdModeller_CreateSuccessors(PPpmdModeller Self, uint8_t skip)
{
  TPpmdState upState;
  CTX_PTR c = Self->MinContext;
  TPpmdByteRef upBranch = (TPpmdByteRef)SUCCESSOR(Self->FoundState);
  TPpmdState *ps[PPMD_MAX_ORDER];
  unsigned numPs = 0;

  if (!skip)
    ps[numPs++] = Self->FoundState;

  while (c->Suffix)
  {
    TPpmdVoidRef successor;
    TPpmdState *s;
    c = SUFFIX(c);
    if (c->NumStats != 1)
    {
      for (s = STATS(c); s->Symbol != Self->FoundState->Symbol; s++);
    }
    else
      s = ONE_STATE(c);
    successor = SUCCESSOR(s);
    if (successor != upBranch)
    {
      c = CTX(successor);
      if (numPs == 0)
        return c;
      break;
    }
    ps[numPs++] = s;
  }

  upState.Symbol = *(const uint8_t *)Ppmd_GetPtr(Self, upBranch);
  PpmdModeller_SetSuccessor(&upState, upBranch + 1);

  if (c->NumStats == 1)
    upState.Freq = ONE_STATE(c)->Freq;
  else
  {
    uint32_t cf, s0;
    TPpmdState *s;
    for (s = STATS(c); s->Symbol != upState.Symbol; s++);
    cf = s->Freq - 1;
    s0 = c->SummFreq - c->NumStats - cf;
    upState.Freq = (uint8_t)(1 + ((2 * cf <= s0) ? (5 * cf > s0) : ((2 * cf + 3 * s0 - 1) / (2 * s0))));
  }

  do
  {
    /* Create Child */
    CTX_PTR c1; /* = AllocContext(p); */
    if (Self->HiUnit != Self->LoUnit)
      c1 = (CTX_PTR)(Self->HiUnit -= UNIT_SIZE);
    else if (Self->FreeList[0] != 0)
      c1 = (CTX_PTR)PpmdModeller_RemoveNode(Self, 0);
    else
    {
      c1 = (CTX_PTR)PpmdModeller_AllocUnitsRare(Self, 0);
      if (!c1)
        return NULL;
    }
    c1->NumStats = 1;
    *ONE_STATE(c1) = upState;
    c1->Suffix = REF(c);
    PpmdModeller_SetSuccessor(ps[--numPs], REF(c1));
    c = c1;
  }
  while (numPs != 0);

  return c;
}

static void PpmdModeller_SwapStates(TPpmdState *t1, TPpmdState *t2)
{
  TPpmdState tmp = *t1;
  *t1 = *t2;
  *t2 = tmp;
}

static void PpmdModeller_UpdateModel(PPpmdModeller Self)
{
  TPpmdVoidRef successor, fSuccessor = SUCCESSOR(Self->FoundState);
  CTX_PTR c;
  unsigned s0, ns;

  if (Self->FoundState->Freq < MAX_FREQ / 4 && Self->MinContext->Suffix != 0)
  {
    c = SUFFIX(Self->MinContext);

    if (c->NumStats == 1)
    {
      TPpmdState *s = ONE_STATE(c);
      if (s->Freq < 32)
        s->Freq++;
    }
    else
    {
      TPpmdState *s = STATS(c);
      if (s->Symbol != Self->FoundState->Symbol)
      {
        do { s++; } while (s->Symbol != Self->FoundState->Symbol);
        if (s[0].Freq >= s[-1].Freq)
        {
          PpmdModeller_SwapStates(&s[0], &s[-1]);
          s--;
        }
      }
      if (s->Freq < MAX_FREQ - 9)
      {
        s->Freq += 2;
        c->SummFreq += 2;
      }
    }
  }

  if (Self->OrderFall == 0)
  {
    Self->MinContext = Self->MaxContext = PpmdModeller_CreateSuccessors(Self, TRUE);
    if (Self->MinContext == 0)
    {
      PpmdModeller_RestartModel(Self);
      return;
    }
    PpmdModeller_SetSuccessor(Self->FoundState, REF(Self->MinContext));
    return;
  }

  *Self->Text++ = Self->FoundState->Symbol;
  successor = REF(Self->Text);
  if (Self->Text >= Self->UnitsStart)
  {
    PpmdModeller_RestartModel(Self);
    return;
  }

  if (fSuccessor)
  {
    if (fSuccessor <= successor)
    {
      CTX_PTR cs = PpmdModeller_CreateSuccessors(Self, FALSE);
      if (cs == NULL)
      {
        PpmdModeller_RestartModel(Self);
        return;
      }
      fSuccessor = REF(cs);
    }
    if (--Self->OrderFall == 0)
    {
      successor = fSuccessor;
      Self->Text -= (Self->MaxContext != Self->MinContext);
    }
  }
  else
  {
    PpmdModeller_SetSuccessor(Self->FoundState, successor);
    fSuccessor = REF(Self->MinContext);
  }

  s0 = Self->MinContext->SummFreq - (ns = Self->MinContext->NumStats) - (Self->FoundState->Freq - 1);

  for (c = Self->MaxContext; c != Self->MinContext; c = SUFFIX(c))
  {
    unsigned ns1;
    uint32_t cf, sf;
    if ((ns1 = c->NumStats) != 1)
    {
      if ((ns1 & 1) == 0)
      {
        /* Expand for one UNIT */
        unsigned oldNU = ns1 >> 1;
        unsigned i = U2I(oldNU);
        if (i != U2I(oldNU + 1))
        {
          void *ptr = PpmdModeller_AllocUnits(Self, i + 1);
          void *oldPtr;
          if (!ptr)
          {
            PpmdModeller_RestartModel(Self);
            return;
          }
          oldPtr = STATS(c);
          MyMem12Cpy(ptr, oldPtr, oldNU);
          PpmdModeller_InsertNode(Self, oldPtr, i);
          c->Stats = STATS_REF(ptr);
        }
      }
      c->SummFreq = (uint16_t)(c->SummFreq + (2 * ns1 < ns) + 2 * ((4 * ns1 <= ns) & (c->SummFreq <= 8 * ns1)));
    }
    else
    {
      TPpmdState *s = (TPpmdState*)PpmdModeller_AllocUnits(Self, 0);
      if (!s)
      {
        PpmdModeller_RestartModel(Self);
        return;
      }
      *s = *ONE_STATE(c);
      c->Stats = REF(s);
      if (s->Freq < MAX_FREQ / 4 - 1)
        s->Freq <<= 1;
      else
        s->Freq = MAX_FREQ - 4;
      c->SummFreq = (uint16_t)(s->Freq + Self->InitEsc + (ns > 3));
    }
    cf = 2 * (uint32_t)Self->FoundState->Freq * (c->SummFreq + 6);
    sf = (uint32_t)s0 + c->SummFreq;
    if (cf < 6 * sf)
    {
      cf = 1 + (cf > sf) + (cf >= 4 * sf);
      c->SummFreq += 3;
    }
    else
    {
      cf = 4 + (cf >= 9 * sf) + (cf >= 12 * sf) + (cf >= 15 * sf);
      c->SummFreq = (uint16_t)(c->SummFreq + cf);
    }
    {
      TPpmdState *s = STATS(c) + ns1;
      PpmdModeller_SetSuccessor(s, successor);
      s->Symbol = Self->FoundState->Symbol;
      s->Freq = (uint8_t)cf;
      c->NumStats = (uint16_t)(ns1 + 1);
    }
  }
  Self->MaxContext = Self->MinContext = CTX(fSuccessor);
}

static void PpmdModeller_Rescale(PPpmdModeller Self)
{
  unsigned i, adder, sumFreq, escFreq;
  TPpmdState *stats = STATS(Self->MinContext);
  TPpmdState *s = Self->FoundState;
  {
    TPpmdState tmp = *s;
    for (; s != stats; s--)
      s[0] = s[-1];
    *s = tmp;
  }
  escFreq = Self->MinContext->SummFreq - s->Freq;
  s->Freq += 4;
  adder = (Self->OrderFall != 0);
  s->Freq = (uint8_t)((s->Freq + adder) >> 1);
  sumFreq = s->Freq;

  i = Self->MinContext->NumStats - 1;
  do
  {
    escFreq -= (++s)->Freq;
    s->Freq = (uint8_t)((s->Freq + adder) >> 1);
    sumFreq += s->Freq;
    if (s[0].Freq > s[-1].Freq)
    {
      TPpmdState *s1 = s;
      TPpmdState tmp = *s1;
      do
        s1[0] = s1[-1];
      while (--s1 != stats && tmp.Freq > s1[-1].Freq);
      *s1 = tmp;
    }
  }
  while (--i);

  if (s->Freq == 0)
  {
    unsigned numStats = Self->MinContext->NumStats;
    unsigned n0, n1;
    do { i++; } while ((--s)->Freq == 0);
    escFreq += i;
    Self->MinContext->NumStats = (uint16_t)(Self->MinContext->NumStats - i);
    if (Self->MinContext->NumStats == 1)
    {
      TPpmdState tmp = *stats;
      do
      {
        tmp.Freq = (uint8_t)(tmp.Freq - (tmp.Freq >> 1));
        escFreq >>= 1;
      }
      while (escFreq > 1);
      PpmdModeller_InsertNode(Self, stats, U2I(((numStats + 1) >> 1)));
      *(Self->FoundState = ONE_STATE(Self->MinContext)) = tmp;
      return;
    }
    n0 = (numStats + 1) >> 1;
    n1 = (Self->MinContext->NumStats + 1) >> 1;
    if (n0 != n1)
      Self->MinContext->Stats = STATS_REF(PpmdModeller_ShrinkUnits(Self, stats, n0, n1));
  }
  Self->MinContext->SummFreq = (uint16_t)(sumFreq + escFreq - (escFreq >> 1));
  Self->FoundState = STATS(Self->MinContext);
}

TPpmdSee *PpmdModeller_MakeEscFreq(PPpmdModeller Self, unsigned numMasked, uint32_t *escFreq)
{
  TPpmdSee *see;
  unsigned nonMasked = Self->MinContext->NumStats - numMasked;
  if (Self->MinContext->NumStats != 256)
  {
    see = Self->See[Self->NS2Indx[nonMasked - 1]] +
        (nonMasked < (unsigned)SUFFIX(Self->MinContext)->NumStats - Self->MinContext->NumStats) +
        2 * (Self->MinContext->SummFreq < 11 * Self->MinContext->NumStats) +
        4 * (numMasked > nonMasked) +
        Self->HiBitsFlag;
    {
      unsigned r = (see->Summ >> see->Shift);
      see->Summ = (uint16_t)(see->Summ - r);
      *escFreq = r + (r == 0);
    }
  }
  else
  {
    see = &Self->DummySee;
    *escFreq = 1;
  }
  return see;
}

static void PpmdModeller_NextContext(PPpmdModeller Self)
{
  CTX_PTR c = CTX(SUCCESSOR(Self->FoundState));
  if (Self->OrderFall == 0 && (uint8_t *)c > Self->Text)
    Self->MinContext = Self->MaxContext = c;
  else
    PpmdModeller_UpdateModel(Self);
}

void PpmdModeller_Update1(PPpmdModeller Self)
{
  TPpmdState *s = Self->FoundState;
  s->Freq += 4;
  Self->MinContext->SummFreq += 4;
  if (s[0].Freq > s[-1].Freq)
  {
    PpmdModeller_SwapStates(&s[0], &s[-1]);
    Self->FoundState = --s;
    if (s->Freq > MAX_FREQ)
      PpmdModeller_Rescale(Self);
  }
  PpmdModeller_NextContext(Self);
}

void PpmdModeller_Update1_0(PPpmdModeller Self)
{
  Self->PrevSuccess = (2 * Self->FoundState->Freq > Self->MinContext->SummFreq);
  Self->RunLength += Self->PrevSuccess;
  Self->MinContext->SummFreq += 4;
  if ((Self->FoundState->Freq += 4) > MAX_FREQ)
    PpmdModeller_Rescale(Self);
  PpmdModeller_NextContext(Self);
}

void PpmdModeller_UpdateBin(PPpmdModeller Self)
{
  Self->FoundState->Freq = (uint8_t)(Self->FoundState->Freq + (Self->FoundState->Freq < 128 ? 1: 0));
  Self->PrevSuccess = 1;
  Self->RunLength++;
  PpmdModeller_NextContext(Self);
}

void PpmdModeller_Update2(PPpmdModeller Self)
{
  Self->MinContext->SummFreq += 4;
  if ((Self->FoundState->Freq += 4) > MAX_FREQ)
    PpmdModeller_Rescale(Self);
  Self->RunLength = Self->InitRL;
  PpmdModeller_UpdateModel(Self);
}

#define MASK(sym) ((signed char *)charMask)[sym]

void PpmdModeller_EncodeSymbol(PPpmdModeller Self, PPpmdRangeEnc rc, int symbol)
{
  size_t charMask[256 / sizeof(size_t)];
  if (Self->MinContext->NumStats != 1)
  {
    TPpmdState *s = Ppmd_GetStats(Self, Self->MinContext);
    uint32_t sum;
    unsigned i;
    if (s->Symbol == symbol)
    {
      PpmdRangeEnc_Encode(rc, 0, s->Freq, Self->MinContext->SummFreq);
      Self->FoundState = s;
      PpmdModeller_Update1_0(Self);
      return;
    }
    Self->PrevSuccess = 0;
    sum = s->Freq;
    i = Self->MinContext->NumStats - 1;
    do
    {
      if ((++s)->Symbol == symbol)
      {
        PpmdRangeEnc_Encode(rc, sum, s->Freq, Self->MinContext->SummFreq);
        Self->FoundState = s;
        PpmdModeller_Update1(Self);
        return;
      }
      sum += s->Freq;
    }
    while (--i);

    Self->HiBitsFlag = Self->HB2Flag[Self->FoundState->Symbol];
    PPMD_SetAllBitsIn256Bytes(charMask);
    MASK(s->Symbol) = 0;
    i = Self->MinContext->NumStats - 1;
    do { MASK((--s)->Symbol) = 0; } while (--i);
    PpmdRangeEnc_Encode(rc, sum, Self->MinContext->SummFreq - sum, Self->MinContext->SummFreq);
  }
  else
  {
    uint16_t *prob = Ppmd_GetBinSumm(Self);
    TPpmdState *s = PpmdContext_OneState(Self->MinContext);
    if (s->Symbol == symbol)
    {
      PpmdRangeEnc_EncodeBit_0(rc, *prob);
      *prob = (uint16_t)PPMD_UPDATE_PROB_0(*prob);
      Self->FoundState = s;
      PpmdModeller_UpdateBin(Self);
      return;
    }
    else
    {
      PpmdRangeEnc_EncodeBit_1(rc, *prob);
      *prob = (uint16_t)PPMD_UPDATE_PROB_1(*prob);
      Self->InitEsc = PPMD_kExpEscape[*prob >> 10];
      PPMD_SetAllBitsIn256Bytes(charMask);
      MASK(s->Symbol) = 0;
      Self->PrevSuccess = 0;
    }
  }
  for (;;)
  {
    uint32_t escFreq;
    TPpmdSee *see;
    TPpmdState *s;
    uint32_t sum;
    unsigned i, numMasked = Self->MinContext->NumStats;
    do
    {
      Self->OrderFall++;
      if (!Self->MinContext->Suffix)
        return; /* EndMarker (symbol = -1) */
      Self->MinContext = Ppmd_GetContext(Self, Self->MinContext->Suffix);
    }
    while (Self->MinContext->NumStats == numMasked);

    see = PpmdModeller_MakeEscFreq(Self, numMasked, &escFreq);
    s = Ppmd_GetStats(Self, Self->MinContext);
    sum = 0;
    i = Self->MinContext->NumStats;
    do
    {
      int cur = s->Symbol;
      if (cur == symbol)
      {
        uint32_t low = sum;
        TPpmdState *s1 = s;
        do
        {
          sum += (s->Freq & (int)(MASK(s->Symbol)));
          s++;
        }
        while (--i);
        PpmdRangeEnc_Encode(rc, low, s1->Freq, sum + escFreq);
        PpmdSee_Update(see);
        Self->FoundState = s1;
        PpmdModeller_Update2(Self);
        return;
      }
      sum += (s->Freq & (int)(MASK(cur)));
      MASK(cur) = 0;
      s++;
    }
    while (--i);

    PpmdRangeEnc_Encode(rc, sum, escFreq, sum + escFreq);
    see->Summ = (uint16_t)(see->Summ + sum + escFreq);
  }
}

int PpmdModeller_DecodeSymbol(PPpmdModeller Self, PPpmdRangeDec rc)
{
  size_t charMask[256 / sizeof(size_t)];
  if (Self->MinContext->NumStats != 1)
  {
    TPpmdState *s = Ppmd_GetStats(Self, Self->MinContext);
    unsigned i;
    uint32_t count, hiCnt;
    if ((count = PpmdRangeDec_GetThreshold(rc, Self->MinContext->SummFreq)) < (hiCnt = s->Freq))
    {
      uint8_t symbol;
      PpmdRangeDec_Decode(rc, 0, s->Freq);
      Self->FoundState = s;
      symbol = s->Symbol;
      PpmdModeller_Update1_0(Self);
      return symbol;
    }
    Self->PrevSuccess = 0;
    i = Self->MinContext->NumStats - 1;
    do
    {
      if ((hiCnt += (++s)->Freq) > count)
      {
        uint8_t symbol;
        PpmdRangeDec_Decode(rc, hiCnt - s->Freq, s->Freq);
        Self->FoundState = s;
        symbol = s->Symbol;
        PpmdModeller_Update1(Self);
        return symbol;
      }
    }
    while (--i);
    if (count >= Self->MinContext->SummFreq)
      return -2;
    Self->HiBitsFlag = Self->HB2Flag[Self->FoundState->Symbol];
    PpmdRangeDec_Decode(rc, hiCnt, Self->MinContext->SummFreq - hiCnt);
    PPMD_SetAllBitsIn256Bytes(charMask);
    MASK(s->Symbol) = 0;
    i = Self->MinContext->NumStats - 1;
    do { MASK((--s)->Symbol) = 0; } while (--i);
  }
  else
  {
    uint16_t *prob = Ppmd_GetBinSumm(Self);
    if (PpmdRangeDec_DecodeBit(rc, *prob) == 0)
    {
      uint8_t symbol;
      *prob = (uint16_t)PPMD_UPDATE_PROB_0(*prob);
      symbol = (Self->FoundState = PpmdContext_OneState(Self->MinContext))->Symbol;
      PpmdModeller_UpdateBin(Self);
      return symbol;
    }
    *prob = (uint16_t)PPMD_UPDATE_PROB_1(*prob);
    Self->InitEsc = PPMD_kExpEscape[*prob >> 10];
    PPMD_SetAllBitsIn256Bytes(charMask);
    MASK(PpmdContext_OneState(Self->MinContext)->Symbol) = 0;
    Self->PrevSuccess = 0;
  }
  for (;;)
  {
    TPpmdState *ps[256], *s;
    uint32_t freqSum, count, hiCnt;
    TPpmdSee *see;
    unsigned i, num, numMasked = Self->MinContext->NumStats;
    do
    {
      Self->OrderFall++;
      if (!Self->MinContext->Suffix)
        return -1;
      Self->MinContext = Ppmd_GetContext(Self, Self->MinContext->Suffix);
    }
    while (Self->MinContext->NumStats == numMasked);
    hiCnt = 0;
    s = Ppmd_GetStats(Self, Self->MinContext);
    i = 0;
    num = Self->MinContext->NumStats - numMasked;
    do
    {
      int k = (int)(MASK(s->Symbol));
      hiCnt += (s->Freq & k);
      ps[i] = s++;
      i -= k;
    }
    while (i != num);

    see = PpmdModeller_MakeEscFreq(Self, numMasked, &freqSum);
    freqSum += hiCnt;
    count = PpmdRangeDec_GetThreshold(rc, freqSum);

    if (count < hiCnt)
    {
      uint8_t symbol;
      TPpmdState **pps = ps;
      for (hiCnt = 0; (hiCnt += (*pps)->Freq) <= count; pps++);
      s = *pps;
      PpmdRangeDec_Decode(rc, hiCnt - s->Freq, s->Freq);
      PpmdSee_Update(see);
      Self->FoundState = s;
      symbol = s->Symbol;
      PpmdModeller_Update2(Self);
      return symbol;
    }
    if (count >= freqSum)
      return -2;
    PpmdRangeDec_Decode(rc, hiCnt, freqSum - hiCnt);
    see->Summ = (uint16_t)(see->Summ + freqSum);
    do { MASK(ps[--i]->Symbol) = 0; } while (i != 0);
  }
}

PPpmdModeller PpmdModeller_Create()
{
  PPpmdModeller Self = malloc(sizeof(struct TPpmdModeller));
  PpmdModeller_Construct(Self);
  return Self;
};

void PpmdModeller_Destroy(PPpmdModeller Self)
{
  PpmdModeller_Free(Self);
  free(Self);
};

void PpmdModeller_SetMemSize(PPpmdModeller Self, uint32_t MemSize)
{
  PpmdModeller_Alloc(Self, MemSize);
};

void PpmdModeller_SetModelOrd(PPpmdModeller Self, uint32_t ModelOrd)
{
  PpmdModeller_Init(Self, ModelOrd);
};

inline int32_t PpmdModeller_Encode(PPpmdModeller Self, PPpmdRangeEnc RangeEnc, uint8_t *Buffer, int32_t BufSize)
{
  int32_t I;
  for (I = 0; I < BufSize; I++)
  {
    PpmdModeller_EncodeSymbol(Self, RangeEnc, Buffer[I]);
  }
  return I;
};

inline int32_t PpmdModeller_Decode(PPpmdModeller Self, PPpmdRangeDec RangeDec, uint8_t *Buffer, int32_t BufSize)
{
  int32_t I;
  for (I = 0; I < BufSize; I++)
  {
   Buffer[I] = PpmdModeller_DecodeSymbol(Self, RangeDec);
  }
  return I;
};

