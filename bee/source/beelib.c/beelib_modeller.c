#include <math.h>
#include <stdlib.h>
#include "beelib_modeller.h"
#include "beelib_rangecoder.h"

#define min(X, Y) ((X) < (Y) ? (X) : (Y))
#define max(X, Y) ((X) > (Y) ? (X) : (Y))

#define BITCHAIN   4             // Size of data portion, bit
#define MAXSYMBOL 15             // Size of source alphabet, symbols
#define INCREMENT  8             // Increment of symbol frequency

/* PPM modeller's node information */

struct TNode{
  unsigned short int K;          // frequency of This symbol
  unsigned char C;               // This symbol itself
  unsigned char D;               // Used FOR incoming data storage
  union {
    int A;                       // source address
    struct TNode *Tear;          // Next free node
  };
  struct TNode *Next;            // Next node of This or high level
  struct TNode *Up;
};

typedef struct TNode *PNode;     // Pointer to
typedef PNode *PPNode;           // Array of nodes...

/* TBaseCoder PPM modeller struct/methods implementation */

struct TBaseCoder {
          void *Codec;
   unsigned int DictLevel;
   unsigned int Symbol;
   unsigned int Pos;
            int LowestPos;       // Maximal heap Size
   unsigned int MaxCounter;      // Current heap Size
   unsigned int SafeCounter;     // Safe heap Size
   unsigned int Counter;

          PNode Heap;
         PPNode Cuts;
         PPNode List;
   unsigned int ListCount;

          PNode Root;
          PNode CurrentFreeNode;
          PNode LastFreeNode;
          PNode Tear;

   unsigned int IncreaseIndex;
   unsigned int I;
   unsigned int R;
   unsigned int Q;

          TFreq Freq;             // symbol frequencyes
     TTableCol *Part;             // Part of parameters Table
  struct TTable Table;            // parameters Table
};

PBaseCoder BaseCoder_Create(void *aCodec)
{
  PBaseCoder Self = malloc(sizeof(struct TBaseCoder));

  Self->Codec     = aCodec;
  Self->DictLevel = 0;
  Self->Freq      = malloc(sizeof(uint32)*(MAXSYMBOL + 1));
  Self->Heap      = NULL;
  Self->Cuts      = NULL;
  Self->List      = malloc(sizeof(uint32)*(MAXSYMBOL + 1));

  return Self;
}

void BaseCoder_Destroy(PBaseCoder Self)
{
  free(Self->Freq);
  free(Self->Heap);
  free(Self->Cuts);
  free(Self->List);
  free(Self);
}

static inline void BaseCoder_Add(PBaseCoder Self, uint32 aSymbol)
{
  Self->Pos++;
  Self->LowestPos++;
  Self->Heap[Self->Pos & Self->MaxCounter].D = aSymbol;
}

static inline void BaseCoder_CreateChild(PBaseCoder Self, PNode Parent)
{
  Self->Counter++;
  PNode result = Self->CurrentFreeNode;
  if (result == Self->LastFreeNode)
  {
    result = Self->Tear;
    PNode Link = result->Tear;
    if (result->Next != NULL)
    {
      result->Next->Tear = Link;
      Link = result->Next;
    }
    if (result->Up != NULL)
    {
      result->Up->Tear = Link;
      Link = result->Up;
    }
    Self->Tear = Link;
  }
  else
    Self->CurrentFreeNode++;

  result->Next = Parent->Up;
  Parent->Up   = result;
  result->Up   = NULL;
  result->A    = Parent->A + 1;
  result->C    = Self->Heap[Parent->A & Self->MaxCounter].D;
  result->K    = INCREMENT;
}

static inline void BaseCoder_CutTail(PBaseCoder Self, PPNode I, PPNode J)
{
  PNode P = Self->Tear;
  do
  {
    (*I)->Up->Tear = P;
    P = (*I)->Up;
    (*I)->Up = NULL;
    I++;
  }
  while (!(I == J));
  Self->Tear = P;
}

static inline void BaseCoder_Cut(PBaseCoder Self)
{
  if (Self->Cuts == NULL)
  {
    Self->Cuts = malloc(sizeof(PPNode)*(Self->MaxCounter + 1));
  }

  PPNode I = &(Self->Cuts[0]);
  PPNode J = I;
  J++;

  (*I) = Self->Root;

  int32 Bound = (Self->SafeCounter * 3) / 4;
  PNode P = NULL;
  do
  {
    P = (*I)->Up;
    do
    {
      Bound--;
      if (P->Up != NULL)
      {
        if (P->A > Self->LowestPos)
        {
          *J = P;
          J++;
        }
        else
        {
          P->Up->Tear = Self->Tear;
          Self->Tear = P->Up;
          P->Up = NULL;
        }
      }
      P = P->Next;
    }
    while (!(P == NULL));
    I++;
  }
  while (!((I == J) || (Bound < 0)));

  if (I != J)
    BaseCoder_CutTail(Self, I, J);

  Self->Counter = ((Self->SafeCounter * 3) / 4) - Bound + 1;
  Self->ListCount = 0;
}

static inline PNode BaseCoder_Tail(PBaseCoder Self, PNode Node)
{
  Node->A = Self->Pos;
  PNode result = Node->Up;

  if (result == NULL)
    BaseCoder_CreateChild(Self, Node);
  else
  {
    uint8 C = Self->Symbol;
    if (result->C != C)
      for (;;)
      {
        PNode P = result;
        result  = result->Next;
        if (result == NULL)
        {
          BaseCoder_CreateChild(Self, Node);
          break;
        }
        else
          if (result->C == C)
          {
            P->Next = result->Next;
            result->Next = Node->Up;
            Node->Up = result;
            break;
          }
      }
  }
  return result;
}

static inline void BaseCoder_Account(PBaseCoder Self)
{
  Self->I = 0;
  Self->Q = 0;
  Self->IncreaseIndex = 0;

  uint32 J = 0, K = 0;
  do
  {
    PNode P = Self->List[Self->I];
    if (P->Up !=NULL)
    {
      P = P->Up;
      if (Self->IncreaseIndex == 0)
        Self->IncreaseIndex = Self->I;

      if (P->Next != NULL)
      {
        // Undetermined context ...
        K = P->K * (*(Self->Part))[MAXSYMBOL + 2] >> 5;

		PNode Stored = P;
        P = P->Next;
        J = 1;
        do
        {
          J++;
          K += P->K;
          P = P->Next;
        }
        while (! (P == NULL));
        Self->Q += (*(Self->Part))[J];

        // Account:
		K = Self->R / (K + Self->Q);
		P = Stored;
		J = K * P->K * (*(Self->Part))[MAXSYMBOL + 2] >> 5;

		Self->R -= J;
        Self->Freq[P->C] += J;

        P = P->Next;
        do
        {
          J = K * P->K;
		  Self->R -= J;
          Self->Freq[P->C] += J;
		  P = P->Next;
        }
        while (!(P == NULL));
      }
      else
      {
        // Determined context ...
        K = (P->K * (*(Self->Part))[1]) / INCREMENT + 256;
		K = (Self->R / K) << 8;
        Self->Freq[P->C] += Self->R - K;
		Self->R = K;
      }
    }
    else
      if (P->A > Self->LowestPos)
      {
        // Determined context, encountered at first time ...
		BaseCoder_CreateChild(Self, P);
        K = Self->R / (*(Self->Part))[0] << 8;
        Self->Freq[P->Up->C] += Self->R - K;
        Self->R = K;
      }
    Self->I++;
  }
  while (!((Self->I == Self->ListCount) || (Self->R <= (*(Self->Part))[MAXSYMBOL + 5])));
  Self->ListCount = Self->I;
}

static inline void BaseCoder_Step(PBaseCoder Self, PRangeCoder_Update Update)
{
  // ClearLongword(&Freq[0], MaxSymbol + 1);
  int32 H;
  for (H = 0; H < MAXSYMBOL + 1; H++)
    Self->Freq[H] = 0;

  Self->R = MAXFREQ - MAXSYMBOL - 1;

  if (Self->ListCount > 0)
    BaseCoder_Account(Self);

  // Update aSymbol...
  // AddLongword(&Freq[0], MaxSymbol + 1, (R >> BitChain) + 1);
  uint32 I = 0, J = (Self->R >> BITCHAIN) + 1;
  do
  {
    Self->Freq[I] += J;
    I++;
  }
  while (!(I == MAXSYMBOL + 1));

  Self->Symbol = Update(Self->Codec, Self->Freq, Self->Symbol);

  BaseCoder_Add(Self, Self->Symbol);

  PNode P = NULL;
  if (Self->ListCount > 0)
  {
    // Update frequencies...
    uint32 I = 0;
    do
    {
      P = Self->List[I];
      if (I == Self->IncreaseIndex)
        P->K += INCREMENT;                // Special case...
      else
        P->K +=  (*(Self->Part))[MAXSYMBOL + 4];  // General case...
      if (P->K > (*(Self->Part))[MAXSYMBOL + 3])
        do
        {
          P->K >>= 1;
          P = P->Next;
        }
        while (!(P == NULL));
      I++;
    }
    while (!(I > Self->IncreaseIndex));

    // Update Tree...
    I = 0;
    J = I;
    do
    {
      P = BaseCoder_Tail(Self, Self->List[I]);
      if (P != NULL)
      {
        Self->List[J] = P;
        J++;
      }
      I++;
    }
    while (!(I == Self->ListCount));
    Self->ListCount = J;
  }
}

void BaseCoder_FreshFlexible(PBaseCoder Self)
{
  Self->Tear            = NULL;
  Self->CurrentFreeNode = &(Self->Heap[0]);
  Self->LastFreeNode    = &(Self->Heap[Self->MaxCounter]);
  Self->Counter         = 0;
  Self->ListCount       = 0;
  Self->Pos             = 0;

  Self->Counter++;
  Self->Root = Self->CurrentFreeNode;
  Self->CurrentFreeNode++;

  Self->Root->Next = NULL;
  Self->Root->Up   = NULL;
  Self->Root->K    = INCREMENT;
  Self->Root->C    = 0;
  Self->Root->A    = 1;
  Self->LowestPos  = - ((int32) Self->MaxCounter);
}

void BaseCoder_FreshSolid(PBaseCoder Self)
{
  if (Self->Counter > 1)
  {
    Self->ListCount = 1;
    Self->List[0]   = Self->Root;
  }
  else
    Self->ListCount = 0;
}

void BaseCoder_SetDictionary(PBaseCoder Self, uint32 aDictLevel)
{
  if (aDictLevel != Self->DictLevel)
  {
	Self->DictLevel   = min(max(0, aDictLevel), 9);
	Self->MaxCounter  = (1 << ( 17 + Self->DictLevel)) - 1;
    Self->SafeCounter = Self->MaxCounter - 64;

	free(Self->Cuts);
	Self->Cuts = NULL;

	free(Self->Heap);
	Self->Heap = malloc(sizeof(struct TNode)*(Self->MaxCounter + 1));
  }
  BaseCoder_FreshFlexible(Self);
}

void BaseCoder_SetTable(PBaseCoder Self, const TTableParameters *T)
{
  Self->Table.Level = (uint32)(*T)[0] & 0xF;

  uint32 I = 1;
  uint32 J , K;

  for (J = 0; J <= TABLECOLS; J++)
    for (K = 0; K <= TABLESIZE; K++)
    {
      Self->Table.T[J][K] = (int32)(*T)[I] + 1;
      I++;
    }

  for (I = 0; I <= 1; I++)
  {
    TTableCol *aPart;
    aPart = &(Self->Table.T[I]);

    (*aPart)[0]             =                  (*aPart)[0] + 256;               // Weight of first-encoutered deterministic symbol
    (*aPart)[MAXSYMBOL + 2] =                  (*aPart)[MAXSYMBOL + 2]  + 32;   // Recency scaling, R = R'' / 32, R'' = (R' + 1) * 32
    (*aPart)[MAXSYMBOL + 3] = INCREMENT *      (*aPart)[MAXSYMBOL + 3] << 2;    // Zero-valued parameter allowed...
    (*aPart)[MAXSYMBOL + 4] =                  (*aPart)[MAXSYMBOL + 4]  / 8;
    (*aPart)[MAXSYMBOL + 5] = floor(pow(1.082, (*aPart)[MAXSYMBOL + 5]) + 0.5); // Lowest value of interval
  }
}

static inline uint32 BaseCoder_Update(PBaseCoder Self, uint32 aSymbol, PRangeCoder_Update Update)
{
  Self->Part = &(Self->Table.T[0]);

  uint32 result = 0;
  Self->Symbol = aSymbol >> 0x4;
  BaseCoder_Step(Self, Update);

  result = Self->Symbol << 4;
  Self->Part = &(Self->Table.T[1]);
  Self->Symbol = aSymbol & 0xF;
  BaseCoder_Step(Self, Update);

  result += Self->Symbol;

  // Reduce Tree...
  if (Self->SafeCounter < Self->Counter)
    BaseCoder_Cut(Self);

  // Update NodeList...
  if (Self->ListCount > Self->Table.Level)
  {
    // MoveLongwordUnchecked(List[1], List[0], ListCount - 1);
    uint32 H;
    for (H = 1; H < Self->ListCount; H++)
      Self->List[H - 1] = Self->List[H];
  }
  else
    Self->ListCount++;

  Self->List[Self->ListCount - 1] = Self->Root;
  return result;
}

inline int32 BaseCoder_Encode(PBaseCoder Self, uint8 *Buffer, int32 BufSize)
{
  int32 I;
  for  (I = 0; I < BufSize; I++)
  {
    BaseCoder_Update(Self, Buffer[I], (PRangeCoder_Update)RangeEncoder_Update);
  }
  return I;
};

inline int32 BaseCoder_Decode(PBaseCoder Self, uint8 *Buffer, int32 BufSize)
{
  int32 I;
  for  (I = 0; I < BufSize; I++)
  {
    Buffer[I] = BaseCoder_Update(Self, 0, (PRangeCoder_Update)RangeDecoder_Update);
  }
  return I;
};
