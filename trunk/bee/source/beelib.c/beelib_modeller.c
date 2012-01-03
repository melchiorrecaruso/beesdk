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
  PUpdateSymbol UpdateSymbol;
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
      TTableCol *Part;            // Part of parameters Table
  struct TTable Table;            // parameters Table
};

PBaseCoder BaseCoder_Malloc(void *aCodec, PUpdateSymbol aUpdateSymbol)
{
  PBaseCoder Self = malloc(sizeof(struct TBaseCoder));

  Self->Codec        = aCodec;
  Self->UpdateSymbol = aUpdateSymbol;
  Self->Freq         = malloc(sizeof(unsigned int)*(MAXSYMBOL + 1));
  Self->Heap         = 0;
  Self->Cuts         = 0;
  Self->List         = malloc(sizeof(unsigned int)*(MAXSYMBOL + 1));

  return Self;
}

void BaseCoder_Free(PBaseCoder Self)
{
  free(Self->Freq);
  free(Self->Heap);
  free(Self->Cuts);
  free(Self->List);
  free(Self);
}

void BaseCoder_Add(PBaseCoder Self, unsigned int aSymbol)
{
  Self->Pos++;
  Self->LowestPos++;
  Self->Heap[Self->Pos & Self->MaxCounter].D = aSymbol;
}

void BaseCoder_CreateChild(PBaseCoder Self, PNode Parent)
{
  Self->Counter++;
  PNode result = Self->CurrentFreeNode;
  if (result == Self->LastFreeNode)
  {
    result = Self->Tear;
    PNode Link = result->Tear;
    if (result->Next != 0)
    {
      result->Next->Tear = Link;
      Link = result->Next;
    }
    if (result->Up != 0)
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
  result->Up   = 0;
  result->A    = Parent->A + 1;
  result->C    = Self->Heap[Parent->A & Self->MaxCounter].D;
  result->K    = INCREMENT;
}

void BaseCoder_CutTail(PBaseCoder Self, PPNode I, PPNode J)
{
  PNode P = Self->Tear;
  do
  {
    (*I)->Up->Tear = P;
    P = (*I)->Up;
    (*I)->Up = 0;
    I++;
  }
  while (!(I == J));
  Self->Tear = P;
}

void BaseCoder_Cut(PBaseCoder Self)
{
  if (Self->Cuts == 0)
  {
    Self->Cuts = malloc(sizeof(PPNode)*(Self->MaxCounter + 1));
  }

  PPNode I = &(Self->Cuts[0]);
  PPNode J = I;
  J++;

  (*I) = Self->Root;

  int Bound = (Self->SafeCounter * 3) / 4;
  PNode P = 0;
  do
  {
    P = (*I)->Up;
    do
    {
      Bound--;
      if (P->Up != 0)
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
          P->Up = 0;
        }
      }
      P = P->Next;
    }
    while (!(P == 0));
    I++;
  }
  while (!((I == J) || (Bound < 0)));

  if (I != J)
    BaseCoder_CutTail(Self, I, J);

  Self->Counter = ((Self->SafeCounter * 3) / 4) - Bound + 1;
  Self->ListCount = 0;
}

PNode BaseCoder_Tail(PBaseCoder Self, PNode Node)
{
  Node->A = Self->Pos;
  PNode result = Node->Up;

  if (result == 0)
    BaseCoder_CreateChild(Self, Node);
  else
  {
    unsigned char C = Self->Symbol;
    if (result->C != C)
      for (;;)
      {
        PNode P = result;
        result  = result->Next;
        if (result == 0)
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

void BaseCoder_Account(PBaseCoder Self)
{
  Self->I = 0;
  Self->Q = 0;
  Self->IncreaseIndex = 0;

  unsigned int J = 0;
  unsigned int K = 0;

  do
  {
    PNode P = Self->List[Self->I];
    if (P->Up !=0)
    {
      P = P->Up;
      if (Self->IncreaseIndex == 0)
        Self->IncreaseIndex = Self->I;

      if (P->Next != 0)
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
        while (! (P == 0));
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
        while (!(P == 0));
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

void BaseCoder_Step(PBaseCoder Self)
{
  // ClearLongword(&Freq[0], MaxSymbol + 1);
   int H;
  for (H = 0; H < MAXSYMBOL + 2; H++)
    Self->Freq[H] = 0;

  Self->R = MAXFREQ - MAXSYMBOL - 1;

  if (Self->ListCount > 0) BaseCoder_Account(Self);

  // Update aSymbol...
  // AddLongword(&Freq[0], MaxSymbol + 1, (R >> BitChain) + 1);
  unsigned int J = (Self->R >> BITCHAIN) + 1;
  unsigned int I = 0;
  do
  {
    Self->Freq[I] += J;
    I++;
  }
  while (!(I == MAXSYMBOL + 1));

  Self->Symbol = Self->UpdateSymbol(Self->Codec, Self->Freq, Self->Symbol);

  BaseCoder_Add(Self, Self->Symbol);

  PNode P = 0;
  if (Self->ListCount > 0)
  {
    // Update frequencies...
    unsigned int I = 0;
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
        while (!(P == 0));
      I++;
    }
    while (!(I > Self->IncreaseIndex));

    // Update Tree...
    I = 0;
    J = I;
    do
    {
      P = BaseCoder_Tail(Self, Self->List[I]);
      if (P != 0)
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

void BaseCoder_SetTable(PBaseCoder Self, const TTableParameters *T)
{
  Self->Table.Level = (unsigned int)(*T)[0] & 0xF;

  unsigned int I = 1;
  unsigned int J , K;

  for (J = 0; J <= TABLECOLS; J++)
    for (K = 0; K <= TABLESIZE; K++)
    {
      Self->Table.T[J][K] = (signed int)(*T)[I] + 1;
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

    // printf(" -DOPO- \n");
    // printf("%d\n", (*aPart)[0]);
    // printf("%d\n", (*aPart)[MAXSYMBOL + 2]);
    // printf("%d\n", (*aPart)[MAXSYMBOL + 3]);
    // printf("%d\n", (*aPart)[MAXSYMBOL + 4]);
    // printf("%d\n", (*aPart)[MAXSYMBOL + 5]);

  }
}

void BaseCoder_FreshFlexible(PBaseCoder Self)
{
  Self->Tear            = 0;
  Self->CurrentFreeNode = &(Self->Heap[0]);
  Self->LastFreeNode    = &(Self->Heap[Self->MaxCounter]);
  Self->Counter         = 0;
  Self->ListCount       = 0;
  Self->Pos             = 0;

  Self->Counter++;
  Self->Root = Self->CurrentFreeNode;
  Self->CurrentFreeNode++;

  Self->Root->Next = 0;
  Self->Root->Up   = 0;
  Self->Root->K    = INCREMENT;
  Self->Root->C    = 0;
  Self->Root->A    = 1;
  Self->LowestPos  = - ((int) Self->MaxCounter);
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

void BaseCoder_SetDictionary(PBaseCoder Self, unsigned int aDictLevel)
{
  if (aDictLevel != Self->DictLevel)
  {
	Self->DictLevel   = min(max(0, aDictLevel), 9);
	Self->MaxCounter  = (1 << ( 17 + Self->DictLevel)) - 1;
    Self->SafeCounter = Self->MaxCounter - 64;

	free(Self->Cuts);
	Self->Cuts = 0;

	free(Self->Heap);
	Self->Heap = malloc(sizeof(struct TNode)*(Self->MaxCounter + 1));
  }
  BaseCoder_FreshFlexible(Self);
}

unsigned int BaseCoder_UpdateSymbol(PBaseCoder Self, unsigned int aSymbol)
{
  Self->Part = &(Self->Table.T[0]);

  unsigned int result = 0;
  Self->Symbol = aSymbol >> 0x4;
  BaseCoder_Step(Self);
  result = Self->Symbol << 4;
  Self->Part = &(Self->Table.T[1]);
  Self->Symbol = aSymbol & 0xF;
  BaseCoder_Step(Self);
  result += Self->Symbol;

  // Reduce Tree...
  if (Self->SafeCounter < Self->Counter)
    BaseCoder_Cut(Self);

  // Update NodeList...
  if (Self->ListCount > Self->Table.Level)
  {
    // MoveLongwordUnchecked(List[1], List[0], ListCount - 1);
    unsigned int H;
    for (H = 1; H < Self->ListCount; H++)
       Self->List[H - 1] = Self->List[H];
  }
  else
    Self->ListCount++;

  Self->List[Self->ListCount - 1] = Self->Root;
  return result;
}
