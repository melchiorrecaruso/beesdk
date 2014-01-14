/*
  Copyright (c) 2012-2014 Melchiorre Caruso

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the free software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT any WARRANTY; WITHOUT even the implied WARRANTY of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License FOR more details.

  you should have received A copy of the GNU General Public License
  along with This program; if not, write to the free software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
  Contains:

    BEE compression library.

    Original source code at http://www.compression.ru/fa/ (public domain).

  Translated:

    v1.0.0 build 2202 - 2014.01.13 by Melchiorre Caruso.

  Modifyed:

*/

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include "libbx_stream.h"

// ------------------------------------------------------------------ //
//  Table parameters                                                  //
// ------------------------------------------------------------------ //

#define TABLESIZE       20  // array [0..20]
#define TABLECOLS        1  // array [0.. 1]
#define TABLEPARAMETERS 42  // array [0..42]

typedef uint32_t TTableCol[TABLESIZE + 1];

struct TTable{
  uint32_t Level;
  TTableCol T[TABLECOLS + 1];
};

typedef uint8_t TTableParameters[TABLEPARAMETERS + 1];

//  Default table parameters

static const TTableParameters DefaultTableParameters = {
    3, 163, 157,  65,  93, 117, 135, 109, 126, 252, 172, 252, 152,
  227, 249, 249, 253, 196,  27,  82,  93,  74, 182, 245,  40,  67,
   77, 143, 133, 135, 128, 155, 207, 177, 225, 251, 253, 248,  73,
   35,  15, 107, 143};

//  Default dictionary level

static const int32_t DefaultDictionaryLevel = 3;

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

// ------------------------------------------------------------------ //
//  Range Coder                                                       //
// ------------------------------------------------------------------ //

#define MAXFREQ   16777215L
#define TFREQSIZE 16L

#define TOP       16777216L
#define THRES     4278190080U

// Array of frequencyes

typedef uint32_t *TFreq;

// BeeRangeEncoder structure

struct TBeeRangeEnc {
  PWriteStream FStream;
      uint32_t FRange;
      uint32_t FLow;
      uint32_t FCode;
      uint32_t FCarry;
      uint32_t FCache;
      uint32_t FFNum;
};

typedef struct TBeeRangeEnc *PBeeRangeEnc;

// BeeRangeDecoder structure

struct TBeeRangeDec {
  PReadStream FStream;
    uint32_t FRange;
    uint32_t FLow;
    uint32_t FCode;
    uint32_t FCarry;
    uint32_t FCache;
    uint32_t FFNum;
};

typedef struct TBeeRangeDec *PBeeRangeDec;

/* BeeRangeCoder_Update definition */

typedef uint32_t (*PRangeCod_Update) (void*, uint32_t*, uint32_t);

// BeeRangeEncoder routines

PBeeRangeEnc BeeRangeEnc_Create(void *aStream, PStreamWrite aStreamWrite)
{
  PBeeRangeEnc Self = malloc(sizeof(struct TBeeRangeEnc));
  Self->FStream     = WriteStream_Create(aStream, aStreamWrite);
  return Self;
}

void BeeRangeEnc_Destroy(PBeeRangeEnc Self)
{
  WriteStream_Destroy(Self->FStream);
  free(Self);
}

void BeeRangeEnc_StartEncode(PBeeRangeEnc Self)
{
  Self->FRange = 0xFFFFFFFF;
  Self->FLow   = 0;
  Self->FFNum  = 0;
  Self->FCarry = 0;
}

static inline void BeeRangeEnc_ShiftLow(PBeeRangeEnc Self)
{
  if ((Self->FLow < THRES) || (Self->FCarry != 0))
  {
    WriteStream_Write(Self->FStream, Self->FCache + Self->FCarry);

    while (Self->FFNum != 0)
    {
      WriteStream_Write(Self->FStream, Self->FCarry - 1);
      Self->FFNum--;
    }
    Self->FCache = Self->FLow >> 24;
    Self->FCarry = 0;
  } else
    Self->FFNum++;

  Self->FLow <<= 8;
}

static inline void BeeRangeEnc_Encode(PBeeRangeEnc Self, uint32_t CumFreq,  uint32_t Freq,  uint32_t TotFreq)
{
  uint32_t Tmp = Self->FLow;
  Self->FLow   += _MulDiv(Self->FRange, CumFreq, TotFreq);
  Self->FCarry += (uint32_t)(Self->FLow < Tmp);
  Self->FRange  = _MulDiv(Self->FRange, Freq, TotFreq);
  while (Self->FRange < TOP)
  {
    Self->FRange <<= 8;
    BeeRangeEnc_ShiftLow(Self);
  }
}

void BeeRangeEnc_FinishEncode(PBeeRangeEnc Self)
{
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  BeeRangeEnc_ShiftLow(Self);
  WriteStream_FlushBuffer(Self->FStream);
}

inline uint32_t BeeRangeEnc_Update(PBeeRangeEnc Self, TFreq Freq, uint32_t aSymbol)
{
  // Count CumFreq...
  uint32_t CumFreq = 0, I = 0;
  while (I < aSymbol)
  {
    CumFreq += Freq[I];
    I++;
  }
  // Count TotFreq...
  uint32_t TotFreq = CumFreq;
  I = TFREQSIZE;
  do
  {
    I--;
    TotFreq += Freq[I];
  }
  while (!(I == aSymbol));
  // Encode...
  BeeRangeEnc_Encode(Self, CumFreq, Freq[aSymbol], TotFreq);
  // Return result...
  return aSymbol;
}

// BeeRangeDecoder routines

PBeeRangeDec BeeRangeDec_Create(void *aStream, PStreamRead aStreamRead)
{
  PBeeRangeDec Self = malloc(sizeof(struct TBeeRangeDec));
  Self->FStream     = ReadStream_Create(aStream, aStreamRead);
  return Self;
}

void BeeRangeDec_Destroy(PBeeRangeDec Self)
{
  ReadStream_Destroy(Self->FStream);
  free(Self);
}

void BeeRangeDec_StartDecode(PBeeRangeDec Self)
{
  Self->FRange = 0xFFFFFFFF;
  Self->FLow   = 0;
  Self->FFNum  = 0;
  Self->FCarry = 0;

  Self->FCode = (Self->FCode << 8) + ReadStream_Read(Self->FStream);
  Self->FCode = (Self->FCode << 8) + ReadStream_Read(Self->FStream);
  Self->FCode = (Self->FCode << 8) + ReadStream_Read(Self->FStream);
  Self->FCode = (Self->FCode << 8) + ReadStream_Read(Self->FStream);
  Self->FCode = (Self->FCode << 8) + ReadStream_Read(Self->FStream);
}

void BeeRangeDec_FinishDecode(PBeeRangeDec Self)
{
  ReadStream_ClearBuffer(Self->FStream);
}

inline uint32_t BeeRangeDec_GetFreq(PBeeRangeDec Self, uint32_t TotFreq)
{
  return _MulDecDiv(Self->FCode + 1, TotFreq, Self->FRange);
}

static inline void BeeRangeDec_Decode(PBeeRangeDec Self, uint32_t CumFreq, uint32_t Freq, uint32_t TotFreq)
{
  Self->FCode -= _MulDiv(Self->FRange, CumFreq, TotFreq);
  Self->FRange = _MulDiv(Self->FRange,    Freq, TotFreq);

  while (Self->FRange < TOP)
  {
    Self->FCode = (Self->FCode << 8) + ReadStream_Read(Self->FStream);
    Self->FRange <<= 8;
  }
}

inline uint32_t BeeRangeDec_Update(PBeeRangeDec Self, TFreq Freq, uint32_t aSymbol)
{
  // Count TotFreq...
  uint32_t TotFreq = 0;
  aSymbol = TFREQSIZE;
  do
  {
    aSymbol--;
    TotFreq += Freq[aSymbol];
  }
  while (!(aSymbol == 0));
  // Count CumFreq...
  uint32_t CumFreq = BeeRangeDec_GetFreq(Self, TotFreq);
  // Search aSymbol...
  uint32_t SumFreq = 0;
  aSymbol = 0;
  while (SumFreq + Freq[aSymbol] <= CumFreq)
  {
    SumFreq += Freq[aSymbol];
    aSymbol++;
  }
  // Finish Decode...
  BeeRangeDec_Decode(Self, SumFreq, Freq[aSymbol], TotFreq);
  // Return Result...
  return aSymbol;
}

// ------------------------------------------------------------------ //
//  Modeller                                                          //
// ------------------------------------------------------------------ //

#define BITCHAIN   4             // Size of data portion, bit
#define MAXSYMBOL 15             // Size of source alphabet, symbols
#define INCREMENT  8             // Increment of symbol frequency

// PPM modeller's node information

struct TNode{
  uint16_t K;                    // frequency of This symbol
   uint8_t C;                    // This symbol itself
   uint8_t D;                    // Used FOR incoming data storage

  union {
   int32_t A;                    // source address
    struct TNode *Tear;          // Next free node
  };
  struct TNode *Next;            // Next node of This or high level
  struct TNode *Up;
};

typedef struct TNode *PNode;     // Pointer to
typedef PNode *PPNode;           // Array of nodes...

// TBaseCoder PPM modeller structure

struct TBeeModeller {
          void *Codec;
       uint32_t DictLevel;
       uint32_t Symbol;
       uint32_t Pos;
        int32_t LowestPos;       // Maximal heap Size
       uint32_t MaxCounter;      // Current heap Size
       uint32_t SafeCounter;     // Safe heap Size
       uint32_t Counter;

           PNode Heap;
          PPNode Cuts;
          PPNode List;
       uint32_t ListCount;

          PNode Root;
          PNode CurrentFreeNode;
          PNode LastFreeNode;
          PNode Tear;

       uint32_t IncreaseIndex;
       uint32_t I;
       uint32_t R;
       uint32_t Q;

          TFreq Freq;            // symbol frequencyes
     TTableCol *Part;            // Part of parameters Table
  struct TTable Table;           // parameters Table
};

typedef struct TBeeModeller *PBeeModeller;

// TBaseCoder PPM modeller routines

PBeeModeller BeeModeller_Create(void *aCodec)
{
  PBeeModeller Self = malloc(sizeof(struct TBeeModeller));

  Self->Codec     = aCodec;
  Self->DictLevel = 0;
  Self->Freq      = malloc(sizeof(uint32_t)*(MAXSYMBOL + 1));

  Self->Heap      = NULL;
  Self->Cuts      = NULL;
  Self->List      = malloc(sizeof(uint32_t)*(MAXSYMBOL + 1));

  return Self;
}

void BeeModeller_Destroy(PBeeModeller Self)
{
  free(Self->Freq);
  free(Self->Heap);
  free(Self->Cuts);
  free(Self->List);
  free(Self);
}

static inline void BeeModeller_Add(PBeeModeller Self, uint32_t aSymbol)
{
  Self->Pos++;
  Self->LowestPos++;
  Self->Heap[Self->Pos & Self->MaxCounter].D = aSymbol;
}

static inline void BeeModeller_CreateChild(PBeeModeller Self, PNode Parent)
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

static inline void BeeModeller_CutTail(PBeeModeller Self, PPNode I, PPNode J)
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

static inline void BeeModeller_Cut(PBeeModeller Self)
{
  if (Self->Cuts == NULL)
  {
    Self->Cuts = malloc(sizeof(PPNode)*(Self->MaxCounter + 1));
  }

  PPNode I = &(Self->Cuts[0]);
  PPNode J = I;
  J++;

  (*I) = Self->Root;
  int32_t Bound = (Self->SafeCounter * 3) / 4;
  PNode P;
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
    BeeModeller_CutTail(Self, I, J);

  Self->Counter = ((Self->SafeCounter * 3) / 4) - Bound + 1;
  Self->ListCount = 0;
}

static inline PNode BeeModeller_Tail(PBeeModeller Self, PNode Node)
{
  Node->A = Self->Pos;
  PNode result = Node->Up;

  if (result == NULL)
    BeeModeller_CreateChild(Self, Node);
  else
  {
    uint8_t C = Self->Symbol;
    if (result->C != C)
      for (;;)
      {
        PNode P = result;
        result  = result->Next;
        if (result == NULL)
        {
          BeeModeller_CreateChild(Self, Node);
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

static inline void BeeModeller_Account(PBeeModeller Self)
{
  Self->I    = 0;
  uint32_t J = 0;
  Self->Q    = 0;
  Self->IncreaseIndex = 0;

  uint32_t K = 0;
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
		BeeModeller_CreateChild(Self, P);
        K = Self->R / (*(Self->Part))[0] << 8;
        Self->Freq[P->Up->C] += Self->R - K;
        Self->R = K;
      }
    Self->I++;
  }
  while (!((Self->I == Self->ListCount) || (Self->R <= (*(Self->Part))[MAXSYMBOL + 5])));
  Self->ListCount = Self->I;
}

static inline void BeeModeller_Step(PBeeModeller Self, PRangeCod_Update Update)
{
  // ClearLongword(&Freq[0], MaxSymbol + 1);
  int32_t H;
  for (H = 0; H < MAXSYMBOL + 1; H++)
    Self->Freq[H] = 0;

  Self->R = MAXFREQ - MAXSYMBOL - 1;

  if (Self->ListCount > 0)
    BeeModeller_Account(Self);

  // Update aSymbol...
  // AddLongword(&Freq[0], MaxSymbol + 1, (R >> BitChain) + 1);
  uint32_t I = 0, J = (Self->R >> BITCHAIN) + 1;
  do
  {
    Self->Freq[I] += J;
    I++;
  }
  while (!(I == MAXSYMBOL + 1));

  Self->Symbol = Update(Self->Codec, Self->Freq, Self->Symbol);

  BeeModeller_Add(Self, Self->Symbol);

  PNode P = NULL;
  if (Self->ListCount > 0)
  {
    // Update frequencies...
    uint32_t I = 0;
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
      P = BeeModeller_Tail(Self, Self->List[I]);
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

void BeeModeller_FreshFlexible(PBeeModeller Self)
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
  Self->LowestPos  = - ((int32_t) Self->MaxCounter);
}

void BeeModeller_FreshSolid(PBeeModeller Self)
{
  if (Self->Counter > 1)
  {
    Self->ListCount = 1;
    Self->List[0]   = Self->Root;
  }
  else
    Self->ListCount = 0;
}

void BeeModeller_SetDictionaryLevel(PBeeModeller Self, uint32_t aDictLevel)
{
  Self->DictLevel   = aDictLevel;
  Self->MaxCounter  = (1 << ( 17 + Self->DictLevel)) - 1;
  Self->SafeCounter = Self->MaxCounter - 64;

  free(Self->Cuts);
  Self->Cuts = NULL;

  free(Self->Heap);
  Self->Heap = malloc(sizeof(struct TNode)*(Self->MaxCounter + 1));

  BeeModeller_FreshFlexible(Self);
}

void BeeModeller_SetTableParameters(PBeeModeller Self, uint8_t *Table)
{
  Self->Table.Level = (uint32_t)Table[0] & 0xF;

  uint32_t I = 1;
  uint32_t J , K;

  for (J = 0; J <= TABLECOLS; J++)
    for (K = 0; K <= TABLESIZE; K++)
    {
      Self->Table.T[J][K] = (int32_t)Table[I] + 1;
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

static inline uint32_t BeeModeller_Update(PBeeModeller Self, uint32_t aSymbol, PRangeCod_Update Update)
{
  Self->Part = &(Self->Table.T[0]);

  Self->Symbol = aSymbol >> 0x4;
  BeeModeller_Step(Self, Update);

  uint32_t result = Self->Symbol << 4;
  Self->Part = &(Self->Table.T[1]);
  Self->Symbol = aSymbol & 0xF;
  BeeModeller_Step(Self, Update);

  result += Self->Symbol;

  // Reduce Tree...
  if (Self->SafeCounter < Self->Counter)
    BeeModeller_Cut(Self);

  // Update NodeList...
  if (Self->ListCount > Self->Table.Level)
  {
    // MoveLongwordUnchecked(List[1], List[0], ListCount - 1);
    uint32_t H;
    for (H = 1; H < Self->ListCount; H++)
      Self->List[H - 1] = Self->List[H];
  }
  else
    Self->ListCount++;

  Self->List[Self->ListCount - 1] = Self->Root;
  return result;
}

inline uint32_t BeeModeller_Encode(PBeeModeller Self, uint8_t *Buffer, uint32_t BufSize)
{
  uint32_t I;
  for (I = 0; I < BufSize; I++)
  {
    BeeModeller_Update(Self, Buffer[I], (PRangeCod_Update)BeeRangeEnc_Update);
  }
  return I;
};

inline uint32_t BeeModeller_Decode(PBeeModeller Self, uint8_t *Buffer, uint32_t BufSize)
{
  uint32_t I;
  for (I = 0; I < BufSize; I++)
  {
     Buffer[I] = BeeModeller_Update(Self, 0, (PRangeCod_Update)BeeRangeDec_Update);
  }
  return I;
};
