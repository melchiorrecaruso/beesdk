#include "beelib_modeller.h"
#include <cmath>

#define min(X, Y)  ((X) < (Y) ? (X) : (Y))
#define max(X, Y)  ((X) > (Y) ? (X) : (Y))

/* TBaseCoder */

TBaseCoder::TBaseCoder(TSecondaryCodec* aCodec)
{
  FCodec  = aCodec;
  Freq    = new unsigned int [MaxSymbol + 1];
  List    = new PNode [MaxSymbol + 1];

  CutsLen = 0;
}


TBaseCoder::~TBaseCoder()
{
  delete[] Freq;
  delete[] heap;
  delete[] Cuts;
  delete[] List;

  CutsLen = 0;
}

void TBaseCoder::SetTable(const TTableParameters& T)
{
  Table.Level = (unsigned int)T[0] & 0xF;

  unsigned int I = 1;
  for (unsigned int J = 0; J <= TableCols - 1; J++)
    for (unsigned int K = 0; K <= TableSize - 1; K++)
    {
      Table.T[J][K] = ((signed int) T[I] ) + 1;
      I++;
    }

  for (I = 0; I <= 1; I++)
  {
    TTableCol& aPart = Table.T[I];

    aPart[0] = aPart[0] + 256;                                            // Weight of first-encoutered deterministic symbol
    aPart[MaxSymbol + 2] = aPart[MaxSymbol + 2] + 32;                     // Recency scaling, R = R'' / 32, R'' = (R' + 1) * 32
    aPart[MaxSymbol + 3] = Increment * aPart[MaxSymbol + 3] << 2;         // Zero-valued parameter allowed...
    aPart[MaxSymbol + 4] = aPart[MaxSymbol + 4] / 8;
    aPart[MaxSymbol + 5] = floor(pow(1.082, aPart[MaxSymbol + 5]) + 0.5); // Lowest value of interval
  }
}

void TBaseCoder::SetDictionary(unsigned int aDictionaryLevel)
{
  if (aDictionaryLevel != FDictionaryLevel)
  {
	FDictionaryLevel = min(max(0, aDictionaryLevel), 9);
	MaxCounter       = (1 << ( 17 + FDictionaryLevel)) - 1;
    SafeCounter      = MaxCounter - 64;

	delete []Cuts;
	CutsLen = 0;

	delete []heap;
	heap = new TNode [MaxCounter + 1];
  }
  FreshFlexible();
}

void TBaseCoder::FreshFlexible()
{
  Tear            = NULL;
  CurrentFreeNode = &heap[0];
  LastFreeNode    = &heap[MaxCounter];
  Counter         = 0;
  ListCount       = 0;
  Pos             = 0;

  Counter++;
  Root = CurrentFreeNode;
  CurrentFreeNode++;

  Root->Next = NULL;
  Root->Up   = NULL;
  Root->K    = Increment;
  Root->c    = 0;
  Root->A    = 1;
  LowestPos  = - ((int) MaxCounter);
}

void TBaseCoder::FreshSolid()
{
  if (Counter > 1)
  {
    ListCount = 1;
    List[0]   = Root;
  }
  else
    ListCount = 0;
}

void TBaseCoder::Add(unsigned int aSymbol)
{
  Pos++;
  LowestPos++;
  heap[Pos & MaxCounter].D = aSymbol;
}

void TBaseCoder::CreateChild(PNode Parent)
{
  Counter++;
  PNode result = CurrentFreeNode;
  if (result == LastFreeNode)
  {
    result = Tear;
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
    Tear = Link;
  }
  else
    CurrentFreeNode++;

  result->Next = Parent->Up;
  Parent->Up   = result;
  result->Up   = NULL;
  result->A    = Parent->A + 1;
  result->c    = heap[Parent->A & MaxCounter].D;
  result->K    = Increment;
}


void TBaseCoder::Cut()
{
  if (CutsLen == 0)
  {
    Cuts = new PNode [MaxCounter + 1];
	CutsLen = MaxCounter + 1;
  }

  PPNode I = &Cuts[0];
  PPNode J = I;
  J++;

  (*I) = Root;

  PNode P = NULL;
  int Bound = (SafeCounter * 3) / 4;
  do
  {
    P = (*I)->Up;
    do
    {
      Bound--;
      if (P->Up != NULL)
        if (P->A > LowestPos)
        {
          *J = P;
          J++;
        }
        else
        {
          P->Up->Tear = Tear;
          Tear = P->Up;
          P->Up = NULL;
        }
      P = P->Next;
    }
    while (!(P == NULL));
    I++;
  }
  while (!((I == J) || (Bound < 0)));

  if (I != J)
    Cut_Tail( I, J );

  Counter = ((SafeCounter * 3) / 4) - Bound + 1;
  ListCount = 0;
}


void TBaseCoder::Cut_Tail(PPNode I, PPNode J)
{
  PNode P = Tear;
  do
  {
    (*I)->Up->Tear = P;
    P = (*I)->Up;
    (*I)->Up = NULL;
    I++;
  }
  while (!(I == J));
  Tear = P;
}


void TBaseCoder::Account()
{
  I = 0;
  Q = 0;
  IncreaseIndex = 0;
  unsigned int J = 0;
  unsigned int K = 0;

  do
  {
    PNode P = List[I];
    if (P->Up != NULL)
    {
      P = P->Up;
      if (IncreaseIndex == 0)
        IncreaseIndex = I;

      if (P->Next != NULL)
      {
        // Undetermined context ...
        K = P->K * (*Part)[MaxSymbol + 2] >> 5;

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
        Q += (*Part)[J];

        // Account:
		K = R / (K + Q);
		P = Stored;
		J = K * P->K * (*Part)[MaxSymbol + 2] >> 5;

		R -= J;
        Freq[P->c] += J;

        P = P->Next;
        do
        {
          J = K * P->K;
		  R -= J;
          Freq[P->c] += J;
		  P = P->Next;
        }
        while (!(P == NULL));
      }
      else
      {
        // Determined context ...
        K = (P->K * (*Part)[1]) / Increment + 256;
		K = (R / K) << 8;
        Freq[P->c] += R - K;
		R = K;
      }
    }
    else
      if (P->A > LowestPos)
      {
        // Determined context, encountered at first time ...
		CreateChild(P);
        K = R / (*Part)[0] << 8;
        Freq[P->Up->c] += R - K;
        R = K;
      }
    I++;
  }
  while (!((I == ListCount) || (R <= (*Part)[MaxSymbol + 5])));
  ListCount = I;
}


PNode TBaseCoder::Tail(PNode node)
{
  node->A = Pos;
  PNode result = node->Up;

  if (result == NULL)
    CreateChild(node);
  else
  {
    unsigned char c = symbol;
    if (result->c != c)
      do
      {
        PNode P = result;
        result  = result->Next;
        if (result == NULL)
        {
          CreateChild(node);
          break;
        }
        else
          if (result->c == c)
          {
            P->Next = result->Next;
            result->Next = node->Up;
            node->Up = result;
            break;
          }
      }
      while (!( false));
  }
  return result;
}


void TBaseCoder::Step()
{
  // ClearLongword(&Freq[0], MaxSymbol + 1);
  for (int H = 0; H < MaxSymbol + 2; H++)
    Freq[H] = 0;

  R = MaxFreq - MaxSymbol - 1;

  if (ListCount > 0) Account();

  // Update aSymbol...
  // AddLongword(&Freq[0], MaxSymbol + 1, (R >> BitChain) + 1);
  unsigned int J = (R >> BitChain) + 1;
  unsigned int I = 0;
  do
  {
    Freq[I] += J;
    I++;
  }
  while (!(I == MaxSymbol + 1));

  symbol = FCodec->UpdateSymbol(Freq, symbol);
  Add(symbol);
  PNode P = NULL;
  if (ListCount > 0)
  {
    // Update frequencies...
    unsigned int I = 0;
    do
    {
      P = List[I];
      if (I == IncreaseIndex)
        P->K += Increment;                // Special case...
      else
        P->K +=  (*Part)[MaxSymbol + 4];  // General case...
      if (P->K > (*Part)[MaxSymbol + 3])
        do
        {
          P->K >>= 1;
          P = P->Next;
        }
        while (!(P == NULL));
      I++;
    }
    while (!(I > IncreaseIndex));

    // Update Tree...
    I = 0;
    J = I;
    do
    {
      P = Tail(List[I]);
      if (P != NULL)
      {
        List[J] = P;
        J++;
      }
      I++;
    }
    while (!(I == ListCount));
    ListCount = J;
  }
}

unsigned int TBaseCoder::UpdateModel( unsigned int aSymbol)
{
  Part = &Table.T[0];

  unsigned int result = 0;
  symbol = aSymbol >> 0x4;
  Step();
  result = symbol << 4;
  Part = &Table.T[1];
  symbol = aSymbol & 0xF;
  Step();
  result += symbol;

  // Reduce Tree...
  if (SafeCounter < Counter) Cut();

  // Update NodeList...
  if (ListCount > Table.Level)
  {
    // MoveLongwordUnchecked(List[1], List[0], ListCount - 1);
    for (unsigned int H = 1; H < ListCount; H++)
       List[H - 1] = List[H];
  }
  else
    ListCount++;

  List[ListCount - 1] = Root;
  return result;
}

