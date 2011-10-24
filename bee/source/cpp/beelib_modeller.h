/*
  Copyright (c) 2003-2011 Andrew Filinsky

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

    TBaseCoder class, PPM modeller;

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
    v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;

    v0.8.0 build 1400 - 2011.08.04 by Melchiorre Caruso.
*/

#ifndef BEELIB_MODELLER_H
#define BEELIB_MODELLER_H

#include "beelib_codec.h"                           // TSecondaryCodec, ...
#include "beelib_types.h"	       	                // TTable, TTableCol, ...
#include "beelib_stream.h"                          // TStream, ...
#include "beelib_assembler.h"                       // Low-level routines ...


static const int BitChain  = 4;                     // Size of data portion, bit
static const int MaxSymbol = (1 << BitChain) - 1;   // Size of source alphabet, symbols
static const int Increment = 8;                     // Increment of symbol frequency

struct  TNode;
typedef TNode* PNode;                               // Pointer to modeller's node information
typedef PNode* PPNode;                              // Array of nodes...


/* modeller's node information */

struct TNode {
  PNode Next, Up;                // Next node of This or high level
  unsigned short int K;          // frequency of This symbol
  unsigned char c;               // This symbol itself
  unsigned char D;               // Used FOR incoming data storage
  union {
    struct {int A;     };      // source address
    struct {PNode Tear;};      // Next free node
  };
};

/* PPM modeller */

class TBaseCoder {
private:
  unsigned int FDictionaryLevel;
  TSecondaryCodec* FCodec;                       // Secondary encoder or decoder
  unsigned int symbol;
  unsigned int Pos;
  int LowestPos;                                 // Maximal heap Size    // Safe heap Size
  unsigned int MaxCounter, SafeCounter, Counter; // Current heap Size
  TNode* heap;
  PNode* Cuts;
  PNode* List;


  unsigned int CutsLen;

  unsigned int ListCount;

  PNode Root;
  PNode CurrentFreeNode;
  PNode LastFreeNode;
  PNode Tear;
  unsigned int IncreaseIndex;
  unsigned int I, R, Q;
  TFreq Freq;                                    // symbol frequencyes
  TTableCol* Part;                               // Part of parameters Table
  TTable Table;                                  // parameters Table
  void Add( unsigned int aSymbol);
  void CreateChild( PNode Parent);
  void Cut();
  void Cut_Tail(PPNode I, PPNode J);
  PNode Tail(PNode node);
  void Account();
  void Step();
public:
  TBaseCoder(TSecondaryCodec* aCodec);
  virtual ~TBaseCoder();
  void SetTable(const TTableParameters& T);
  void SetDictionary(unsigned int aDictionaryLevel);
  void FreshFlexible();
  void FreshSolid();
  unsigned int UpdateModel(unsigned int aSymbol);
};

#endif //  BEELIB_MODELLER_H
