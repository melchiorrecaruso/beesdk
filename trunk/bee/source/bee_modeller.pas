{
  Copyright (c) 2003-2008 Andrew Filinsky

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

  TBaseCoder class, PPM modeller;

  Modifyed:

  v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
  v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
  v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;
  
  v0.7.9 build 0890 - 2008.10.18 by Melchiorre Caruso.
}

unit Bee_Modeller;

{$I compiler.inc}

interface

uses
  Math,                                 // Max (), Min (), ...
  Classes,                              // TStream

  Bee_Codec,                            // TSecondaryFCodec, ...
  Bee_Assembler,                        // Low-level routines ...
  Bee_Configuration;                    // TTable, TTableCol, ...

const
  BitChain  = 4;                        // Size of data portion, bit
  MaxSymbol = 1 shl BitChain - 1;       // Size of source alphabet, symbols
  Increment = 8;                        // Increment of symbol frequency

type
  PNode  = ^TNode;
  // Pointer to modeller's node information...
  PPNode = ^PNode;                      // Array of nodes...

  // Modeller's node information...

  TNode = record
    Next, Up: PNode;                    // Next node of this or high level
    K: word;                            // Frequency of this symbol
    C: byte;                            // This symbol itself
    D: byte;                            // Used for incoming data storage
    case cardinal of
      1: (A: integer);                  // Source address
      2: (Tear: PNode);                 // Next free node
  end;

  // PPM modeller...

  TBaseCoder = class(TObject)
  public
    constructor Create(aCodec: TSecondaryCodec);
    destructor Destroy; override;

    procedure SetTable(const T: TTableParameters);
    procedure SetDictionary(aDictionaryLevel: cardinal);
    procedure FreshFlexible;
    procedure FreshSolid;
    function UpdateModel(aSymbol: cardinal): cardinal;

  private
    procedure Add(aSymbol: cardinal);
    procedure CreateChild(Parent: PNode);
    procedure Cut;
    procedure Cut_Tail(I, J: PPNode);
    function Tail(Node: PNode): PNode;
    procedure Account;
    procedure Step;

  private
    FDictionaryLevel: cardinal;
    FCodec: TSecondaryCodec;            // Secondary encoder or decoder...

    Symbol: cardinal;
    Pos:    cardinal;
    LowestPos: integer;

    MaxCounter,                         // Maximal heap size
    SafeCounter,                        // Safe heap size
    Counter: cardinal;                  // Current heap size

    Heap:      array of TNode;
    Cuts:      array of PNode;
    List:      array of PNode;
    ListCount: cardinal;

    Root: PNode;
    CurrentFreeNode: PNode;
    LastFreeNode: PNode;
    Tear: PNode;

    IncreaseIndex: cardinal;
    I, R, Q: cardinal;

    Freq:  TFreq;                       // Symbol frequencyes...
    Part:  ^TTableCol;                  // Part of parameters Table...
    Table: TTable;                      // Parameters Table...
  end;

implementation

// TBaseCoder...

constructor TBaseCoder.Create(aCodec: TSecondaryCodec);
begin
  inherited Create;
  FCodec := aCodec;
  SetLength(Freq, MaxSymbol + 1);
  SetLength(List, 16);
end;

destructor TBaseCoder.Destroy;
begin
  Freq := nil;
  Heap := nil;
  Cuts := nil;
  List := nil;
  inherited Destroy;
end;

procedure TBaseCoder.SetTable(const T: TTableParameters);
var
  I:     integer;
  P:     ^integer;
  aPart: ^TTableCol;
begin
  P := @Table;
  I := 1;
  repeat
    P^ := integer(T[I]) + 1;
    Inc(P);
    Inc(I);
  until I > SizeOf(T);

  Table.Level := Table.Level - 1;
  Table.Level := Table.Level and $F;

  for I := 0 to 1 do
  begin
    aPart    := @Table.T[I];
    aPart[0] := aPart[0] + 256;
    // Weight of first-encoutered deterministic symbol
    aPart[MaxSymbol + 2] := aPart[MaxSymbol + 2] + 32;
    // Recency scaling, r = r'' / 32, r'' = (r' + 1) * 32
    aPart[MaxSymbol + 3] := Increment * aPart[MaxSymbol + 3] shl 2;
    aPart[MaxSymbol + 4] := aPart[MaxSymbol + 4] div 8;
    // Zero-valued parameter allowed...
    aPart[MaxSymbol + 5] := Round(IntPower(1.082, aPart[MaxSymbol + 5]));
    // Lowest value of interval 
  end;
end;

procedure TBaseCoder.SetDictionary(aDictionaryLevel: cardinal);
begin
  if (aDictionaryLevel = 0) or (FDictionaryLevel <> aDictionaryLevel) then
  begin
    FDictionaryLevel := aDictionaryLevel;
    MaxCounter := 1 shl (17 + Min(Max(0, FDictionaryLevel), 9)) - 1;
    SafeCounter := MaxCounter - 64;
    Cuts := nil;
    Heap := nil;
    SetLength(Heap, MaxCounter + 1);
  end;
  FreshFlexible;
end;

procedure TBaseCoder.FreshFlexible;
begin
  Tear := nil;
  CurrentFreeNode := @Heap[0];
  LastFreeNode := @Heap[MaxCounter];
  Counter := 0;
  ListCount := 0;
  Pos := 0;

  Inc(Counter);
  Root := CurrentFreeNode;
  Inc(CurrentFreeNode);

  Root.Next := nil;
  Root.Up   := nil;
  Root.K    := Increment;
  Root.C    := 0;
  Root.A    := 1;

  LowestPos := -integer(MaxCounter);
end;

procedure TBaseCoder.FreshSolid;
begin
  if Counter > 1 then
  begin
    ListCount := 1;
    List[0]   := Root;
  end else
    ListCount := 0;
end;

procedure TBaseCoder.Add(aSymbol: cardinal);
begin
  Inc(Pos);
  Inc(LowestPos);
  Heap[Pos and MaxCounter].D := aSymbol;
end;

procedure TBaseCoder.CreateChild(Parent: PNode);
var
  Result, Link: PNode;
  Address:      integer;
begin
  Inc(Counter);
  Result := CurrentFreeNode;
  if Result = LastFreeNode then
  begin
    Result := Tear;
    Link   := Result.Tear;
    if Result.Next <> nil then
    begin
      Result.Next.Tear := Link;
      Link := Result.Next;
    end;
    if Result.Up <> nil then
    begin
      Result.Up.Tear := Link;
      Link := Result.Up;
    end;
    Tear := Link;
  end else
    Inc(CurrentFreeNode);

  Result.Next := Parent.Up;
  Parent.Up   := Result;
  Result.Up   := nil;
  Result.K    := Increment;
  Address     := Parent.A;
  Result.C    := Heap[Address and MaxCounter].D;
  Result.A    := Address + 1;
end;

procedure TBaseCoder.Cut;
var
  P:     PNode;
  I, J:  PPNode;
  Bound: integer;
begin
  if Cuts = nil then
    SetLength(Cuts, MaxCounter + 1);

  I := @Cuts[0];
  J := I;
  Inc(J);
  I^    := Root;
  Bound := SafeCounter * 3 div 4;

  repeat
    P := I^.Up;
    repeat
      Dec(Bound);
      if P.Up <> nil then
        if P.A > LowestPos then
        begin
          J^ := P;
          Inc(J);
        end else
        begin
          P.Up.Tear := Tear;
          Tear      := P.Up;
          P.Up      := nil;
        end;
      P := P.Next;
    until P = nil;
    Inc(I);
  until (I = J) or (Bound < 0);

  if I <> J then
    Cut_Tail(I, J);

  Counter   := integer(SafeCounter * 3 div 4) - Bound + 1;
  ListCount := 0;
end;

procedure TBaseCoder.Cut_Tail(I, J: PPNode);
var
  P: PNode;
begin
  P := Tear;
  repeat
    I^.Up.Tear := P;
    P     := I^.Up;
    I^.Up := nil;
    Inc(I);
  until I = J;
  Tear := P;
end;

procedure TBaseCoder.Account;
var
  J, K:      cardinal;
  P, Stored: PNode;
begin
  I := 0;
  J := I;
  Q := J;
  IncreaseIndex := Q;
  repeat
    P := List[I];
    if P.Up <> nil then
    begin
      P := P.Up;
      if IncreaseIndex = 0 then
        IncreaseIndex := I;
      if P.Next <> nil then
      begin
        // Undetermined context ...
        K      := P.K * Part[MaxSymbol + 2] shr 5;
        Stored := P;
        P      := P.Next;
        J      := 1;
        repeat
          Inc(J);
          Inc(K, P.K);
          P := P.Next;
        until P = nil;
        Inc(Q, Part[J]);
        // Account:
        K := R div (K + Q);
        P := Stored;
        J := K * P.K * Part[MaxSymbol + 2] shr 5;
        Dec(R, J);
        Inc(Freq[P.C], J);
        P := P.Next;
        repeat
          J := K * P.K;
          Dec(R, J);
          Inc(Freq[P.C], J);
          P := P.Next;
        until P = nil;
      end else
      begin
        // Determined context ...
        K := P.K * Part[1] div Increment + 256;
        K := (R div K) shl 8;
        Inc(Freq[P.C], R - K);
        R := K;
      end;
    end else
    if P.A > LowestPos then
    begin
      // Determined context, encountered at first time ...
      CreateChild(P);
      K := R div Part[0] shl 8;
      Inc(Freq[P.Up.C], R - K);
      R := K;
    end;
    Inc(I);
  until (I = ListCount) or (R <= Part[MaxSymbol + 5]);
  ListCount := I;
end;

function TBaseCoder.Tail(Node: PNode): PNode;
var
  P: PNode;
  C: byte;
begin
  Node.A := Pos;
  Result := Node.Up;

  if Result = nil then
    CreateChild(Node)
  else
  begin
    C := Symbol;
    if Result.C <> C then
      repeat
        P      := Result;
        Result := Result.Next;
        if Result = nil then
        begin
          CreateChild(Node);
          Break;
        end else
        if Result.C = C then
        begin
          P.Next      := Result.Next;
          Result.Next := Node.Up;
          Node.Up     := Result;
          Break;
        end;
      until False;
  end;
end;

procedure TBaseCoder.Step;
var
  I, J: cardinal;
  P:    PNode;
begin
  ClearCardinal(Freq[0], MaxSymbol + 1);
  R := MaxFreq - MaxSymbol - 1;

  if ListCount > 0 then
    Account;

  // Update aSymbol...
  AddCardinal(Freq[0], MaxSymbol + 1, R shr BitChain + 1);

  Symbol := FCodec.UpdateSymbol(Freq, Symbol);
  Add(Symbol);

  if ListCount > 0 then
  begin
    // Update frequencies...
    I := 0;
    repeat
      P := List[I];
      if I = IncreaseIndex then
        Inc(P.K, Increment)             // Special case...
      else
        Inc(P.K, Part[MaxSymbol + 4]);  // General case...

      if P.K > Part[MaxSymbol + 3] then
        repeat
          P.K := P.K shr 1;
          P   := P.Next;
        until P = nil;
      Inc(I);
    until I > IncreaseIndex;

    // Update Tree:
    I := 0;
    J := I;
    repeat
      P := Tail(List[I]);
      if P <> nil then
      begin
        List[J] := P;
        Inc(J);
      end;
      Inc(I);
    until I = ListCount;
    ListCount := J;
  end;
end;

function TBaseCoder.UpdateModel(aSymbol: cardinal): cardinal;
begin
  Part   := @Table.T[0];
  Symbol := aSymbol shr $4;
  Step;
  Result := Symbol shl 4;

  Part   := @Table.T[1];
  Symbol := aSymbol and $F;
  Step;
  Inc(Result, Symbol);

  // Reduce tree...
  if SafeCounter < Counter then
    Cut;

  // Update NodeList...
  if ListCount > Table.Level then
    MoveCardinalUnchecked(List[1], List[0], ListCount - 1)
  else
    Inc(ListCount);

  List[ListCount - 1] := Root;
end;

end.
