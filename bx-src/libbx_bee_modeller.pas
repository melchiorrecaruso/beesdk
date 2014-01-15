{
  Copyright (c) 2003-2011 Andrew Filinsky;
  Copyright (c) 2012-2014 Melchiorre Caruso.

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

    BaseCoder class, PPM modeller;

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
    v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;
  
    v1.0.0 build 2210 - 2014.01.15 by Melchiorre Caruso.
}

unit libbx_bee_modeller;

{$I bx_compiler.inc}

interface

uses
  Math,
  libbx_bee_common,
  libbx_bee_rangecoder;

const
  BitChain  = 4;                    { Size of data portion, bit                }
  MaxSymbol = 1 shl BitChain - 1;   { Size of source alphabet, symbols         }
  Increment = 8;                    { Increment of symbol frequency            }

type
  PNode  = ^TNode;                  { Pointer to modeller's node information   }
  PPNode = ^PNode;                  { Array of nodes...                        }

  { Modeller's node information }

  TNode = packed record
    Next, Up: PNode;                { Next node of this or high level          }
    K: word;                        { Frequency of this symbol                 }
    C: byte;                        { This symbol itself                       }
    D: byte;                        { Used for incoming data storage           }
    case longword of
      1: (A: longint);              { Source address                           }
      2: (Tear: PNode);             { Next free node                           }
  end;

  { PPM modeller }

  PBeeModeller = ^TBeeModeller;

  TBeeModeller = packed record
    Codec: pointer;
    DictLevel: longword;
    Symbol: longword;
    Pos: longword;
    LowestPos: longint;
    MaxCounter: longint;            { Maximal heap size                        }
    SafeCounter: longint;           { Safe heap size                           }
    Counter: longword;              { Current heap size                        }

    Heap: array of TNode;
    Cuts: array of PNode;
    List: array of PNode;
    ListCount: longword;

    Root: PNode;
    CurrentFreeNode: PNode;
    LastFreeNode: PNode;
    Tear: PNode;

    IncreaseIndex: longword;
    I: longword;
    R: longword;
    Q: longword;

    Freq:  TFreq;                   { Symbol frequencyes                       }
    Part:  ^TTableCol;              { Part of parameters Table                 }
    Table: TTable;                  { Parameters Table                         }
  end;

  function  BeeModeller_Create (aCodec: pointer): PBeeModeller;
  procedure BeeModeller_Destroy(Self: PBeeModeller);

  procedure BeeModeller_SetTableParameters(Self: PBeeModeller; Table: PByte);
  procedure BeeModeller_SetDictionaryLevel(Self: PBeeModeller; aDictLevel: longword);
  procedure BeeModeller_FreshFlexible     (Self: PBeeModeller);
  procedure BeeModeller_FreshSolid        (Self: PBeeModeller);
  function  BeeModeller_Update            (Self: PBeeModeller; aSymbol: longword; Update: PBeeRangeCodUpdate): longword;

  function  BeeModeller_Encode(Self: PBeeModeller; Buffer: PByte; BufSize: longword): longword;
  function  BeeModeller_Decode(Self: PBeeModeller; Buffer: PByte; BufSize: longword): longword;

implementation

{ TBaseCoder }

function BeeModeller_Create(aCodec: pointer): PBeeModeller;
begin
  Result :=GetMem(sizeof(TBeeModeller));

  Result^.Codec     := aCodec;
  Result^.DictLevel := 0;
  SetLength(Result^.Freq, MAXSYMBOL + 1);

  Result^.Heap := nil;
  Result^.Cuts := nil;
  SetLength(Result^.List, MAXSYMBOL + 1);
end;

procedure BeeModeller_Destroy(Self: PBeeModeller);
begin
  Self^.Freq := nil;
  Self^.Heap := nil;
  Self^.Cuts := nil;
  Self^.List := nil;
  FreeMem(Self);
end;

procedure BeeModeller_Add(Self: PBeeModeller; aSymbol: longword);
begin
  Inc(Self^.Pos);
  Inc(Self^.LowestPos);
  Self^.Heap[Self^.Pos and Self^.MaxCounter].D := aSymbol;
end;

procedure BeeModeller_CreateChild(Self: PBeeModeller; Parent: PNode);
var
  Result, Link: PNode;
begin
  Inc(Self^.Counter);
  Result := Self^.CurrentFreeNode;
  if Result = Self^.LastFreeNode then
  begin
    Result := Self^.Tear;
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
    Self^.Tear := Link;
  end else
    Inc(Self^.CurrentFreeNode);

  Result.Next := Parent.Up;
  Parent.Up   := Result;
  Result.Up   := nil;
  Result.A    := Parent.A + 1;
  Result.C    := Self^.Heap[Parent.A and Self^.MaxCounter].D;
  Result.K    := Increment;
end;

procedure BeeModeller_Cut_Tail(Self: PBeeModeller; I, J: PPNode);
var
  P: PNode;
begin
  P := Self^.Tear;
  repeat
    I^.Up.Tear := P;
    P     := I^.Up;
    I^.Up := nil;
    Inc(I);
  until I = J;
  Self^.Tear := P;
end;

procedure BeeModeller_Cut(Self: PBeeModeller);
var
  P:     PNode;
  I, J:  PPNode;
  Bound: longint;
begin
  if Self^.Cuts = nil then
    SetLength(Self^.Cuts, Self^.MaxCounter + 1);

  I := @Self^.Cuts[0];
  J := I;
  Inc(J);

  I^    := Self^.Root;
  Bound := Self^.SafeCounter * 3 div 4;
  repeat
    P := I^.Up;
    repeat
      Dec(Bound);
      if P.Up <> nil then
        if P.A > Self^.LowestPos then
        begin
          J^ := P;
          Inc(J);
        end else
        begin
          P.Up.Tear  := Self^.Tear;
          Self^.Tear := P.Up;
          P.Up       := nil;
        end;
      P := P.Next;
    until P = nil;
    Inc(I);
  until (I = J) or (Bound < 0);

  if I <> J then
    BeeModeller_Cut_Tail(Self, I, J);

  Self^.Counter   := longint(Self^.SafeCounter * 3 div 4) - Bound + 1;
  Self^.ListCount := 0;
end;

function BeeModeller_Tail(Self: PBeeModeller; Node: PNode): PNode;
var
  P: PNode;
  C: byte;
begin
  Node.A := Self^.Pos;
  Result := Node.Up;

  if Result = nil then
    BeeModeller_CreateChild(Self, Node)
  else
  begin
    C := Self^.Symbol;
    if Result.C <> C then
      repeat
        P      := Result;
        Result := Result.Next;
        if Result = nil then
        begin
          BeeModeller_CreateChild(Self, Node);
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

procedure BeeModeller_Account(Self: PBeeModeller);
var
  J, K: longword;
  P, Stored: PNode;
begin
  Self^.I := 0;
  J       := 0;
  Self^.Q := 0;
  Self^.IncreaseIndex := 0;
  repeat
    P := Self^.List[Self^.I];
    if P.Up <> nil then
    begin
      P := P.Up;
      if Self^.IncreaseIndex = 0 then
        Self^.IncreaseIndex := Self^.I;

      if P.Next <> nil then
      begin
        // Undetermined context ...
        K      := P.K * Self^.Part[MaxSymbol + 2] shr 5;
        Stored := P;
        P      := P.Next;
        J      := 1;
        repeat
          Inc(J);
          Inc(K, P.K);
          P := P.Next;
        until P = nil;
        Inc(Self^.Q, Self^.Part[J]);
        // Account:
        P := Stored;
        K := Self^.R div (K + Self^.Q);
        J := K * P.K * Self^.Part[MaxSymbol + 2] shr 5;

        Dec(Self^.R, J);
        Inc(Self^.Freq[P.C], J);

        P := P.Next;
        repeat
          J := K * P.K;

          Dec(Self^.R, J);
          Inc(Self^.Freq[P.C], J);

          P := P.Next;
        until P = nil;
      end else
      begin
        // Determined context ...
        K := P.K * Self^.Part[1] div Increment + 256;
        K := (Self^.R div K) shl 8;
        Inc(Self^.Freq[P.C], Self^.R - K);
        Self^.R := K;
      end;
    end else
    if P.A > Self^.LowestPos then
    begin
      // Determined context, encountered at first time ...
      BeeModeller_CreateChild(Self, P);
      K := Self^.R div Self^.Part[0] shl 8;
      Inc(Self^.Freq[P.Up.C], Self^.R - K);
      Self^.R := K;
    end;
    Inc(Self^.I);
  until (Self^.I = Self^.ListCount) or (Self^.R <= Self^.Part[MaxSymbol + 5]);
  Self^.ListCount := Self^.I;
end;

procedure BeeModeller_Step(Self: PBeeModeller; UpdateSymbol: PBeeRangeCodUpdate);
var
  I, J: longword;
  P:    PNode;
begin
  ClearLongword(Self^.Freq[0], MaxSymbol + 1);
  Self^.R := MaxFreq - MaxSymbol - 1;

  if Self^.ListCount > 0 then
    BeeModeller_Account(Self);

  // Update aSymbol...
  AddLongword(Self^.Freq[0], MaxSymbol + 1, Self^.R shr BitChain + 1);

  Self^.Symbol := UpdateSymbol(Self^.Codec, Self^.Freq, Self^.Symbol);

  BeeModeller_Add(Self, Self^.Symbol);

  if Self^.ListCount > 0 then
  begin
    // Update frequencies...
    I := 0;
    repeat
      P := Self^.List[I];
      if I = Self^.IncreaseIndex then
        Inc(P.K, Increment)             // Special case...
      else
        Inc(P.K, Self^.Part[MaxSymbol + 4]);  // General case...
      if P.K > Self^.Part[MaxSymbol + 3] then
        repeat
          P.K := P.K shr 1;
          P   := P.Next;
        until P = nil;
      Inc(I);
    until I > Self^.IncreaseIndex;

    // Update Tree...
    I := 0;
    J := I;
    repeat
      P := BeeModeller_Tail(Self, Self^.List[I]);
      if P <> nil then
      begin
        Self^.List[J] := P;
        Inc(J);
      end;
      Inc(I);
    until I = Self^.ListCount;
    Self^.ListCount := J;
  end;
end;

procedure BeeModeller_FreshFlexible(Self: PBeeModeller);
begin
  Self^.Tear            := nil;
  Self^.CurrentFreeNode := @Self^.Heap[0];
  Self^.LastFreeNode    := @Self^.Heap[Self^.MaxCounter];
  Self^.Counter         := 0;
  Self^.ListCount       := 0;
  Self^.Pos             := 0;

  Inc(Self^.Counter);
  Self^.Root := Self^.CurrentFreeNode;
  Inc(Self^.CurrentFreeNode);

  Self^.Root.Next := nil;
  Self^.Root.Up   := nil;
  Self^.Root.K    := Increment;
  Self^.Root.C    := 0;
  Self^.Root.A    := 1;

  Self^.LowestPos := -longint(Self^.MaxCounter);
end;

procedure BeeModeller_FreshSolid(Self: PBeeModeller);
begin
  if Self^.Counter > 1 then
  begin
    Self^.ListCount := 1;
    Self^.List[0]   := Self^.Root;
  end else
    Self^.ListCount := 0;
end;

procedure BeeModeller_SetDictionaryLevel(Self: PBeeModeller; aDictLevel: longword);
begin
  Self^.DictLevel  := aDictLevel;
  Self^.MaxCounter  := (1 shl (17 + Self^.DictLevel)) - 1;
  Self^.SafeCounter := Self^.MaxCounter - 64;

  Self^.Cuts := nil;
  Self^.Heap := nil;
  SetLength(Self^.Heap, Self^.MaxCounter + 1);

  BeeModeller_FreshFlexible(Self);
end;

procedure BeeModeller_SetTableParameters(Self: PBeeModeller; Table: PByte);
var
  I, J, K:  longint;
  aPart: ^TTableCol;
begin
  Self^.Table.Level := longword(Table[0]) and $F;

  I := 1;
  for J := 0 to TABLECOLS do
    for K := 0 to TABLESIZE do
    begin
      Self^.Table.T[J][K] := longint(Table[I] + 1);
      Inc(I);
    end;

  for I := 0 to 1 do
  begin
    aPart := @Self^.Table.T[I];

    aPart[0]             :=                       aPart[0] + 256;             // Weight of first-encoutered deterministic symbol
    aPart[MaxSymbol + 2] :=                       aPart[MaxSymbol + 2]  + 32; // Recency scaling, r = r'' / 32, r'' = (r' + 1) * 32
    aPart[MaxSymbol + 3] := Increment *           aPart[MaxSymbol + 3] shl 2; // Zero-valued parameter allowed...
    aPart[MaxSymbol + 4] :=                       aPart[MaxSymbol + 4] div 8;
    aPart[MaxSymbol + 5] := Round(IntPower(1.082, aPart[MaxSymbol + 5]));     // Lowest value of interval
  end;
end;

function BeeModeller_Update(Self: PBeeModeller; aSymbol: longword; Update: PBeeRangeCodUpdate): longword;
begin
  Self^.Part   := @Self^.Table.T[0];
  Self^.Symbol := aSymbol shr $4;
  BeeModeller_Step(Self, Update);

  Result := Self^.Symbol shl 4;
  Self^.Part := @Self^.Table.T[1];
  Self^.Symbol := aSymbol and $F;
  BeeModeller_Step(Self, Update);

  Inc(Result, Self^.Symbol);

  // Reduce tree...
  if Self^.SafeCounter < Self^.Counter then
    BeeModeller_Cut(Self);

  // Update NodeList...
  if Self^.ListCount > Self^.Table.Level then
    MoveLongwordUnchecked(Self^.List[1], Self^.List[0], Self^.ListCount - 1)
  else
    Inc(Self^.ListCount);

  Self^.List[Self^.ListCount - 1] := Self^.Root;
end;

function BeeModeller_Encode(Self: PBeeModeller; Buffer: PByte; BufSize: longword): longword;
var
  I: longint;
begin
  for I := 0 to BufSize - 1 do
  begin
    BeeModeller_Update(Self, Buffer[I], @BeeRangeEnc_Update);
  end;
  Result := BufSize;
end;

function BeeModeller_Decode(Self: PBeeModeller; Buffer: PByte; BufSize: longword): longword;
var
  I: longint;
begin
  for I := 0 to BufSize - 1 do
  begin
    Buffer[I] := BeeModeller_Update(Self, 0, @BeeRangeDec_Update);
  end;
  Result := BufSize;
end;

end.

