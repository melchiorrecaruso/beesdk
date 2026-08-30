unit Bee_Model;

{$mode objfpc}{$H+}
{$R-}{$Q-}

interface

uses
  Classes, SysUtils, Math, IniFiles, Bee_Assembler, Bee_RangeCoder;

const
  BeeIncrement = 8;
  BeeMaxExportedOrders = 17;
  BeeMaxTableLevel = 15;
  BeeTableTextFormat = 'BEE-TABLE';
  BeeTableTextVersion = 1;

type
  TBeeTableParameters = array of Byte;
  TBeeFrequency = array of Cardinal;

const
  BeeLegacyDefaultTable: array[0..42] of Byte =
    (3, 163, 157, 65, 93, 117, 135, 109, 126, 252, 172, 252, 152,
     227, 249, 249, 253, 196, 27, 82, 93, 74, 182, 245, 40, 67, 77,
     143, 133, 135, 128, 155, 207, 177, 225, 251, 253, 248, 73, 35,
     15, 107, 143);

type
  PBeeNode = ^TBeeNode;
  PPBeeNode = ^PBeeNode;

  TBeeNode = packed record
    Next, Up: PBeeNode;
    K: Word;
    C, D: Byte;
    case Cardinal of
      1: (A: LongInt);
      2: (Tear: PBeeNode);
  end;

  { Runtime core shared by the four generated expert types. }
  TBeeModel = class
  private
    FBits, FMaxSymbol: Cardinal;
    FTableColumnCount, FTableColumnSize: Cardinal;
    FDictionaryLevel: Cardinal;
    FSymbol, FPos: Cardinal;
    FLowestPos: LongInt;
    FMaxCounter, FSafeCounter: LongInt;
    FCounter: Cardinal;
    FHeap: array of TBeeNode;
    FCuts, FList: array of PBeeNode;
    FListCount: Cardinal;
    FRoot, FCurrentFreeNode, FLastFreeNode, FTear: PBeeNode;
    FIncreaseIndex, FI, FR, FQ: Cardinal;
    FFrequencies: TBeeFrequency;
    FOrderFrequencies: array of Cardinal;
    FOrderCount: Cardinal;
    FPart: ^Cardinal;
    FTableLevel: Cardinal;
    FTableValues: array of Cardinal;
    procedure Add(Symbol: Cardinal);
    procedure CreateChild(Parent: PBeeNode);
    procedure Cut;
    procedure CutTail(I, J: PPBeeNode);
    function Tail(Node: PBeeNode): PBeeNode;
    procedure Account;
    procedure CaptureOrder;
    procedure PrepareCurrentSymbol;
    procedure LearnCurrentSymbol(Symbol: Cardinal);
  public
    constructor Create(ABits: Cardinal);
    procedure SetDictionaryLevel(Level: Cardinal);
    procedure SetTable(const Parameters: TBeeTableParameters);
    procedure FreshFlexible;
    procedure FreshSolid;
    procedure PrepareSymbol(TablePartIndex: Cardinal);
    procedure LearnSymbol(Symbol: Cardinal);
    procedure FinishByte;
    procedure EncodeByte(Value: Byte; RangeCoder: TRangeCoder);
    function DecodeByte(RangeCoder: TRangeCoder): Byte;
    function GetOrderBitFrequencies(OrderIndex, Prefix, PrefixBits: Cardinal;
      out ZeroFrequency, OneFrequency: Cardinal): Boolean;
    property Bits: Cardinal read FBits;
    property MaxSymbol: Cardinal read FMaxSymbol;
    property Frequencies: TBeeFrequency read FFrequencies;
    property OrderCount: Cardinal read FOrderCount;
  end;

  TBeeOneBitAlphabet = class
    class function Bits: Cardinal; static;
  end;
  TBeeTwoBitAlphabet = class
    class function Bits: Cardinal; static;
  end;
  TBeeNibbleAlphabet = class
    class function Bits: Cardinal; static;
  end;
  TBeeByteAlphabet = class
    class function Bits: Cardinal; static;
  end;

  generic TBeeTypedModel<TAlphabet> = class(TBeeModel)
    constructor Create;
  end;

  TBeeOneBitModel = specialize TBeeTypedModel<TBeeOneBitAlphabet>;
  TBeeTwoBitModel = specialize TBeeTypedModel<TBeeTwoBitAlphabet>;
  TBeeNibbleModel = specialize TBeeTypedModel<TBeeNibbleAlphabet>;
  TBeeByteModel = specialize TBeeTypedModel<TBeeByteAlphabet>;

function BeeTableByteSize(Bits: Cardinal): Cardinal;
function BeeTableColumnCount(Bits: Cardinal): Cardinal;
function BeeCreateDefaultTable(Bits: Cardinal): TBeeTableParameters;
procedure BeeLoadTable(const FileName: string; Bits: Cardinal;
  out Table: TBeeTableParameters);
procedure BeeSaveTable(const FileName: string; Bits: Cardinal;
  const Table: TBeeTableParameters); overload;
procedure BeeSaveTable(const FileName: string;
  const Table: TBeeTableParameters); overload;

implementation

function BeeTableColumnCount(Bits: Cardinal): Cardinal;
begin
  if not (Bits in [1, 2, 4, 8]) then
    raise ERangeError.Create('Bee table bits must be 1, 2, 4 or 8');
  Result := 8 div Bits;
end;

function BeeTableByteSize(Bits: Cardinal): Cardinal;
begin
  Result := 1 + BeeTableColumnCount(Bits) *
    ((Cardinal(1) shl Bits) + 5);
end;

function BeeCreateDefaultTable(Bits: Cardinal): TBeeTableParameters;
var
  Column, Index, SourceBase, DestBase: Integer;
  MaxSymbol, ColumnSize: Cardinal;
begin
  MaxSymbol := (Cardinal(1) shl Bits) - 1;
  ColumnSize := MaxSymbol + 6;
  Result := nil;
  SetLength(Result, BeeTableByteSize(Bits));
  Result[0] := BeeLegacyDefaultTable[0] and BeeMaxTableLevel;
  for Column := 0 to BeeTableColumnCount(Bits) - 1 do
  begin
    { The historical table distinguishes the first nibble from the second.
      Use its second column as the neutral starting point for every later
      symbol position in the smaller alphabets. }
    SourceBase := 1 + Min(Column, 1) * 21;
    DestBase := 1 + Column * ColumnSize;
    for Index := 0 to MaxSymbol + 1 do
      Result[DestBase + Index] :=
        BeeLegacyDefaultTable[SourceBase + Min(Index, 16)];
    for Index := 0 to 3 do
      Result[DestBase + MaxSymbol + 2 + Index] :=
        BeeLegacyDefaultTable[SourceBase + 17 + Index];
  end;
end;

function BeeTableValuesToText(const Table: TBeeTableParameters): string;
var I: Integer;
begin
  Result := '';
  for I := 0 to High(Table) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + IntToStr(Table[I]);
  end;
end;

procedure BeeTextToTable(const Value: string; Bits: Cardinal;
  out Table: TBeeTableParameters);
var Values: TStringList; I, Parameter, ExpectedSize: Integer;
begin
  ExpectedSize := BeeTableByteSize(Bits);
  Values := TStringList.Create;
  try
    Values.StrictDelimiter := True;
    Values.Delimiter := ',';
    Values.DelimitedText := Value;
    if Values.Count <> ExpectedSize then
      raise EStreamError.CreateFmt('Table contains %d values; expected %d',
        [Values.Count, ExpectedSize]);
    SetLength(Table, ExpectedSize);
    for I := 0 to ExpectedSize - 1 do
    begin
      Parameter := StrToInt(Trim(Values[I]));
      if (Parameter < 0) or (Parameter > 255) then
        raise EStreamError.CreateFmt('Table value %d is outside 0..255', [I]);
      Table[I] := Parameter;
    end;
  finally
    Values.Free;
  end;
end;

procedure BeeLoadTable(const FileName: string; Bits: Cardinal;
  out Table: TBeeTableParameters);
var
  Ini: TMemIniFile;
  StoredBits, StoredSize, Version: Integer;
  Format, Values: string;
begin
  if SameText(FileName, 'default') then
  begin
    Table := BeeCreateDefaultTable(Bits);
    Exit;
  end;
  Ini := TMemIniFile.Create(FileName);
  try
    Format := Ini.ReadString('BeeTable', 'Format', '');
    Version := Ini.ReadInteger('BeeTable', 'Version', 0);
    StoredBits := Ini.ReadInteger('BeeTable', 'Bits', 0);
    StoredSize := Ini.ReadInteger('BeeTable', 'Size', 0);
    Values := Ini.ReadString('BeeTable', 'Values', '');
    if (Format <> BeeTableTextFormat) or (Version <> BeeTableTextVersion) then
      raise EStreamError.Create('Unsupported Bee table text format');
    if (StoredBits <> Integer(Bits)) or
      (StoredSize <> Integer(BeeTableByteSize(Bits))) then
      raise EStreamError.Create('Bee table geometry does not match the model');
    BeeTextToTable(Values, Bits, Table);
  finally
    Ini.Free;
  end;
end;

procedure BeeSaveTable(const FileName: string; Bits: Cardinal;
  const Table: TBeeTableParameters);
var Lines: TStringList;
begin
  if Length(Table) <> Integer(BeeTableByteSize(Bits)) then
    raise ERangeError.CreateFmt('A %d-bit Bee table must contain %d values',
      [Bits, BeeTableByteSize(Bits)]);
  Lines := TStringList.Create;
  try
    Lines.Add('[BeeTable]');
    Lines.Add('Format=' + BeeTableTextFormat);
    Lines.Add('Version=' + IntToStr(BeeTableTextVersion));
    Lines.Add('Bits=' + IntToStr(Bits));
    Lines.Add('Size=' + IntToStr(Length(Table)));
    Lines.Add('Values=' + BeeTableValuesToText(Table));
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

procedure BeeSaveTable(const FileName: string;
  const Table: TBeeTableParameters);
var Bits: Cardinal;
begin
  for Bits in [Cardinal(1), Cardinal(2), Cardinal(4), Cardinal(8)] do
    if Length(Table) = Integer(BeeTableByteSize(Bits)) then
    begin
      BeeSaveTable(FileName, Bits, Table);
      Exit;
    end;
  raise ERangeError.CreateFmt('Cannot infer Bee table bit width from %d values',
    [Length(Table)]);
end;

constructor TBeeModel.Create(ABits: Cardinal);
begin
  inherited Create;
  if not (ABits in [1, 2, 4, 8]) then
    raise ERangeError.Create('Bee model bits must be 1, 2, 4 or 8');
  FBits := ABits;
  FMaxSymbol := (Cardinal(1) shl FBits) - 1;
  FTableColumnCount := BeeTableColumnCount(FBits);
  FTableColumnSize := FMaxSymbol + 6;
  SetLength(FFrequencies, FMaxSymbol + 1);
  SetLength(FOrderFrequencies, BeeMaxExportedOrders * (FMaxSymbol + 1));
  { The context list depth is controlled by the table level and is independent
    of the source alphabet size. }
  SetLength(FList, BeeMaxTableLevel + 1);
  SetTable(BeeCreateDefaultTable(FBits));
  SetDictionaryLevel(3);
end;

procedure TBeeModel.SetDictionaryLevel(Level: Cardinal);
var
  SafetyMargin: Cardinal;
begin
  if Level > 9 then
    raise ERangeError.Create('Dictionary level must be between 0 and 9');
  if (Length(FHeap) = 0) or (FDictionaryLevel <> Level) then
  begin
    FDictionaryLevel := Level;
    FMaxCounter := (Cardinal(1) shl (17 + Level)) - 1;
    { Cut is checked once per byte. Account and Learn can each create one
      child per active context at every symbol step. The historical reserve
      of 64 is exact for two 4-bit steps, but 2-bit and 1-bit models require
      reserves of 128 and 256 nodes at level 15. }
    SafetyMargin := Max(64,
      2 * FTableColumnCount * (BeeMaxTableLevel + 1));
    FSafeCounter := FMaxCounter - SafetyMargin;
    FCuts := nil;
    FHeap := nil;
    SetLength(FHeap, FMaxCounter + 1);
  end;
  FreshFlexible;
end;

procedure TBeeModel.SetTable(const Parameters: TBeeTableParameters);
var
  Column, Index, ParameterIndex, PartBase: Integer;
  Part: ^Cardinal;
begin
  if Length(Parameters) <> BeeTableByteSize(FBits) then
    raise ERangeError.CreateFmt('The %d-bit model requires a %d-byte table',
      [FBits, BeeTableByteSize(FBits)]);
  FTableLevel := Cardinal(Parameters[0]) and BeeMaxTableLevel;
  SetLength(FTableValues, FTableColumnCount * FTableColumnSize);
  ParameterIndex := 1;
  for Column := 0 to FTableColumnCount - 1 do
    for Index := 0 to FTableColumnSize - 1 do
    begin
      FTableValues[Column * FTableColumnSize + Index] :=
        Cardinal(Parameters[ParameterIndex]) + 1;
      Inc(ParameterIndex);
    end;
  for Column := 0 to FTableColumnCount - 1 do
  begin
    PartBase := Column * FTableColumnSize;
    Part := @FTableValues[PartBase];
    Part[0] := Part[0] + 256;
    Part[FMaxSymbol + 2] := Part[FMaxSymbol + 2] + 32;
    Part[FMaxSymbol + 3] := BeeIncrement * Part[FMaxSymbol + 3] shl 2;
    Part[FMaxSymbol + 4] := Part[FMaxSymbol + 4] div 8;
    Part[FMaxSymbol + 5] := Round(IntPower(1.082, Part[FMaxSymbol + 5]));
  end;
end;

procedure TBeeModel.FreshFlexible;
begin
  FTear := nil;
  FCurrentFreeNode := @FHeap[0];
  FLastFreeNode := @FHeap[FMaxCounter];
  FCounter := 1;
  FListCount := 0;
  FPos := 0;
  FRoot := FCurrentFreeNode;
  Inc(FCurrentFreeNode);
  FRoot^.Next := nil;
  FRoot^.Up := nil;
  FRoot^.K := BeeIncrement;
  FRoot^.C := 0;
  FRoot^.A := 1;
  FLowestPos := -FMaxCounter;
end;

procedure TBeeModel.FreshSolid;
begin
  if FCounter > 1 then
  begin
    FListCount := 1;
    FList[0] := FRoot;
  end
  else
    FListCount := 0;
end;

procedure TBeeModel.Add(Symbol: Cardinal);
begin
  Inc(FPos);
  Inc(FLowestPos);
  FHeap[FPos and FMaxCounter].D := Symbol;
end;

procedure TBeeModel.CreateChild(Parent: PBeeNode);
var
  NewNode, Link: PBeeNode;
begin
  Inc(FCounter);
  NewNode := FCurrentFreeNode;
  if NewNode = FLastFreeNode then
  begin
    NewNode := FTear;
    Link := NewNode^.Tear;
    if NewNode^.Next <> nil then
    begin
      NewNode^.Next^.Tear := Link;
      Link := NewNode^.Next;
    end;
    if NewNode^.Up <> nil then
    begin
      NewNode^.Up^.Tear := Link;
      Link := NewNode^.Up;
    end;
    FTear := Link;
  end
  else
    Inc(FCurrentFreeNode);
  NewNode^.Next := Parent^.Up;
  Parent^.Up := NewNode;
  NewNode^.Up := nil;
  NewNode^.A := Parent^.A + 1;
  NewNode^.C := FHeap[Parent^.A and FMaxCounter].D;
  NewNode^.K := BeeIncrement;
end;

procedure TBeeModel.CutTail(I, J: PPBeeNode);
var
  P: PBeeNode;
begin
  P := FTear;
  repeat
    I^^.Up^.Tear := P;
    P := I^^.Up;
    I^^.Up := nil;
    Inc(I);
  until I = J;
  FTear := P;
end;

procedure TBeeModel.Cut;
var
  P: PBeeNode;
  I, J: PPBeeNode;
  Bound: LongInt;
begin
  if FCuts = nil then
    SetLength(FCuts, FMaxCounter + 1);
  I := @FCuts[0];
  J := I;
  Inc(J);
  I^ := FRoot;
  Bound := FSafeCounter * 3 div 4;
  repeat
    P := I^^.Up;
    repeat
      Dec(Bound);
      if P^.Up <> nil then
        if P^.A > FLowestPos then
        begin
          J^ := P;
          Inc(J);
        end
        else
        begin
          P^.Up^.Tear := FTear;
          FTear := P^.Up;
          P^.Up := nil;
        end;
      P := P^.Next;
    until P = nil;
    Inc(I);
  until (I = J) or (Bound < 0);
  if I <> J then
    CutTail(I, J);
  FCounter := LongInt(FSafeCounter * 3 div 4) - Bound + 1;
  FListCount := 0;
end;

function TBeeModel.Tail(Node: PBeeNode): PBeeNode;
var
  P: PBeeNode;
  C: Byte;
begin
  Node^.A := FPos;
  Result := Node^.Up;
  if Result = nil then
    CreateChild(Node)
  else
  begin
    C := FSymbol;
    if Result^.C <> C then
      repeat
        P := Result;
        Result := Result^.Next;
        if Result = nil then
        begin
          CreateChild(Node);
          Break;
        end
        else if Result^.C = C then
        begin
          P^.Next := Result^.Next;
          Result^.Next := Node^.Up;
          Node^.Up := Result;
          Break;
        end;
      until False;
  end;
end;

procedure TBeeModel.Account;
var
  J, K: Cardinal;
  P, Stored: PBeeNode;
begin
  FI := 0;
  J := 0;
  FQ := 0;
  FIncreaseIndex := 0;
  repeat
    P := FList[FI];
    if P^.Up <> nil then
    begin
      P := P^.Up;
      if FIncreaseIndex = 0 then
        FIncreaseIndex := FI;
      if P^.Next <> nil then
      begin
        K := P^.K * FPart[FMaxSymbol + 2] shr 5;
        Stored := P;
        P := P^.Next;
        J := 1;
        repeat
          Inc(J);
          Inc(K, P^.K);
          P := P^.Next;
        until P = nil;
        Inc(FQ, FPart[J]);
        P := Stored;
        K := FR div (K + FQ);
        J := K * P^.K * FPart[FMaxSymbol + 2] shr 5;
        Dec(FR, J);
        Inc(FFrequencies[P^.C], J);
        P := P^.Next;
        repeat
          J := K * P^.K;
          Dec(FR, J);
          Inc(FFrequencies[P^.C], J);
          P := P^.Next;
        until P = nil;
      end
      else
      begin
        K := P^.K * FPart[1] div BeeIncrement + 256;
        K := (FR div K) shl 8;
        Inc(FFrequencies[P^.C], FR - K);
        FR := K;
      end;
    end
    else if P^.A > FLowestPos then
    begin
      CreateChild(P);
      K := FR div FPart[0] shl 8;
      Inc(FFrequencies[P^.Up^.C], FR - K);
      FR := K;
    end;
    Inc(FI);
    CaptureOrder;
  until (FI = FListCount) or (FR <= FPart[FMaxSymbol + 5]);
  FListCount := FI;
end;

procedure TBeeModel.CaptureOrder;
var
  SymbolIndex: Cardinal;
  UniformFrequency: Cardinal;
begin
  if FOrderCount >= BeeMaxExportedOrders then Exit;
  UniformFrequency := (FR shr FBits) + 1;
  for SymbolIndex := 0 to FMaxSymbol do
    FOrderFrequencies[FOrderCount * (FMaxSymbol + 1) + SymbolIndex] :=
      FFrequencies[SymbolIndex] + UniformFrequency;
  Inc(FOrderCount);
end;

procedure TBeeModel.PrepareCurrentSymbol;
begin
  ClearCardinal(FFrequencies[0], FMaxSymbol + 1);
  FR := MaxFreq - FMaxSymbol - 1;
  FOrderCount := 0;
  CaptureOrder;
  if FListCount > 0 then
    Account;
  AddCardinal(FFrequencies[0], FMaxSymbol + 1, (FR shr FBits) + 1);
end;

function TBeeModel.GetOrderBitFrequencies(OrderIndex, Prefix,
  PrefixBits: Cardinal; out ZeroFrequency, OneFrequency: Cardinal): Boolean;
var
  Remaining, FirstSymbol, MiddleSymbol, LastSymbol, Symbol, Offset: Cardinal;
begin
  Result := (OrderIndex < FOrderCount) and (PrefixBits < FBits);
  if not Result then Exit;
  Remaining := FBits - PrefixBits;
  FirstSymbol := Prefix shl Remaining;
  MiddleSymbol := FirstSymbol + (Cardinal(1) shl (Remaining - 1));
  LastSymbol := FirstSymbol + (Cardinal(1) shl Remaining) - 1;
  Offset := OrderIndex * (FMaxSymbol + 1);
  ZeroFrequency := 0;
  OneFrequency := 0;
  for Symbol := FirstSymbol to MiddleSymbol - 1 do
    Inc(ZeroFrequency, FOrderFrequencies[Offset + Symbol]);
  for Symbol := MiddleSymbol to LastSymbol do
    Inc(OneFrequency, FOrderFrequencies[Offset + Symbol]);
end;

procedure TBeeModel.LearnCurrentSymbol(Symbol: Cardinal);
var
  I, J: Cardinal;
  P: PBeeNode;
begin
  FSymbol := Symbol;
  Add(FSymbol);
  if FListCount = 0 then
    Exit;
  I := 0;
  repeat
    P := FList[I];
    if I = FIncreaseIndex then
      Inc(P^.K, BeeIncrement)
    else
      Inc(P^.K, FPart[FMaxSymbol + 4]);
    if P^.K > FPart[FMaxSymbol + 3] then
      repeat
        P^.K := P^.K shr 1;
        P := P^.Next;
      until P = nil;
    Inc(I);
  until I > FIncreaseIndex;
  I := 0;
  J := 0;
  repeat
    P := Tail(FList[I]);
    if P <> nil then
    begin
      FList[J] := P;
      Inc(J);
    end;
    Inc(I);
  until I = FListCount;
  FListCount := J;
end;

procedure TBeeModel.PrepareSymbol(TablePartIndex: Cardinal);
begin
  FPart := @FTableValues[Min(TablePartIndex, FTableColumnCount - 1) *
    FTableColumnSize];
  PrepareCurrentSymbol;
end;

procedure TBeeModel.LearnSymbol(Symbol: Cardinal);
begin
  if Symbol > FMaxSymbol then
    raise ERangeError.Create('Symbol outside model alphabet');
  LearnCurrentSymbol(Symbol);
end;

procedure TBeeModel.FinishByte;
begin
  if FSafeCounter < FCounter then
    Cut;
  if FListCount > FTableLevel then
    MovePointerUnchecked(FList[1], FList[0], FListCount - 1)
  else
    Inc(FListCount);
  FList[FListCount - 1] := FRoot;
end;

procedure TBeeModel.EncodeByte(Value: Byte; RangeCoder: TRangeCoder);
var
  Shift, PartIndex, Symbol: Cardinal;
  CumFreq, TotalFreq, I: Cardinal;
begin
  Shift := 8 - FBits;
  PartIndex := 0;
  repeat
    PrepareSymbol(PartIndex);
    Symbol := (Value shr Shift) and FMaxSymbol;
    CumFreq := 0;
    if Symbol > 0 then
      for I := 0 to Symbol - 1 do
        Inc(CumFreq, FFrequencies[I]);
    TotalFreq := CumFreq;
    for I := Symbol to FMaxSymbol do
      Inc(TotalFreq, FFrequencies[I]);
    RangeCoder.Encode(CumFreq, FFrequencies[Symbol], TotalFreq);
    LearnSymbol(Symbol);
    Inc(PartIndex);
    if Shift = 0 then
      Break;
    Dec(Shift, FBits);
  until False;
  FinishByte;
end;

function TBeeModel.DecodeByte(RangeCoder: TRangeCoder): Byte;
var
  Shift, PartIndex, Symbol: Cardinal;
  CumFreq, TotalFreq, Target: Cardinal;
begin
  Result := 0;
  Shift := 8 - FBits;
  PartIndex := 0;
  repeat
    PrepareSymbol(PartIndex);
    TotalFreq := 0;
    for Symbol := 0 to FMaxSymbol do
      Inc(TotalFreq, FFrequencies[Symbol]);
    Target := RangeCoder.GetFreq(TotalFreq);
    CumFreq := 0;
    Symbol := 0;
    while Target >= CumFreq + FFrequencies[Symbol] do
    begin
      Inc(CumFreq, FFrequencies[Symbol]);
      Inc(Symbol);
    end;
    RangeCoder.Decode(CumFreq, FFrequencies[Symbol], TotalFreq);
    Result := Result or Byte(Symbol shl Shift);
    LearnSymbol(Symbol);
    Inc(PartIndex);
    if Shift = 0 then
      Break;
    Dec(Shift, FBits);
  until False;
  FinishByte;
end;

class function TBeeOneBitAlphabet.Bits: Cardinal;
begin
  Result := 1;
end;

class function TBeeTwoBitAlphabet.Bits: Cardinal;
begin
  Result := 2;
end;

class function TBeeNibbleAlphabet.Bits: Cardinal;
begin
  Result := 4;
end;

class function TBeeByteAlphabet.Bits: Cardinal;
begin
  Result := 8;
end;

constructor TBeeTypedModel.Create;
begin
  inherited Create(TAlphabet.Bits);
end;

end.
