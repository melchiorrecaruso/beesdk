program BeeOpt;

{$mode objfpc}{$H+}{$R-}{$Q-}

uses
  Classes, SysUtils, Math, Bee_Model, Bee_RangeCoder;

const
  CrossoverProbability = 0.15;
  MutationProbability = 0.01;
  StateMagic = $32474F42; { BOG2 }
  StateVersion = 3;
  PreviousStateVersion = 2;
  AnnealStateMagic = $32414F42; { BOA2 }
  AnnealStateVersion = 3;
  PreviousAnnealStateVersion = 2;

type
  TOptimizer = class;

  TPerson = class
  public
    Genome: TBeeTableParameters;
    Cost: Int64;
    constructor CreateRandom(TableSize: Integer);
    constructor CreateTable(const Table: TBeeTableParameters);
    constructor CreateChild(A, B: TPerson; Bits: Cardinal);
    constructor Load(Stream: TStream; TableSize: Integer);
    procedure Save(Stream: TStream);
    function SameGenome(Person: TPerson): Boolean;
  end;

  TPopulation = class(TList)
  public
    constructor Load(Stream: TStream; TableSize: Integer);
    destructor Destroy; override;
    procedure AddSorted(Person: TPerson);
    procedure MarkToRecalculate;
    function ContainsGenome(Person: TPerson): Boolean;
    procedure Save(Stream: TStream);
  end;

  TWorld = class(TList)
  private
    FOwner: TOptimizer;
  public
    CurrentPopulation, Age, Improvements: Integer;
    constructor Load(Owner: TOptimizer; const FileName: string);
    destructor Destroy; override;
    procedure Live;
    procedure Save(const FileName: string);
  end;

  TCountingStream = class(TStream)
  private
    FPosition, FSize: Int64;
  public
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Reset;
  end;

  TOptions = record
    Bits: Cardinal;
    Method, InputName, StateName, OutputName: string;
    Turns: Int64;
  end;

  TAnnealingState = class
  private
    FOwner: TOptimizer;
    FCurrent, FBest: TBeeTableParameters;
    FCurrentCost, FBestCost, FIteration, FImprovements, FStagnation: Int64;
    FDictionary: Cardinal;
    FRngState: QWord;
    FTemperature, FInitialTemperature: Double;
    function NextRandom: QWord;
    function RandomInteger(Limit: Cardinal): Cardinal;
    function RandomUnit: Double;
    procedure Mutate(var Table: TBeeTableParameters);
  public
    constructor Load(Owner: TOptimizer; const FileName: string);
    procedure Step;
    procedure Save(const FileName: string);
    property Best: TBeeTableParameters read FBest;
    property BestCost: Int64 read FBestCost;
    property Iteration: Int64 read FIteration;
    property Improvements: Int64 read FImprovements;
    property Dictionary: Cardinal read FDictionary;
  end;

  TOptimizer = class
  private
    FOptions: TOptions;
    FBodies: array of TBytes;
    FModel: TBeeModel;
    FCounter: TCountingStream;
    FCoder: TRangeCoder;
    FWorld: TWorld;
    function CreateModel: TBeeModel;
    procedure AddFile(const FileName: string);
    procedure LoadInput;
    function SelectBest: TPerson;
    procedure ExportBest;
    procedure Report(TotalBytes: Int64);
    procedure RunGenetic(TotalBytes: Int64);
    procedure RunAnnealing(TotalBytes: Int64);
  public
    constructor Create(const Options: TOptions);
    destructor Destroy; override;
    function Evaluate(Person: TPerson; Dictionary: Cardinal): Int64;
    function EvaluateTable(const Table: TBeeTableParameters;
      Dictionary: Cardinal): Int64;
    function LevelCount: Cardinal;
    procedure Run;
    property Bits: Cardinal read FOptions.Bits;
  end;

procedure Usage;
begin
  WriteLn('BeeOpt - original Bee genetic optimizer for FPC');
  WriteLn('usage: beeopt --bits=1|2|4|8 --input=<file-or-directory> [options]');
  WriteLn('  --method=genetic|annealing (default: genetic)');
  WriteLn('  --state=<file>      resumable optimizer state');
  WriteLn('  --output=<file>     best current table');
  WriteLn('  --turns=<n>         stop after n evaluations (default: continuous)');
  WriteLn('Dictionary rule: d=(age div 2000)+1, capped at d9.');
end;

function OptionValue(const Argument, Name: string; out Value: string): Boolean;
var Prefix: string;
begin
  Prefix := '--' + Name + '=';
  Result := Copy(Argument, 1, Length(Prefix)) = Prefix;
  if Result then Value := Copy(Argument, Length(Prefix) + 1, MaxInt);
end;

procedure ParseOptions(out Options: TOptions);
var I: Integer; Argument, Value, Stem: string;
begin
  FillChar(Options, SizeOf(Options), 0);
  Options.Method := 'genetic';
  for I := 1 to ParamCount do
  begin
    Argument := ParamStr(I);
    if OptionValue(Argument, 'bits', Value) then Options.Bits := StrToInt(Value)
    else if OptionValue(Argument, 'method', Value) then Options.Method := LowerCase(Value)
    else if OptionValue(Argument, 'input', Value) then Options.InputName := Value
    else if OptionValue(Argument, 'state', Value) then Options.StateName := Value
    else if OptionValue(Argument, 'output', Value) then Options.OutputName := Value
    else if OptionValue(Argument, 'turns', Value) then Options.Turns := StrToInt64(Value)
    else if (Argument = '-h') or (Argument = '--help') then begin Usage; Halt(0); end
    else if (Argument <> '') and (Argument[1] <> '-') and
      (Options.InputName = '') then Options.InputName := Argument
    else raise EArgumentException.CreateFmt('Unknown option: %s', [Argument]);
  end;
  if not (Options.Bits in [1, 2, 4, 8]) then
    raise EArgumentException.Create('--bits must be 1, 2, 4 or 8');
  if (Options.Method <> 'genetic') and (Options.Method <> 'annealing') then
    raise EArgumentException.Create('--method must be genetic or annealing');
  if Options.InputName = '' then raise EArgumentException.Create('--input is required');
  if Options.Turns < 0 then raise EArgumentException.Create('--turns cannot be negative');
  Stem := ExcludeTrailingPathDelimiter(Options.InputName) + '.bits' + IntToStr(Options.Bits);
  if Options.Method = 'annealing' then Stem := Stem + '.annealing';
  if Options.StateName = '' then Options.StateName := Stem + '.dat';
  if Options.OutputName = '' then Options.OutputName := Stem + '.tbl';
end;

function Interleave(const Genome: TBeeTableParameters; Bits: Cardinal;
  Reverse: Boolean): TBeeTableParameters;
var Column, Parameter, Columns, ColumnSize, Linear, Grouped: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Genome));
  Result[0] := Genome[0];
  Columns := BeeTableColumnCount(Bits);
  ColumnSize := (Cardinal(1) shl Bits) + 5;
  for Parameter := 0 to ColumnSize - 1 do
    for Column := 0 to Columns - 1 do
    begin
      Linear := 1 + Parameter * Columns + Column;
      Grouped := 1 + Column * ColumnSize + Parameter;
      if Reverse then Result[Grouped] := Genome[Linear]
      else Result[Linear] := Genome[Grouped];
    end;
end;

constructor TPerson.CreateRandom(TableSize: Integer);
var I: Integer;
begin
  inherited Create;
  SetLength(Genome, TableSize);
  for I := 0 to High(Genome) do Genome[I] := Random(256);
  Cost := 0;
end;

constructor TPerson.CreateTable(const Table: TBeeTableParameters);
begin
  inherited Create;
  Genome := Copy(Table, 0, Length(Table));
  Cost := 0;
end;

constructor TPerson.CreateChild(A, B: TPerson; Bits: Cardinal);
var Parents: array[0..1] of TBeeTableParameters;
  Child: TBeeTableParameters; Parent, I, J: Integer;
begin
  inherited Create;
  Parents[0] := Interleave(A.Genome, Bits, False);
  Parents[1] := Interleave(B.Genome, Bits, False);
  SetLength(Child, Length(A.Genome));
  Child[0] := A.Genome[0];
  Parent := Random(2);
  for I := 1 to High(Child) do
  begin
    if Random < CrossoverProbability then Parent := Parent xor 1;
    Child[I] := Parents[Parent][I];
    for J := 0 to 7 do
      if Random < MutationProbability then Child[I] := Child[I] xor (1 shl J);
  end;
  Genome := Interleave(Child, Bits, True);
  Cost := 0;
end;

constructor TPerson.Load(Stream: TStream; TableSize: Integer);
begin
  inherited Create;
  SetLength(Genome, TableSize);
  Stream.ReadBuffer(Genome[0], Length(Genome));
  Stream.ReadBuffer(Cost, SizeOf(Cost));
end;

procedure TPerson.Save(Stream: TStream);
begin
  Stream.WriteBuffer(Genome[0], Length(Genome));
  Stream.WriteBuffer(Cost, SizeOf(Cost));
end;

function TPerson.SameGenome(Person: TPerson): Boolean;
var I: Integer;
begin
  Result := False;
  if Length(Genome) <> Length(Person.Genome) then Exit;
  for I := 1 to High(Genome) do if Genome[I] <> Person.Genome[I] then Exit;
  Result := True;
end;

constructor TPopulation.Load(Stream: TStream; TableSize: Integer);
var N: Integer;
begin
  inherited Create;
  Stream.ReadBuffer(N, SizeOf(N));
  if (N < 0) or (N > 100000) then raise EStreamError.Create('Invalid population');
  while Count < N do Add(TPerson.Load(Stream, TableSize));
end;

destructor TPopulation.Destroy;
begin
  while Count > 0 do TPerson(Extract(First)).Free;
  inherited Destroy;
end;

procedure TPopulation.AddSorted(Person: TPerson);
var I: Integer;
begin
  I := 0;
  while (I < Count) and (TPerson(Items[I]).Cost < Person.Cost) do Inc(I);
  Insert(I, Person);
end;

procedure TPopulation.MarkToRecalculate;
begin
  while Count > 1 do TPerson(Extract(Last)).Free;
  if Count > 0 then TPerson(First).Cost := 0;
end;

function TPopulation.ContainsGenome(Person: TPerson): Boolean;
var I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do if Person.SameGenome(TPerson(Items[I])) then Exit;
  Result := False;
end;

procedure TPopulation.Save(Stream: TStream);
var I, N: Integer;
begin
  N := Count;
  Stream.WriteBuffer(N, SizeOf(N));
  for I := 0 to Count - 1 do TPerson(Items[I]).Save(Stream);
end;

constructor TWorld.Load(Owner: TOptimizer; const FileName: string);
var Stream: TFileStream; Magic, Version, StoredBits, TableSize, Populations, I: Cardinal;
  Recovery, Source: string;
  Recalculate: Boolean;
begin
  inherited Create;
  FOwner := Owner;
  Recovery := FileName + '.Err';
  if FileExists(Recovery) then Source := Recovery else Source := FileName;
  if not FileExists(Source) then
  begin
    while Count < Owner.LevelCount do Add(TPopulation.Create);
    Exit;
  end;
  Stream := TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    Stream.ReadBuffer(Magic, SizeOf(Magic));
    Stream.ReadBuffer(Version, SizeOf(Version));
    Stream.ReadBuffer(StoredBits, SizeOf(StoredBits));
    Stream.ReadBuffer(TableSize, SizeOf(TableSize));
    Stream.ReadBuffer(Populations, SizeOf(Populations));
    if (Magic <> StateMagic) or
      ((Version <> StateVersion) and (Version <> PreviousStateVersion)) then
      raise EStreamError.Create('Unsupported BeeOpt state format');
    if StoredBits <> Owner.Bits then
      raise EStreamError.CreateFmt('State is for a %d-bit model', [StoredBits]);
    if (TableSize <> BeeTableByteSize(Owner.Bits)) or
      (Populations <> Owner.LevelCount) then
      raise EStreamError.Create('State geometry does not match the model');
    Stream.ReadBuffer(CurrentPopulation, SizeOf(CurrentPopulation));
    Stream.ReadBuffer(Age, SizeOf(Age));
    Stream.ReadBuffer(Improvements, SizeOf(Improvements));
    for I := 0 to Populations - 1 do Add(TPopulation.Load(Stream, TableSize));
    Recalculate := Version = PreviousStateVersion;
    if Recalculate then
    begin
      { Version 3 changed the safe heap cutoff for 1/2-bit models. Preserve
        every evolved genome but recalculate its cost under the corrected
        model before evolution resumes. Zero costs remain resumable if this
        migration is interrupted. }
      for I := 0 to Count - 1 do TPopulation(Items[I]).MarkToRecalculate;
      CurrentPopulation := 0;
    end;
  finally
    Stream.Free;
  end;
end;

destructor TWorld.Destroy;
begin
  while Count > 0 do TPopulation(Extract(First)).Free;
  inherited Destroy;
end;

procedure TWorld.Live;
var P1, P2: TPopulation; A, B, Child: TPerson; Dictionary, I: Integer;
begin
  if Age mod 2000 = 0 then
  begin
    for I := 0 to Count - 1 do TPopulation(Items[I]).MarkToRecalculate;
    Inc(Age);
  end;
  P1 := TPopulation(Items[CurrentPopulation]);
  if P1.Count = 0 then Child := TPerson.CreateRandom(BeeTableByteSize(FOwner.Bits))
  else if TPerson(P1.First).Cost = 0 then Child := TPerson(P1.Extract(P1.First))
  else
  begin
    if Count = 1 then begin A := TPerson(P1.First); B := A; end
    else repeat
      repeat
        P2 := TPopulation(Items[Max(0, Min(CurrentPopulation + Random(3) - 1, Count - 1))]);
      until P2.Count > 0;
      A := TPerson(P1.Items[Random(P1.Count)]);
      B := TPerson(P2.Items[Random(P2.Count)]);
    until A <> B;
    repeat
      Child := TPerson.CreateChild(A, B, FOwner.Bits);
      if P1.ContainsGenome(Child) then FreeAndNil(Child);
    until Child <> nil;
  end;
  Child.Genome[0] := CurrentPopulation + 1;
  Dictionary := Min(Age div 2000 + 1, 9);
  Child.Cost := FOwner.Evaluate(Child, Dictionary);
  if P1.Count > 0 then
  begin
    if TPerson(P1.First).Cost > 0 then Inc(Age);
    if TPerson(P1.First).Cost > Child.Cost then Inc(Improvements);
  end;
  P1.AddSorted(Child);
  while (P1.Count > 1) and (TPerson(P1.First).Cost > 0) do
    TPerson(P1.Extract(P1.Last)).Free;
  CurrentPopulation := (CurrentPopulation + 1) mod Count;
end;

procedure TWorld.Save(const FileName: string);
var Stream: TFileStream; Magic, Version, StoredBits, TableSize, Populations, I: Cardinal;
  Recovery: string;
begin
  Recovery := FileName + '.Err';
  if FileExists(Recovery) then DeleteFile(Recovery);
  if FileExists(FileName) and not RenameFile(FileName, Recovery) then
    raise EStreamError.CreateFmt('Cannot back up state: %s', [FileName]);
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Magic := StateMagic; Version := StateVersion; StoredBits := FOwner.Bits;
      TableSize := BeeTableByteSize(StoredBits); Populations := FOwner.LevelCount;
      Stream.WriteBuffer(Magic, SizeOf(Magic));
      Stream.WriteBuffer(Version, SizeOf(Version));
      Stream.WriteBuffer(StoredBits, SizeOf(StoredBits));
      Stream.WriteBuffer(TableSize, SizeOf(TableSize));
      Stream.WriteBuffer(Populations, SizeOf(Populations));
      Stream.WriteBuffer(CurrentPopulation, SizeOf(CurrentPopulation));
      Stream.WriteBuffer(Age, SizeOf(Age));
      Stream.WriteBuffer(Improvements, SizeOf(Improvements));
      for I := 0 to Count - 1 do TPopulation(Items[I]).Save(Stream);
    finally Stream.Free; end;
    if FileExists(Recovery) then DeleteFile(Recovery);
  except
    if FileExists(FileName) then DeleteFile(FileName);
    if FileExists(Recovery) then RenameFile(Recovery, FileName);
    raise;
  end;
end;

constructor TAnnealingState.Load(Owner: TOptimizer; const FileName: string);
var
  Stream: TFileStream;
  Magic, Version, StoredBits, TableSize: Cardinal;
  Recovery, Source: string;
begin
  inherited Create;
  FOwner := Owner;
  Recovery := FileName + '.Err';
  if FileExists(Recovery) then Source := Recovery else Source := FileName;
  if not FileExists(Source) then
  begin
    FCurrent := BeeCreateDefaultTable(Owner.Bits);
    FBest := Copy(FCurrent, 0, Length(FCurrent));
    FDictionary := 1;
    FRngState := QWord(GetTickCount64) xor QWord(PtrUInt(Self)) xor
      QWord(DateTimeToTimeStamp(Now).Time) xor $9E3779B97F4A7C15;
    if FRngState = 0 then FRngState := $2545F4914F6CDD1D;
    FCurrentCost := Owner.EvaluateTable(FCurrent, FDictionary);
    FBestCost := FCurrentCost;
    FInitialTemperature := Max(1.0, FCurrentCost / 10000.0);
    FTemperature := FInitialTemperature;
    Exit;
  end;
  Stream := TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    Stream.ReadBuffer(Magic, SizeOf(Magic));
    Stream.ReadBuffer(Version, SizeOf(Version));
    Stream.ReadBuffer(StoredBits, SizeOf(StoredBits));
    Stream.ReadBuffer(TableSize, SizeOf(TableSize));
    if (Magic <> AnnealStateMagic) or
      ((Version <> AnnealStateVersion) and
       (Version <> PreviousAnnealStateVersion)) then
      raise EStreamError.Create('Unsupported annealing state format');
    if (StoredBits <> Owner.Bits) or (TableSize <> BeeTableByteSize(Owner.Bits)) then
      raise EStreamError.Create('Annealing state geometry does not match the model');
    Stream.ReadBuffer(FIteration, SizeOf(FIteration));
    Stream.ReadBuffer(FImprovements, SizeOf(FImprovements));
    Stream.ReadBuffer(FStagnation, SizeOf(FStagnation));
    Stream.ReadBuffer(FDictionary, SizeOf(FDictionary));
    Stream.ReadBuffer(FRngState, SizeOf(FRngState));
    Stream.ReadBuffer(FTemperature, SizeOf(FTemperature));
    Stream.ReadBuffer(FInitialTemperature, SizeOf(FInitialTemperature));
    Stream.ReadBuffer(FCurrentCost, SizeOf(FCurrentCost));
    Stream.ReadBuffer(FBestCost, SizeOf(FBestCost));
    SetLength(FCurrent, TableSize);
    SetLength(FBest, TableSize);
    Stream.ReadBuffer(FCurrent[0], TableSize);
    Stream.ReadBuffer(FBest[0], TableSize);
    if Version = PreviousAnnealStateVersion then
    begin
      { Re-score both preserved tables after the safe heap cutoff change. }
      FCurrentCost := Owner.EvaluateTable(FCurrent, FDictionary);
      FBestCost := Owner.EvaluateTable(FBest, FDictionary);
      if FCurrentCost < FBestCost then
      begin
        FBest := Copy(FCurrent, 0, Length(FCurrent));
        FBestCost := FCurrentCost;
      end;
      FInitialTemperature := Max(1.0, FCurrentCost / 10000.0);
      FTemperature := FInitialTemperature;
      FStagnation := 0;
    end;
  finally
    Stream.Free;
  end;
end;

function TAnnealingState.NextRandom: QWord;
begin
  FRngState := FRngState xor (FRngState shr 12);
  FRngState := FRngState xor (FRngState shl 25);
  FRngState := FRngState xor (FRngState shr 27);
  Result := FRngState * QWord($2545F4914F6CDD1D);
end;

function TAnnealingState.RandomInteger(Limit: Cardinal): Cardinal;
begin
  if Limit = 0 then Exit(0);
  Result := NextRandom mod Limit;
end;

function TAnnealingState.RandomUnit: Double;
begin
  Result := (NextRandom shr 11) * (1.0 / 9007199254740992.0);
end;

procedure TAnnealingState.Mutate(var Table: TBeeTableParameters);
var
  Index, Roll, Delta, Value: Integer;
begin
  { Mutate exactly one parameter.  Fine steps dominate; the rare large steps
    retain a limited ability to escape a local basin without making ordinary
    refinement depend on the annealing temperature. }
  Index := RandomInteger(Length(Table));
  if (Index = 0) and (FOwner.LevelCount = 1) then
    Index := 1 + RandomInteger(Length(Table) - 1);

  if Index = 0 then
  begin
    Value := Table[0];
    if Value <= 1 then Inc(Value)
    else if Value >= Integer(FOwner.LevelCount) then Dec(Value)
    else if RandomInteger(2) = 0 then Dec(Value)
    else Inc(Value);
    Table[0] := Value;
  end
  else
  begin
    Roll := RandomInteger(200);
    if Roll < 120 then Delta := 1       { 60.0% }
    else if Roll < 170 then Delta := 2  { 25.0% }
    else if Roll < 190 then Delta := 4  { 10.0% }
    else if Roll < 196 then Delta := 8  {  3.0% }
    else if Roll < 199 then Delta := 16 {  1.5% }
    else Delta := 32;                   {  0.5% }

    Value := Table[Index];
    if Value = 0 then
      Value := Delta
    else if Value = 255 then
      Value := 255 - Delta
    else
    begin
      if RandomInteger(2) = 0 then Delta := -Delta;
      Inc(Value, Delta);
      if Value < 0 then Value := 0 else if Value > 255 then Value := 255;
    end;
    Table[Index] := Value;
  end;
end;

procedure TAnnealingState.Step;
var
  Candidate: TBeeTableParameters;
  CandidateCost, Delta: Int64;
  NewDictionary: Cardinal;
  Accept: Boolean;
begin
  NewDictionary := Min(Cardinal(FIteration div 2000 + 1), 9);
  if NewDictionary <> FDictionary then
  begin
    FDictionary := NewDictionary;
    FCurrentCost := FOwner.EvaluateTable(FCurrent, FDictionary);
    FBestCost := FOwner.EvaluateTable(FBest, FDictionary);
    if FCurrentCost < FBestCost then
    begin
      FBest := Copy(FCurrent, 0, Length(FCurrent));
      FBestCost := FCurrentCost;
    end;
    FInitialTemperature := Max(1.0, FCurrentCost / 10000.0);
    FTemperature := FInitialTemperature;
    FStagnation := 0;
  end;
  Candidate := Copy(FCurrent, 0, Length(FCurrent));
  Mutate(Candidate);
  CandidateCost := FOwner.EvaluateTable(Candidate, FDictionary);
  Delta := CandidateCost - FCurrentCost;
  Accept := Delta <= 0;
  if (not Accept) and (FTemperature > 0) then
    Accept := RandomUnit < Exp(-Delta / FTemperature);
  if Accept then
  begin
    FCurrent := Candidate;
    FCurrentCost := CandidateCost;
  end;
  if CandidateCost < FBestCost then
  begin
    FBest := Candidate;
    FBestCost := CandidateCost;
    Inc(FImprovements);
    FStagnation := 0;
  end
  else
    Inc(FStagnation);
  FTemperature := Max(0.01, FTemperature * 0.9995);
  if FStagnation >= 500 then
  begin
    FTemperature := Max(FTemperature, FInitialTemperature * 0.25);
    FStagnation := 0;
  end;
  Inc(FIteration);
end;

procedure TAnnealingState.Save(const FileName: string);
var
  Stream: TFileStream;
  Magic, Version, StoredBits, TableSize: Cardinal;
  Recovery: string;
begin
  Recovery := FileName + '.Err';
  if FileExists(Recovery) then DeleteFile(Recovery);
  if FileExists(FileName) and not RenameFile(FileName, Recovery) then
    raise EStreamError.CreateFmt('Cannot back up state: %s', [FileName]);
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Magic := AnnealStateMagic;
      Version := AnnealStateVersion;
      StoredBits := FOwner.Bits;
      TableSize := Length(FCurrent);
      Stream.WriteBuffer(Magic, SizeOf(Magic));
      Stream.WriteBuffer(Version, SizeOf(Version));
      Stream.WriteBuffer(StoredBits, SizeOf(StoredBits));
      Stream.WriteBuffer(TableSize, SizeOf(TableSize));
      Stream.WriteBuffer(FIteration, SizeOf(FIteration));
      Stream.WriteBuffer(FImprovements, SizeOf(FImprovements));
      Stream.WriteBuffer(FStagnation, SizeOf(FStagnation));
      Stream.WriteBuffer(FDictionary, SizeOf(FDictionary));
      Stream.WriteBuffer(FRngState, SizeOf(FRngState));
      Stream.WriteBuffer(FTemperature, SizeOf(FTemperature));
      Stream.WriteBuffer(FInitialTemperature, SizeOf(FInitialTemperature));
      Stream.WriteBuffer(FCurrentCost, SizeOf(FCurrentCost));
      Stream.WriteBuffer(FBestCost, SizeOf(FBestCost));
      Stream.WriteBuffer(FCurrent[0], TableSize);
      Stream.WriteBuffer(FBest[0], TableSize);
    finally
      Stream.Free;
    end;
    if FileExists(Recovery) then DeleteFile(Recovery);
  except
    if FileExists(FileName) then DeleteFile(FileName);
    if FileExists(Recovery) then RenameFile(Recovery, FileName);
    raise;
  end;
end;

function TCountingStream.Read(var Buffer; Count: LongInt): LongInt;
begin Result := 0; end;
function TCountingStream.Write(const Buffer; Count: LongInt): LongInt;
begin Inc(FPosition, Count); if FPosition > FSize then FSize := FPosition; Result := Count; end;
function TCountingStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of soBeginning: FPosition := Offset; soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize + Offset; end;
  Result := FPosition;
end;
procedure TCountingStream.Reset;
begin FPosition := 0; FSize := 0; end;

function TOptimizer.LevelCount: Cardinal;
begin Result := BeeMaxTableLevel; end;

function TOptimizer.CreateModel: TBeeModel;
begin
  case FOptions.Bits of
    1: Result := TBeeOneBitModel.Create; 2: Result := TBeeTwoBitModel.Create;
    4: Result := TBeeNibbleModel.Create; 8: Result := TBeeByteModel.Create;
  else Result := nil; end;
end;

procedure TOptimizer.AddFile(const FileName: string);
var Stream: TFileStream; N: Integer;
begin
  N := Length(FBodies); SetLength(FBodies, N + 1);
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size > High(Integer) then raise ERangeError.Create('Input file too large');
    SetLength(FBodies[N], Stream.Size);
    if Stream.Size > 0 then Stream.ReadBuffer(FBodies[N][0], Stream.Size);
  finally Stream.Free; end;
end;

procedure TOptimizer.LoadInput;
var Search: TSearchRec; Names: TStringList; Base: string; I, ErrorCode: Integer;
begin
  if FileExists(FOptions.InputName) then begin AddFile(FOptions.InputName); Exit; end;
  if not DirectoryExists(FOptions.InputName) then
    raise EFileNotFoundException.CreateFmt('Input not found: %s', [FOptions.InputName]);
  Names := TStringList.Create;
  try
    Base := IncludeTrailingPathDelimiter(FOptions.InputName);
    ErrorCode := FindFirst(Base + '*', faAnyFile, Search);
    while ErrorCode = 0 do begin
      if (Search.Name <> '.') and (Search.Name <> '..') and
        ((Search.Attr and faDirectory) = 0) then Names.Add(Base + Search.Name);
      ErrorCode := FindNext(Search);
    end;
    FindClose(Search); Names.Sort;
    for I := 0 to Names.Count - 1 do AddFile(Names[I]);
  finally Names.Free; end;
  if Length(FBodies) = 0 then raise EFileNotFoundException.Create('No sample files');
end;

constructor TOptimizer.Create(const Options: TOptions);
begin
  inherited Create; FOptions := Options; Randomize; LoadInput;
  FCounter := TCountingStream.Create; FCoder := TRangeCoder.Create(FCounter);
  FModel := CreateModel;
  if FOptions.Method = 'genetic' then FWorld := TWorld.Load(Self, FOptions.StateName);
end;

destructor TOptimizer.Destroy;
begin FWorld.Free; FModel.Free; FCoder.Free; FCounter.Free; inherited Destroy; end;

function TOptimizer.Evaluate(Person: TPerson; Dictionary: Cardinal): Int64;
var Body, I: Integer;
begin
  FModel.SetTable(Person.Genome); FModel.SetDictionaryLevel(Dictionary);
  FCounter.Reset; FCoder.StartEncode;
  for Body := 0 to High(FBodies) do begin
    FModel.FreshFlexible;
    for I := 0 to High(FBodies[Body]) do FModel.EncodeByte(FBodies[Body][I], FCoder);
  end;
  FCoder.FinishEncode; Result := FCounter.Size;
end;

function TOptimizer.EvaluateTable(const Table: TBeeTableParameters;
  Dictionary: Cardinal): Int64;
var
  Person: TPerson;
begin
  Person := TPerson.CreateTable(Table);
  try
    Result := Evaluate(Person, Dictionary);
  finally
    Person.Free;
  end;
end;

function TOptimizer.SelectBest: TPerson;
var Population: TPopulation; Candidate: TPerson; I: Integer;
begin
  Result := nil;
  for I := 0 to FWorld.Count - 1 do begin
    Population := TPopulation(FWorld.Items[I]); if Population.Count = 0 then Continue;
    Candidate := TPerson(Population.First);
    if (Candidate.Cost <= 0) or (Candidate.Genome[0] < 3) then Continue;
    if (Result = nil) or (Candidate.Cost <= Result.Cost) then Result := Candidate;
  end;
  if Result <> nil then Exit;
  for I := 0 to FWorld.Count - 1 do begin
    Population := TPopulation(FWorld.Items[I]); if Population.Count = 0 then Continue;
    Candidate := TPerson(Population.First); if Candidate.Cost <= 0 then Continue;
    if (Result = nil) or (Candidate.Cost < Result.Cost) then Result := Candidate;
  end;
end;

procedure TOptimizer.ExportBest;
var Best: TPerson;
begin Best := SelectBest; if Best <> nil then BeeSaveTable(FOptions.OutputName, Best.Genome); end;

procedure TOptimizer.Report(TotalBytes: Int64);
var Best: TPerson; Dictionary: Integer; Ratio: Double;
begin
  Best := SelectBest;
  if Best = nil then Exit;
  Dictionary := Min(FWorld.Age div 2000 + 1, 9);
  if TotalBytes > 0 then Ratio := Best.Cost / TotalBytes * 100 else Ratio := 0;
  WriteLn('age', #9, FWorld.Age, #9,
    'population=', FWorld.CurrentPopulation + 1, #9,
    'dictionary=', Dictionary, #9,
    'improvements=', FWorld.Improvements, #9,
    'compressed=', Best.Cost, ' bytes', #9,
    'percentage=', FormatFloat('0.000', Ratio), '%');
end;

procedure TOptimizer.RunGenetic(TotalBytes: Int64);
var Completed: Int64;
begin
  Completed := 0;
  repeat
    FWorld.Live; Inc(Completed); FWorld.Save(FOptions.StateName); ExportBest;
    if (FWorld.CurrentPopulation = 0) or
      ((FOptions.Turns > 0) and (Completed = FOptions.Turns)) then
      Report(TotalBytes);
  until (FOptions.Turns > 0) and (Completed >= FOptions.Turns);
end;

procedure TOptimizer.RunAnnealing(TotalBytes: Int64);
var
  State: TAnnealingState;
  Completed, PreviousImprovements: Int64;
  Ratio: Double;
begin
  State := TAnnealingState.Load(Self, FOptions.StateName);
  try
    Completed := 0;
    repeat
      PreviousImprovements := State.Improvements;
      State.Step;
      Inc(Completed);
      State.Save(FOptions.StateName);
      BeeSaveTable(FOptions.OutputName, State.Best);
      if (State.Improvements > PreviousImprovements) or
        ((FOptions.Turns > 0) and (Completed = FOptions.Turns)) then
      begin
        if TotalBytes > 0 then Ratio := State.BestCost / TotalBytes * 100
        else Ratio := 0;
        WriteLn('age', #9, State.Iteration, #9,
          'dictionary=', State.Dictionary, #9,
          'improvements=', State.Improvements, #9,
          'compressed=', State.BestCost, ' bytes', #9,
          'percentage=', FormatFloat('0.000', Ratio), '%');
      end;
    until (FOptions.Turns > 0) and (Completed >= FOptions.Turns);
  finally
    State.Free;
  end;
end;

procedure TOptimizer.Run;
var TotalBytes: Int64; I: Integer;
begin
  TotalBytes := 0; for I := 0 to High(FBodies) do Inc(TotalBytes, Length(FBodies[I]));
  if FOptions.Method = 'annealing' then RunAnnealing(TotalBytes)
  else RunGenetic(TotalBytes);
end;

var Options: TOptions; Optimizer: TOptimizer;
begin
  try
    ParseOptions(Options); Optimizer := TOptimizer.Create(Options);
    try Optimizer.Run; finally Optimizer.Free; end;
  except
    on E: Exception do begin WriteLn(StdErr, 'error: ', E.Message); Usage; Halt(1); end;
  end;
end.
