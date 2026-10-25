unit BeeOptCore;

{$mode objfpc}{$H+}{$R-}{$Q-}

interface

uses
  SysUtils;

type
  TBeeOptProgress = record
    Corpus: string;
    Age, Population, Dictionary, Improvements, Threads: Integer;
    TotalBytes, CompressedBytes: Int64;
    Percentage: Double;
  end;

  TBeeOptProgressEvent = procedure(const Progress: TBeeOptProgress) of object;
  TBeeOptMessageEvent = procedure(const Text: string) of object;
  TBeeOptStopEvent = function: Boolean of object;
  EBeeOptHelp = class(Exception);

procedure RunBeeOpt(OnProgress: TBeeOptProgressEvent;
  OnMessage: TBeeOptMessageEvent; ShouldStop: TBeeOptStopEvent);
function BeeOptUsage: string;

implementation

uses
  Classes, Math, IniFiles, SyncObjs, BeeConfiguration, BeeModel, BeeMixer,
  BeeRangeCoder, BeeExeFilter;

const
  CrossoverProbability = 0.15;
  MutationProbability = 0.01;
  StateVersion = 1;

var
  GOnProgress: TBeeOptProgressEvent;
  GOnMessage: TBeeOptMessageEvent;
  GShouldStop: TBeeOptStopEvent;

procedure EmitMessage(const Text: string);
begin
  if Assigned(GOnMessage) then GOnMessage(Text);
end;

function StopRequested: Boolean;
begin
  Result := Assigned(GShouldStop) and GShouldStop();
end;

type
  TOptimizer = class;
  TEvaluationWorker = class;

  TPerson = class
  public
    Genome: TBeeTableParameters;
    Cost: Int64;
    constructor CreateRandom(TableSize: Integer);
    constructor CreateChild(A, B: TPerson);
    constructor Load(Ini: TCustomIniFile; const Section, Prefix: string;
      TableSize: Integer);
    procedure Save(Ini: TCustomIniFile; const Section, Prefix: string);
    function SameGenome(Person: TPerson): Boolean;
  end;

  TPersonArray = array of TPerson;
  TIntegerArray = array of Integer;

  TPopulation = class(TList)
  public
    constructor Load(Ini: TCustomIniFile; const Section: string;
      TableSize: Integer);
    destructor Destroy; override;
    procedure AddSorted(Person: TPerson);
    procedure MarkToRecalculate;
    function ContainsGenome(Person: TPerson): Boolean;
    procedure Save(Ini: TCustomIniFile; const Section: string);
  end;

  TWorld = class(TList)
  private
    FOwner: TOptimizer;
    procedure UpdateDictionary;
  public
    CurrentPopulation, Age, Improvements, CostDictionary: Integer;
    constructor Load(Owner: TOptimizer; const FileName: string);
    destructor Destroy; override;
    procedure Live(Amount: Integer);
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

  TEvaluationWorker = class(TThread)
  private
    FOwner: TOptimizer;
    FStartEvent, FDoneEvent: TEvent;
    FPerson: TPerson;
    FDictionary: Cardinal;
    FError: string;
    FStarted: Boolean;
    FMixer: TBeeMixer;
    FModel: TBeeModel;
    FCounter: TCountingStream;
    FCoder: TRangeCoder;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TOptimizer);
    destructor Destroy; override;
    procedure Queue(Person: TPerson; Dictionary: Cardinal);
    procedure WaitForResult;
  end;

  TOptions = record
    Threads: Integer;
    CorpusPath, StateName, SeedName, ExportName: string;
    RandomSeed: Integer;
    RandomSeedSpecified: Boolean;
    Turns: Int64;
    MixerEnabled, MixerSpecified, ExportOnly: Boolean;
    ExeFilterEnabled, ExeFilterSpecified: Boolean;
  end;

  TOptimizer = class
  private
    FOptions: TOptions;
    FBodies: array of TBytes;
    FBodyNames: TStringList;
    FBodySizes: array of Int64;
    FBodyHashes: array of QWord;
    FBodyExecutable: array of Boolean;
    FExeFilteredFiles: Integer;
    FWorkers: array of TEvaluationWorker;
    FWorld: TWorld;
    function CreateModel: TBeeModel;
    function GenomeSize: Integer;
    function CorpusExtension: string;
    procedure AddFile(const FileName: string);
    procedure LoadCorpus;
    procedure SeedPopulations(const FileName: string);
    function SelectBest: TPerson;
    procedure ExportBest;
    procedure Report(TotalBytes: Int64);
    procedure RunGenetic(TotalBytes: Int64);
    procedure EvaluateBatch(const Persons: TPersonArray; Dictionary: Cardinal);
    function EvaluateWithContext(Person: TPerson; Dictionary: Cardinal;
      Mixer: TBeeMixer; Model: TBeeModel; Counter: TCountingStream;
      Coder: TRangeCoder): Int64;
  public
    constructor Create(const Options: TOptions);
    destructor Destroy; override;
    function LevelCount: Cardinal;
    procedure Run;
    property MixerEnabled: Boolean read FOptions.MixerEnabled;
  end;

function BeeOptUsage: string;
begin
  Result := 'BeeOpt - Bee nibble-model genetic optimizer for FPC' + LineEnding +
    'new:    beeopt --corpus=<folder> [options]' + LineEnding +
    'mixer:  beeopt --mixer --corpus=<folder> [options]' + LineEnding +
    'resume: beeopt --state=<file> [--threads=<n>] [--turns=<n>]' + LineEnding +
    'export: beeopt --state=<file> --export=<bee.ini>' + LineEnding +
    '  --state=<file>      state; on resume it supplies the corpus folder' + LineEnding +
    '  --export=<bee.ini>  put the tables in \m1, \m2, \m3 of bee.ini and exit' + LineEnding +
    '  --seed=<file>       seed a new run from bee.ini or a hex table' + LineEnding +
    '  --threads=<n>       parallel evaluators, 1..15 (default: 1)' + LineEnding +
    '  --turns=<n>         stop after n evaluations (default: continuous)' + LineEnding +
    '  --random-seed=<n>   reproducible random sequence for this run' + LineEnding +
    '  --mixer             enable the context mixer (default: disabled)' + LineEnding +
    '  --exe-filter        enable EXE-X86 preprocessing (default: disabled)' + LineEnding +
    'Dictionary rule: d=(age div 2000)+1, capped at d9.';
end;

procedure LoadOptionsFromState(var Options: TOptions);
var Ini: TMemIniFile; Source, Recovery, CorpusName: string;
  Version: Integer;
begin
  if Options.StateName = '' then Exit;
  Options.StateName := ExpandFileName(Options.StateName);
  Recovery := Options.StateName + '.Err';
  if FileExists(Recovery) then Source := Recovery else Source := Options.StateName;
  if not FileExists(Source) then Exit;
  Ini := TMemIniFile.Create(Source);
  try
    Version := Ini.ReadInteger('BeeOptState', 'Version', 0);
    if Version <> StateVersion then
      raise EStreamError.CreateFmt('Unsupported BeeOpt state version: %d', [Version]);
    CorpusName := Ini.ReadString('BeeOptState', 'Corpus', '');
    if (CorpusName = '') or (CorpusName = '.') or (CorpusName = '..') or
      (ExtractFileName(CorpusName) <> CorpusName) then
      raise EStreamError.Create('State contains an invalid corpus folder name');
    if Options.CorpusPath = '' then
      Options.CorpusPath := IncludeTrailingPathDelimiter(
        ExtractFileDir(Options.StateName)) + CorpusName
    else if not SameText(ExtractFileName(ExcludeTrailingPathDelimiter(
      Options.CorpusPath)), CorpusName) then
      raise EArgumentException.Create(
        'State corpus folder conflicts with command line');
    { An explicit --mixer or --exe-filter wins; otherwise the state selects
      the mode. }
    if not Options.MixerSpecified then
      Options.MixerEnabled := Ini.ReadBool('BeeOptState', 'MixerEnabled', False);
    if not Options.ExeFilterSpecified then
      Options.ExeFilterEnabled := Ini.ReadBool('BeeOptState',
        'ExeFilterEnabled', False);
  finally
    Ini.Free;
  end;
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
  Options.Threads := 1;
  Options.MixerEnabled := False;
  Options.ExeFilterEnabled := False;
  for I := 1 to ParamCount do
  begin
    Argument := ParamStr(I);
    if OptionValue(Argument, 'corpus', Value) then Options.CorpusPath := Value
    else if OptionValue(Argument, 'state', Value) then Options.StateName := Value
    else if OptionValue(Argument, 'export', Value) then Options.ExportName := Value
    else if OptionValue(Argument, 'seed', Value) then Options.SeedName := Value
    else if OptionValue(Argument, 'random-seed', Value) then
    begin
      Options.RandomSeed := StrToInt(Value);
      Options.RandomSeedSpecified := True;
    end
    else if OptionValue(Argument, 'threads', Value) then Options.Threads := StrToInt(Value)
    else if OptionValue(Argument, 'turns', Value) then Options.Turns := StrToInt64(Value)
    else if Argument = '--mixer' then
    begin Options.MixerEnabled := True; Options.MixerSpecified := True; end
    else if Argument = '--exe-filter' then
    begin
      Options.ExeFilterEnabled := True;
      Options.ExeFilterSpecified := True;
    end
    else if (Argument = '-h') or (Argument = '--help') then
      raise EBeeOptHelp.Create(BeeOptUsage)
    else if (Argument <> '') and (Argument[1] <> '-') and
      (Options.CorpusPath = '') then Options.CorpusPath := Argument
    else raise EArgumentException.CreateFmt('Unknown option: %s', [Argument]);
  end;
  Options.ExportOnly := Options.ExportName <> '';
  if Options.ExportOnly and (Options.StateName = '') then
    raise EArgumentException.Create('--export requires --state');
  LoadOptionsFromState(Options);
  if Options.CorpusPath = '' then
    raise EArgumentException.Create('--corpus is required');
  if (Options.Threads < 1) or (Options.Threads > BeeMaxTableLevel) then
    raise EArgumentException.CreateFmt('--threads must be between 1 and %d',
      [BeeMaxTableLevel]);
  if Options.Turns < 0 then raise EArgumentException.Create('--turns cannot be negative');
  Options.CorpusPath := ExpandFileName(Options.CorpusPath);
  Stem := ExcludeTrailingPathDelimiter(Options.CorpusPath);
  if Options.MixerEnabled then Stem := Stem + '.mixer';
  if Options.StateName = '' then Options.StateName := Stem + '.state';
  Options.StateName := ExpandFileName(Options.StateName);
  if Options.ExportName <> '' then
    Options.ExportName := ExpandFileName(Options.ExportName);
  if Options.SeedName <> '' then Options.SeedName := ExpandFileName(Options.SeedName);
end;

function Interleave(const Genome: TBeeTableParameters;
  Reverse: Boolean): TBeeTableParameters;
var Column, Parameter, Columns, ColumnSize, Linear, Grouped: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Genome));
  Result[0] := Genome[0];
  Columns := BeeTableColumnCount;
  ColumnSize := BeeTableColumnSize;
  for Parameter := 0 to ColumnSize - 1 do
    for Column := 0 to Columns - 1 do
    begin
      Linear := 1 + Parameter * Columns + Column;
      Grouped := 1 + Column * ColumnSize + Parameter;
      if Reverse then Result[Grouped] := Genome[Linear]
      else Result[Linear] := Genome[Grouped];
    end;
end;

function GenomeTable(const Genome: TBeeTableParameters): TBeeTableParameters;
begin
  Result := Copy(Genome, 0, BeeTableParameterCount);
end;

constructor TPerson.CreateRandom(TableSize: Integer);
var I: Integer;
begin
  inherited Create;
  SetLength(Genome, TableSize);
  for I := 0 to High(Genome) do Genome[I] := Random(256);
  Cost := 0;
end;

constructor TPerson.CreateChild(A, B: TPerson);
var Parents: array[0..1] of TBeeTableParameters;
  Child: TBeeTableParameters; Parent, I, J: Integer;
begin
  inherited Create;
  Parents[0] := Interleave(A.Genome, False);
  Parents[1] := Interleave(B.Genome, False);
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
  Genome := Interleave(Child, True);
  Cost := 0;
end;

function ReadIniValue(Ini: TCustomIniFile; const Section, Key: string): string;
const MissingValue = #1;
begin
  Result := Ini.ReadString(Section, Key, MissingValue);
  if Result = MissingValue then
    raise EStreamError.CreateFmt('Missing [%s] %s in BeeOpt text file',
      [Section, Key]);
end;

{ Genomes are Bee tables: stored as their bee.ini hexadecimal string. }
function GenomeToText(const Genome: TBeeTableParameters): string;
begin
  Result := BeeTableToHex(Genome);
end;

function TextToGenome(const Value: string; TableSize: Integer): TBeeTableParameters;
begin
  if (TableSize <> BeeTableParameterCount) or
    not BeeHexToTable(Value, Result) then
    raise EStreamError.CreateFmt('Genome is not a %d-byte hexadecimal string',
      [TableSize]);
end;

constructor TPerson.Load(Ini: TCustomIniFile; const Section, Prefix: string;
  TableSize: Integer);
begin
  inherited Create;
  Cost := StrToInt64(ReadIniValue(Ini, Section, Prefix + 'Cost'));
  Genome := TextToGenome(ReadIniValue(Ini, Section, Prefix + 'Genome'),
    TableSize);
end;

procedure TPerson.Save(Ini: TCustomIniFile; const Section, Prefix: string);
begin
  Ini.WriteString(Section, Prefix + 'Cost', IntToStr(Cost));
  Ini.WriteString(Section, Prefix + 'Genome', GenomeToText(Genome));
end;

function TPerson.SameGenome(Person: TPerson): Boolean;
var I: Integer;
begin
  Result := False;
  if Length(Genome) <> Length(Person.Genome) then Exit;
  for I := 1 to High(Genome) do if Genome[I] <> Person.Genome[I] then Exit;
  Result := True;
end;

constructor TPopulation.Load(Ini: TCustomIniFile; const Section: string;
  TableSize: Integer);
var I, N: Integer;
begin
  inherited Create;
  N := StrToInt(ReadIniValue(Ini, Section, 'Count'));
  if (N < 0) or (N > 100000) then raise EStreamError.Create('Invalid population');
  for I := 0 to N - 1 do
    Add(TPerson.Load(Ini, Section, 'Person' + IntToStr(I) + '.', TableSize));
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

procedure TPopulation.Save(Ini: TCustomIniFile; const Section: string);
var I: Integer;
begin
  Ini.WriteInteger(Section, 'Count', Count);
  for I := 0 to Count - 1 do
    TPerson(Items[I]).Save(Ini, Section, 'Person' + IntToStr(I) + '.');
end;

{ Dictionary rule: d = (age div 2000) + 1, capped at d9. }
function DictionaryForAge(Age: Integer): Integer;
begin
  Result := Min(Age div 2000 + 1, 9);
end;

constructor TWorld.Load(Owner: TOptimizer; const FileName: string);
var Ini: TMemIniFile; Version, I, StoredCorpusCount: Integer;
  StoredCorpusSize: Int64;
  StoredCorpusHash: QWord;
  Recovery, Source: string;
  ModeChanged: Boolean;
begin
  inherited Create;
  FOwner := Owner;
  Recovery := FileName + '.Err';
  if FileExists(Recovery) then Source := Recovery else Source := FileName;
  if not FileExists(Source) then
  begin
    CostDictionary := 1;
    while Count < Owner.LevelCount do Add(TPopulation.Create);
    Exit;
  end;
  Ini := TMemIniFile.Create(Source);
  try
    Version := StrToInt(ReadIniValue(Ini, 'BeeOptState', 'Version'));
    if Version <> StateVersion then
      raise EStreamError.CreateFmt('Unsupported BeeOpt state version: %d', [Version]);
    StoredCorpusCount := StrToInt(ReadIniValue(Ini, 'BeeOptState',
      'CorpusFileCount'));
    if StoredCorpusCount <> Owner.FBodyNames.Count then
      raise EStreamError.CreateFmt(
        'Corpus changed: state has %d files, folder has %d',
        [StoredCorpusCount, Owner.FBodyNames.Count]);
    for I := 0 to StoredCorpusCount - 1 do
    begin
      if ReadIniValue(Ini, 'BeeOptState',
        'CorpusFile' + IntToStr(I + 1) + '.Name') <> Owner.FBodyNames[I] then
        raise EStreamError.CreateFmt('Corpus file %d name changed', [I + 1]);
      StoredCorpusSize := StrToInt64(ReadIniValue(Ini, 'BeeOptState',
        'CorpusFile' + IntToStr(I + 1) + '.Size'));
      StoredCorpusHash := StrToQWord(ReadIniValue(Ini, 'BeeOptState',
        'CorpusFile' + IntToStr(I + 1) + '.Hash'));
      if (StoredCorpusSize <> Owner.FBodySizes[I]) or
        (StoredCorpusHash <> Owner.FBodyHashes[I]) then
        raise EStreamError.CreateFmt('Corpus file changed: %s',
          [Owner.FBodyNames[I]]);
    end;
    { Costs are reused only when they come from the same evaluator: the
      same mixer mode and the same exe filter mode. }
    ModeChanged :=
      (Ini.ReadBool('BeeOptState', 'MixerEnabled', False) <>
        Owner.MixerEnabled) or
      (Ini.ReadBool('BeeOptState', 'ExeFilterEnabled', False) <>
        Owner.FOptions.ExeFilterEnabled);
    CurrentPopulation := StrToInt(ReadIniValue(Ini, 'BeeOptState',
      'CurrentPopulation'));
    Age := StrToInt(ReadIniValue(Ini, 'BeeOptState', 'Age'));
    Improvements := StrToInt(ReadIniValue(Ini, 'BeeOptState', 'Improvements'));
    if (CurrentPopulation < 0) or
      (CurrentPopulation >= Integer(Owner.LevelCount)) or
      (Age < 0) or (Improvements < 0) then
      raise EStreamError.Create('Invalid counters in BeeOpt state');
    { the costs of a saved state always belong to the dictionary of its age }
    CostDictionary := DictionaryForAge(Age);
    for I := 0 to Owner.LevelCount - 1 do
      Add(TPopulation.Load(Ini, 'Population' + IntToStr(I),
        Owner.GenomeSize));
    if ModeChanged then
    begin
      if Owner.FOptions.ExportOnly then
        raise EStreamError.Create(
          'Evaluation mode changed: resume optimization before exporting');
      for I := 0 to Count - 1 do
        TPopulation(Items[I]).MarkToRecalculate;
      CurrentPopulation := 0;
      Improvements := 0;
      EmitMessage(Format('Evaluation mode changed: recalculating costs at d%d',
        [CostDictionary]));
    end;
  finally
    Ini.Free;
  end;
end;

destructor TWorld.Destroy;
begin
  while Count > 0 do TPopulation(Extract(First)).Free;
  inherited Destroy;
end;

procedure TWorld.UpdateDictionary;
var Dictionary, I: Integer;
begin
  Dictionary := DictionaryForAge(Age);
  if Dictionary = CostDictionary then Exit;
  for I := 0 to Count - 1 do TPopulation(Items[I]).MarkToRecalculate;
  CurrentPopulation := 0;
  CostDictionary := Dictionary;
  EmitMessage(Format('Dictionary d%d: recalculating population costs',
    [CostDictionary]));
end;

procedure TWorld.Live(Amount: Integer);
var
  P1, P2: TPopulation;
  A, B, Child: TPerson;
  Persons: TPersonArray;
  PopulationIndexes: TIntegerArray;
  Dictionary, I, J, PopulationIndex, ParentAttempts: Integer;
begin
  if Amount <= 0 then Exit;
  Amount := Min(Amount, Count);
  if Age = 0 then Inc(Age);
  UpdateDictionary;
  Dictionary := CostDictionary;
  SetLength(Persons, Amount);
  SetLength(PopulationIndexes, Amount);
  for I := 0 to Amount - 1 do
  begin
    PopulationIndex := (CurrentPopulation + I) mod Count;
    PopulationIndexes[I] := PopulationIndex;
    P1 := TPopulation(Items[PopulationIndex]);
    if P1.Count = 0 then
      Child := TPerson.CreateRandom(FOwner.GenomeSize)
    else if TPerson(P1.First).Cost = 0 then
      Child := TPerson(P1.Extract(P1.First))
    else
    begin
      if Count = 1 then begin A := TPerson(P1.First); B := A; end
      else
      begin
        ParentAttempts := 0;
        repeat
          repeat
            J := Max(0, Min(PopulationIndex + Random(3) - 1, Count - 1));
            P2 := TPopulation(Items[J]);
          until P2.Count > 0;
          A := TPerson(P1.Items[Random(P1.Count)]);
          B := TPerson(P2.Items[Random(P2.Count)]);
          Inc(ParentAttempts);
        until (A <> B) or (ParentAttempts >= 100);
        { During a dictionary-wide recalculation the preceding populations
          have already been extracted into this batch.  If only one parent
          remains, self-crossover plus mutation is the valid fallback. }
      end;
      repeat
        Child := TPerson.CreateChild(A, B);
        if P1.ContainsGenome(Child) then FreeAndNil(Child);
      until Child <> nil;
    end;
    Child.Genome[0] := PopulationIndex + 1;
    Persons[I] := Child;
  end;
  try
    FOwner.EvaluateBatch(Persons, Dictionary);
    for I := 0 to Amount - 1 do
    begin
      PopulationIndex := PopulationIndexes[I];
      P1 := TPopulation(Items[PopulationIndex]);
      Child := Persons[I];
      if P1.Count > 0 then
      begin
        if TPerson(P1.First).Cost > 0 then Inc(Age);
        if TPerson(P1.First).Cost > Child.Cost then Inc(Improvements);
      end;
      P1.AddSorted(Child);
      Persons[I] := nil;
      while (P1.Count > 1) and (TPerson(P1.First).Cost > 0) do
        TPerson(P1.Extract(P1.Last)).Free;
      CurrentPopulation := (PopulationIndex + 1) mod Count;
    end;
    { A parallel batch may cross a dictionary threshold.  Keep Age monotonic
      and invalidate the old-dictionary costs immediately after that batch. }
    UpdateDictionary;
  finally
    for I := 0 to High(Persons) do Persons[I].Free;
  end;
end;

procedure TWorld.Save(const FileName: string);
var Ini: TMemIniFile; I: Integer;
  Recovery: string;
begin
  Recovery := FileName + '.Err';
  if FileExists(Recovery) then DeleteFile(Recovery);
  if FileExists(FileName) and not RenameFile(FileName, Recovery) then
    raise EStreamError.CreateFmt('Cannot back up state: %s', [FileName]);
  try
    Ini := TMemIniFile.Create(FileName);
    try
      Ini.WriteInteger('BeeOptState', 'Version', StateVersion);
      Ini.WriteString('BeeOptState', 'Corpus', ExtractFileName(
        ExcludeTrailingPathDelimiter(FOwner.FOptions.CorpusPath)));
      Ini.WriteBool('BeeOptState', 'MixerEnabled', FOwner.MixerEnabled);
      Ini.WriteBool('BeeOptState', 'ExeFilterEnabled',
        FOwner.FOptions.ExeFilterEnabled);
      Ini.WriteInteger('BeeOptState', 'Age', Age);
      Ini.WriteInteger('BeeOptState', 'Improvements', Improvements);
      Ini.WriteInteger('BeeOptState', 'CurrentPopulation', CurrentPopulation);
      Ini.WriteInteger('BeeOptState', 'CorpusFileCount',
        FOwner.FBodyNames.Count);
      for I := 0 to FOwner.FBodyNames.Count - 1 do
      begin
        Ini.WriteString('BeeOptState', 'CorpusFile' + IntToStr(I + 1) +
          '.Name', FOwner.FBodyNames[I]);
        Ini.WriteString('BeeOptState', 'CorpusFile' + IntToStr(I + 1) +
          '.Size', IntToStr(FOwner.FBodySizes[I]));
        Ini.WriteString('BeeOptState', 'CorpusFile' + IntToStr(I + 1) +
          '.Hash', UIntToStr(FOwner.FBodyHashes[I]));
      end;
      for I := 0 to Count - 1 do
        TPopulation(Items[I]).Save(Ini, 'Population' + IntToStr(I));
      Ini.UpdateFile;
    finally Ini.Free; end;
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

constructor TEvaluationWorker.Create(Owner: TOptimizer);
begin
  inherited Create(True);
  FOwner := Owner;
  FStartEvent := TEvent.Create(nil, False, False, '');
  FDoneEvent := TEvent.Create(nil, False, False, '');
  FCounter := TCountingStream.Create;
  FCoder := TRangeCoder.Create(FCounter);
  FModel := FOwner.CreateModel;
  if FOwner.MixerEnabled then FMixer := TBeeMixer.Create(FModel);
  Start;
  FStarted := True;
end;

destructor TEvaluationWorker.Destroy;
begin
  if FStarted then
  begin
    Terminate;
    FStartEvent.SetEvent;
    WaitFor;
  end;
  FMixer.Free;
  FModel.Free;
  FCoder.Free;
  FCounter.Free;
  FDoneEvent.Free;
  FStartEvent.Free;
  inherited Destroy;
end;

procedure TEvaluationWorker.Execute;
begin
  while True do
  begin
    if FStartEvent.WaitFor(INFINITE) <> wrSignaled then Break;
    if Terminated then Break;
    FError := '';
    try
      FPerson.Cost := FOwner.EvaluateWithContext(FPerson, FDictionary,
        FMixer, FModel, FCounter, FCoder);
    except
      on E: Exception do FError := E.ClassName + ': ' + E.Message;
    end;
    FDoneEvent.SetEvent;
  end;
end;

procedure TEvaluationWorker.Queue(Person: TPerson; Dictionary: Cardinal);
begin
  FPerson := Person;
  FDictionary := Dictionary;
  FError := '';
  FDoneEvent.ResetEvent;
  FStartEvent.SetEvent;
end;

procedure TEvaluationWorker.WaitForResult;
begin
  if FDoneEvent.WaitFor(INFINITE) <> wrSignaled then
    raise ESyncObjectException.Create('Evaluation worker wait failed');
  if FError <> '' then
    raise Exception.Create('Parallel evaluation failed: ' + FError);
end;

function TOptimizer.LevelCount: Cardinal;
begin Result := BeeMaxTableLevel; end;

function TOptimizer.GenomeSize: Integer;
begin
  Result := BeeTableParameterCount;
end;

function TOptimizer.CreateModel: TBeeModel;
begin
  Result := TBeeModel.Create;
end;

procedure TOptimizer.AddFile(const FileName: string);
var Stream: TFileStream; N, I: Integer; Filtered: TBytes;
  Hash: QWord;
begin
  N := Length(FBodies);
  SetLength(FBodies, N + 1);
  SetLength(FBodyExecutable, N + 1);
  SetLength(FBodySizes, N + 1);
  SetLength(FBodyHashes, N + 1);
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size > High(Integer) then raise ERangeError.Create('Input file too large');
    SetLength(FBodies[N], Stream.Size);
    if Stream.Size > 0 then Stream.ReadBuffer(FBodies[N][0], Stream.Size);
  finally Stream.Free; end;
  Hash := QWord($CBF29CE484222325);
  for I := 0 to High(FBodies[N]) do
  begin
    Hash := Hash xor FBodies[N][I];
    Hash := Hash * QWord($100000001B3);
  end;
  FBodyNames.Add(ExtractFileName(FileName));
  FBodySizes[N] := Length(FBodies[N]);
  FBodyHashes[N] := Hash;
  { Same recognition and filter as the Bee compressor; the hash and size
    above describe the original file. }
  FBodyExecutable[N] := FOptions.ExeFilterEnabled and
    BeeExeEncode(FBodies[N], Filtered);
  if FBodyExecutable[N] then
  begin
    FBodies[N] := Filtered;
    Inc(FExeFilteredFiles);
  end;
end;

procedure TOptimizer.LoadCorpus;
var Search: TSearchRec; Names: TStringList; Base: string; I, ErrorCode: Integer;
begin
  if not DirectoryExists(FOptions.CorpusPath) then
    raise EFileNotFoundException.CreateFmt('Corpus folder not found: %s',
      [FOptions.CorpusPath]);
  Names := TStringList.Create;
  try
    Base := IncludeTrailingPathDelimiter(FOptions.CorpusPath);
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
  if FExeFilteredFiles > 0 then
    EmitMessage(Format('Preprocessor %s: %d files',
      [BeeExePreprocessorName, FExeFilteredFiles]));
end;

{ The corpus folder is named after the extension it optimizes (txt), as Bee
  selects its tables. }
function TOptimizer.CorpusExtension: string;
begin
  Result := '.' + ExtractFileName(ExcludeTrailingPathDelimiter(
    FOptions.CorpusPath));
end;

type
  TBeeTableList = array of TBeeTableParameters;

{ The tables of Extension in the \m1..\m3 sections of a bee.ini, found as
  Bee finds them (".default" and aliases included). }
function LoadMethodTables(const FileName, Extension: string): TBeeTableList;
var
  Cfg: TConfiguration;
  Method, N: Integer;
  Table: TTableParameters;
begin
  Result := nil;
  Cfg := TConfiguration.Create;
  try
    Cfg.LoadFromFile(FileName);
    for Method := 1 to 3 do
    begin
      if Cfg.IndexOfName('\m' + IntToStr(Method)) < 0 then Continue;
      Cfg.Selector('\m' + IntToStr(Method));
      if not Cfg.CurrentSection.GetTable(Extension, Table) then Continue;
      N := Length(Result);
      SetLength(Result, N + 1);
      SetLength(Result[N], BeeTableParameterCount);
      Move(Table, Result[N][0], BeeTableParameterCount);
    end;
  finally
    Cfg.Free;
  end;
end;

procedure TOptimizer.SeedPopulations(const FileName: string);
var
  Tables: TBeeTableList;
  Table: TBeeTableParameters;
  Person: TPerson;
  PopulationIndex, I, Nearest: Integer;
begin
  { From a bee.ini each population starts from the method table nearest to
    its level; otherwise the file holds one table for all populations. }
  Tables := LoadMethodTables(FileName, CorpusExtension);
  if Length(Tables) = 0 then
  begin
    SetLength(Tables, 1);
    BeeLoadTable(FileName, Tables[0]);
  end;
  for PopulationIndex := 0 to FWorld.Count - 1 do
  begin
    Nearest := 0;
    for I := 1 to High(Tables) do
      if Abs(Tables[I][0] - (PopulationIndex + 1)) <
        Abs(Tables[Nearest][0] - (PopulationIndex + 1)) then Nearest := I;
    Table := Tables[Nearest];
    Person := TPerson.CreateRandom(GenomeSize);
    for I := 0 to BeeTableParameterCount - 1 do
      Person.Genome[I] := Table[I];
    Person.Genome[0] := PopulationIndex + 1;
    Person.Cost := 0;
    TPopulation(FWorld.Items[PopulationIndex]).AddSorted(Person);
  end;
  EmitMessage(Format('Seed loaded: %s (%d populations)',
    [FileName, FWorld.Count]));
end;

constructor TOptimizer.Create(const Options: TOptions);
var I, ThreadCount: Integer;
  NewState: Boolean;
begin
  inherited Create;
  FOptions := Options;
  FBodyNames := TStringList.Create;
  if Options.RandomSeedSpecified then RandSeed := Options.RandomSeed
  else Randomize;
  LoadCorpus;
  if FOptions.ExportOnly then
  begin
    FWorld := TWorld.Load(Self, FOptions.StateName);
    Exit;
  end;
  ThreadCount := FOptions.Threads;
  FOptions.Threads := ThreadCount;
  NewState := not FileExists(FOptions.StateName) and
    not FileExists(FOptions.StateName + '.Err');
  FWorld := TWorld.Load(Self, FOptions.StateName);
  if NewState and (FOptions.SeedName <> '') then
    SeedPopulations(FOptions.SeedName);
  SetLength(FWorkers, ThreadCount);
  for I := 0 to High(FWorkers) do
    FWorkers[I] := TEvaluationWorker.Create(Self);
  EmitMessage(Format('Running with %d threads', [ThreadCount]));
end;

destructor TOptimizer.Destroy;
var I: Integer;
begin
  for I := High(FWorkers) downto 0 do FWorkers[I].Free;
  FWorkers := nil;
  FWorld.Free;
  FBodyNames.Free;
  inherited Destroy;
end;

function TOptimizer.EvaluateWithContext(Person: TPerson; Dictionary: Cardinal;
  Mixer: TBeeMixer; Model: TBeeModel; Counter: TCountingStream;
  Coder: TRangeCoder): Int64;
var
  Body, I: Integer;
  Table: TBeeTableParameters;
begin
  Table := GenomeTable(Person.Genome);
  Counter.Reset;
  Coder.StartEncode;
  if FOptions.MixerEnabled then
  begin
    Model.SetTable(Table);
    Model.SetDictionaryLevel(Dictionary);
    Mixer.SetDictionaryLevel(Dictionary);
    for Body := 0 to High(FBodies) do begin
      Model.FreshFlexible;
      Mixer.Reset;
      for I := 0 to High(FBodies[Body]) do
        Mixer.EncodeByte(FBodies[Body][I], Coder);
    end;
  end
  else
  begin
    Model.SetTable(Table);
    Model.SetDictionaryLevel(Dictionary);
    for Body := 0 to High(FBodies) do begin
      Model.FreshFlexible;
      for I := 0 to High(FBodies[Body]) do
        Model.EncodeByte(FBodies[Body][I], Coder);
    end;
  end;
  Coder.FinishEncode;
  Result := Counter.Size;
end;

procedure TOptimizer.EvaluateBatch(const Persons: TPersonArray;
  Dictionary: Cardinal);
var
  Offset, Active, I: Integer;
  ErrorMessage: string;
begin
  Offset := 0;
  while Offset < Length(Persons) do
  begin
    Active := Min(Length(FWorkers), Length(Persons) - Offset);
    for I := 0 to Active - 1 do
      FWorkers[I].Queue(Persons[Offset + I], Dictionary);
    ErrorMessage := '';
    for I := 0 to Active - 1 do
      try
        FWorkers[I].WaitForResult;
      except
        on E: Exception do
          if ErrorMessage = '' then ErrorMessage := E.Message;
      end;
    if ErrorMessage <> '' then raise Exception.Create(ErrorMessage);
    Inc(Offset, Active);
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

{ As the original BeeOpt: from level 3 upward, the best of each population
  that does not compress worse than the previous one chosen; the lowest
  goes to \m1, the middle one to \m2 and the last to \m3 of bee.ini. }
procedure TOptimizer.ExportBest;
var
  Levels: TList;
  Cfg: TConfiguration;
  Person: TPerson;
  I: Integer;
  Summary: string;

  procedure PutMethod(Method: Integer; Person: TPerson);
  var Table: TBeeTableParameters;
  begin
    Table := GenomeTable(Person.Genome);
    Cfg.Selector('\m' + IntToStr(Method));
    Cfg.CurrentSection.Values[CorpusExtension + '.Size'] :=
      IntToStr(Person.Cost);
    Cfg.CurrentSection.PutData(CorpusExtension, Table[0],
      BeeTableParameterCount);
    Summary := Summary + Format(' m%d=level %d (%d bytes)',
      [Method, Table[0], Person.Cost]);
  end;

begin
  Levels := TList.Create;
  Cfg := TConfiguration.Create;
  try
    for I := 0 to FWorld.Count - 1 do
    begin
      if TPopulation(FWorld.Items[I]).Count = 0 then Continue;
      Person := TPerson(TPopulation(FWorld.Items[I]).First);
      if (Person.Cost <= 0) or (Person.Genome[0] < 3) then Continue;
      if (Levels.Count > 0) and
        (Person.Cost > TPerson(Levels.Last).Cost) then Continue;
      Levels.Add(Person);
    end;
    if Levels.Count = 0 then
      raise EStreamError.Create('State does not contain an evaluated table');
    if FileExists(FOptions.ExportName) then
      Cfg.LoadFromFile(FOptions.ExportName);
    Summary := '';
    PutMethod(1, TPerson(Levels.First));
    PutMethod(2, TPerson(Levels[Levels.Count div 2]));
    PutMethod(3, TPerson(Levels.Last));
    Cfg.SaveToFile(FOptions.ExportName);
    EmitMessage(Format('Tables exported to %s for %s:%s',
      [FOptions.ExportName, CorpusExtension, Summary]));
  finally
    Cfg.Free;
    Levels.Free;
  end;
end;

procedure TOptimizer.Report(TotalBytes: Int64);
var Best: TPerson; Ratio: Double; Progress: TBeeOptProgress;
begin
  Best := SelectBest;
  if (Best <> nil) and (TotalBytes > 0) then
    Ratio := Best.Cost / TotalBytes * 100
  else
    Ratio := 0;
  Progress.Corpus := FOptions.CorpusPath;
  Progress.Age := FWorld.Age;
  Progress.Population := FWorld.CurrentPopulation + 1;
  Progress.Dictionary := FWorld.CostDictionary;
  Progress.Improvements := FWorld.Improvements;
  Progress.Threads := Length(FWorkers);
  Progress.TotalBytes := TotalBytes;
  if Best <> nil then Progress.CompressedBytes := Best.Cost
  else Progress.CompressedBytes := -1;
  Progress.Percentage := Ratio;
  if Assigned(GOnProgress) then GOnProgress(Progress);
end;

procedure TOptimizer.RunGenetic(TotalBytes: Int64);
var
  Completed, Remaining: Int64;
  BatchSize: Integer;
begin
  Completed := 0;
  repeat
    BatchSize := Min(FOptions.Threads, FWorld.Count);
    if FOptions.Turns > 0 then
    begin
      Remaining := FOptions.Turns - Completed;
      if Remaining < BatchSize then BatchSize := Remaining;
    end;
    FWorld.Live(BatchSize);
    Inc(Completed, BatchSize);
    FWorld.Save(FOptions.StateName);
    Report(TotalBytes);
  until StopRequested or
    ((FOptions.Turns > 0) and (Completed >= FOptions.Turns));
end;

procedure TOptimizer.Run;
var TotalBytes: Int64; I: Integer;
begin
  if FOptions.ExportOnly then
  begin
    ExportBest;
    Exit;
  end;
  TotalBytes := 0; for I := 0 to High(FBodies) do Inc(TotalBytes, Length(FBodies[I]));
  Report(TotalBytes);
  RunGenetic(TotalBytes);
end;

procedure RunBeeOpt(OnProgress: TBeeOptProgressEvent;
  OnMessage: TBeeOptMessageEvent; ShouldStop: TBeeOptStopEvent);
var Options: TOptions; Optimizer: TOptimizer;
begin
  GOnProgress := OnProgress;
  GOnMessage := OnMessage;
  GShouldStop := ShouldStop;
  try
    ParseOptions(Options);
    Optimizer := TOptimizer.Create(Options);
    try
      Optimizer.Run;
    finally
      Optimizer.Free;
    end;
  finally
    GOnProgress := nil;
    GOnMessage := nil;
    GShouldStop := nil;
  end;
end;

end.
