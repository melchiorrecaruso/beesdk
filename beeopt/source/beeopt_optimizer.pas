{
  Copyright (c) 1999-2011 Andrew Filinsky and Melchiorre Caruso

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

  Modifyed:
}

unit BeeOpt_Optimizer;

{$I compiler.inc}

interface

uses
  Math,
  Forms,
  Classes,
  Dialogs,
  SysUtils,
  // ---
  Bee_Files,
  Bee_Consts,
  Bee_Common,
  {$IFDEF cppDLL}
    Bee_LibLink,
  {$ELSE}
    Bee_Modeller,
  {$ENDIF}
  Bee_Configuration,
  BeeLib_Configuration;

type
  // TBody class

  TBody = class
  public
    Data: array of byte;
    Size: longint;
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
  end;

  // TPerson class

  TPerson = class(TObject)
  public
    Genome: TTableParameters;
    Cost: longint;
  public
    constructor Create; overload;
    constructor Create(Parent1, Parent2: TPerson); overload;
    constructor Create(Stream: TStream); overload;
    procedure Save(Stream: TStream);
    procedure MarkToRecalculate;
    procedure ConvertIn;
    procedure ConvertOut;
    function IsEqual(Person: TPerson): boolean;
  end;

  // TPopulation class

  TPopulation = class(TList)
  public
    constructor Create;overload;
    constructor Create(Stream: TStream); overload;
    destructor Destroy; override;
    procedure Save(Stream: TStream);
    procedure Add(P: TPerson);
    procedure MarkToRecalculate;
    function HasPerson(Person: TPerson): boolean;
  end;

  // TPopulations class

  TPopulations = class(TList)
  public
    CurrentPopulation: longint;
    CurrentAge: longint;
    Improvements: longint;
  public
    constructor Create; overload;
    constructor Create(const FileName: string); overload;
    destructor Destroy; override;
    procedure Save(const FileName: string);
    procedure Live;
    procedure MarkToRecalculate;
  end;

  // TOptimizer class

  TOptimizer = class
  private
    FSourceFile: string;
    FConfigurationName: string;
    FConfiguration: TConfiguration;
    FWorld: TPopulations;
    // Reduce unneeded strings from given Section of bee.Ini
    procedure ReduceSection(Section: TConfigSection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Display;
    procedure Evolution;

    procedure CollectWorlds(const Path: string);
    procedure CollectConfigurations(CfgName: string);
    procedure ExtractLevels(World: TPopulations; const Ext: string);
    procedure MergeDataRecursively(const Path, Name: string; World: TPopulations);
  public
    NeedToRecalculate: boolean;
    // Is it need to collect configurations to bee.ini?
    NeedToCollectConfig: boolean;
    // Is it need to reduce Bee.ini?
    NeedToReduceIni: boolean;
    NeedToMerge: boolean;
    NeedToClose: boolean;
    NeedToRun: boolean;
  end;

  TDrawMessage             = procedure(const Message: string) of object;
  TDrawCurrAge             = procedure(CurrentAge: longint) of object;
  TDrawCurrPopulation      = procedure(CurrentPopulation: longint) of object;
  TDrawImprovment          = procedure(Improvment: double) of object;
  TDrawCost                = procedure(Cost: longint) of object;
  TDrawSampleSize          = procedure(SampleSize: longint) of object;
  TDrawBestPackedSize      = procedure(BestPackedSize: longint) of object;
  TDrawDictionaryLevel     = procedure(DictionaryLevel: longint) of object;
  TDrawExtension           = procedure(const Extension: string) of object;

var
  Optimizer: TOptimizer = nil;

  DrawMessage:             TDrawMessage             = nil;
  DrawCurrAge:             TDrawCurrAge             = nil;
  DrawCurrPopulation:      TDrawCurrPopulation      = nil;
  DrawImprovment:          TDrawImprovment          = nil;
  DrawCost:                TDrawCost                = nil;
  DrawSampleSize:          TDrawSampleSize          = nil;
  DrawBestPackedSize:      TDrawBestPackedSize      = nil;
  DrawDictionaryLevel:     TDrawDictionaryLevel     = nil;
  DrawExtension:           TDrawExtension           = nil;

implementation

const
  CrossoverProbability = 0.15;
  MutationProbability  = 0.01;

var
  Bodyes:     TList;
  BodyesSize: longint;
  BestPackedSize:   longint;

  Coder:      pointer;
  Modeller:   pointer;
  NulWriter:  TNulWriter;

  function DoFlushNul(Stream: pointer; Data: pointer; DataSize: longint): longint; {$IFDEF cppDLL} cdecl; {$ENDIF}
  begin
    Result := TNulWriter(Stream).Write(Data^, DataSize);
  end;

  /// TBody...

  constructor TBody.Create;
  var
    F: TFileStream;
  begin
    try
      F := TFileStream.Create(aFileName, fmOpenRead + fmShareDenyWrite);
    except
      F := nil;
    end;

    if F <> nil then
    begin
      Size := F.Size;
      SetLength(Data, F.Size);
      F.Read(Data[0], F.Size);
      F.Free;
    end;
  end;

  destructor  TBody.Destroy;
  begin
    Data := nil;
  end;

  /// TPerson

  constructor TPerson.Create;
  var
    I: longint;
  begin
    for I := 1 to SizeOf(Genome) do
    begin
      Genome[I] := Random(256);
    end;
    Cost := 0;
  end;

  constructor TPerson.Create(Parent1, Parent2: TPerson);
  var
    Parents: array [0..1] of TPerson;
    ParentIndex: longint;
    I, J: longint;
  begin
    Parent1.ConvertIn;
    Parent2.ConvertIn;

    Parents [0] := Parent1;
    Parents [1] := Parent2;
    ParentIndex := Random(2);

    for I := 2 to SizeOf(Genome) do
    begin
      if Random < CrossoverProbability then
      begin
        ParentIndex := ParentIndex xor 1;
      end;
      Genome[I] := Parents[ParentIndex].Genome[I];

      for J := 0 to 7 do
        if Random < MutationProbability then
        begin
          Genome[I] := Genome[I] xor (1 shl J);
        end;
    end;
    Cost := 0;

    Parent1.ConvertOut;
    Parent2.ConvertOut;
    ConvertOut;
  end;

  constructor TPerson.Create(Stream: TStream);
  begin
    Stream.ReadBuffer(Genome, SizeOf(Genome));
    Stream.ReadBuffer(Cost, SizeOf(Cost));
  end;

  procedure TPerson.Save(Stream: TStream);
  begin
    Stream.WriteBuffer(Genome, SizeOf(Genome));
    Stream.WriteBuffer(Cost, SizeOf(Cost));
  end;

  procedure TPerson.MarkToRecalculate;
  begin
    Cost := 0;
  end;

  procedure TPerson.ConvertIn;
  var
    Temp: TTableParameters;
    I: longint;
  begin
    Temp := Genome;
    for I := 1 to SizeOf(Genome) div 2 do
    begin
      Genome[I * 2    ] := Temp[1 + I];
      Genome[I * 2 + 1] := Temp[1 + I + SizeOf(Genome) div 2];
    end;
  end;

  procedure TPerson.ConvertOut;
  var
    Temp: TTableParameters;
    I: longint;
  begin
    Temp := Genome;
    for I := 1 to SizeOf(Genome) div 2 do
    begin
      Genome[1 + I] := Temp[I * 2];
      Genome[1 + I + SizeOf(Genome) div 2] := Temp[I * 2 + 1];
    end;
  end;

  function TPerson.IsEqual(Person: TPerson): boolean;
  var
    I: longint;
  begin
    Result := False;
    for I := 2 to SizeOf(Genome) do
    begin
      if Genome[I] <> Person.Genome[I] then Exit;
    end;
    Result := True;
  end;

  /// TPopulation

  constructor TPopulation.Create;
  begin
    inherited Create;
  end;

  constructor TPopulation.Create(Stream: TStream);
  var
    NewCount: longint;
  begin
    inherited Create;
    Stream.ReadBuffer(NewCount, SizeOf(NewCount));
    while Count < NewCount do
    begin
      Add(TPerson.Create(Stream));
    end;
  end;

  procedure TPopulation.Save(Stream: TStream);
  var
    I: longint;
  begin
    Stream.WriteBuffer(Count, SizeOf(Count));
    for I := 0 to Count -1 do
    begin
      TPerson(List[I]).Save(Stream);
    end;
  end;

  procedure TPopulation.Add(P: TPerson);
  var
    I: longint;
  begin
    I := 0;
    while (I < Count) and (TPerson(List[I]).Cost < P.Cost) do
    begin
      Inc(I);
    end;
    Insert(I, P);
  end;

  procedure TPopulation.MarkToRecalculate;
  begin
    while Count > 1 do
    begin
      TPerson(Extract(Last)).Free;
    end;
    if Count > 0 then TPerson(First).MarkToRecalculate;
  end;

  destructor TPopulation.Destroy;
  begin
    while Count > 0 do
    begin
      TPerson(Extract(First)).Free;
    end;
    inherited Destroy;
  end;

  function TPopulation.HasPerson(Person: TPerson): boolean;
  var
    I: longint;
  begin
    Result := True;
    for I := 0 to Count -1 do
    begin
      if Person.IsEqual(TPerson(List[I])) then Exit;
    end;
    Result := False;
  end;

  /// TPopulations

  constructor TPopulations.Create;
  begin
    inherited Create;
    CurrentPopulation := 0;
    CurrentAge        := 0;
    Improvements      := 0;
    while Count < 15 do
    begin
      Add(TPopulation.Create);
    end;
  end;

  constructor TPopulations.Create(const FileName: string);
  var
    Stream: TFileReader;
  begin
    inherited Create;

    Stream := nil;
    if FileExists(FileName + '.bkp') then
      Stream := CreateTFileReader(FileName + '.bkp', fmOpenRead)
    else
      if FileExists(FileName) then
        Stream := CreateTFileReader(FileName, fmOpenRead);

    if Stream <> nil then
    begin
      Stream.ReadBuffer(CurrentPopulation, SizeOf(CurrentPopulation));
      Stream.ReadBuffer(CurrentAge,        SizeOf(CurrentAge));
      Stream.ReadBuffer(Improvements,      SizeOf(Improvements));
      while Count < 15 do
      begin
        Add(TPopulation.Create(Stream));
      end;
      Stream.Free;
    end else
    begin
      CurrentPopulation := 0;
      CurrentAge        := 0;
      Improvements      := 0;
      while Count < 15 do
      begin
        Add(TPopulation.Create);
      end;
      Stream.Free;
    end;
  end;

  procedure TPopulations.Save(const FileName: string);
  var
    I: longint;
    Stream: TFileWriter;
  begin
    RenameFile(FileName, FileName + '.bkp');
    DeleteFile(FileName);

    Stream := CreateTFileWriter(FileName, fmCreate);
    if Stream <> nil then
    begin
      Stream.WriteBuffer(CurrentPopulation, SizeOf(CurrentPopulation));
      Stream.WriteBuffer(CurrentAge,        SizeOf(CurrentAge));
      Stream.WriteBuffer(Improvements,      SizeOf(Improvements));
      for I := 0 to Count -1 do
      begin
        TPopulation(List[I]).Save(Stream);
      end;
      Stream.Free;
      DeleteFile(FileName + '.bkp');
    end;
  end;

  procedure TPopulations.Live;
  var
    FullSize: longint;
    HalfSize: longint;
    Population1: TPopulation;
    Population2: TPopulation;
    Parent1: TPerson;
    Parent2: TPerson;
    Person: TPerson;
  var
    DictionaryLevel: longint;
    I, J, Loop, Readed: longint;
  begin
    FullSize := 1;
    HalfSize := 1;

    if CurrentAge mod 2000 = 0 then
    begin
      for I := 0 to Count -1 do
      begin
        TPopulation(List[I]).MarkToRecalculate;
      end;
      Inc(CurrentAge);
    end;

    Population1 := List[CurrentPopulation];

    if Population1.Count = 0 then
    begin
      Person := TPerson.Create;
      if Assigned(DrawMessage) then
        DrawMessage('Generate random creature ...');
    end else
      if TPerson(Population1.First).Cost = 0 then
      begin
        Person := Population1.Extract(Population1.First);
        if Assigned(DrawMessage) then
          DrawMessage('Recalculate creature estimation ...');
      end else
      begin
        repeat
          repeat
            Population2 := List[Max(0, Min(CurrentPopulation + Random(3) -1, Count -1))];
          until Population2.Count > 0;
          Parent1 := Population1.List[Random(Population1.Count)];
          Parent2 := Population2.List[Random(Population2.Count)];
        until Parent1 <> Parent2;

        repeat
          Person := TPerson.Create(Parent1, Parent2);
          if Population1.HasPerson(Person) then
          begin
            FreeAndNil(Person);
          end;
        until Person <> nil;
        if Assigned(DrawMessage) then
          DrawMessage('Creatures optimization ...');
      end;

    begin
      Person.Genome[1] := CurrentPopulation + 1;
      BaseCoder_SetTable(Modeller, @Person.Genome);

      DictionaryLevel := CurrentAge div 2000 + 1;
      BaseCoder_SetDictionary(Modeller, DictionaryLevel);

      if Assigned(DrawDictionaryLevel) then
        DrawDictionaryLevel(DictionaryLevel);

      NulWriter.Seek(0, 0);
      RangeEncoder_StartEncode(Coder);
      for I := 0 to Bodyes.Count -1 do
      begin
        BaseCoder_FreshFlexible(Modeller);

        Readed := 0;
        Loop   := TBody(Bodyes[I]).Size div $FFFF;
        while Loop <> 0 do
        begin
          Inc(Readed, BaseCoder_Encode(Modeller,
            @TBody(Bodyes[I]).Data[Readed], $FFFF));
          Dec(Loop);

          Application.ProcessMessages;
          if Optimizer.NeedToClose = True then Break;
        end;
        Inc(Readed, BaseCoder_Encode(Modeller,
          @TBody(Bodyes[I]).Data[Readed], TBody(Bodyes[I]).Size mod $FFFF));

        Application.ProcessMessages;
        if Optimizer.NeedToClose = True then Break;
      end;
      RangeEncoder_FinishEncode(Coder);
    end;

    if Optimizer.NeedToClose = True then
    begin
      Population1.Add(Person);
      Exit;
    end else
      Person.Cost := NulWriter.Seek(0, 1);

    begin
      if Population1.Count > 0 then
      begin
        if TPerson(Population1.First).Cost > 0 then Inc(CurrentAge);
        if TPerson(Population1.First).Cost > Person.Cost then Inc(Improvements);
      end;
      Population1.Add(Person);

      if Population1.Count > 0 then
      begin
        BestPackedSize := Min(BestPackedSize, TPerson(Population1.First).Cost);
      end;

      if (Population1.Count > FullSize) and (TPerson(Population1.First).Cost > 0) then
      begin
        while Population1.Count > HalfSize do
        begin
          TPerson(Population1.Extract(Population1.Last)).Free;
        end;
      end;
    end;

    WriteText(Optimizer.FSourceFile + '.log.txt', Format('%d' + #9, [TPerson(Population1.First).Cost]));
    if CurrentPopulation = 14 then
    begin
      WriteText(Optimizer.FSourceFile + '.log.txt', Format('| %5d turn, -d%d, %d jumps (%1.1f%%).' + Cr,
        [CurrentAge, DictionaryLevel, Improvements, Improvements / (CurrentAge + 1) * 100]));
    end;
    CurrentPopulation := (CurrentPopulation + 1) mod 15;
  end;

  procedure TPopulations.MarkToRecalculate;
  var
    I: longint;
  begin
    for I := 0 to Count -1 do
    begin
      TPopulation(List[I]).MarkToRecalculate;
    end;
    CurrentPopulation := 0;
    CurrentAge        := 0;
    Improvements      := 0;
  end;

  destructor  TPopulations.Destroy;
  begin
    while Count > 0 do
    begin
      TPopulation(Extract(First)).Free;
    end;
    inherited Destroy;
  end;

  /// TOptimizer

  constructor TOptimizer.Create;
  var
    T: TSearchRec;
    I: longint;
    S: string;
  begin
    inherited Create;
    Randomize;

    FSourceFile := '';

    NeedToRecalculate := False;
    NeedToReduceIni   := False;
    NeedToClose       := False;
    NeedToRun         := True;

    FWorld := TPopulations.Create;

      FConfiguration := TConfiguration.Create;
      FConfiguration.Selector ('\main');
      FConfiguration.CurrentSection.Values ['Method']     := '3';
      FConfiguration.CurrentSection.Values ['Dictionary'] := '5';

      for I := 1 to ParamCount do
      begin
        S := UpperCase(ParamStr(I));
        if Pos('-RECALCULATE', S) = 1 then
        begin
          NeedToRecalculate := True;
        end else
        if Pos('-REDUCE', S) = 1 then
        begin
          NeedToReduceIni := True;
        end else
        if Pos('-MERGE', S) = 1 then
        begin
          NeedToMerge := True;
        end else
        if Pos('-M', S) = 1 then
        begin
          Delete(S, 1, 2);
          FConfiguration.Selector('\main');
          FConfiguration.CurrentSection.Values['Method'] := IntToStr(Max(1, Min(StrToInt(S), 3)));
        end else
        if Pos('-D', S) = 1 then
        begin
          Delete(S, 1, 2);
          FConfiguration.Selector('\main');
          FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(Max(0, Min(StrToInt(S), 9)));
        end else
        if Pos('-C', S) = 1 then
        begin
          FConfigurationName := Copy(ParamStr(I), 3, MaxInt);
          NeedToCollectConfig := True;
          NeedToRun := False;
        end else
        if Pos('-PRI', S) = 1 then
        begin
          SetPriority(StrToInt(Copy(ParamStr(I), 5, MaxInt)));
        end else

        if S[1] in ['-', '/'] then
        begin
          // nothing to do
        end else
        if FSourceFile = '' then
        begin
          FSourceFile := ParamStr(I);
        end;
      end;

      BodyesSize := 0;
      Bodyes     := TList.Create;
      if FSourceFile <> '' then
      begin
        I := FindFirst(IncludeTrailingBackSlash(FSourceFile) + '*', faAnyFile, T);
        while I = 0 do
        begin
          if (T.Attr and faDirectory) = 0 then
          begin
            Bodyes.Add(TBody.Create(IncludeTrailingBackSlash(FSourceFile) + T.Name));
            Inc(BodyesSize, T.Size);
          end;
          I := FindNext(T);
        end;
        FindClose(T);
      end;
      BestPackedSize := BodyesSize;

      if Assigned(DrawSampleSize)     then DrawSampleSize    (BodyesSize);
      if Assigned(DrawBestPackedSize) then DrawBestPackedSize(BodyesSize);
      if Assigned(DrawExtension)      then DrawExtension     (FSourceFile);

      NulWriter := TNulWriter.Create;
      Coder     := RangeEncoder_Create(NulWriter, @DoFlushNul);
      Modeller  := BaseCoder_Create(Coder);
    end;

    destructor TOptimizer.Destroy;
    begin
      FWorld.Free;
      FConfiguration.Free;
      while Bodyes.Count > 0 do
      begin
        TBody(Bodyes.First).Free;
        Bodyes.Delete(0);
      end;
      Bodyes.Free;

      BaseCoder_Destroy(Modeller);
      RangeEncoder_Destroy(Coder);
      NulWriter.Free;
      inherited Destroy;
    end;

    procedure TOptimizer.Display;
    begin
      Writeln('This is BeeOpt 0.8.0 utility (C) 2002-2009 Andrew Filinsky.');
      Writeln('This program calculates parameters for Bee 0.8.0 archiver utility,');
      Writeln('then better compression ratio will be available by using them.');
      Writeln;
      Writeln('Usage: BeeOpt <Ext> [<Options>]');
      Writeln;
      Writeln('<Ext>: Is the name of folder, which contains a set of');
      Writeln('       typical files with <ext> extension. This files will');
      Writeln('       be used for parameters optimization.');
      Writeln;
      Writeln('<Options> is:');
      Writeln;
      Writeln('  -d<1..5>  Set dictionary size (d1 < 8m, d2 (default) <16m, d3 < 32m, ...)');
      Writeln('  -c<name>  Collect configuration files from current folder and its');
      Writeln('            subfolders, then place they into configuration file into');
      Writeln('            current folder, without duplicates.');
      Writeln;
      Writeln('  -recalculate  Recalculate parameters for start new optimization');
      Writeln('                session at next running of BeeOpt.');
      Writeln;
      Writeln('  -merge  Collect all <ext>.dat files in subfolders to one <ext>.dat,');
      Writeln('          and prepare it to recalculate.');
      Writeln;
      Writeln('  -pri<0..3>  Set process priority (0-Idle, 1-Normal, 2-High, 3-RealTime)');
      Writeln('  -cfg<name>  Use specified configuration file. By default will');
      Writeln('              use "<ext>.dat" parameters file and "bee.ini"');
      Writeln('              configuration file.');
      Writeln;
      Writeln('How to use BeeOpt:');
      Writeln;
      Writeln('  For optimization of parameters the <ext> subfolder must');
      Writeln('  be created, in which the set of typical files must be');
      Writeln('  placed. Total size of files must be at least 1 mbyte.');
      Writeln;
      Writeln('  Then you can start BeeOpt, using <ext> as parameter.');
      Writeln('  The result of optimization will be placed in BeeOpt.ini');
      Writeln('  file.');
      Writeln;
      Writeln('  At last, you can construct configuration file for Bee');
      Writeln('  0.8.x, by starting BeeOpt with an option -c. Send');
      Writeln('  constructed configuration files to e-mail:');
      Writeln;
      Writeln('              filinsky@mail.ru,');
      Writeln;
      Writeln('  then Yours configuration files will be used for reception');
      Writeln('  of the better compression ratio by the Bee 0.8.x.');
      Writeln;
      Writeln('Examples:');
      Writeln;
      Writeln('  Create parameters for *.doc files. "doc" subfolder');
      Writeln('  with set of *.doc files inside must be previously');
      Writeln('  created:');
      Writeln;
      Writeln('  # BeeOpt doc');
      Writeln;
      Writeln('  Create parameters for *.xls files, dictionary will use');
      Writeln('  32m of memory. "xls" folder with set of *.xls files');
      Writeln('  inside must be previously created:');
      Writeln;
      Writeln('  # BeeOpt xls -d3');
      Writeln;
      Writeln('  Create parameters for *.* files. "default" folder with');
      Writeln('  set of different files inside must be previously');
      Writeln('  created:');
      Writeln;
      Writeln('  # BeeOpt default');
      Writeln;
      Writeln('  Construct configuration file for Bee 0.8.x:');
      Writeln;
      Writeln('  # BeeOpt -c');
    end;

    procedure TOptimizer.ExtractLevels(World: TPopulations; const Ext: string);
    var
      Levels: TList;
      I: longint;
    begin
      Levels := TList.Create;

      WriteText('collect.txt', Cr + Ext + ':' + Cr);
      for I := 0 to World.Count -1 do
      begin
        if TPopulation(World.List[I]).Count = 0 then Continue;
        WriteText('collect.txt', Format('%d:', [TPerson(TPopulation(World.List[I]).First).Genome[1]]) + #9 + Format('%d', [TPerson(TPopulation(World.List[I]).First).Cost]));

        if TPerson(TPopulation (World.List [I]).First).Genome [1] < 3 then
        begin
          WriteText('collect.txt', Cr);
          Continue;
        end else
          if (Levels.Count > 0) and (TPerson(TPopulation(World.List[I]).First).Cost > TPerson(Levels.Last).Cost) then
          begin
            WriteText('collect.txt', Cr);
            Continue;
          end else
          begin
            WriteText('collect.txt', ' +' + Cr);
            Levels.Add(TPopulation(World.List[I]).First);
          end;
      end;

      if Levels.Count > 0 then
      begin
        FConfiguration.Selector('\m1');
        FConfiguration.CurrentSection.Values[Ext + '.Size'] := IntToStr(TPerson(Levels.First).Cost);
        FConfiguration.PutData(Ext, TPerson(Levels.First).Genome, SizeOf(TPerson(Levels.First).Genome));
        FConfiguration.Selector('\m2');
        FConfiguration.CurrentSection.Values[Ext + '.Size'] := IntToStr(TPerson(Levels.List[Levels.Count div 2]).Cost);
        FConfiguration.PutData(Ext, TPerson(Levels.List [Levels.Count div 2]).Genome, SizeOf(TPerson(Levels.First).Genome));
        FConfiguration.Selector('\m3');
        FConfiguration.CurrentSection.Values[Ext + '.Size'] := IntToStr(TPerson(Levels.Last).Cost);
        FConfiguration.PutData(Ext, TPerson(Levels.Last).Genome, SizeOf(TPerson(Levels.First).Genome));
      end;
    end;

    procedure TOptimizer.CollectWorlds(const Path: string);
    var
      World: TPopulations;
      T: TSearchRec;
      Err: longint;
    begin
      Err := FindFirst(Path + '*.dat', faAnyFile, T);
      while Err = 0 do
      begin
        if (T.Attr and faDirectory) = 0 then
        begin
          World := TPopulations.Create(Path + T.Name);
          ExtractLevels(World, '.' + ChangeFileExt(ExtractFileName(T.Name), ''));
          World.Free;
        end;
        Err := FindNext(T);
      end;
      FindClose(T);

      Err := FindFirst(Path + '*', faAnyFile, T);
      while Err = 0 do
      begin
        if (T.Attr and faDirectory) = faDirectory then
        begin
          if (T.Name <> '.') and (T.Name <> '..') then
          begin
            CollectWorlds(Path + T.Name + PathDelim);
          end;
        end;
        Err := FindNext(T);
      end;
      FindClose(T);
    end;

    procedure TOptimizer.CollectConfigurations(CfgName: string);
    begin
      if CfgName = '' then
      begin
        CfgName := DefaultCfgName;
      end;

      if ExtractFileExt(CfgName) = '' then
      begin
        CfgName := CfgName + '.ini';
      end;

      if FileExists(SelfPath + CfgName) then
      begin
        FConfiguration.LoadFromFile(SelfPath + CfgName);
      end;

      CollectWorlds(SelfPath);

      if NeedToReduceIni then
      begin
        FConfiguration.Selector('\m1'); ReduceSection(FConfiguration.CurrentSection);
        FConfiguration.Selector('\m2'); ReduceSection(FConfiguration.CurrentSection);
        FConfiguration.Selector('\m3'); ReduceSection(FConfiguration.CurrentSection);
      end;
      FConfiguration.SaveToFile(CfgName);
    end;

    procedure TOptimizer.MergeDataRecursively(const Path, Name: string; World: TPopulations);
    var
      TmpWorld: TPopulations;
      T: TSearchRec;
      I: longint;
      Err: longint;
    begin
      Err := FindFirst(Path + Name, faAnyFile, T);
      while Err = 0 do
      begin
        if (T.Attr and faDirectory) = 0 then
        begin
          TmpWorld := TPopulations.Create(Path + T.Name);
          World.CurrentAge := Max(0, Min(World.CurrentAge, TmpWorld.CurrentAge - 2000));
          TmpWorld.MarkToRecalculate;
          for I := 0 to TmpWorld.Count - 1 do
            if TPopulation (TmpWorld.List [I]).Count > 0 then
            begin
              TPopulation(World.List[I]).Add(TPopulation(TmpWorld.List[I]).First);
              TPopulation(TmpWorld.List[I]).Delete (0);
            end;
          TmpWorld.Free;
        end;
        Err := FindNext(T);
      end;
      FindClose (T);

      Err := FindFirst(Path + '*', faAnyFile, T);
      while Err = 0 do
      begin
        if (T.Attr and faDirectory) = faDirectory then
        begin
          if (T.Name <> '.') and (T.Name <> '..') then
          begin
            MergeDataRecursively(Path + T.Name + PathDelim, Name, World);
          end;
        end;
        Err := FindNext(T);
      end;
      FindClose (T);
    end;

    procedure TOptimizer.Evolution;
    begin
      if NeedToCollectConfig then
      begin
        CollectConfigurations(FConfigurationName);
      end;

      if NeedToRecalculate then
      begin
        FWorld.Free;
        FWorld := TPopulations.Create(FSourceFile + '.dat');
        FWorld.MarkToRecalculate;
        FWorld.Save(FSourceFile + '.dat');

        if Assigned(DrawMessage) then
          DrawMessage('Parameters will be re-estimated at next run...');

        NeedToClose := True;
      end;

      if NeedToMerge then
      begin
        FWorld.Free;
        FWorld := TPopulations.Create;
        FWorld.CurrentAge := 12000;
        MergeDataRecursively('.\', FSourceFile + '.dat', FWorld);
        FWorld.CurrentAge := (FWorld.CurrentAge div 2000) * 2000 + 1;
        FWorld.Save (FSourceFile + '.dat');

        if Assigned(DrawMessage) then
          DrawMessage('All "' + FSourceFile + '.dat" merged. Run again to continue...');

        NeedToClose := True;
      end;

      if NeedToRun = False then
      begin
        if Assigned(DrawMessage) then
          DrawMessage('Configuration file was maked.');

        NeedToClose := True;
      end;

      if FSourceFile = '' then
      begin
        if Assigned(DrawMessage) then
          DrawMessage('Folder is not selected.');

        NeedToClose := True;
      end;

      if Bodyes.Count = 0 then
      begin
        if Assigned(DrawMessage) then
          DrawMessage('No Files for Optimization.');

        NeedToClose := True;
      end;

      if not NeedToClose then
      begin
        if Assigned(DrawMessage) then
          DrawMessage('Optimization of ".' + FSourceFile + '" file extension:');

        FWorld.Free;
        FWorld := TPopulations.Create(FSourceFile + '.dat');
        while not NeedToClose do
        begin
          if Assigned(DrawCurrAge)        then DrawCurrAge(FWorld.CurrentAge);
          if Assigned(DrawCurrPopulation) then DrawCurrPopulation(FWorld.CurrentPopulation + 1);
          if Assigned(DrawImprovment)     then DrawImprovment(FWorld.Improvements / (FWorld.CurrentAge + 1) * 100);
          if Assigned(DrawCost) then
          begin
            if TPopulation (FWorld.List[FWorld.CurrentPopulation]).Count > 0 then
               DrawCost(TPerson(TPopulation(FWorld.List[FWorld.CurrentPopulation]).First).Cost)
             else
               DrawCost(0);
          end;
          FWorld.Live;
          FWorld.Save(FSourceFile + '.dat');
          if Assigned(DrawBestPackedSize) then DrawBestPackedSize(BestPackedSize);
        end;
      end;
    end;

    procedure TOptimizer.ReduceSection(Section: TConfigSection);
    var
      I: longint;
    begin
      I := 0;
      while I < Section.Count do
      begin
        if Pos('.Size', FConfiguration.CurrentSection.Names[I]) > 0 then
          Section.Delete(I)
        else
          Inc(I);
      end;
    end;

end.

