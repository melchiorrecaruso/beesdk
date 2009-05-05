{
  Copyright (c) 1999-2009 Andrew Filinsky and Melchiorre Caruso

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

    Implements Genetic optimization for make optimal parameters for Bee 0.7.7.

  Modifyed:

}

{$I compiler.inc}

unit BeeOpt_App;

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Math,
  Classes,
  SysUtils,
  // ---
  Bee_Files,
  Bee_Codec, // TSecondaryEncoder, TSecondaryDecoder...
  Bee_Common,
  Bee_Modeller, // TBaseCoder...
  Bee_Configuration;

const
  CrossoverProbability = 0.15;
  MutationProbability  = 0.01;

type
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
    procedure Save(const FileName: string);
    procedure Live;
    procedure MarkToRecalculate;
    destructor Destroy; override;
  end;

  // TBody class

  TBody = class
  public
    Data: pointer;
    Size: longint;
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
  end;

  // TOptApp class

  TOptApp = class
  public
    constructor Create;
    procedure Evolution;
    destructor Destroy; override;
    procedure Display;
    procedure ExtractLevels(World: TPopulations; const Ext: string);
    procedure CollectWorlds(const Path: string);
    procedure CollectConfigurations(CfgName: string);

    procedure MergeDataRecursively(const Path, Name: string; World: TPopulations);
    procedure DrawLevelProgress;

  public
    SrcName: string;
    World: TPopulations;
    Configuration: TConfiguration;

    Bodyes: TList;
    BodyesSize: longint;

    Nowhere: TNulWriter;
    Encoder: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;

    Priority: longword;

    NeedToRecalculate: boolean;
    NeedToCollectConfig: boolean; // Is it need to collect configurations to Bee.ini?
    NeedToReduceIni: boolean;     // Is it need to reduce Bee.ini?
    NeedToMerge: boolean;
    NeedToClose: boolean;
    NeedToRun: boolean;
  private
    ConfigurationName: string;

  private
    /// Reduce unneeded strings from given Section of Bee.Ini
    procedure ReduceSection(Section: TConfigSection);
  end;

var
  App: TOptApp;

implementation

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
      Genome[I * 2] := Temp[1 + I];
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
    Result := false;
    for I := 2 to SizeOf(Genome) do
    begin
      if Genome[I] <> Person.Genome[I] then exit;
    end;
    Result := true;
  end;

  /// TPopulation

  constructor TPopulation.Create;
  begin
    inherited Create;
  end;

  constructor TPopulation.Create (Stream: TStream);
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
      Inc (I);
    end;
    Insert (I, P);
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

  function TPopulation.HasPerson (Person: TPerson): boolean;
  var
    I: longint;
  begin
    Result := true;
    for I := 0 to Count -1 do
    begin
      if Person.IsEqual(TPerson(List[I])) then Exit;
    end;
    Result := false;
  end;

  /// TPopulations

  constructor TPopulations.Create;
  begin
    inherited Create;
    CurrentPopulation := 0;
    CurrentAge := 0;
    Improvements := 0;
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

    if FileExists(FileName + '.err') then
      Stream := CreateTFileReader(FileName + '.err', fmOpenRead)
    else
      if FileExists(FileName) then
        Stream := CreateTFileReader(FileName, fmOpenRead);

    if Stream <> nil then
    begin
      Stream.ReadBuffer(CurrentPopulation, SizeOf(CurrentPopulation));
      Stream.ReadBuffer(CurrentAge, SizeOf(CurrentAge));
      Stream.ReadBuffer(Improvements, SizeOf(Improvements));
      while Count < 15 do
      begin
        Add(TPopulation.Create(Stream));
      end;
      Stream.Free;
    end else
    begin
      CurrentPopulation := 0;
      CurrentAge   := 0;
      Improvements := 0;
      while Count < 15 do
      begin
        Add(TPopulation.Create);
      end;
    end;
  end;

  procedure TPopulations.Save (const FileName: string);
  var
    Stream: TFileWriter;
    I: longint;
  begin
    RenameFile(FileName, FileName + '.err');
    DeleteFile(FileName);

    Stream := CreateTFileWriter(FileName, fmCreate);
    if Stream <> nil then
    begin
      Stream.WriteBuffer(CurrentPopulation, SizeOf(CurrentPopulation));
      Stream.WriteBuffer(CurrentAge, SizeOf(CurrentAge));
      Stream.WriteBuffer(Improvements, SizeOf(Improvements));
      for I := 0 to Count -1 do
      begin
        TPopulation(List[I]).Save(Stream);
      end;
      Stream.Free;
    end;
    DeleteFile(FileName + '.err');
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
    I, Start, Finish: longint;
    Pi: ^Byte;
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
      Write('|gen| ');
    end else
      if TPerson(Population1.First).Cost = 0 then
      begin
        Person := Population1.Extract (Population1.First);
        Write('|rec| ');
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
        Write('|opt| ');
      end;

    begin
      Person.Genome[1] := CurrentPopulation + 1;

      App.Encoder.SetTable(Person.Genome);

      DictionaryLevel := CurrentAge div 2000 + 1;
      App.Encoder.SetDictionary(DictionaryLevel);

      Write(Format('-d%d', [DictionaryLevel]));

      App.Nowhere.Seek(0, 0);
      App.SecondaryCodec.Start;

      for I := 0 to App.Bodyes.Count -1 do
      begin
        App.Encoder.FreshFlexible;

        Start := 0;
        Pi := TBody(App.Bodyes [I]).Data;
        while Start < TBody (App.Bodyes[I]).Size do
        begin
          Finish := Min(Start + 1024, TBody(App.Bodyes[I]).Size);
          while Start < Finish do
          begin
            App.Encoder.UpdateModel(Pi^);
            Inc(Pi);
            Inc(Start);
          end;

          if App.NeedToClose = True then Break;
        end;
        if App.NeedToClose = True then Break;
      end;

      App.SecondaryCodec.Flush;
      if App.NeedToClose = True then
      begin
        Population1.Add(Person);
        Exit;
      end else
        Person.Cost := App.Nowhere.Seek(0, 1);
    end;

    begin
      if Population1.Count > 0 then
      begin
        if TPerson(Population1.First).Cost > 0 then Inc(CurrentAge);
        if TPerson(Population1.First).Cost > Person.Cost then Inc(Improvements);
      end;
      Population1.Add (Person);

      if (Population1.Count > FullSize) and (TPerson(Population1.First).Cost > 0) then
      begin
        while Population1.Count > HalfSize do
        begin
          TPerson(Population1.Extract(Population1.Last)).Free;
        end;
      end;
    end;

    WriteText(App.SrcName + '.log.txt', Format('%d' + #9, [TPerson(Population1.First).Cost]));
    if CurrentPopulation = 14 then
    begin
      WriteText(App.SrcName + '.log.txt', Format('| %5d turn, -d%d, %d jumps (%1.1f%%).' + Cr,
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
    CurrentAge   := 0;
    Improvements := 0;
  end;

  destructor  TPopulations.Destroy;
  begin
    while Count > 0 do
    begin
      TPopulation(Extract(First)).Free;
    end;
    inherited Destroy;
  end;

  /// TOptApp

  constructor TOptApp.Create;
  var
    T: TSearchRec;
    I: longint;
    S: string;
  begin
    inherited Create;
    {$IFDEF MSWINDOWS}
    SetFileApisToOEM;
    {$ENDIF}
    Randomize;

    SrcName := '';
    Priority := 0;

    NeedToRecalculate := False;
    NeedToReduceIni := False;
    NeedToClose := False;
    NeedToRun := True;

    World := TPopulations.Create;

    Configuration := TConfiguration.Create;
    Configuration.Selector ('\main');
    Configuration.CurrentSection.Values ['Method']     := '3';
    Configuration.CurrentSection.Values ['Dictionary'] := '5';

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
        Configuration.Selector('\main');
        Configuration.CurrentSection.Values['Method'] := IntToStr(Max(1, Min(StrToInt(S), 3)));
      end else
      if Pos('-D', S) = 1 then
      begin
        Delete(S, 1, 2);
        Configuration.Selector('\main');
        Configuration.CurrentSection.Values['Dictionary'] := IntToStr(Max(0, Min(StrToInt(S), 9)));
      end else
      if Pos('-C', S) = 1 then
      begin
        ConfigurationName := Copy(ParamStr(I), 3, MaxInt);
        NeedToCollectConfig := True;
        NeedToRun := False;
      end else
      if Pos('-PRI', S) = 1 then
      begin
        Priority := StrToInt(Copy(ParamStr(I), 5, MaxInt));
      end else
      if S[1] in ['-', '/'] then
      begin

      end else
      if SrcName = '' then
      begin
        SrcName := ParamStr(I);
      end;
    end;

    Bodyes := TList.Create;
    BodyesSize := 0;
    if SrcName <> '' then
    begin
      I := FindFirst(IncludeTrailingBackSlash(SrcName) + '*', faAnyFile, T);
      while I = 0 do
      begin
        if (T.Attr and faDirectory) = 0 then
        begin
          Bodyes.Add(TBody.Create(IncludeTrailingBackSlash(SrcName) + T.Name));
          Inc(BodyesSize, TBody(Bodyes[I]).Size);
        end;
        I := FindNext(T);
      end;
    end;
    FindClose(T);;

    Nowhere := TNulWriter.Create;
    SecondaryCodec := TSecondaryEncoder.Create(Nowhere);
    Encoder := TBaseCoder.Create(SecondaryCodec);
  end;

  destructor TOptApp.Destroy;
  begin
    World.Free;
    Configuration.Free;
    while Bodyes.Count > 0 do
    begin
      TBody(Bodyes.First).Free;
      Bodyes.Delete(0);
    end;
    Bodyes.Free;
    Encoder.Free;
    SecondaryCodec.Free;
    Nowhere.Free;
    inherited Destroy;
  end;

  procedure TOptApp.Display;
  begin
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

  procedure TOptApp.ExtractLevels(World: TPopulations; const Ext: string);
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
      Configuration.Selector('\m1');
      Configuration.CurrentSection.Values[Ext + '.Size'] := IntToStr(TPerson(Levels.First).Cost);
      Configuration.PutData(Ext, TPerson(Levels.First).Genome, SizeOf(TPerson(Levels.First).Genome));
      Configuration.Selector('\m2');
      Configuration.CurrentSection.Values[Ext + '.Size'] := IntToStr(TPerson(Levels.List[Levels.Count div 2]).Cost);
      Configuration.PutData(Ext, TPerson(Levels.List [Levels.Count div 2]).Genome, SizeOf(TPerson(Levels.First).Genome));
      Configuration.Selector('\m3');
      Configuration.CurrentSection.Values[Ext + '.Size'] := IntToStr(TPerson(Levels.Last).Cost);
      Configuration.PutData(Ext, TPerson(Levels.Last).Genome, SizeOf(TPerson(Levels.First).Genome));
    end;
  end;

  procedure TOptApp.CollectWorlds(const Path: string);
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

  procedure TOptApp.CollectConfigurations(CfgName: string);
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
      Configuration.LoadFromFile(SelfPath + CfgName);
    end;

    CollectWorlds(SelfPath);

    if NeedToReduceIni then
    begin
      Configuration.Selector('\m1'); ReduceSection(Configuration.CurrentSection);
      Configuration.Selector('\m2'); ReduceSection(Configuration.CurrentSection);
      Configuration.Selector('\m3'); ReduceSection(Configuration.CurrentSection);
    end;
    Configuration.SaveToFile(CfgName);
  end;

  procedure TOptApp.MergeDataRecursively(const Path, Name: string; World: TPopulations);
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

  procedure TOptApp.Evolution;
  begin
    Writeln('This is BeeOpt 0.8.0 utility (C) 2002-2009 Andrew Filinsky.');
    Writeln('This program calculates parameters for Bee 0.8.0 archiver utility,');
    Writeln('then better compression ratio will be available by using them.');
    Writeln;

    {$IFDEF MSWINDOWS}
    SetPriority(Priority);
    {$ENDIF}

    if NeedToCollectConfig then
    begin
      CollectConfigurations(ConfigurationName);
    end;

    if NeedToRecalculate then
    begin
      World.Free;
      World := TPopulations.Create(SrcName + '.dat');
      World.MarkToRecalculate;
      World.Save(SrcName + '.dat');
      Write('Parameters will be re-estimated at next run...');
      Exit;
    end;

    if NeedToMerge then
    begin
      World.Free;
      World := TPopulations.Create;
      World.CurrentAge := 12000;
      MergeDataRecursively('.\', SrcName + '.dat', World);
      World.CurrentAge := (World.CurrentAge div 2000) * 2000 + 1;
      World.Save (SrcName + '.dat');
      Write('All "' + SrcName + '.dat" merged. Run again to continue...');
      Exit;
    end;

    if NeedToRun = False then
    begin
      Write('Configuration file was maked.');
      Exit;
    end;

    if SrcName = '' then
    begin
      Display;
      Exit;
    end;

    if Bodyes.Count = 0 then
    begin
      Write('No Files for Optimization.');
      Exit;
    end;

    Write('Optimization of ".' + SrcName + '" file extension:');
    Writeln;

    World.Free;
    World := TPopulations.Create(SrcName + '.dat');
    repeat
      DrawLevelProgress;
      World.Live;
      if not NeedToClose then
      begin
        World.Save(SrcName + '.dat');
      end;
    until NeedToClose;
    Writeln;
  end;

  procedure  TOptApp.DrawLevelProgress;
  begin
    Writeln;
    Write(Format('%3d level ', [World.CurrentPopulation + 1]));
    Write(Format('%5d variants ', [World.CurrentAge]));
    Write(Format('%6.3f%% improvements ', [World.Improvements / (World.CurrentAge + 1) * 100]));

    if TPopulation(World.List[World.CurrentPopulation]).Count > 0 then
      Write(Format('%10d packed ', [TPerson(TPopulation(World.List[World.CurrentPopulation]).First).Cost]))
    else
      Write('---------- packed ');
  end;

  procedure TOptApp.ReduceSection(Section: TConfigSection);
  var
    I: longint;
  begin
    I := 0;
    while I < Section.Count do
    begin
      if Pos('.Size', Configuration.CurrentSection.Names[I]) > 0 then
        Section.Delete(I)
      else
        Inc(I);
    end;
  end;

  /// TBody...

  constructor TBody.Create;
  var
    F: TFileStream;
  begin
    F := TFileStream.Create(aFileName, fmOpenRead + fmShareDenyWrite);
    Size := F.Size;
    GetMem(Data, Size);
    F.ReadBuffer(Data^, Size);
    F.Free;
  end;

  destructor  TBody.Destroy;
  begin
    FreeMem(Data);
  end;

end.

