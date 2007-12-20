{Implements Genetic optimization for make optimal parameters
  for Bee 0.7.7.
}

unit BeeOpt_Form;

interface

uses
  Math,
  Forms,
  Classes,
  dialogs,
  Messages,
  SysUtils,
  ComCtrls,
  StdCtrls,
  Controls,
  LResources,
  //
  Bee_Codec,
  Bee_Files,
  Bee_Common,
  Bee_Headers,
  Bee_Modeller,
  Bee_Configuration;

const
  CrossoverProbability = 0.15;
  MutationProbability  = 0.01;
  DefaultCfgName = 'Bee.ini';

type

  TPerson = class (TObject)
    Genome: TTableParameters;
    Cost: integer;
    constructor Create; overload;
    constructor Create (Parent1, Parent2: TPerson); overload;
    constructor Create (Stream: TStream); overload;
    procedure   Save (Stream: TStream);
    procedure   MarkToRecalculate;
    procedure   ConvertIn;
    procedure   ConvertOut;
    function    IsEqual (Person: TPerson): boolean;
  end;

  TPopulation = class (TList)
    constructor Create; overload;
    constructor Create (Stream: TStream); overload;
    destructor  Destroy; override;
    procedure   Save (Stream: TStream);
    procedure   Add (P: TPerson);
    procedure   MarkToRecalculate;
    function    HasPerson (Person: TPerson): boolean;
  end;

  TPopulations = class (TList)
    CurrentPopulation: integer;
    CurrentAge: integer;
    Improvements: integer;
    constructor Create; overload;
    constructor Create (const FileName: string); overload;
    procedure   Save (const FileName: string);
    procedure   Live;
    procedure   MarkToRecalculate;
    destructor  Destroy; override;
  end;

  TBody = class
    Data: Pointer;
    Size: integer;
    constructor Create (const aFileName: string);
    destructor  Destroy; override;
  end;

  TApp = class
    constructor Create;
    procedure   Evolution;
    destructor  Destroy; override;
    procedure   ExtractLevels (World: TPopulations; const Ext: string);
    procedure   CollectWorlds (const Path: string);
    procedure   CollectConfigurations (CfgName: string);
    procedure   MergeDataRecursively (const Path, Name: string; World: TPopulations);
    procedure   DrawLevelProgress;

  public
    SrcName: string;
    Cfg: TConfiguration;
    World: TPopulations;

    Headers: THeaders;
    Bodyes: TList;
    SamplesSize: integer;

    Encoder: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Nowhere: TNulWriter;

    Priority: Cardinal;

  private
    ConfigurationName: string;

  private
    /// Reduce unneeded strings from given Section of Bee.Ini
    procedure   ReduceSection (Section: TConfigSection);
  end;

  TMainForm = class (TForm)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    Label_Extension: TLabel;
    Label_ExtensionValue: TLabel;
    Label_variantsEstimated: TLabel;
    Label_variantsEstimatedValue: TLabel;
    Label_PercentOfImprovements: TLabel;
    Label_PercentOfImprovementsValue: TLabel;
    Label_LevelValue: TLabel;
    StatusBar: TStatusBar;
    Label_About: TLabel;
    Label_SampleSize: TLabel;
    Label_SampleSizeValue: TLabel;
    Label_PackedSize: TLabel;
    Label_PackedSizeValue: TLabel;
    Label_DictionaryLevel: TLabel;
    Label_DictionaryLevelValue: TLabel;

    procedure FormCreate (Sender: TObject);
    procedure FormClose (Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy (Sender: TObject);

  private
    { Private declarations }
    NeedToRecalculate,
    NeedToCollectConfig,      /// Is it need to collect configurations to Bee.ini?
    NeedToReduceIni,          /// Is it need to reduce Bee.ini?
    NeedToMerge,
    NeedToClose,
    NeedToRun: Boolean;

    NeedToHide: Boolean;
  public
    { Public declarations }
    App: TApp;
  end;

var
  MainForm: TMainForm;

implementation

  /// TPerson

  constructor TPerson.Create;
  var
    I: integer;
  begin
    for I := 1 to SizeOf (Genome) do Genome [I] := Random (256);
    Cost := 0;
  end;

  constructor TPerson.Create (Parent1, Parent2: TPerson);
  var
    Parents: array [0..1] of TPerson;
    ParentIndex: integer;
    I, J: integer;
  begin
    Parent1.ConvertIn;
    Parent2.ConvertIn;

    Parents [0] := Parent1;
    Parents [1] := Parent2;
    ParentIndex := Random (2);

    for I := 2 to SizeOf (Genome) do
    begin
      if Random < CrossoverProbability then ParentIndex := ParentIndex xor 1;

      Genome [I] := Parents [ParentIndex].Genome [I];

      for J := 0 to 7 do
        if Random < MutationProbability then Genome [I] := Genome [I] xor (1 shl J);
    end;

    Cost := 0;

    Parent1.ConvertOut;
    Parent2.ConvertOut;
    ConvertOut;
  end;

  constructor TPerson.Create (Stream: TStream);
  begin
    Stream.ReadBuffer (Genome, SizeOf (Genome));
    Stream.ReadBuffer (Cost, SizeOf (Cost));
  end;

  procedure   TPerson.Save (Stream: TStream);
  begin
    Stream.WriteBuffer (Genome, SizeOf (Genome));
    Stream.WriteBuffer (Cost, SizeOf (Cost));
  end;

  procedure   TPerson.MarkToRecalculate;
  begin
    Cost := 0;
  end;

  procedure  TPerson.ConvertIn;
  var
    Temp: TTableParameters;
    I: integer;
  begin
    Temp := Genome;
    for I := 1 to SizeOf (Genome) div 2 do
    begin
      Genome [I * 2] := Temp [1 + I];
      Genome [I * 2 + 1] := Temp [1 + I + SizeOf (Genome) div 2];
    end;
  end;

  procedure  TPerson.ConvertOut;
  var
    Temp: TTableParameters;
    I: integer;
  begin
    Temp := Genome;
    for I := 1 to SizeOf (Genome) div 2 do begin
      Genome [1 + I] := Temp [I * 2];
      Genome [1 + I + SizeOf (Genome) div 2] := Temp [I * 2 + 1];
    end;
  end;

  function  TPerson.IsEqual (Person: TPerson): boolean;
  var
    I: integer;
  begin
    Result := false;
    for I := 2 to SizeOf (Genome) do
      if Genome [I] <> Person.Genome [I] then exit;
    Result := true;
  end;

  /// TPopulation

  constructor TPopulation.Create;
  begin
    Inherited Create;
  end;

  constructor TPopulation.Create (Stream: TStream);
  var
    NewCount: integer;
  begin
    Inherited Create;
    
    Stream.ReadBuffer (NewCount, SizeOf (NewCount));

    while Count < NewCount do
      Add (TPerson.Create (Stream));
  end;

  procedure   TPopulation.Save (Stream: TStream);
  var
    I: integer;
    NewCount: integer;
  begin
    NewCount := Count;

    Stream.WriteBuffer (NewCount, SizeOf (NewCount));

    for I := 0 to NewCount - 1 do TPerson (List [I]).Save (Stream);
  end;

  procedure   TPopulation.Add (P: TPerson);
  var
    I: integer;
  begin
    I := 0;
    while (I < Count) and (TPerson (List [I]).Cost < P.Cost) do Inc (I);
    Insert (I, P);
  end;

  procedure   TPopulation.MarkToRecalculate;
  begin
    while Count > 1 do TPerson (Extract (Last)).Free;
    if Count > 0 then TPerson (First).MarkToRecalculate;
  end;

  destructor  TPopulation.Destroy;
  begin
    while Count > 0 do TPerson (Extract (First)).Free;
    Inherited;
  end;

  function  TPopulation.HasPerson (Person: TPerson): boolean;
  var
    I: integer;
  begin
    Result := true;
    for I := 0 to Count - 1 do
      if Person.IsEqual (TPerson (List [I])) then exit;
    Result := false;
  end;

  /// TPopulations

  constructor TPopulations.Create;
  begin
    Inherited Create;
    CurrentPopulation := 0;
    CurrentAge := 0;
    Improvements := 0;
    while Count < 15 do Add (TPopulation.Create);
  end;

  constructor TPopulations.Create (const FileName: string);
  var
    Stream: TFileReader;
  begin
    Inherited Create;

    Stream := Nil;

    if FileExists (FileName + '.Err') then Stream := TFileReader.Create (FileName + '.Err', fmOpenRead) else
    if FileExists (FileName) then Stream := TFileReader.Create (FileName, fmOpenRead);

    if Stream <> Nil then
    begin
      Stream.ReadBuffer (CurrentPopulation, SizeOf (CurrentPopulation));
      Stream.ReadBuffer (CurrentAge, SizeOf (CurrentAge));
      Stream.ReadBuffer (Improvements, SizeOf (Improvements));
      while Count < 15 do Add (TPopulation.Create (Stream));

      Stream.Free;
    end else
    begin
      CurrentPopulation := 0;
      CurrentAge := 0;
      Improvements := 0;
      while Count < 15 do Add (TPopulation.Create);
    end;
  end;

  procedure   TPopulations.Save (const FileName: string);
  var
    Stream: TFileWriter;
    I: integer;
  begin
    RenameFile (FileName, FileName + '.Err');
    DeleteFile (FileName);

    Stream := TFileWriter.Create (FileName, fmCreate);
    Stream.WriteBuffer (CurrentPopulation, SizeOf (CurrentPopulation));
    Stream.WriteBuffer (CurrentAge, SizeOf (CurrentAge));
    Stream.WriteBuffer (Improvements, SizeOf (Improvements));
    for I := 0 to Count - 1 do
      TPopulation (List [I]).Save (Stream);

    Stream.Free;
    DeleteFile (FileName + '.Err');
  end;

  procedure  TPopulations.Live;
  var
    FullSize,
    HalfSize: integer;
    Population1,
    Population2: TPopulation;
    Parent1,
    Parent2,
    Person: TPerson;
  var
    DictionaryLevel: integer;
    I, Start, Finish: integer;
    Pi: ^Byte;
  begin

    FullSize := 1;
    HalfSize := 1;

    if CurrentAge mod 2000 = 0 then
    begin
      for I := 0 to Count - 1 do
        TPopulation (List [I]).MarkToRecalculate;

      Inc (CurrentAge);
    end;

    Population1 := List [CurrentPopulation];

    if Population1.Count = 0 then
    begin
      Person := TPerson.Create;
      MainForm.StatusBar.Panels.Items [0].Text := 'Generate random creature...';
    end else
      if TPerson (Population1.First).Cost = 0 then
    begin
      Person := Population1.Extract (Population1.First);
      MainForm.StatusBar.Panels.Items [0].Text := 'Recalculate creature estimation...';
    end else
    begin
      repeat
        repeat
          Population2 := List [Max (0, Min (CurrentPopulation + Random (3) - 1, Count - 1))];
        until Population2.Count > 0;

        Parent1 := Population1.List [Random (Population1.Count)];
        Parent2 := Population2.List [Random (Population2.Count)];
      until Parent1 <> Parent2;

      repeat
        Person := TPerson.Create (Parent1, Parent2);
        if Population1.HasPerson (Person) then FreeAndNil (Person);
      until Person <> nil;
      MainForm.StatusBar.Panels.Items [0].Text := 'Creatures optimization...';
    end;


    begin
      Person.Genome [1] := CurrentPopulation + 1;

      MainForm.App.Encoder.SetTable (Person.Genome);

      DictionaryLevel := CurrentAge div 2000 + 1;
      MainForm.App.Encoder.SetDictionary (DictionaryLevel);
      MainForm.Label_DictionaryLevelValue.Caption := Format ('%d (~%d Mb)', [DictionaryLevel, (1 shl (17 + Min (Max (0, DictionaryLevel), 9))) * 20 shr 20]);

      MainForm.App.Nowhere.Seek (0 ,0);

      MainForm.App.SecondaryCodec.Start;

      for I := 0 to MainForm.App.Bodyes.Count - 1 do
      begin
        MainForm.App.Encoder.FreshFlexible;

        Start := 0;
        Pi := TBody (MainForm.App.Bodyes [I]).Data;
        while Start < TBody (MainForm.App.Bodyes [I]).Size do
        begin
          Finish := Min (Start + 1024, TBody (MainForm.App.Bodyes [I]).Size);
          while Start < Finish do
          begin
            MainForm.App.Encoder.UpdateModel (Pi^);
            Inc (Pi);
            Inc (Start);
          end;

          Application.ProcessMessages;
          if MainForm.NeedToClose = True then Break;
        end;
        if MainForm.NeedToClose = True then Break;
      end;

      MainForm.App.SecondaryCodec.Flush;

      if MainForm.NeedToClose = True then
      begin
        Population1.Add (Person);
        Exit;
      end else
      begin
        Person.Cost := MainForm.App.Nowhere.Seek (0 ,1);
      end;
    end;

    begin
      if Population1.Count > 0 then
      begin
        if TPerson (Population1.First).Cost > 0 then Inc (CurrentAge);

        if TPerson (Population1.First).Cost > Person.Cost then Inc (Improvements);
      end;

      Population1.Add (Person);

      if (Population1.Count > FullSize) and (TPerson (Population1.First).Cost > 0) then

        while Population1.Count > HalfSize do TPerson (Population1.Extract (Population1.Last)).Free;
    end;

    WriteText (MainForm.App.SrcName + '.log.txt', Format ('%d' + #9, [TPerson (Population1.First).Cost]));

    if CurrentPopulation = 14 then
      WriteText (MainForm.App.SrcName + '.log.txt', Format ('| %5d turn, -d%d, %d jumps (%1.1f%%).' + Cr, [CurrentAge, DictionaryLevel, Improvements, Improvements / (CurrentAge + 1) * 100]));

    CurrentPopulation := (CurrentPopulation + 1) mod 15;
  end;

  procedure   TPopulations.MarkToRecalculate;
  var
    I: integer;
  begin
    for I := 0 to Count - 1 do TPopulation (List [I]).MarkToRecalculate;
    CurrentPopulation := 0;
    CurrentAge := 0;
    Improvements := 0;
  end;

  destructor  TPopulations.Destroy;
  begin
    while Count > 0 do TPopulation (Extract (First)).Free;
    Inherited Destroy;
  end;

  constructor TApp.Create;
  var
    I: integer;
    S: string;
  begin
    inherited Create;

    Randomize;

    World := TPopulations.Create;

    SrcName := '';

    Cfg := TConfiguration.Create;

    Cfg.Selector ('\main');
    Cfg.CurrentSection.Values ['Method']     := '3';
    Cfg.CurrentSection.Values ['Dictionary'] := '5';

    Priority := 0;

    for I := 1 to ParamCount do begin
      S := UpperCase (ParamStr (I));

      if Pos ('-RECALCULATE', S) = 1 then begin
        MainForm.NeedToRecalculate := True;
      end Else

      if Pos ('-REDUCE', S) = 1 then begin
        MainForm.NeedToReduceIni := True;
      end Else

      if Pos ('-MERGE', S) = 1 then begin
        MainForm.NeedToMerge := True;
      end Else

      if Pos ('-HIDE', S) = 1 then begin
        MainForm.NeedToHide := True;
      end Else

      if Pos ('-M', S) = 1 then begin
        Delete (S, 1, 2);
        Cfg.Selector ('\main');
        Cfg.CurrentSection.Values ['Method'] := IntToStr (Max (1, Min (StrToInt (S), 3)));
      end Else

      if Pos ('-D', S) = 1 then begin
        Delete (S, 1, 2);
        Cfg.Selector ('\main');
        Cfg.CurrentSection.Values ['Dictionary'] := IntToStr (Max (0, Min (StrToInt (S), 9)));
      end Else

      if Pos ('-C', S) = 1 then begin
        ConfigurationName := Copy (ParamStr (I), 3, MaxInt);
        MainForm.NeedToCollectConfig := True;
        MainForm.NeedToRun := False;
      end Else

      if Pos ('-PRI', S) = 1 then begin
        Priority := StrToInt (Copy (ParamStr (I), 5, MaxInt));
      end else

      if S [1] in ['-', '/'] then begin
        /// Nothing
      end else

      if SrcName = '' then
      begin
        SrcName := ParamStr (I);
        MainForm.Label_ExtensionValue.Caption := '<' + SrcName + '>';
      end;
    end;

    Headers     := THeaders.Create;
    Bodyes      := TList.Create;
    SamplesSize := 0;

    if SrcName <> '' then
    begin
      Headers.AddNews (SrcName + '\*');
      Headers.SortNews (Cfg, False {not Solid}, False, '');

      for I := 0 to Headers.Count - 1 do
      begin
        Bodyes.Add (TBody.Create (THeader (Headers [I]).Name));
        SamplesSize := SamplesSize + TBody (Bodyes [I]).Size;
      end;
    end;

    Nowhere := TNulWriter.Create;
    SecondaryCodec := TSecondaryEncoder.Create (Nowhere);
    Encoder := TBaseCoder.Create (SecondaryCodec);

    MainForm.Label_SampleSizeValue.Caption := Format ('%d', [SamplesSize]);
  end;

  destructor  TApp.Destroy;
  begin
    World.Free;
    Cfg.Free;
    Headers.Free;
    while Bodyes.Count > 0 do begin TBody (Bodyes.First).Free; Bodyes.Delete (0); end;
    Bodyes.Free;
    Encoder.Free;
    SecondaryCodec.Free;
    Nowhere.Free;
    Inherited Destroy;
  end;

  procedure  TApp.ExtractLevels (World: TPopulations; const Ext: string);
  var
    Levels: TList;
    I: integer;
  begin
    Levels := TList.Create;

    WriteText ('Collect.txt', Cr + Ext + ':' + Cr);
    for I := 0 to World.Count - 1 do
    begin
      if TPopulation (World.List [I]).Count = 0 then Continue;
      WriteText ('Collect.txt', Format ('%d:', [TPerson (TPopulation (World.List [I]).First).Genome [1]]) + #9 + Format ('%d', [TPerson (TPopulation (World.List [I]).First).Cost]));
      if TPerson (TPopulation (World.List [I]).First).Genome [1] < 3 then
      begin
        WriteText ('Collect.txt', Cr);
        Continue;
      end else if (Levels.Count > 0) and (TPerson (TPopulation (World.List [I]).First).Cost > TPerson (Levels.Last).Cost) then
      begin
        WriteText ('Collect.txt', Cr);
        Continue;
      end else
      begin
        WriteText ('Collect.txt', ' +' + Cr);
        Levels.Add (TPopulation (World.List [I]).First);
      end;
    end;

    if Levels.Count > 0 then
    begin
      Cfg.Selector ('\m1');
      Cfg.CurrentSection.Values [Ext + '.Size'] := IntToStr (TPerson (Levels.First).Cost);
      Cfg.PutData (Ext, TPerson (Levels.First).Genome, SizeOf (TPerson (Levels.First).Genome));
      Cfg.Selector ('\m2');
      Cfg.CurrentSection.Values [Ext + '.Size'] := IntToStr (TPerson (Levels.List [Levels.Count div 2]).Cost);
      Cfg.PutData (Ext, TPerson (Levels.List [Levels.Count div 2]).Genome, SizeOf (TPerson (Levels.First).Genome));
      Cfg.Selector ('\m3');
      Cfg.CurrentSection.Values [Ext + '.Size'] := IntToStr (TPerson (Levels.Last).Cost);
      Cfg.PutData (Ext, TPerson (Levels.Last).Genome, SizeOf (TPerson (Levels.First).Genome));
    end;
  end;

  procedure  TApp.CollectWorlds (const Path: string);
  var
    World: TPopulations;
    T: TSearchRec;
    Err: integer;
  begin
    Err := FindFirst (Path + '*.dat', faAnyFile, T);
    while Err = 0 do
    begin
      if (T.Name <> '.') and (T.Name <> '..') and ((T.Attr and faDirectory) = 0) then
      begin
        World := TPopulations.Create (Path + T.Name);
        ExtractLevels (World, '.' + ChangeFileExt (ExtractFileName (T.Name), ''));
        World.Free;
      end;
      Err := FindNext (T);
    end;
    FindClose (T);

    Err := SysUtils.FindFirst (Path + '*.*', faAnyFile, T);
    while Err = 0 do
    begin
      if (T.Name <> '.') and (T.Name <> '..') and ((T.Attr and faDirectory) = faDirectory) then
      begin
        CollectWorlds (Path + T.Name + '\');
      end;
      Err := FindNext (T);
    end;
    FindClose (T);
  end;

  procedure  TApp.CollectConfigurations (CfgName: string);
  begin
    if CfgName = '' then CfgName := DefaultCfgName;

    if ExtractFileExt (CfgName) = '' then CfgName := CfgName + '.ini';

    if FileExists (SelfPath + CfgName) then Cfg.LoadFromFile (SelfPath + CfgName);

    CollectWorlds ('');
    /// Reduce Cfg if need ...
    if MainForm.NeedToReduceIni then
    begin
      Cfg.Selector ('\m1'); ReduceSection (Cfg.CurrentSection);
      Cfg.Selector ('\m2'); ReduceSection (Cfg.CurrentSection);
      Cfg.Selector ('\m3'); ReduceSection (Cfg.CurrentSection);
    end;

    Cfg.SaveToFile (CfgName);
  end;

  procedure  TApp.MergeDataRecursively (const Path, Name: string; World: TPopulations);
  var
    TmpWorld: TPopulations;
    T: TSearchRec;
    I: integer;
  begin
    if FindFirst (Path + Name, faAnyFile - faDirectory, T) = 0 then
      repeat
        if (T.Name <> '.') and (T.Name <> '..') then
        begin
          TmpWorld := TPopulations.Create (Path + T.Name);
          World.CurrentAge := Max (0, Min (World.CurrentAge, TmpWorld.CurrentAge - 2000));
          TmpWorld.MarkToRecalculate;
          for I := 0 to TmpWorld.Count - 1 do
            if TPopulation (TmpWorld.List [I]).Count > 0 then
            begin
              TPopulation (World.List [I]).Add (TPopulation (TmpWorld.List [I]).First);
              TPopulation (TmpWorld.List [I]).Delete (0);
            end;
          TmpWorld.Free;
        end;
      until FindNext (T) <> 0;
    FindClose (T);

    if FindFirst (Path + '*.*', faDirectory, T) = 0 then
      repeat
        if (T.Name <> '.') and (T.Name <> '..') then
          MergeDataRecursively (Path + T.Name + '\', Name, World);
      until FindNext (T) = 0;

    FindClose (T);
  end;

  procedure TApp.Evolution;
  begin
    SetPriority (Priority);
    /// Collect configuration, if needed ...
    if MainForm.NeedToCollectConfig then
      CollectConfigurations (ConfigurationName);

    if MainForm.NeedToRecalculate then begin
      World.Free;
      World := TPopulations.Create (SrcName + '.dat');
      World.MarkToRecalculate;
      World.Save (SrcName + '.dat');
      MainForm.StatusBar.Panels.Items [0].Text := 'Parameters will be re-estimated at next run...';
      Exit;
    end;

    if MainForm.NeedToMerge then begin
      World.Free;
      World := TPopulations.Create;
      World.CurrentAge := 12000;

      MergeDataRecursively ('.\', SrcName + '.dat', World);

      World.CurrentAge := (World.CurrentAge div 2000) * 2000 + 1;

      World.Save (SrcName + '.dat');

      MainForm.StatusBar.Panels.Items [0].Text := 'All "' + SrcName + '.dat" merged. Run again to continue...';
      Exit;
    end;

    if MainForm.NeedToRun = False then begin MainForm.StatusBar.Panels.Items [0].Text := 'Configuration file was maked.'; Exit; end;
    if SrcName = '' then begin MainForm.StatusBar.Panels.Items [0].Text := 'Folder is not selected.'; Exit; end;
    if Headers.Count = 0 then begin MainForm.StatusBar.Panels.Items [0].Text := 'No Files for Optimization.'; Exit; end;
    MainForm.StatusBar.Panels.Items [0].Text := 'Optimization...';

    World.Free;
    World := TPopulations.Create (SrcName + '.dat');

    repeat
      DrawLevelProgress;
      World.Live;
      if Not MainForm.NeedToClose then World.Save (SrcName + '.dat');
    until MainForm.NeedToClose;
  end;

  procedure  TApp.DrawLevelProgress;
  begin
    MainForm.Label_variantsEstimatedValue.Caption := Format ('%d', [World.CurrentAge]);
    MainForm.Label_PercentOfImprovementsValue.Caption := Format ('%1.3f%%', [World.Improvements / (World.CurrentAge + 1) * 100]);
    MainForm.Label_LevelValue.Caption := Format ('%d', [World.CurrentPopulation + 1]);

    if TPopulation (World.List [World.CurrentPopulation]).Count > 0 then
      MainForm.Label_PackedSizeValue.Caption := Format ('%d', [TPerson (TPopulation (World.List [World.CurrentPopulation]).First).Cost])
    else
      MainForm.Label_PackedSizeValue.Caption := '?';
  end;

  procedure TApp.ReduceSection (Section: TConfigSection);
  var
    I: Integer;
  begin
    I := 0;
    while I < Section.Count do
      if Pos ('.Size', Cfg.CurrentSection.Names [I]) > 0 then
        Section.Delete (I)
      else
        Inc (I);
  end;

  /// TBody...

  constructor TBody.Create;
  var
    F: TFileStream;
    Readed: integer;
  begin
    F := TFileStream.Create (aFileName, fmOpenRead + fmShareDenyWrite);
    Size := F.Size;
    GetMem (Data, Size);
    F.Read(Data^, Size);
    F.Free;
  end;

  destructor  TBody.Destroy;
  begin
    FreeMem (Data);
  end;

  // TForm1

  procedure TMainForm.FormCreate (Sender: TObject);
  begin
    NeedToRecalculate := False;
    NeedToReduceIni := False;
    NeedToClose := False;
    NeedToRun := True;
    NeedToHide := False;

    App := TApp.Create;
  end;

  procedure TMainForm.FormClose (Sender: TObject; var Action: TCloseAction);
  begin
    NeedToClose := True;
    MainForm.StatusBar.Panels.Items [0].Text := 'Saving...';
  end;

  procedure TMainForm.FormDestroy (Sender: TObject);
  begin
    if App <> Nil then App.Free;
  end;

initialization

  {$I BeeOpt_Form.lrs}

end.

