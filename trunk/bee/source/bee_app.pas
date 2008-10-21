{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

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

  Bee archiver shell.

  Modifyed:

  v0.7.8 build 0150 - 2005.06.27 by Melchiorre Caruso;
  v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
  v0.7.8 build 0154 - 2005.07.23 by Melchiorre Caruso;
  v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;
  v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
  v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;

  v0.7.9 build 0896 - 2008.10.21 by Melchiorre Caruso.
}

unit Bee_App;

{$I compiler.inc}

interface

uses
  Classes, // TStringList, ...
  // ---
  Bee_Files,
  Bee_Headers,
  Bee_Interface,
  Bee_CommandLine,
  Bee_Configuration; // TConfiguration, TTable;

type

  // TBeeApp class

  TBeeApp = class(TApp)
  public
    constructor Create(aInterface: TInterfaces; aParams: TParams);
    destructor Destroy; override;
    procedure Execute; override;
  private
    function OpenArchive(Headers: THeaders; aAction: THeaderAction): boolean;
    // decode solid sequences using a swapfile
    function ProcessFilesToSwap(Headers: THeaders): boolean;
    // find and prepare sequences
    procedure ProcessFilesToFresh(Headers: THeaders);
    procedure ProcessFilesToDelete(Headers: THeaders);
    procedure ProcessFilesToDecode(Headers: THeaders; aAction: THeaderAction);
    procedure ProcessFilesToExtract(Headers: THeaders);
    procedure ProcessFilesToOverWrite(Headers: THeaders);
    function ProcessFilesToRename(Headers: THeaders): boolean;
    procedure ProcessFilesDeleted(Headers: THeaders);
    // overwrite sub-routines
    procedure ProcessFilesToOverWriteDefault(Headers: THeaders);
    procedure ProcessFilesToOverWriteAdvanced(Headers: THeaders);
    procedure ProcessFileToOverWrite(Headers: THeaders; FileIndex: integer);

    function AlreadyFileExists(Headers: THeaders; FileIndex: integer;
      FileActions: THeaderActions; const FileName: string): integer;

    procedure ProcesstOption;
    procedure ProcesslOption;

    procedure DisplayUsage;
    procedure EncodeShell;
    procedure DecodeShell(Action: THeaderAction);
    procedure RenameShell;
    procedure DeleteShell;
    procedure ListShell;

    procedure SetPriority(aPriority: integer);
  protected
    // string routines
    function VersionToStr(VersionId: cardinal): string;
    function MethodToStr(P: THeader; Method, Dictionary: integer): string;

  private
    FSelfName: string;
    FArcFile:  TFileReader; // archive file stream
    FSwapFile: TFileReader; // swap file stream
    FSwapName: string;      // swap file name

    FConfiguration: TConfiguration;
    FCommandLine:   TCommandLine;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Math,
  Windows,
  {$ENDIF}
  SysUtils, // faReadOnly, ...

  Bee_Common, // Various helper routines
  Bee_Assembler,
  Bee_MainPacker; // TEncoder...

// TBeeApp ...

constructor TBeeApp.Create(aInterface: TInterfaces; aParams: TParams);
begin
  inherited Create(aInterface);
  Randomize; // randomize, uses for unique filename generation...

  FSelfName :=
    'The Bee 0.7.9 build 0896 archiver utility, freeware version, Oct 2008.'
    + Cr + '(C) 1999-2008 Andrew Filinsky and Melchiorre Caruso.';

  FArcFile  := nil;
  FSwapName := '';
  FSwapFile := nil;

  // process command line
  FCommandLine := TCommandLine.Create;
  FCommandLine.Process(aParams);

  // load configuration
  FConfiguration := TConfiguration.Create;
  if not FileExists(FCommandLine.cfgOption) then
  begin
    Interfaces.OnWarning.Data.Msg :=
      (Cr + 'Configuration file ' + FCommandLine.cfgOption +
      ' not found, using default settings' + Cr);
    Synchronize(Interfaces.OnWarning.Method);
    SetExitCode(1);
  end
  else
    FConfiguration.LoadFromFile(FCommandLine.cfgOption);

  // load method and dictionary level
  FConfiguration.Selector('\main');
  FConfiguration.CurrentSection.Values['Method']     :=
    IntToStr(FCommandLine.mOption);
  FConfiguration.CurrentSection.Values['Dictionary'] :=
    IntToStr(FCommandLine.dOption);

  // set thread priority
  SetPriority(FCommandLine.priOption);
end;

destructor TBeeApp.Destroy;
begin
  FConfiguration.Destroy;
  FCommandLine.Destroy;
  inherited Destroy;
end;

procedure TBeeApp.DisplayUsage;
begin
  Interfaces.OnDisplay.Data.Msg :=
    (Cr + '  Usage: Bee <Command> -<Option 1> -<Option N> <ArchiveName> <FileNames...>');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := (Cr + '  Commands:' + Cr);
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    a   Add files to archive');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    d   Delete files from archive');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    e   Extract files from archive');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    ('    x   eXtract files from archive with path name');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    l   List archive');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    t   Test archive files');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    r   Rename files in archive');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := (Cr + '  Options:' + Cr);
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    r       Recurse subdirectories');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    u       Update files');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    f       Freshen files');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    e       force file Extention');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    s       create Solid archive');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    a       add self-extrActor module');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    ('    o<mode> set overwrite file Mode (Q-Quit, A-All, S-Skip all)');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    ('    m<0..3> set compression Method (0-store...1-default...3-maximal)');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    ('    d<0..9> set Dictionary size (d1 uses < 5M, d2 (default) < 10M, d3 < 20M...)'
    + Cr);
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    x       eXclude filenames');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    t       Test archive after process');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    l       List archive after process');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg := ('    y       set temporany directory');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    ('    k       use blowfish crypter/decrypter (min key-length 4 bytes)');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    ('    cd<dir> set current archive directory' + Cr);
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    ('    cfg<filename> use specified configuration file');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    ('    pri<priority> set process Priority (0-Idle, 1-Normal, 2-High, 3-RealTime)');
  Synchronize(Interfaces.OnDisplay.Method);
  Interfaces.OnDisplay.Data.Msg :=
    (Cr + '  Use BeeOpt to make most optimal parameters.');
  Synchronize(Interfaces.OnDisplay.Method);
end;

procedure TBeeApp.Execute;
const
  SetOfCommands = ['A', 'D', 'E', 'L', 'R', 'T', 'X'];
begin
  Interfaces.OnDisplay.Data.Msg := FSelfName;
  Synchronize(Interfaces.OnDisplay.Method);
  with FCommandLine do
  begin
    if ((Command in SetOfCommands) and (ArchiveName > '')) or
      (Command = '?') then
      case Command of
        'A': EncodeShell;
        'D': DeleteShell;
        'E': DecodeShell(toExtract);
        'L': ListShell;
        'R': RenameShell;
        'T': DecodeShell(toTest);
        'X': DecodeShell(toExtract);
        '?': DisplayUsage;
      end
    else
      DisplayUsage;
  end;
end;

procedure TBeeApp.SetPriority(aPriority: integer); // Priority is 0..3
begin
  {$IFDEF CONSOLEAPPLICATION}
    {$IFDEF MSWINDOWS}
  Bee_Common.SetPriority(aPriority);
    {$ELSE}
  case aPriority of
    0: Priority := tpIdle;
    1: Priority := tpNormal;
    2: Priority := tpHigher;
    3: Priority := tpTimeCritical;
    else
      Priority := tpNormal;
  end;
    {$ENDIF}
  {$ELSE}
  case aPriority of
    0: Priority := tpIdle;
    1: Priority := tpNormal;
    2: Priority := tpHigher;
    3: Priority := tpTimeCritical;
    else
      Priority := tpNormal;
  end;
  {$ENDIF}
end;

function TBeeApp.OpenArchive(Headers: THeaders;
  aAction: THeaderAction): boolean;
begin
  Result := True;
  if FileExists(FCommandLine.ArchiveName) then
  begin
    FArcFile := CreateTFileReader(FCommandLine.ArchiveName,
      fmOpenRead + fmShareDenyWrite);
    try
      Headers.ReadItems(FArcFile, aAction);
      if (Headers.Count = 0) and (FArcFile.Size <> 0) then
      begin
        Result := False;
        Interfaces.OnFatalError.Data.Msg := ('Error: can''t open archive');
        Synchronize(Interfaces.OnFatalError.Method);
        SetExitCode(2);
      end;
    except
      Result := False;
      Interfaces.OnFatalError.Data.Msg := ('Error: can''t open archive');
      Synchronize(Interfaces.OnFatalError.Method);
      SetExitCode(2);
    end;
  end;
end;

// -------------------------------------------------------------------------- //
// Extract file processing                                                    //
// -------------------------------------------------------------------------- //

procedure TBeeApp.ProcessFilesToExtract;
var
  I: integer;
begin
  if FCommandLine.Command = 'E' then
  begin
    for I := 0 to Headers.Count - 1 do
      with THeader(Headers.Items[I]).Data do
      begin
        FileName := ExtractFileName(FileName);
      end;
  end
  else
  if Length(FCommandLine.cdOption) > 0 then
  begin
    for I := 0 to Headers.Count - 1 do
      with THeader(Headers.Items[I]).Data do
      begin
        FileName := DeleteFilePath(FCommandLine.cdOption, FileName);
      end;
  end;
end;

// -------------------------------------------------------------------------- //
// OvewWrite file processing                                                  //
// -------------------------------------------------------------------------- //

procedure TBeeApp.ProcessFilesToOverWrite;
begin
  if (FCommandLine.uOption = False) and (FCommandLine.fOption = False) then
    ProcessFilesToOverWriteDefault(Headers)
  else
    ProcessFilesToOverWriteAdvanced(Headers);
end;

procedure TBeeApp.ProcessFilesToOverWriteDefault(Headers: THeaders);
var
  I, J: integer;
  NewFileName: string;
begin
  I := 0;
  while I < Headers.Count do
  begin
    if (THeader(Headers.Items[I]).Action = toExtract) and
      (FileExists(THeader(Headers.Items[I]).Data.FileName) = True) then
    begin
      if (FCommandLine.oOption in ['A', 'Q', 'S']) = False then
      begin
        repeat
          Interfaces.OnOverWrite.Data.FileName :=
            ExtractFileName(THeader(Headers.Items[I]).Data.FileName);

          Interfaces.OnOverWrite.Data.FilePath :=
            ExtractFilePath(THeader(Headers.Items[I]).Data.FileName);

          Interfaces.OnOverWrite.Data.FileSize :=
            THeader(Headers.Items[I]).Data.FileSize;

          Interfaces.OnOverWrite.Data.FileTime :=
            THeader(Headers.Items[I]).Data.FileTime;

          Interfaces.OnOverWrite.Answer := 'A';
          Synchronize(Interfaces.OnOverWrite.Method);
        until (Length(Interfaces.OnOverWrite.Answer) = 1) and
          (UpCase(Interfaces.OnOverWrite.Answer[1]) in
            ['A', 'N', 'R', 'S', 'Q', 'Y']);

        FCommandLine.oOption := UpCase(Interfaces.OnOverWrite.Answer[1]);
      end;

      case UpCase(FCommandLine.oOption) of
        'A': Break;
        'N': THeader(Headers.Items[I]).Action := toNone;
        'R':
        begin
          while True do
          begin
            Interfaces.OnRename.Data.FileName :=
              ExtractFileName(THeader(Headers.Items[I]).Data.FileName);

            Interfaces.OnRename.Data.FilePath :=
              ExtractFilePath(THeader(Headers.Items[I]).Data.FileName);

            Interfaces.OnRename.Data.FileSize :=
              THeader(Headers.Items[I]).Data.FileSize;

            Interfaces.OnRename.Data.FileTime :=
              THeader(Headers.Items[I]).Data.FileTime;

            Interfaces.OnRename.Data.FileAttr :=
              THeader(Headers.Items[I]).Data.FileAttr;

            Interfaces.OnRename.Answer := '';
            Synchronize(Interfaces.OnRename.Method);

            NewFileName := FixFileName(Interfaces.OnRename.Answer);

            if (FileExists(NewFileName) = True) or
              (AlreadyFileExists(Headers, I, [toExtract],
              NewFileName) <> -1) then
            begin
              Interfaces.OnWarning.Data.Msg :=
                ('Warning: file "' + NewFileName + '" already exists.');
              Synchronize(Interfaces.OnDisplay.Method);
            end
            else
              Break;
          end;

          if Length(NewFileName) = 0 then
            THeader(Headers.Items[I]).Action := toNone
          else
            THeader(Headers.Items[I]).Data.FileName := NewFileName;
        end;
        'S': for J := I to Headers.Count - 1 do
          begin
            THeader(Headers.Items[J]).Action := toNone;
            I := J;
          end;
        'Q': for J := 0 to Headers.Count - 1 do
          begin
            THeader(Headers.Items[J]).Action := toNone;
            I := J;
          end;
      end;
    end;
    Inc(I);
  end;

  for I := 0 to Headers.Count - 1 do
  begin
    if (THeader(Headers.Items[I]).Action = toExtract) then
    begin
      J := Headers.GetBack(I - 1, toExtract,
        THeader(Headers.Items[I]).Data.FileName);

      if J > -1 then
      begin
        THeader(Headers.Items[J]).Action := toNone;
      end;
    end;
  end;
end;

procedure TBeeApp.ProcessFilesToOverWriteAdvanced(Headers: THeaders);
var
  I: integer;
begin
  if (FCommandLine.uOption xor FCommandLine.fOption) then
  begin

    if FCommandLine.uOption then
    begin
      for I := 0 to Headers.Count - 1 do
        if (THeader(Headers.Items[I]).Action = toExtract) then
        begin
          if (FileExists(THeader(Headers.Items[I]).Data.FileName) = True) then
            THeader(Headers.Items[I]).Action := toNone
          else
            ProcessFileToOverWrite(Headers, I);
        end;
    end
    else
      for I := 0 to Headers.Count - 1 do
        if (THeader(Headers.Items[I]).Action = toExtract) then
        begin
          if (FileExists(THeader(Headers.Items[I]).Data.FileName) = False) then
            THeader(Headers.Items[I]).Action := toNone
          else
          begin
            if FileAge(THeader(Headers.Items[I]).Data.FileName) >=
              THeader(Headers.Items[I]).Data.FileTime then
              THeader(Headers.Items[I]).Action := toNone
            else
              ProcessFileToOverWrite(Headers, I);
          end;
        end;

  end
  else

  begin
    for I := 0 to Headers.Count - 1 do
      if FileExists(THeader(Headers.Items[I]).Data.FileName) = True then
      begin
        if FileAge(THeader(Headers.Items[I]).Data.FileName) >=
          THeader(Headers.Items[I]).Data.FileTime then
          THeader(Headers.Items[I]).Action := toNone
        else
          ProcessFileToOverWrite(Headers, I);
      end
      else
        ProcessFileToOverWrite(Headers, I);
  end;
end;

function TBeeApp.AlreadyFileExists(Headers: THeaders;
  FileIndex: integer; FileActions: THeaderActions;
  const FileName: string): integer;
begin
  if Length(FileName) > 0 then
  begin
    Result := Headers.GetBack(FileIndex - 1, FileActions, FileName);
    if Result = -1 then
    begin
      Result := Headers.GetNext(FileIndex + 1, FileActions, FileName);
    end;
  end
  else
    Result := -1;
end;

procedure TBeeApp.ProcessFileToOverWrite(Headers: THeaders;
  FileIndex: integer);
var
  J: integer;
begin
  J := Headers.GetBack(FileIndex - 1, toExtract,
    THeader(Headers.Items[FileIndex]).Data.FileName);

  if J > -1 then
  begin
    if (THeader(Headers.Items[FileIndex]).Data.FileTime >
      THeader(Headers.Items[J]).Data.FileTime) then
      THeader(Headers.Items[J]).Action := toNone
    else
      THeader(Headers.Items[FileIndex]).Action := toNone;
  end;
end;

// -------------------------------------------------------------------------- //
// Rename file processing                                                     //
// -------------------------------------------------------------------------- //

function TBeeApp.ProcessFilesToRename(Headers: THeaders): boolean;
var
  i: integer;
  iFileName: string;
begin
  Headers.MarkItems(FCommandLine.FileMasks, toCopy, toRename,
    FCommandLine.rOption);
  Headers.MarkItems(FCommandLine.xOption, toRename, toCopy,
    FCommandLine.rOption);

  if (Headers.GetNext(0, toRename) > -1) then
  begin
    for i := 0 to Headers.Count - 1 do
    begin
      if (THeader(Headers.Items[i]).Action = toRename) then
      begin
        while True do
        begin
          Interfaces.OnRename.Data.FileName :=
            ExtractFileName(THeader(Headers.Items[i]).Data.FileName);

          Interfaces.OnRename.Data.FilePath :=
            ExtractFilePath(THeader(Headers.Items[i]).Data.FileName);

          Interfaces.OnRename.Data.FileSize :=
            THeader(Headers.Items[i]).Data.FileSize;

          Interfaces.OnRename.Data.FileTime :=
            THeader(Headers.Items[i]).Data.FileTime;

          Interfaces.OnRename.Data.FileAttr :=
            THeader(Headers.Items[i]).Data.FileAttr;

          SetLength(Interfaces.OnRename.Answer, 0);
          Synchronize(Interfaces.OnRename.Method);

          iFileName := FixFileName(Interfaces.OnRename.Answer);
          if (AlreadyFileExists(Headers, I, [toCopy, toRename],
            iFileName) <> -1) then
          begin
            Interfaces.OnWarning.Data.Msg :=
              ('Warning: file "' + iFileName +
              '" already existing in archive.');
            Synchronize(Interfaces.OnWarning.Method);
          end
          else
            Break;
        end;

        if Length(iFileName) > 0 then
        begin
          THeader(Headers.Items[i]).Data.FileName := iFileName;
        end;
      end;
    end;
    Result := True;
  end
  else
    Result := ((Length(FCommandLine.aOption) > 0) and
      (Headers.GetNext(0, toCopy) > -1));
end;

// -------------------------------------------------------------------------- //
// Sequences processing                                                       //
// -------------------------------------------------------------------------- //

procedure TBeeApp.ProcessFilesToFresh;
var
  I, J, BackTear, NextTear: integer;
begin
  I := Headers.GetBack(Headers.Count - 1, toFresh);
  // find sequences and mark as toSwap files that not toFresh
  while I > -1 do
  begin
    BackTear := Headers.GetBack(I, foTear);
    NextTear := Headers.GetNext(I + 1, foTear);

    if NextTear = -1 then
      NextTear := Headers.Count;

    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := Headers.GetBack(NextTear - 1, toCopy);
      for J := BackTear to NextTear do
      begin
        case THeader(Headers.Items[J]).Action of
          toCopy:
          begin
            THeader(Headers.Items[J]).Action := toSwap;
            Inc(Interfaces.OnTick.Data.TotalSize,
              THeader(Headers.Items[J]).Data.FileSize * 2);
            // decoding  and Encoding size
          end;
          toFresh: Inc(Interfaces.OnTick.Data.TotalSize,
              THeader(Headers.Items[J]).Data.FileSize); // decoding size
        end;
      end;
      I := BackTear;
    end;
    I := Headers.GetBack(I - 1, toFresh);
  end;
  Inc(Interfaces.OnTick.Data.TotalSize,
    Headers.GetPackedSize(toCopy));
end;

procedure TBeeApp.ProcessFilesToDelete;
var
  I, J, BackTear, NextTear: integer;
begin
  I := Headers.GetBack(Headers.Count - 1, toDelete);
  // find sequences and ...
  while I > -1 do
  begin
    BackTear := Headers.GetBack(I, foTear);
    NextTear := Headers.GetNext(I + 1, foTear);

    if NextTear = -1 then
      NextTear := Headers.Count;

    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := Headers.GetBack(NextTear - 1, toCopy);
      // if exists an header toDelete
      if Headers.GetBack(NextTear, toDelete) > (BackTear - 1) then
      begin
        for J := BackTear to NextTear do
          case THeader(Headers.Items[J]).Action of
            toCopy:
            begin
              THeader(Headers.Items[J]).Action := toSwap;
              Inc(Interfaces.OnTick.Data.TotalSize,
                THeader(Headers.Items[J]).Data.FileSize * 2);
            end;
            toDelete: Inc(Interfaces.OnTick.Data.TotalSize,
                THeader(Headers.Items[J]).Data.FileSize);
          end;
      end;
      I := BackTear;
    end;
    I := Headers.GetBack(I - 1, toDelete);
  end;
  Inc(Interfaces.OnTick.Data.TotalSize,
    Headers.GetPackedSize(toCopy));
end;

function TBeeApp.ProcessFilesToSwap(Headers: THeaders): boolean;
var
  I, J:      integer;
  Decoder:   TDecoder;
  FSwapStrm: TFileWriter;
  iDictionary, iTable, iTear: integer;
  CurrDictionary, CurrTable: integer;
begin
  Result := True;

  I := Headers.GetBack(Headers.Count - 1, toSwap);
  if (I > -1) then
  begin
    FSwapName := GenerateFileName(FCommandLine.yOption);
    FSwapStrm := CreateTFileWriter(FSwapName, fmCreate);

    if (FSwapStrm <> nil) then
    begin
      CurrDictionary := Headers.Count;
      CurrTable      := Headers.Count;

      Decoder := TDecoder.Create(FArcFile, Interfaces, Synchronize);
      // get GeneralSize
      while (I > -1) and (Interfaces.Stop = False) do
      begin
        iDictionary := Headers.GetBack(I, foDictionary);
        // find dictionary info
        iTable := Headers.GetBack(I, foTable); // find table info
        iTear := Headers.GetBack(I, foTear);   // find tear info

        if (iDictionary > -1) and (iDictionary <> CurrDictionary) and
          (iDictionary <> iTear) then
        begin
          CurrDictionary := iDictionary;
          Decoder.DecodeStrm(THeader(Headers.Items[iDictionary]),
            pmQuit, FSwapStrm);
        end;

        if (iTable > -1) and (iTable <> CurrTable) and (iTable <> iTear) then
        begin
          CurrTable := iTable;
          Decoder.DecodeStrm(THeader(Headers.Items[iTable]),
            pmQuit, FSwapStrm);
        end;

        for J := iTear to I do
        begin
          if Interfaces.Stop = False then
          begin
            if THeader(Headers.Items[J]).Action = toSwap then
              Result := Decoder.DecodeStrm(Headers.Items[J], pmNorm, FSwapStrm)
            else
              Result := Decoder.DecodeStrm(Headers.Items[J],
                pmSkip, FSwapStrm);
          end
          else
            Result := False;

          if Result = False then
            Break;
        end;

        if Result = False then
          Break;
        I := Headers.GetBack(iTear - 1, toSwap);
      end;
      Decoder.Destroy;
      FreeAndNil(FSwapStrm);

    end
    else
      Result := False;
  end;
end;

procedure TBeeApp.ProcessFilesDeleted;
var
  I: integer;
begin
  // rescue header informations
  with Headers do
    for I := 0 to Count - 2 do
    begin
      if THeader(Items[I]).Action = toDelete then
      begin
        if (foVersion in THeader(Items[I]).Data.FileFlags) and
          (not (foVersion in THeader(Items[I + 1]).Data.FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).Data.FileFlags, foVersion);
          THeader(Items[I + 1]).Data.FileVersion :=
            THeader(Items[I]).Data.FileVersion;
        end;

        if (foMethod in THeader(Items[I]).Data.FileFlags) and
          (not (foMethod in THeader(Items[I + 1]).Data.FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).Data.FileFlags, foMethod);
          THeader(Items[I + 1]).Data.FileMethod :=
            THeader(Items[I]).Data.FileMethod;
        end;

        if (foDictionary in THeader(Items[I]).Data.FileFlags) and
          (not (foDictionary in THeader(Items[I + 1]).Data.FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).Data.FileFlags, foDictionary);
          THeader(Items[I + 1]).Data.FileDictionary :=
            THeader(Items[I]).Data.FileDictionary;
        end;

        if (foTable in THeader(Items[I]).Data.FileFlags) and
          (not (foTable in THeader(Items[I + 1]).Data.FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).Data.FileFlags, foTable);
          THeader(Items[I + 1]).Data.FileTable :=
            THeader(Items[I]).Data.FileTable;
        end;

        if (foTear in THeader(Items[I]).Data.FileFlags) and
          (not (foTear in THeader(Items[I + 1]).Data.FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).Data.FileFlags, foTear);
        end;
      end;
    end;
end;

procedure TBeeApp.ProcessFilesToDecode;
var
  I, J: integer;
  iDictionary, iTable, iTear: integer;
begin
  I := Headers.GetBack(Headers.Count - 1, aAction); // last header
  while I > -1 do
  begin
    iDictionary := Headers.GetBack(I, foDictionary); // find dictionary info
    iTable := Headers.GetBack(I, foTable); // find table info
    iTear := Headers.GetBack(I, foTear);   // find tear info

    for J := iTear to (I - 1) do
      if THeader(Headers.Items[J]).Action in [toNone, toQuit] then
      begin
        THeader(Headers.Items[J]).Action := toSkip;
        Inc(Interfaces.OnTick.Data.TotalSize,
          THeader(Headers.Items[J]).Data.FileSize);
      end;

    if (iDictionary > -1) and (THeader(Headers.Items[iDictionary]).Action =
      toNone) then
      THeader(Headers.Items[iDictionary]).Action := toQuit;

    if (iTable > -1) and (THeader(Headers.Items[iTable]).Action = toNone) then
      THeader(Headers.Items[iTable]).Action := toQuit;

    I := Headers.GetBack(iTear - 1, aAction);
  end;
end;

// -------------------------------------------------------------------------- //
// Option processing                                                          //
// -------------------------------------------------------------------------- //

procedure TBeeApp.ProcesstOption;
begin
  if FCommandLine.tOption then
  begin
    with Interfaces.OnTick.Data do
    begin
      TotalSize     := 0;
      ProcessedSize := 0;
    end;
    FCommandLine.rOption := True; // force recursion
    FCommandLine.xOption.Clear;   // clear xOption
    FCommandLine.FileMasks.Clear; // clear FileMasks
    FCommandLine.FileMasks.Add('*');
    DecodeShell(toTest);
  end;
end;

procedure TBeeApp.ProcesslOption;
begin
  if FCommandLine.lOption then
  begin
    with Interfaces.OnTick.Data do
    begin
      TotalSize     := 0;
      ProcessedSize := 0;
    end;
    FCommandLine.rOption := True; // force recursion
    FCommandLine.xOption.Clear;   // clear xOption
    FCommandLine.FileMasks.Clear; // clear FileMasks
    FCommandLine.FileMasks.Add('*');
    ListShell;
  end;
end;

// -------------------------------------------------------------------------- //
// Shell procedures                                                           //
// -------------------------------------------------------------------------- //

procedure TBeeApp.EncodeShell;
var
  I:    integer;
  Encoder: TEncoder;
  TmpFileName: string;
  TmpFile: TFileWriter;
  Headers: THeaders;
  Time: double;
begin
  Interfaces.OnDisplay.Data.Msg :=
    (Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);
  Synchronize(Interfaces.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);
    // process FileMasks and xFileMasks
    with Interfaces.OnTick.Data do
      Headers.AddItems(
        FCommandLine.FileMasks,
        FCommandLine.cdOption,
        FCommandLine.fOption,
        FCommandLine.rOption,
        FCommandLine.uOption,
        FCommandLine.xOption,
        TotalSize);

    if (Headers.GetCount([toUpdate, toFresh]) > 0) or
      ((Length(FCommandLine.aOption) > 0) and
      (Headers.GetNext(0, toCopy) > -1)) then
    begin
      Time    := Now;
      TmpFileName := GenerateFileName(FCommandLine.yOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);

      // find sequences and...
      ProcessFilesToFresh(Headers);

      // decode solid header modified in a swap file
      if (TmpFile <> nil) and ProcessFilesToSwap(Headers) then
      begin
        // sort headers (only toUpdate headers)
        with FCommandLine do
          Headers.SortNews(FConfiguration, sOption, kOption, eOption);

        // if exists a modified solid sequence open swap file
        if Length(FSwapName) > 0 then
          FSwapFile := CreateTFileReader(FSwapName, fmOpenRead +
            fmShareDenyWrite);

        // set sfx module
        with FCommandLine do
          if Length(aOption) > 0 then
            Headers.SetModule(aOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Interfaces, Synchronize);
        for I := 0 to Headers.Count - 1 do
          if Interfaces.Stop = False then
          begin
            case THeader(Headers.Items[I]).Action of
              toCopy: Encoder.CopyStrm(Headers.Items[I], emNorm, FArcFile);
              toSwap: Encoder.EncodeStrm(Headers.Items[I], emNorm, FSwapFile);
              toFresh: Encoder.EncodeFile(Headers.Items[I], emNorm);
              toUpdate: Encoder.EncodeFile(Headers.Items[I], emNorm);
            end;
          end;
        Encoder.Destroy;
        // rewrite Headers
        Headers.WriteItems(TmpFile);

        if Interfaces.Stop = False then
        begin
          Interfaces.OnDisplay.Data.Msg :=
            (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) +
            ' bytes - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnDisplay.Method);
        end
        else
        begin
          Interfaces.OnError.Data.Msg :=
            (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnError.Method);
          SetExitCode(255);
        end;

        if Assigned(FSwapFile) then
          FreeAndNil(FSwapFile);
        if Assigned(FArcFile) then
          FreeAndNil(FArcFile);
        if Assigned(TmpFile) then
          FreeAndNil(TmpFile);

        DeleteFile(FSwapName);
        if Interfaces.Stop = False then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
          begin
            Interfaces.OnError.Data.Msg :=
              ('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName);
            Synchronize(Interfaces.OnError.Method);
            SetExitCode(2);
          end
          else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption
          end;
        end
        else
          DeleteFile(TmpFileName);

      end
      else // if ProcessFilesToSwap
      begin
        if TmpFile = nil then
          Interfaces.OnError.Data.Msg := ('Error: can''t open temp file')
        else
          Interfaces.OnError.Data.Msg :=
            ('Error: can''t decode solid sequences');
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(2);

        if Assigned(FSwapFile) then
          FreeAndNil(FSwapFile);
        if Assigned(FArcFile) then
          FreeAndNil(FArcFile);
        if Assigned(TmpFile) then
          FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        SysUtils.DeleteFile(TmpFileName);
      end;

    end
    else // if Headers.GetCount
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to process');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;
  end;
  Headers.Free;

  if Assigned(FArcFile) then
    FreeAndNil(FArcFile);
end;

procedure TBeeApp.DecodeShell(Action: THeaderAction);
var
  Decoder: TDecoder;
  Headers: THeaders;
  Return: boolean;
  Time: double;
  I: integer;
begin
  Interfaces.OnDisplay.Data.Msg :=
    (Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);
  Synchronize(Interfaces.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toNone) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);

    with FCommandLine do
    begin
      Headers.MarkItems(FileMasks, toNone, Action, rOption);
      Headers.MarkItems(xOption, Action, toNone, rOption);
    end;

    if (Action = toExtract) then
    begin
      ProcessFilesToExtract(Headers);
      ProcessFilesToOverWrite(Headers);
    end;

    Interfaces.OnTick.Data.TotalSize := Headers.GetSize(Action);
    if (Headers.GetNext(0, Action) > -1) then // action = toTest or toExtract
    begin
      Time := Now;
      ProcessFilesToDecode(Headers, Action);

      Return  := True;
      Decoder := TDecoder.Create(FArcFile, Interfaces, Synchronize);
      for I := 0 to Headers.Count - 1 do
      begin
        if Interfaces.Stop = False then
          case THeader(Headers.Items[I]).Action of
            toExtract: Return := Decoder.DecodeFile(Headers.Items[I], pmNorm);
            toTest: Return    := Decoder.DecodeFile(Headers.Items[I], pmTest);
            toSkip: Return    := Decoder.DecodeFile(Headers.Items[I], pmSkip);
            toQuit: Return    := Decoder.Decodefile(Headers.Items[I], pmQuit);
          end;
        if Return = False then
          Break;
      end;
      Decoder.Destroy;

      if Interfaces.Stop = False then
      begin
        if Return = True then
        begin
          Interfaces.OnDisplay.Data.Msg :=
            (Cr + 'Everything went ok - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnDisplay.Method);
        end
        else
        begin
          Interfaces.OnError.Data.Msg :=
            (Cr + 'Process aborted, a fatal error occourred - ' +
            TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnError.Method);
          SetExitCode(2);
        end;
      end
      else
      begin
        Interfaces.OnError.Data.Msg :=
          (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(255);
      end;

    end
    else // if Headers.GetNext
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to decode');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;
  end;
  Headers.Free;

  if Assigned(FArcFile) then
    FreeAndNil(FArcFile);
end;

procedure TBeeApp.DeleteShell;
var
  TmpFileName: string;
  TmpFile: TFileWriter;
  I:    integer;
  Time: double;
  Headers: THeaders;
  Encoder: TEncoder;
begin
  Interfaces.OnDisplay.Data.Msg :=
    (Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);
  Synchronize(Interfaces.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);

    with FCommandLine do
    begin
      Headers.MarkItems(FileMasks, toCopy, toDelete, rOption);
      Headers.MarkItems(xOption, toDelete, toCopy, rOption);
    end;

    if (Headers.GetNext(0, toDelete) > -1) or
      ((Length(FCommandLine.aOption) > 0) and
      (Headers.GetNext(0, toCopy) > -1)) then
    begin
      Time    := Now;
      TmpFileName := GenerateFileName(FCommandLine.yOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);

      // find sequences
      ProcessFilesToDelete(Headers);
      if (TmpFile <> nil) and ProcessFilesToSwap(Headers) then
      begin
        // rescue headers information
        ProcessFilesDeleted(Headers);

        // if SwapSequences has found a modified sequence open Swap file
        if Length(FSwapName) > 0 then
          FSwapFile := CreateTFileReader(FSwapName, fmOpenRead +
            fmShareDenyWrite);

        // set sfx module
        with FCommandLine do
          if Length(aOption) > 0 then
            Headers.SetModule(aOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Interfaces, Synchronize);
        for I := 0 to Headers.Count - 1 do
          if Interfaces.Stop = False then
          begin
            case THeader(Headers.Items[I]).Action of
              toCopy: Encoder.CopyStrm(Headers.Items[I], emNorm, FArcFile);
              toSwap: Encoder.EncodeStrm(Headers.Items[I], emNorm, FSwapFile);
              toDelete:
              begin
                Interfaces.OnDisplay.Data.Msg :=
                  (msgDeleting + THeader(Headers.Items[I]).Data.FileName);
                Synchronize(Interfaces.OnDisplay.Method);
              end;
            end;
          end;
        Encoder.Destroy;
        Headers.WriteItems(TmpFile);

        if Interfaces.Stop = False then
        begin
          Interfaces.OnDisplay.Data.Msg :=
            (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) +
            ' bytes - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnDisplay.Method);
        end
        else
        begin
          Interfaces.OnError.Data.Msg :=
            (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnError.Method);
          SetExitCode(255);
        end;

        if Assigned(FSwapFile) then
          FreeAndNil(FSwapFile);
        if Assigned(FArcFile) then
          FreeAndNil(FArcFile);
        if Assigned(TmpFile) then
          FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        if Interfaces.Stop = False then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
          begin
            Interfaces.OnError.Data.Msg :=
              ('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName);
            Synchronize(Interfaces.OnError.Method);
            SetExitCode(2);
          end
          else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption  
          end;
        end
        else
          SysUtils.DeleteFile(TmpFileName);

      end
      else // if ProcessFilesToSwap
      begin
        if TmpFile = nil then
          Interfaces.OnError.Data.Msg := ('Error: can''t open temp file')
        else
          Interfaces.OnError.Data.Msg :=
            ('Error: can''t decode solid sequences');
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(2);

        if Assigned(FSwapFile) then
          FreeAndNil(FSwapFile);
        if Assigned(FArcFile) then
          FreeAndNil(FArcFile);
        if Assigned(TmpFile) then
          FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        SysUtils.DeleteFile(TmpFileName);
      end;

    end
    else // if Headers.GetNext
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to delete');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;
  end;
  Headers.Free;

  if Assigned(FArcFile) then
    FreeAndNil(FArcFile);
end;

procedure TBeeApp.RenameShell;
var
  TmpFile: TFileWriter;
  TmpFileName: string;
  Headers: THeaders;
  Encoder: TEncoder;
  Time: double;
  I:    integer;
begin
  Interfaces.OnDisplay.Data.Msg :=
    (Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);
  Synchronize(Interfaces.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);

    if ProcessFilesToRename(Headers) then
    begin
      Time    := Now;
      TmpFileName := GenerateFileName(FCommandLine.yOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);

      if (TmpFile <> nil) then
      begin
        Interfaces.OnTick.Data.TotalSize :=
          Headers.GetPackedSize([toCopy, toRename]);

        // set sfx module
        with FCommandLine do
          if Length(aOption) > 0 then
            Headers.SetModule(aOption);

        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Interfaces, Synchronize);
        for I := 0 to Headers.Count - 1 do
          if Interfaces.Stop = False then
          begin
            Encoder.CopyStrm(Headers.Items[I], emNorm, FArcFile);
          end;
        Encoder.Destroy;
        Headers.WriteItems(TmpFile);

        if Interfaces.Stop = False then
        begin
          Interfaces.OnDisplay.Data.Msg :=
            (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) +
            ' bytes - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnDisplay.Method);
        end
        else
        begin
          Interfaces.OnError.Data.Msg :=
            (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnError.Method);
          SetExitCode(255);
        end;

        if Assigned(FArcFile) then
          FreeAndNil(FArcFile);
        if Assigned(TmpFile) then
          FreeAndNil(TmpFile);

        if Interfaces.Stop = False then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
          begin
            Interfaces.OnError.Data.Msg :=
              ('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName);
            Synchronize(Interfaces.OnError.Method);
            SetExitCode(2);
          end
          else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption
          end;
        end
        else
          SysUtils.DeleteFile(TmpFileName);

      end
      else // if (TmpFile <> nil)
      begin
        if TmpFile = nil then
          Interfaces.OnError.Data.Msg := ('Error: can''t open temp file')
        else
          Interfaces.OnError.Data.Msg :=
            ('Error: can''t decode solid sequences');
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(2);

        if Assigned(FArcFile) then
          FreeAndNil(FArcFile);
        if Assigned(TmpFile) then
          FreeAndNil(TmpFile);

        SysUtils.DeleteFile(TmpFileName);
      end;

    end
    else // if ProcessFilesToRename
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to rename');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;
  end;
  Headers.Free;

  if Assigned(FArcFile) then
    FreeAndNil(FArcFile);
end;

procedure TBeeApp.ListShell;
var
  P:    THeader;
  I:    integer;
  Info: THeaders;
  Version, Method, Dictionary: integer;
  TotalPack, TotalSize: integer;
  CountFiles: integer;
  Time: double;
begin
  Interfaces.OnDisplay.Data.Msg :=
    (Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);
  Synchronize(Interfaces.OnDisplay.Method);

  Info := THeaders.Create;
  if OpenArchive(Info, toNone) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);

    Version    := -1;
    Method     := -1;
    Dictionary := -1;

    TotalSize  := 0;
    TotalPack  := 0;
    CountFiles := 0;

    with FCommandLine do
    begin
      Info.MarkItems(FileMasks, toNone, toList, rOption);
      Info.MarkItems(xOption, toList, toNone, rOption);
    end;

    if (Info.GetNext(0, toList) > -1) then
    begin
      Time := Now;

      {$IFDEF CONSOLEAPPLICATION}
      Interfaces.OnDisplay.Data.Msg := StringOfChar('-', 79);
      Synchronize(Interfaces.OnDisplay.Method);

      Interfaces.OnDisplay.Data.Msg :=
        ('Name' + StringOfChar(' ', 18) +
        'Size     Packed Ratio     Date  Time   Attr      CRC Meth');
      Synchronize(Interfaces.OnDisplay.Method);

      Interfaces.OnDisplay.Data.Msg := StringOfChar('-', 79);
      Synchronize(Interfaces.OnDisplay.Method);
      {$ENDIF}

      for I := 0 to Info.Count - 1 do
      begin
        if THeader(Info.Items[I]).Action = toList then
        begin
          P := Info.Items[I];

          if foVersion in P.Data.FileFlags then
            Version := P.Data.FileVersion;
          if foMethod in P.Data.FileFlags then
            Method := P.Data.FileMethod;
          if foDictionary in P.Data.FileFlags then
            Dictionary := P.Data.FileDictionary;

          with Interfaces.OnList.Data do
          begin
            FileName   := ExtractFileName(P.Data.FileName);
            FilePath   := ExtractFilePath(P.Data.FileName);
            FileSize   := P.Data.FileSize;
            FilePacked := P.Data.FilePacked;

            if FileSize > 0 then
              FileRatio := MulDiv(FilePacked, 100, FileSize)
            else
              FileRatio := 100;

            FileAttr    := P.Data.FileAttr;
            FileTime    := P.Data.FileTime;
            FileComm    := '';
            FileCrc     := P.Data.FileCrc;
            FileMethod  := MethodToStr(P, Method, Dictionary);
            FileVersion := VersionToStr(Version);

            if foPassword in P.Data.FileFlags then
              FilePassword := 'Yes'
            else
              FilePassword := 'No';

            FilePosition := I;
          end;
          Synchronize(Interfaces.OnList.Method);

          Inc(TotalSize, P.Data.FileSize);
          Inc(TotalPack, P.Data.FilePacked);
          Inc(CountFiles);
        end;
      end;
      {$IFDEF CONSOLEAPPLICATION}
      Interfaces.OnDisplay.Data.Msg := StringOfChar('-', 79);
      Synchronize(Interfaces.OnDisplay.Method);

      Interfaces.OnDisplay.Data.Msg :=
        (Format('%d files', [CountFiles])) +
        StringOfChar(' ', 15 - Length((Format('%d files', [CountFiles])))) +
        (Format(' %10s %10s %5s', [SizeToStr(TotalSize), SizeToStr(TotalPack),
        RatioToStr(TotalPack, TotalSize)]));
      Synchronize(Interfaces.OnDisplay.Method);
      {$ENDIF}

      {$IFDEF CONSOLEAPPLICATION}
      // self-extractor module size
      if Info.GetModule > 0 then
      begin
        Interfaces.OnDisplay.Data.Msg :=
          (Cr + 'Note: Bee Self-Extractor module founded');
        Synchronize(Interfaces.OnDisplay.Method);
      end;
      {$ENDIF}
      Interfaces.OnDisplay.Data.Msg :=
        (Cr + 'Everything went ok - ' + TimeDifference(Time) + ' seconds');
      Synchronize(Interfaces.OnDisplay.Method);
    end
    else
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to list');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;

  end;
  Info.Free;

  if Assigned(FArcFile) then
    FreeAndNil(FArcFile);
end;

// -------------------------------------------------------------------------- //
// String routines                                                            //
// -------------------------------------------------------------------------- //

function TBeeApp.MethodToStr(P: THeader; Method, Dictionary: integer): string;
begin
  Result := 'm0a';

  if not (foTear in P.Data.FileFlags) then
    Result[1] := 's';

  if not (foMoved in P.Data.FileFlags) then
  begin
    if Method in [1..3] then
      Result[2] := char(byte('0') + Method)
    else
      Result[2] := '?';
  end;

  if Dictionary in [0..9] then
    Result[3] := char(byte('a') + Dictionary)
  else
    Result[3] := '?';
end;

function TBeeApp.VersionToStr(VersionId: cardinal): string;
begin
  case VersionId of
    0: Result := ' 0' + DecimalSeparator + '2';
    1: Result := ' 0' + DecimalSeparator + '3';
    else
      Result := ' 0' + DecimalSeparator + '0';
  end;
end;

end.
