{
  Copyright (c) 2003-2007 Andrew Filinsky and Melchiorre Caruso

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

  v0.7.9 build 0848 - 2008.08.28 by Melchiorre Caruso.
}

unit Bee_App;

{$I compiler.inc}

interface

uses
  Classes,           // TStringList, ...
  // ---
  Bee_Files,
  Bee_Headers,
  Bee_Interface,
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

    procedure ProcessOption(var S: string; var Option: boolean);
    procedure ProcessOptions;
    procedure ProcessMasks;
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
    //
    function AlreadyFileExists(Headers: THeaders; FileIndex: integer;
      FileActions: THeaderActions; const FileName: string): integer;
    //
    procedure ProcesstOption;
    procedure ProcesslOption;
    //
    procedure DisplayUsage;
    procedure EncodeShell;
    procedure DecodeShell(Action: THeaderAction);
    procedure RenameShell;
    procedure DeleteShell;
    procedure ListShell;
    //
    procedure SetPriority(aPriority: integer);
  protected
    // string routines
    function VersionToStr(VersionId: cardinal): string;
    function MethodToStr(P: THeader; Method, Dictionary: integer): string;
    
  private
    FSelfName: string;

    FArcName:  string;      // archive file name
    FArcFile:  TFileReader; // archive file stream
    FSwapName: string;      // swap file name
    FSwapFile: TFileReader; // swap file stream

    FCfgName: string;
    FCfg: TConfiguration;

    FCommand: char;     // command
    FaOption: string;
    FcdOption: string;
    FeOption: string;   // forced file extension
    FfOption: boolean;
    FkOption: boolean;
    FlOption: boolean;
    FoOption: char;
    FrOption: boolean;
    FsOption: boolean;
    FtOption: boolean;
    FuOption: boolean;
    FxOption: TStringList;
    FyOption: string;

    FFileMasks: TStringList; // file masks
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
  inherited Create(aInterface, aParams);
  Randomize; // randomize, uses for unique filename generation...

  FSelfName := 'The Bee 0.7.9 build 0848 archiver utility, freeware version, Aug 2008.'
    + Cr + '(C) 1999-2008 Andrew Filinsky and Melchiorre Caruso.';

  FArcName  := '';
  FArcFile  := nil;
  FSwapName := '';
  FSwapFile := nil;
  
  FCfgName := SelfPath + 'bee.ini';
  FCfg := TConfiguration.Create;

  FCommand := ' ';
  FaOption := '';
  FcdOption := '';
  FeOption := ''; // forced file extension
  FfOption := False;
  FkOption := False;
  FlOption := False;
  FoOption := 'Y';
  FrOption := False;
  FsOption := False;
  FtOption := False;
  FuOption := False;
  FxOption := TStringList.Create;
  FyOption := '';

  FFileMasks := TStringList.Create;

  ProcessOptions; // process options
  ProcessMasks; // porcess masks
end;

destructor TBeeApp.Destroy;
begin
  FCfg.Free;
  FxOption.Free;
  FFileMasks.Free;
  inherited Destroy;
end;

procedure TBeeApp.DisplayUsage;
begin
                                            Interfaces.OnDisplay.Data.Msg := (Cr + '  Usage: Bee <Command> -<Option 1> -<Option N> <ArchiveName> <FileNames...>');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := (Cr + '  Commands:' + Cr);
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    a   Add files to archive');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    d   Delete files from archive');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    e   Extract files from archive');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    x   eXtract files from archive with path name');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    l   List archive');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    t   Test archive files');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    r   Rename files in archive');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := (Cr + '  Options:' + Cr);
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    r       Recurse subdirectories');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    u       Update files');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    f       Freshen files');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    e       force file Extention');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    s       create Solid archive');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    a       add self-extrActor module');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    o<mode> set overwrite file Mode (Q-Quit, A-All, S-Skip all)');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    m<0..3> set compression Method (0-store...1-default...3-maximal)');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    d<0..9> set Dictionary size (d1 uses < 5M, d2 (default) < 10M, d3 < 20M...)' + Cr);
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    x       eXclude filenames');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    t       Test archive after process');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    l       List archive after process');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    y       set temporany directory');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    k       use blowfish crypter/decrypter (min key-length 4 bytes)');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    cd<dir> set current archive directory' + Cr);
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    cfg<filename> use specified configuration file');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := ('    pri<priority> set process Priority (0-Idle, 1-Normal, 2-High, 3-RealTime)');
  Synchronize(Interfaces.OnDisplay.Method); Interfaces.OnDisplay.Data.Msg := (Cr + '  Use BeeOpt to make most optimal parameters.');
  Synchronize(Interfaces.OnDisplay.Method);
end;

procedure TBeeApp.Execute;
const
  SetOfCommands = ['A', 'D', 'E', 'L', 'R', 'T', 'X'];
begin
  // inherited Execute;
  Interfaces.OnDisplay.Data.Msg := FSelfName;
  Synchronize(Interfaces.OnDisplay.Method);
  /// process command
  if ((FCommand in SetOfCommands) and (FArcName > '')) or (FCommand = '?') then
    case FCommand of
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

function TBeeApp.OpenArchive(Headers: THeaders; aAction: THeaderAction): boolean;
begin
  Result := True;
  if FileExists(FArcName) then
  begin
    try
      FArcFile := TFileReader.Create(FArcName, fmOpenRead + fmShareDenyWrite);
      Headers.ReadItems(FArcFile, aAction);
      if (Headers.Count = 0) and (FArcFile.Size <> 0) then
      begin
        Interfaces.OnFatalError.Data.Msg := ('Error: can''t open archive');
        Synchronize(Interfaces.OnFatalError.Method);
        SetExitCode(2);
        Result := False;
      end;
    except
      Interfaces.OnFatalError.Data.Msg := ('Error: can''t open archive');
      Synchronize(Interfaces.OnFatalError.Method);
      SetExitCode(2);
      Result := False;
    end;
  end;
end;

// Options processing

procedure TBeeApp.ProcessOption(var S: string; var Option: boolean);
begin
  if Length(S) > 1 then
  begin
    Delete(S, 1, 2);
    if (S = '') or (S = '+') then
      Option := True
    else
      if (S = '-') then
        Option := False;
  end;
end;

procedure TBeeApp.ProcessOptions;
var
  I: integer;
  S: string;
begin
  // default configuration
  FCfg.Selector('\main');
  FCfg.CurrentSection.Values['Method'] := '1';
  FCfg.CurrentSection.Values['Dictionary'] := '2';

  // catch options, command, archive name and name of files
  for I := 0 to Params.Count - 1 do
  begin
    S := Params.Strings[I];
    if (FArcName = '') and (Length(S) > 1) and (S[1] = '-') then
    begin
      // options...
      case UpCase(S[2]) of
        'S': ProcessOption(S, FsOption);
        'U': ProcessOption(S, FuOption);
        'F': ProcessOption(S, FfOption);
        'T': ProcessOption(S, FtOption);
        'L': ProcessOption(S, FlOption);
        'K': ProcessOption(S, FkOption);
        'R': ProcessOption(S, FrOption);
        'Y': begin
               Delete(S, 1, 2);
               if DirectoryExists(ExcludeTrailingBackslash(S)) then
               begin
                 FyOption := ExcludeTrailingBackslash(S);
               end;
             end;
        'A': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 FaOption := 'bee.sfx'
               else
                 if (S = '-') then
                   FaOption := 'nul'
                 else
                   FaOption := S;
             end;
        'M': begin
               Delete(S, 1, 2);
               if (Length(S)= 1) and (S[1] in ['0'..'3']) then
               begin
                 FCfg.Selector('\main');
                 FCfg.CurrentSection.Values['Method'] := S;
               end;
             end;
        'O': begin
               Delete(S, 1, 2);
               if (Length(S) = 1) and (UpCase(S[1]) in ['A', 'S', 'Q']) then
               begin
                 FoOption := UpCase(S[1]);
               end;
             end;
        'D': begin
               Delete(S, 1, 2);
               if (Length(S)= 1) and (S[1] in ['0'..'9']) then
               begin
                 FCfg.Selector('\main');
                 FCfg.CurrentSection.Values['Dictionary'] := S;
               end;
             end;
        'E': begin
               Delete(S, 1, 2);
               if ExtractFileExt('.' + S) <> '.' then
               begin
                 FeOption := ExtractFileExt('.' + S);
               end;
             end;
        'X': begin
               Delete(S, 1, 2);
               if Length(S) > 0 then
               begin
                 FxOption.Add(S);
               end;
             end;
        else if Pos('-PRI', UpperCase(S)) = 1 then
             begin
               Delete(S, 1, 4);
               if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
               begin
                 SetPriority(StrToInt(S[1]));
               end;
             end else
             begin
               if Pos('-CD', UpperCase(S)) = 1 then
               begin
                 Delete(S, 1, 3);
                 if Length(S) > 0 then
                 begin
                   FcdOption := IncludeTrailingBackslash(FixDirName(S));
                 end;
               end else
               begin
                 if Pos('-CFG', UpperCase(S)) = 1 then
                 begin
                   Delete(S, 1, 4);
                   FCfgName := S;
                 end;
               end;
             end;
        end; // end case
    end else
    begin
      // command or filenames...
      if FCommand = ' ' then
      begin
        if Length(S) = 1 then
          FCommand := UpCase(S[1])
        else
          FCommand := '?';
      end else
        if FArcName = '' then
        begin
          FArcName := S;
          if ExtractFileExt(FArcName) = '' then
          begin
            FArcName := ChangeFileExt(FArcName, '.bee');
          end;
        end else
          FFileMasks.Add(S);
    end;
  end; // end for loop

  // process configuration
  if not FileExists(FCfgName) then
  begin
    Interfaces.OnWarning.Data.Msg := (Cr + 'Configuration file ' + FCfgName + ' not found, using default settings' + Cr);
    Synchronize(Interfaces.OnWarning.Method);
    SetExitCode(1);
  end else
    FCfg.LoadFromFile(FCfgName);
end;

procedure TBeeApp.ProcessMasks;
begin
  if FFileMasks.Count = 0 then
  begin
    case FCommand of
     {'a': nothing to do}
     {'D': nothing to do}
      'E': FFileMasks.Add('*');
      'L': FFileMasks.Add('*');
     {'R': nothing to do}
      'T': FFileMasks.Add('*');
      'X': FFileMasks.Add('*');
     {'?': nothing to do}
    end;
    FrOption := True; // force recursion
  end;
end;

procedure TBeeApp.ProcessFilesToExtract;
var
  I: integer;
begin
  if FCommand = 'E' then
  begin
    for I := 0 to Headers.Count - 1 do
      with THeader(Headers.Items[I]) do
        FileName := ExtractFileName(FileName);
  end else
    if Length(FcdOption) > 0 then
    begin
      for I := 0 to Headers.Count - 1 do
        with THeader(Headers.Items[I]) do
          FileName := DeleteFilePath(FcdOption, FileName);
    end;
end;

// OvewWrite file processing

procedure TBeeApp.ProcessFilesToOverWrite;
begin
  if (FuOption = False) and (FfOption = False) then
    ProcessFilesToOverWriteDefault(Headers)
  else
    ProcessFilesToOverWriteAdvanced(Headers);
end;

// ProcessOverWrite sub-routines

procedure TBeeApp.ProcessFilesToOverWriteDefault(Headers: THeaders);
var
  I, J: integer;
  NewFileName: string;
begin
  I := 0;
  while I < Headers.Count do
  begin
    if (THeader(Headers.Items[I]).Action = toExtract) and
       (FileExists(THeader(Headers.Items[I]).FileName) = True) then
    begin
      if (FoOption in ['A', 'Q', 'S']) = False then
      begin
        repeat
          Interfaces.OnOverWrite.Data.FileName := ExtractFileName(THeader(Headers.Items[I]).FileName);
          Interfaces.OnOverWrite.Data.FilePath := ExtractFilePath(THeader(Headers.Items[I]).FileName);
          Interfaces.OnOverWrite.Data.FileSize := THeader(Headers.Items[I]).FileSize;
          Interfaces.OnOverWrite.Data.FileTime := THeader(Headers.Items[I]).FileTime;

          Interfaces.OnOverWrite.Answer := 'A';
          Synchronize(Interfaces.OnOverWrite.Method);
        until (Length(Interfaces.OnOverWrite.Answer) = 1)
          and (UpCase(Interfaces.OnOverWrite.Answer[1]) in ['A', 'N', 'R', 'S', 'Q', 'Y']);

        FoOption := UpCase(Interfaces.OnOverWrite.Answer[1]);
      end;

      case UpCase(FoOption) of
        'A': Break;
        'N': THeader(Headers.Items[I]).Action := toNone;
        'R': begin
               while True do
               begin
                 Interfaces.OnRename.Data.FileName := ExtractFileName(THeader(Headers.Items[I]).FileName);
                 Interfaces.OnRename.Data.FilePath := ExtractFilePath(THeader(Headers.Items[I]).FileName);
                 Interfaces.OnRename.Data.FileSize := THeader(Headers.Items[I]).FileSize;
                 Interfaces.OnRename.Data.FileTime := THeader(Headers.Items[I]).FileTime;
                 Interfaces.OnRename.Data.FileAttr := THeader(Headers.Items[I]).FileAttr;

                 Interfaces.OnRename.Answer := '';
                 Synchronize(Interfaces.OnRename.Method);
                 
                 NewFileName := FixFileName(Interfaces.OnRename.Answer);
                 if (FileExists(NewFileName) = True) or (AlreadyFileExists(Headers, I, [toExtract], NewFileName) <> -1) then
                 begin
                   Interfaces.OnWarning.Data.Msg := ('Warning: file "' + NewFileName + '" already exists.');
                   Synchronize(Interfaces.OnDisplay.Method);
                 end else
                   Break;
               end;

               if Length(NewFileName) = 0 then
                 THeader(Headers.Items[I]).Action := toNone
               else
                 THeader(Headers.Items[I]).FileName := NewFileName;
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
      J := Headers.GetBack(I - 1, toExtract, THeader(Headers.Items[I]).FileName);

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
  if (FuOption xor FfOption) then
  begin

    if FuOption then
    begin
      for I := 0 to Headers.Count - 1 do
        if (THeader(Headers.Items[I]).Action = toExtract) then
        begin
          if (FileExists(THeader(Headers.Items[I]).FileName) = True) then
            THeader(Headers.Items[I]).Action := toNone
          else
            ProcessFileToOverWrite(Headers, I);
        end;
    end else
      for I := 0 to Headers.Count - 1 do
        if (THeader(Headers.Items[I]).Action = toExtract) then
        begin
          if (FileExists(THeader(Headers.Items[I]).FileName) = False) then
            THeader(Headers.Items[I]).Action := toNone
          else
          begin
            if FileAge(THeader(Headers.Items[I]).FileName) >=
              THeader(Headers.Items[I]).FileTime then
              THeader(Headers.Items[I]).Action := toNone
            else
              ProcessFileToOverWrite(Headers, I);
          end;
        end;

  end else

  begin
    for I := 0 to Headers.Count - 1 do
      if FileExists(THeader(Headers.Items[I]).FileName) = True then
      begin
        if FileAge(THeader(Headers.Items[I]).FileName) >=
          THeader(Headers.Items[I]).FileTime then
          THeader(Headers.Items[I]).Action := toNone
        else
          ProcessFileToOverWrite(Headers, I);
      end else
        ProcessFileToOverWrite(Headers, I);
  end;
end;

function TBeeApp.AlreadyFileExists(Headers: THeaders; FileIndex: integer;
  FileActions: THeaderActions; const FileName: string): integer;
begin
  if Length(FileName) > 0 then
  begin
    Result := Headers.GetBack(FileIndex - 1, FileActions, FileName);
    if Result = -1 then
    begin
      Result := Headers.GetNext(FileIndex + 1, FileActions, FileName);
    end;
  end else
    Result := -1
end;

procedure TBeeApp.ProcessFileToOverWrite(Headers: THeaders; FileIndex: integer);
var
  J: integer;
begin
  J := Headers.GetBack(FileIndex - 1, toExtract, THeader(Headers.Items[FileIndex]).FileName);

  if J > -1 then
  begin
    if (THeader(Headers.Items[FileIndex]).FileTime > THeader(Headers.Items[J]).FileTime) then
      THeader(Headers.Items[J]).Action := toNone
    else
      THeader(Headers.Items[FileIndex]).Action := toNone;
  end;
end;

function TBeeApp.ProcessFilesToRename(Headers: THeaders): boolean;
var
  i: integer;
  iFileName: string;
begin
  Headers.MarkItems(FFileMasks, toCopy, toRename, FrOption);
  Headers.MarkItems(FxOption, toRename, toCopy, FrOption);

  if (Headers.GetNext(0, toRename) > -1) then
  begin
    for i := 0 to Headers.Count - 1 do
    begin
      if (THeader(Headers.Items[i]).Action = toRename) then
      begin
        while True do
        begin
          Interfaces.OnRename.Data.FileName := ExtractFileName(THeader(Headers.Items[i]).FileName);
          Interfaces.OnRename.Data.FilePath := ExtractFilePath(THeader(Headers.Items[i]).FileName);
          Interfaces.OnRename.Data.FileSize := THeader(Headers.Items[i]).FileSize;
          Interfaces.OnRename.Data.FileTime := THeader(Headers.Items[i]).FileTime;
          Interfaces.OnRename.Data.FileAttr := THeader(Headers.Items[i]).FileAttr;

          SetLength(Interfaces.OnRename.Answer, 0);
          Synchronize(Interfaces.OnRename.Method);

          iFileName := FixFileName(Interfaces.OnRename.Answer);
          if (AlreadyFileExists(Headers, I, [toCopy, toRename], iFileName) <> -1) then
          begin
            Interfaces.OnWarning.Data.Msg := ('Warning: file "' + iFileName + '" already existing in archive.');
            Synchronize(Interfaces.OnWarning.Method);
          end else
            Break;
        end;

        if Length(iFileName) > 0 then
        begin
          THeader(Headers.Items[i]).FileName := iFileName;
        end;
      end;
    end;
    Result := True
  end else
    Result := ((Length(FaOption) > 0) and (Headers.GetNext(0, toCopy) > -1)) ;
end;

// Sequences processing

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
           toCopy: begin
                    THeader(Headers.Items[J]).Action := toSwap;
                    Inc(Interfaces.OnTick.Data.TotalSize,
                      THeader(Headers.Items[J]).FileSize * 2); // decoding  and Encoding size
                   end;
          toFresh: Inc(Interfaces.OnTick.Data.TotalSize,
                     THeader(Headers.Items[J]).FileSize); // decoding size
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
            toCopy:   begin
                        THeader(Headers.Items[J]).Action := toSwap;
                        Inc(Interfaces.OnTick.Data.TotalSize,
                          THeader(Headers.Items[J]).FileSize * 2);
                      end;
            toDelete: Inc(Interfaces.OnTick.Data.TotalSize,
                        THeader(Headers.Items[J]).FileSize);
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
  I, J: integer;
  Decoder: TDecoder;
  FSwapStrm: TFileWriter;
  iDictionary, iTable, iTear: integer;
  CurrDictionary, CurrTable: integer;
begin
  Result := True;
  I := Headers.GetBack(Headers.Count - 1, toSwap);
  if (I > -1) and (Interfaces.Stop = False) then
  begin
    FSwapName := GenerateFileName(FyOption);
    FSwapStrm := TFileWriter.Create(FSwapName, fmCreate);

    CurrDictionary := Headers.Count;
    CurrTable := Headers.Count;

    Decoder := TDecoder.Create(FArcFile, Interfaces, Synchronize); // get GeneralSize
    while (I > -1) and (Interfaces.Stop = False) do
    begin
      iDictionary := Headers.GetBack(I, foDictionary); // find dictionary info
      iTable := Headers.GetBack(I, foTable); // find table info
      iTear  := Headers.GetBack(I, foTear); // find tear info

      if (iDictionary > -1) and (iDictionary <> CurrDictionary) and (iDictionary <> iTear) then
      begin
        CurrDictionary := iDictionary;
        Decoder.DecodeStrm(THeader(Headers.Items[iDictionary]), pmQuit, FSwapStrm);
      end;

      if (iTable > -1) and (iTable <> CurrTable) and (iTable <> iTear) then
      begin
        CurrTable := iTable;
        Decoder.DecodeStrm(THeader(Headers.Items[iTable]), pmQuit, FSwapStrm);
      end;

      for J := iTear to I do
      begin
        if Interfaces.Stop = False then
        begin
          if THeader(Headers.Items[J]).Action = toSwap then
            Result := Decoder.DecodeStrm(Headers.Items[J], pmNorm, FSwapStrm)
          else
            Result := Decoder.DecodeStrm(Headers.Items[J], pmSkip, FSwapStrm);
        end else
          Result := True;

        if Result = False then Break;
      end;
      if Result = False then Break;

      I := Headers.GetBack(iTear - 1, toSwap);
    end;
    Decoder.Destroy;
    FreeAndNil(FSwapStrm);
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
        if (foVersion in THeader(Items[I]).FileFlags) and (not (foVersion in THeader(Items[I + 1]).FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).FileFlags, foVersion);
          THeader(Items[I + 1]).FileVersion := THeader(Items[I]).FileVersion;
        end;

        if (foMethod in THeader(Items[I]).FileFlags) and (not (foMethod in THeader(Items[I + 1]).FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).FileFlags, foMethod);
          THeader(Items[I + 1]).FileMethod := THeader(Items[I]).FileMethod;
        end;

        if (foDictionary in THeader(Items[I]).FileFlags) and (not (foDictionary in THeader(Items[I + 1]).FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).FileFlags, foDictionary);
          THeader(Items[I + 1]).FileDictionary := THeader(Items[I]).FileDictionary;
        end;

        if (foTable in THeader(Items[I]).FileFlags) and (not (foTable in THeader(Items[I + 1]).FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).FileFlags, foTable);
          THeader(Items[I + 1]).FileTable := THeader(Items[I]).FileTable;
        end;

        if (foTear in THeader(Items[I]).FileFlags) and (not (foTear in THeader(Items[I + 1]).FileFlags)) then
        begin
          Include(THeader(Items[I + 1]).FileFlags, foTear);
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
    iTear  := Headers.GetBack(I, foTear); // find tear info

    for J := iTear to (I - 1) do
      if THeader(Headers.Items[J]).Action in [toNone, toQuit] then
      begin
        THeader(Headers.Items[J]).Action := toSkip;
        Inc(Interfaces.OnTick.Data.TotalSize,
          THeader(Headers.Items[J]).FileSize);
      end;

    if (iDictionary > -1) and (THeader(Headers.Items[iDictionary]).Action = toNone) then
      THeader(Headers.Items[iDictionary]).Action := toQuit;

    if (iTable > -1) and (THeader(Headers.Items[iTable]).Action = toNone) then
      THeader(Headers.Items[iTable]).Action := toQuit;

    I := Headers.GetBack(iTear - 1, aAction);
  end;
end;

/// Option processing

procedure TBeeApp.ProcesstOption;
begin
  if FtOption then
  begin
    with Interfaces.OnTick.Data do
    begin
      TotalSize := 0;
      ProcessedSize := 0;
    end;
    FrOption := True; // force recursion
    FxOption.Clear; // clear xOption
    FFileMasks.Clear; // clear FileMasks
    FFileMasks.Add('*');
    DecodeShell(toTest);
  end;
end;

procedure TBeeApp.ProcesslOption;
begin
  if FlOption then
  begin
    with Interfaces.OnTick.Data do
    begin
      TotalSize := 0;
      ProcessedSize := 0;
    end;
    FrOption := True; // force recursion
    FxOption.Clear; // clear xOption
    FFileMasks.Clear; // clear FileMasks
    FFileMasks.Add('*');
    ListShell;
  end;
end;

/// Shell procedures

procedure TBeeApp.EncodeShell;
var
  I: integer;
  Encoder: TEncoder;
  TmpFileName: string;
  TmpFile: TFileWriter;
  Headers: THeaders;
  Time: double;
begin
  Interfaces.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + FArcName);
  Synchronize(Interfaces.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);

    // process FileMasks and xFileMasks

    with Interfaces.OnTick.Data do
      Headers.AddItems(
        FFileMasks,
        FcdOption,
         FfOption,
         FrOption,
         FuOption,
         FxOption,
        TotalSize);

    if (Headers.GetCount([toUpdate, toFresh]) > 0) or
      ((Length(FaOption) > 0) and (Headers.GetNext(0, toCopy) > -1)) then
    begin
      Time := Now;
      TmpFileName := GenerateFileName(FyOption);
      TmpFile := TFileWriter.Create(TmpFileName, fmCreate);

      // find sequences and...
      ProcessFilesToFresh(Headers);
      // decode solid header modified in a swap file
      if ProcessFilesToSwap(Headers) then
      begin
        // sort headers (only toUpdate headers)
        Headers.SortNews(FCfg, FsOption, FkOption, FeOption);

        // if exists a modified solid sequence open swap file
        if Length(FSwapName) > 0 then
          FSwapFile := TFileReader.Create(FSwapName, fmOpenRead + fmShareDenyWrite);

        // set sfx module
        if Length(FaOption) > 0 then Headers.SetModule(FaOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Interfaces, Synchronize);
        for I := 0 to Headers.Count - 1 do
          if Interfaces.Stop = False then
          begin
            case THeader(Headers.Items[I]).Action of
              toCopy:   Encoder.CopyStrm  (Headers.Items[I], emNorm, FArcFile);
              toSwap:   Encoder.EncodeStrm(Headers.Items[I], emNorm, FSwapFile);
              toFresh:  Encoder.EncodeFile(Headers.Items[I], emNorm);
              toUpdate: Encoder.EncodeFile(Headers.Items[I], emNorm);
            end;
          end;
        Encoder.Destroy;
        // rewrite Headers
        Headers.WriteItems(TmpFile);

        if Interfaces.Stop = False then
        begin
          Interfaces.OnDisplay.Data.Msg := (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnDisplay.Method);
        end else
        begin
          Interfaces.OnError.Data.Msg := (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnError.Method);
          SetExitCode(255);
        end;

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        DeleteFile(FSwapName);
        if Interfaces.Stop = False then
        begin
          SysUtils.DeleteFile(FArcName);
          if not RenameFile(TmpFileName, FArcName) then
          begin
            Interfaces.OnError.Data.Msg := ('Error: can''t rename TempFile to ' + FArcName);
            Synchronize(Interfaces.OnError.Method);
            SetExitCode(2);
          end else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption
          end;
        end else
          DeleteFile(TmpFileName);
        
      end else // if ProcessFilesToSwap
      begin
        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        SysUtils.DeleteFile(TmpFileName);

        Interfaces.OnError.Data.Msg := ('Error: can''t decode solid sequences');
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(2);
      end;

    end else // if Headers.GetCount
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to process');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;
  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

procedure TBeeApp.DecodeShell(Action: THeaderAction);
var
  Decoder: TDecoder;
  Headers: THeaders;
  Return: boolean;
  Time: double;
  I: integer;
begin
  Interfaces.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + FArcName);
  Synchronize(Interfaces.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toNone) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);

    Headers.MarkItems(FFileMasks, toNone, Action, FrOption);
    Headers.MarkItems(FxOption, Action, toNone, FrOption);

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
            toTest:    Return := Decoder.DecodeFile(Headers.Items[I], pmTest);
            toSkip:    Return := Decoder.DecodeFile(Headers.Items[I], pmSkip);
            toQuit:    Return := Decoder.Decodefile(Headers.Items[I], pmQuit);
          end;
          if Return = False then Break;
        end;
      Decoder.Destroy;

      if Interfaces.Stop = False then
      begin
        if Return = True then
        begin
          Interfaces.OnDisplay.Data.Msg := (Cr + 'Everything went ok - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnDisplay.Method);
        end else
        begin
          Interfaces.OnError.Data.Msg := (Cr + 'Process aborted, a fatal error occourred - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnError.Method);
          SetExitCode(2);
        end
      end else
      begin
        Interfaces.OnError.Data.Msg := (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(255);
      end;
      
    end else // if Headers.GetNext
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to decode');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;
  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

procedure TBeeApp.DeleteShell;
var
  TmpFileName: string;
  TmpFile: TFileWriter;
  I: integer;
  Time: double;
  Headers: THeaders;
  Encoder: TEncoder;
begin
  Interfaces.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + FArcName);
  Synchronize(Interfaces.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);

    Headers.MarkItems(FFileMasks, toCopy, toDelete, FrOption);
    Headers.MarkItems(FxOption, toDelete, toCopy, FrOption);

    if (Headers.GetNext(0, toDelete) > -1) or ((Length(FaOption) > 0) and (Headers.GetNext(0, toCopy) > -1)) then
    begin
      Time := Now;
      TmpFileName := GenerateFileName(FyOption);
      TmpFile := TFileWriter.Create(TmpFileName, fmCreate);

      // find sequences
      ProcessFilesToDelete(Headers);
      if ProcessFilesToSwap(Headers) then
      begin
        // rescue headers information
        ProcessFilesDeleted(Headers);

        // if SwapSequences has found a modified sequence open Swap file
        if Length(FSwapName) > 0 then
          FSwapFile := TFileReader.Create(FSwapName, fmOpenRead + fmShareDenyWrite);

        // set sfx module
        if Length(FaOption) > 0 then Headers.SetModule(FaOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Interfaces, Synchronize);
        for I := 0 to Headers.Count - 1 do
          if Interfaces.Stop = False then
          begin
            case THeader(Headers.Items[I]).Action of
              toCopy:   Encoder.CopyStrm  (Headers.Items[I], emNorm, FArcFile);
              toSwap:   Encoder.EncodeStrm(Headers.Items[I], emNorm, FSwapFile);
              toDelete: begin
                          Interfaces.OnDisplay.Data.Msg := (msgDeleting + THeader(Headers.Items[I]).FileName);
                          Synchronize(Interfaces.OnDisplay.Method);
                        end;
            end;
          end;
        Encoder.Destroy;
        Headers.WriteItems(TmpFile);

        if Interfaces.Stop = False then
        begin
          Interfaces.OnDisplay.Data.Msg := (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnDisplay.Method);
        end else
        begin
          Interfaces.OnError.Data.Msg := (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
          Synchronize(Interfaces.OnError.Method);
          SetExitCode(255);
        end;

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        if Interfaces.Stop = False then
        begin
          SysUtils.DeleteFile(FArcName);
          if not RenameFile(TmpFileName, FArcName) then
          begin
            Interfaces.OnError.Data.Msg := ('Error: can''t rename TempFile to ' + FArcName);
            Synchronize(Interfaces.OnError.Method);
            SetExitCode(2);
          end else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption  
          end;
        end else
          SysUtils.DeleteFile(TmpFileName);

      end else // if ProcessFilesToSwap
      begin
        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        SysUtils.DeleteFile(TmpFileName);

        Interfaces.OnError.Data.Msg := ('Error: can''t decode solid sequences');
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(2);
      end;

    end else // if Headers.GetNext
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to delete');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;
  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

procedure TBeeApp.RenameShell;
var
  TmpFile: TFileWriter;
  TmpFileName: string;
  Headers: THeaders;
  Encoder: TEncoder;
  Time: double;
  I: integer;
begin
  Interfaces.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + FArcName);
  Synchronize(Interfaces.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    Interfaces.OnDisplay.Data.Msg := (msgScanning + '...');
    Synchronize(Interfaces.OnDisplay.Method);
    
    if ProcessFilesToRename(Headers) then
    begin
      Time := Now;
      TmpFileName := GenerateFileName(FyOption);
      TmpFile := TFileWriter.Create(TmpFileName, fmCreate);

      Interfaces.OnTick.Data.TotalSize :=
        Headers.GetPackedSize([toCopy, toRename]);

      // set sfx module
      if Length(FaOption) > 0 then Headers.SetModule(FaOption);

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
        Interfaces.OnDisplay.Data.Msg := (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(Time) + ' seconds');
        Synchronize(Interfaces.OnDisplay.Method);
      end else
      begin
        Interfaces.OnError.Data.Msg := (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(255);
      end;

      if Assigned(FArcFile) then FreeAndNil(FArcFile);
      if Assigned(TmpFile) then FreeAndNil(TmpFile);

      if Interfaces.Stop = False then
      begin
        SysUtils.DeleteFile(FArcName);
        if not RenameFile(TmpFileName, FArcName) then
        begin
          Interfaces.OnError.Data.Msg := ('Error: can''t rename TempFile to ' + FArcName);
          Synchronize(Interfaces.OnError.Method);
          SetExitCode(2);
        end else
        begin
          ProcesstOption; // process tOption
          ProcesslOption; // process lOption
        end;
      end else
        SysUtils.DeleteFile(TmpFileName);

    end else // if ProcessFilesToRename
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to rename');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;
  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

procedure TBeeApp.ListShell;
var
  P: THeader;
  I: integer;
  Info: THeaders;  
  Version, Method, Dictionary: integer;
  TotalPack, TotalSize: integer;
  CountFiles: integer;
  Time: double;
begin
  Interfaces.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + FArcName);
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

    Info.MarkItems(FFileMasks, toNone, toList, FrOption);
    Info.MarkItems(FxOption, toList, toNone, FrOption);

    if (Info.GetNext(0, toList) > -1) then
    begin
      Time := Now;
      
      {$IFDEF CONSOLEAPPLICATION}
      Interfaces.OnDisplay.Data.Msg := StringOfChar('-', 79);
      Synchronize(Interfaces.OnDisplay.Method);

      Interfaces.OnDisplay.Data.Msg := ('Name' + StringOfChar(' ', 18)
        + 'Size     Packed Ratio     Date  Time   Attr      CRC Meth');
      Synchronize(Interfaces.OnDisplay.Method);

      Interfaces.OnDisplay.Data.Msg := StringOfChar('-', 79);
      Synchronize(Interfaces.OnDisplay.Method);
      {$ENDIF}

      for I := 0 to Info.Count - 1 do
      begin
        if THeader(Info.Items[I]).Action = toList then
        begin
          P := Info.Items[I];

          if foVersion    in P.FileFlags then Version    := P.FileVersion;
          if foMethod     in P.FileFlags then Method     := P.FileMethod;
          if foDictionary in P.FileFlags then Dictionary := P.FileDictionary;

          with Interfaces.OnList.Data do
          begin
            FileName := ExtractFileName(P.FileName);
            FilePath := ExtractFilePath(P.FileName);
            FileSize := P.FileSize;
            FilePacked := P.FilePacked;

            if FileSize > 0 then
              FileRatio := MulDiv(FilePacked, 100, FileSize)
            else
              FileRatio := 100;

            FileAttr := P.FileAttr;
            FileTime := P.FileTime;
            FileComm := '';
            FileCrc := P.FileCrc;
            FileMethod := MethodToStr(P, Method, Dictionary);
            FileVersion := VersionToStr(Version);

            if foPassword in P.FileFlags then
              FilePassword := 'Yes'
            else
              FilePassword := 'No';

            FilePosition := I;
          end;
          Synchronize(Interfaces.OnList.Method);
          
          Inc(TotalSize, P.FileSize);
          Inc(TotalPack, P.FilePacked);
          Inc(CountFiles);
        end;
      end;
      {$IFDEF CONSOLEAPPLICATION}
      Interfaces.OnDisplay.Data.Msg := StringOfChar('-', 79);
      Synchronize(Interfaces.OnDisplay.Method);

      Interfaces.OnDisplay.Data.Msg := (Format('%d files', [CountFiles]))
        + StringOfChar(' ', 15 - Length((Format('%d files', [CountFiles]))))
        + (Format(' %10s %10s %5s', [SizeToStr(TotalSize), SizeToStr(TotalPack), RatioToStr(TotalPack, TotalSize)]));
      Synchronize(Interfaces.OnDisplay.Method);
      {$ENDIF}
      
      {$IFDEF CONSOLEAPPLICATION}
      // self-extractor module size
      if Info.GetModule > 0 then
      begin
        Interfaces.OnDisplay.Data.Msg := (Cr + 'Note: Bee Self-Extractor module founded');
        Synchronize(Interfaces.OnDisplay.Method);
      end;
      {$ENDIF}
       Interfaces.OnDisplay.Data.Msg := (Cr + 'Everything went ok - ' + TimeDifference(Time) + ' seconds');
       Synchronize(Interfaces.OnDisplay.Method);
    end else
    begin
      Interfaces.OnWarning.Data.Msg := ('Warning: no files to list');
      Synchronize(Interfaces.OnWarning.Method);
      SetExitCode(1);
    end;

  end;
  Info.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

// string routines

function TBeeApp.MethodToStr(P: THeader; Method, Dictionary: integer): string;
begin
  Result := 'm0a';

  if not (foTear in P.FileFlags) then
    Result[1] := 's';

  if not (foMoved in P.FileFlags) then
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
  else Result := ' 0' + DecimalSeparator + '0';
  end;
end;

end.
