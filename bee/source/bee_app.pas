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

  v0.7.9 build 0962 - 2009.02.27 by Melchiorre Caruso.
}

unit Bee_App;

{$I compiler.inc}

interface

uses
  Classes, // TStringList, ...
  // ---
  Bee_Files,
  Bee_Types,
  Bee_Headers,
  Bee_Interface,
  Bee_CommandLine,
  Bee_Configuration; // TConfiguration, TTable;

type

  // TBeeApp class

  TBeeApp = class(TApp)
  public
    constructor Create(aParams: TStringList);
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
  protected
    // string routines
    function VersionToStr(P: THeader): string;
    function MethodToStr(P: THeader): string;

  private
    FSelfName: string;
    FArcFile:  TFileReader; // archive file stream
    FSwapFile: TFileReader; // swap file stream
    FSwapName: string;      // swap file name

    FConfiguration: TConfiguration;
    FCommandLine: TCommandLine;
  end;

implementation

uses
  Strings,
  SysUtils, // faReadOnly, ...

  Bee_Common, // Various helper routines
  Bee_Assembler,
  Bee_MainPacker; // TEncoder...

// TBeeApp ...

constructor TBeeApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  Randomize; // randomize, uses for unique filename generation...

  FSelfName := 'The Bee 0.7.9 build 0984 archiver utility, February 2009' + Cr +
               '(C) 1999-2009 Andrew Filinsky and Melchiorre Caruso';

  FArcFile  := nil;
  FSwapName := '';
  FSwapFile := nil;

  // process command line
  FCommandLine := TCommandLine.Create;
  FCommandLine.Process(aParams);

  // load configuration
  FConfiguration := TConfiguration.Create;
  if not FileExists(FCommandLine.cfgOption) then
    ProcessWarning('Warning: configuration file "' + FCommandLine.cfgOption +
                   '" not found, data will be stored' + Cr, 1)
  else
    FConfiguration.LoadFromFile(FCommandLine.cfgOption);

  // load method and dictionary level
  FConfiguration.Selector('\main');
  FConfiguration.CurrentSection.Values['Method']     := IntToStr(FCommandLine.mOption);
  FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(FCommandLine.dOption);

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
  ProcessMessage(Cr + '  Usage: Bee <Command> -<Option 1> -<Option N> <ArchiveName> <FileNames...>');
  ProcessMessage(Cr + '  Commands:' + Cr);
  ProcessMessage('    a   Add files to archive');
  ProcessMessage('    d   Delete files from archive');
  ProcessMessage('    e   Extract files from archive');
  ProcessMessage('    x   eXtract files from archive with path name');
  ProcessMessage('    l   List archive');
  ProcessMessage('    t   Test archive files');
  ProcessMessage('    r   Rename files in archive');
  ProcessMessage(Cr + '  Options:' + Cr);
  ProcessMessage('    r       Recurse subdirectories');
  ProcessMessage('    u       Update files');
  ProcessMessage('    f       Freshen files');
  ProcessMessage('    e       force file Extention');
  ProcessMessage('    s       create Solid archive');
  ProcessMessage('    a       add self-extrActor module');
  ProcessMessage('    o<mode> set overwrite file Mode (Q-Quit, A-All, S-Skip all)');
  ProcessMessage('    m<0..3> set compression Method (0-store...1-default...3-maximal)');
  ProcessMessage('    d<0..9> set Dictionary size (d1 uses < 5M, d2 (default) < 10M, d3 < 20M...)' + Cr);
  ProcessMessage('    x       eXclude filenames');
  ProcessMessage('    t       Test archive after process');
  ProcessMessage('    l       List archive after process');
  ProcessMessage('    y       set temporany directory');
  ProcessMessage('    k       use blowfish crypter/decrypter (min key-length 4 bytes)');
  ProcessMessage('    cd<dir> set current archive directory' + Cr);
  ProcessMessage('    cfg<filename> use specified configuration file');
  ProcessMessage('    pri<priority> set process Priority (0-Idle, 1-Normal, 2-High, 3-RealTime)');
  ProcessMessage(Cr + '  Use BeeOpt to make most optimal parameters.' + Cr);
end;

procedure TBeeApp.Execute;
const
  SetOfCommands = ['A', 'D', 'E', 'L', 'R', 'T', 'X'];
begin
  inherited Execute;
  ProcessMessage(FSelfName);
  with FCommandLine do
  begin
    if ((Command in SetOfCommands) and (ArchiveName > '')) or (Command = '?') then
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
  FTerminated := True;
end;

function TBeeApp.OpenArchive(Headers: THeaders; aAction: THeaderAction): boolean;
begin
  Result := True;
  if FileExists(FCommandLine.ArchiveName) then
  begin
    FArcFile := CreateTFileReader(FCommandLine.ArchiveName, fmOpenRead + fmShareDenyWrite);
    try
      Headers.ReadItems(FArcFile, aAction);
      if (Headers.Count = 0) and (FArcFile.Size <> 0) then
      begin
        Result := False;  ProcessFatalError('Error: can''t open archive', 2);
      end;
    except
      Result := False;  ProcessFatalError('Error: can''t open archive', 2);
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
    for I := 0 to Headers.Count - 1 do
      with THeader(Headers.Items[I]).Data do
        FileName := ExtractFileName(FileName)
  else
  if Length(FCommandLine.cdOption) > 0 then
    for I := 0 to Headers.Count - 1 do
      with THeader(Headers.Items[I]).Data do
        FileName := DeleteFilePath(FCommandLine.cdOption, FileName);
end;

// -------------------------------------------------------------------------- //
// Ovewwrite file processing                                                  //
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
  S: string;
  I, J: integer;
  FileInfo: TFileInfo;
begin
  I := 0;
  while I < Headers.Count do
  begin
    if THeader(Headers.Items[I]).Action = toExtract then
      if FileExists(THeader(Headers.Items[I]).Data.FileName) then
      begin
        if not(FCommandLine.oOption in ['A', 'Q', 'S']) then
        begin
          FileInfo.FileName := StringToPChar(ExtractFileName(THeader(Headers.Items[I]).Data.FileName));
          FileInfo.FilePath := StringToPChar(ExtractFilePath(THeader(Headers.Items[I]).Data.FileName));

          FileInfo.FileSize := THeader(Headers.Items[I]).Data.FileSize;
          FileInfo.FileTime := THeader(Headers.Items[I]).Data.FileTime;
          FileInfo.FileAttr := THeader(Headers.Items[I]).Data.FileAttr;
          repeat
            FCommandLine.oOption := UpCase(ProcessOverwrite(FileInfo, 'A'));
          until FCommandLine.oOption in ['A', 'N', 'R', 'S', 'Q', 'Y'];

          StrDispose(FileInfo.FileName);
          StrDispose(FileInfo.FilePath);
        end;

        case FCommandLine.oOption of
        'A': Break;
        'N': THeader(Headers.Items[I]).Action := toNone;
        'R': begin
               FileInfo.FileName := StringToPChar(ExtractFileName(THeader(Headers.Items[I]).Data.FileName));
               FileInfo.FilePath := StringToPChar(ExtractFilePath(THeader(Headers.Items[I]).Data.FileName));

               FileInfo.FileSize := THeader(Headers.Items[I]).Data.FileSize;
               FileInfo.FileTime := THeader(Headers.Items[I]).Data.FileTime;
               FileInfo.FileAttr := THeader(Headers.Items[I]).Data.FileAttr;
               while True do
               begin
                 S := FixFileName(ProcessRename(FileInfo, ''));
                 if FileExists(S) or (AlreadyFileExists(Headers, I, [toExtract], S) <> -1) then
                   ProcessWarning('Warning: file "' + S + '" already exists', 0)
                 else
                   Break;
               end;
               if Length(S) = 0 then
                 THeader(Headers.Items[I]).Action := toNone
               else
                 THeader(Headers.Items[I]).Data.FileName := S;
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
    if THeader(Headers.Items[I]).Action = toExtract then
    begin
      J := Headers.GetBack(I - 1, toExtract, THeader(Headers.Items[I]).Data.FileName);
      if J > -1 then
      begin
        THeader(Headers.Items[J]).Action := toNone;
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
        if THeader(Headers.Items[I]).Action = toExtract then
          if FileExists(THeader(Headers.Items[I]).Data.FileName) then
            THeader(Headers.Items[I]).Action := toNone
          else
            ProcessFileToOverWrite(Headers, I);
    end else
    begin
      for I := 0 to Headers.Count - 1 do
        if THeader(Headers.Items[I]).Action = toExtract then
          if FileExists(THeader(Headers.Items[I]).Data.FileName) then
            THeader(Headers.Items[I]).Action := toNone
          else
          if FileAge(THeader(Headers.Items[I]).Data.FileName) >=
            THeader(Headers.Items[I]).Data.FileTime then
            THeader(Headers.Items[I]).Action := toNone
          else
            ProcessFileToOverWrite(Headers, I);
    end;
  end else
    for I := 0 to Headers.Count - 1 do
      if FileExists(THeader(Headers.Items[I]).Data.FileName) then
      begin
        if FileAge(THeader(Headers.Items[I]).Data.FileName) >=
          THeader(Headers.Items[I]).Data.FileTime then
          THeader(Headers.Items[I]).Action := toNone
        else
        begin
          ProcessFileToOverWrite(Headers, I);
        end;
      end else
        ProcessFileToOverWrite(Headers, I);
end;

function TBeeApp.AlreadyFileExists(Headers: THeaders;
  FileIndex: integer; FileActions: THeaderActions;
  const FileName: string): integer;
begin
  if Length(FileName) > 0 then
  begin
    Result := Headers.GetBack(FileIndex - 1, FileActions, FileName);
    if Result = -1 then
      Result := Headers.GetNext(FileIndex + 1, FileActions, FileName);
  end else
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
    if (THeader(Headers.Items[FileIndex]).Data.FileTime >
      THeader(Headers.Items[J]).Data.FileTime) then
      THeader(Headers.Items[J]).Action := toNone
    else
      THeader(Headers.Items[FileIndex]).Action := toNone;
end;

// -------------------------------------------------------------------------- //
// Rename file processing                                                     //
// -------------------------------------------------------------------------- //

function TBeeApp.ProcessFilesToRename(Headers: THeaders): boolean;
var
  S: string;
  I: integer;
  FileInfo: TFileInfo;
begin
  Headers.MarkItems(FCommandLine.FileMasks, toCopy, toRename, FCommandLine.rOption);
  Headers.MarkItems(FCommandLine.xOption, toRename, toCopy, FCommandLine.rOption);

  if (Headers.GetNext(0, toRename) > -1) then
  begin
    for I := 0 to Headers.Count - 1 do
      if THeader(Headers.Items[i]).Action = toRename then
      begin
        FileInfo.FileName := StringToPChar(ExtractFileName(THeader(Headers.Items[I]).Data.FileName));
        FileInfo.FilePath := StringToPChar(ExtractFilePath(THeader(Headers.Items[I]).Data.FileName));

        FileInfo.FileSize := THeader(Headers.Items[I]).Data.FileSize;
        FileInfo.FileTime := THeader(Headers.Items[I]).Data.FileTime;
        FileInfo.FileAttr := THeader(Headers.Items[I]).Data.FileAttr;
        while True do
        begin
          S := FixFileName(ProcessRename(FileInfo, ''));
          if AlreadyFileExists(Headers, I, [toCopy, toRename], S) <> -1 then
            ProcessWarning('Warning: file "' + S + '" already existing in archive', 0)
          else
            Break;
        end;
        StrDispose(FileInfo.FileName);
        StrDispose(FileInfo.FilePath);

        if Length(S) > 0 then THeader(Headers.Items[I]).Data.FileName := S;
      end;
    Result := True;
  end else
    Result := ((Length(FCommandLine.aOption) > 0) and (Headers.GetNext(0, toCopy) > -1));
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

    if NextTear = -1 then NextTear := Headers.Count;
    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := Headers.GetBack(NextTear - 1, toCopy);
      for J := BackTear to NextTear do
        case THeader(Headers.Items[J]).Action of
          toCopy:  begin
                     THeader(Headers.Items[J]).Action := toSwap;
                     Inc(FTotalSize, THeader(Headers.Items[J]).Data.FileSize * 2);
                   end;
          toFresh: Inc(FTotalSize, THeader(Headers.Items[J]).Data.FileSize);
        end;
      I := BackTear;
    end;
    I := Headers.GetBack(I - 1, toFresh);
  end;
  Inc(FTotalSize, Headers.GetPackedSize(toCopy));
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

    if NextTear = -1 then NextTear := Headers.Count;
    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := Headers.GetBack(NextTear - 1, toCopy);
      // if exists an header toDelete
      if Headers.GetBack(NextTear, toDelete) > (BackTear - 1) then
        for J := BackTear to NextTear do
          case THeader(Headers.Items[J]).Action of
            toCopy:   begin
                        THeader(Headers.Items[J]).Action := toSwap;
                        Inc(FTotalSize, THeader(Headers.Items[J]).Data.FileSize * 2);
                      end;
            toDelete: Inc(FTotalSize, THeader(Headers.Items[J]).Data.FileSize);
          end;
      I := BackTear;
    end;
    I := Headers.GetBack(I - 1, toDelete);
  end;
  Inc(FTotalSize, Headers.GetPackedSize(toCopy));
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
  if (I > -1) then
  begin
    FSwapName := GenerateFileName(FCommandLine.yOption);
    FSwapStrm := CreateTFileWriter(FSwapName, fmCreate);

    if (FSwapStrm <> nil) then
    begin
      CurrDictionary := Headers.Count;
      CurrTable      := Headers.Count;

      Decoder := TDecoder.Create(FArcFile, Self);
      // get GeneralSize
      while (I > -1) and (not FTerminated) do
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
          if not FTerminated then
          begin
            if THeader(Headers.Items[J]).Action = toSwap then
              Result := Decoder.DecodeStrm(Headers.Items[J], pmNorm, FSwapStrm)
            else
            begin
              Result := Decoder.DecodeStrm(Headers.Items[J], pmSkip, FSwapStrm);
            end;
          end else
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

    end else
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
          Include(THeader(Items[I + 1]).Data.FileFlags, foTear);
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
        Inc(FTotalSize, THeader(Headers.Items[J]).Data.FileSize);
      end;

    if (iDictionary > -1) and (THeader(Headers.Items[iDictionary]).Action = toNone) then
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
    FTotalSize := 0;
    FSize      := 0;

    FCommandLine.rOption := True;
    FCommandLine.xOption.Clear;
    FCommandLine.FileMasks.Clear;
    FCommandLine.FileMasks.Add('*');
    DecodeShell(toTest);
  end;
end;

procedure TBeeApp.ProcesslOption;
begin
  if FCommandLine.lOption then
  begin
    FTotalSize := 0;
    FSize      := 0;

    FCommandLine.rOption := True;
    FCommandLine.xOption.Clear;
    FCommandLine.FileMasks.Clear;
    FCommandLine.FileMasks.Add('*');
    ListShell;
  end;
end;

// -------------------------------------------------------------------------- //
// Shell procedures                                                           //
// -------------------------------------------------------------------------- //

procedure TBeeApp.EncodeShell;
var
  I: integer;
  Encoder: TEncoder;
  TmpFileName: string;
  TmpFile: TFileWriter;
  Headers: THeaders;
begin
  ProcessMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    ProcessMessage(msgScanning + '...');
    // process FileMasks and xFileMasks
    with FCommandLine do
      Headers.AddItems(FileMasks, cdOption, fOption, rOption, uOption, xOption, FTotalSize);

    if (Headers.GetCount([toUpdate, toFresh]) > 0) or ((Length(FCommandLine.aOption) > 0) and (Headers.GetNext(0, toCopy) > -1)) then
    begin
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
          FSwapFile :=
            CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);

        // set sfx module
        with FCommandLine do
          if Length(aOption) > 0 then
            Headers.SetModule(aOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Self);
        for I := 0 to Headers.Count - 1 do
          if not FTerminated then
            case THeader(Headers.Items[I]).Action of
              toCopy:   Encoder.CopyStrm  (Headers.Items[I], emNorm, FArcFile);
              toSwap:   Encoder.EncodeStrm(Headers.Items[I], emNorm, FSwapFile);
              toFresh:  Encoder.EncodeFile(Headers.Items[I], emNorm);
              toUpdate: Encoder.EncodeFile(Headers.Items[I], emNorm);
            end;
        Encoder.Destroy;
        // rewrite Headers
        Headers.WriteItems(TmpFile);

        if not Terminated then
          ProcessMessage(Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(FTotalTime) + ' seconds')
        else
          ProcessError(Cr + 'Process aborted - ' + TimeDifference(FTotalTime) + ' seconds', 255);

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        DeleteFile(FSwapName);
        if not FTerminated then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
            ProcessError('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName, 2)
          else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption
          end;
        end else
        begin
          DeleteFile(TmpFileName);
        end;

      end else // if ProcessFilesToSwap
      begin
        if TmpFile = nil then
          ProcessError('Error: can''t open temp file',2)
        else
          ProcessError('Error: can''t decode solid sequences', 2);

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        SysUtils.DeleteFile(TmpFileName);
      end;

    end else // if Headers.GetCount
      ProcessWarning('Warning: no files to process', 1);

  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

procedure TBeeApp.DecodeShell(Action: THeaderAction);
var
  Decoder: TDecoder;
  Headers: THeaders;
  Return: boolean;
  I: integer;
begin
  ProcessMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toNone) then
  begin
    ProcessMessage(msgScanning + '...');

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

    FTotalSize := Headers.GetSize(Action);
    if (Headers.GetNext(0, Action) > -1) then // action = toTest or toExtract
    begin
      ProcessFilesToDecode(Headers, Action);

      Return  := True;
      Decoder := TDecoder.Create(FArcFile, Self);
      for I := 0 to Headers.Count - 1 do
      begin
        if not FTerminated then
          case THeader(Headers.Items[I]).Action of
            toExtract: Return := Decoder.DecodeFile(Headers.Items[I], pmNorm);
            toTest:    Return := Decoder.DecodeFile(Headers.Items[I], pmTest);
            toSkip:    Return := Decoder.DecodeFile(Headers.Items[I], pmSkip);
            toQuit:    Return := Decoder.Decodefile(Headers.Items[I], pmQuit);
          end;
        if Return = False then Break;
      end;
      Decoder.Destroy;

      if not FTerminated then
      begin
        if Return = True then
          ProcessMessage(Cr + 'Everything went ok - ' + TimeDifference(FTotalTime) + ' seconds')
        else
          ProcessError(Cr + 'Process aborted, a fatal error occourred - ' + TimeDifference(FTotalTime) + ' seconds', 2);
      end else
        ProcessError(Cr + 'Process aborted - ' + TimeDifference(FTotalTime) + ' seconds', 255);

    end else // if Headers.GetNext
      ProcessWarning('Warning: no files to decode', 1);
  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

procedure TBeeApp.DeleteShell;
var
  TmpFileName: string;
  TmpFile: TFileWriter;
  I:    integer;
  Headers: THeaders;
  Encoder: TEncoder;
begin
  ProcessMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    ProcessMessage(msgScanning + '...');

    with FCommandLine do
    begin
      Headers.MarkItems(FileMasks, toCopy, toDelete, rOption);
      Headers.MarkItems(xOption, toDelete, toCopy, rOption);
    end;

    if (Headers.GetNext(0, toDelete) > -1) or
      ((Length(FCommandLine.aOption) > 0) and
      (Headers.GetNext(0, toCopy) > -1)) then
    begin
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
          FSwapFile :=
            CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);

        // set sfx module
        with FCommandLine do
          if Length(aOption) > 0 then
            Headers.SetModule(aOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Self);
        for I := 0 to Headers.Count - 1 do
          if not FTerminated then
            case THeader(Headers.Items[I]).Action of
              toCopy:   Encoder.CopyStrm(Headers.Items[I], emNorm, FArcFile);
              toSwap:   Encoder.EncodeStrm(Headers.Items[I], emNorm, FSwapFile);
              toDelete: ProcessMessage(msgDeleting + THeader(Headers.Items[I]).Data.FileName);
            end;
        Encoder.Destroy;
        Headers.WriteItems(TmpFile);

        if not FTerminated then
          ProcessMessage (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(FTotalTime) + ' seconds')
        else
          ProcessError(Cr + 'Process aborted - ' + TimeDifference(FTotalTime) + ' seconds', 255);

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        if not FTerminated then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
            ProcessError('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName, 2)
          else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption  
          end;
        end else
          SysUtils.DeleteFile(TmpFileName);

      end else // if ProcessFilesToSwap
      begin
        if TmpFile = nil then
          ProcessError('Error: can''t open temp file', 2)
        else
          ProcessError('Error: can''t decode solid sequences', 2);

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        SysUtils.DeleteFile(TmpFileName);
      end;

    end else // if Headers.GetNext
      ProcessWarning('Warning: no files to delete', 1);
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
  I:    integer;
begin
  ProcessMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    ProcessMessage(msgScanning + '...');

    if ProcessFilesToRename(Headers) then
    begin
      TmpFileName := GenerateFileName(FCommandLine.yOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);

      if (TmpFile <> nil) then
      begin
        FTotalSize := Headers.GetPackedSize([toCopy, toRename]);

        // set sfx module
        with FCommandLine do
          if Length(aOption) > 0 then
            Headers.SetModule(aOption);

        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Self);
        for I := 0 to Headers.Count - 1 do
        begin
          if not FTerminated then
          begin
            Encoder.CopyStrm(Headers.Items[I], emNorm, FArcFile);
          end;
        end;
        Encoder.Destroy;
        Headers.WriteItems(TmpFile);

        if not FTerminated then
          ProcessMessage(Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(FTotalTime) + ' seconds')
        else
          ProcessError(Cr + 'Process aborted - ' + TimeDifference(FTotalTime) + ' seconds', 255);

        if Assigned(FArcFile) then FreeAndNil(FArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        if not FTerminated then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
            ProcessError('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName, 2)
          else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption
          end;
        end else
          SysUtils.DeleteFile(TmpFileName);

      end else // if (TmpFile <> nil)
      begin
        if TmpFile = nil then
          ProcessError('Error: can''t open temp file', 2)
        else
          ProcessError('Error: can''t decode solid sequences', 2);

        if Assigned(FArcFile) then FreeAndNil(FArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(TmpFileName);
      end;

    end else // if ProcessFilesToRename
      ProcessWarning('Warning: no files to rename', 1);
  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

function CompareFn(Item1, Item2: pointer): integer;
begin
  Result := CompareFileName(
    ExtractFilePath(THeader(Item1).Data.FileName),
    ExtractFilePath(THeader(Item2).Data.FileName));

  if Result = 0 then
    Result := CompareText(
      ExtractFileName(THeader(Item1).Data.FileName),
      ExtractFileName(THeader(Item2).Data.FileName));
end;

procedure TBeeApp.ListShell;
var
  P: THeader;
  I: integer;
  Headers: THeaders;
  FileInfo: TFileInfoExtra;
  {$IFDEF CONSOLEAPPLICATION}
  HeadersToList: TList;
  HeadersToListPath: string;
  {$ENDIF}
  TotalPack, TotalSize, TotalFiles: integer;
  Version, Method, Dictionary: integer;
begin
  ProcessMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create;
  {$IFDEF CONSOLEAPPLICATION}
  HeadersToList := TList.Create;
  {$ENDIF}
  if OpenArchive(Headers, toNone) then
  begin
    ProcessMessage(msgScanning + '...');

    with FCommandLine do
    begin
      Headers.MarkItems(FileMasks, toNone, toList, rOption);
      Headers.MarkItems(xOption, toList, toNone, rOption);
    end;

    if (Headers.GetNext(0, toList) > -1) then
    begin
      {$IFDEF CONSOLEAPPLICATION}
      ProcessMessage(StringOfChar('-', 79));
      ProcessMessage('Directory|File' + StringOfChar(' ', 8) + 'Size     Packed Ratio     Date  Time   Attr      CRC Meth');
      ProcessMessage(StringOfChar('-', 79));
      {$ENDIF}

      Version    := -1;
      Method     := -1;
      Dictionary := -1;

      for I := 0 to Headers.Count - 1 do
      begin
        P := Headers.Items[I];

        if foVersion in P.Data.FileFlags then
          Version := P.Data.FileVersion;

        if foMethod in P.Data.FileFlags then
          Method := P.Data.FileMethod;

        if foDictionary in P.Data.FileFlags then
          Dictionary := P.Data.FileDictionary;

        if P.Action = toList then
        begin
          P.Data.FileVersion := Version;
          P.Data.FileMethod := Method;
          P.Data.FileDictionary := Dictionary;
          {$IFDEF CONSOLEAPPLICATION}
          HeadersToList.Add(P);
          {$ENDIF}
        end;
      end;

      TotalSize  := 0;
      TotalPack  := 0;
      TotalFiles := 0;

      {$IFDEF CONSOLEAPPLICATION}
      HeadersToList.Sort(CompareFn);
      HeadersToListPath := '';
      {$ENDIF}

      {$IFDEF CONSOLEAPPLICATION}
      for I := 0 to HeadersToList.Count -1 do
      begin
        P := HeadersToList.Items[I];
      {$ELSE}
      for I := 0 to Headers.Count -1 do
      begin
        P := Headers.Items[I];
      {$ENDIF}

        with FileInfo do
        begin
          FileName := StringToPChar(ExtractFileName(P.Data.FileName));
          FilePath := StringToPChar(ExtractFilePath(P.Data.FileName));

          {$IFDEF CONSOLEAPPLICATION}
          if CompareFileName(HeadersToListPath, FilePath) <> 0 then
          begin
            HeadersToListPath := FilePath;
            if I = 0 then
              ProcessMessage(HeadersToListPath)
            else
              ProcessMessage(Cr + HeadersToListPath);
          end;
          {$ENDIF}

          FileSize   := P.Data.FileSize;
          FilePacked := P.Data.FilePacked;

          if FileSize > 0 then
            FileRatio := MulDiv(FilePacked, 100, FileSize)
          else
            FileRatio := 100;

          FileAttr    := P.Data.FileAttr;
          FileTime    := P.Data.FileTime;
          FileComm    := StringToPChar('');
          FileCrc     := P.Data.FileCrc;
          FileMethod  := StringToPChar(MethodToStr(P));
          FileVersion := StringToPChar(VersionToStr(P));

          if foPassword in P.Data.FileFlags then
            FilePassword := StringToPchar('Yes')
          else
            FilePassword := StringToPchar('No');

          {$IFDEF CONSOLEAPPLICATION}
          FilePosition := Headers.GetNext(0, toList, P.Data.FileName);
          {$ELSE}
          FilePosition := I;
          {$ENDIF}
        end;
        ProcessList(FileInfo);

        StrDispose(FileInfo.FileName);
        StrDispose(FileInfo.FilePath);
        StrDispose(FileInfo.FileComm);
        StrDispose(FileInfo.FileMethod);
        StrDispose(FileInfo.FileVersion);
        StrDispose(FileInfo.FilePassword);

        Inc(TotalSize, P.Data.FileSize);
        Inc(TotalPack, P.Data.FilePacked);
        Inc(TotalFiles);
      end;
      {$IFDEF CONSOLEAPPLICATION}
      ProcessMessage(StringOfChar('-', 79));
      ProcessMessage(Format('%d files', [TotalFiles]) + StringOfChar(' ', 15 - Length((Format('%d files', [TotalFiles])))) + (Format(' %10s %10s %5s', [SizeToStr(TotalSize), SizeToStr(TotalPack), RatioToStr(TotalPack, TotalSize)])));
      {$ENDIF}

      {$IFDEF CONSOLEAPPLICATION}
      // self-extractor module size
      if Headers.GetModule > 0 then
        ProcessMessage(Cr + 'Note: Bee Self-Extractor module founded');
      {$ENDIF}
      ProcessMessage(Cr + 'Everything went ok - ' + TimeDifference(FTotalTime) + ' seconds');
    end else
      ProcessWarning('Warning: no files to list', 1);

  end;
  {$IFDEF CONSOLEAPPLICATION}
  HeadersToList.Free;
  {$ENDIF}
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

// -------------------------------------------------------------------------- //
// String routines                                                            //
// -------------------------------------------------------------------------- //

function TBeeApp.MethodToStr(P: THeader): string;
begin
  Result := 'm0a';

  if not (foTear in P.Data.FileFlags) then
    Result[1] := 's';

  if not (foMoved in P.Data.FileFlags) then
    if P.Data.FileMethod in [1..3] then
      Result[2] := char(byte('0') + P.Data.FileMethod)
    else
      Result[2] := '?';

  if P.Data.FileDictionary in [0..9] then
    Result[3] := char(byte('a') + P.Data.FileDictionary)
  else
    Result[3] := '?';
end;

function TBeeApp.VersionToStr(P: THeader): string;
begin
  case P.Data.FileVersion of
    0: Result := ' 0' + DecimalSeparator + '2';
    1: Result := ' 0' + DecimalSeparator + '3';
  else Result := ' 0' + DecimalSeparator + '0';
  end;
end;

end.

