{
  Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1046 - 2009.08.20 by Melchiorre Caruso.
}

unit Bee_App;

{$I compiler.inc}

interface

uses
  Classes,           // TStringList, ...
  // ---
  Bee_Files,
  Bee_Types,
  Bee_Common,        // Various helper routines
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
    function  OpenArchive(Headers: THeaders; aAction: THeaderAction): boolean;
    // decode solid sequences using a swapfile
    function  ProcessFilesToSwap(Headers: THeaders): boolean;
    // find and prepare sequences
    function  ProcessFilesToAdd(Headers: THeaders): int64;
    procedure ProcessFilesToFresh(Headers: THeaders);
    procedure ProcessFilesToDelete(Headers: THeaders);
    procedure ProcessFilesToDecode(Headers: THeaders; aAction: THeaderAction);
    procedure ProcessFilesToExtract(Headers: THeaders);
    function  ProcessFilesToRename(Headers: THeaders): boolean;
    procedure ProcessFilesDeleted(Headers: THeaders);
    // overwrite sub-routines
    function  ProcessFileToOverWrite4Add(Headers: THeaders; Item: THeader; New: TCustomSearchRec): TUpdateMode;
    function  ProcessFileToOverWrite4Extract(Headers: THeaders; Item: THeader): TUpdateMode;
    // already file exists in archive
    function AlreadyFileExists(Headers: THeaders; aIndex: longint;
      aActions: THeaderActions; const aFileName: string): longint; overload;

    function AlreadyFileExists(Headers: THeaders; const
      aFileName: string): longint; overload;

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
  SysUtils,       // faReadOnly, ...
  // ---
  Bee_Consts,
  Bee_MainPacker; // TEncoder...

// TBeeApp ...

constructor TBeeApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  Randomize; // randomize, uses for unique filename generation...

  FSelfName := 'The Bee 0.8.0 build 1106 archiver utility, Dec 2009' + Cr +
               '(C) 1999-2009 Andrew Filinsky and Melchiorre Caruso';

  FArcFile  := nil;
  FSwapName := '';
  FSwapFile := nil;

  // process command line
  FCommandLine := TCommandLine.Create;
  FCommandLine.CommandLine := aParams.Text;

  // load configuration
  FConfiguration := TConfiguration.Create;
  if not FileExists(FCommandLine.cfgOption) then
    DoWarning('Warning: configuration file "' + FCommandLine.cfgOption + '" not found, data will be stored' + Cr, 1)
  else
    FConfiguration.LoadFromFile(FCommandLine.cfgOption);

  // load method and dictionary level
  FConfiguration.Selector('\main');
  FConfiguration.CurrentSection.Values['Method']     := IntToStr(Ord(FCommandLine.mOption));
  FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(Ord(FCommandLine.dOption));

  // set thread priority
  SetPriority(Ord(FCommandLine.priOption));
end;

destructor TBeeApp.Destroy;
begin
  FConfiguration.Destroy;
  FCommandLine.Destroy;
  inherited Destroy;
end;

procedure TBeeApp.DisplayUsage;
begin
  DoMessage(Cr + '  Usage: Bee <Command> -<Option 1> -<Option N> <ArchiveName> <FileNames...>');
  DoMessage(Cr + '  Commands:' + Cr);
  DoMessage('    a   Add files to archive');
  DoMessage('    d   Delete files from archive');
  DoMessage('    e   Extract files from archive');
  DoMessage('    x   eXtract files from archive with path name');
  DoMessage('    l   List archive');
  DoMessage('    t   Test archive files');
  DoMessage('    r   Rename files in archive');
  DoMessage(Cr + '  Options:' + Cr);
  DoMessage('    r       Recurse subdirectories');
  DoMessage('    u       Update files');
  DoMessage('    f       Freshen files');
  DoMessage('    e       force file Extention');
  DoMessage('    s       create Solid archive');
  DoMessage('    a       add self-extrActor module');
  DoMessage('    o<mode> set overwrite file Mode (Q-Quit, A-All, S-Skip all)');
  DoMessage('    m<0..3> set compression Method (0-store...1-default...3-maximal)');
  DoMessage('    d<0..9> set Dictionary size (d1 uses < 5M, d2 (default) < 10M, d3 < 20M...)' + Cr);
  DoMessage('    x       eXclude filenames');
  DoMessage('    t       Test archive after process');
  DoMessage('    l       List archive after process');
  DoMessage('    y       set temporany directory');
  DoMessage('    k       use blowfish crypter/decrypter (min key-length 4 bytes)');
  DoMessage('    v       show technical information for l (List) command)');
  DoMessage('    cd<dir> set current archive directory' + Cr);
  DoMessage('    cfg<filename> use specified configuration file');
  DoMessage('    pri<0..3>     set process Priority (0-Idle, 1-Normal, 2-High, 3-RealTime)');
  DoMessage(Cr + '  Use BeeOpt to make most optimal parameters.' + Cr);
end;

procedure TBeeApp.Execute;
begin
  inherited Execute;
  DoMessage(FSelfName);
  with FCommandLine do
    if (Command <> ccNone) and (ArchiveName > '') then
      case Command of
        ccAdd: EncodeShell;
        ccDelete: DeleteShell;
        ccExtract: DecodeShell(toExtract);
        ccList: ListShell;
        ccRename: RenameShell;
        ccTest: DecodeShell(toTest);
        ccxExtract: DecodeShell(toExtract);
        ccHelp: DisplayUsage;
      end
    else
      DisplayUsage;
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
      if (Headers.GetCount = 0) and (FArcFile.Size <> 0) then
      begin
        Result := False;  DoFatalError('Error: archive unsupported', ccError);
      end;
    except
      Result := False;  DoFatalError('Error: can''t open archive', ccError);
    end;
  end;
end;

function TBeeApp.AlreadyFileExists(Headers: THeaders; aIndex: longint;
  aActions: THeaderActions; const aFileName: string): longint;
begin
  if Length(aFileName) > 0 then
  begin
    Result := Headers.GetBack(aIndex - 1, aActions, aFileName);
    if Result = -1 then
    begin
      Result := Headers.GetNext(aIndex + 1, aActions, aFileName);
    end;
  end else
    Result := -1;
end;

function TBeeApp.AlreadyFileExists(Headers: THeaders; const aFileName: string): longint;
var
  I:longint;
begin
  Result := -1;
  if Length(aFileName) > 0 then
    for I := 0 to Headers.GetCount - 1 do
      if CompareFileName(aFileName, Headers.GetItem(I).FileName) = 0 then
      begin
        Result := I;
        Break;
      end;
end;

// -------------------------------------------------------------------------- //
// Add file processing                                                        //
// -------------------------------------------------------------------------- //

function TBeeApp.ProcessFileToOverWrite4Add(Headers: THeaders; Item: THeader; New: TCustomSearchRec): TUpdateMode;
var
  FI: TFileInfo;
  S: string;
begin
  FI.FileName := StringToPChar(ExtractFileName(Item.FileName));
  FI.FilePath := StringToPChar(ExtractFilePath(Item.FileName));

  FI.FileSize := Item.FileSize;
  FI.FileTime := Item.FileTime;
  FI.FileAttr := Item.FileAttr;

  case DoOverwrite(FI, omAddReplace) of
    omAdd:     Result := umAdd;
    omUpdate:  Result := umUpdate;
    omReplace: Result := umReplace;

    omRename:
    begin
      while True do
      begin
        S := FixFileName(DoRename(FI, ''));
        if Length(S) > 0 then
        begin
          if AlreadyFileExists(Headers, S) > -1 then
            DoWarning('Warning: file "' + S + '" already existing in archive', 0)
          else
            Break;
        end else
          Break;
      end;
      New.FileName := S;

      Result := umAdd;
    end;

    omAddUpdate:
    begin
      FCommandLine.uOption := umAddUpdate;
      Result := umAddUpdate;
    end;

    omAddReplace:
    begin
      FCommandLine.uOption := umAddReplace;
      Result := umAddReplace;
    end;

    omAddAutoRename:
    begin
      FCommandLine.uOption := umAddAutoRename;
      Result := umAddAutoRename;
    end;

    omSkip:
    begin
      Result := umAddQuery;
    end;

    omQuit:
    begin
      DoFatalError('Process aborted' , ccUserAbort);
      Result :=  umAddQuery;
    end;
  end;

  StrDispose(FI.FileName);
  StrDispose(FI.FilePath);
end;

function TBeeApp.ProcessFilesToAdd(Headers: THeaders): int64;
var
  I: longint;
  P: THeader;
  U: TUpdateMode;
  Scanner: TFileScanner;
begin
  Result  := 0;
  Scanner := TFileScanner.Create;
  with FCommandLine do
    for I := 0 to FileMasks.Count - 1 do
    begin
      Scanner.Scan(FileMasks[I], xOptions, rOption);
    end;

  for I := 0 to Scanner.Count -1 do
  begin
    P := Headers.SearchItem(Scanner.Items[I].FileName);

    if (P = nil) or (U <> umAddQuery) then
      U := FCommandLine.uOption
    else
      U := ProcessFileToOverwrite4Add(Headers, P, Scanner.Items[I]);

    if Terminated = False then
      case U of
      //umAddQuery:      Nothing to do
        umAdd:           Result := Result + Headers.AddItem       (Scanner.Items[I], P);
        umUpdate:        Result := Result + Headers.UpdateItem    (Scanner.Items[I], P);
        umReplace:       Result := Result + Headers.ReplaceItem   (Scanner.Items[I], P);
        umAddUpdate:     Result := Result + Headers.AddUpdateItem (Scanner.Items[I], P);
        umAddReplace:    Result := Result + Headers.AddReplaceItem(Scanner.Items[I], P);
        umAddAutoRename:
        with Scanner.Items[I] do
        begin
          repeat
            FileName := GenerateAlternativeFileName(FileName, False);
          until Headers.SearchItem(FileName) = nil;
          Result := Result + Headers.AddItem(Scanner.Items[I], P);
        end;
      end;
  end;
  Headers.SortNews(FConfiguration);
  Scanner.Destroy;
end;

// -------------------------------------------------------------------------- //
// Extract file processing                                                    //
// -------------------------------------------------------------------------- //

function TBeeApp.ProcessFileToOverWrite4Extract(Headers: THeaders; Item: THeader): TUpdateMode;
begin


end;

procedure TBeeApp.ProcessFilesToExtract;
var
  I: longint;
  P: THeader;
  U: TUpdateMode;
begin
  for I := 0 to Headers.GetCount - 1 do
  begin
    P := Headers.GetItem(I);
    if P.FileAction = toExtract then
    begin
      if FCommandLine.Command <> ccExtract then
        P.FileName := DeleteFilePath(FCommandLine.cdOption, P.FileName)
      else
        P.FileName := ExtractFileName(P.FileName);

      if not FileExists(P.FileName) then
        U := FCommandLine.uOption
      else
        U := ProcessFileToOverwrite4Extract(Headers, P);

      case U of
      //umReplace:    nothing to do
      //umAddReplace: nothing to do
      //umAddQuery:   nothing to do
        umAdd:
        if FileExists(P.FileName) then
        begin
          P.FileAction := toNone;
        end;
        umUpdate:
        if (not FileExists(P.FileName)) or (P.FileTime <= FileAge(P.FileName)) then
        begin
          P.FileAction := toNone;
        end;
        umAddUpdate:
        if FileExists(P.FileName) and (P.FileTime <= FileAge(P.FileName))then
        begin
          P.FileAction := toNone;
        end;
        umAddAutoRename:
        while FileExists(P.FileName) do
        begin
          P.FileName := GenerateAlternativeFileName(P.FileName, False);
          if AlreadyFileExists(Headers, -1, [toExtract], P.FileName) = -1 then
          begin
            Break;
          end;
        end;
      end;
    end;
  end;
end;

// -------------------------------------------------------------------------- //
// Rename file processing                                                     //
// -------------------------------------------------------------------------- //

function TBeeApp.ProcessFilesToRename(Headers: THeaders): boolean;
var
  S: string;
  I: longint;
  P: THeader;
  FI: TFileInfo;
begin
  Headers.MarkItems(FCommandLine.FileMasks, toCopy,   toRename);
  Headers.MarkItems(FCommandLine.xOptions,   toRename, toCopy);

  if Headers.GetNext(0, toRename) > -1 then
  begin
    Result := False;
    for I := 0 to Headers.GetCount -1 do
    begin
      P := Headers.GetItem(I);

      if P.FileAction = toRename then
      begin
        FI.FileName := StringToPChar(ExtractFileName(P.FileName));
        FI.FilePath := StringToPChar(ExtractFilePath(P.FileName));

        FI.FileSize := P.FileSize;
        FI.FileTime := P.FileTime;
        FI.FileAttr := P.FileAttr;
        while True do
        begin
          S := FixFileName(DoRename(FI, ''));
          if Length(S) > 0 then
          begin
            if AlreadyFileExists(Headers, I, [toCopy, toRename], S) > -1 then
              DoWarning('Warning: file "' + S + '" already existing in archive', 0)
            else
              Break;
          end else
            Break;
        end;
        StrDispose(FI.FileName);
        StrDispose(FI.FilePath);

        if (Length(S) > 0) and (CompareFileName(S, P.FileName) <> 0) then
        begin
          P.FileName := S;
          Result := True;
        end;
      end;
      if Terminated then Break;
    end;
    if Result then Result := not Terminated;
  end else
    Result := ((Length(FCommandLine.sfxOption) > 0) and (Headers.GetNext(0, toCopy) > -1));
end;

// -------------------------------------------------------------------------- //
// Sequences processing                                                       //
// -------------------------------------------------------------------------- //

procedure TBeeApp.ProcessFilesToFresh(Headers: THeaders);
var
  I, J, BackTear, NextTear: longint;
  P: THeader;
begin
  I := Headers.GetBack(Headers.GetCount -1, toFresh);
  // find sequences and mark as toSwap files that not toFresh
  while I > -1 do
  begin
    BackTear := Headers.GetBack(I, foTear);
    NextTear := Headers.GetNext(I + 1, foTear);

    if NextTear = -1 then NextTear := Headers.GetCount;
    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := Headers.GetBack(NextTear - 1, toCopy);
      for J := BackTear to NextTear do
      begin
        P := Headers.GetItem(J);
        case P.FileAction of
          toCopy:  begin
                     P.FileAction := toSwap;
                     Inc(FTotalSize, P.FileSize * 2);
                   end;
          toFresh: Inc(FTotalSize, P.FileSize);
        end;
      end;
      I := BackTear;
    end;
    I := Headers.GetBack(I - 1, toFresh);
  end;
  Inc(FTotalSize, Headers.GetPackedSize(toCopy));
end;

procedure TBeeApp.ProcessFilesToDelete(Headers: THeaders);
var
  I, J, BackTear, NextTear: longint;
  P: THeader;
begin
  I := Headers.GetBack(Headers.GetCount -1, toDelete);
  // find sequences and ...
  while I > -1 do
  begin
    BackTear := Headers.GetBack(I, foTear);
    NextTear := Headers.GetNext(I + 1, foTear);

    if NextTear = -1 then NextTear := Headers.GetCount;
    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := Headers.GetBack(NextTear - 1, toCopy);
      // if exists an header toDelete
      if Headers.GetBack(NextTear, toDelete) > (BackTear - 1) then
        for J := BackTear to NextTear do
        begin
          P := Headers.GetItem(J);
          case P.FileAction of
            toCopy:   begin
                        P.FileAction := toSwap;
                        Inc(FTotalSize, P.FileSize * 2);
                      end;
            toDelete: Inc(FTotalSize, P.FileSize);
          end;
        end;
      I := BackTear;
    end;
    I := Headers.GetBack(I - 1, toDelete);
  end;
  Inc(FTotalSize, Headers.GetPackedSize(toCopy));
end;

function TBeeApp.ProcessFilesToSwap(Headers: THeaders): boolean;
var
  P: THeader;
  I, J: longint;
  Decoder: TDecoder;
  FSwapStrm: TFileWriter;
  iDictionary, iTable, iTear: longint;
  CurrDictionary, CurrTable: longint;
begin
  Result := True;

  I := Headers.GetBack(Headers.GetCount -1, toSwap);
  if (I > -1) then
  begin
    FSwapName := GenerateFileName(FCommandLine.wdOption);
    FSwapStrm := CreateTFileWriter(FSwapName, fmCreate);

    if (FSwapStrm <> nil) then
    begin
      CurrDictionary := Headers.GetCount;
      CurrTable      := CurrDictionary;

      Decoder := TDecoder.Create(FArcFile, Self);

      while (I > -1) and (not FTerminated) do
      begin
        iDictionary := Headers.GetBack(I, foDictionary); // find dictionary info
        iTable := Headers.GetBack(I, foTable);           // find table info
        iTear  := Headers.GetBack(I, foTear);            // find tear info

        if (iDictionary > -1) and (iDictionary <> CurrDictionary) and (iDictionary <> iTear) then
        begin
          CurrDictionary := iDictionary;
          P := Headers.GetItem(CurrDictionary);
          Decoder.DecodeStrm(P, pmQuit, FSwapStrm, P.FileSize, foPassword in P.FileFlags);
        end;

        if (iTable > -1) and (iTable <> CurrTable) and (iTable <> iTear) then
        begin
          CurrTable := iTable;
          P := Headers.GetItem(CurrTable);
          Decoder.DecodeStrm(P, pmQuit, FSwapStrm, P.FileSize, foPassword in P.FileFlags);
        end;

        for J := iTear to I do
        begin
          P := Headers.GetItem(J);
          if not FTerminated then
          begin
            if P.FileAction = toSwap then
              Result := Decoder.DecodeStrm(P, pmNorm, FSwapStrm, P.FileSize, foPassword in P.FileFlags)
            else
              Result := Decoder.DecodeStrm(P, pmSkip, FSwapStrm, P.FileSize, foPassword in P.FileFlags);
          end else
            Result := False;

          if Result = False then Break;
        end;
        if Result = False then Break;

        I := Headers.GetBack(iTear - 1, toSwap);
      end;
      Decoder.Destroy;
      FreeAndNil(FSwapStrm);

    end else
      Result := False;
  end;
end;

procedure TBeeApp.ProcessFilesDeleted(Headers: THeaders);
var
  I: longint;
  Back, Next: THeader;
begin
  // rescue header informations
  for I := 0 to Headers.GetCount -2 do
  begin
    Back := Headers.GetItem(I);
    Next := Headers.GetItem(I + 1);

    if Back.FileAction = toDelete then
    begin
      if (foVersion in Back.FileFlags) and (not(foVersion in Next.FileFlags)) then
      begin
        Next.FileVersion := Back.FileVersion;
        Include(Next.FileFlags, foVersion);
      end;

      if (foMethod in Back.FileFlags) and (not(foMethod in Next.FileFlags)) then
      begin
        Next.FileMethod := Back.FileMethod;
        Include(Next.FileFlags, foMethod);
      end;

      if (foDictionary in Back.FileFlags) and (not(foDictionary in Next.FileFlags)) then
      begin
        Next.FileDictionary := Back.FileDictionary;
        Include(Next.FileFlags, foDictionary);
      end;

      if (foTable in Back.FileFlags) and (not(foTable in Next.FileFlags)) then
      begin
        Next.FileTable := Back.FileTable;
        Include(Next.FileFlags, foTable);
      end;

      if (foTear in Back.FileFlags) and (not(foTear in Next.FileFlags)) then
      begin
        Include(Next.FileFlags, foTear);
      end;
    end;
  end;
end;

procedure TBeeApp.ProcessFilesToDecode;
var
  P: THeader;
  I, J: longint;
  iDictionary, iTable, iTear: longint;
begin
  I := Headers.GetBack(Headers.GetCount -1, aAction); // last header
  while I > -1 do
  begin
    iDictionary := Headers.GetBack(I, foDictionary);  // find dictionary info
    iTable := Headers.GetBack(I, foTable);            // find table info
    iTear := Headers.GetBack(I, foTear);              // find tear info

    for J := iTear to (I -1) do
    begin
      P := Headers.GetItem(J);
      if P.FileAction in [toNone, toQuit] then
      begin
        P.FileAction := toSkip;
        Inc(FTotalSize, P.FileSize);
      end;
    end;

    if iDictionary > -1 then
    begin
      P := Headers.GetItem(iDictionary);
      if P.FileAction = toNone then P.FileAction := toQuit;
    end;

    if iTable > -1 then
    begin
      P := Headers.GetItem(iTable);
      if P.FileAction = toNone then P.FileAction := toQuit;
    end;

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

    FCommandLine.rOption := rmFull;
    FCommandLine.xOptions.Clear;
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

    FCommandLine.rOption := rmFull;
    FCommandLine.xOptions.Clear;
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
  I: longint;
  P: THeader;
  Encoder: TEncoder;
  TmpFileName: string;
  TmpFile: TFileWriter;
  Headers: THeaders;
begin
  DoMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create(FCommandLine);
  if OpenArchive(Headers, toCopy) then
  begin
    DoMessage(msgScanning + '...');

    // process FileMasks and xFileMasks
    ProcessFilesToAdd(Headers);

    if (Headers.GetCount([toUpdate, toFresh]) > 0) or ((Length(FCommandLine.sfxOption) > 0) and (Headers.GetNext(0, toCopy) > -1)) then
    begin
      TmpFileName := GenerateFileName(FCommandLine.wdOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);

      // find sequences and...
      ProcessFilesToFresh(Headers);

      // decode solid header modified in a swap file
      if (TmpFile <> nil) and ProcessFilesToSwap(Headers) then
      begin
        // if exists a modified solid sequence open swap file
        if Length(FSwapName) > 0 then
          FSwapFile := CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);

        // set sfx module
        with FCommandLine do
          if Length(sfxOption) > 0 then
            Headers.SetModule(sfxOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Self);
        for I := 0 to Headers.GetCount -1 do
        begin
          if not FTerminated then
          begin
            P := Headers.GetItem(I);
            case P.FileAction of
              toCopy:   Encoder.CopyStrm  (P, emNorm, FArcFile, P.FileStartPos, P.FilePacked, False);
              toSwap:   Encoder.EncodeStrm(P, emNorm, FSwapFile, P.FileSize, foPassword in P.FileFlags);
              toFresh:  Encoder.EncodeFile(P, emNorm);
              toUpdate: Encoder.EncodeFile(P, emNorm);
            end;
          end;
        end;
        Encoder.Destroy;
        // rewrite Headers
        Headers.WriteItems(TmpFile);

        if not Terminated then
          DoMessage(Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(FStartTime) + ' seconds')
        else
          DoError(Cr + 'Process aborted - ' + TimeDifference(FStartTime) + ' seconds', 255);

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        DeleteFile(FSwapName);
        if not FTerminated then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
            DoError('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName, 2)
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
          DoError('Error: can''t open temp file',2)
        else
          DoError('Error: can''t decode solid sequences', 2);

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        SysUtils.DeleteFile(TmpFileName);
      end;

    end else // if Headers.GetCount
      DoWarning('Warning: no files to process', 1);

  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

procedure TBeeApp.DecodeShell(Action: THeaderAction);
var
  Decoder: TDecoder;
  Headers: THeaders;
  Return: boolean;
  P: THeader;
  I: longint;
begin
  DoMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create(FCommandLine);
  if OpenArchive(Headers, toNone) then
  begin
    DoMessage(msgScanning + '...');

    Headers.MarkItems(FCommandLine.FileMasks, toNone, Action);
    Headers.MarkItems(FCommandLine.xOptions, Action, toNone);

    if (Action = toExtract) then
    begin
      ProcessFilesToExtract(Headers);
      // ProcessFilesToOverWrite4Extract(Headers);
    end;

    FTotalSize := Headers.GetSize(Action);
    if (Headers.GetNext(0, Action) > -1) then // action = toTest or toExtract
    begin
      ProcessFilesToDecode(Headers, Action);

      Return  := True;
      Decoder := TDecoder.Create(FArcFile, Self);
      for I := 0 to Headers.GetCount -1 do
      begin

        if not FTerminated then
        begin
          P := Headers.GetItem(I);
          case P.FileAction of
            toExtract: Return := Decoder.DecodeFile(P, pmNorm);
            toTest:    Return := Decoder.DecodeFile(P, pmTest);
            toSkip:    Return := Decoder.DecodeFile(P, pmSkip);
            toQuit:    Return := Decoder.Decodefile(P, pmQuit);
          end;
        end;
        if Return = False then Break;
      end;
      Decoder.Destroy;

      if not FTerminated then
      begin
        if Return = True then
          DoMessage(Cr + 'Everything went ok - ' + TimeDifference(FStartTime) + ' seconds')
        else
          DoError(Cr + 'Process aborted, a fatal error occourred - ' + TimeDifference(FStartTime) + ' seconds', 2);
      end else
        DoError(Cr + 'Process aborted - ' + TimeDifference(FStartTime) + ' seconds', 255);

    end else // if Headers.GetNext
      DoWarning('Warning: no files to decode', 1);
  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

procedure TBeeApp.DeleteShell;
var
  TmpFileName: string;
  TmpFile: TFileWriter;
  I: longint;
  P: THeader;
  Headers: THeaders;
  Encoder: TEncoder;
begin
  DoMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create(FCommandLine);
  if OpenArchive(Headers, toCopy) then
  begin
    DoMessage(msgScanning + '...');

    Headers.MarkItems(FCommandLine.FileMasks, toCopy, toDelete);
    Headers.MarkItems(FCommandLine.xOptions, toDelete, toCopy);

    if (Headers.GetNext(0, toDelete) > -1) or
      ((Length(FCommandLine.sfxOption) > 0) and
      (Headers.GetNext(0, toCopy) > -1)) then
    begin
      TmpFileName := GenerateFileName(FCommandLine.wdOption);
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
          if Length(sfxOption) > 0 then
            Headers.SetModule(sfxOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Self);
        for I := 0 to Headers.GetCount -1 do
        begin
          if not FTerminated then
          begin
            P := Headers.GetItem(I);
            case P.FileAction of
              toCopy:   Encoder.CopyStrm  (P, emNorm, FArcFile, P.FileStartPos, P.FilePacked, False);
              toSwap:   Encoder.EncodeStrm(P, emNorm, FSwapFile, P.FileSize, foPassword in P.FileFlags);
              toDelete: DoMessage(msgDeleting + P.FileName);
            end;
          end;
        end;
        Encoder.Destroy;
        Headers.WriteItems(TmpFile);

        if not FTerminated then
          DoMessage (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(FStartTime) + ' seconds')
        else
          DoError(Cr + 'Process aborted - ' + TimeDifference(FStartTime) + ' seconds', 255);

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        if not FTerminated then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
            DoError('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName, 2)
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
          DoError('Error: can''t open temp file', 2)
        else
          DoError('Error: can''t decode solid sequences', 2);

        if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
        if Assigned(FArcFile)  then FreeAndNil(FArcFile);
        if Assigned(TmpFile)   then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(FSwapName);
        SysUtils.DeleteFile(TmpFileName);
      end;

    end else // if Headers.GetNext
      DoWarning('Warning: no files to delete', 1);
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
  P: THeader;
  I: longint;
begin
  DoMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create(FCommandLine);
  if OpenArchive(Headers, toCopy) then
  begin
    DoMessage(msgScanning + '...');

    if ProcessFilesToRename(Headers) then
    begin
      TmpFileName := GenerateFileName(FCommandLine.wdOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);

      if (TmpFile <> nil) then
      begin
        FTotalSize := Headers.GetPackedSize([toCopy, toRename]);

        // set sfx module
        if Length(FCommandLine.sfxOption) > 0 then
        begin
          Headers.SetModule(FCommandLine.sfxOption);
        end;

        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Self);
        for I := 0 to Headers.GetCount -1 do
        begin
          if not FTerminated then
          begin
            P := Headers.GetItem(I);
            Encoder.CopyStrm(P, emNorm, FArcFile, P.FileStartPos, P.FilePacked, False);
          end;
        end;
        Encoder.Destroy;
        Headers.WriteItems(TmpFile);

        if not FTerminated then
          DoMessage(Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(FStartTime) + ' seconds')
        else
          DoError(Cr + 'Process aborted - ' + TimeDifference(FStartTime) + ' seconds', 255);

        if Assigned(FArcFile) then FreeAndNil(FArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        if not FTerminated then
        begin
          SysUtils.DeleteFile(FCommandLine.ArchiveName);
          if not RenameFile(TmpFileName, FCommandLine.ArchiveName) then
            DoError('Error: can''t rename TempFile to ' + FCommandLine.ArchiveName, 2)
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
          DoError('Error: can''t open temp file', 2)
        else
          DoError('Error: can''t decode solid sequences', 2);

        if Assigned(FArcFile) then FreeAndNil(FArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(TmpFileName);
      end;

    end else // if ProcessFilesToRename
      DoWarning('Warning: no files to rename', 1);
  end;
  Headers.Free;

  if Assigned(FArcFile) then FreeAndNil(FArcFile);
end;

function CompareFn(P1, P2: pointer): longint;
begin
  Result := CompareFileName(
    ExtractFilePath(THeader(P1).FileName),
    ExtractFilePath(THeader(P2).FileName));

  if Result = 0 then
  begin
    Result := CompareText(
      ExtractFileName(THeader(P1).FileName),
      ExtractFileName(THeader(P2).FileName));
  end;
end;

procedure TBeeApp.ListShell;
var
  P: THeader;
  I: longint;
  Headers: THeaders;
  FI: TFileInfoExtra;
  {$IFDEF CONSOLEAPPLICATION}
  HeadersToList: TList;
  HeadersToListPath: string;
  {$ENDIF}
  TotalPack, TotalSize, TotalFiles: longint;
  Version, Method, Dictionary: longint;
begin
  DoMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);

  Headers := THeaders.Create(FCommandLine);
  {$IFDEF CONSOLEAPPLICATION}
  HeadersToList := TList.Create;
  {$ENDIF}
  if OpenArchive(Headers, toNone) then
  begin
    DoMessage(msgScanning + '...');

    Headers.MarkItems(FCommandLine.FileMasks, toNone, toList);
    Headers.MarkItems(FCommandLine.xOptions,   toList, toNone);

    if (Headers.GetNext(0, toList) > -1) then
    begin
      {$IFDEF CONSOLEAPPLICATION}
      DoMessage(StringOfChar(' ', 79));
      if FCommandLine.stlOption then
        DoMessage('Directory|File' + StringOfChar(' ',  8) + 'Size     Packed Ratio     Date  Time    Attr CRC     Meth')
      else
        DoMessage('Directory|File' + StringOfChar(' ', 32) + 'Size Ratio     Date  Time    Attr');
      DoMessage(StringOfChar('-', 79));
      {$ENDIF}

      Version    := -1;
      Method     := -1;
      Dictionary := -1;

      for I := 0 to Headers.GetCount -1 do
      begin
        P := Headers.GetItem(I);

        if foVersion in P.FileFlags then
          Version := P.FileVersion;

        if foMethod in P.FileFlags then
          Method := P.FileMethod;

        if foDictionary in P.FileFlags then
          Dictionary := P.FileDictionary;

        if P.FileAction = toList then
        begin
          P.FileVersion := Version;
          P.FileMethod := Method;
          P.FileDictionary := Dictionary;
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

      I := 0;
      {$IFDEF CONSOLEAPPLICATION}
      while I < HeadersToList.Count do
      begin
        P := HeadersToList.Items[I];
      {$ELSE}
      while I < Headers.GetCount do
      begin
        P := Headers.GetItem(I);
      {$ENDIF}

        FI.FileName := StringToPChar(ExtractFileName(P.FileName));
        FI.FilePath := StringToPChar(ExtractFilePath(P.FileName));

        {$IFDEF CONSOLEAPPLICATION}
        if CompareFileName(HeadersToListPath, FI.FilePath) <> 0 then
        begin
          HeadersToListPath := FI.FilePath;
          if I = 0 then
            DoMessage(HeadersToListPath)
          else
            DoMessage(Cr + HeadersToListPath);
        end;
        {$ENDIF}

        FI.FileSize   := P.FileSize;
        FI.FilePacked := P.FilePacked;

        if FI.FileSize > 0 then
          FI.FileRatio := Round((FI.FilePacked / FI.FileSize) * 100)
        else
          FI.FileRatio := 100;

        FI.FileAttr    := P.FileAttr;
        FI.FileTime    := P.FileTime;
        FI.FileComm    := StringToPChar('');
        FI.FileCrc     := P.FileCrc;
        FI.FileMethod  := StringToPChar(MethodToStr(P));
        FI.FileVersion := StringToPChar(VersionToStr(P));

        if foPassword in P.FileFlags then
          FI.FilePassword := StringToPchar('Yes')
        else
          FI.FilePassword := StringToPchar('No');

        {$IFDEF CONSOLEAPPLICATION}
        FI.FilePosition := Headers.GetNext(0, toList, P.FileName);
        {$ELSE}
        FI.FilePosition := I;
        {$ENDIF}
        DoList(FI, FCommandLine.stlOption);

        StrDispose(FI.FileName);
        StrDispose(FI.FilePath);
        StrDispose(FI.FileComm);
        StrDispose(FI.FileMethod);
        StrDispose(FI.FileVersion);
        StrDispose(FI.FilePassword);

        Inc(TotalSize, P.FileSize);
        Inc(TotalPack, P.FilePacked);
        Inc(TotalFiles);
        Inc(I);
      end;
      {$IFDEF CONSOLEAPPLICATION}
      DoMessage(StringOfChar('-', 79));
      if FCommandLine.stlOption then
        DoMessage(Format('%d files', [TotalFiles]) + StringOfChar(' ', 15 - Length((Format('%d files', [TotalFiles])))) + (Format(' %10s %10s %5s', [SizeToStr(TotalSize), SizeToStr(TotalPack), RatioToStr(TotalPack, TotalSize)])))
      else
        DoMessage(Format('%d files', [TotalFiles]) + StringOfChar(' ', 39 - Length((Format('%d files', [TotalFiles])))) + (Format(' %10s %5s', [SizeToStr(TotalSize), RatioToStr(TotalPack, TotalSize)])));
      {$ENDIF}

      {$IFDEF CONSOLEAPPLICATION}
      // self-extractor module size
      if Headers.GetModule > 0 then
        DoMessage(Cr + 'Note: Bee Self-Extractor module founded');
      {$ENDIF}

      DoMessage(Cr + 'Everything went ok - ' + TimeDifference(FStartTime) + ' seconds');
    end else
      DoWarning('Warning: no files to list', 1);

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

  if not (foTear in P.FileFlags) then
  begin
    Result[1] := 's';
  end;

  if not (foMoved in P.FileFlags) then
  begin
    if P.FileMethod in [1..3] then
      Result[2] := char(byte('0') + P.FileMethod)
    else
      Result[2] := '?';
  end;

  if P.FileDictionary in [0..9] then
    Result[3] := char(byte('a') + P.FileDictionary)
  else
    Result[3] := '?';
end;

function TBeeApp.VersionToStr(P: THeader): string;
begin
  case P.FileVersion of
    Ord(hv02): Result := ' 0' + DecimalSeparator + '2';
    Ord(hv03): Result := ' 0' + DecimalSeparator + '3';
    Ord(hv04): Result := ' 0' + DecimalSeparator + '4';
    else       Result := ' ?' + DecimalSeparator + '?';
  end;
end;

end.

