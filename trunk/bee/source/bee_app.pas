{
  Copyright (c) 2003-2010 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1112 - 2010.03.10 by Melchiorre Caruso.
}

unit Bee_App;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Files,
  Bee_Types,
  Bee_Common,
  Bee_Headers,
  Bee_Interface,
  Bee_CommandLine,
  Bee_Configuration;

type
  { TBeeApp class }

  TBeeApp = class(TApp)
  private
    FSelfName: string;
    FHeaders: THeaders;
    FArcFile: TFileReader;
    FSwapName: string;
    FSwapFile: TFileReader;
    FTempName: string;
    FTempFile: TFileWriter;
    FCommandLine: TCommandLine;
    FConfiguration: TConfiguration;
    { open/close archive routine }
    procedure OpenArchive(const aAction: THeaderAction);
    procedure CloseArchive(IsModified: boolean);
    { decode solid sequences using a swapfile }
    procedure ProcessFilesToSwap;
    { find and prepare sequences }
    function  ProcessFilesToAdd: int64;
    function  ProcessFilesToDelete: int64;
    function  ProcessFilesToExtract: int64;
    function  ProcessFileToTest: int64;
    function  ProcessFilesToRename: int64;

    procedure ProcessFilesToDecode(const aAction: THeaderAction);
    procedure ProcessFilesToFresh;
    procedure ProcessFilesDeleted;
    { process options }
    procedure ProcesstOption;
    procedure ProcesslOption;
    { overwrite sub-routines }
    function ProcessFileToOverWrite4Add(var New: TCustomSearchRec; aItem: THeader): TUpdateMode;
    function ProcessFileToOverWrite4Extract(aItem: THeader): TUpdateMode;
    { already file exists in archive routines}
    function AlreadyFileExists(const aIndex: longint; const aActions: THeaderActions; const aFileName: string): longint; overload;
    function AlreadyFileExists(const aFileName: string): longint; overload;
    { sheels routines}
    procedure EncodeShell;
    procedure DecodeShell(const aAction: THeaderAction);
    procedure RenameShell;
    procedure DeleteShell;
    procedure ListShell;
    procedure HelpShell;
  protected
    function VersionToStr(const aItem: THeader): string;
    function MethodToStr(const aItem: THeader): string;
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  SysUtils,
  Bee_Consts,
  Bee_MainPacker;

{ TBeeApp class }

constructor TBeeApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  Randomize; { randomize, uses for unique filename generation }
  FSelfName := 'The Bee 0.8.0 build 1114 archiver utility, Feb 2010' + Cr +
               '(C) 1999-2010 Andrew Filinsky and Melchiorre Caruso';

  FHeaders  := nil;
  FArcFile  := nil;
  FSwapFile := nil;
  FTempFile := nil;
  FSwapName := '';
  FTempName := '';

  { store command line }
  FCommandLine := TCommandLine.Create;
  FCommandLine.CommandLine := aParams.Text;

  { load configuration }
  FConfiguration := TConfiguration.Create;
  if not FileExists(FCommandLine.cfgOption) then
    DoError('Warning: configuration file "' + FCommandLine.cfgOption +
      '" not found, data will be stored' + Cr, ccWarning)
  else
    FConfiguration.LoadFromFile(FCommandLine.cfgOption);

  { load method and dictionary level }
  FConfiguration.Selector('\main');
  FConfiguration.CurrentSection.Values['Method']     := IntToStr(Ord(FCommandLine.mOption));
  FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(Ord(FCommandLine.dOption));

  { set thread priority }
  SetPriority(Ord(FCommandLine.priOption));
end;

destructor TBeeApp.Destroy;
begin
  FConfiguration.Destroy;
  FCommandLine.Destroy;
  inherited Destroy;
end;

procedure TBeeApp.HelpShell;
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
    if (Command <> ccNone) and (Length(ArchiveName) <> 0) then
    begin
      case FCommandLine.Command of
        ccAdd:      EncodeShell;
        ccDelete:   DeleteShell;
        ccExtract:  DecodeShell(toExtract);
        ccxExtract: DecodeShell(toExtract);
        ccList:     ListShell;
        ccTest:     DecodeShell(toTest);
        ccRename:   RenameShell;
        ccHelp:     HelpShell;
      end;
    end else
      HelpShell;
  FTerminated := True;
end;

procedure TBeeApp.OpenArchive(const aAction: THeaderAction);
begin
  if FileExists(FCommandLine.ArchiveName) then
  begin
    FArcFile := CreateTFileReader(FCommandLine.ArchiveName, fmOpenRead + fmShareDenyWrite);
    if FArcFile <> nil then
    begin
      FHeaders.ReadItems(FArcFile, aAction);
      if (FHeaders.GetCount = 0) and (FArcFile.Size <> 0) then
        DoError('Error: archive unsupported', ccError);
    end else
      DoError('Error: can''t open archive', ccError);
  end;
end;

procedure TBeeApp.CloseArchive(IsModified: boolean);
begin
  if IsModified then
  begin
    if FCode < ccError then
      DoMessage(Cr + 'Archive size ' + SizeToStr(FTempFile.Size) + ' bytes - ' + TimeDifference(FStartTime) + ' seconds')
    else
      DoMessage(Cr + 'Process aborted - ' + TimeDifference(FStartTime) + ' seconds');

    if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
    if Assigned(FTempFile) then FreeAndNil(FTempFile);
    if Assigned(FArcFile)  then FreeAndNil(FArcFile);

    if FCode < ccError then
    begin
      SysUtils.DeleteFile(FSwapName);
      SysUtils.DeleteFile(FCommandLine.ArchiveName);
      if RenameFile(FTempName, FCommandLine.ArchiveName) then
      begin
        ProcesstOption; // process tOption
        ProcesslOption; // process lOption
      end else
        DoError('Error: can''t rename "' + FTempName + '" to "' + FCommandLine.ArchiveName + '"', ccError);
    end else
    begin
      SysUtils.DeleteFile(FSwapName);
      SysUtils.DeleteFile(FTempName);
    end;
  end else
  begin
    if Assigned(FArcFile)  then FreeAndNil(FArcFile);
  end;
end;

function TBeeApp.AlreadyFileExists(const aIndex: longint; const aActions: THeaderActions; const aFileName: string): longint;
begin
  Result := FHeaders.GetBack(aIndex - 1, aActions, aFileName);
  if Result = -1 then
  begin
    Result := FHeaders.GetNext(aIndex + 1, aActions, aFileName);
  end;
end;

function TBeeApp.AlreadyFileExists(const aFileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := 0 to FHeaders.GetCount - 1 do
    if CompareFileName(aFileName, FHeaders.GetItem(I).FileName) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

{ Process File To OverWrite 4 Add } { DA RIVEDERE }

function TBeeApp.ProcessFileToOverWrite4Add(var New: TCustomSearchRec; aItem: THeader): TUpdateMode;
var
  FI: TFileInfo;
  S: string;
begin
  Result := FCommandLine.uOption;
  if (aItem <> nil) and (Result = umAddQuery) then
  begin
    FI.FileName := StringToPChar(ExtractFileName(aItem.FileName));
    FI.FilePath := StringToPChar(ExtractFilePath(aItem.FileName));

    FI.FileSize := aItem.FileSize;
    FI.FileTime := aItem.FileTime;
    FI.FileAttr := aItem.FileAttr;

    case DoOverwrite(FI, omAddReplace) of
      omAdd: begin
               Result := umAdd;
               FCommandLine.uOption := Result;
             end;
      omUpdate: begin
                  Result := umUpdate;
                  FCommandLine.uOption :=  Result;
                end;
      omReplace: begin
                   Result := umReplace;
                   FCommandLine.uOption :=  Result;
                 end;
      omAddUpdate: begin
                     Result := umAddUpdate;
                     FCommandLine.uOption :=  Result;
                   end;
      omAddReplace: begin
                      Result := umAddReplace;
                      FCommandLine.uOption :=  Result;
                    end;
      omAddAutoRename: begin
                         Result := umAddAutoRename;
                         FCommandLine.uOption :=  Result;
                       end;
      omUpdateOne: Result := umUpdate;
      omReplaceOne: Result := umReplace;
      omRenameOne: begin
                     repeat
                       S := FixFileName(DoRename(FI, ''));
                       if Length(S) <> 0 then
                       begin
                         if AlreadyFileExists(S) <> -1 then
                           DoError('Warning: file "' + S + '" already existing in archive' + Cr, ccWarning)
                         else
                           Break;
                       end else
                         Break;
                     until False;
                    if Length(S) <> 0 then
                    begin
                      New.FileName := S;
                      Result := umAdd;
                    end;
                  end;
      // omSkip: Result := umAddquery;
      omQuit: Code := ccUserAbort;
    end;
    StrDispose(FI.FileName);
    StrDispose(FI.FilePath);
  end;
end;

function TBeeApp.ProcessFilesToAdd: int64;
var
  I: longint;
  P: THeader;
  T: TCustomSearchRec;
  Scanner: TFileScanner;
begin
  Result  := 0;
  Scanner := TFileScanner.Create;
  with FCommandLine do
    for I := 0 to FileMasks.Count - 1 do
    begin
      Scanner.Scan(FileMasks[I], xOptions, rOption);
    end;

  for I := 0 to Scanner.Count - 1 do
  begin
    if Terminated = False then
    begin
      T := Scanner.Items[I];
      P := FHeaders.SearchItem(T.FileName);
      case ProcessFileToOverWrite4Add(T, P) of
        umAdd:        Result := Result + FHeaders.AddItem       (T, P);
        umUpdate:     Result := Result + FHeaders.UpdateItem    (T, P);
        umReplace:    Result := Result + FHeaders.ReplaceItem   (T, P);
        umAddUpdate:  Result := Result + FHeaders.AddUpdateItem (T, P);
        umAddReplace: Result := Result + FHeaders.AddReplaceItem(T, P);
        umAddAutoRename:
        begin
          while P <> nil do
          begin
            T.FileName := GenerateAlternativeFileName(T.FileName, False);
            P := FHeaders.SearchItem(T.FileName);
          end;
          Result := Result + FHeaders.AddItem(T, P);
        end;
        // umAddQuery: nothing to do, skip file
      end;
    end;
  end;
  FHeaders.SortNews(FConfiguration);
  Scanner.Destroy;
end;

{ Process File To OverWrite 4 Extract } { DA RIVEDERE }

function TBeeApp.ProcessFileToOverWrite4Extract(aItem: THeader): TUpdateMode;
var
  FI: TFileInfo;
  S: string;
begin
  Result := FCommandLine.uOption;
  if (Result = umAddQuery) and FileExists(aItem.FileName) then
  begin
    FI.FileName := StringToPChar(ExtractFileName(aItem.FileName));
    FI.FilePath := StringToPChar(ExtractFilePath(aItem.FileName));

    FI.FileSize := aItem.FileSize;
    FI.FileTime := aItem.FileTime;
    FI.FileAttr := aItem.FileAttr;

    case DoOverwrite(FI, omAddReplace) of
      omAdd: begin
               Result := umAdd;
               FCommandLine.uOption := Result;
             end;
      omUpdate: begin
                  Result := umUpdate;
                  FCommandLine.uOption := Result;
                end;
      omReplace: begin
                   Result := umReplace;
                   FCommandLine.uOption := Result;
                 end;
      omAddUpdate: begin
                     Result := umAddUpdate;
                     FCommandLine.uOption := Result;
                   end;
      omAddReplace: begin
                      Result := umAddReplace;
                      FCommandLine.uOption := Result;
                    end;
      omAddAutoRename: begin
                         Result := umAddAutoRename;
                         FCommandLine.uOption := Result;
                       end;
      omUpdateOne: Result := umUpdate;
      omReplaceOne: Result := umReplace;
      omRenameOne: begin
                     repeat
                       S := DoRename(FI, '');
                       if Length(S) <> 0 then
                       begin
                         if FileExists(S) then
                           DoError('Warning: file "' + S + '" already existing ' + Cr, ccWarning)
                         else
                           Break;
                       end else
                         Break;
                     until False;
                     if Length(S) > 0 then
                     begin
                       aItem.FileName := S;
                       Result := umAdd;
                     end;
                   end;
      omSkip: aItem.FileAction := toNone;
      omQuit: Code := ccUserAbort;
    end;

    StrDispose(FI.FileName);
    StrDispose(FI.FilePath);
  end;
end;

procedure TBeeApp.ProcessFilesToExtract;
var
  I: longint;
  P: THeader;
  U: TUpdateMode;
begin
  for I := 0 to FHeaders.GetCount - 1 do
  begin
    if Terminated = False then
    begin
      P := FHeaders.GetItem(I);
      if P.FileAction = toExtract then
      begin
        if FCommandLine.Command = ccXextract then
          P.FileName := DeleteFilePath(FCommandLine.cdOption, P.FileName)
        else
          P.FileName := ExtractFileName(P.FileName);

        case ProcessFileToOverWrite4Extract(P) of
          umAdd: begin
                   if FileExists (P.FileName) then
                     P.FileAction := toNone;
                 end;
          umUpdate: begin
                      if (not FileExists(P.FileName)) or (not (P.FileTime > FileAge(P.FileName))) then
                        P.FileAction := toNone;
                    end;
          umReplace: begin
                       if (not FileExists(P.FileName)) then
                         P.FileAction := toNone;
                     end;
          umAddUpdate: begin
                         if (FileExists(P.FileName)) and (not (P.FileTime > FileAge(P.FileName))) then
                           P.FileAction := toNone;
                       end;
          // umAddReplace: extract file
          umAddAutoRename: begin
                             while FileExists(P.FileName) do
                             begin
                               P.FileName := GenerateAlternativeFileName(P.FileName, False);
                             end;
                           end;
          umAddQuery: P.FileAction := toNone;
        end;
      end;
    end;
  end;
end;

// -------------------------------------------------------------------------- //
// Rename file processing                                                     //
// -------------------------------------------------------------------------- //

function TBeeApp.ProcessFilesToRename: longint;
var
  S: string;
  I: longint;
  P: THeader;
  FI: TFileInfo;
begin
  if Result <> 0 then
    for I := 0 to FHeaders.GetCount - 1 do
    begin
      P := FHeaders.GetItem(I);
      if P.FileAction = toRename then
      begin
        FI.FileName := StringToPChar(ExtractFileName(P.FileName));
        FI.FilePath := StringToPChar(ExtractFilePath(P.FileName));

        FI.FileSize := P.FileSize;
        FI.FileTime := P.FileTime;
        FI.FileAttr := P.FileAttr;
        repeat
          S := FixFileName(DoRename(FI, ''));
          if Length(S) <> 0 then
          begin
            if AlreadyFileExists(I, [toCopy, toRename], S) <> -1 then
              DoError('Warning: file "' + S + '" already existing in archive', ccWarning)
            else
              Break;
          end else
            Break;
        until False;
        StrDispose(FI.FileName);
        StrDispose(FI.FilePath);

        if (Length(S) <> 0) and (CompareFileName(S, P.FileName) <> 0) then
          P.FileName := S
        else
          Dec(Result);
      end;
      if Terminated then Break;
    end;
end;

// -------------------------------------------------------------------------- //
// Sequences processing                                                       //
// -------------------------------------------------------------------------- //

procedure TBeeApp.ProcessFilesToFresh;
var
  I, J, BackTear, NextTear: longint;
  P: THeader;
begin
  I := FHeaders.GetBack(FHeaders.GetCount -1, toFresh);
  // find sequences and mark as toSwap files that not toFresh
  while I > -1 do
  begin
    BackTear := FHeaders.GetBack(I, foTear);
    NextTear := FHeaders.GetNext(I + 1, foTear);

    if NextTear = -1 then NextTear := FHeaders.GetCount;
    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := FHeaders.GetBack(NextTear - 1, toCopy);
      for J := BackTear to NextTear do
      begin
        P := FHeaders.GetItem(J);
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
    I := FHeaders.GetBack(I - 1, toFresh);
  end;
  Inc(FTotalSize, FHeaders.GetPackedSize(toCopy));
end;

procedure TBeeApp.ProcessFilesToDelete;
var
  I, J, BackTear, NextTear: longint;
  P: THeader;
begin
  I := FHeaders.GetBack(FHeaders.GetCount -1, toDelete);
  // find sequences and ...
  while I > -1 do
  begin
    BackTear := FHeaders.GetBack(I, foTear);
    NextTear := FHeaders.GetNext(I + 1, foTear);

    if NextTear = -1 then NextTear := FHeaders.GetCount;
    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := FHeaders.GetBack(NextTear - 1, toCopy);
      // if exists an header toDelete
      if FHeaders.GetBack(NextTear, toDelete) > (BackTear - 1) then
        for J := BackTear to NextTear do
        begin
          P := FHeaders.GetItem(J);
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
    I := FHeaders.GetBack(I - 1, toDelete);
  end;
  Inc(FTotalSize, FHeaders.GetPackedSize(toCopy));
end;

procedure TBeeApp.ProcessFilesToSwap;
var
  P: THeader;
  I, J: longint;
  Decoder: TDecoder;
  FSwapStrm: TFileWriter;
  iDictionary, iTable, iTear: longint;
  CurrDictionary, CurrTable: longint;
begin
  I := FHeaders.GetBack(FHeaders.GetCount - 1, toSwap);
  if (I > -1) then
  begin
    FSwapName := GenerateFileName(FCommandLine.wdOption);
    FSwapStrm := CreateTFileWriter(FSwapName, fmCreate);

    if (FSwapStrm <> nil) then
    begin
      CurrDictionary := FHeaders.GetCount;
      CurrTable      := CurrDictionary;

      Decoder := TDecoder.Create(FArcFile, Self);
      while (I > -1) and (Code < ccError) do
      begin
        iDictionary := FHeaders.GetBack(I, foDictionary); // find dictionary info
        iTable := FHeaders.GetBack(I, foTable);           // find table info
        iTear  := FHeaders.GetBack(I, foTear);            // find tear info

        if (iDictionary > -1) and (iDictionary <> CurrDictionary) and (iDictionary <> iTear) then
        begin
          CurrDictionary := iDictionary;
          P := FHeaders.GetItem(CurrDictionary);
          Decoder.DecodeStrm(P, pmQuit, FSwapStrm, P.FileSize, foPassword in P.FileFlags);
        end;

        if (iTable > -1) and (iTable <> CurrTable) and (iTable <> iTear) then
        begin
          CurrTable := iTable;
          P := FHeaders.GetItem(CurrTable);
          Decoder.DecodeStrm(P, pmQuit, FSwapStrm, P.FileSize, foPassword in P.FileFlags);
        end;

        for J := iTear to I do
        begin
          if Code < ccError then
          begin
            P := FHeaders.GetItem(J);
            if P.FileAction = toSwap then
              Decoder.DecodeStrm(P, pmNorm, FSwapStrm, P.FileSize, foPassword in P.FileFlags)
            else
              Decoder.DecodeStrm(P, pmSkip, FSwapStrm, P.FileSize, foPassword in P.FileFlags);
          end;
        end;
        I := FHeaders.GetBack(iTear - 1, toSwap);
      end;
      Decoder.Destroy;
      FreeAndNil(FSwapStrm);
    end;
  end;
end;

procedure TBeeApp.ProcessFilesDeleted;
var
  I: longint;
  Back, Next: THeader;
begin
  // rescue header informations
  for I := 0 to FHeaders.GetCount -2 do
  begin
    Back := FHeaders.GetItem(I);
    Next := FHeaders.GetItem(I + 1);

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
  I := FHeaders.GetBack(FHeaders.GetCount -1, aAction); // last header
  while I > -1 do
  begin
    iDictionary := FHeaders.GetBack(I, foDictionary);  // find dictionary info
    iTable := FHeaders.GetBack(I, foTable);            // find table info
    iTear := FHeaders.GetBack(I, foTear);              // find tear info

    for J := iTear to (I -1) do
    begin
      P := FHeaders.GetItem(J);
      if P.FileAction in [toNone, toQuit] then
      begin
        P.FileAction := toSkip;
        Inc(FTotalSize, P.FileSize);
      end;
    end;

    if iDictionary > -1 then
    begin
      P := FHeaders.GetItem(iDictionary);
      if P.FileAction = toNone then P.FileAction := toQuit;
    end;

    if iTable > -1 then
    begin
      P := FHeaders.GetItem(iTable);
      if P.FileAction = toNone then P.FileAction := toQuit;
    end;

    I := FHeaders.GetBack(iTear - 1, aAction);
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
  Headers: THeaders;
begin
  DoMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);
  Headers := THeaders.Create(FCommandLine);

  OpenArchive(toCopy);
  if Code < ccError then
  begin
    DoMessage(msgScanning + '...');

    FTotalSize := ProcessFilesToAdd;
    if FTotalSize <> 0 then
    begin
      FTempName := GenerateFileName(FCommandLine.wdOption);
      FTempFile := CreateTFileWriter(FTempName, fmCreate);

      if (FTempFile <> nil) then
      begin
        ProcessFilesToFresh; // find sequences
        ProcessFilesToSwap;  // decode solid sequences

        if Code < ccError then
        begin
          // if exists a modified solid sequence open swap file
          if Length(FSwapName) <> 0 then
          begin
            FSwapFile := CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);
          end;

          if FSwapFile <> nil then
          begin
            // write Headers
            Headers.WriteItems(FTempFile);
            Encoder := TEncoder.Create(FTempFile, Self);
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
            Headers.WriteItems(FTempFile);
          end else
            DoError('Error: can''t open a swap file', ccError);
        end else
          DoError('Error: can''t decode solid sequence', ccError);
      end else
        DoError('Error: can''t open temp file', ccError);

    end else // if FtotalSize <> 0
      DoError('Warning: no files to process', ccWarning);
  end;
  CloseArchive(FtotalSize <> 0);
  Headers.Free;
end;

procedure TBeeApp.DecodeShell(const aAction: THeaderAction);
var
  Decoder: TDecoder;
  Headers: THeaders;
  P: THeader;
  I: longint;
begin
  DoMessage(Cr + msgOpening + 'archive ' + FCommandLine.ArchiveName);
  Headers := THeaders.Create(FCommandLine);

  OpenArchive(toNone);
  if Code < ccError then
  begin
    DoMessage(msgScanning + '...');

    FTotalSize := FHeaders.MarkItems(FCommandLine.FileMasks, toNone, aAction) -
                  FHeaders.MarkItems(FCommandLine.xOptions, aAction, toNone);

    if (aAction = toExtract) then
    begin
      ProcessFilesToExtract;
    end;

    if (FTotalSize <> 0) then
    begin
      ProcessFilesToDecode(aAction);
      Decoder := TDecoder.Create(FArcFile, Self);
      for I := 0 to FHeaders.GetCount - 1 do
      begin
        if Code < ccError then
        begin
          P := Headers.GetItem(I);
          case P.FileAction of
            toExtract: Decoder.DecodeFile(P, pmNorm);
            toTest:    Decoder.DecodeFile(P, pmTest);
            toSkip:    Decoder.DecodeFile(P, pmSkip);
            toQuit:    Decoder.Decodefile(P, pmQuit);
          end;
        end;
      end;
      Decoder.Destroy;

      if Code < ccError then
        DoMessage(Cr + 'Everything went ok - ' + TimeDifference(FStartTime) + ' seconds')
      else
        if Code = ccUserabort then
          DoError(Cr + 'Process aborted - ' + TimeDifference(FStartTime) + ' seconds', 255)
        else
          DoError(Cr + 'Process aborted, a fatal error occourred - ' + TimeDifference(FStartTime) + ' seconds', 2);

    end else // if FTotalSize <> 0
      DoError('Warning: no files to decode', ccWarning);
  end;
  CloseArchive(False);
  FHeaders.Free;
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
  FHeaders := THeaders.Create(FCommandLine);

  OpenArchive(toCopy);
  if Code < ccError then
  begin
    DoMessage(msgScanning + '...');

    FTotalSize:= FHeaders.MarkItems(FCommandLine.FileMasks, toCopy, toDelete) -
                 FHeaders.MarkItems(FCommandLine.xOptions, toDelete, toCopy);

    if FTotalSize <> 0 then
    begin
      TmpFileName := GenerateFileName(FCommandLine.wdOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);

      if (TmpFile <> nil) then
      begin
        ProcessFilesToDelete;  // find sequences
        ProcessFilesToSwap;    // decode solid sequences

        if Code < ccError then
        begin
          // if SwapSequences has found a modified sequence open Swap file
          if Length(FSwapName) > 0 then
          begin
            FSwapFile := CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);
          end;

          if FSwapFile <> nil then
          begin
            // write Headers
            FHeaders.WriteItems(TmpFile);
            Encoder := TEncoder.Create(TmpFile, Self);
            for I := 0 to FHeaders.GetCount -1 do
            begin
              if Code < ccError then
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
            FHeaders.WriteItems(TmpFile);
          end else
            DoError('Error: can''t open swap file', ccError);
        end else
          DoError('Error: can''t decode solid sequences', ccError);
      end else
        DoError('Error: can''t open temp file', ccError);

    end else // if FTotalSize <> 0
      DoError('Warning: no files to delete', ccWarning);
  end;
  CloseArchive(FTotalSize <> 0);
  FHeaders.Free;
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

  OpenArchive(toCopy);
  if not FTerminated then
  begin
    DoMessage(msgScanning + '...');

    TFotalSize := FHeaders.MarkItems(FCommandLine.FileMasks, toCopy,   toRename) -
                  FHeaders.MarkItems(FCommandLine.xOptions,  toRename, toCopy);

    ProcessFileToRename;

    if FTotalSize <> 0 then
    begin
      TmpFileName := GenerateFileName(FCommandLine.wdOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);

      if (TmpFile <> nil) then
      begin
        FTotalSize := Headers.GetPackedSize([toCopy, toRename]);

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
      DoError('Warning: no files to rename', ccWarning);
  end;
  FHeaders.Free;

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

  OpenArchive(toNone);
  if FTerminated then
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
      DoError('Warning: no files to list', ccWarning);

  end;
  CloseArchive(False);
  {$IFDEF CONSOLEAPPLICATION}
  HeadersToList.Free;
  {$ENDIF}
  Headers.Free;
end;



// -------------------------------------------------------------------------- //
// String routines                                                            //
// -------------------------------------------------------------------------- //

function TBeeApp.MethodToStr(const aItem: THeader): string;
begin
  Result := 'm0a';

  if not (foTear in aItem.FileFlags) then
  begin
    Result[1] := 's';
  end;

  if not (foMoved in aItem.FileFlags) then
  begin
    if aItem.FileMethod in [1..3] then
      Result[2] := char(byte('0') + aItem.FileMethod)
    else
      Result[2] := '?';
  end;

  if aItem.FileDictionary in [0..9] then
    Result[3] := char(byte('a') + aItem.FileDictionary)
  else
    Result[3] := '?';
end;

function TBeeApp.VersionToStr(const aItem: THeader): string;
begin
  case aItem.FileVersion of
    Ord(hv02): Result := ' 0' + DecimalSeparator + '2';
    Ord(hv03): Result := ' 0' + DecimalSeparator + '3';
    Ord(hv04): Result := ' 0' + DecimalSeparator + '4';
    else       Result := ' ?' + DecimalSeparator + '?';
  end;
end;

end.

