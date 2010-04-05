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
    { find and prepare sequences }
    procedure ProcessFilesToAdd;
    procedure ProcessFilesToUpdate;
    procedure ProcessFilesToSwap;
    procedure ProcessFilesToExtract;
    procedure ProcessFilesToDecode(const aAction: THeaderAction);
    procedure ProcessFilesToTest;
    procedure ProcessFilesToDelete;
    procedure ProcessFilesDeleted;
    procedure ProcessFilesToRename;
    procedure ProcessFilesToList;
    { process options }
    procedure ProcesstOption;
    procedure ProcesslOption;
    { sheels routines}
    procedure HelpShell;
    procedure EncodeShell;
    procedure DecodeShell(const aAction: THeaderAction);
    procedure RenameShell;
    procedure DeleteShell;
    procedure ListShell;
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

{ TBeeApp class }

constructor TBeeApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  Randomize; { randomize, uses for unique filename generation }
  FSelfName := 'The Bee 0.8.0 build 1100 archiver utility, Apr 2010' + Cr +
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
  if FileExists(FCommandLine.cfgOption) then
    FConfiguration.LoadFromFile(FCommandLine.cfgOption)
  else
    DoMessage(Format(cmConfigWarning, [FCommandLine.cfgOption]), ccWarning);

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

procedure TBeeApp.Execute;
begin
  inherited Execute;
  DoMessage(FSelfName);
  with FCommandLine do
  begin
    if (Command <> ccNone) and (Length(ArchiveName) <> 0) then
    begin
      case FCommandLine.Command of
        ccAdd:      EncodeShell;
        ccDelete:   DeleteShell;
        ccExtract:  DecodeShell(haExtract);
        ccxExtract: DecodeShell(haExtract);
        ccList:     ListShell;
        ccTest:     DecodeShell(haDecode);
        ccRename:   RenameShell;
        ccHelp:     HelpShell;
      end;

      if FCommandLine.Command in [ccAdd, ccDelete, ccRename] then
      begin
        ProcesstOption;
        ProcesslOption;
      end;
    end else
      HelpShell;
  end;
  SetTerminated(True);
end;

{ Open / Close archive routines }

procedure TBeeApp.OpenArchive(const aAction: THeaderAction);
begin
  DoMessage(Format(Cr + msgOpening, [FCommandLine.ArchiveName]));
  FHeaders := THeaders.Create(FCommandLine);
  if FileExists(FCommandLine.ArchiveName) then
  begin
    FArcFile := CreateTFileReader(FCommandLine.ArchiveName, fmOpenRead + fmShareDenyWrite);
    if FArcFile <> nil then
    begin
      FHeaders.ReadItems(FArcFile, aAction);
      if (FHeaders.GetCount = 0) and (FArcFile.Size <> 0) then
        DoMessage(cmArcTypeError, ccError);
    end else
      DoMessage(Format(cmArcOpenError, [FCommandLine.ArchiveName]), ccError);
  end;
end;

procedure TBeeApp.CloseArchive(IsModified: boolean);
begin
  if IsModified then
  begin
    if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
    if Assigned(FTempFile) then FreeAndNil(FTempFile);
    if Assigned(FArcFile)  then FreeAndNil(FArcFile);

    if Code < ccError then
    begin
      SysUtils.DeleteFile(FSwapName);
      SysUtils.DeleteFile(FCommandLine.ArchiveName);
      if not RenameFile(FTempName, FCommandLine.ArchiveName) then
        DoMessage(Format(cmRenameFileError, [FTempName, FCommandLine.ArchiveName]), ccError);
    end else
    begin
      SysUtils.DeleteFile(FSwapName);
      SysUtils.DeleteFile(FTempName);
    end;
  end else
  begin
    if Assigned(FArcFile) then
      FreeAndNil(FArcFile);
  end;
  FHeaders.Free;

  with FCommandLine do
    case Code of
      ccSuccesful: DoMessage(Format(Cr + cmSuccesful, [TimeDifference(FStartTime)]));
      ccWarning:   DoMessage(Format(Cr + cmWarning,   [TimeDifference(FStartTime)]));
      ccUserAbort: DoMessage(Format(Cr + cmUserAbort, [TimeDifference(FStartTime)]));
      else         DoMessage(Format(Cr + cmError,     [TimeDifference(FStartTime)]));
  end;
end;

{ Sequences processing }

procedure TBeeApp.ProcessFilesToAdd;
var
  I: longint;
  T: TCustomSearchRec;
  Scanner: TFileScanner;
begin
  DoMessage(Format(msgScanning, ['...']));
  Scanner := TFileScanner.Create;
  with FCommandLine do
    for I := 0 to FileMasks.Count - 1 do
    begin
      Scanner.Scan(FileMasks[I], xOptions, rOption);
    end;

  for I := 0 to Scanner.Count - 1 do
  begin
    if Code < ccError then
    begin
      T := Scanner.Items[I];
      case FCommandLine.uOption of
        umAdd:           Inc(FTotalSize, FHeaders.AddItem          (T));
        umUpdate:        Inc(FTotalSize, FHeaders.UpdateItem       (T));
        umReplace:       Inc(FTotalSize, FHeaders.ReplaceItem      (T));
        umAddUpdate:     Inc(FTotalSize, FHeaders.AddUpdateItem    (T));
        umAddReplace:    Inc(FTotalSize, FHeaders.AddReplaceItem   (T));
        umAddAutoRename: Inc(FTotalSize, FHeaders.AddAutoRenameItem(T));
        else DoMessage(cmSequenceError,  ccError);
      end;
    end;
  end;
  Scanner.Destroy;
  FHeaders.SortNews(FConfiguration);
end;

procedure TBeeApp.ProcessFilesToUpdate;
var
  I, J, BackTear, NextTear: longint;
  P: THeader;
begin
  // find sequences and mark as toSwap files that not toUpdate
  I := FHeaders.GetBack(FHeaders.GetCount - 1, haUpdate);
  while I > -1 do
  begin
    BackTear := FHeaders.GetBack(I, foTear);
    NextTear := FHeaders.GetNext(I + 1, foTear);

    if NextTear = -1 then NextTear := FHeaders.GetCount;
    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := FHeaders.GetBack(NextTear - 1, haCopy);
      for J := BackTear to NextTear do
      begin
        P := FHeaders.GetItem(J);
        case P.FileAction of
          haUpdate: Inc(FTotalSize, P.FileSize);
          haCopy:
          begin
            P.FileAction := haExtract;
            Inc(FTotalSize, P.FileSize * 2);
          end;
          else DoMessage(cmSequenceError,  ccError);
        end;
      end;
      I := BackTear;
    end;
    I := FHeaders.GetBack(I - 1, haUpdate);
  end;
  Inc(FTotalSize, FHeaders.GetPackedSize(haCopy));
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
  I := FHeaders.GetBack(FHeaders.GetCount - 1, haExtract);
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
          Decoder.DecodeStrm(P, pmSkip, FSwapStrm, P.FileSize, foPassword in P.FileFlags);
        end;

        if (iTable > -1) and (iTable <> CurrTable) and (iTable <> iTear) then
        begin
          CurrTable := iTable;
          P := FHeaders.GetItem(CurrTable);
          Decoder.DecodeStrm(P, pmSkip, FSwapStrm, P.FileSize, foPassword in P.FileFlags);
        end;

        for J := iTear to I do
        begin
          if Code < ccError then
          begin
            P := FHeaders.GetItem(J);
            if P.FileAction = haExtract then
              Decoder.DecodeStrm(P, pmNorm, FSwapStrm, P.FileSize, foPassword in P.FileFlags)
            else
              Decoder.DecodeStrm(P, pmNul,  FSwapStrm, P.FileSize, foPassword in P.FileFlags);
          end;
        end;
        I := FHeaders.GetBack(iTear - 1, haExtract);
      end;
      Decoder.Destroy;
      FreeAndNil(FSwapStrm);
    end else
      DoMessage(cmSwapOpenError, ccError);
  end;
end;

procedure TBeeApp.ProcessFilesToDelete;
var
  I, J, BackTear, NextTear: longint;
  P: THeader;
begin
  DoMessage(Format(msgScanning, ['...']));
  FHeaders.MarkItems(FCommandLine.FileMasks, haCopy, haDelete);
  FHeaders.MarkItems(FCommandLine.xOptions, haDelete, haCopy);

  I := FHeaders.GetBack(FHeaders.GetCount -1, haDelete);
  // find sequences and ...
  while I > -1 do
  begin
    BackTear := FHeaders.GetBack(I, foTear);
    NextTear := FHeaders.GetNext(I + 1, foTear);

    if NextTear = -1 then NextTear := FHeaders.GetCount;
    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := FHeaders.GetBack(NextTear - 1, haCopy);
      // if exists an header toDelete
      if FHeaders.GetBack(NextTear, haDelete) > (BackTear - 1) then
        for J := BackTear to NextTear do
        begin
          P := FHeaders.GetItem(J);
          case P.FileAction of
            haDelete: Inc(FTotalSize, P.FileSize);
            haCopy:
            begin
              P.FileAction := haExtract;
              Inc(FTotalSize, P.FileSize * 2);
            end;
          end;
        end;
      I := BackTear;
    end;
    I := FHeaders.GetBack(I - 1, haDelete);
  end;
  Inc(FTotalSize, FHeaders.GetPackedSize(haCopy));
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

    if Back.FileAction = haDelete then
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
      if P.FileAction in [haNone, haSkip] then
      begin
        P.FileAction := haDecode;
        Inc(FTotalSize, P.FileSize);
      end;
    end;

    if iDictionary > -1 then
    begin
      P := FHeaders.GetItem(iDictionary);
      if P.FileAction = haNone then
        P.FileAction := haSkip;
    end;

    if iTable > -1 then
    begin
      P := FHeaders.GetItem(iTable);
      if P.FileAction = haNone then
        P.FileAction := haSkip;
    end;

    I := FHeaders.GetBack(iTear - 1, aAction);
  end;
end;

procedure TBeeApp.ProcessFilesToExtract;
var
  I: longint;
  P: THeader;
  U: TUpdateMode;
begin
  DoMessage(Format(msgScanning, ['...']));
  FHeaders.MarkItems(FCommandLine.FileMasks, haNone, haExtract);
  FHeaders.MarkItems(FCommandLine.xOptions, haExtract, haNone);

  for I  := 0 to FHeaders.GetCount - 1 do
  begin
    if Code < ccError then
    begin
      P := FHeaders.GetItem(I);
      if P.FileAction = haExtract then
      begin
        if FCommandLine.Command = ccXextract then
          P.FileName := DeleteFilePath(FCommandLine.cdOption, P.FileName)
        else
          P.FileName := ExtractFileName(P.FileName);

        case FCommandLine.uOption of
          umAdd:       if not FileExists(P.FileName) then
                         Inc(FTotalSize, P.FileSize)
                       else
                         P.FileAction := haNone;
          umUpdate:    if (FileExists(P.FileName)) and (P.FileTime > FileAge(P.FileName)) then
                         Inc(FTotalSize, P.FileSize)
                       else
                         P.FileAction := haNone;
          umReplace:   if FileExists(P.FileName) then
                         Inc(FTotalSize, P.FileSize)
                       else
                         P.FileAction := haNone;
          umAddUpdate: if (not FileExists(P.FileName)) or (P.FileTime > FileAge(P.FileName)) then
                         Inc(FTotalSize, P.FileSize)
                       else
                         P.FileAction := haNone;
       // umAddReplace: extract file always
          umAddAutoRename:
          begin
            if FileExists(P.FileName) then
            begin
              P.FileName := GenerateAlternativeFileName(P.FileName, 1, True);
            end;
            Inc(FTotalSize, P.FileSize);
          end;
        end;
      end;
    end;
  end;
end;

procedure  TBeeApp.ProcessFilesToTest;
var
  I: longint;
  P: THeader;
begin
  DoMessage(Format(msgScanning, ['...']));
  FHeaders.MarkItems(FCommandLine.FileMasks, haNone, haDecode);
  FHeaders.MarkItems(FCommandLine.xOptions, haDecode, haNone);

  for I  := 0 to FHeaders.GetCount - 1 do
  begin
    if Code < ccError then
    begin
      P := FHeaders.GetItem(I);
      if P.FileAction = haDecode then
      begin
        Inc(FTotalSize, P.FileSize);
      end;
    end;
  end;
end;

procedure TBeeApp.ProcessFilesToRename;
var
  S: string;
  I: longint;
  P: THeader;
  FI: TFileInfo;
begin
  DoMessage(Format(msgScanning, ['...']));
  FHeaders.MarkItems(FCommandLine.FileMasks, haCopy,  haOther);
  FHeaders.MarkItems(FCommandLine.xOptions,  haOther, haCopy);

  for I  := 0 to FHeaders.GetCount - 1 do
  begin
    if Code < ccError then
    begin
      P := FHeaders.GetItem(I);
      if P.FileAction = haOther then
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
            if FHeaders.AlreadyFileExists(I, [haCopy, haOther], S) <> -1 then
              DoMessage(Format(cmFileExistsWarning, [S]))
            else
              Break;
          end else
            Break;
        until False;
        StrDispose(FI.FileName);
        StrDispose(FI.FilePath);

        if (Length(S) <> 0) and (CompareFileName(S, P.FileName) <> 0) then
        begin
          Inc(FTotalSize, P.FilePacked);
          P.FileName := S;
        end;
      end;
    end;
  end;
end;

procedure TBeeApp.ProcessFilesToList;
var
  I: longint;
  P: THeader;
begin
  DoMessage(Format(msgScanning, ['...']));
  FHeaders.MarkItems(FCommandLine.FileMasks, haNone,  haOther);
  FHeaders.MarkItems(FCommandLine.xOptions,  haOther, haNone);

  for I := 0 to FHeaders.GetCount - 1 do
  begin
    if Code < ccError then
    begin
      P := FHeaders.GetItem(I);
      if P.FileAction = haOther then
      begin
        Inc(FTotalSize, P.FileSize);
      end;
    end;
  end;
end;

{ Option processing }

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
    DecodeShell(haDecode);
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

{ Shell procedures }

procedure TBeeApp.HelpShell;
begin
  DoMessage(Cr + '  Usage: Bee <command> -<switch 1> -<switch N> <archive-name> <file-names...>');
  DoMessage(Cr + '  Commands:' + Cr);
  DoMessage('    a  Add files to archive');
  DoMessage('    d  Delete files from archive');
  DoMessage('    e  Extract files from archive');
  DoMessage('    l  List archive');
  DoMessage('    r  Rename files in archive');
  DoMessage('    t  Test archive files');
  DoMessage('    x  eXtract files from archive with path name');
  DoMessage(Cr + '  Switches:' + Cr);
  DoMessage('    -              stop switches parsing');
  DoMessage('    cd[directory]  set current archive directory');
  DoMessage('    cfg[filename]  use specified Configuration file');
  DoMessage('    d<0..9>    set Dictionary size (d1 uses < 5M, d2 (default) < 10M, ...)');
  DoMessage('    f          Force file extention');
  DoMessage('    hv<03,04>  set archive file header version');
  DoMessage('    l          List archive after process');
  DoMessage('    m<0..3>    set compression Method (0-store, 1-default, ...)');
  DoMessage('    p          set Password (min length 4 bytes)');
  DoMessage('    pri<0..3>  set process priority (0-Idle, 1-Normal, 2-High, 3-RealTime)');
  DoMessage('    r    Recurse subdirectories');
  DoMessage('    rw   recurse subdirectories only for wildcard names');
  DoMessage('    s    create Solid archive');
  DoMessage('    sfx  add self-extractor module');
  DoMessage('    stl  show technical information for l (list) command');
  DoMessage('    t    Test archive after process');
  DoMessage('    u<0..5>  Update files method (0-add files, 1-update older files, 2-replace');
  DoMessage('             files, 3-add and update older files (default), 4-add and replace');
  DoMessage('             files, 5-add and autorename existing files)');
  DoMessage('    wd[direcroty]  set temporany work directory');
  DoMessage('    x[filenames]   eXclude filenames');
  DoMessage(Cr + '  Use BeeOpt to make most optimal parameters.' + Cr);
end;

procedure TBeeApp.EncodeShell;
var
  I: longint;
  P: THeader;
  Encoder: TEncoder;
begin
  OpenArchive(haCopy);
  if Code < ccError then
  begin
    ProcessFilesToAdd;
    if FTotalSize <> 0 then
    begin
      FTempName := GenerateFileName(FCommandLine.wdOption);
      FTempFile := CreateTFileWriter(FTempName, fmCreate);
      if FTempFile <> nil then
      begin
        ProcessFilesToUpdate; // find sequences
        ProcessFilesToSwap;   // decode solid sequences
        if Code < ccError then
        begin
          if Length(FSwapName) <> 0 then // if exists a modified
          begin                          // solid sequence open swap file
            FSwapFile := CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);
            if FSwapFile = nil then
              DoMessage(cmSwapOpenError, ccError);
          end;

          if Code < ccError then
          begin
            FHeaders.WriteItems(FTempFile);
            Encoder := TEncoder.Create(FTempFile, Self);
            for I := 0 to FHeaders.GetCount - 1 do
            begin
              if Code < ccError then
              begin
                P := FHeaders.GetItem(I);
                case P.FileAction of
                  haAdd:     Encoder.EncodeFile(P, emNorm);
                  haUpdate:  Encoder.EncodeFile(P, emNorm);
                  haCopy:    Encoder.CopyStrm  (P, emNorm, FArcFile, P.FileStartPos, P.FilePacked, False);
                  haExtract: Encoder.EncodeStrm(P, emNorm, FSwapFile, P.FileSize, foPassword in P.FileFlags);
                end;
              end;
            end;
            Encoder.Destroy;
            FHeaders.WriteItems(FTempFile);
          end;
        end;
      end else
        DoMessage(cmTempOpenError, ccError);
    end else
      DoMessage(cmNoFilesWarning, ccWarning);
  end;
  CloseArchive(FTotalSize <> 0);
end;

procedure TBeeApp.DecodeShell(const aAction: THeaderAction);
var
  I: longint;
  P: THeader;
  Decoder: TDecoder;
begin
  OpenArchive(haNone);
  if Code < ccError then
  begin
    case aAction of
      haExtract: ProcessFilesToExtract;
      haDecode:  ProcessFilesToTest;
    end;

    if FTotalSize <> 0 then
    begin
      ProcessFilesToDecode(aAction);
      Decoder := TDecoder.Create(FArcFile, Self);
      for I := 0 to FHeaders.GetCount - 1 do
      begin
        if Code < ccError then
        begin
          P := FHeaders.GetItem(I);
          case P.FileAction of
            haExtract: Decoder.DecodeFile(P, pmNorm);
            haDecode:  Decoder.DecodeFile(P, pmNul);
            haSkip:    Decoder.Decodefile(P, pmSkip);
          end;
        end;
      end;
      Decoder.Destroy;
    end else
      DoMessage(cmNoFilesWarning, ccWarning);
  end;
  CloseArchive(False);
end;

procedure TBeeApp.DeleteShell;
var
  TmpFileName: string;
  TmpFile: TFileWriter;
  I: longint;
  P: THeader;
  Encoder: TEncoder;
begin
  OpenArchive(haCopy);
  if Code < ccError then
  begin
    ProcessFilesToDelete;
    if FTotalSize <> 0 then
    begin
      TmpFileName := GenerateFileName(FCommandLine.wdOption);
      TmpFile := CreateTFileWriter(TmpFileName, fmCreate);
      if (TmpFile <> nil) then
      begin
        ProcessFilesToSwap;    // decode solid sequences
        if Code < ccError then
        begin

          if Length(FSwapName) > 0 then  // if SwapSequences has found a
          begin                          // modified sequence open Swap file
            FSwapFile := CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);
            if FSwapFile = nil then
              DoMessage(cmSwapOpenError, ccError);
          end;

          if FSwapFile <> nil then
          begin
            FHeaders.WriteItems(TmpFile);
            Encoder := TEncoder.Create(TmpFile, Self);
            for I := 0 to FHeaders.GetCount -1 do
            begin
              if Code < ccError then
              begin
                P := FHeaders.GetItem(I);
                case P.FileAction of
                  haCopy:    Encoder.CopyStrm  (P, emNorm, FArcFile, P.FileStartPos, P.FilePacked, False);
                  haExtract: Encoder.EncodeStrm(P, emNorm, FSwapFile, P.FileSize, foPassword in P.FileFlags);
                  haDelete:  DoMessage(Format(msgDeleting, [P.FileName]));
                end;
              end;
            end;
            Encoder.Destroy;
            FHeaders.WriteItems(TmpFile);
          end;
        end;
      end else
        DoMessage(cmTempOpenError, ccError);
    end else
      DoMessage(cmNoFilesWarning, ccWarning);
  end;
  CloseArchive(FTotalSize <> 0);
end;

procedure TBeeApp.RenameShell;
var
  Encoder: TEncoder;
  P: THeader;
  I: longint;
begin
  OpenArchive(haCopy);
  if Code < ccError then
  begin
    ProcessFilesToRename;
    if FTotalSize <> 0 then
    begin
      FTempName := GenerateFileName(FCommandLine.wdOption);
      FTempFile := CreateTFileWriter(FTempName, fmCreate);

      if (FTempFile <> nil) then
      begin
        FHeaders.WriteItems(FTempFile);
        Encoder := TEncoder.Create(FTempFile, Self);
        for I := 0 to FHeaders.GetCount -1 do
        begin
          if Code < ccError then
          begin
            P := FHeaders.GetItem(I);
            Encoder.CopyStrm(P, emNorm, FArcFile, P.FileStartPos, P.FilePacked, False);
          end;
        end;
        Encoder.Destroy;
        FHeaders.WriteItems(FTempFile);
      end else
        DoMessage(cmTempOpenError, ccError);
    end else
      DoMessage(cmNoFilesWarning, ccWarning);
  end;
  CloseArchive(FTotalSize <> 0);
end;

procedure TBeeApp.ListShell;
var
  I: longint;
  P: THeader;
  FI: TFileInfoExtra;
  {$IFDEF CONSOLEAPPLICATION}
  FHeadersToListPath: string;
  FHeadersToList: TList;
  {$ENDIF}
  TotalPack, TotalSize, TotalFiles: longint;
  Version, Method, Dictionary: longint;
begin
  {$IFDEF CONSOLEAPPLICATION}
  FHeadersToList := TList.Create;
  {$ENDIF}

  OpenArchive(haNone);
  if Code < ccError then
  begin
    ProcessFilesToList;
    if FTotalSize <> 0 then
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

      for I := 0 to FHeaders.GetCount -1 do
      begin
        P := FHeaders.GetItem(I);

        if foVersion in P.FileFlags then
          Version := P.FileVersion;

        if foMethod in P.FileFlags then
          Method := P.FileMethod;

        if foDictionary in P.FileFlags then
          Dictionary := P.FileDictionary;

        if P.FileAction = haOther then
        begin
          P.FileVersion := Version;
          P.FileMethod := Method;
          P.FileDictionary := Dictionary;
          {$IFDEF CONSOLEAPPLICATION}
          FHeadersToList.Add(P);
          {$ENDIF}
        end;
      end;

      TotalSize  := 0;
      TotalPack  := 0;
      TotalFiles := 0;

      {$IFDEF CONSOLEAPPLICATION}
      if not FCommandLine.stlOption then
      begin
        FHeadersToList.Sort(CompareFn);
        FHeadersToListPath := '';
      end;
      {$ENDIF}

      I := 0;
      {$IFDEF CONSOLEAPPLICATION}
      while I < FHeadersToList.Count do
      begin
        P := FHeadersToList.Items[I];
      {$ELSE}
      while I < FHeaders.GetCount do
      begin
        P := FHeaders.GetItem(I);
      {$ENDIF}

        FI.FileName := StringToPChar(ExtractFileName(P.FileName));
        FI.FilePath := StringToPChar(ExtractFilePath(P.FileName));

        {$IFDEF CONSOLEAPPLICATION}
        if CompareFileName(FHeadersToListPath, FI.FilePath) <> 0 then
        begin
          FHeadersToListPath := FI.FilePath;
          if I = 0 then
            DoMessage(FHeadersToListPath)
          else
            DoMessage(Cr + FHeadersToListPath);
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
        FI.FilePosition := FHeaders.GetNext(0, haOther, P.FileName);
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
      if FHeaders.GetModule > 0 then
        DoMessage(Cr + 'Note: Bee Self-Extractor module founded');
      {$ENDIF}
    end else
      DoMessage(cmNoFilesWarning, ccWarning);
  end;
  CloseArchive(False);
  {$IFDEF CONSOLEAPPLICATION}
  FHeadersToList.Free;
  {$ENDIF}
end;

{ Protected string routines }

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

