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

    v0.8.0 build 1157 - 2010.11.06 by Melchiorre Caruso.
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
    function OpenArchive: longint;
    function CheckArchivePassword: longint;
    procedure CloseArchive(IsModified: boolean);
    { find and prepare sequences }
    function SetItemsToEncode: int64;
    function SetItemsToDelete: int64;
    function SetItemsToDecode: int64;
    // procedure SetItemsToTest: int64;
    function SetItemsToRename: int64;
    function SetItemsToList: int64;
    // procedure SetItemsDecode(const aAction: THeaderAction);
    procedure OpenSwapFile;
    // procedure CloseSwapFile;
    procedure RecoverSequences;
    { process options }
    procedure ProcesstOption;
    procedure ProcesslOption;
    { sheels routines}
    procedure HelpShell;
    procedure EncodeShell;
    procedure DeleteShell;
    procedure DecodeShell;
    procedure TestShell;
    procedure RenameShell;
    procedure ListShell;
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  Math,
  SysUtils,
  Bee_Consts,
  Bee_MainPacker;

{ TBeeApp class }

constructor TBeeApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  Randomize; { randomize, uses for unique filename generation }
  FSelfName := 'The Bee 0.8.0 build 1159 archiver utility, Nov 2010' + Cr +
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
  if (FCommandLine.Command <> ccNone) and (FCommandLine.ArchiveName <> '') then
  begin
    case FCommandLine.Command of
      ccAdd:      EncodeShell;
      ccDelete:   DeleteShell;
      ccExtract:  DecodeShell;
      ccxExtract: DecodeShell;
      ccTest:     TestShell;
      ccRename:   RenameShell;
      ccList:     ListShell;
      ccHelp:     HelpShell;
    end;

    if FCommandLine.Command in [ccAdd, ccDelete, ccRename] then
    begin
      ProcesstOption;
      ProcesslOption;
    end;
  end else
    HelpShell;

  SetTerminated(True);
end;

{ Open / Close archive routines }

function TBeeApp.OpenArchive: longint;
begin
  DoMessage(Format(Cr + cmOpening, [FCommandLine.ArchiveName]));
  FHeaders := THeaders.Create(FCommandLine);
  if FileExists(FCommandLine.ArchiveName) then
  begin
    FArcFile := CreateTFileReader(FCommandLine.ArchiveName, fmOpenRead + fmShareDenyWrite);
    if FArcFile <> nil then
    begin
      FHeaders.Read(FArcFile);
      if (FHeaders.Count = 0) and (FArcFile.Size <> 0) then
        DoMessage(Format(cmArcTypeError, []), ccError);
    end else
      DoMessage(Format(cmOpenArcError, [FCommandLine.ArchiveName]), ccError);
  end;

  if FCommandLine.Command in [ccAdd, ccExtract, ccXextract, ccDelete, ccTest] then
  begin
    CheckArchivePassword;
  end;
  Result := Code;
end;

function TBeeApp.CheckArchivePassword: longint;
var
  P: THeader;
  I, J: longint;
  Decoder: THeaderStreamDecoder;
begin
  if (Code < ccError) and (FHeaders.GetNext(0, foPassword) > -1) then
  begin
    // select smaller size item
    P := FHeaders.Items[0];
    for I := 1 to FHeaders.Count - 1 do
      if (foTear in FHeaders.Items[I].Flags) and (P.Size > FHeaders.Items[I].Size) then
      begin
        P := FHeaders.Items[I];
      end;
    // test item
    DoMessage(Format(cmChecking, [P.Name]));
    Decoder := THeaderStreamDecoder.Create(FArcFile, nil);
    for I := 0 to FHeaders.IndexOf(P) do
    begin
      Decoder.InitializeCoder(FHeaders.Items[I]);
    end;

    FArcFile.StartDecode(FCommandLine.pOption);
    if not Decoder.DecodeToNul(P) then
    begin
      DoMessage(Format(cmTestPswError, [FCommandLine.ArchiveName]), ccError);
    end;
    FArcFile.FinishDecode;
    Decoder.Free;
  end;
  Result := Code;
end;

procedure TBeeApp.CloseArchive(IsModified: boolean);
var
  S: string;
begin
  if Assigned(FTempFile) then FreeAndNil(FTempFile);
  if Assigned(FSwapFile) then FreeAndNil(FSwapFile);
  if Assigned(FArcFile)  then FreeAndNil(FArcFile);

  if IsModified then
  begin
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
  end;

  S := TimeDifference(FStartTime);
  case Code of
    ccSuccesful: DoMessage(Format(Cr + cmSuccesful, [S]));
    ccWarning:   DoMessage(Format(Cr + cmWarning,   [S]));
    ccUserAbort: DoMessage(Format(Cr + cmUserAbort, [S]));
    else         DoMessage(Format(Cr + cmError,     [S]));
  end;
  FHeaders.Free;
end;

{ Sequences processing }

function TBeeApp.SetItemsToEncode: int64;
var
  P: THeader;
  S: TFileScanner;
  I, J, BackTear, NextTear: longint;
begin
  DoMessage(Format(cmScanning, ['...']));
  // STEP1: scan file system ...
  S := TFileScanner.Create;
  with FCommandLine do
    for I := 0 to FileMasks.Count - 1 do
      S.Scan(FileMasks[I], xOptions, rOption);

  FHeaders.SetActions([haCopy]);
  for I := 0 to S.Count - 1 do
    case FCommandLine.uOption of
      umAdd:           FHeaders.Add          (S.Items[I]);
      umUpdate:        FHeaders.Update       (S.Items[I]);
      umReplace:       FHeaders.Replace      (S.Items[I]);
      umAddUpdate:     FHeaders.AddUpdate    (S.Items[I]);
      umAddReplace:    FHeaders.AddReplace   (S.Items[I]);
      umAddAutoRename: FHeaders.AddAutoRename(S.Items[I]);
    end;
  S.Free;

  // STEP2: configure new items ...
  FHeaders.Configure(FConfiguration);
  // STEP3: find sequences and set actions ...
  I := FHeaders.GetBack(FHeaders.Count - 1, haUpdate);
  while I > -1 do
  begin
    BackTear := FHeaders.GetBack(I, foTear);
    NextTear := FHeaders.GetBack(I + 1, foTear);

    if NextTear = -1 then
      NextTear := FHeaders.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := FHeaders.GetBack(NextTear, haCopy);
      for J := BackTear to NextTear do
      begin
        Include(FHeaders.Items[J].Actions, haDecode);
      end;
      I := BackTear;
    end;
    I := FHeaders.GetBack(I - 1, haUpdate);
  end;

  // STEP4: calculate bytes to process ...
  if FHeaders.GetNext(0, haUpdate) > -1 then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      if P.Actions = [haUpdate]           then Inc(FSize, P.ExtSize);
      if P.Actions = [haUpdate, haDecode] then Inc(FSize, P.Size + P.ExtSize);
      if P.Actions = [haCopy]             then Inc(FSize, P.PackedSize);
      if P.Actions = [haCopy, haDecode]   then Inc(FSize, P.Size + P.Size);
    end;
  if FSize = 0 then
  begin
    DoMessage(cmNoFilesWarning, ccWarning)
  end;
  Result := FSize;
end;

function TBeeApp.SetItemsToDelete: int64;
var
  P: THeader;
  I, J, BackTear, NextTear: longint;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetActions(FCommandLine.FileMasks, [haCopy], []);
  FHeaders.SetActions(FCommandLine.xOptions,  [], [haCopy]);

  // STEP1: find sequences and set actions ...
  I := FHeaders.GetBack(FHeaders.Count - 1, []);
  while I > -1 do
  begin
    BackTear := FHeaders.GetBack(I, foTear);
    NextTear := FHeaders.GetNext(I + 1, foTear);

    if NextTear = -1 then
      NextTear := FHeaders.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := FHeaders.GetBack(NextTear - 1, haCopy);
      for J := BackTear to NextTear do
      begin
        Include(FHeaders.Items[J].Actions, haDecode);
      end;
      I := BackTear;
    end;
    I := FHeaders.GetBack(I - 1, []);
  end;

  // STEP4: calculate bytes to process ...
  if FHeaders.GetNext(0, []) > -1 then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      if P.Actions = [haCopy]           then Inc(FSize, P.PackedSize);
      if P.Actions = [haCopy, haDecode] then Inc(FSize, P.Size + P.Size);
    end;
  Result := FSize;
end;

function TBeeApp.SetItemsToDecode: int64;
var
  I, J: longint;
  P: THeader;
  U: TUpdateMode;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetActions(FCommandLine.FileMasks, [], [haUpdate]);
  FHeaders.SetActions(FCommandLine.xOptions,  [haUpdate], []);
  // STEP1: overwrite routines ...
  if FCommandline.Command in [ccXextract, ccExtract] then
  begin
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      if (P.Actions = [haUpdate]) and (Code < ccError) then
      begin
        case FCommandLine.Command of
          ccExtract:  P.ExtName := ExtractFileName(P.Name);
          ccXextract: P.ExtName := DeleteFilePath(FCommandLine.cdOption, P.Name);
        end;

        case FCommandLine.uOption of
          umUpdate:    if (not FileExists(P.ExtName)) or  (P.Time <= FileAge(P.ExtName)) then P.Actions := [];
          umAddUpdate: if (    FileExists(P.ExtName)) and (P.Time <= FileAge(P.ExtName)) then P.Actions := [];
          umReplace:   if (not FileExists(P.ExtName)) then P.Actions := [];
          umAdd:       if (    FileExists(P.ExtName)) then P.Actions := [];
          // umAddReplace: extract file always
          umAddAutoRename: if FileExists(P.ExtName) then P.ExtName := GenerateAlternativeFileName(P.ExtName, 1, True);
        end;
      end;
    end;
  end;

  // STEP2: find sequences and mark ...
  I := FHeaders.GetBack(FHeaders.Count - 1, haUpdate);
  while I > -1 do
  begin
    J := FHeaders.GetBack(I, foTear);
    if J > -1 then
      repeat
        if FHeaders.Items[J].Actions = [] then
        begin
          Include (FHeaders.Items[J].Actions, haDecode);
        end;
        Inc(J);
      until (J > I);
    I := FHeaders.GetBack(I - 1, haUpdate);
  end;

  // STEP4: calculate bytes to process ...
  if FHeaders.GetNext(0, [haUpdate]) > -1 then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      if P.Actions = [haUpdate] then Inc(FSize, P.Size);
      if P.Actions = [haDecode] then Inc(FSize, P.Size);
    end;
  if FSize = 0 then
  begin
    DoMessage(cmNoFilesWarning, ccWarning);
  end;
  Result := FSize;
end;

function TBeeApp.SetItemsToRename: int64;
var
  I: longint;
  P: THeader;
  S: string;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetActions(FCommandLine.FileMasks, [], [haUpdate]);
  FHeaders.SetActions(FCommandLine.xOptions,  [haUpdate], []);

  for I  := 0 to FHeaders.Count - 1 do
    if (Code < ccError) then
    begin
      P := FHeaders.Items[I];
      if P.Actions = [haUpdate] then
      begin
        repeat
          S := FixFileName(DoRename(P, ''));
          if FHeaders.Search(S) <> nil then
            DoMessage(Format(cmFileExistsWarning, [S]))
          else
            Break;
        until False or (Code >= ccError);

        if S <> '' then P.Name := S;
      end;
    end;

  // STEP4: calculate bytes to process ...
  if FHeaders.GetNext(0, haUpdate) > -1 then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      if P.Actions = [haUpdate] then Inc(FSize, P.PackedSize);
    end;
  Result := FSize;
end;

function TBeeApp.SetItemsToList: int64;
var
  I: longint;
  P: THeader;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetActions(FCommandLine.FileMasks, [], [haUpdate]);
  FHeaders.SetActions(FCommandLine.xOptions,  [haUpdate], []);

  // STEP4: calculate bytes to process ...
  if FHeaders.GetNext(0, haUpdate) > -1 then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      if P.Actions = [haUpdate] then Inc(FSize, P.PackedSize);
    end;
  if FSize = 0 then
  begin
    DoMessage(cmNoFilesWarning, ccWarning)
  end;
  Result := FSize;
end;

procedure TBeeApp.OpenSwapFile;
var
  P: THeader;
  I: longint;
  CRC: longword;
  FSwapStrm: TFileWriter;
  Decoder: THeaderStreamDecoder;
begin
  if (Code < ccError) and (FHeaders.GetNext(0, haDecode) > -1) then
  begin
    FSwapName := GenerateFileName(FCommandLine.wdOption);
    FSwapStrm := CreateTFileWriter(FSwapName, fmCreate);
    if Assigned(FSwapStrm) then
    begin
      Decoder := THeaderStreamDecoder.Create(FArcFile, DoTick);

      for I := 0 to FHeaders.Count - 1 do
        if (Code < ccError) then
        begin
          P := FHeaders.Items[I];
          Decoder.InitializeCoder(P);

          if haDecode in P.Actions then
          begin
            DoMessage(Format(cmDecoding, [P.Name]));
            if foPassword in P.Flags then
            begin
              FArcFile.StartDecode(FCommandLine.pOption);
              FSwapStrm.StartEncode(FCommandLine.pOption);
            end;
            FArcFile.Seek(P.StartPos, soBeginning);
            P.StartPos := FSwapStrm.Seek(0, soCurrent);
            if (Decoder.DecodeTo(FSwapStrm, P.Size, CRC) <> P.Size) or (P.Crc <> CRC) then
            begin
              DoMessage(Format(cmSequenceError, []), ccError);
            end;
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
            FSwapStrm.FinishEncode;
            FArcFile.FinishDecode;
          end;
        end;
      Decoder.Free;
      FreeAndNil(FSwapStrm);

      if Code < ccError then
      begin
        FSwapFile := CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);
        if not Assigned(FSwapFile) then
          DoMessage(cmOpenSwapError, ccError);
      end;
    end else
      DoMessage(cmCreateSwapError, ccError);
  end;
end;

procedure TBeeApp.RecoverSequences;
var
  I: longint;
  Back, Next: THeader;
begin
  for I := 0 to FHeaders.Count - 2 do
  begin
    Back := FHeaders.Items[I];
    Next := FHeaders.Items[I + 1];

    if Back.Actions = [] then
    begin
      if (foVersion in Back.Flags) and (not(foVersion in Next.Flags)) then
      begin
        Next.Version := Back.Version;
        Include(Next.Flags, foVersion);
      end;

      if (foMethod in Back.Flags) and (not(foMethod in Next.Flags)) then
      begin
        Next.Method := Back.Method;
        Include(Next.Flags, foMethod);
      end;

      if (foDictionary in Back.Flags) and (not(foDictionary in Next.Flags)) then
      begin
        Next.Dictionary := Back.Dictionary;
        Include(Next.Flags, foDictionary);
      end;

      if (foTable in Back.Flags) and (not(foTable in Next.Flags)) then
      begin
        Next.Table := Back.Table;
        Include(Next.Flags, foTable);
      end;

      if (foTear in Back.Flags) and (not(foTear in Next.Flags)) then
      begin
        Include(Next.Flags, foTear);
      end;
    end;
  end;
end;

{ Option processing }

procedure TBeeApp.ProcesstOption;
begin
  if FCommandLine.tOption then
  begin
    FSize := 0;
    FProcessedSize := 0;
    FCommandLine.xOptions.Clear;
    FCommandLine.FileMasks.Clear;
    FCommandLine.FileMasks.Add('*');
    FCommandLine.rOption := rmFull;
    DecodeShell;
  end;
end;

procedure TBeeApp.ProcesslOption;
begin
  if FCommandLine.lOption then
  begin
    FSize := 0;
    FProcessedSize := 0;
    FCommandLine.xOptions.Clear;
    FCommandLine.FileMasks.Clear;
    FCommandLine.FileMasks.Add('*');
    FCommandLine.rOption := rmFull;
    ListShell;
  end;
end;

{ Shell procedures }

procedure TBeeApp.HelpShell;
begin
  DoMessage(Cr + 'Usage: Bee <command> [<switches>...] <archive-name> [<file-names>...]' + Cr);
  DoMessage('<Commands>');
  DoMessage('  a  Add files to archive');
  DoMessage('  d  Delete files from archive');
  DoMessage('  e  Extract files from archive');
  DoMessage('  l  List archive');
  DoMessage('  r  Rename files in archive');
  DoMessage('  t  Test archive files');
  DoMessage('  x  eXtract files from archive with path name');
  DoMessage('<Switches>');
  DoMessage('  --              stop switches parsing');
  DoMessage('  -cd[directory]  set current archive directory');
  DoMessage('  -cfg[filename]  use specified Configuration file');
  DoMessage('  -d<0..9>    set Dictionary size (d1 uses < 5M, d2 (default) < 10M, ...)');
  DoMessage('  -f          Force file extention');
  DoMessage('  -hv<03,04>  set archive file header version');
  DoMessage('  -l          List archive after process');
  DoMessage('  -m<0..3>    set compression Method (0-store, 1-default, ...)');
  DoMessage('  -p          set Password (min length 4 bytes)');
  DoMessage('  -pri<0..3>  set process priority (0-Idle, 1-Normal, 2-High, 3-RealTime)');
  DoMessage('  -r    Recurse subdirectories');
  DoMessage('  -rw   recurse subdirectories only for wildcard names');
  DoMessage('  -s    create Solid archive');
  DoMessage('  -sfx  add self-extractor module');
  DoMessage('  -stl  show technical information for l (list) command');
  DoMessage('  -t    Test archive after process');
  DoMessage('  -u<0..5>  Update files method (0-add files, 1-update older files, 2-replace');
  DoMessage('            files, 3-add and update older files (default), 4-add and replace');
  DoMessage('            files, 5-add and autorename existing files)');
  DoMessage('  -wd[direcroty]  set temporany work directory');
  DoMessage('  -x[filenames]   eXclude filenames');
  DoMessage(Cr + 'Use BeeOpt to make most optimal parameters.' + Cr);
end;

procedure TBeeApp.EncodeShell;
var
  I: longint;
  P: THeader;
  Encoder: THeaderStreamEncoder;
begin
  if OpenArchive < ccError then
  begin
    if SetItemsToEncode > 0 then
    begin
      FTempName := GenerateFileName(FCommandLine.wdOption);
      FTempFile := CreateTFileWriter(FTempName, fmCreate);
      if Assigned(FTempFile) then
      begin
        OpenSwapFile;
        if Code < ccError then
        begin
          FHeaders.Write(FTempFile);
          Encoder := THeaderStreamEncoder.Create(FTempFile, DoTick);
          for I := 0 to FHeaders.Count - 1 do
            if Code < ccError then
            begin
              P := FHeaders.Items[I];
              Encoder.InitializeCoder(P);

              if foPassword in P.Flags then
              begin
                if Assigned(FArcFile)  then FArcFile .StartDecode(FCommandLine.pOption);
                if Assigned(FSwapFile) then FSwapFile.StartDecode(FCommandLine.pOption);
                if Assigned(FTempFile) then FTempFile.StartEncode(FCommandLine.pOption);
              end;

              DoMessage(Format(cmUpdating, [P.Name]));
              if P.Actions = [haUpdate]           then Encoder.EncodeFrom(P);
              if P.Actions = [haUpdate, haDecode] then Encoder.EncodeFrom(P);
              if P.Actions = [haCopy]             then Encoder.CopyFrom(FArcFile,  P.PackedSize, P);
              if P.Actions = [haCopy, haDecode]   then Encoder.EncodeFrom(FSwapFile, P.Size, P);
              {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}

              if Assigned(FTempFile) then FTempFile.FinishEncode;
              if Assigned(FSwapFile) then FSwapFile.FinishDecode;
              if Assigned(FArcFile)  then FArcFile .FinishDecode;
            end;
          Encoder.Destroy;
          FHeaders.Write(FTempFile);
        end;

      end else
        DoMessage(cmOpenTempError, ccError);
    end;
  end;
  CloseArchive(FSize > 0);
end;

procedure TBeeApp.DecodeShell;
var
  I: longint;
  P: THeader;
  Check: boolean;
  Decoder: THeaderStreamDecoder;
begin
  if OpenArchive < ccError then
  begin
    if SetItemsToDecode > 0 then
    begin
      Decoder := THeaderStreamDecoder.Create(FArcFile, DoTick);
      for I := 0  to FHeaders.Count - 1 do
        if Code < ccError then
        begin
          P := FHeaders.Items[I];
          Decoder.InitializeCoder(P);

          if (haUpdate in P.Actions) or (haDecode in P.Actions)  then
          begin
            if foPassword in P.Flags  then FArcFile.StartDecode(FCommandLine.pOption);

            if P.Actions = [haUpdate] then DoMessage(Format(cmExtracting, [P.Name]));
            if P.Actions = [haDecode] then DoMessage(Format(cmDecoding,   [P.Name]));

            // process item ...
            if P.Actions = []         then Check := True;
            if P.Actions = [haUpdate] then Check := Decoder.DecodeTo(P);
            if P.Actions = [haDecode] then Check := Decoder.DecodeToNul(P);
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
            FArcFile.FinishDecode;

            if not Check then DoMessage(Format(cmCrcError, [P.Name]), ccError);
          end;
        end;
      Decoder.Destroy;
    end;
  end;
  CloseArchive(False);
end;

procedure TBeeApp.TestShell;
var
  I: longint;
  P: THeader;
  Check: boolean;
  Decoder: THeaderStreamDecoder;
begin
  if OpenArchive < ccError then
  begin
    if SetItemsToDecode > 0 then
    begin
      Decoder := THeaderStreamDecoder.Create(FArcFile, DoTick);
      for I := 0  to FHeaders.Count - 1 do
        if Code < ccError then
        begin
          P := FHeaders.Items[I];
          Decoder.InitializeCoder(P);

          if (haUpdate in P.Actions) or (haDecode in P.Actions)  then
          begin
            if foPassword in P.Flags  then FArcFile.StartDecode(FCommandLine.pOption);

            if P.Actions = [haUpdate] then DoMessage(Format(cmTesting,  [P.Name]));
            if P.Actions = [haDecode] then DoMessage(Format(cmDecoding, [P.Name]));

            // process item ...
            if P.Actions = []         then Check := True;
            if P.Actions = [haUpdate] then Check := Decoder.DecodeToNul(P);
            if P.Actions = [haDecode] then Check := Decoder.DecodeToNul(P);
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
            FArcFile.FinishDecode;

            if not Check then DoMessage(Format(cmCrcError, [P.Name]), ccError);
          end;
        end;
      Decoder.Destroy;
    end;
  end;
  CloseArchive(False);
end;

procedure TBeeApp.DeleteShell;
begin
(*
var
  I: longint;
  P: THeader;
  Encoder: TEncoder;
begin
  OpenArchive(haCopy);
  if Code < ccError then
  begin
    MarkItems2Delete;
    if FHeaders.GetCount([haDelete]) <> 0 then
    begin
      FTempName := GenerateFileName(FCommandLine.wdOption);
      FTempFile := CreateTFileWriter(FTempName, fmCreate);
      if (FTempFile <> nil) then
      begin
        DecodeSequences; // decode solid sequences
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
            FHeaders.WriteItems(FTempFile);
            Encoder := TEncoder.Create(FTempFile, Self);
            for I := 0 to FHeaders.GetCount -1 do
            begin
              if Code < ccError then
              begin
                P := FHeaders.GetItem(I);
                case P.FileAction of
                  haCopy:    Encoder.CopyStrm  (P, emNorm, FArcFile, P.FileStartPos, P.FilePacked, False);
                  haExtract: Encoder.EncodeStrm(P, emNorm, FSwapFile, P.FileSize, foPassword in P.FileFlags);
                  haDelete:  DoMessage(Format(cmDeleting, [P.FileName]));
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
  *)
end;

procedure TBeeApp.RenameShell;
begin
(*
var
  I: longint;
  P: THeader;
  Encoder: TEncoder;
begin
  OpenArchive(haCopy);
  if Code < ccError then
  begin
    MarkItems2Rename;
    if FHeaders.GetCount([haOther]) <> 0 then
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
  *)
end;

function CompareFn(P1, P2: pointer): longint;
begin
  Result := CompareFileName(
    ExtractFilePath(THeader(P1).Name),
    ExtractFilePath(THeader(P2).Name));

  if Result = 0 then
  begin
    Result := CompareText(
      ExtractFileName(THeader(P1).Name),
      ExtractFileName(THeader(P2).Name));
  end;
end;

{$IFDEF CONSOLEAPPLICATION}
procedure TBeeApp.ListShell;
var
  I: longint;
  P: THeader;
  FHeadersToList: TList;
  Version, Method, Dictionary: longint;
  Sequences, Passwords, MaxDict: longint;
  TotalPacked, TotalSize, TotalFiles: int64;
begin
  OpenArchive;
  if Code < ccError then
  begin
    if SetItemsToList > 0 then
    begin
      Version     := -1;
      Method      := -1;
      Dictionary  := -1;

      Sequences   :=  0;
      Passwords   :=  0;
      MaxDict     :=  0;

      TotalPacked :=  0;
      TotalSize   :=  0;
      TotalFiles  :=  0;

      FHeadersToList := TList.Create;
      for I := 0 to FHeaders.Count - 1 do
        if Code < ccError then
        begin
          P := FHeaders.Items[I];
          if foVersion    in P.Flags then Version    := P.Version    else P.Version    := Version;
          if foMethod     in P.Flags then Method     := P.Method     else P.Method     := Method;
          if foDictionary in P.Flags then Dictionary := P.Dictionary else P.Dictionary := Dictionary;
          if foTear       in P.Flags then Inc(Sequences);
          if foPassword   in P.Flags then Inc(Passwords);

          if Dictionary > MaxDict then MaxDict := Dictionary;

          Inc(TotalPacked, P.PackedSize);
          Inc(TotalSize, P.Size);
          Inc(TotalFiles);

          if haUpdate in P.Actions then FHeadersToList.Add(P);
        end;

      DoMessage(Cr + 'Extraction requirements:');
      DoMessage('  Headers version  = ' + VersionToStr(Version));
      DoMessage('  Free memory size = ' + IntToStr(Trunc(Power(2, MaxDict)*2560000)));
      DoMessage('  Free disk space  = ' + IntToStr(TotalSize));

      DoMessage(Cr + 'Archive features:');
      if Passwords > 0 then
        DoMessage('  Password  = yes' )
      else
        DoMessage('  Password  = no' );

      if Sequences <> FHeaders.Count then
        DoMessage('  Solid     = yes')
      else
        DoMessage('  Solid     = no');

      DoMessage('  Items     = ' + IntToStr(TotalFiles));
      DoMessage('  Sequences = ' + IntToStr(Sequences));

      DoMessage('  Module  size = ' + IntToStr(FHeaders.ModuleSize));
      DoMessage('  Packed  size = ' + IntToStr(TotalPacked));
      DoMessage('  Archive size = ' + SizeToStr(SizeOfFile(FCommandLine.ArchiveName)));

      DoMessage(Cr + '   Date      Time     Attr          Size       Packed MTD Name                 ');
      DoMessage(     '---------- -------- ------- ------------ ------------ --- ---------------------');

      if FCommandLine.slsOption then FHeadersToList.Sort(CompareFn);

      TotalPacked := 0;
      TotalSize   := 0;
      TotalFiles  := 0;

      for I := 0 to FHeadersToList.Count - 1 do
        if Code < ccError then
        begin
          P := FHeadersToList.Items[I];
          Inc(TotalPacked, P.PackedSize);
          Inc(TotalSize, P.Size);
          Inc(TotalFiles);
          DoList(P);
        end;
      DoMessage('---------- -------- ------- ------------ ------------ --- ---------------------');
      DoMessage(StringOfChar(' ', 27) + Format(' %12s %12s     %d file(s)', [SizeToStr(TotalSize), SizeToStr(TotalPacked), TotalFiles]));

      FHeadersToList.Destroy;
    end;
  end;
  CloseArchive(False);
end;
{$ELSE}
procedure TBeeApp.ListShell;
var
  I: longint;
  P: THeader;
  Version, Method, Dictionary: longint;
begin
  OpenArchive;
  if Code < ccError then
  begin
    if SetItemsToList > 0 then
    begin
      Version     := -1;
      Method      := -1;
      Dictionary  := -1;

      for I := 0 to FHeaders.Count - 1 do
        if Code < ccError then
        begin
          P := FHeaders.Items[I];
          if foVersion    in P.Flags then Version    := P.Version    else P.Version    := Version;
          if foMethod     in P.Flags then Method     := P.Method     else P.Method     := Method;
          if foDictionary in P.Flags then Dictionary := P.Dictionary else P.Dictionary := Dictionary;

          DoList(P);
        end;
    end;
  end;
  CloseArchive(False);
end;
{$ENDIF}

end.

