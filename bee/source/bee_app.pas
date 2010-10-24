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

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
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
    procedure OpenArchive;
    procedure CheckArchivePassword;
    procedure CloseArchive(IsModified: boolean);
    { find and prepare sequences }
    procedure SetItemsToAdd;
    procedure SetItemsToDelete;
    procedure SetItemsToExtract;
    // procedure SetItemsToTest;
    procedure SetItemsToRename;
    procedure SetItemsToList;
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
    procedure DecodeShell(const aAction: THeaderAction);
    procedure RenameShell;
    procedure ListShell;
  protected
    procedure ActionsSize;
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
  FSelfName := 'The Bee 0.8.0 build 1153 archiver utility, Set 2010' + Cr +
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
      ccExtract:  DecodeShell(haExtract);
      ccxExtract: DecodeShell(haExtract);
      ccTest:     DecodeShell(haDecode);
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

procedure TBeeApp.OpenArchive;
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

  // if not (FCommandLine.Command in [ccList, ccRename, ccHelp]) then
  begin
    CheckArchivePassword;
  end;
end;

procedure TBeeApp.CheckArchivePassword;
var
  P: THeader;
  I: longint;
  Decoder: THeaderStreamDecoder;
begin
  if (Code < ccError) and (FHeaders.GetNext(0, foPassword) <> -1) then
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
      {To-do: Aggiungere ripristino archivio in caso di errore}
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

procedure TBeeApp.SetItemsToAdd;
var
  I, J: longint;
  S: TFileScanner;
begin
  DoMessage(Format(cmScanning, ['...']));
  S := TFileScanner.Create;
  with FCommandLine do
    for I := 0 to FileMasks.Count - 1 do
      S.Scan(FileMasks[I], xOptions, rOption);

  for I := 0 to S.Count - 1 do
  begin
    if Code < ccError then
      case FCommandLine.uOption of
        umAdd:           FHeaders.Add          (S.Items[I]);
        umUpdate:        FHeaders.Update       (S.Items[I]);
        umReplace:       FHeaders.Replace      (S.Items[I]);
        umAddUpdate:     FHeaders.AddUpdate    (S.Items[I]);
        umAddReplace:    FHeaders.AddReplace   (S.Items[I]);
        umAddAutoRename: FHeaders.AddAutoRename(S.Items[I]);
        else DoMessage(Format(cmCmdError, []), ccError);
      end;
  end;
  S.Free;
  FHeaders.Configure(FConfiguration);

  // find sequences and set actions ...
  I := FHeaders.GetBack(FHeaders.Count - 1, [haUpdate]);
  while (I > -1) and (Code < ccError) do
  begin
    J := FHeaders.GetBack(I, foTear);
    if J > -1 then
      repeat
        case FHeaders.Items[J].Action of
          haUpdate: {nothing to do};
          haNone:   FHeaders.Items[J].Action := haExtract;
          else      DoMessage(Format(cmSequenceError, []), ccError);
        end;
        Inc(J);
      until (J = FHeaders.Count) or (foTear in FHeaders.Items[J].Flags);

    I := FHeaders.GetBack(I - 1, [haUpdate]);
  end;
  ActionsSize;
end;

procedure TBeeApp.SetItemsToDelete;
var
  I, J: longint;
  P: THeader;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone, haDelete);
  FHeaders.SetAction(FCommandLine.xOptions,  haDelete, haNone);

  // getback ...
  repeat
    I := FHeaders.GetBack(FHeaders.Count - 1, [haDelete]);
    P := FHeaders.Items[I + 1];
    if P <> nil then
      if P.Action = haNone then
        if not (foTear in P.Flags) then Break;
  until I = -1;

  // find sequences and set actions ...
  while (I > -1) and (Code < ccError) do
  begin
    J := FHeaders.GetBack(I, foTear);
    if J > -1 then
      repeat
        case FHeaders.Items[J].Action of
          haDelete: {nothing to do};
          haNone:   FHeaders.Items[J].Action := haExtract;
          else      DoMessage(Format(cmSequenceError, []), ccError);
        end;
        Inc(J);
      until (J = FHeaders.Count) or (foTear in FHeaders.Items[J].Flags);

    // getback ...
    repeat
      I := FHeaders.GetBack(I - 1, [haDelete]);
      P := FHeaders.Items[I + 1];
      if P <> nil then
        if P.Action = haNone then
          if not (foTear in P.Flags) then Break;
    until I = -1;
  end;
end;

procedure TBeeApp.SetItemsToExtract;
var
  I, J: longint;
  P: THeader;
  U: TUpdateMode;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone,    haExtract);
  FHeaders.SetAction(FCommandLine.xOptions,  haExtract, haNone);
  // overwrite routines ...
  if FCommandline.Command in [ccXextract, ccExtract] then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      if (P.Action = haExtract) and (Code < ccError) then
      begin
        case FCommandLine.Command of
          ccXextract: P.Link := DeleteFilePath (FCommandLine.cdOption, P.Name)
          else        P.Link := ExtractFileName(P.Name);
        end;

        case FCommandLine.uOption of
          umUpdate:    if (not FileExists(P.Link)) or  (P.Time <= FileAge(P.Name)) then P.Action := haNone;
          umAddUpdate: if (    FileExists(P.Link)) and (P.Time <= FileAge(P.Name)) then P.Action := haNone;
          umReplace:   if (not FileExists(P.Link)) then P.Action := haNone;
          umAdd:       if (    FileExists(P.Link)) then P.Action := haNone;
          // umAddReplace: extract file always
          umAddAutoRename: if FileExists(P.Name) then P.Name := GenerateAlternativeFileName(P.Name, 1, True);
        end;
      end;
    end;

  // find sequences and mark ...
  I := FHeaders.GetBack(FHeaders.Count - 1, [haExtract]);
  while (I > -1) and (Code < ccError) do
  begin
    J := FHeaders.GetBack(I, foTear);
    if J > -1 then
      repeat
        case FHeaders.Items[J].Action of
          haExtract: {nothing to do};
          haNone:    FHeaders.Items[J].Action := haDecode;
          else       DoMessage(Format(cmSequenceError, []), ccError);
        end;
        Inc(J);
      until (J = FHeaders.Count) or (foTear in  FHeaders.Items[J].Flags);

    I := FHeaders.GetBack(I - 1, [haExtract]);
  end;
end;

procedure TBeeApp.SetItemsToRename;
var
  I: longint;
  P: THeader;
  S: string;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone,   haUpdate);
  FHeaders.SetAction(FCommandLine.xOptions,  haUpdate, haNone);

  for I  := 0 to FHeaders.Count - 1 do
    if Code < ccError then
    begin
      P := FHeaders.Items[I];
      if P.Action = haUpdate then
      begin
        repeat
          S := FixFileName(DoRename(P, ''));
          if FHeaders.Search(S) <> nil then
            DoMessage(Format(cmFileExistsWarning, [S]))
          else
            Break;
        until False;

        if S <> '' then P.Name := S;
      end;
    end;
end;

procedure TBeeApp.SetItemsToList;
var
  I: longint;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone,    haExtract);
  FHeaders.SetAction(FCommandLine.xOptions,  haExtract, haNone);
end;

procedure TBeeApp.OpenSwapFile;
var
  P: THeader;
  I: longint;
  CRC: longword;
  FSwapStrm: TFileWriter;
  Decoder: THeaderStreamDecoder;
begin
  if (Code < ccError) and (FHeaders.GetNext(0, [haExtract]) <> -1) then
  begin
    FSwapName := GenerateFileName(FCommandLine.wdOption);
    FSwapStrm := CreateTFileWriter(FSwapName, fmCreate);
    if Assigned(FSwapStrm) then
    begin
      Decoder := THeaderStreamDecoder.Create(FArcFile, DoTick);
      for I := 0 to FHeaders.Count - 1 do
        if Code < ccError then
        begin
          P := FHeaders.Items[I];

          Decoder.InitializeCoder(P);
          if P.Action = haExtract then
          begin
            DoMessage(Format(cmExtracting, [P.Name]));
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

    if Back.Action = haDelete then
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
    DecodeShell(haDecode);
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
  OpenArchive;
  if Code < ccError then
  begin
    SetItemsToAdd;
    if FHeaders.GetNext(0, [haNew, haUpdate]) <> -1 then
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
              case P.Action of
                haNew:     Encoder.EncodeFrom(P);
                haUpdate:  Encoder.EncodeFrom(P);
                haExtract: Encoder.EncodeFrom(FSwapFile, P.Size, P);
                haNone:    Encoder.CopyFrom  (FArcFile,  P.PackedSize, P);
                else DoMessage(Format(cmActionError, []), ccError);
              end;
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
    end else
      DoMessage(cmNoFilesWarning, ccWarning);
  end;
  CloseArchive(FHeaders.GetNext(0, [haNew, haUpdate]) <> -1);
end;

procedure TBeeApp.DecodeShell(const aAction: THeaderAction);
var
  I: longint;
  P: THeader;
  Decoder: THeaderStreamDecoder;
begin
  OpenArchive;
  if Code < ccError then
  begin
    SetItemsToExtract;
    if FHeaders.GetNext(0, [haExtract]) <> -1 then
    begin
      Decoder := THeaderStreamDecoder.Create(FArcFile, DoTick);
      for I := 0 to FHeaders.Count - 1 do
      begin
        if Code < ccError then
        begin
          P := FHeaders.Items[I];

          Decoder.InitializeCoder(P);
          if foPassword in P.Flags then
          begin
            FArcFile.StartDecode(FCommandLine.pOption);
          end;

          DoMessage(Format(cmExtracting, [P.Name]));
          case P.Action of
            haNone:        {nothing to do};
            haExtract:
              case FCommandLine.Command of
                ccExtract: if not Decoder.DecodeTo   (P) then DoMessage(Format(cmCrcError, [P.Name]), ccError);
                ccTest:    if not Decoder.DecodeToNul(P) then DoMessage(Format(cmCrcError, [P.Name]), ccError);
                else       DoMessage(Format(cmActionError, []), ccError);
              end;
            haDecode:      if not Decoder.DecodeToNul(P) then DoMessage(Format(cmCrcError, [P.Name]), ccError);
            else           DoMessage(Format(cmActionError, []), ccError);
          end;
          {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
          FArcFile.FinishDecode;
        end;
      end;
      Decoder.Destroy;
    end else
      DoMessage(cmNoFilesWarning, ccWarning);
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

procedure TBeeApp.ListShell;
var
  I: longint;
  P: THeader;
  FHeadersToList: TList;
  TotalPack, TotalSize, TotalFiles: longint;
  Version, Method, Dictionary: longint;
begin
  OpenArchive;
  if Code < ccError then
  begin
    SetItemsToList;
    if FHeaders.GetNext(0, [haExtract]) <> -1 then
    begin
      {$IFDEF CONSOLEAPPLICATION}
      DoMessage('');
      DoMessage('   Date      Time     Attr          Size       Packed MTD Name                 ');
      DoMessage('---------- -------- ------- ------------ ------------ --- ---------------------');
      {$ENDIF}

      Version    := -1;
      Method     := -1;
      Dictionary := -1;

      FHeadersToList := TList.Create;
      for I := 0 to FHeaders.Count - 1 do
      begin
        P := FHeaders.Items[I];
        if foVersion in P.Flags then
          Version := P.Version
        else
          P.Version := Version;

        if foMethod in P.Flags then
          Method := P.Method
        else
          P.Method := Method;

        if foDictionary in P.Flags then
          Dictionary := P.Dictionary
        else
          P.Dictionary := Dictionary;

        if P.Action = haExtract then
        begin
          FHeadersToList.Add(P);
        end;
      end;
      {$IFDEF CONSOLEAPPLICATION}
      if FCommandLine.slsOption then
      begin
        FHeadersToList.Sort(CompareFn);
      end;
      {$ENDIF}

      TotalSize  := 0;
      TotalPack  := 0;
      TotalFiles := 0;

      for I := 0 to FHeadersToList.Count - 1 do
      begin
        P := FHeadersToList.Items[I];
        DoList(P);
        Inc(TotalSize, P.Size);
        Inc(TotalPack, P.PackedSize);
        Inc(TotalFiles);
      end;

      {$IFDEF CONSOLEAPPLICATION}
      DoMessage('---------- -------- ------- ------------ ------------ --- ---------------------');
      DoMessage(StringOfChar(' ', 27) + Format(' %12s %12s     %d file(s)', [SizeToStr(TotalSize), SizeToStr(TotalPack), TotalFiles]));
      // self-extractor module size
      if FHeaders.SfxSize > 0 then
        DoMessage(Cr + 'Note: Bee Self-Extractor module founded');
      {$ENDIF}
      FHeadersToList.Free;
    end else
      DoMessage(cmNoFilesWarning, ccWarning);
  end;
  CloseArchive(False);
end;

{ Protected string routines }



procedure TBeeApp.ActionsSize;
var
  I: longint;
begin
  FProcessedSize := 0;
  FSize := 0;
  for I := 0 to FHeaders.Count - 1 do
  begin
    case FHeaders.Items[I].Action of

      haNew:  Inc(FSize, FHeaders.Items[I].Size);
      haNone: Inc(FSize, FHeaders.Items[I].PackedSize);





    end;





  end;
end;


end.

