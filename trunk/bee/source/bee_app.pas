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
    FPassword: string;
    FCommandLine: TCommandLine;
    FConfiguration: TConfiguration;
    { open/close archive routine }
    procedure OpenArchive;
    function CheckArchivePassword: boolean;
    procedure CloseArchive(IsModified: boolean);
    { find and prepare sequences }
    procedure SetItemsToAdd;
    procedure SetItemsToDelete;
    procedure SetItemsToExtract;
    procedure SetItemsToTest;
    procedure SetItemsToRename;
    procedure SetItemsToList;
    procedure SetItemsToUpdate;
    procedure SetItemsDecode(const aAction: THeaderAction);
    procedure ExtractToSwapFile;
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
  Bee_MainPacker2;

//function CompareFunc(P1, P2: pointer): longint;
//begin
//  Result := CompareFileName(
//    ExtractFilePath(THeader(P1).Name),
//    ExtractFilePath(THeader(P2).Name));
//
//  if Result = 0 then
//  begin
//    Result := CompareFileName(
//      ExtractFileName(THeader(P1).Name),
//      ExtractFileName(THeader(P2).Name));
//  end;
//end;

{ TBeeApp class }

constructor TBeeApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  Randomize; { randomize, uses for unique filename generation }
  FSelfName := 'The Bee 0.8.0 build 1134 archiver utility, May 2010' + Cr +
               '(C) 1999-2010 Andrew Filinsky and Melchiorre Caruso';

  FHeaders  := nil;
  FArcFile  := nil;
  FSwapFile := nil;
  FTempFile := nil;
  FSwapName := '';
  FTempName := '';
  FPassword := '';

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
      begin
        DoMessage(Format(cmArcTypeError, []), ccError);
      end;
    end else
      DoMessage(Format(cmArcOpenError, [FCommandLine.ArchiveName]), ccError);
  end;
end;

function TBeeApp.CheckArchivePassword: boolean;
var
  I: longint;
  P: THeader;
  Decoder: THeaderStreamCoder;
begin
  Result := FHeaders.GetNext(0, foPassword) = -1;
  if not Result then
  begin
    // get password
    FPassword := IGetPassword;
    // select smaller size item
    P := FHeaders.Items[0];
    for I := 1 to FHeaders.Count - 1 do
      if P.Size > FHeaders.Items[I].Size then
        P := FHeaders.Items[I];
    // test item
    Decoder := THeaderStreamCoder.Create(FArcFile);
    Decoder.InitializeCoder(P);

    FArcFile.StartDecode(FPassword);
    Result := Decoder.Test(P);
    if not Result then
    begin
      DoMessage(Format(cmTestPswError, []), ccError);
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
  I: longint;
  S: TFileScanner;
begin
  DoMessage(Format(cmScanning, ['...']));
  S := TFileScanner.Create;
  with FCommandLine do
    for I := 0 to FileMasks.Count - 1 do
    begin
      S.Scan(FileMasks[I], xOptions, rOption);
    end;

  for I := 0 to S.Count - 1 do
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
  S.Free;

  FHeaders.Configure(FConfiguration);
end;

procedure TBeeApp.SetItemsToUpdate;
var
  I, J: longint;
begin
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

  if FCommandline.Command in [ccXextract, ccExtract] then
    for I  := 0 to FHeaders.Count - 1 do
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
  S: string;
  I: longint;
  P: THeader;
  FI: TFileInfo;
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
        FI := NewFileInfo(P);
        repeat
          S := FixFileName(DoRename(FI, ''));
          if FHeaders.Search(S) <> nil then
            DoMessage(Format(cmFileExistsWarning, [S]))
          else
            Break;
        until False;
        FreeFileInfo(FI);

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

procedure TBeeApp.ExtractToSwapFile;
var
  P: THeader;
  I, CRC: longint;
  FSwapStrm: TFileWriter;
  Decoder: THeaderStreamCoder;
begin
  FSwapName := GenerateFileName(FCommandLine.wdOption);
  FSwapStrm := CreateTFileWriter(FSwapName, fmCreate);
  if FSwapStrm <> nil then
  begin
    Decoder := THeaderStreamCoder.Create(FArcFile);
    for I := 0 to FHeaders.Count - 1 do
      if Code < ccError then
      begin
        P := FHeaders.Items[I];

        Decoder.InitializeCoder(P);
        if P.Action = haExtract then
        begin
          if foPassword in P.Flags then
          begin
            FArcFile.StartDecode(FPassword);
            FSwapStrm.StartEncode(FPassword);
          end;
          FArcFile.Seek(P.StartPos, soBeginning);

          P.StartPos := FSwapStrm.Seek(0, soCurrent);
          if Decoder.DecodeTo(FSwapStrm, P.Size) <> P.Crc then
          begin
            DoMessage(Format(cmSequenceError, []), ccError);
          end;
          FSwapStrm.FinishEncode;
          FArcFile.FinishDecode;
        end;
      end;
    Decoder.Free;
    FreeAndNil(FSwapStrm);
  end else
    DoMessage(cmSwapOpenError, ccError);
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
  Encoder: THeaderStreamCoder;
begin
  OpenArchive;
  if Code < ccError then
  begin
    MarkItems2Add;
    if FHeaders.ActionCount[[haNew, haUpdate]] <> 0 then
    begin
      FTempName := GenerateFileName(FCommandLine.wdOption);
      FTempFile := CreateTFileWriter(FTempName, fmCreate);
      if FTempFile <> nil then
      begin
        MarkItems2Update;
        DecodeSequences;
        if Code < ccError then
        begin
          if Length(FSwapName) <> 0 then
          begin
            FSwapFile := CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);
            if FSwapFile = nil then
            begin
              DoMessage(cmSwapOpenError, ccError);
            end;
          end;

          if Code < ccError then
          begin
            FHeaders.Write(FTempFile);
            Encoder := THeaderStreamCoder.Create(FTempFile);
            for I := 0 to FHeaders.Count - 1 do
            begin
              if Code < ccError then
              begin
                P := FHeaders.Items[I];

                Encoder.InitializeCoder(P);
                if P.Action in [haNew, haUpdate, haNone, haExtract] then
                begin
                  if foPassword in P.Flags then
                  begin
                    FArcFile.StartDecode(GetPassword(P));
                    FSwapFile.StartDecode(GetPassword(P));
                  end;

                  FArcFile.Seek(P.StartPos, soBeginning);
                  case P.Action of
                    haNew:     Encoder.EncodeFrom(P);
                    haUpdate:  Encoder.EncodeFrom(P);
                    haNone:    Encoder.CopyFrom  (P);
                    haExtract: Encoder.EncodeStrm(P);
                  end;





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
      haExtract: MarkItems2Extract;
      haDecode:  MarkItems2Test;
    end;

    if FHeaders.GetCount([haExtract, haDecode]) <> 0 then
    begin
      MarkItems2Decode(aAction);
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
end;

procedure TBeeApp.RenameShell;
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
    MarkItems2List;
    if FHeaders.GetCount([haOther]) <> 0 then
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

