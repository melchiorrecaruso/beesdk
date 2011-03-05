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

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
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
    FArchReader: TFileReader;
    FSwapName: string;
    FSwapReader: TFileReader;
    FSwapWriter: TFileWriter;
    FTempName: string;
    FTempWriter: TFileWriter;
    FCommandLine: TCommandLine;
    FConfiguration: TConfiguration;
    { open/close archive routine }
    function OpenArchive: longint;
    function CheckArchivePassword: longint;
    procedure CloseArchive(IsModified: boolean);
    { open/close swapfile routine }
    function OpenSwapFile: longint;
    { find and prepare items }
    function SetItemsToEncode: int64;
    function SetItemsToDelete: int64;
    function SetItemsToDecode: int64;
    function SetItemsToRename: int64;
    function SetItemsToList: int64;
    { process options }
    procedure ProcesstOption;
    procedure ProcesslOption;
    { shells routines}
    procedure HelpShell;
    procedure EncodeShell;
    procedure DeleteShell;
    procedure DecodeShell;
    procedure TestShell;
    procedure RenameShell;
    procedure ListShell;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  Math,
  SysUtils,
  Bee_Consts,
  Bee_MainPacker;

{ help functions }

function CompareFilePath(P1, P2: pointer): longint;
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

{ TBeeApp class }

constructor TBeeApp.Create(const aCommandLine: string);
begin
  inherited Create;
  Randomize; { randomize, uses for unique filename generation }
  FSelfName := 'The Bee 0.8.0 build 1285 archiver utility, Feb 2011' + Cr +
               '(C) 1999-2010 Andrew Filinsky and Melchiorre Caruso';

  FHeaders    := nil;
  FArchReader := nil;
  FSwapReader := nil;
  FSwapWriter := nil;
  FTempWriter := nil;
  FSwapName   := '';
  FTempName   := '';

  { store command line }
  FCommandLine := TCommandLine.Create;
  FCommandLine.CommandLine := aCommandLine;

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

{ open/close archive routines }

function TBeeApp.OpenArchive: longint;
begin
  DoMessage(Format(Cr + cmOpening, [FCommandLine.ArchiveName]));
  FHeaders := THeaders.Create(FCommandLine);
  if FileExists(FCommandLine.ArchiveName) then
  begin
    FArchReader := CreateTFileReader(FCommandLine.ArchiveName, fmOpenRead + fmShareDenyWrite);
    if Assigned(FArchReader) then
    begin
      FHeaders.Read(FArchReader);
      if (FHeaders.Count = 0) and (FArchReader.Size <> 0) then
        DoMessage(Format(cmArcTypeError, []), ccError);
    end else
      DoMessage(Format(cmOpenArcError, [FCommandLine.ArchiveName]), ccError);
  end;

  if FCommandLine.Command in [ccAdd, ccDelete] then
  begin
    CheckArchivePassword;
  end;
  Result := Code;
end;

function TBeeApp.CheckArchivePassword: longint;
var
  Item: THeader;
  Smaller, I: longint;
  Decoder: THeaderDecoder;
begin
  if (Code < ccError) and (FHeaders.GetNext(0, foPassword) > -1) then
  begin
    // select smaller size item ...
    Smaller := 0;
    for I := 1 to FHeaders.Count - 1 do
    begin
      Item := FHeaders.Items[I];
      if (foTear in Item.Flags) and (Item.Size < FHeaders.Items[Smaller].Size)  then
      begin
        Smaller := I;
      end;
    end;
    Item := FHeaders.Items[Smaller];

    // test item ...
    DoMessage(Format(cmChecking, [Item.Name]));
    Decoder := THeaderDecoder.Create(FArchReader);
    Decoder.Password := FCommandLine.pOption;
    Decoder.OnUserAbortEvent := DoUserAbort;

    for I := 0 to Smaller do
      Decoder.Initialize(FHeaders.Items[I]);

    if Decoder.ReadToNul(Item) = False then
      DoMessage(Format(cmTestPswError, [FCommandLine.ArchiveName]), ccError);
    Decoder.Free;
  end;
  Result := Code;
end;

procedure TBeeApp.CloseArchive(IsModified: boolean);
var
  S: string;
begin
  if Assigned(FArchReader) then FreeAndNil(FArchReader);
  if Assigned(FSwapWriter) then FreeAndNil(FSwapWriter);
  if Assigned(FSwapReader) then FreeAndNil(FSwapReader);
  if Assigned(FTempWriter) then FreeAndNil(FTempWriter);

  if IsModified then
  begin
    if Code < ccError then
    begin
      SysUtils.DeleteFile(FSwapName);
      SysUtils.DeleteFile(FCommandLine.ArchiveName);

      if RenameFile(FTempName, FCommandLine.ArchiveName) = False then
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

{ open/close swapfile routine }

function TBeeApp.OpenSwapFile: longint;
var
  I: longint;
  P: THeader;
  Decoder: THeaderDecoder;
begin
  if (Code < ccError) and (FHeaders.GetNext(0, haDecode) > -1) then
  begin
    FSwapName   := GenerateFileName(FCommandLine.wdOption);
    FSwapWriter := CreateTFileWriter(FSwapName, fmCreate);
    if Assigned(FSwapWriter) then
    begin
      Decoder := THeaderDecoder.Create(FArchReader);
      Decoder.Password := FCommandLine.pOption;
      Decoder.OnUserAbortEvent := DoUserAbort;

      for I := 0 to FHeaders.Count - 1 do
        if Code < ccError then
        begin
          P := FHeaders.Items[I];
          Decoder.Initialize(P);

          if P.Action in [haDecode, haDecodeAndUpdate] then
          begin
            case P.Action of
              // haNone: nothing to do
              // haUpdate: nothing to do
              haDecode: begin
                DoMessage(Format(cmSwapping, [P.Name]));
                if Decoder.ReadTo(P, FSwapWriter) = False then
                  DoMessage(Format(cmCrcError, [P.Name]), ccError);
              end;
              haDecodeAndUpdate: begin
                DoMessage(Format(cmDecoding, [P.Name]));
                if Decoder.ReadToNul(P) = False then
                  DoMessage(Format(cmCrcError, [P.Name]), ccError);
              end;
            end;
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
          end;
        end;
      Decoder.Free;
      FreeAndNil(FSwapWriter);

      if Code < ccError then
      begin
        FSwapReader := CreateTFileReader(FSwapName, fmOpenRead + fmShareDenyWrite);
        if Assigned(FSwapReader) = False then
          DoMessage(cmOpenSwapError, ccError);
      end;
    end else
      DoMessage(cmCreateSwapError, ccError);
  end;
  Result := Code;
end;

{ sequences processing }

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
    NextTear := FHeaders.GetNext(I + 1, foTear);

    if NextTear = -1 then
      NextTear := FHeaders.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := FHeaders.GetBack(NextTear - 1, haNone);
      for J := BackTear to NextTear do
        case FHeaders.Items[J].Action of
          haNone:               FHeaders.Items[J].Action := haDecode;
          haUpdate:             FHeaders.Items[J].Action := haDecodeAndUpdate;
          // haDecode:          nothing to do
          // haDecodeAndUpdate: nothing to do
        end;
      I := BackTear;
    end;
    I := FHeaders.GetBack(I - 1, haUpdate);
  end;

  // STEP4: calculate bytes to process ...
  if (FHeaders.GetNext(0, haUpdate) > -1) or
     (FHeaders.GetNext(0, haDecodeAndUpdate) > -1) then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      case P.Action of
        haNone:            Inc(FSize, P.PackedSize);
        haUpdate:          Inc(FSize, P.ExtSize);
        haDecode:          Inc(FSize, P.Size + P.Size);
        haDecodeAndUpdate: Inc(FSize, P.Size + P.ExtSize);
      end;
    end;

  if FCommandLine.sfxOption <> '' then
    FHeaders.LoadModule(FCommandLine.sfxOption);

  Result := FSize;
  if Result = 0 then
  begin
    DoMessage(cmNoFilesWarning, ccWarning);
  end;
end;

function TBeeApp.SetItemsToDelete: int64;
var
  P: THeader;
  I, J, BackTear, NextTear: longint;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone, haUpdate);
  FHeaders.SetAction(FCommandLine.xOptions,  haUpdate, haNone);

  // STEP1: find sequences and set actions ...
  I := FHeaders.GetBack(FHeaders.Count - 1, haUpdate);
  while I > -1 do
  begin
    BackTear := FHeaders.GetBack(I, foTear);
    NextTear := FHeaders.GetNext(I + 1, foTear);

    if NextTear = -1 then
      NextTear := FHeaders.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := FHeaders.GetBack(NextTear - 1, haNone);
      for J := BackTear to NextTear do
        case FHeaders.Items[J].Action of
          haNone:               FHeaders.Items[J].Action := haDecode;
          haUpdate:             FHeaders.Items[J].Action := haDecodeAndUpdate;
          // haDecode:          nothing to do
          // haDecodeAndUpdate: nothing to do
        end;
      I := BackTear;
    end;
    I := FHeaders.GetBack(I - 1, haUpdate);
  end;
  // STEP4: calculate bytes to process ...
  if (FHeaders.GetNext(0, haUpdate) > -1) or
     (FHeaders.GetNext(0, haDecodeAndUpdate) > -1) then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      case P.Action of
        haNone:            Inc(FSize, P.PackedSize);
        // haUpdate:       nothing to do
        haDecode:          Inc(FSize, P.Size + P.Size);
        haDecodeAndUpdate: Inc(FSize, P.Size);
      end;
    end;

  if FCommandLine.sfxOption <> '' then
    FHeaders.LoadModule(FCommandLine.sfxOption);

  Result := FSize;
  if Result = 0 then
  begin
    DoMessage(cmNoFilesWarning, ccWarning);
  end;
end;

function TBeeApp.SetItemsToDecode: int64;
var
  P: THeader;
  U: TUpdateMode;
  I, J: longint;
  BackTear, NextTear: longint;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone, haUpdate);
  FHeaders.SetAction(FCommandLine.xOptions,  haUpdate, haNone);
  // STEP1: overwrite routines ...
  if FCommandline.Command in [ccXextract, ccExtract] then
  begin
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      if (P.Action = haUpdate) and (Code < ccError) then
      begin
        case FCommandLine.Command of
          ccExtract:  P.ExtName := ExtractFileName(P.Name);
          ccXextract: P.ExtName := DeleteFilePath(FCommandLine.cdOption, P.Name);
        end;

        case FCommandLine.uOption of
          umUpdate:    if (not FileExists(P.ExtName)) or  (P.Time <= FileAge(P.ExtName)) then P.Action := haNone;
          umAddUpdate: if (    FileExists(P.ExtName)) and (P.Time <= FileAge(P.ExtName)) then P.Action := haNone;
          umReplace:   if (not FileExists(P.ExtName)) then P.Action := haNone;
          umAdd:       if (    FileExists(P.ExtName)) then P.Action := haNone;
          // umAddReplace: extract file always
          umAddAutoRename: if FileExists(P.ExtName)   then P.ExtName := GenerateAlternativeFileName(P.ExtName, 1, True);
        end;
      end;
    end;
  end;

  // STEP2: find sequences and mark ...
  I := FHeaders.GetBack(FHeaders.Count - 1, haUpdate);
  while I > -1 do
  begin
    BackTear := FHeaders.GetBack(I, foTear);
    NextTear := FHeaders.GetNext(I + 1, foTear);

    if NextTear = -1 then
      NextTear := FHeaders.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := FHeaders.GetBack(NextTear - 1, haUpdate);
      for J := BackTear to NextTear do
        case FHeaders.Items[J].Action of
          haNone:               FHeaders.Items[J].Action := haDecode;
          // haUpdate:          FHeaders.Items[J].Action := haDecodeAndUpdate;
          // haDecode:          nothing to do
          // haDecodeAndUpdate: nothing to do
        end;
      I := BackTear;
    end;
    I := FHeaders.GetBack(I - 1, haUpdate);
  end;

  // STEP4: calculate bytes to process ...
  if (FHeaders.GetNext(0, haUpdate) > -1) or
     (FHeaders.GetNext(0, haDecodeAndUpdate) > -1) then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      case P.Action of
        // haNone:            nothing to do
        haUpdate:             Inc(FSize, P.Size);
        haDecode:             Inc(FSize, P.Size);
        // haDecodeAndUpdate: nothing to do
      end;
    end;

  Result := FSize;
  if Result = 0 then
  begin
    DoMessage(cmNoFilesWarning, ccWarning);
  end;
end;

function TBeeApp.SetItemsToRename: int64;
var
  I: longint;
  P, S: THeader;
  NewName: string;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone, haUpdate);
  FHeaders.SetAction(FCommandLine.xOptions,  haUpdate, haNone);

  for I  := 0 to FHeaders.Count - 1 do
    if Code < ccError then
    begin
      P := FHeaders.Items[I];
      if P.Action = haUpdate then
      begin
        repeat
          NewName := FixFileName(DoRename(P, P.Name));
          S := FHeaders.Search(NewName);
          if (S <> nil) and (S <> P) then
            DoMessage(Format(cmFileExistsWarning, [NewName]))
          else
            Break;
        until Code >= ccError;

        if (Length(NewName) = 0) or (CompareFileName(NewName, P.Name) = 0) then
          P.Action := haNone
        else
          P.Name := NewName;
      end;
    end;

  // STEP4: calculate bytes to process ...
  if FHeaders.GetNext(0, haUpdate) > -1 then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      case P.Action of
        haNone:               Inc(FSize, P.PackedSize);
        haUpdate:             Inc(FSize, P.PackedSize);
        // haDecode:          nothing to do
        // haDecodeAndUpdate: nothing to do
      end;
    end;

  Result := FSize;
  if Result = 0 then
  begin
    DoMessage(cmNoFilesWarning, ccWarning);
  end;
end;

function TBeeApp.SetItemsToList: int64;
var
  I: longint;
  P: THeader;
begin
  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone, haUpdate);
  FHeaders.SetAction(FCommandLine.xOptions,  haUpdate, haNone);

  // STEP4: calculate bytes to process ...
  if FHeaders.GetNext(0, haUpdate) > -1 then
    for I := 0 to FHeaders.Count - 1 do
    begin
      P := FHeaders.Items[I];
      case P.Action of
        // haNone:            nothing to do
        haUpdate:             Inc(FSize, P.PackedSize);
        // haDecode:          nothing to do
        // haDecodeAndUpdate: nothing to do
      end;
    end;

  Result := FSize;
  if Result = 0 then
  begin
    DoMessage(cmNoFilesWarning, ccWarning);
  end;
end;

{ option processing }

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
    TestShell;
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

{ shell procedures }

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
  DoMessage('  -sls  show list sorted by filename - for l (list) command');
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
  Check: boolean;
  Encoder: THeaderEncoder;
begin
  if (OpenArchive < ccError) and (SetItemsToEncode > 0) then
  begin
    FTempName   := GenerateFileName(FCommandLine.wdOption);
    FTempWriter := CreateTFileWriter(FTempName, fmCreate);
    if Assigned(FTempWriter) then
    begin
      if OpenSwapFile < ccError then
      begin
        FHeaders.Write(FTempWriter);
        Encoder := THeaderEncoder.Create(FTempWriter);
        Encoder.Password := FCommandLine.pOption;
        Encoder.OnUserAbortEvent := DoUserAbort;

        Check := True;
        for I := 0 to FHeaders.Count - 1 do
          if Code < ccError then
          begin
            P := FHeaders.Items[I];
            Encoder.Initialize(P);

            case P.Action of
              haNone: begin
                DoMessage(Format(cmCopying, [P.Name]));
                Check := Encoder.CopyFrom(P, FArchReader);
              end;
              haUpdate: begin
                DoMessage(Format(cmUpdating, [P.Name]));
                Check := Encoder.Write(P);
              end;
              haDecode: begin
                DoMessage(Format(cmEncoding, [P.Name]));
                Check := Encoder.WriteFrom(P, FSwapReader);
              end;
              haDecodeAndUpdate: begin
                DoMessage(Format(cmUpdating, [P.Name]));
                Check := Encoder.Write(P);
              end;
            end;
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
            if Check = False then
              DoMessage(cmStrmReadError, ccError);
          end;
        Encoder.Destroy;
        FHeaders.Write(FTempWriter);
      end;
    end else
      DoMessage(cmOpenTempError, ccError);
  end;
  CloseArchive(FSize > 0);
end;

procedure TBeeApp.DecodeShell;
var
  I: longint;
  P: THeader;
  Check: boolean;
  Decoder: THeaderDecoder;
begin
  if (OpenArchive < ccError) and (SetItemsToDecode > 0) then
  begin
    Decoder := THeaderDecoder.Create(FArchReader);
    Decoder.Password := FCommandLine.pOption;
    Decoder.OnUserAbortEvent := DoUserAbort;

    Check := True;
    for I := 0  to FHeaders.Count - 1 do
      if Code < ccError then
      begin
        P := FHeaders.Items[I];
        Decoder.Initialize(P);

        if P.Action in [haUpdate, haDecode] then
        begin
          case P.Action of
            // haNone:            nothing to do
            // haDecodeAndUpdate: nothing to do
            haUpdate: begin
              DoMessage(Format(cmExtracting, [P.Name]));
              Check := Decoder.Read(P);
            end;
            haDecode: begin
              DoMessage(Format(cmDecoding, [P.Name]));
              Check := Decoder.ReadToNul(P);
            end;
          end;
          {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
          if Check = False then
            DoMessage(Format(cmCrcError, [P.Name]), ccError);
        end;
      end;
    Decoder.Destroy;
  end;
  CloseArchive(False);
end;

procedure TBeeApp.TestShell;
var
  I: longint;
  P: THeader;
  Check: boolean;
  Decoder: THeaderDecoder;
begin
  if (OpenArchive < ccError) and (SetItemsToDecode > 0) then
  begin
    Decoder := THeaderDecoder.Create(FArchReader);
    Decoder.Password := FCommandLine.pOption;
    Decoder.OnUserAbortEvent := DoUserAbort;

    Check := True;
    for I := 0  to FHeaders.Count - 1 do
      if Code < ccError then
      begin
        P := FHeaders.Items[I];
        Decoder.Initialize(P);

        if P.Action in [haUpdate, haDecode] then
        begin
          case P.Action of
            // haNone:            nothing to do
            // haDecodeAndUpdate: nothing to do
            haUpdate: begin
              DoMessage(Format(cmTesting, [P.Name]));
              Check := Decoder.ReadToNul(P);
            end;
            haDecode: begin
              DoMessage(Format(cmDecoding, [P.Name]));
              Check := Decoder.ReadToNul(P);
            end;
          end;
          {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
          if Check = False then
            DoMessage(Format(cmCrcError, [P.Name]), ccError);
        end;
      end;
    Decoder.Destroy;
  end;
  CloseArchive(False);
end;

procedure TBeeApp.DeleteShell;
var
  I: longint;
  P: THeader;
  Check: boolean;
  Encoder: THeaderEncoder;
begin
  if (OpenArchive < ccError) and (SetItemsToDelete > 0) then
  begin
    FTempName   := GenerateFileName(FCommandLine.wdOption);
    FTempWriter := CreateTFileWriter(FTempName, fmCreate);
    if Assigned(FTempWriter) then
    begin
      if OpenSwapFile < ccError then
      begin
        // delete items ...
        for I := FHeaders.Count - 1 downto 0 do
        begin
          P := FHeaders.Items[I];
          if P.Action in [haUpdate, haDecodeAndUpdate] then
          begin
            DoMessage(Format(cmDeleting, [P.Name]));
            FHeaders.Delete(I);
          end;
        end;

        FHeaders.Write(FTempWriter);
        Encoder := THeaderEncoder.Create(FTempWriter);
        Encoder.Password := FCommandLine.pOption;
        Encoder.OnUserAbortEvent := DoUserAbort;

        Check := True;
        for I := 0 to FHeaders.Count - 1 do
          if Code < ccError then
          begin
            P := FHeaders.Items[I];
            Encoder.Initialize(P);

            case P.Action of
              haNone: begin
                DoMessage(Format(cmCopying, [P.Name]));
                Check := Encoder.CopyFrom(P, FArchReader);
              end;
              haUpdate: DoMessage(Format(cmDeleting, [P.Name]));
              haDecode: begin
                DoMessage(Format(cmEncoding, [P.Name]));
                Check := Encoder.WriteFrom(P, FSwapReader);
              end;
              haDecodeAndUpdate: DoMessage(Format(cmDeleting, [P.Name]));
            end;
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
            if Check = False then
              DoMessage(cmStrmReadError, ccError);
          end;
        Encoder.Destroy;
        FHeaders.Write(FTempWriter);
      end;
    end else
      DoMessage(cmOpenTempError, ccError);
  end;
  CloseArchive(FSize > 0);
end;

procedure TBeeApp.RenameShell;
var
  I: longint;
  P: THeader;
  Check: boolean;
  Encoder: THeaderEncoder;
begin
  if (OpenArchive < ccError) and (SetItemsToRename > 0) then
  begin
    FTempName   := GenerateFileName(FCommandLine.wdOption);
    FTempWriter := CreateTFileWriter(FTempName, fmCreate);
    if Assigned(FTempWriter) then
    begin
      FHeaders.Write(FTempWriter);
      Encoder := THeaderEncoder.Create(FTempWriter);
      Encoder.Password := FCommandLine.pOption;
      Encoder.OnUserAbortEvent := DoUserAbort;

      Check := True;
      for I := 0 to FHeaders.Count - 1 do
        if Code < ccError then
        begin
          P := FHeaders.Items[I];

          case P.Action of
            // haDecode:          nothing to do
            // haDecodeAndUpdate: nothing to do
            haNone: begin
              DoMessage(Format(cmCopying, [P.Name]));
              Check := Encoder.CopyFrom(P, FArchReader);
            end;
            haUpdate: begin
              DoMessage(Format(cmRenaming, [P.Name]));
              Check := Encoder.CopyFrom(P, FArchReader);
            end;
          end;
          {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
          if Check = False then
            DoMessage(cmStrmReadError, ccError);
        end;
      Encoder.Destroy;
      FHeaders.Write(FTempWriter);
    end else
      DoMessage(cmOpenTempError, ccError);
  end;
  CloseArchive(FSize > 0);
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
  if (OpenArchive < ccError) and (SetItemsToList > 0) then
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

        if P.Action = haUpdate then
        begin
          FHeadersToList.Add(P);
        end;
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

    if FCommandLine.slsOption then FHeadersToList.Sort(CompareFilePath);

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
  CloseArchive(False);
end;
{$ELSE}
procedure TBeeApp.ListShell;
var
  I: longint;
  P: THeader;
  Version, Method, Dictionary: longint;
begin
  if (OpenArchive < ccError) and (SetItemsToList > 0) then
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

        if P.Action = haUpdate then
        begin
          DoList(P);
        end;
      end;
  end;
  CloseArchive(False);
end;
{$ENDIF}

end.

