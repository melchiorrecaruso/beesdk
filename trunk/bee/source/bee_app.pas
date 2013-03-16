{
  Copyright (c) 2003-2013 Andrew Filinsky and Melchiorre Caruso.

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

    v0.8.0 build 1864 - 2013.02.15 by Melchiorre Caruso.
}

unit Bee_App;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Files,
  Bee_Common,
  Bee_CommandLine,
  BeeSDK_Archive;

type
  { TBeeApp class }

  TBeeApp = class(TObject)
  private
    FSelfName: string;
    FArchiver: TArchiver;
    FCommandLine: TCommandLine;
    FUpdateMethod: TUpdateMethod;
    FAssumeYesOnAllQueries: boolean;
    { Help routines}
    function QueryToUser(const Message: string;
      var Confirm: TArchiveConfirm): boolean;
    { Events routines}
    procedure DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
    procedure DoRequestImage(ImageNumber: longint;
      var ImageName: string; var Abort: boolean);
    procedure DoMessage(const Message: string);
    procedure DoProgress(Percentage: longint);
    procedure DoExtract(Item: TArchiveItem;
      var ExtractAs: string; var Confirm: TArchiveConfirm);
    procedure DoRename(Item: TArchiveItem;
      var RenameAs: string; var Confirm: TArchiveConfirm);
    procedure DoDelete(Item: TArchiveItem; var Confirm: TArchiveConfirm);
    procedure DoUpdate(SearchRec: TCustomSearchRec;
      var UpdateAs: string; var Confirm: TArchiveConfirm);
    { Open/close routines}
    procedure OpenArchive;
    procedure CloseArchive;
    { shells routines}
    procedure EncodeShell;
    procedure DecodeShell(TestMode: boolean);
    procedure DeleteShell;
    procedure HelpShell;
    procedure ListShell;
    procedure RenameShell;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    procedure Terminate;
  end;

implementation

uses
  Math,
  SysUtils,
  Bee_Interface;

{ help functions }

function CompareFilePath(P1, P2: pointer): longint;
begin
  Result := AnsiCompareFileName(
    ExtractFilePath(TArchiveItem(P1).FileName),
    ExtractFilePath(TArchiveItem(P2).FileName));

  if Result = 0 then
  begin
    Result := CompareText(
      ExtractFileName(TArchiveItem(P1).FileName),
      ExtractFileName(TArchiveItem(P2).FileName));
  end;
end;

{ TBeeApp class }

constructor TBeeApp.Create;
begin
  inherited Create;
  FSelfName := 'The Bee 0.8.0 build 1895 archiver utility, Mar 2013' + Cr +
               '(C) 1999-2013 Andrew Filinsky and Melchiorre Caruso';
  { set archiver events }
  FArchiver := TArchiver.Create;
  FArchiver.OnRequestBlankImage := DoRequestBlankDisk;
  FArchiver.OnRequestImage      := DoRequestImage;
  FArchiver.OnProgress          := DoProgress;
  FArchiver.OnMessage           := DoMessage;
  FArchiver.OnExtract           := DoExtract;
  FArchiver.OnRename            := DoRename;
  FArchiver.OnDelete            := DoDelete;
  FArchiver.OnUpdate            := DoUpdate;
  { load command line }
  FCommandLine := TCommandLine.Create;
  FCommandLine.Execute;
  { set update method }
  FUpdateMethod := umAddUpdate;
  if cluOption in FCommandLine.Options then
    FUpdateMethod := FCommandLine.uOption;
  { set if assume yes on all queries }
  FAssumeYesOnAllQueries := FALSE;
  if clyOption in FCommandLine.Options then
    FAssumeYesOnAllQueries := FCommandLine.yOption;
  { set thread priority }
  if clppOption in FCommandLine.Options then
    SetPriority(Ord(FCommandLine.ppOption));
end;

destructor TBeeApp.Destroy;
begin
  FCommandLine.Destroy;
  FArchiver.Destroy;
  inherited Destroy;
end;

procedure TBeeApp.Execute;
var
  StartTime: double;
begin
  StartTime := Now;
  DoMessage(FSelfName);
  if ExitStatus = esNoError then
    case FCommandLine.Command of
      cAdd     : EncodeShell;
      cDelete  : DeleteShell;
      cExtract : DecodeShell(FALSE);
      cHelp    : HelpShell;
      cList    : ListShell;
      cRename  : RenameShell;
      cTest    : DecodeShell(TRUE);
      cxExtract: DecodeShell(FALSE);
    end;

  if FCommandLine.Command <> cHelp then
    case ExitStatus of
      esNoError          : DoMessage(Cr + Format(emNoError,           [TimeDifference(StartTime)]));
      esUnknowError      : DoMessage(Cr + Format(emUnknowError,       [TimeDifference(StartTime)]));
      esCmdLineError     : DoMessage(Cr + Format(emCmdLineError,      [TimeDifference(StartTime)]));
      esAllocMemError    : DoMessage(Cr + Format(emAllocMemError,     [TimeDifference(StartTime)]));

      esCreateStreamError: DoMessage(Cr + Format(emCreateStreamError, [TimeDifference(StartTime)]));
      esOpenStreamError  : DoMessage(Cr + Format(emOpenStreamError,   [TimeDifference(StartTime)]));
      esFillStreamError  : DoMessage(Cr + Format(emFillStreamError,   [TimeDifference(StartTime)]));
      esFlushStreamError : DoMessage(Cr + Format(emFlushStreamError,  [TimeDifference(StartTime)]));
      esResizeStreamError: DoMessage(Cr + Format(emResizeStreamError, [TimeDifference(StartTime)]));
      esSplitStreamError : DoMessage(Cr + Format(emSplitStreamError,  [TimeDifference(StartTime)]));
      esRenameTempError  : DoMessage(Cr + Format(emRenameTempError,   [TimeDifference(StartTime)]));
      esBlowFishError    : DoMessage(Cr + Format(emBlowFishError,     [TimeDifference(StartTime)]));

      esUserAbortError   : DoMessage(Cr + Format(emUserAbortError,    [TimeDifference(StartTime)]));

      esArchiveTypeError : DoMessage(Cr + Format(emArchiveTypeError,  [TimeDifference(StartTime)]));
      esArchiveVerError  : DoMessage(Cr + Format(emArchiveVerError,   [TimeDifference(StartTime)]));
      esArchiveCDError   : DoMessage(Cr + Format(emArchiveCDError,    [TimeDifference(StartTime)]));
      esCRCError         : DoMessage(Cr + Format(emCRCError,          [TimeDifference(StartTime)]));
      esCaseError        : DoMessage(Cr + Format(emCaseError,         [TimeDifference(StartTime)]));
      esLoadConfigError  : DoMessage(Cr + Format(emLoadConfigError,   [TimeDifference(StartTime)]));
      else                 DoMessage(Cr + Format(emUnknowError,       [TimeDifference(StartTime)]));
    end;
end;

procedure TBeeApp.Terminate;
begin
  FArchiver.Terminate;
end;

procedure TBeeApp.DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
var
  Answer: string;
begin
  Write(#8#8#8#8#8#8, ParamToOem('Insert blank disk number #'
    + IntToStr(DiskNumber) + '. Continue? '));

  repeat
    Readln(Answer);
    Answer := UpperCase(OemToParam(Answer));
    if Length(Answer) = 1 then
      if Pos(Answer, 'YNQ') > -1 then Break;
    Write(ParamToOem('Yes, No or Quit? '));
  until TRUE;

  if Answer = 'Y' then
    Abort := FALSE
  else
    Abort := TRUE;
end;

procedure TBeeApp.DoRequestImage(ImageNumber: longint;
  var ImageName: string; var Abort: boolean);
var
  Answer: string;
begin
  Write(#8#8#8#8#8#8, ParamToOem('Insert disk number #'
    + IntToStr(ImageNumber) + '. Continue? '));

  repeat
    Readln(Answer);
    Answer := UpperCase(OemToParam(Answer));
    if Length(Answer) = 1 then
      if Pos(Answer, 'YNQ') > -1 then Break;
    Write(ParamToOem('Yes, No or Quit? '));
  until TRUE;

  if Answer = 'Y' then
    Abort := FALSE
  else
    Abort := TRUE;
end;

procedure TBeeApp.DoMessage(const Message: string);
begin
  Writeln(#13, #13: 8, ParamToOem(Message));
end;

procedure TBeeApp.DoProgress(Percentage: longint);
begin
  Write(#8#8#8#8#8#8, ParamToOem(Format('(%3d%%)', [Percentage])));
end;

procedure TBeeApp.DoExtract(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TArchiveConfirm);
var
  B: boolean;
  StartName: string;
  StartIndex: longint;
begin
  case FCommandLine.Command of
    cExtract : ExtractAs := ExtractFileName(                       Item.FileName);
    cXextract: ExtractAs := DeleteFilePath (FCommandLine.cdOption, Item.FileName);
  end;

  Confirm := arcCancel;
  case FUpdateMethod of
    umAdd          : if FileExists(ExtractAs) = FALSE then Confirm := arcOk;
    umReplace      : if FileExists(ExtractAs) = TRUE  then Confirm := arcOk;
    umUpdate       : if FileExists(ExtractAs) = TRUE  then
                       if Item.LastModifiedTime > FileAge(ExtractAs) then
                         Confirm := arcOk;

    umAddUpdate    : if FileExists(ExtractAs) = FALSE then
                       Confirm := arcOk
                     else
                       if Item.LastModifiedTime > FileAge(ExtractAs) then
                         Confirm := arcOk;

    umAddReplace   : Confirm := arcOk;
    umAddAutoRename: begin
      StartIndex := 0;
      StartName  := ExtractAs;
      while FileExists(ExtractAs) = TRUE do
        ExtractAs := GenerateAlternativeFileName(StartName, StartIndex);
      Confirm := arcOk;
    end;
    umQuery        : begin
      if FileExists(ExtractAs) = TRUE then
        B := QueryToUser('Overwrite "' + ExtractAs + '"? ', Confirm)
      else
        B := QueryToUser('Extract "'   + ExtractAs + '"? ', Confirm);

      if B = FALSE then
        DoExtract(Item, ExtractAs, Confirm);
    end;
    umAddQuery     : begin
      if FileExists(ExtractAs) = TRUE then
      begin
        B := QueryToUser('Overwrite "' + ExtractAs + '"? ', Confirm);

        if B = FALSE then
          DoExtract(Item, ExtractAs, Confirm);
      end else
        Confirm := arcOk;
    end;
  end;
end;

procedure TBeeApp.DoRename(Item: TArchiveItem;
  var RenameAs: string; var Confirm: TArchiveConfirm);
begin


  //BUG:0002    FileNameIsValid  &  FileNameFind

  Write(#8#8#8#8#8#8, ParamToOem('Rename file "'
    + RenameAs + '" as (empty to skip): '));

  Readln(RenameAs);
  // convert oem to param
  RenameAs := OemToParam(RenameAs);
end;

procedure TBeeApp.DoDelete(Item: TArchiveItem;
  var Confirm: TArchiveConfirm);
begin
  Confirm := arcOk;
end;

procedure TBeeApp.DoUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs: string; var Confirm: TArchiveConfirm);
var
  B: boolean;
  StartIndex: longint;
  StartName: string;
  Item: TArchiveItem;
  I: longint;
begin
  UpdateAs := FCommandLine.cdOption + SearchRec.Name;
  I := FArchiver.IndexOf(UpdateAs);
  if I <> -1 then
    Item := FArchiver.Items[I];

  Confirm := arcCancel;
  case FUpdateMethod of
    umAdd          : if I =  -1 then Confirm := arcOk;
    umReplace      : if I <> -1 then Confirm := arcOk;
    umUpdate       : if I <> -1 then
                       if SearchRec.LastModifiedTime > Item.LastModifiedTime then
                         Confirm := arcOk;

    umAddUpdate    : if I =  -1 then
                       Confirm := arcOk
                     else
                       if SearchRec.LastModifiedTime > Item.LastModifiedTime then
                         Confirm := arcOk;

    umAddReplace   : Confirm := arcOk;
    umAddAutoRename: begin
      StartIndex := 0;
      StartName  := UpdateAs;
      while FArchiver.IndexOf(UpdateAs) <> -1 do
        UpdateAs := GenerateAlternativeFileName(StartName, StartIndex);
      Confirm := arcOk;
    end;
    umAddQuery     : begin
      if I <> - 1 then
      begin
        B := QueryToUser('Overwrite "' + Item.FileName + '"? ', Confirm);

        if B = FALSE then
          DoUpdate(SearchRec, UpdateAs, Confirm);
      end else
        Confirm := arcOk;
    end;
    umQuery        : begin
      if I <> - 1 then
        B := QueryToUser('Overwrite "' + Item.FileName  + '"? ', Confirm)
      else
        B := QueryToUser('Add "'       + SearchRec.Name + '"? ', Confirm);

      if B = FALSE then
        DoUpdate(SearchRec, UpdateAs, Confirm);
    end;
  end;
end;

function TBeeApp.QueryToUser(const Message: string;
  var Confirm: TArchiveConfirm): boolean;
var
  Answer: string;
begin
  Result := TRUE;
  if FAssumeYesOnAllQueries = FALSE then
  begin
    Write(#8#8#8#8#8#8, ParamToOem(Message));
    repeat
      Readln(Answer);
      Answer := UpperCase(OemToParam(Answer));
      if Length(Answer) = 1 then
        if Pos(Answer, 'YNQA') > -1 then
        begin
          case Answer[1] of
            'Y': Confirm := arcOk;
            'N': Confirm := arcCancel;
            'Q': Confirm := arcQuit;
          end;
          Break;
        end;

      if GetUpdateMethod(Answer) <> -1 then
      begin
        FUpdateMethod := TUpdateMethod(GetUpdateMethod(Answer));
        Result := FALSE;
        Break;
      end;
      Write(#8#8#8#8#8#8, ParamToOem('Yes, No, or Quit? '));
    until TRUE;
  end else
    Confirm := arcOk;
end;

// --- //

procedure TBeeApp.OpenArchive;
begin
  FArchiver.OpenArchive(FCommandLine.ArchiveName);
  // work directory
  if clwdOption in FCommandLine.Options then
    FArchiver.WorkDirectory := FCommandLine.wdOption;
  // compression mode
  if clcpOption in FCommandLine.Options then
    FArchiver.CompressionParams := FCommandLine.cpOption;
  // encryption mode
  if clepOption in FCommandLine.Options then
    FArchiver.EncryptionParams := FCommandLine.epOption;
  // self extractor
  if clsfxOption in FCommandLine.Options then
    FArchiver.SelfExtractor := FCommandLine.sfxOption;
  // archive comment
  if clcOption in FCommandLine.Options then
    FArchiver.Comment := FCommandLine.acOption;
  // test temporary archive
  if cltOption in FCommandLine.Options then
    FArchiver.TestTempArchive := FCommandLine.tOption;
  // volume size
  if clvOption in FCommandLine.Options then
    FArchiver.VolumeSize := FCommandLine.vOption;
end;

procedure TBeeApp.CloseArchive;
begin
  FArchiver.CloseArchive;
end;

{ shell procedures }

procedure TBeeApp.HelpShell;
begin
  DoMessage(Cr + 'Usage: Bee <command> [<switches>...] <archive-name> [<file-names>...]');
  DoMessage(Cr + '<Commands>');
  DoMessage('  a: add files to archive');
  DoMessage('  d: delete files from archive');
  DoMessage('  e: extract files from archive');
  DoMessage('  h: show this help');
  DoMessage('  l: list contents of archive');
  DoMessage('  r: rename files in archive');
  DoMessage('  t: test integrity of archive files');
  DoMessage('  x: extract files from archive with path name');
  DoMessage('<Switches>');
  DoMessage('  -c{comment}: set archive comment');
  DoMessage('  -cd{path}: set current archive directory');
  DoMessage('  -cp{parameters}: set compression parameters');
  DoMessage('  -ep{parameters}: set encryption parameters');
  DoMessage('  -pp{parameters}: set process Priority ');
  DoMessage('  -r[-|w]: recurse subdirectories');
  DoMessage('  -sfx[{sfx-name}]: create self-extracting archive');
  DoMessage('  -sls: show list sorted by filename - for l (list) command');
  DoMessage('  -ss: stop switches parsing');
  DoMessage('  -t: Test temorary archive after process');
  DoMessage('  -u{parameters}: update files method');
  DoMessage('  -v{size}[b|k|m|g]: create volumes ');
  DoMessage('  -wd[{path}]: set temporany work directory');
  DoMessage('  -x{names}: eXclude filenames');
  DoMessage('  -y: assume yes on all queries');
  DoMessage(Cr + 'Use BeeOpt to make most optimal parameters.');
end;

procedure TBeeApp.EncodeShell;
var
  Scanner: TFileScanner;
  I: longint;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    Scanner := TFileScanner.Create;
    for I := 0 to FCommandLine.FileMasks.Count - 1 do
      Scanner.Scan(
        FCommandLine.FileMasks[I],
        FCommandLine.xOptions,
        FCommandLine.rOption);

    for I := 0 to Scanner.Count - 1 do
      FArchiver.Tag(Scanner.Items[I]);
    FreeAndNil(Scanner);

    FArchiver.UpdateTagged;
  end;
  CloseArchive;
end;

procedure TBeeApp.DecodeShell(TestMode: boolean);
var
  I: longint;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    for I := 0 to FArchiver.Count - 1 do
      if FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.FileMasks, FCommandLine.rOption) then FArchiver.Tag(I);

    for I := 0 to FArchiver.Count - 1 do
      if FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.xOptions, FCommandLine.rOption) then FArchiver.UnTag(I);

    case TestMode of
      FALSE: FArchiver.ExtractTagged;
      TRUE:  FArchiver.TestTagged;
    end;
  end;
  CloseArchive;
end;

procedure TBeeApp.DeleteShell;
var
  I: longint;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    for I := 0 to FArchiver.Count - 1 do
      if FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.FileMasks, FCommandLine.rOption) then FArchiver.Tag(I);

    for I := 0 to FArchiver.Count - 1 do
      if  FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.xOptions, FCommandLine.rOption) then FArchiver.UnTag(I);

    FArchiver.DeleteTagged;
  end;
  CloseArchive;
end;

procedure TBeeApp.RenameShell;
var
  I: longint;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    for I := 0 to FArchiver.Count - 1 do
      if FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.FileMasks, FCommandLine.rOption) then FArchiver.Tag(I);

    for I := 0 to FArchiver.Count - 1 do
      if  FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.xOptions, FCommandLine.rOption) then FArchiver.UnTag(I);

    FArchiver.RenameTagged;
  end;
  CloseArchive;
end;

procedure TBeeApp.ListShell;
var
  I: longint;
  Item: TArchiveItem;
  ItemToList: TList;

  TotalSize: int64;
  TotalPackedSize: int64;
  TotalFiles:longint;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    for I := 0 to FArchiver.Count - 1 do
      if FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.FileMasks, FCommandLine.rOption) then FArchiver.Tag(I);

    for I := 0 to FArchiver.Count - 1 do
      if  FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.xOptions, FCommandLine.rOption) then FArchiver.UnTag(I);

    TotalSize       := 0;
    TotalPackedSize := 0;
    TotalFiles      := 0;

    ItemToList := TList.Create;
    for I := 0 to FArchiver.Count - 1 do
      if FArchiver.IsTagged(I) then
      begin
        Item := FArchiver.Items[I];
        ItemToList.Add(Item);

        Inc(TotalSize, Item.UncompressedSize);
        Inc(TotalPackedSize, Item.CompressedSize);
        Inc(TotalFiles);
      end;

    if ItemToList.Count > 0 then
    begin
      if FCommandLine.slsOption then
        ItemToList.Sort(CompareFilePath);

      DoMessage(Cr + '   Date      Time     Attr          Size       Packed MTD Name                 ');
      DoMessage(     '---------- -------- ------- ------------ ------------ --- ---------------------');
      for I := 0 to ItemToList.Count - 1 do
      begin
        Item := ItemToList.Items[I];
        DoMessage(Format('%16s %7s %12s %12s %3s %s', [
          FileTimeToString(Item.LastModifiedTime), AttrToStr(Item.Attributes),
          SizeToStr(Item.UncompressedSize), SizeToStr(Item.CompressedSize),
          CompressionMethodToStr(Item), Item.FileName]));
      end;
      DoMessage('---------- -------- ------- ------------ ------------ --- ---------------------');
      DoMessage(StringOfChar(' ', 27) + Format(' %12s %12s     %d file(s)', [SizeToStr(TotalSize),
        SizeToStr(TotalPackedSize), TotalFiles]));
    end;
    ItemToList.Destroy;
  end;
  CloseArchive;
end;

end.

