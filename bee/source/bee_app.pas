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
  Bee_Common,
  Bee_CommandLine,
  BeeSDK_Archive;

type
  { TBeeApp class }

  TBeeApp = class(TObject)
  private
    FSelfName: string;
    FCommandLine: TCommandLine;
    FExtractor: TArchiveExtractor;
    FUpdater: TArchiveUpdater;
    FRenamer: TArchiveRenamer;
    FEraser: TArchiveEraser;
    FReader: TCustomArchiveReader;
    function QueryToUser(var Confirm: TArchiveConfirm): char;
    procedure DoRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
    procedure DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
    procedure DoMessage(const Message: string);
    procedure DoProgress(Percentage: longint);
    procedure DoExtract(Item: TArchiveItem; var ExtractAs: string; var Confirm: TArchiveConfirm);
    procedure DoUpdate(SearchRec: TCustomSearchRec; var UpdateAs: string; var Confirm: TArchiveConfirm);
    procedure DoRename(Item: TArchiveItem; var RenameAs: string; var Confirm: TArchiveConfirm);
    procedure DoErase(Item: TArchiveItem; var Confirm: TArchiveConfirm);
    procedure DoClear;
    { shells routines}
    procedure HelpShell;
    procedure EncodeShell;
    procedure DeleteShell;
    procedure DecodeShell(TestMode: boolean);
    procedure RenameShell;
    procedure ListShell;
  public
    constructor Create(const aCommandLine: string);
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

constructor TBeeApp.Create(const aCommandLine: string);
begin
  inherited Create;
  FExtractor := nil;
  FUpdater   := nil;
  FRenamer   := nil;
  FEraser    := nil;
  FReader    := nil;
  FSelfName  := 'The Bee 0.8.0 build 1611 archiver utility, July 2012' + Cr +
                '(C) 1999-2013 Andrew Filinsky and Melchiorre Caruso';

  ErrorCode := ecSuccesful;
  { store command line }
  FCommandLine := TCommandLine.Create;
  FCommandLine.CommandLine := aCommandLine;
  { set thread priority }
  SetPriority(Ord(FCommandLine.priOption));
end;

destructor TBeeApp.Destroy;
begin
  FCommandLine.Destroy;
  inherited Destroy;
end;

procedure TBeeApp.Terminate;
begin
  if Assigned(FExtractor) then FExtractor.Terminate;
  if Assigned(FUpdater)   then FUpdater.Terminate;
  if Assigned(FRenamer)   then FRenamer.Terminate;
  if Assigned(FEraser)    then FEraser.Terminate;
  if Assigned(FReader)    then FReader.Terminate;
end;

procedure TBeeApp.Execute;
var
  S: string;
  StartTime: double;
begin
  StartTime := Now;
  DoMessage(FSelfName);
  if ErrorCode < ecError then
    case FCommandLine.Command of
      ccAdd:      EncodeShell;
      ccDelete:   DeleteShell;
      ccExtract:  DecodeShell(FALSE);
      ccxExtract: DecodeShell(FALSE);
      ccTest:     DecodeShell(TRUE);
      ccRename:   RenameShell;
      ccList:     ListShell;
      ccHelp:     HelpShell;
    end;

  if (FCommandLine.Command in [ccList, ccHelp]) = FALSE then
  begin
    S := TimeDifference(StartTime);
    case ErrorCode of
      ecSuccesful: DoMessage(Format(Cr + emSuccesful, [S]));
      ecWarning:   DoMessage(Format(Cr + emWarning,   [S]));
      ecUserAbort: DoMessage(Format(Cr + emUserAbort, [S]));
      ecCmdError:  DoMessage(Format(Cr + emCmdError,  [ ]));
      else         DoMessage(Format(Cr + emError,     [S]));
    end;
  end;
end;

procedure TBeeApp.DoRequestImage(ImageNumber: longint;
  var ImageName: string; var Abort: boolean);
begin
  Writeln(ParamToOem(ImageName));
  Readln;
end;

procedure TBeeApp.DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
var
  Ch: char;
begin
  Write(ParamToOem('Insert blank disk number #'+ IntToStr(DiskNumber)) + '. Continue? ');
  Readln(Ch);
  Ch := UpCase(OemToParam(Ch)[1]);
  while Pos(Ch, 'YQ') < 1 do
  begin
    Write('Yes, or Quit? ');
    Readln(Ch);
    Ch := UpCase(OemToParam(Ch)[1]);
  end;

  case Ch of
    'Y': Abort := FALSE;
    else Abort := TRUE;
  end;
end;

procedure TBeeApp.DoMessage(const Message: string);
begin
  Writeln(ParamToOem(Message));
end;

procedure TBeeApp.DoProgress(Percentage: longint);
begin
  Write(#8#8#8#8#8#8, Format('(%3d%%)', [Percentage]));
end;

procedure TBeeApp.DoExtract(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TArchiveConfirm);
begin
  case FCommandLine.Command of
    ccExtract:  ExtractAs := ExtractFileName(Item.FileName);
    ccXextract: ExtractAs := DeleteFilePath(FCommandLine.cdOption, Item.FileName);
  end;

  Confirm := arcCancel;
  case FCommandLine.uOption of
    umAdd:           if (not FileExists(ExtractAs)) then Confirm := arcOk;
    umReplace:       if (    FileExists(ExtractAs)) then Confirm := arcOk;
    umUpdate:        if (    FileExists(ExtractAs)) and (Item.LastModifiedTime > FileAge(ExtractAs)) then Confirm := arcOk;
    umAddUpdate:     if (not FileExists(ExtractAs)) or  (Item.LastModifiedTime > FileAge(ExtractAs)) then Confirm := arcOk;
    umAddReplace:    Confirm := arcOk;
    umAddAutoRename: begin
      if FileExists(ExtractAs) then
        ExtractAs := GenerateAlternativeFileName(ExtractAs, 1, True);
      Confirm := arcOk;
    end;
  end;
end;

function TBeeApp.QueryToUser(var Confirm: TArchiveConfirm): char;
begin
  Readln(Result);
  Result := UpCase(Result);
  while Pos(Result, 'YNQ01234567') < 1 do
  begin
    Write(ParamToOem('Yes, No, or Quit? '));
    Readln(Result);
    Result := UpCase(Result);
  end;

  Confirm := arcCancel;
  case Result of
    'Y': Confirm := arcOk;
    'N': Confirm := arcCancel;
    'Q': Confirm := arcQuit;
    '0': FCommandLine.uOption := umAdd;
    '1': FCommandLine.uOption := umUpdate;
    '2': FCommandLine.uOption := umReplace;
    '3': FCommandLine.uOption := umQuery;
    '4': FCommandLine.uOption := umAddUpdate;
    '5': FCommandLine.uOption := umAddReplace;
    '6': FCommandLine.uOption := umAddQuery;
    '7': FCommandLine.uOption := umAddAutoRename;
  end;
end;

procedure TBeeApp.DoUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs: string; var Confirm: TArchiveConfirm);
var
  I, Index: longint;
  Item: TArchiveItem;
  AlternativeFileName: string;
begin
  UpdateAs := FCommandLine.cdOption + SearchRec.Name;
  I := FUpdater.Find(UpdateAs);
  if I <> -1 then
    Item := FUpdater.Items[I];

  Confirm := arcCancel;
  case FCommandLine.uOption of
    umAdd:        if (I =  -1) then Confirm := arcOk;
    umReplace:    if (I <> -1) then Confirm := arcOk;
    umUpdate:     if (I <> -1) and (SearchRec.LastModifiedTime > Item.LastModifiedTime) then Confirm := arcOk;
    umAddUpdate:  if (I =  -1) or  (SearchRec.LastModifiedTime > Item.LastModifiedTime) then Confirm := arcOk;
    umAddReplace: Confirm := arcOk;
    umAddAutoRename: begin
      if (I <> - 1) then
      begin
        Index := 0;
        repeat
          AlternativeFileName := ChangeFileExt(UpdateAs, '.' + IntToStr(Index) + ExtractFileExt(UpdateAs));
          Inc(Index);
        until FUpdater.Find(AlternativeFileName) = -1;
        UpdateAs := AlternativeFileName;
      end;
      Confirm := arcOk;
    end;
    umAddQuery: begin
      if (I <> - 1) then
      begin
        Write(ParamToOem('Replace "' + Item.FileName + '" with "' + SearchRec.Name + '"? '));
        if Pos(QueryToUser(Confirm), '01234567') > 0 then
          DoUpdate(SearchRec, UpdateAs, Confirm);
      end else
        Confirm := arcOk;
    end;
    umQuery: begin
      if (I <> - 1) then
        Write(ParamToOem('Replace "' + Item.FileName + '" with "' + SearchRec.Name + '"? '))
      else
        Write(ParamToOem('Add "' + SearchRec.Name + '"? '));
      if Pos(QueryToUser(Confirm), '01234567') > 0 then
        DoUpdate(SearchRec, UpdateAs, Confirm);
    end;
  end;
end;

procedure TBeeApp.DoRename(Item: TArchiveItem;
  var RenameAs: string; var Confirm: TArchiveConfirm);
begin
  Write('Rename file "', ParamToOem(RenameAs), '" as (empty to skip):');
  Readln(RenameAs);
  // convert oem to param
  RenameAs := OemToParam(RenameAs);
end;

procedure TBeeApp.DoErase(Item: TArchiveItem;
  var Confirm: TArchiveConfirm);
begin
  Confirm :=arcOk;
end;

procedure TBeeApp.DoClear;
begin
  Write(#13, #13:80);
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
  DoMessage('  h  show command line Help');
  DoMessage('<Switches>');
  DoMessage('  --              stop switches parsing');
  DoMessage('  -cd[directory]  set current archive directory');
  DoMessage('  -cfg[filename]  use specified Configuration file');
  DoMessage('  -d<0..9>    set Dictionary size (d1 uses < 5M, d2 (default) < 10M, ...)');
  DoMessage('  -f          Force file extention');
  DoMessage('  -m<0..3>    set compression Method (0-store, 1-default, ...)');
  DoMessage('  -p          set Password (min length 4 bytes)');
  DoMessage('  -pri<0..3>  set process priority (0-idle, 1-normal, 2-high, 3-realtime)');
  DoMessage('  -r    Recurse subdirectories');
  DoMessage('  -rw   recurse subdirectories only for wildcard names');
  DoMessage('  -s    create Solid archive');
  DoMessage('  -sfx  add self-extractor module');
  DoMessage('  -sls  show list sorted by filename - for l (list) command');
  DoMessage('  -t    Test archive after process');
  DoMessage('  -u<0..7>  Update files method (0-add files, 1-update files, ...)');
  DoMessage('  -wd[direcroty]  set temporany work directory');
  DoMessage('  -x[filenames]   eXclude filenames');
  DoMessage(Cr + 'Use BeeOpt to make most optimal parameters.' + Cr);
end;

procedure TBeeApp.EncodeShell;
var
  I: longint;
  Scanner: TFileScanner;
begin
  FUpdater                    := TArchiveUpdater.Create;
  FUpdater.OnRequestBlankDisk := DoRequestBlankDisk;
  FUpdater.OnRequestImage     := DoRequestImage;
  FUpdater.OnProgress         := DoProgress;
  FUpdater.OnMessage          := DoMessage;
  FUpdater.OnFailure          := DoMessage;
  FUpdater.OnClear            := DoClear;
  FUpdater.OnUpdate           := DoUpdate;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FUpdater.OpenArchive(FCommandLine.ArchiveName);
  case FCommandLine.mOption of
    moStore: FUpdater.CompressionMethod := actNone;
    else     FUpdater.CompressionMethod := actMain;
  end;
  FUpdater.CompressionLevel   := FCommandLine.mOption;
  FUpdater.DictionaryLevel    := FCommandLine.dOption;
  FUpdater.SolidCompression   := FCommandLine.sOption;
  FUpdater.ForceFileExtension := FCommandLine.fOption;
  FUpdater.ArchivePassword    := FCommandLine.pOption;
  FUpdater.TestTempArchive    := FCommandLine.tOption;
  FUpdater.Threshold          := FCommandLine.iOption;
//FUpdater.ArchiveComment     :=

  if ErrorCode < ecError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    Scanner := TFileScanner.Create;
    with FCommandLine do
      for I := 0 to FileMasks.Count - 1 do
        Scanner.Scan(FileMasks[I], xOptions, rOption);

    for I := 0 to Scanner.Count - 1 do
      FUpdater.Tag(Scanner.Items[I]);
    Scanner.Free;

    FUpdater.UpdateTagged;
  end;
  FreeAndNil(FUpdater);
end;

procedure TBeeApp.DecodeShell(TestMode: boolean);
var
  I: longint;
begin
  FExtractor                 := TArchiveExtractor.Create;
  FExtractor.OnRequestImage  := DoRequestImage;
  FExtractor.OnProgress      := DoProgress;
  FExtractor.OnMessage       := DoMessage;
  FExtractor.OnFailure       := DoMessage;
  FExtractor.OnClear         := DoClear;
  FExtractor.OnExtraction    := DoExtract;

  FExtractor.ArchivePassword := FCommandLine.pOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FExtractor.OpenArchive(FCommandLine.ArchiveName);
  if ErrorCode < ecError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    for I := 0 to FExtractor.Count - 1 do
      if FileNameMatch(FExtractor.Items[I].FileName,
        FCommandLine.FileMasks, FCommandLine.rOption) then FExtractor.Tag(I);

    for I := 0 to FExtractor.Count - 1 do
      if  FileNameMatch(FExtractor.Items[I].FileName,
        FCommandLine.xOptions, FCommandLine.rOption) then FExtractor.UnTag(I);

    case TestMode of
      FALSE: FExtractor.ExtractTagged;
      TRUE:  FExtractor.TestTagged;
    end;
  end;
  FreeAndNil(FExtractor);
end;

procedure TBeeApp.DeleteShell;
var
  I: longint;
begin
  FEraser                    := TArchiveEraser.Create;
  FEraser.OnRequestBlankDisk := DoRequestBlankDisk;
  FEraser.OnRequestImage     := DoRequestImage;
  FEraser.OnProgress         := DoProgress;
  FEraser.OnMessage          := DoMessage;
  FEraser.OnFailure          := DoMessage;
  FEraser.OnClear            := DoClear;
  FEraser.OnEraseEvent       := DoErase;

  FEraser.TestTempArchive    := FCommandLine.tOption;
  FEraser.ArchivePassword    := FCommandLine.pOption;
  FEraser.Threshold          := FCommandLine.iOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FEraser.OpenArchive(FCommandLine.ArchiveName);
  if ErrorCode < ecError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    for I := 0 to FEraser.Count - 1 do
      if FileNameMatch(FEraser.Items[I].FileName,
        FCommandLine.FileMasks, FCommandLine.rOption) then FEraser.Tag(I);

    for I := 0 to FEraser.Count - 1 do
      if  FileNameMatch(FEraser.Items[I].FileName,
        FCommandLine.xOptions, FCommandLine.rOption) then FEraser.UnTag(I);

    FEraser.EraseTagged;
  end;
  FreeAndNil(FEraser);
end;

procedure TBeeApp.RenameShell;
var
  I: longint;
begin
  FRenamer                    := TArchiveRenamer.Create;
  FRenamer.OnRequestBlankDisk := DoRequestBlankDisk;
  FRenamer.OnRequestImage     := DoRequestImage;
  FRenamer.OnProgress         := DoProgress;
  FRenamer.OnMessage          := DoMessage;
  FRenamer.OnFailure          := DoMessage;
  FRenamer.OnClear            := DoClear;
  FRenamer.OnRenameEvent      := DoRename;

  FRenamer.TestTempArchive    := FCommandLine.tOption;
  FRenamer.ArchivePassword    := FCommandLine.pOption;
  FRenamer.Threshold          := FCommandLine.iOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FRenamer.OpenArchive(FCommandLine.ArchiveName);
  if ErrorCode < ecError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    for I := 0 to FRenamer.Count - 1 do
      if FileNameMatch(FRenamer.Items[I].FileName,
        FCommandLine.FileMasks, FCommandLine.rOption) then FRenamer.Tag(I);

    for I := 0 to FRenamer.Count - 1 do
      if  FileNameMatch(FRenamer.Items[I].FileName,
        FCommandLine.xOptions, FCommandLine.rOption) then FRenamer.UnTag(I);

    FRenamer.RenameTagged;
  end;
  FreeAndNil(FRenamer);
end;

procedure TBeeApp.ListShell;
var
  I: longint;
  Item: TArchiveItem;
  ItemToList: TList;

  VersionNeededToExtract: longword;
  CompressionMethod: TArchiveCompressionMethod;
  CompressionLevel: TmOption;
  DictionaryLevel: TdOption;
  MaxDictionaryLevel: TdOption;
  WithSolidCompression: longint;
  WithArchivePassword: longint;

  TotalSize: int64;
  TotalPackedSize: int64;
  TotalFiles:longint;
begin
  FReader                 := TCustomArchiveReader.Create;
  FReader.OnRequestImage  := DoRequestImage;
  FReader.OnProgress      := DoProgress;
  FReader.OnMessage       := DoMessage;
  FReader.OnFailure       := DoMessage;
  FReader.OnClear         := DoClear;

  FReader.ArchivePassword := FCommandLine.pOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FReader.OpenArchive(FCommandLine.ArchiveName);
  if ErrorCode < ecError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    for I := 0 to FReader.Count - 1 do
      if FileNameMatch(FReader.Items[I].FileName,
        FCommandLine.FileMasks, FCommandLine.rOption) then FReader.Tag(I);

    for I := 0 to FReader.Count - 1 do
      if  FileNameMatch(FReader.Items[I].FileName,
        FCommandLine.xOptions, FCommandLine.rOption) then FReader.UnTag(I);

    VersionNeededToExtract := 0;
    CompressionMethod      := actNone;
    CompressionLevel       := moStore;
    DictionaryLevel        := do2MB;
    WithSolidCompression   := 0;
    WithArchivePassword    := 0;
    MaxDictionaryLevel     := do2MB;

    TotalPackedSize := 0;
    TotalSize       := 0;
    TotalFiles      := 0;

    ItemToList := TList.Create;
    for I := 0 to FReader.Count - 1 do
    begin
      Item := FReader.Items[I];
      if FReader.IsTagged(I) then
        ItemToList.Add(Item);

      if aifVersionNeededToExtract in Item.Flags            then VersionNeededToExtract := Item.VersionNeededToExtract;
      if acfCompressionMethod      in Item.CompressionFlags then CompressionMethod      := Item.CompressionMethod;
      if acfCompressionLevel       in Item.CompressionFlags then CompressionLevel       := Item.CompressionLevel;
      if acfDictionaryLevel        in Item.CompressionFlags then DictionaryLevel        := Item.DictionaryLevel;
      if acfSolidCompression       in Item.CompressionFlags then Inc(WithSolidCompression);
      if aefEncryptionMethod       in Item.EncryptionFlags  then
        if Item.EncryptionMethod <> acrtNone then
          Inc(WithArchivePassword);

      if DictionaryLevel > MaxDictionaryLevel then
        MaxDictionaryLevel := DictionaryLevel;

      Inc(TotalPackedSize, Item.CompressedSize);
      Inc(TotalSize, Item.UncompressedSize);
      Inc(TotalFiles);
    end;

    DoMessage(Cr + 'Extraction requirements:');
    DoMessage('  Minimun version archive extractor = ' + VersionToStr(VersionNeededToExtract));
    DoMessage('  Minimun free memory size = ' + IntToStr(Round($280000*power(2, Ord(MaxDictionaryLevel)))));
    DoMessage('  Minimun free disk size = ' + IntToStr(TotalSize));

    DoMessage(Cr + 'Archive features:');
    DoMessage('  Items archived = ' + IntToStr(TotalFiles));
    DoMessage('  Items encrypted = ' + IntToStr(WithArchivePassword));
    DoMessage('  Items with solid compression = ' + IntToStr(WithSolidCompression));

    DoMessage(Cr + '  Self-extractor module size = ' + IntToStr(0));

    DoMessage(Cr + '  Archive images number = ' + IntToStr(FReader.ArchiveSpanning));
    DoMessage('  Archive packed data size = ' + IntToStr(TotalPackedSize));
    DoMessage('  Archive size = ' + SizeToStr(SizeOfFile(FCommandLine.ArchiveName)));

    if ItemToList.Count > 0 then
    begin
      DoMessage(Cr + '   Date      Time     Attr          Size       Packed MTD Name                 ');
      DoMessage(     '---------- -------- ------- ------------ ------------ --- ---------------------');

      if FCommandLine.slsOption then
        ItemToList.Sort(CompareFilePath);

      TotalPackedSize := 0;
      TotalSize       := 0;
      TotalFiles      := 0;

      for I := 0 to ItemToList.Count - 1 do
      begin
        Item := ItemToList.Items[I];
        Inc(TotalSize, Item.UncompressedSize);
        Inc(TotalPackedSize, Item.CompressedSize);
        Inc(TotalFiles);

        DoMessage(Format('%16s %7s %12s %12s %3s %s', [
          FileTimeToString(Item.LastModifiedTime), AttrToStr(Item.Attributes),
          SizeToStr(Item.UncompressedSize), SizeToStr(Item.CompressedSize),
          CompressionMethodToStr(Item), Item.FileName]));
      end;
      DoMessage('---------- -------- ------- ------------ ------------ --- ---------------------');
      DoMessage(StringOfChar(' ', 27) + Format(' %12s %12s     %d file(s)' + Cr, [SizeToStr(TotalSize), SizeToStr(TotalPackedSize), TotalFiles]));
    end else
    begin
      DoMessage(Cr + 'Nothing to list.' + Cr);
    end;
    ItemToList.Destroy;
  end;
  FreeAndNil(FReader);
end;

end.

