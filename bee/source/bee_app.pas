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
  // Crt,
  Classes,
  Bee_Files,
  Bee_Types,
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
    FReader: TArchiveReader;
    function CompressionMethodToStr(Item: TArchiveItem): string;
    procedure DoRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
    procedure DoRequestBlankDisk(var Abort : Boolean);
    procedure DoMessage(const Message: string);
    procedure DoProgress(Percentage: longint);
    procedure DoExtract(Item: TArchiveItem; var ExtractAs: string; var Confirm: TArchiveConfirm);
    procedure DoUpdate(SearchRec: TCustomSearchRec; var UpdateAs: string; var Confirm: TArchiveConfirm);
    procedure DoRename(Item: TArchiveItem; var RenameAs: string; var Confirm: TArchiveConfirm);
    procedure DoErase(Item: TArchiveItem; var Confirm: TArchiveConfirm);
    procedure DoClear;
    { process options }
    procedure ProcesstOption;
    procedure ProcesslOption;
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
  SysUtils,
  Bee_Consts;

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
  FSelfName := 'The Bee 0.8.0 build 1565 archiver utility, July 2012' + Cr +
               '(C) 1999-2013 Andrew Filinsky and Melchiorre Caruso';
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
  DoMessage(FSelfName);
  if (FCommandLine.Command <> ccNone) and
     (FCommandLine.ArchiveName <> '') then
  begin
    StartTime := Now;
    case FCommandLine.Command of
      ccAdd:      EncodeShell;
      ccDelete:   DeleteShell;
      ccExtract:  DecodeShell(FALSE);
      ccxExtract: DecodeShell(FALSE);
      ccTest:     DecodeShell(TRUE);
      ccRename:   RenameShell;
      ccList:     ListShell;
    end;

    S := TimeDifference(StartTime);
    case ExitCode of
      ccSuccesful: DoMessage(Format(Cr + cmSuccesful, [S]));
      ccWarning:   DoMessage(Format(Cr + cmWarning,   [S]));
      ccUserAbort: DoMessage(Format(Cr + cmUserAbort, [S]));
      else         DoMessage(Format(Cr + cmError,     [S]));
    end;
  end else
    HelpShell;
end;

procedure TBeeApp.DoRequestImage(ImageNumber: longint;
  var ImageName: string; var Abort: boolean);
begin
  Writeln(ParamToOem(ImageName));
  Readln;
end;

procedure TBeeApp.DoRequestBlankDisk(var Abort : Boolean);
begin
  Writeln(ParamToOem('Insert blank disk.'));
  Readln;
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

procedure TBeeApp.DoUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs: string; var Confirm: TArchiveConfirm);
var
  ch: char;
  I: longint;
  Item: TArchiveItem;
begin
  UpdateAs := FCommandLine.cdOption + SearchRec.Name;
  I := FUpdater.Find(UpdateAs);
  if I <> -1 then
    Item := FUpdater.Items[I];

  Confirm := arcCancel;
  case FCommandLine.uOption of
    umAdd:           if (I =  -1) then Confirm := arcOk;
    umReplace:       if (I <> -1) then Confirm := arcOk;
    umUpdate:        if (I <> -1) and (SearchRec.LastModifiedTime > Item.LastModifiedTime) then Confirm := arcOk;
    umAddUpdate:     if (I =  -1) or  (SearchRec.LastModifiedTime > Item.LastModifiedTime) then Confirm := arcOk;
    umAddReplace:    Confirm := arcOk;
    umAddAutoRename: begin
      while I <> -1 do
      begin
        UpdateAs := UpdateAs;
        I := FUpdater.Find(UpdateAs);
      end;
      Confirm := arcOk;
    end;
    umAddQuery: begin

       repeat
         Write('Replace "', Item.FileName ,'" with "', SearchRec.Name, '"?');
         Readln(ch);
         DoClear;
        // case Upcase(ch) of
        //   'Y':
        //   'N':
        //   'A':
        //   'Q':
        //   else ;
        // end

       until Upcase(ch) in [];

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

{ option processing }

procedure TBeeApp.ProcesstOption;
begin
  if (ExitCode < ccError) and FCommandLine.tOption then
  begin
    FCommandLine.xOptions.Clear;
    FCommandLine.FileMasks.Clear;
    FCommandLine.FileMasks.Add('*');
    FCommandLine.rOption := rmFull;
    DecodeShell(TRUE);
  end;
end;

procedure TBeeApp.ProcesslOption;
begin
  if (ExitCode < ccError) and FCommandLine.lOption then
  begin
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
  Scanner: TFileScanner;
begin
  FUpdater := TArchiveUpdater.Create;
  FUpdater.OnRequestBlankDisk := DoRequestBlankDisk;
  FUpdater.OnRequestImage     := DoRequestImage;
  FUpdater.OnFailure          := DoMessage;
  FUpdater.OnMessage          := DoMessage;
  FUpdater.OnProgress         := DoProgress;
  FUpdater.OnClearProgress    := DoClear;
  FUpdater.OnUpdate           := DoUpdate;

  case FCommandLine.mOption of
    moStore: FUpdater.CompressionMethod := actNone;
    else     FUpdater.CompressionMethod := actMain;
  end;
  FUpdater.CompressionLevel  := FCommandLine.mOption;
  FUpdater.DictionaryLevel   := FCommandLine.dOption;
  FUpdater.SolidCompression  := FCommandLine.sOption;
  FUpdater.ArchivePassword   := FCommandLine.pOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FUpdater.OpenArchive(FCommandLine.ArchiveName);
  DoMessage(Format(cmScanning, ['...']));
  Scanner := TFileScanner.Create;
  with FCommandLine do
    for I := 0 to FileMasks.Count - 1 do
      Scanner.Scan(FileMasks[I], xOptions, rOption);

  for I := 0 to Scanner.Count - 1 do
    FUpdater.Tag(Scanner.Items[I]);
  Scanner.Free;

  FUpdater.UpdateTagged;
  FUpdater.Destroy;
  ProcesstOption;
  ProcesslOption;
end;

procedure TBeeApp.DecodeShell(TestMode: boolean);
var
  I: longint;
begin
  FExtractor := TArchiveExtractor.Create;
  FExtractor.OnRequestImage := DoRequestImage;
  FExtractor.OnFailure      := DoMessage;
  FExtractor.OnMessage      := DoMessage;
  FExtractor.OnProgress     := DoProgress;
  FExtractor.OnClearProgress:= DoClear;
  FExtractor.OnExtraction   := DoExtract;

  FExtractor.ArchivePassword := FCommandLine.pOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FExtractor.OpenArchive(FCommandLine.ArchiveName);
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FExtractor.Count - 1 do
    if FileNameMatch(FExtractor.Items[I].FileName,
      FCommandLine.FileMasks, FCommandLine.rOption) then FExtractor.Tag(I);

  for I := 0 to FExtractor.Count - 1 do
    if  FileNameMatch(FExtractor.Items[I].FileName,
      FCommandLine.xOptions, FCommandLine.rOption) then FExtractor.UnTag(I);

  case TestMode of
    True:  FExtractor.TestTagged;
    False: FExtractor.ExtractTagged;
  end;
  FExtractor.Destroy;
end;

procedure TBeeApp.DeleteShell;
var
  I: longint;
begin
  FEraser := TArchiveEraser.Create;
  FEraser.OnRequestBlankDisk := DoRequestBlankDisk;
  FEraser.OnRequestImage     := DoRequestImage;
  FEraser.OnFailure          := DoMessage;
  FEraser.OnMessage          := DoMessage;
  FEraser.OnProgress         := DoProgress;
  FEraser.OnClearProgress    := DoClear;
  FEraser.OnEraseEvent       := DoErase;

  FEraser.ArchivePassword    :=  FCommandLine.pOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FEraser.OpenArchive(FCommandLine.ArchiveName);
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FEraser.Count - 1 do
    if FileNameMatch(FEraser.Items[I].FileName,
      FCommandLine.FileMasks, FCommandLine.rOption) then FEraser.Tag(I);

  for I := 0 to FEraser.Count - 1 do
    if  FileNameMatch(FEraser.Items[I].FileName,
      FCommandLine.xOptions, FCommandLine.rOption) then FEraser.UnTag(I);

  FEraser.EraseTagged;
  FEraser.Destroy;
  ProcesstOption;
  ProcesslOption;
end;

procedure TBeeApp.RenameShell;
var
  I: longint;
begin
  FRenamer := TArchiveRenamer.Create;
  FRenamer.OnRequestBlankDisk := DoRequestBlankDisk;
  FRenamer.OnRequestImage     := DoRequestImage;
  FRenamer.OnFailure          := DoMessage;
  FRenamer.OnMessage          := DoMessage;
  FRenamer.OnProgress         := DoProgress;
  FRenamer.OnClearProgress    := DoClear;
  FRenamer.OnRenameEvent      := DoRename;

  FRenamer.ArchivePassword    := FCommandLine.pOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FRenamer.OpenArchive(FCommandLine.ArchiveName);
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FRenamer.Count - 1 do
    if FileNameMatch(FRenamer.Items[I].FileName,
      FCommandLine.FileMasks, FCommandLine.rOption) then FRenamer.Tag(I);

  for I := 0 to FRenamer.Count - 1 do
    if  FileNameMatch(FRenamer.Items[I].FileName,
      FCommandLine.xOptions, FCommandLine.rOption) then FRenamer.UnTag(I);

  FRenamer.RenameTagged;
  FRenamer.Destroy;
  ProcesstOption;
  ProcesslOption;
end;

function TBeeApp.CompressionMethodToStr(Item: TArchiveItem): string;
begin
  case Item.CompressionMethod of
    actNone: Result := 'm0';
    actMain: Result := 'me';
  end;
end;

procedure TBeeApp.ListShell;
var
  I: longint;
  Item: TArchiveItem;
  FItemToList: TList;
  CompressionMethod: TArchiveCompressionMethod;
  CompressionLevel: TmOption;
  DictionaryLevel: TdOption;
  WithSolidCompression: longint;
  WithArchivePassword: longint;

  TotalSize: int64;
  TotalPackedSize: int64;
  TotalFiles:longint;
begin
  FReader := TArchiveReader.Create;
  FReader.OnRequestImage  := DoRequestImage;
  FReader.OnFailure       := DoMessage;
  FReader.OnMessage       := DoMessage;
  FReader.OnProgress      := DoProgress;
  FReader.OnClearProgress := DoClear;

  FReader.ArchivePassword := FCommandLine.pOption;

  DoMessage(Format(cmOpening, [FCommandLine.ArchiveName]));
  FReader.OpenArchive(FCommandLine.ArchiveName);
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FReader.Count - 1 do
    if FileNameMatch(FReader.Items[I].FileName,
      FCommandLine.FileMasks, FCommandLine.rOption) then FReader.Tag(I);

  for I := 0 to FReader.Count - 1 do
    if  FileNameMatch(FReader.Items[I].FileName,
      FCommandLine.xOptions, FCommandLine.rOption) then FReader.UnTag(I);

  CompressionMethod := actNone;
  CompressionLevel  := moStore;
  DictionaryLevel   := do2MB;
  WithSolidCompression  := 0;
  WithArchivePassword   := 0;

  TotalPackedSize   := 0;
  TotalSize         := 0;
  TotalFiles        := 0;

  FItemToList := TList.Create;
  for I := 0 to FReader.Count - 1 do
  begin
    Item := FReader.Items[I];

    if acfCompressionMethod in Item.CompressionFlags then CompressionMethod := Item.CompressionMethod;
    if acfCompressionLevel  in Item.CompressionFlags then CompressionLevel  := Item.CompressionLevel;
    if acfDictionaryLevel   in Item.CompressionFlags then DictionaryLevel   := Item.DictionaryLevel;
    if acfSolidCompression  in Item.CompressionFlags then Inc(WithSolidCompression);
    if aefEncryptionMethod  in Item.EncryptionFlags  then Inc(WithArchivePassword);

    Inc(TotalSize, Item.UncompressedSize);
    Inc(TotalPackedSize, Item.CompressedSize);
    Inc(TotalFiles);

    if FReader.IsTagged(I) then
    begin
      FItemToList.Add(Item);
    end;
  end;

  DoMessage(Cr + 'Extraction requirements:');
  // DoMessage('  Headers version  = ' + VersionToStr(Version));
  // DoMessage('  Free memory size = ' + IntToStr(Trunc(Power(2, MaxDict)*2560000)));
  DoMessage('  Free disk space  = ' + IntToStr(TotalSize));

  DoMessage(Cr + 'Archive features:');
  case WithArchivePassword of
    0:   DoMessage('  Password  = no' );
    else DoMessage('  Password  = yes');
  end;

  case WithSolidCompression of
    0:   DoMessage('  Solid     = no' );
    else DoMessage('  Solid     = yes');
  end;

  DoMessage('  Items     = ' + IntToStr(TotalFiles));
  //DoMessage('  Sequences = ' + IntToStr(Sequences));

  //DoMessage('  Module  size = ' + IntToStr(FHeaders.ModuleSize));
  //DoMessage('  Packed  size = ' + IntToStr(TotalPacked));
  //DoMessage('  Archive size = ' + SizeToStr(SizeOfFile(FCommandLine.ArchiveName)));

  DoMessage(Cr + '   Date      Time     Attr          Size       Packed MTD Name                 ');
  DoMessage(     '---------- -------- ------- ------------ ------------ --- ---------------------');

  if FCommandLine.slsOption then FItemToList.Sort(CompareFilePath);

  TotalPackedSize := 0;
  TotalSize       := 0;
  TotalFiles      := 0;

  for I := 0 to FItemToList.Count - 1 do
  begin
    Item := FItemToList.Items[I];
    Inc(TotalSize, Item.UncompressedSize);
    Inc(TotalPackedSize, Item.CompressedSize);
    Inc(TotalFiles);

    Writeln(Format('%16s %7s %12s %12s %3s %s', [
      FileTimeToString(Item.LastModifiedTime), AttrToStr(Item.Attributes),
      SizeToStr(Item.UncompressedSize), SizeToStr(Item.CompressedSize),
      CompressionMethodToStr(Item), Item.FileName]));
  end;
  DoMessage('---------- -------- ------- ------------ ------------ --- ---------------------');
  // DoMessage(StringOfChar(' ', 27) + Format(' %12s %12s     %d file(s)', [SizeToStr(TotalSize), SizeToStr(TotalPacked), TotalFiles]));

  FItemToList.Destroy;
  FReader.Destroy;
end;

end.

