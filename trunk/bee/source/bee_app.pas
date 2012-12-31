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
    FArchiver: TArchiver;
    procedure OpenArchive;
    procedure CloseArchive;

    function QueryToUser(var Confirm: TArchiveConfirm): char;
    procedure DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
    procedure DoRequestImage(ImageNumber: longint;
      var ImageName: string; var Abort: boolean);
    procedure DoProgress(Percentage: longint);
    procedure DoMessage(const Message: string);
    procedure DoExtract(Item: TArchiveItem;
      var ExtractAs: string; var Confirm: TArchiveConfirm);
    procedure DoRename(Item: TArchiveItem;
      var RenameAs: string; var Confirm: TArchiveConfirm);
    procedure DoDelete(Item: TArchiveItem; var Confirm: TArchiveConfirm);
    procedure DoUpdate(SearchRec: TCustomSearchRec;
      var UpdateAs: string; var Confirm: TArchiveConfirm);



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
    procedure Terminate;
    procedure Execute;
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
  FSelfName := 'The Bee 0.8.0 build 1643 archiver utility, July 2012' + Cr +
               '(C) 1999-2013 Andrew Filinsky and Melchiorre Caruso';
  FArchiver := TArchiver.Create;
  FArchiver.OnRequestBlankImage := DoRequestBlankDisk;
  FArchiver.OnRequestImage      := DoRequestImage;
  FArchiver.OnProgress          := DoProgress;
  FArchiver.OnMessage           := DoMessage;
  FArchiver.OnExtract           := DoExtract;
  FArchiver.OnRename            := DoRename;
  FArchiver.OnDelete            := DoDelete;
  FArchiver.OnUpdate            := DoUpdate;
  { store command line }
  FCommandLine := TCommandLine.Create;
  FCommandLine.CommandLine := aCommandLine;
  { set thread priority }
  SetPriority(Ord(FCommandLine.priOption));
end;

destructor TBeeApp.Destroy;
begin
  FCommandLine.Destroy;
  FArchiver.Destroy;
  inherited Destroy;
end;

procedure TBeeApp.Terminate;
begin
  FArchiver.Terminate;
end;

procedure TBeeApp.Execute;
var
  S: string;
  StartTime: double;
begin
  StartTime := Now;
  DoMessage(FSelfName);
  if ExitStatus = esNoError then
    case FCommandLine.Command of
      cAdd:      EncodeShell;
      cDelete:   DeleteShell;
      cExtract:  DecodeShell(FALSE);
      cxExtract: DecodeShell(FALSE);
      cTest:     DecodeShell(TRUE);
      cRename:   RenameShell;
      cList:     ListShell;
      cHelp:     HelpShell;
    end;

  S := TimeDifference(StartTime);
  case ExitStatus of
    esNoError:          DoMessage(Format(emNoError,       [S]));
    esUnknowError:      DoMessage(Format(emUnknowError,   [S]));
    esCmdLineError:     DoMessage(Format(emCmdLineError,  [S]));
    esAllocMemError:    DoMessage(Format(emAllocMemError, [S]));
    esUserAbortError:   DoMessage(Format(emUserAbortError,[S]));
    else                DoMessage(Format(emUnknowError,   [S]));
  end;
end;

//

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

//

procedure TBeeApp.DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
var
  Ch: char;
begin
  Write(#8#8#8#8#8#8);
  Write(ParamToOem('Insert blank disk number #'+ IntToStr(DiskNumber)) + '. Continue? ');
  Readln(Ch);
  Ch := UpCase(OemToParam(Ch)[1]);
  while Pos(Ch, 'YNQ') < 1 do
  begin
    Write('Yes, No or Quit? ');
    Readln(Ch);
    Ch := UpCase(OemToParam(Ch)[1]);
  end;

  case Ch of
    'Y': Abort := FALSE;
    else Abort := TRUE;
  end;
end;

procedure TBeeApp.DoRequestImage(ImageNumber: longint;
  var ImageName: string; var Abort: boolean);
begin
  Write(#8#8#8#8#8#8);
  Writeln(ParamToOem(ImageName));
  Readln;
end;

procedure TBeeApp.DoMessage(const Message: string);
begin
  Write(#8#8#8#8#8#8);
  Writeln(ParamToOem(Message));
end;

procedure TBeeApp.DoProgress(Percentage: longint);
begin
  Write(#8#8#8#8#8#8);
  Write(Format('(%3d%%)', [Percentage]));
end;

procedure TBeeApp.DoExtract(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TArchiveConfirm);
begin
  case FCommandLine.Command of
    cExtract:  ExtractAs := ExtractFileName(Item.FileName);
    cXextract: ExtractAs := DeleteFilePath(FCommandLine.cdOption, Item.FileName);
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

procedure TBeeApp.DoRename(Item: TArchiveItem;
  var RenameAs: string; var Confirm: TArchiveConfirm);
begin
  Write('Rename file "', ParamToOem(RenameAs), '" as (empty to skip):');
  Readln(RenameAs);
  // convert oem to param
  RenameAs := OemToParam(RenameAs);
end;

procedure TBeeApp.DoDelete(Item: TArchiveItem;
  var Confirm: TArchiveConfirm);
begin
  Confirm :=arcOk;
end;

procedure TBeeApp.DoUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs: string; var Confirm: TArchiveConfirm);
var
  I, Index: longint;
  Item: TArchiveItem;
  AlternativeFileName: string;
begin
  UpdateAs := FCommandLine.cdOption + SearchRec.Name;
  I := FArchiver.Find(UpdateAs);
  if I <> -1 then
    Item := FArchiver.Items[I];

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
        until FArchiver.Find(AlternativeFileName) = -1;
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

//

procedure TBeeApp.OpenArchive;
begin
  FArchiver.OpenArchive(FCommandLine.ArchiveName);
  // compression mode
  case FCommandLine.mOption of
    moStore: FArchiver.CompressionMethod := actNone;
    else     FArchiver.CompressionMethod := actMain;
  end;
  FArchiver.CompressionLevel   := FCommandLine.mOption;
  FArchiver.DictionaryLevel    := FCommandLine.dOption;
  FArchiver.CompressionBlock   := FCommandLine.sOption;
  FArchiver.ConfigurationName  := FCommandLine.cfgOption;
  FArchiver.ForceFileExtension := FCommandLine.fOption;
  // encryption mode
  case Length(FCommandLine.pOption) of
    0:   FArchiver.EncrypionMethod := acrtNone;
    else FArchiver.EncrypionMethod := acrtMain;
  end;
  FArchiver.ArchivePassword := FCommandLine.pOption;
  // ...
  FArchiver.Threshold       := FCommandLine.iOption;
  FArchiver.ArchiveSFX      := FCommandLine.sfxOption;
  FArchiver.TestTempArchive := FCommandLine.tOption;
  FArchiver.WorkDirectory   := FCommandLine.wdOption;
  //FUpdater.ArchiveComment :=
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
  DoMessage('  a  Add files to archive');
  DoMessage('  d  Delete files from archive');
  DoMessage('  e  Extract files from archive');
  DoMessage('  h  show command line Help');
  DoMessage('  l  List archive');
  DoMessage('  r  Rename files in archive');
  DoMessage('  t  Test archive files');
  DoMessage('  x  eXtract files from archive with path name');
  DoMessage(Cr + '<Switches>');
  DoMessage('  --            stop switches parsing');
  DoMessage('  -cd[dirname]  set current archive directory');
  DoMessage('  -cm{params}   set Compression method');
  DoMessage('  -em{params}   set Encryption method)');
  DoMessage('  -pri{params}  set process Priority ');
  DoMessage('  -r[-|w]       Recurse subdirectories');
  DoMessage('  -sfx[filename]  add self-extractor module');
  DoMessage('  -sls  show list sorted by filename - for l (list) command');
  DoMessage('  -t    Test temorary archive after process');
  DoMessage('  -um   Update files method');
  DoMessage('  -wd[dirname]   set temporany work directory');
  DoMessage('  -x[filenames]  eXclude filenames');
  DoMessage(Cr + 'Use BeeOpt to make most optimal parameters.' + Cr);
end;

procedure TBeeApp.EncodeShell;
var
  I: longint;
  Scanner: TFileScanner;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, ['...']));
    Scanner := TFileScanner.Create;
    with FCommandLine do
      for I := 0 to FileMasks.Count - 1 do
        Scanner.Scan(FileMasks[I], xOptions, rOption);

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
      if  FileNameMatch(FArchiver.Items[I].FileName,
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
    for I := 0 to FArchiver.Count - 1 do
    begin
      Item := FArchiver.Items[I];
      if FArchiver.IsTagged(I) then
        ItemToList.Add(Item);

      if aifVersionNeededToExtract in Item.Flags            then VersionNeededToExtract := Item.VersionNeededToExtract;
      if acfCompressionMethod      in Item.CompressionFlags then CompressionMethod      := Item.CompressionMethod;
      if acfCompressionLevel       in Item.CompressionFlags then CompressionLevel       := Item.CompressionLevel;
      if acfDictionaryLevel        in Item.CompressionFlags then DictionaryLevel        := Item.DictionaryLevel;
      if acfCompressionBlock       in Item.CompressionFlags then Inc(WithSolidCompression);
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
      DoMessage(StringOfChar(' ', 27) + Format(' %12s %12s     %d file(s)', [SizeToStr(TotalSize), SizeToStr(TotalPackedSize), TotalFiles]));
    end;
    ItemToList.Destroy;
  end;
  CloseArchive;
end;

end.

