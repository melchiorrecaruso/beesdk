{
  Copyright (c) 2012-2014 Melchiorre Caruso.

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

{
  Contains:

    Bx archiver shell.

  Modifyed:

    v1.0.0 build 2225 - 2014.01.26 by Melchiorre Caruso.

}

unit bx_application;

{$I bx_compiler.inc}

interface

uses
  Classes,
  SysUtils,
  bx_archiver,
  bx_commandline,
  bx_common,
  bx_dirscanner,
  bx_messages;

type
  { TBxAssumeValueOnAllQueries type }

  TBxAssumeValueOnAllQueries = (avNone, avNo, avYes);

  { TBxApplication class }

  TBxApplication = class(TObject)
  private
    FArchiver: TArchiver;
    FArchiveFinder: TArchiveItemFinder;
    FAssumeValueOnAllQueries: TBxAssumeValueOnAllQueries;
    FCommandLine: TCommandLineParser;
    FUpdateMethod: TUpdateMethod;
    procedure TagItems;
    { Help routines}
    function  QueryHowToRename(const Message: string): string;
    function  QueryHowToUpdate(const Message: string): char;
    procedure ExtractItem(Index: longint);
    procedure RenameItem (Index: longint);
    procedure UpdateItem (Rec: TDirScannerItem);
    { Events routines}
    procedure DoMessage(const Message: string);
    procedure DoPercentage(Percentage: longint);
    procedure DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
    procedure DoRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
    { Open/close routines}
    procedure OpenArchive;
    procedure CloseArchive;
    { shells routines}
    procedure DeleteShell;
    procedure ExtractShell;
    procedure HelpShell;
    procedure ListShell;
    procedure QuickShell;
    procedure RenameShell;
    procedure TestShell;
    procedure UpdateShell;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Terminate;
    procedure Execute;
  end;

implementation

{ TBxApplication class }

constructor TBxApplication.Create;
begin
  inherited Create;
  { set archiver events }
  FArchiver := TArchiver.Create;
  FArchiver.OnMessage          := DoMessage;
  FArchiver.OnPercentage       := DoPercentage;
  FArchiver.OnRequestBlankDisk := DoRequestBlankDisk;
  FArchiver.OnRequestImage     := DoRequestImage;
  { load command line }
  FCommandLine := TCommandLineParser.Create;
  FCommandLine.Execute;
  { set idle priority }
  if swtB in FCommandLine.Options then
    SetIdlePriority;
  { set update method }
  FUpdateMethod := umAddQuery;
  if swtU in FCommandLine.Options then
    FUpdateMethod := FCommandLine.SwitchU;
  { set if assume value on all queries }
  FAssumeValueOnAllQueries := avNone;
  if swtY in FCommandLine.Options then
  begin
    if FCommandLine.SwitchY then
      FAssumeValueOnAllQueries := avYes
    else
      FAssumeValueOnAllQueries := avNo;
  end;
  { create archive item finder }
  FArchiveFinder := TArchiveItemFinder.Create;
end;

destructor TBxApplication.Destroy;
begin
  FArchiveFinder.Destroy;
  FCommandLine.Destroy;
  FArchiver.Destroy;
  inherited Destroy;
end;

procedure TBxApplication.Execute;
var
  StartTime: double;
begin
  StartTime := Now;

  DoMessage('The BX 1.0.0 archiver utility, Copyright (c) 2014 Melchiorre Caruso.');
  if ExitStatus = esNoError then
    case FCommandLine.Command of
      cmdA: UpdateShell;
      cmdD: DeleteShell;
      cmdE: ExtractShell;
      cmdH: HelpShell;
      cmdL: ListShell;
      cmdQ: TestShell;
      cmdR: RenameShell;
      cmdT: TestShell;
      cmdX: ExtractShell;
    end;

  if FCommandLine.Command <> cmdH then
  begin
    DoMessage(LineEnding + Format(GetExitMessage, [TimeDifference(StartTime)]));
  end;
end;

procedure TBxApplication.Terminate;
begin
  FArchiver.Terminate;
end;

{ Help routines}

function TBxApplication.QueryHowToUpdate(const Message: string): char;
var
  Answer: string;
begin
  if FAssumeValueOnAllQueries = avNone then
  begin
    Write(#8#8#8#8#8#8, ParamToOem(Message));
    repeat
      Readln(Answer);
      Answer := UpperCase(OemToParam(Answer));
      if Length(Answer) = 1 then
        if Pos(Answer, 'YNASQ01234567') > 0 then
        begin
          Result := Answer[1];
          case Result of
            'Y': ;// nothing do to
            'N': ;// nothing do to
            'A': FAssumeValueOnAllQueries := avYes;
            'S': FAssumeValueOnAllQueries := avNo;
            'Q': SetExitStatus(esUserAbortError);
            else FUpdateMethod := TUpdateMethod(StrToInt(Result));
          end;
          Break;
        end;
      Writeln('Choices:');
      Writeln('[0] Add only new files                [Y] Yes');
      Writeln('[1] Update only existing files        [N] No');
      Writeln('[2] Replace only existing files       [A] Yes to all queries');
      Writeln('[3] Query always                      [S] No to all queries');
      Writeln('[4] Add and update existing files     [Q] Quit, abort process');
      Writeln('[5] Add and replace existing files');
      Writeln('[6] Add and query if already exists');
      Writeln('[7] Add and autorename if already exists');
      Write(#8#8#8#8#8#8, ParamToOem(Message));
    until ExitStatus = esNoError;
  end else
    if FAssumeValueOnAllQueries = avNo then
      Result := 'N'
    else
      if FAssumeValueOnAllQueries = avYes then
        Result := 'Y'
      else
        SetExitStatus(esCaseError);
end;

function TBXApplication.QueryHowToRename(const Message: string): string;
begin
  Write(#8#8#8#8#8#8, ParamToOem(Message));
  Readln(Result);
  begin
    Result := OemToParam(Result);
  end;
end;

procedure TBxApplication.ExtractItem(Index: longint);
var
  Count: longint;
  Item: TArchiveItem;
begin
  Item := FArchiveFinder.Items[Index];
  if FCommandLine.Command = cmdE then
    Item.ExternalFileName := ExtractFileName(Item.FileName)
  else
    if FCommandLine.Command = cmdX then
      Item.ExternalFileName := DeleteFilePath(FCommandLine.SwitchCD, Item.FileName)
    else
      SetExitStatus(esCaseError);

  Item.UnTag;
  case FUpdateMethod of
    umAdd: begin
      if FileExists(Item.ExternalFileName) = FALSE then
      begin
        Item.Tag;
      end;
    end;
    umReplace: begin
      if FileExists(Item.ExternalFileName) = TRUE then
      begin
        Item.Tag;
      end;
    end;
    umUpdate: begin
      if FileExists(Item.ExternalFileName) = TRUE then
        if Item.LastModifiedTime > FileAge(Item.ExternalFileName) then
        begin
          Item.Tag;
        end;
    end;
    umAddUpdate: begin
      if FileExists(Item.ExternalFileName) = FALSE then
      begin
        Item.Tag;
      end else
        if Item.LastModifiedTime > FileAge(Item.ExternalFileName) then
        begin
          Item.Tag;
        end;
    end;
    umAddReplace: begin
      Item.Tag;
    end;
    umAddAutoRename: begin
      Count := 0;
      while FileExists(GenerateAltFileName(Item.ExternalFileName, Count)) = TRUE do
      begin
        Inc(Count);
      end;
      Item.ExternalFileName := GenerateAltFileName(Item.ExternalFileName, Count);
      Item.Tag;
    end;
    umAddQuery: begin
      if FileExists(Item.ExternalFileName) = FALSE then
      begin
        Item.Tag;
      end else
        case QueryHowToUpdate('Overwrite "' + Item.ExternalFileName + '"? ') of
          'Y': Item.Tag;
          'N': ;// nothing to do
          'A': Item.Tag;
          'S': ;// nothing to do
          'Q': ;// nothing to do
          else ExtractItem(Index);
        end;
    end;
    umQuery: begin
      if FileExists(Item.ExternalFileName) = FALSE then
        case QueryHowToUpdate('Extract "' + Item.ExternalFileName + '"? ') of
          'Y': Item.Tag;
          'N': ;// nothing to do
          'A': Item.Tag;
          'S': ;// nothing to do
          'Q': ;// nothing to do
          else ExtractItem(Index);
        end
      else
        case QueryHowToUpdate('Overwrite "' + Item.ExternalFileName + '"? ') of
          'Y': Item.Tag;
          'N': ;// nothing to do
          'A': Item.Tag;
          'S': ;// nothing to do
          'Q': ;// nothing to do
          else ExtractItem(Index);
        end;
    end;
  end;
end;

procedure TBxApplication.RenameItem(Index: longint);
var
  Item: TArchiveItem;
  RenameAs: string;
begin
  Item := FArchiveFinder.Items[Index];
  Item.UnTag;
  repeat
    RenameAs := QueryHowToRename('Rename file "' +
      Item.FileName + '" as (empty to skip): ');

    if RenameAs <> '' then
    begin
      if FArchiveFinder.Find(RenameAs) = -1 then
      begin
        Item.FileName := RenameAs;
        Item.Tag;

        FArchiveFinder.Delete(Index);
        FArchiveFinder.Add(Item);
        Break;
      end;
    end else
      Break;

  until ExitStatus = esNoError;

  if Item.Tagged then
  begin
    if swtCC in FCommandLine.Options then
      Item.Comment := FCommandLine.SwitchCC.Text;
  end;
end;

procedure TBxApplication.UpdateItem(Rec: TDirScannerItem);
var
  Count: longint;
  Item: TArchiveItem;
  ItemIndex: longint;
  ItemName: string;
begin
  ItemName  := FCommandLine.SwitchCD + Rec.FileName;
  ItemIndex := FArchiveFinder.Find(ItemName);
  if ItemIndex <> -1 then
    Item := FArchiveFinder.Items[ItemIndex]
  else
    Item := nil;

  // Update method...
  case FUpdateMethod of
    umAdd: begin
      if Item = nil then
      begin
        Item := TArchiveItem.Create(ItemName);
        FArchiveFinder.Add(FArchiver.Add(Item)).Tag(Rec);
      end;
    end;
    umReplace: begin
      if Item <> nil then
      begin
        Item.Tag(Rec);
      end;
    end;
    umUpdate: begin
      if Item <> nil then
        if Item.LastModifiedTime < Rec.FileTime then
        begin
          Item.Tag(Rec);
        end;
    end;
    umAddUpdate: begin
      if Item = nil then
      begin
        Item := TArchiveItem.Create(ItemName);
        FArchiveFinder.Add(FArchiver.Add(Item)).Tag(Rec);
      end else
        if Item.LastModifiedTime < Rec.FileTime then
        begin
          Item.Tag(Rec);
        end;
    end;
    umAddReplace: begin
      if Item = nil then
      begin
        Item := TArchiveItem.Create(ItemName);
        FArchiveFinder.Add(FArchiver.Add(Item)).Tag(Rec);
      end else
        Item.Tag(Rec);
    end;
    umAddAutoRename: begin
      Count := 0;
      while FArchiveFinder.Find(GenerateAltFileName(ItemName, Count)) <> -1 do
      begin
        Inc(Count);
      end;
      Item := TArchiveItem.Create(GenerateAltFileName(ItemName, Count));
      FArchiveFinder.Add(FArchiver.Add(Item)).Tag(Rec);
    end;
    umAddQuery: begin
      if Item = nil then
      begin
        Item := TArchiveItem.Create(ItemName);
        FArchiveFinder.Add(FArchiver.Add(Item)).Tag(Rec);
      end else
        case QueryHowToUpdate('Overwrite "' + Item.FileName + '"? ') of
          'Y': Item.Tag(Rec);
          'N': ;// nothing to do
          'A': Item.Tag(Rec);
          'S': ;// nothing to do
          'Q': ;// nothing to do
          else UpdateItem(Rec);
        end;
    end;
    umQuery: begin
      if Item = nil then
        case QueryHowToUpdate('Add "' + ItemName + '"? ') of
          'Y': begin
            Item := TArchiveItem.Create(ItemName);
            FArchiveFinder.Add(FArchiver.Add(Item)).Tag(Rec);
          end;
          'N': ;// nothing to do
          'A': begin
            Item := TArchiveItem.Create(ItemName);
            FArchiveFinder.Add(FArchiver.Add(Item)).Tag(Rec);
          end;
          'S': ;// nothing to do
          'Q': ;// nothing to do
          else UpdateItem(Rec);
        end
      else
        case QueryHowToUpdate('Overwrite "' + Item.FileName  + '"? ') of
          'Y': Item.Tag(Rec);
          'N': ;// nothing to do
          'A': Item.Tag(Rec);
          'S': ;// nothing to do
          'Q': ;// nothing to do
          else UpdateItem(Rec);
        end;
    end;
  end;

  if Item <> nil then
    if Item.Tagged then
    begin
      if swtCC in FCommandLine.Options then
        Item.Comment := FCommandLine.SwitchCC.Text;
    end;
end;

{ TBxApplicatin class events }

procedure TBxApplication.DoMessage(const Message: string);
begin
  Writeln(#13, #13: 8, ParamToOem(Message));
end;

procedure TBxApplication.DoPercentage(Percentage: longint);
begin
  Write(#8#8#8#8#8#8, ParamToOem(Format('(%3d%%)', [Percentage])));
end;

procedure TBxApplication.DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
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

procedure TBxApplication.DoRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
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

// --- //

procedure TBxApplication.OpenArchive;
var
  I: longint;
begin
  FArchiver.OpenArchive(FCommandLine.ArchiveName);
  for I := 0 to FArchiver.Count - 1 do
    FArchiveFinder.Add(FArchiver.Items[I]);
  // archive comment
  if swtACC in FCommandLine.Options then
    FArchiver.Comment := FCommandLine.SwitchACC.Text;
  // compression mode
  if swtC in FCommandLine.Options then
    FArchiver.CompressionParams := FCommandLine.SwitchC;
  // encryption mode
  if swtE in FCommandLine.Options then
    FArchiver.EncryptionParams := FCommandLine.SwitchE;
  // hashing mode
  if swtH in FCommandLine.Options then
    FArchiver.HashingParams := FCommandLine.SwitchH;
  // password
  if swtP in FCommandLine.Options then
    FArchiver.Password := FCommandLine.SwitchP;
  // self extractor
  if swtSFX in FCommandLine.Options then
    FArchiver.SelfExtractor := FCommandLine.SwitchSFX;
  // test temporary archive
  if swtT in FCommandLine.Options then
    FArchiver.TestTempArchive := FCommandLine.SwitchT;
  // volume size
  if swtV in FCommandLine.Options then
    FArchiver.VolumeSize := FCommandLine.SwitchV;
  // verbose mode
  if swtVB in FCommandLine.Options then
    FArchiver.VerboseMode := FCommandLine.SwitchVB;
  // work directory
  if swtW in FCommandLine.Options then
    FArchiver.WorkDirectory := FCommandLine.SwitchW;
end;

procedure TBxApplication.CloseArchive;
begin
  FArchiveFinder.Clear;
  FArchiver.CloseArchive;
end;

{ shell procedures }

function CompareFileExt(P1, P2: pointer): longint;
begin

  if (TDirScannerItem(P1).FileAttr and (faDirectory or faSymLink)) =
     (TDirScannerItem(P2).FileAttr and (faDirectory or faSymLink)) then
  begin
    Result := AnsiCompareText(
      ExtractFileExt(TDirScannerItem(P1).FileName),
      ExtractFileExt(TDirScannerItem(P2).FileName));
  end else
    if (TDirScannerItem(P1).FileAttr and faDirectory) > 0 then
      Result := - 1
    else
      if (TDirScannerItem(P2).FileAttr and faDirectory) > 0 then
        Result := 1
      else
        if (TDirScannerItem(P1).FileAttr and faSymLink) > 0 then
          Result := - 1
        else
          if (TDirScannerItem(P2).FileAttr and faSymLink) > 0 then
            Result := 1
          else
            Result := 0;


  if Result = 0 then
    Result := AnsiCompareText(
      ExtractFileExt(TDirScannerItem(P1).FileName),
      ExtractFileExt(TDirScannerItem(P2).FileName));

  if Result = 0 then
  begin
    if TDirScannerItem(P1).FileSize > TDirScannerItem(P2).FileSize then
      Result := - 1
    else
      if TDirScannerItem(P1).FileSize < TDirScannerItem(P2).FileSize then
        Result := 1;
  end;
end;

function CompareFilePath(P1, P2: pointer): longint;
begin
  Result := AnsiCompareFileName(
    ExtractFilePath(TArchiveItem(P1).FileName),
    ExtractFilePath(TArchiveItem(P2).FileName));

  if Result = 0 then
  begin
    Result := AnsiCompareFileName(
      ExtractFileName(TArchiveItem(P1).FileName),
      ExtractFileName(TArchiveItem(P2).FileName));
  end;
end;

procedure TBxApplication.TagItems;
var
  I, J: longint;
begin
  for I := 0 to FArchiveFinder.Count - 1 do
    for J := 0 to FCommandLine.FileMasks.Count - 1 do
      if FileNameMatch(FArchiveFinder.Items[I].FileName,
        FCommandLine.FileMasks[J], FCommandLine.SwitchR[J]) then
          FArchiveFinder.Items[I].Tag;

  for I := 0 to FArchiveFinder.Count - 1 do
    for J := 0 to FCommandLine.SwitchX.Count - 1 do
      if FileNameMatch(FArchiveFinder.Items[I].FileName,
        FCommandLine.SwitchX[J], FCommandLine.SwitchRX[J]) then
          FArchiveFinder.Items[I].UnTag;
end;

procedure TBxApplication.DeleteShell;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, []));
    TagItems;
    begin
      FArchiver.DeleteTagged;
    end;
  end;
  CloseArchive;
end;

procedure TBxApplication.ExtractShell;
var
  I: longint;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, []));
    TagItems;

    for I := 0 to FArchiveFinder.Count - 1 do
      if ExitStatus = esNoError then
        if FArchiveFinder.Items[I].Tagged then
        begin
          ExtractItem(I);
        end;

    FArchiver.ExtractTagged;
  end;
  CloseArchive;
end;

procedure TBxApplication.HelpShell;
begin
  DoMessage('Usage: BX <command> [<switches>...] <archive-name> [<file-names>...]' + LineEnding);
  DoMessage('<Commands>');
  DoMessage('  a: add files to archive');
  DoMessage('  d: delete files from archive');
  DoMessage('  e: extract files from archive');
  DoMessage('  h: show this help');
  DoMessage('  l: list contents of archive');
  DoMessage('  r: rename files in archive');
  DoMessage('  t: test integrity of archive files');
  DoMessage('  x: extract files from archive with path name');
  DoMessage('<Switches>');
  DoMessage('  -acc: set archive comment');
  DoMessage('  -b: work in Background');
  DoMessage('  -c: set compression parameters');
  DoMessage('  -cc: set comment');
  DoMessage('  -cd: set current archive directory');
  DoMessage('  -ci: set check interity parameters');
  DoMessage('  -e: set encryption parameters');
  DoMessage('  -p: set Password ');
  DoMessage('  -r: recurse subdirectories');
  DoMessage('  -sfx: create self-extracting archive');
  DoMessage('  -sl: show list sorted by filename - for l (list) command');
  DoMessage('  -ss: stop switches parsing');
  DoMessage('  -t: test temporary archive after process');
  DoMessage('  -u: update files method');
  DoMessage('  -v: create volumes ');
  DoMessage('  -vb: verbose mode ');
  DoMessage('  -w: set temporary work directory');
  DoMessage('  -x: exclude filenames');
  DoMessage('  -y: assume yes on all queries' + LineEnding);
end;

procedure TBxApplication.ListShell;
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
    DoMessage(Format(cmScanning, []));
    TagItems;

    TotalSize       := 0;
    TotalPackedSize := 0;
    TotalFiles      := 0;

    ItemToList := TList.Create;
    for I := 0 to FArchiver.Count - 1 do
    begin
      Item := FArchiver.Items[I];
      if Item.Tagged then
      begin
        ItemToList.Add(Item);

        Inc(TotalSize, Item.UncompressedSize);
        Inc(TotalPackedSize, Item.CompressedSize);
        Inc(TotalFiles);
      end;
    end;

    if FCommandLine.SwitchSL then
      ItemToList.Sort(@CompareFilePath);

    if FCommandLine.SwitchVB then
    begin
      for I := 0 to ItemToList.Count - 1 do
      begin
        if ExitStatus <> esNoError then Break;
        Item := TArchiveItem(ItemToList.Items[I]);
        DoMessage(Format(     '                   Index: %u',       [Item.Index]));
        DoMessage(Format(     '                    Name: %s',       [Item.FileName]));
        if Item.UncompressedSize > 0 then
          DoMessage(Format(   '       Size/Packed/Ratio: %s/%s/%s', [SizeToStr(Item.UncompressedSize), SizeToStr(Item.CompressedSize),
                                                                       RatioToStr(Item.CompressedSize, Item.UncompressedSize)]));
        DoMessage(Format(     '      Last modified time: %s',       [bx_common.FileTimeToString(Item.LastModifiedTime)]));
        DoMessage(Format(     '        Last stored time: %s',       [bx_common.FileTimeToString(Item.LastStoredTime)]));
        DoMessage(Format(     '              Attributes: %s',       [AttrToStr(Item.Attributes)]));
        if Item.Comment <> '' then
          DoMessage(Format(   '                 Comment: %s',       [Item.Comment]));
        if Item.Link <> '' then
          DoMessage(Format(   '                    Link: %s',       [Item.Link]));
        if Item.UncompressedSize > 0 then
          DoMessage(Format(     '    Disk number/position: %u/%u',    [Item.ImageNumber, Item.ImageSeek]));
        if Ord(Item.CheckMethod) <> 0 then
          DoMessage(Format(   '     Check method/digest: %s/%s',    [HashingMethodTostr(Item.CheckMethod), Item.CheckDigest]));
        if Ord(Item.CheckMethodAux) <> 0 then
          DoMessage(Format(   ' Check method/digest aux: %s/%s',    [HashingMethodToStr(Item.CheckMethodAux), Item.CheckDigestAux]));
        if Ord(Item.CompressionMethod) <> 0 then
        begin
          DoMessage(Format(   'Compression method/block: %s/%u',    [CompressionMethodToStr(Item.CompressionMethod), Item.CompressionBlock]));
          DoMessage(Format(   '   Compression level/aux: %u/%u',    [Item.CompressionLevel, Item.CompressionLevelAux]));
          DoMessage(Format(   '  Compression filter/aux: %s/%s',    [Item.CompressionFilter, Item.CompressionFilterAux]));
        end;
        if Ord(Item.EncryptionMethod) <> 0 then
          DoMessage(Format(   '       Encryption method: %s',       [EncryptionMethodTostr(Item.EncryptionMethod)]));
        DoMessage('');
      end;
    end else
    begin
      DoMessage('   Date      Time     Attr          Size       Packed Name                 ');
      DoMessage(     '---------- -------- ------- ------------ ------------ ---------------------');
      for I := 0 to ItemToList.Count - 1 do
      begin
        if ExitStatus <> esNoError then Break;
        Item := TArchiveItem(ItemToList.Items[I]);
        DoMessage(Format('%16s %7s %12s %12s %s', [
          bx_common.FileTimeToString(Item.LastModifiedTime), AttrToStr(Item.Attributes),
          SizeToStr(Item.UncompressedSize), SizeToStr(Item.CompressedSize), Item.FileName]));
      end;
      DoMessage('---------- -------- ------- ------------ ------------ ---------------------');
      DoMessage(StringOfChar(' ', 27) + Format(' %12s %12s %d file(s)', [SizeToStr(TotalSize),
        SizeToStr(TotalPackedSize), TotalFiles]));
    end;

    if FArchiver.Comment <> '' then
    begin
      DoMessage('   Archive comment');
      DoMessage('---------------------------------------------------------------------------');
      DoMessage(FArchiver.Comment);
      DoMessage('---------------------------------------------------------------------------');
    end;
    DoMessage(Format(LineEnding + 'Last modified time: %16s', [bx_Common.FileTimeToString(FArchiver.LastModifiedTime)]));
    ItemToList.Destroy;
  end;
  CloseArchive;
end;

procedure TBxApplication.QuickShell;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, []));
    TagItems;
    begin
      FArchiver.TestTagged;
    end;
  end;
  CloseArchive;
end;

procedure TBxApplication.RenameShell;
var
  I: longint;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, []));
    TagItems;

    for I := 0 to FArchiveFinder.Count - 1 do
      if ExitStatus = esNoError then
        if FArchiveFinder.Items[I].Tagged then
        begin
          RenameItem(I);
        end;

    FArchiver. RenameTagged;
  end;
  CloseArchive;
end;

procedure TBxApplication.TestShell;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, []));
    TagItems;
    begin
      FArchiver.TestTagged;
    end;
  end;
  CloseArchive;
end;

procedure TBxApplication.UpdateShell;
var
  I: longint;
  Scanner: TDirScanner;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoMessage(Format(cmScanning, []));
    Scanner := TDirScanner.Create;
    for I := 0 to FCommandLine.FileMasks.Count - 1 do
      Scanner.Add(FCommandLine.FileMasks[I], FCommandLine.SwitchR[I]);
    for I := 0 to FCommandLine.SwitchX.Count - 1 do
      Scanner.Delete(FCommandLine.SwitchX[I], FCommandLine.SwitchRX[I]);
    Scanner.Sort(@CompareFileExt);

    for I := 0 to Scanner.Count - 1 do
      if ExitStatus = esNoError then
      begin
        UpdateItem(Scanner.Items[I]);
      end;

    FArchiver.UpdateTagged;
    FreeAndNil(Scanner);
  end;
  CloseArchive;
end;

end.
