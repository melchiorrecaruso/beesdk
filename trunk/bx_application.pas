{
  Copyright (c) 2013 Melchiorre Caruso.

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

    Bx archiver shell.

  Fist release:

    v1.0 build 0021 - 2013.10.07 by Melchiorre Caruso.

  Modifyed:

}

unit bx_Application;

{$I bx_compiler.inc}

interface

uses
  Classes,
  SysUtils,
  // ---
  bx_Archiver,
  bx_CommandLine,
  bx_Common,
  bx_FileStream,
  bx_Messages;

type
  { TBxApplication class }

  TBxApplication = class(TObject)
  private
    FSelfName: string;
    FArchiver: TArchiver;
    FCommandLine: TCommandLineParser;
    FUpdateMethod: TUpdateMethod;
    FAssumeYesOnAllQueries: boolean;
    procedure TagItems;
    { Help routines}
    function QueryToUser(const Message: string;
      var Confirm: TConfirm): boolean;
    { Events routines}
    procedure DoCommentArc(var CommentAs: string; var Confirm: TConfirm);
    procedure DoCommentItem(Item: TArchiveItem; var CommentAs: string; var Confirm: TConfirm);
    procedure DoExtractItem(Item: TArchiveItem; var ExtractAs: string; var Confirm: TConfirm);
    procedure DoDeleteItem(Item: TArchiveItem; var Confirm: TConfirm);
    procedure DoSendMessage(const Message: string);
    procedure DoSendPercentage(Percentage: longint);
    procedure DoRequestBlankDisk(DiskNumber: longint; var Abort : Boolean);
    procedure DoRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
    procedure DoRenameItem(Item: TArchiveItem; var RenameAs: string; var Confirm: TConfirm);
    procedure DoUpdateItem(SearchRec: TCustomSearchRec; var UpdateAs: string; var Confirm: TConfirm);
    { Open/close routines}
    procedure OpenArchive;
    procedure CloseArchive;
    { shells routines}
    procedure CustomShell;
    procedure EncodeShell;
    procedure HelpShell;
    procedure ListShell;
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
  FSelfName := 'The BX 1.0 archiver utility, Copyright (c) 2013 Melchiorre Caruso.';
  { set archiver events }
  FArchiver := TArchiver.Create;
  FArchiver.OnCommentArc        := DoCommentArc;
  FArchiver.OnCommentItem       := DoCommentItem;
  FArchiver.OnDeleteItem        := DoDeleteItem;
  FArchiver.OnExtractItem       := DoExtractItem;
  FArchiver.OnSendMessage       := DoSendMessage;
  FArchiver.OnSendPercentage    := DoSendPercentage;
  FArchiver.OnRenameItem        := DoRenameItem;
  FArchiver.OnRequestBlankDisk  := DoRequestBlankDisk;
  FArchiver.OnRequestImage      := DoRequestImage;
  FArchiver.OnUpdateItem        := DoUpdateItem;
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
  { set if assume yes on all queries }
  FAssumeYesOnAllQueries := FALSE;
  if swtY in FCommandLine.Options then
    FAssumeYesOnAllQueries := FCommandLine.SwitchY;
end;

destructor TBxApplication.Destroy;
begin
  FCommandLine.Destroy;
  FArchiver.Destroy;
  inherited Destroy;
end;

procedure TBxApplication.Execute;
var
  StartTime: double;
begin
  StartTime := Now;
  DoSendMessage(FSelfName);
  if ExitStatus = esNoError then
    case FCommandLine.Command of
      cmdA: EncodeShell;
      cmdD: CustomShell;
      cmdE: CustomShell;
      cmdH: HelpShell;
      cmdL: ListShell;
      cmdQ: CustomShell;
      cmdR: CustomShell;
      cmdT: CustomShell;
      cmdX: CustomShell;
    end;

  if FCommandLine.Command <> cmdH then
    DoSendMessage(LineEnding + Format(GetExitMessage, [TimeDifference(StartTime)]));
end;

procedure TBxApplication.Terminate;
begin
  FArchiver.Terminate;
end;

{ Help routines}

function TBxApplication.QueryToUser(const Message: string; var Confirm: TConfirm): boolean;
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
        if Pos(Answer, 'YNAQ01234567') > 0 then
        begin
          case Answer[1] of
            'Y': Confirm := arcOk;
            'N': Confirm := arcCancel;
            'A': begin
              Confirm := arcOk;
              FAssumeYesOnAllQueries := TRUE;
            end;
            'Q': Confirm := arcQuit;
            else begin
              Result := FALSE;
              FUpdateMethod := TUpdateMethod(StrToInt(Answer));
            end;
          end;
          Break;
        end;
      Writeln('Choices:');
      Writeln('[Y] Yes                         [2] Replace only existing files');
      Writeln('[N] No                          [3] Query always');
      Writeln('[A] Yes to all queries          [4] Add and update existing files');
      Writeln('[Q] Quit, abort process         [5] Add and replace existing files');
      Writeln('[0] Add only new files          [6] Add and query if already exists');
      Writeln('[1] Update only existing files  [7] Add and rename if already exists');
      Write(#8#8#8#8#8#8, ParamToOem(Message));
    until FALSE;
  end else
    Confirm := arcOk;
end;

{ TBxApplicatin class events }

procedure TBxApplication.DoCommentArc(var CommentAs: string; var Confirm: TConfirm);
begin
  Confirm := arcCancel;
  if swtACC in FCommandLine.Options then
  begin
    Confirm   := arcOk;
    CommentAs := FCommandLine.SwitchACC;
  end;
end;

procedure TBxApplication.DoCommentItem(Item: TArchiveItem; var CommentAs: string; var Confirm: TConfirm);
begin
  Confirm := arcCancel;
  if swtCC in FCommandLine.Options then
  begin
    Confirm   := arcOk;
    CommentAs := FCommandLine.SwitchCC;
  end;
end;

procedure TBxApplication.DoExtractItem(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TConfirm);
var
  B: boolean;
  StartName: string;
  StartIndex: longint;
begin
  case FCommandLine.Command of
    cmdE: ExtractAs := ExtractFileName(                       Item.FileName);
    cmdX: ExtractAs := DeleteFilePath (FCommandLine.SwitchCD, Item.FileName);
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
        ExtractAs := GenerateAltFileName(StartName, StartIndex);
      Confirm := arcOk;
    end;
    umQuery        : begin
      if FileExists(ExtractAs) = TRUE then
        B := QueryToUser('Overwrite "' + ExtractAs + '"? ', Confirm)
      else
        B := QueryToUser('Extract "'   + ExtractAs + '"? ', Confirm);

      if B = FALSE then
        DoExtractItem(Item, ExtractAs, Confirm);
    end;
    umAddQuery     : begin
      if FileExists(ExtractAs) = TRUE then
      begin
        B := QueryToUser('Overwrite "' + ExtractAs + '"? ', Confirm);

        if B = FALSE then
          DoExtractItem(Item, ExtractAs, Confirm);
      end else
        Confirm := arcOk;
    end;
  end;
end;

procedure TBxApplication.DoDeleteItem(Item: TArchiveItem; var Confirm: TConfirm);
begin
  Confirm := arcOk;
end;

procedure TBxApplication.DoSendMessage(const Message: string);
begin
  Writeln(#13, #13: 8, ParamToOem(Message));
end;

procedure TBxApplication.DoSendPercentage(Percentage: longint);
begin
  Write(#8#8#8#8#8#8, ParamToOem(Format('(%3d%%)', [Percentage])));
end;

procedure TBxApplication.DoRenameItem(Item: TArchiveItem; var RenameAs: string; var Confirm: TConfirm);
var
  Answer: string;
begin
  Write(#8#8#8#8#8#8, ParamToOem('Rename file "'
    + RenameAs + '" as (empty to skip): '));

  Confirm := arcCancel;
  repeat
    Readln(Answer);
    if Answer = '' then Break;
    if FArchiver.IndexOf(Answer) = -1 then
    begin
      // convert oem to param
      RenameAs := OemToParam(Answer);
      Confirm  := arcOk;
      Break;
    end;
  until TRUE;
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

procedure TBxApplication.DoUpdateItem(SearchRec: TCustomSearchRec; var UpdateAs: string; var Confirm: TConfirm);
var
  B: boolean;
  StartIndex: longint;
  StartName: string;
  Item: TArchiveItem;
  I: longint;
begin
  UpdateAs := FCommandLine.SwitchCD + SearchRec.Name;
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
        UpdateAs := GenerateAltFileName(StartName, StartIndex);
      Confirm := arcOk;
    end;
    umAddQuery     : begin
      if I <> - 1 then
      begin
        B := QueryToUser('Overwrite "' + Item.FileName + '"? ', Confirm);

        if B = FALSE then
          DoUpdateItem(SearchRec, UpdateAs, Confirm);
      end else
        Confirm := arcOk;
    end;
    umQuery        : begin
      if I <> - 1 then
        B := QueryToUser('Overwrite "' + Item.FileName  + '"? ', Confirm)
      else
        B := QueryToUser('Add "'       + SearchRec.Name + '"? ', Confirm);

      if B = FALSE then
        DoUpdateItem(SearchRec, UpdateAs, Confirm);
    end;
  end;
end;

// --- //

procedure TBxApplication.OpenArchive;
begin
  FArchiver.OpenArchive(FCommandLine.ArchiveName);
  // compression mode
  if swtC in FCommandLine.Options then
    FArchiver.CompressionParams := FCommandLine.SwitchC;
  // check data integrity mode
  if swtCI in FCommandLine.Options then
    FArchiver.CheckParams := FCommandLine.SwitchCI;
  // encryption mode
  if swtE in FCommandLine.Options then
    FArchiver.EncryptionParams := FCommandLine.SwitchE;
  // current layer
  if swtL in FCommandLine.Options then
    FArchiver.NewLayer := FCommandLine.SwitchL = -1;
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
  FArchiver.CloseArchive;
end;

{ shell procedures }

procedure TBxApplication.TagItems;
var
  I, J: longint;
begin
  for I := 0 to FArchiver.Count - 1 do
    for J := 0 to FCommandLine.FileMasks.Count - 1 do
      if FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.FileMasks[J], FCommandLine.SwitchR[J]) then FArchiver.Tag(I);

  for I := 0 to FArchiver.Count - 1 do
    for J := 0 to FCommandLine.SwitchX.Count - 1 do
      if FileNameMatch(FArchiver.Items[I].FileName,
        FCommandLine.SwitchX[J], FCommandLine.SwitchRX[J]) then FArchiver.UnTag(I);
end;

procedure TBxApplication.CustomShell;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoSendMessage(Format(cmScanning, ['...']));

    TagItems;
    case FCommandLine.Command of
      cmdD: FArchiver. DeleteTagged;
      cmdE: FArchiver.ExtractTagged;
      cmdQ: FArchiver.   TestTagged;
      cmdR: FArchiver. RenameTagged;
      cmdT: FArchiver.   TestTagged;
      cmdX: FArchiver.ExtractTagged;
    end;
  end;
  CloseArchive;
end;

procedure TBxApplication.EncodeShell;
var
  Scanner: TFileScanner;
  I: longint;
begin
  OpenArchive;
  if ExitStatus = esNoError then
  begin
    DoSendMessage(Format(cmScanning, ['...']));
    Scanner := TFileScanner.Create;
    for I := 0 to FCommandLine.FileMasks.Count - 1 do
      Scanner.Add(FCommandLine.FileMasks[I], FCommandLine.SwitchR[I]);
    for I := 0 to FCommandLine.SwitchX.Count - 1 do
      Scanner.Delete(FCommandLine.SwitchX[I], FCommandLine.SwitchRX[I]);
    for I := 0 to Scanner.Count - 1 do
      FArchiver.Tag(Scanner.Items[I]);
    FreeAndNil(Scanner);
    FArchiver.UpdateTagged;
  end;
  CloseArchive;
end;

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

procedure TBxApplication.HelpShell;
begin
  DoSendMessage('Usage: BX <command> [<switches>...] <archive-name> [<file-names>...]' + LineEnding);
  DoSendMessage('<Commands>');
  DoSendMessage('  a: add files to archive');
  DoSendMessage('  d: delete files from archive');
  DoSendMessage('  e: extract files from archive');
  DoSendMessage('  h: show this help');
  DoSendMessage('  l: list contents of archive');
  DoSendMessage('  r: rename files in archive');
  DoSendMessage('  t: test integrity of archive files');
  DoSendMessage('  x: extract files from archive with path name');
  DoSendMessage('<Switches>');
  DoSendMessage('  -acc: set archive comment');
  DoSendMessage('  -b: work in Background');
  DoSendMessage('  -c: set compression parameters');
  DoSendMessage('  -cc: set comment');
  DoSendMessage('  -cd: set current archive directory');
  DoSendMessage('  -ci: set check interity parameters');
  DoSendMessage('  -e: set encryption parameters');
  DoSendMessage('  -p: set Password ');
  DoSendMessage('  -r: recurse subdirectories');
  DoSendMessage('  -sfx: create self-extracting archive');
  DoSendMessage('  -sl: show list sorted by filename - for l (list) command');
  DoSendMessage('  -ss: stop switches parsing');
  DoSendMessage('  -t: test temporary archive after process');
  DoSendMessage('  -u: update files method');
  DoSendMessage('  -v: create volumes ');
  DoSendMessage('  -vb: verbose mode ');
  DoSendMessage('  -w: set temporary work directory');
  DoSendMessage('  -x: exclude filenames');
  DoSendMessage('  -y: assume yes on all queries' + LineEnding);
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
    DoSendMessage(Format(cmScanning, ['...']));
    TagItems;

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

    if FCommandLine.SwitchSL then
      ItemToList.Sort(CompareFilePath);

    if FCommandLine.SwitchVB then
    begin
      for I := 0 to ItemToList.Count - 1 do
      begin
        if ExitStatus <> esNoError then Break;
        Item := ItemToList.Items[I];
        DoSendMessage(Format(     '                   Index: %u',       [Item.Index]));
        DoSendMessage(Format(     '                    Name: %s',       [Item.FileName]));
        if Item.UncompressedSize > 0 then
          DoSendMessage(Format(   '       Size/Packed/Ratio: %s/%s/%s', [SizeToStr(Item.UncompressedSize), SizeToStr(Item.CompressedSize),
                                                                       RatioToStr(Item.CompressedSize, Item.UncompressedSize)]));
        DoSendMessage(Format(     '      Last modified time: %s',       [FileTimeToString(Item.LastModifiedTime)]));
        DoSendMessage(Format(     '              Attributes: %s',       [AttrToStr(Item.Attributes)]));
        if Item.Comment <> '' then
          DoSendMessage(Format(   '                 Comment: %s',       [Item.Comment]));
        DoSendMessage(Format(     '    Disk number/position: %u/%u',    [Item.ImageNumber, Item.ImageSeek]));
        if Ord(Item.CheckMethod) <> 0 then
          DoSendMessage(Format(   '     Check method/digest: %s/%s',    [CheckIntegrityMethodTostr(Item.CheckMethod), Item.CheckDigest]));
        if Ord(Item.CheckMethodAux) <> 0 then
          DoSendMessage(Format(   ' Check method/digest aux: %s/%s',    [CheckIntegrityMethodToStr(Item.CheckMethodAux), Item.CheckDigestAux]));
        if Ord(Item.CompressionMethod) <> 0 then
        begin
          DoSendMessage(Format(   'Compression method/block: %s/%u',    [CompressionMethodToStr(Item.CompressionMethod), Item.CompressionBlock]));
          DoSendMessage(Format(   '   Compression level/aux: %u/%u',    [Item.CompressionLevel, Item.CompressionLevelAux]));
          DoSendMessage(Format(   '  Compression filter/aux: %s/%s',    [Item.CompressionFilter, Item.CompressionFilterAux]));
        end;
        if Ord(Item.EncryptionMethod) <> 0 then
          DoSendMessage(Format(   '       Encryption method: %s',       [EncryptionMethodTostr(Item.EncryptionMethod)]));
        DoSendMessage('');
      end;
    end else
    begin
      DoSendMessage('   Date      Time     Attr          Size       Packed Name                 ');
      DoSendMessage(     '---------- -------- ------- ------------ ------------ ---------------------');
      for I := 0 to ItemToList.Count - 1 do
      begin
        if ExitStatus <> esNoError then Break;
        Item := ItemToList.Items[I];
        DoSendMessage(Format('%16s %7s %12s %12s %s', [
          bx_Common.FileTimeToString(Item.LastModifiedTime), AttrToStr(Item.Attributes),
          SizeToStr(Item.UncompressedSize), SizeToStr(Item.CompressedSize), Item.FileName]));
      end;
      DoSendMessage('---------- -------- ------- ------------ ------------ ---------------------');
      DoSendMessage(StringOfChar(' ', 27) + Format(' %12s %12s %d file(s)', [SizeToStr(TotalSize),
        SizeToStr(TotalPackedSize), TotalFiles]));
    end;

    if FArchiver.Comment <> '' then
    begin
      DoSendMessage('   Archive comment');
      DoSendMessage('---------------------------------------------------------------------------');
      DoSendMessage(FArchiver.Comment);
      DoSendMessage('---------------------------------------------------------------------------');
    end;
    DoSendMessage(Format(LineEnding + 'Last modified time: %16s', [bx_Common.FileTimeToString(FArchiver.LastModifiedTime)]));
    ItemToList.Destroy;
  end;
  CloseArchive;
end;

end.
