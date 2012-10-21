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
  Bee_CommandLine,
  Bee_Configuration,
  BeeSDK_Archive;

type
  { TBeeApp class }

  TBeeApp = class(TObject)
  private
    FSelfName: string;
    FCommandLine: TCommandLine;

    procedure DoMessage(const Message: string);

    procedure OnProgress(Value: longint);

    procedure OnFailure(const ErrorMessage: string; ErrorCode: longint);
    procedure OnRename(Item: TArchiveItem; var RenameAs: string; var Confirm: TArchiveConfirm);
    procedure OnExtract(Item: TArchiveItem; var ExtractAs: string; var Confirm: TArchiveConfirm);
    procedure OnErase(Item: TArchiveItem; var Confirm: TArchiveConfirm);
    procedure OnUpdate(SearchRec: TCustomSearchRec; var UpdateAs; var Confirm: TArchiveConfirm);

    procedure OnRequestBlankDisk(var Abort : Boolean);
    procedure OnRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);

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
    procedure Execute;
  end;

implementation

uses
  Math,
  SysUtils,
  Bee_Consts,
  Bee_MainPacker,
  Bee_Assembler;

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
  FSelfName := 'The Bee 0.8.0 build 1561 archiver utility, July 2012' + Cr +
               '(C) 1999-2012 Andrew Filinsky and Melchiorre Caruso';

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

procedure TBeeApp.Execute;
begin
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

  // SetTerminated(True);
end;


procedure TBeeApp.OnProgress(Value: longint);
begin
end;

procedure TBeeApp.DoMessage(const Message: string);
begin
end;

procedure TBeeApp.OnFailure(const ErrorMessage: string; ErrorCode: longint);
begin
end;

procedure TBeeApp.OnRename(Item: TArchiveItem;
  var RenameAs: string; var Confirm: TArchiveConfirm);
begin
end;

procedure TBeeApp.OnExtract(Item: TArchiveItem;
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

procedure TBeeApp.OnErase(Item: TArchiveItem;
  var Confirm: TArchiveConfirm);
begin
end;

procedure TBeeApp.OnUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs; var Confirm: TArchiveConfirm);
begin
  if Updater.Find(CurrentMask) = -1 then
     begin
       case FCommandLine.uOption of
         umAdd:           Updater.Tag(Scanner.Items[I]);
         umAddUpdate:     Updater.Tag(Scanner.Items[I]);
         umAddReplace:    Updater.Tag(Scanner.Items[I]);
         umAddAutoRename: Updater.Tag(Scanner.Items[I]);
       end;
     end else
     begin
       case FCommandLine.uOption of
         umUpdate:        Updater.Tag(Scanner.Items[I]);
         umReplace:       Updater.Tag(Scanner.Items[I]);
         umAddUpdate:     Updater.Tag(Scanner.Items[I]);
         umAddReplace:    Updater.Tag(Scanner.Items[I]);
         umAddAutoRename: Updater.Tag(Scanner.Items[I]);
       end;
     end;
   end;


end;

procedure TBeeApp.OnRequestBlankDisk(var Abort : Boolean);
begin
end;

procedure TBeeApp.OnRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
begin

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
    TestShell;
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
  Updater: TArchiveUpdater;
begin
  Updater := TArchiveUpdater.Create;
  //Updater.OnUpdate           := OnUpdate;
  //Updater.OnProgress         := OnProgress;
  //Updater.OnMessage          := OnMessage;
  //Updater.OnFailure          := OnFailure;
  //Updater.OnRequestImage     := OnRequestImage;
  //Updater.OnRequestBlankDisk := OnRequestBlankDisk;

  DoMessage(Format(cmScanning, ['...']));
  Scanner := TFileScanner.Create;
  with FCommandLine do
    for I := 0 to FileMasks.Count - 1 do
      Scanner.Scan(FileMasks[I], xOptions, rOption);

  for I := 0 to Scanner.Count - 1 do
    Updater.Tag(Scanner.Items[I]);
  Scanner.Free;

  Updater.ArchiveName       := FCommandLine.ArchiveName;
  Updater.ArchivePassword   := FCommandLine.pOption;
  case FCommandLine.mOption of
    moStore: Updater.CompressionMethod := actNone;
    else     Updater.CompressionMethod := actMain;
  end;
  Updater.CompressionLevel  := FCommandLine.mOption;
  Updater.DictionaryLevel   := FCommandLine.dOption;
  Updater.SolidCompression  := FCommandLine.sOption;

  Updater.UpdateTagged;
end;

procedure TBeeApp.DecodeShell;
var
  I: longint;
  Extractor: TArchiveExtractor;
begin
  Extractor := TArchiveExtractor.Create;
  // ...
  Extractor.OnExtraction := OnExtract;



  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FCommandLine.FileMasks.Count - 1 do
    Extractor.Tag(FCommandLine.FileMasks[I], FCommandLine.rOption);

  for I := 0 to FCommandLine.xOptions.Count - 1 do
    Extractor.UnTag(FCommandLine.xOptions[I], FCommandLine.rOption);

  Extractor.ExtractTagged;
end;

procedure TBeeApp.TestShell;
var
  I: longint;
  P: THeader;
  Check: boolean;
  Decoder: THeaderDecoder;
begin
  if (OpenArchive < ccError) and (SetItemsToDecode = TRUE) then
  begin
    Decoder := THeaderDecoder.Create(FArchReader, DoTick);
    Decoder.Password := FCommandLine.pOption;

    Check := True;
    for I := 0  to FHeaders.Count - 1 do
      if ExitCode < ccError then
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

  DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone, haUpdate);
  FHeaders.SetAction(FCommandLine.xOptions,  haUpdate, haNone);



  if (OpenArchive < ccError) and (SetItemsToDelete = TRUE) then
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
        Encoder := THeaderEncoder.Create(FTempWriter, DoTick);
        Encoder.Password := FCommandLine.pOption;

        Check := True;
        for I := 0 to FHeaders.Count - 1 do
          if ExitCode < ccError then
          begin
            P := FHeaders.Items[I];
            Encoder.Initialize(P);

            case P.Action of
              haNone: begin
                DoMessage(Format(cmCopying, [P.Name]));
                Check := Encoder.WriteFromArch(P, FArchReader);
              end;
              haUpdate: DoMessage(Format(cmDeleting, [P.Name]));
              haDecode: begin
                DoMessage(Format(cmEncoding, [P.Name]));
                Check := Encoder.WriteFromSwap(P, FSwapReader);
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
  CloseArchive(FTotalSize > 0);
end;

procedure TBeeApp.RenameShell;
var
  I: longint;
  P: THeader;
  Check: boolean;
  Encoder: THeaderEncoder;
begin

   DoMessage(Format(cmScanning, ['...']));
  FHeaders.SetAction(FCommandLine.FileMasks, haNone, haUpdate);
  FHeaders.SetAction(FCommandLine.xOptions,  haUpdate, haNone);



  if (OpenArchive < ccError) and (SetItemsToRename = TRUE) then
  begin
    FTempName   := GenerateFileName(FCommandLine.wdOption);
    FTempWriter := CreateTFileWriter(FTempName, fmCreate);
    if Assigned(FTempWriter) then
    begin
      FHeaders.Write(FTempWriter);
      Encoder := THeaderEncoder.Create(FTempWriter, DoTick);
      Encoder.Password := FCommandLine.pOption;

      Check := True;
      for I := 0 to FHeaders.Count - 1 do
        if ExitCode < ccError then
        begin
          P := FHeaders.Items[I];

          case P.Action of
            // haDecode:          nothing to do
            // haDecodeAndUpdate: nothing to do
            haNone: begin
              DoMessage(Format(cmCopying, [P.Name]));
              Check := Encoder.WriteFromArch(P, FArchReader);
            end;
            haUpdate: begin
              DoMessage(Format(cmRenaming, [P.Name]));
              Check := Encoder.WriteFromArch(P, FArchReader);
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
  CloseArchive(FTotalSize > 0);
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

  DoMessage(Format(cmScanning, ['...']));
    FHeaders.SetAction(FCommandLine.FileMasks, haNone, haUpdate);
    FHeaders.SetAction(FCommandLine.xOptions,  haUpdate, haNone);


  if (OpenArchive < ccError) and (SetItemsToList = TRUE) then
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
      if ExitCode < ccError then
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
      if ExitCode < ccError then
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
  if (OpenArchive < ccError) and (SetItemsToList = TRUE) then
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

