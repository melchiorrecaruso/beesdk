{
    Copyright (c) 2003-2007 Andrew Filinsky and Melchiorre Caruso

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

  The data compression utility.

  Modifyed:

  v0.1.0 build 0121 - 2007.11.24 by Melchiorre Caruso;
}

unit BeeGui_ZipApp;

{$I compiler.inc}

interface

uses
  Classes,
  ///
  AbUtils,
  AbGZTyp,
  AbArcTyp,
  AbZipTyp,
  AbTarTyp,
  AbZipKit,
  AbSelfEx,
  ///
  Bee_Interface;

type
  TZipApp = class(TApp)
  public
    constructor Create(aAppInterface: TAppInterface; aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; override;
    function Tick: boolean;
  private
    procedure OpenArchive(aAction: TAbArchiveAction);
    procedure CloseArchive;
    ///
    procedure ProcessMasks;
    procedure ProcessOptions;
    procedure ProcesstOption;
    procedure ProcesslOption;
    procedure ProcessaOption;
    ///
    procedure DisplayUsage;
    procedure EncodeShell;
    procedure DecodeShell;
    procedure RenameShell;
    procedure DeleteShell;
    procedure ListShell;
    ///
    procedure ListShell_Zip(aList: TList);
    procedure ListShell_Tar(aList: TList);
    procedure ListShell_GZip(aList: TList);
    ///
    procedure SetPriority(aPriority: integer);
    ///
    function FixArcName(const aArcName: string): string;
    procedure ExpandMask(const Mask: string; Masks: TStringList);
    procedure ScanFileSystem(Mask: string);
    procedure AddItems(const Mask: string);

    /// Events
    procedure OnArchiveProgress(Sender: TObject; Progress: byte; var Abort: boolean);
    procedure OnArchiveSaveProgress(Sender: TObject; Progress: byte; var Abort: boolean);
    procedure OnArchiveItemProgress(Sender: TObject; Item: TAbArchiveItem;
      Progress: byte; var Abort: boolean);

    procedure OnChange(Serder: TObject);

    procedure OnConfirmProcessItem(Sender: TObject; Item: TAbArchiveItem;
      ProcessType: TAbProcessType; var Confirm: boolean);

    procedure OnConfirmOverwrite(var Name: string; var Confirm: boolean);
    procedure OnNeedPassword(Sender: TObject; var NewPassword: string);
    procedure OnProcessItemFailure(Sender: TObject; Item: TAbArchiveItem;
      ProcessType: TAbProcessType; ErrorClass: TAbErrorClass;
      ErrorCode: integer);

    procedure OnConfirmSave(Sender: TObject; var Confirm: boolean);
    procedure OnLoad(Sender: TObject);
    procedure OnRequestBlankDisk(Sender: TObject; var Confirm: boolean);

    procedure OnRequestImage(Sender: TObject; ImageNubmer: integer;
      var ImageName: string; var Confirm: boolean);

    procedure OnRequestLastDisk(Sender: TObject; var Confirm: boolean);
    procedure OnRequestNthDisk(Sender: TObject; DiskNumber: byte;
      var Confirm: boolean);

    procedure OnSave(Sender: TObject);
    
  protected
    /// String sub-routines
    function SizeToStr(Size: integer): string;
    function RatioToStr(PackedSize, Size: integer): string;
    function AttrToStr(Attr: integer): string;
    
  private
    SelfName: string;

    CurItem: TAbArchiveItem;
    CurItemPos: integer;

    ArcName: string;      // archive file name
    Archive: TAbZipKit;
    SelfExe: TAbMakeSelfExe;

    Command: char;        // command
    aOption: string;
    cdOption: string;
    fOption: boolean;
    kOption: boolean;
    lOption: boolean;
    oOption: char;
    rOption: boolean;
    tOption: boolean;
    uOption: boolean;
    xOption: TStringList;

    FileMasks: TStringList; // file masks
  end;

implementation

uses
  Math,
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  SysUtils,       // faReadOnly, ...
  // ---
  Bee_Common;

/// TAbbreviaApp ...

constructor TZipApp.Create;
begin
  inherited Create(aAppInterface, aParams);
  Randomize; // randomize, uses for unique filename generation...

  SelfName := 'The Zip Plugin 0.1.0 build 0121 archiver utility, freeware version, Nov 2007.'
    + Cr + '(C) 2007-2008 Andrew Filinsky and Melchiorre Caruso.';

  // -- //

  Archive := TAbZipKit.Create(nil);
  Archive.CompressionMethodToUse := smDeflated;
  Archive.BaseDirectory := GetCurrentDir;
  Archive.DeflationOption := doNormal;
  Archive.AutoSave := False;

  Archive.OnArchiveProgress := OnArchiveProgress;
  Archive.OnArchiveSaveProgress := OnArchiveSaveProgress;
  Archive.OnArchiveItemProgress := OnArchiveItemProgress;

  Archive.OnChange := OnChange;
  Archive.OnConfirmOverwrite := OnConfirmOverwrite;
  Archive.OnConfirmProcessItem := OnConfirmProcessItem;

  Archive.OnConfirmSave := OnConfirmSave;
  Archive.OnLoad := OnLoad;

  Archive.OnNeedPassword := OnNeedPassword;
  Archive.OnProcessItemFailure := OnProcessItemFailure;

  Archive.OnRequestBlankDisk := OnRequestBlankDisk;
  Archive.OnRequestImage := OnRequestImage;

  Archive.OnRequestLastDisk := OnRequestLastDisk;
  Archive.OnRequestNthDisk := OnRequestNthDisk;
  Archive.OnSave := OnSave;

  CurItem := nil;
  CurItemPos := 0;

  // -- //

  ArcName := '';
  Command := ' ';
  aOption := '';
  cdOption := '';
  fOption := False;
  kOption := False;
  lOption := False;
  oOption := 'Y';
  rOption := False;
  tOption := False;
  uOption := False;
  xOption := TStringList.Create;

  FileMasks := TStringList.Create;

  ProcessOptions; // process options
  ProcessMasks; // process file masks
end;

destructor TZipApp.Destroy;
begin
  if Assigned(Archive) then
  begin
    FreeAndNil(Archive);
  end;
  xOption.Free;
  FileMasks.Free;
  inherited Destroy;
end;

procedure TZipApp.DisplayUsage;
begin
  AppInterface.cMsg := (Cr + '  Usage: Zip <Command> -<Option 1> -<Option N> <ArchiveName> <FileNames...>'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := (Cr + '  Commands:' + Cr); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    a   Add files to archive'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    d   Delete files from archive'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    e   Extract files from archive'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    x   eXtract files from archive with path name'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    l   List archive'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    t   Test archive files'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    r   Rename files in archive'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := (Cr + '  Options:' + Cr); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    r       Recurse subdirectories'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    u       Update files'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    f       Freshen files'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    a       add self-extrActor module'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    o<mode> set overwrite file Mode (Q-Query (default), A-All, S-Skip all)'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    m<0..2> set compression Method (0-store...1-deflate...3-best)'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    d<0..3> set compression level (superfast, fast, normal and maximum)' + Cr); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    x       eXclude filenames'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    t       Test archive after process'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    l       List archive after process'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    y       set temporany directory'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    k       use blowfish crypter/decrypter (min key-length 4 bytes)'); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    cd<dir> set current archive directory' + Cr); Synchronize(AppInterface.OnDisplay);
  AppInterface.cMsg := ('    pri<priority>  set process Priority (0-Idle, 1-Normal, 2-High, 3-RealTime)'); Synchronize(AppInterface.OnDisplay);
end;

procedure TZipApp.Execute;
const
  SetOfCommands = ['A', 'D', 'E', 'L', 'R', 'T', 'X'];
begin
  AppInterface.cMsg := SelfName;
  Synchronize(AppInterface.OnDisplay);

  /// Process Command
  if ((Command in SetOfCommands) and (ArcName > '')) or (Command = '?') then
    case Command of
      'A': EncodeShell;
      'D': DeleteShell;
      'E': DecodeShell;
      'L': ListShell;
      'R': RenameShell;
      'T': DecodeShell;
      'X': DecodeShell;
      '?': DisplayUsage;
    end
  else
    DisplayUsage;
end;

function TZipApp.Tick;
begin
  Result := Terminated;
end;

procedure TZipApp.SetPriority(aPriority: integer); // Priority is 0..3
begin
  {$IFDEF CONSOLEAPPLICATION}
    {$IFDEF MSWINDOWS}
    Bee_Common.SetPriority(aPriority);
    {$ELSE}
    case aPriority of
      0: Priority := tpIdle;
      1: Priority := tpNormal;
      2: Priority := tpHigher;
      3: Priority := tpTimeCritical;
      else
        Priority := tpNormal;
    end;
    {$ENDIF}
  {$ELSE}
  case aPriority of
    0: Priority := tpIdle;
    1: Priority := tpNormal;
    2: Priority := tpHigher;
    3: Priority := tpTimeCritical;
    else
      Priority := tpNormal;
  end;
  {$ENDIF}
end;

procedure TZipApp.OpenArchive(aAction: TAbArchiveAction);
var
  I: integer;
begin
  AppInterface.cMsg := (Cr + msgOpening + 'archive ' + ArcName);
  Synchronize(AppInterface.OnDisplay);
  try
    Archive.OpenArchive(ExpandFileName(ArcName));
    AppInterface.cMsg := (msgScanning + '...');
    Synchronize(AppInterface.OnDisplay);
    for I := 0 to Archive.Count - 1 do
    begin
      with Archive.Items[I] do
      begin
        Action := aAction;
        FileName := Bee_Common.DoDirSeparators(FileName);
      end;
    end;
  except
    AppInterface.cMsg := ('Error: can''t open archive');
    Synchronize(AppInterface.OnFatalError);
  end;
end;

procedure TZipApp.CloseArchive;
begin
  if Assigned(Archive) then
  begin
    Archive.CloseArchive;
  end;
end;

/// Options processing

procedure TZipApp.ProcessOptions;
var
  I: integer;
  S: string;
begin
  // catch options, command, archive name and name of files
  for I := 0 to AppParams.Count - 1 do
  begin
    S := AppParams.Strings[I];
    if (Length(S) > 1) and (S[1] = '-') then
    begin
      // options...
      case UpCase(S[2]) of
        'U': uOption := True;
        'F': fOption := True;
        'T': tOption := True;
        'L': lOption := True;
        'K': kOption := True;
        'R': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 rOption := True
               else
                 if (S = '-') then rOption := False;
             end;
        'Y': begin
               Delete(S, 1, 2);
               if DirectoryExists(Bee_Common.ExcludeTrailingBackslash(S)) then
               begin
                 Archive.TempDirectory := Bee_Common.ExcludeTrailingBackslash(S);
               end;
             end;
        'A': begin
               Delete(S, 1, 2);
               if (S <> '') and FileExists(S) then
               begin
                 aOption := S;
               end;
             end;
        'M': begin
               Delete(S, 1, 2);
               if (Length(S) = 1) and (S[1] in ['0'..'2']) then
               begin
                 case StrToInt(S) of
                   0: Archive.CompressionMethodToUse := smStored;
                   1: Archive.CompressionMethodToUse := smDeflated;
                   2: Archive.CompressionMethodToUse := smBestMethod;
                 end;
               end;
             end;
        'O': begin
               Delete(S, 1, 2);
               if (Length(S) = 1) and (UpCase(S[1]) in ['A', 'S', 'Q']) then
               begin
                 oOption := UpCase(S[1]);
               end;
             end;
        'D': begin
               Delete(S, 1, 2);
               if (Length(S) = 1) and (S[1] in ['0'..'3']) then
               begin
                 case StrToInt(S) of
                   0: Archive.DeflationOption := doSuperFast;
                   1: Archive.DeflationOption := doFast;
                   2: Archive.DeflationOption := doNormal;
                   3: Archive.DeflationOption := doMaximum;
                 end;
               end;
             end;
        'X': begin
               Delete(S, 1, 2);
               xOption.Add(S);
             end;
        else if FileNamePos('-pri', S) = 1 then
             begin
               Delete(S, 1, 4);
               if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
               begin
                 SetPriority(StrToInt(S[1]));
               end;
             end else
             begin
               if FileNamePos('-cd', S) = 1 then
               begin
                 Delete(S, 1, 3);
                 cdOption := Bee_Common.IncludeTrailingBackslash(Bee_Common.FixDirName(S));
               end;
             end;
        end; //end case
    end else
    begin
      // command or filenames...
      if Command = ' ' then
      begin
        if Length(S) = 1 then
          Command := UpCase(S[1])
        else
          Command := '?';
      end else
      if ArcName = '' then
      begin
        ArcName := FixArcName(S);
      end else
        FileMasks.Add(Bee_Common.DoDirSeparators(S));
    end;
  end; // end for loop
end;

procedure TZipApp.ProcessMasks;
var
  I: integer;
begin
  if rOption then
  begin
    for I := 0 to FileMasks.Count - 1 do
    begin
      if System.Pos('!', FileMasks.Strings[I]) = 0 then
      begin
        FileMasks.Strings[I] := FileMasks.Strings[I] + '!';
      end;
    end;

    for I := 0 to xOption.Count - 1 do
    begin
      if System.Pos('!', xOption.Strings[I]) = 0 then
      begin
        xOption.Strings[I] := xOption.Strings[I] + '!';
      end;
    end;
  end;
end;

/// Option processing

procedure TZipApp.ProcesstOption;
begin
  if tOption then
  begin
    xOption.Clear;       // clear xOption
    FileMasks.Clear;     // clear FileMasks
    FileMasks.Add('*!');
    DecodeShell;
  end;
end;

procedure TZipApp.ProcesslOption;
begin
  if lOption then
  begin
    xOption.Clear;       // clear xOption
    FileMasks.Clear;     // clear FileMasks
    FileMasks.Add('*!');
    ListShell;
  end;
end;

procedure TZipApp.ProcessaOption;
begin
  if (Length(aOption) > 0) and Assigned(Archive) then
  begin
    SelfExe := TAbMakeSelfExe.Create(nil);
    SelfExe.StubExe := 'ZipSfx.bin';
    SelfExe.ZipFile := ArcName;
    if SelfExe.Execute then
    begin
      // todo
    end;
  end;
end;

/// Shell procedures

procedure TZipApp.EncodeShell;
var
  I: integer;
  Time: double;
  ItemIndex: integer;
  Item: TAbArchiveItem;
  Options: TAbStoreOptions;
begin
  Time := Now;
  OpenArchive(aaCopy);
  if Archive.Status = asIdle then
  begin
    Archive.StoreOptions := [soStripDrive, soRemoveDots];
    for I := 0 to FileMasks.Count - 1 do
    begin
      AddItems(FileMasks.Strings[I]);
    end;

    if (Archive.ZipArchive.IsDirty = False) and (aOption = '') then
    begin
      AppInterface.cMsg := ('Warning: no files to process');
      Synchronize(AppInterface.OnWarning);
    end else
      Archive.CloseArchive;

    Synchronize(AppInterface.OnClear);
    if not Terminated then
    begin
      AppInterface.cMsg := (Cr + 'Archive size ' + SizeToStr(
        SizeOfFile(ArcName)) + ' bytes - ' + TimeDifference(Time) + ' seconds');
      Synchronize(AppInterface.OnDisplay);
      // -- //
      ProcesstOption;
      ProcesslOption;
      ProcessaOption;
    end else
    begin
      AppInterface.cMsg := (Cr + 'Process aborted - ' +
        TimeDifference(Time) + ' seconds');
      Synchronize(AppInterface.OnDisplay);
    end;
  end;
end;

procedure TZipApp.DecodeShell;
var
  I: integer;
  Time: double;
begin
  if FileExists(ArcName) then
  begin
    Time := Now;
    OpenArchive(aaNone);
    if Archive.Status = asIdle then
    begin
      Archive.ClearTags;
      for I := 0 to Archive.Count - 1 do
      begin
        if Bee_Common.FileNameMatch(Archive.Items[I].FileName, FileMasks) then
          if Bee_Common.FileNameMatch(Archive.Items[I].FileName, xOption) = False then
          begin
            Archive.Items[I].Tagged := True;
          end;
      end;

      Archive.ExtractOptions := [eoCreateDirs, eoRestorePath];
      case Command of
        'X': if Length(cdOption) > 0 then
          begin
            for I := 0 to Archive.Count - 1 do
              with Archive.Items[I] do
                FileName := Bee_Common.DeleteFilePath(cdOption, FileName);
          end;

        'E': for I := 0 to Archive.Count - 1 do
          begin
            with Archive.Items[I] do
              FileName := ExtractFileName(FileName);
          end;
      end;

      case Command of
        'T': Archive.TestTaggedItems;
        'E': Archive.ExtractTaggedItems;
        'X': Archive.ExtractTaggedItems;
      end;
      Archive.CloseArchive;
      
      Synchronize(AppInterface.OnClear);
      if not Terminated then
      begin
        AppInterface.cMsg :=
          (Cr + 'Archive size ' + SizeToStr(SizeOfFile(ArcName)) +
          ' bytes - ' + TimeDifference(Time) + ' seconds');
        Synchronize(AppInterface.OnDisplay);
      end else
      begin
        AppInterface.cMsg :=
          (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Synchronize(AppInterface.OnDisplay);
      end;
    end;
  end;
end;

procedure TZipApp.DeleteShell;
var
  I: integer;
  Time: double;
begin
  if FileExists(ArcName) then
  begin
    Time := Now;
    OpenArchive(aaCopy);
    if Archive.Status = asIdle then
    begin
      for I := 0 to Archive.Count - 1 do
      begin
        if Bee_Common.FileNameMatch(Archive.Items[I].FileName, FileMasks) then
          if Bee_Common.FileNameMatch(Archive.Items[I].FileName, xOption) = False then
          begin
            Archive.Items[I].Action := aaDelete;
            Archive.ZipArchive.IsDirty := True;
          end;
      end;

      if (Archive.ZipArchive.IsDirty = False) and (aOption = '') then
      begin
        AppInterface.cMsg := ('Warning: no files to process');
        Synchronize(AppInterface.OnWarning);
      end else
        Archive.CloseArchive;

      Synchronize(AppInterface.OnClear);
      if not Terminated then
      begin
        AppInterface.cMsg :=
          (Cr + 'Archive size ' + SizeToStr(SizeOfFile(ArcName)) +
          ' bytes - ' + TimeDifference(Time) + ' seconds');
        Synchronize(AppInterface.OnDisplay);
        // -- //
        ProcesstOption;
        ProcesslOption;
        ProcessaOption;
      end else
      begin
        AppInterface.cMsg :=
          (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Synchronize(AppInterface.OnDisplay);
      end;
    end;
  end;
end;

procedure TZipApp.RenameShell;
var
  I: integer;
  Time: double;
  NewFileName: string;
begin
  if FileExists(ArcName) then
  begin
    Time := Now;
    OpenArchive(aaCopy);
    if Archive.Status = asIdle then
    begin
      for I := 0 to Archive.Count - 1 do
      begin
        if Bee_Common.FileNameMatch(Archive.Items[I].FileName, FileMasks) then
          if Bee_Common.FileNameMatch(Archive.Items[I].FileName, xOption) = False then
          begin
            Archive.Items[I].Action := aaRename;
          end;
      end;

      for I := 0 to Archive.Count - 1 do
      begin
        if Archive.Items[I].Action = aaRename then
        begin
          while True do
          begin
            AppInterface.cFileName := Archive.Items[I].FileName;
            AppInterface.cFileSize := Archive.Items[I].UncompressedSize;
            AppInterface.cFileTime :=
              DateTimeToFileDate(Archive.Items[I].LastModTimeAsDateTime);
            AppInterface.cMsg := '';

            Synchronize(AppInterface.OnRename);

            NewFileName := Bee_Common.FixFileName(AppInterface.cMsg);
            if Archive.FindFile(NewFileName) <> -1 then
            begin
              AppInterface.cMsg :=
                ('File "' + NewFileName + '" already existing in archive!');
              Synchronize(AppInterface.OnWarning);
            end else
              Break;
          end;

          if Length(NewFileName) > 0 then
          begin
            Archive.Items[I].FileName  := NewFileName;
            Archive.Items[I].DiskFileName := NewFileName;
            Archive.ZipArchive.IsDirty := True;
          end;
        end;
      end;
      Archive.CloseArchive;

      Synchronize(AppInterface.OnClear);
      if not Terminated then
      begin
        AppInterface.cMsg :=
          (Cr + 'Archive size ' + SizeToStr(SizeOfFile(ArcName)) +
          ' bytes - ' + TimeDifference(Time) + ' seconds');
        Synchronize(AppInterface.OnDisplay);
        // -- //
        ProcesstOption;
        ProcesslOption;
        ProcessaOption;
      end else
      begin
        AppInterface.cMsg :=
          (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Synchronize(AppInterface.OnDisplay);
      end;
    end;
  end;
end;

{$IFDEF CONSOLEAPPLICATION}

procedure TZipApp.ListShell;
var
  Time: double;
  I: integer;
begin
  if FileExists(ArcName) then
  begin
    Time := Now;
    OpenArchive(aaNone);
    if Archive.Status = asIdle then
    begin
      Archive.ClearTags;
      for I := 0 to Archive.Count - 1 do
      begin
        if Bee_Common.FileNameMatch(Archive.Items[I].FileName, FileMasks) then
          if Bee_Common.FileNameMatch(Archive.Items[I].FileName, xOption) = False then
          begin
            Archive.Items[I].Tagged := True;
          end;
      end;

      for I := 0 to Archive.Count - 1 do
      begin
        if Archive.Items[I].Tagged then
        begin
          case Archive.ArchiveType of
            atZip: AppInterface.cMsg  := TAbZipItem(Archive.Items[I]).FileName;
            atSelfExtZip: AppInterface.cMsg := TAbZipItem(Archive.Items[I]).FileName;
            atTar: AppInterface.cMsg  := TAbTarItem(Archive.Items[I]).FileName;
            atGZip: AppInterface.cMsg := TAbGZipItem(Archive.Items[I]).FileName;
            atGZippedTar: AppInterface.cMsg := TAbGZipItem(Archive.Items[I]).FileName;
            else
              AppInterface.cMsg := 'Unknow';
          end;
          Synchronize(AppInterface.OnDisplay);
        end;
      end;
      Archive.CloseArchive;

      Synchronize(AppInterface.OnClear);
      if not Terminated then
      begin
        AppInterface.cMsg :=
          (Cr + 'Archive size ' + SizeToStr(SizeOfFile(ArcName)) +
          ' bytes - ' + TimeDifference(Time) + ' seconds');
        Synchronize(AppInterface.OnDisplay);
      end else
      begin
        AppInterface.cMsg :=
          (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Synchronize(AppInterface.OnDisplay);
      end;
    end;
  end;
end;

{$ELSE}

procedure TZipApp.ListShell;
var
  I: integer;
  Time: double;
  Node: TAppItem;
begin
  AppInterface.cList := nil;
  Synchronize(AppInterface.OnList);
  if Assigned(AppInterface.cList) then
  begin
    if FileExists(ArcName) then
    begin
      Time := Now;
      OpenArchive(aaNone);
      if Archive.Status = asIdle then
      begin
        AppInterface.cList.Clear;
        case Archive.ArchiveType of
          atZip:        ListShell_Zip (AppInterface.cList);
          atSelfExtZip: ListShell_Zip (AppInterface.cList);
          atTar:        ListShell_Tar (AppInterface.cList);
          atGZip:       ListShell_GZip(AppInterface.cList);
          atGZippedTar: ListShell_GZip(AppInterface.cList);
        end;
        Archive.CloseArchive;

        Synchronize(AppInterface.OnClear);
        if not Terminated then
        begin
          AppInterface.cMsg :=
            (Cr + 'Archive size ' + SizeToStr(SizeOfFile(ArcName)) +
            ' bytes - ' + TimeDifference(Time) + ' seconds');
          Synchronize(AppInterface.OnDisplay);
        end else
        begin
          AppInterface.cMsg :=
            (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
          Synchronize(AppInterface.OnDisplay);
        end;
      end;
    end;
  end;
end;

{$ENDIF}

procedure TZipApp.ListShell_Zip(aList: TList);
var
  I: integer;
  Node: TAppItem;
begin
  for I := 0 to Archive.Count - 1 do
    with TAbZipItem(Archive.Items[I]) do
      if (ExternalFileAttributes and faDirectory) = 0 then
      begin
        Node := TAppItem.Create;

        Node.FileName := ExtractFileName(Bee_Common.DoDirSeparators(FileName));

        Node.FilePath := ExtractFilePath(Bee_Common.DoDirSeparators(FileName));

        Node.FileSize  := UncompressedSize;
        Node.FilePacked := CompressedSize;
        Node.FileRatio := Round(CompressionRatio);
        Node.FileAttr  := ExternalFileAttributes;
        Node.FileTime  := DateTimeToFileDate(LastModTimeAsDateTime);

        Node.FileComm := FileComment;
        Node.FileCrc  := CRC32;

        case CompressionMethod of
          cmStored: Node.FileMethod := 'Stored';
          cmDeflated: Node.FileMethod := 'Deflated';
          cmBestMethod: Node.FileMethod := 'BestMethod';
          else
            Node.FileMethod := 'Unknow';
        end;
        Node.FileVersion := IntToStr(VersionNeededToExtract);

        if IsEncrypted then
          Node.FilePassword := 'Yes'
        else
          Node.FilePassword := 'No';

        Node.FilePosition := RelativeOffset;
        Node.FileIcon := -1;

        aList.Add(Node);
      end;
end;

procedure TZipApp.ListShell_Tar(aList: TList);
var
  I: integer;
  Node: TAppItem;
begin
  for I := 0 to Archive.Count - 1 do
  begin
    Node := TAppItem.Create;
    with TAbTarItem(Archive.Items[I]) do
    begin
      Node.FileName := ExtractFileName(Bee_Common.DoDirSeparators(FileName));
      Node.FilePath := ExtractFilePath(Bee_Common.DoDirSeparators(FileName));

      Node.FileSize := UncompressedSize;
      Node.FilePacked := CompressedSize;

      if Node.FileSize = 0 then
        Node.FileRatio := 0
      else
        Node.FileRatio := MulDiv(Node.FilePacked, 100, Node.FileSize);

      Node.FileAttr := 0;
      Node.FileTime := DateTimeToFileDate(LastModTimeAsDateTime);

      Node.FileComm := '';
      Node.FileCrc  := CRC32;

      if Node.FileRatio = 100 then
        Node.FileMethod := 'Stored'
      else
        Node.FileMethod := 'Unknow';

      Node.FileVersion := '0000';

      if IsEncrypted then
        Node.FilePassword := 'Yes'
      else
        Node.FilePassword := 'No';

      Node.FilePosition := 0;
      Node.FileIcon := -1;
    end;
    aList.Add(Node);
  end;
end;

procedure TZipApp.ListShell_GZip(aList: TList);
var
  I: integer;
  Node: TAppItem;
begin
  for I := 0 to Archive.Count - 1 do
  begin
    Node := TAppItem.Create;
    with TAbGZipItem(Archive.Items[I]) do
    begin
      Node.FileName := ExtractFileName(FileName);
      Node.FilePath := ExtractFilePath(FileName);

      Node.FileSize := UncompressedSize;
      Node.FilePacked := CompressedSize;

      if Node.FileSize = 0 then
        Node.FileRatio := 0
      else
        Node.FileRatio := MulDiv(Node.FilePacked, 100, Node.FileSize);

      Node.FileAttr := ExternalFileAttributes;
      Node.FileTime := DateTimeToFileDate(LastModTimeAsDateTime);
    end;
  end;
end;

// ----------------------------------------------------------------------- //
//                                                                         //
//                                                                         //
//                                                                         //
//                                                                         //
// ----------------------------------------------------------------------- //


function TZipApp.FixArcName(const aArcName: string): string;
var
  FExt: string;
begin
  FExt := ExtractFileExt(aArcName);
  if (AnsiCompareText(FExt, '.zip') = 0) or
     (AnsiCompareText(FExt, '.tar') = 0) or
     (AnsiCompareText(FExt, '.tgz') = 0) or
     (AnsiCompareText(FExt, '.gz' ) = 0) then
  begin
    Result := aArcName;
  end else
  begin
    Result := aArcName + '.zip';
  end;
end;

procedure TZipApp.AddItems(const Mask: string);
var
  I: integer;
  Masks: TStringList;
begin
  Masks  := TStringList.Create;
  ExpandMask(Mask, Masks);
  for I := 0 to Masks.Count - 1 do
  begin
    ScanFileSystem(Masks.Strings[I]);
  end;
  Masks.Free;
end;

procedure TZipApp.ExpandMask(const Mask: string; Masks: TStringList);
var
  I: integer;
  Error: integer;
  Rec: TSearchRec;
  Card: boolean;
  LastSlash: integer;
  FirstSlash: integer;
  FolderName: string;
  FolderPath: string;
begin
  Card := False;
  LastSlash := 0;
  FirstSlash := 0;
  for I := 1 to Length(Mask) do
  begin
    if Card = False then
    begin
      if Mask[I] in ['*', '?'] then
      begin
        Card := True;
      end;
      if Mask[I] = PathDelim then
      begin
        FirstSlash := I;
      end;
    end else
    begin
      if Mask[I] = PathDelim then
      begin
        LastSlash := I;
        Break;
      end;
    end;
  end;
  
  if LastSlash > 0 then
  begin
    FolderPath := Copy(Mask, 1, FirstSlash);
    FolderName := Copy(Mask, FirstSlash + 1, LastSlash - (FirstSlash + 1));
    Error := FindFirst(FolderPath + '*', faAnyFile, Rec);
    while Error = 0 do
    begin
      if ((Rec.Attr and faDirectory) > 0) and (Rec.Name[1] <> '.') then
      begin
        if FileNameMatch(Rec.Name, FolderName) then
        begin
          ExpandMask(FolderPath + Rec.Name +
            Copy(Mask, LastSlash, (Length(Mask) + 1) - LastSlash), Masks);
        end;
      end;
      Error := FindNext(Rec);
    end;
    FindClose(Rec);
  end else
  begin
    Masks.Add(Mask);
  end;
end;

procedure TZipApp.ScanFileSystem(Mask: string);
var
  Recursive: boolean;
  I, J: integer;
  Error: integer;
  Rec:  TSearchRec;
  RecName: string;
  RecPath: string;
  Item: TAbArchiveItem;
begin
  I := System.Pos('!', Mask);
  if I > 0 then
  begin
    Recursive := True;
    repeat
      System.Delete(Mask, I, 1);
      I := System.Pos('!', Mask);
    until I = 0;
  end else
    Recursive := False;

  if (Length(Mask) > 0) and (Mask[Length(Mask)] = PathDelim) then
  begin
    Mask := Bee_Common.IncludeTrailingBackSlash(Mask) + '*';
    Recursive := True;
  end else
    if Bee_Common.DirectoryExists(Mask) then
    begin
      Mask := Bee_Common.IncludeTrailingBackSlash(Mask) + '*';
      Recursive := True;
    end;

  RecPath := ExtractFilePath(Mask);
  Error := FindFirst(RecPath + '*', faAnyFile, Rec);
  while Error = 0 do
  begin
    RecName := RecPath + Rec.Name;
    if (Rec.Attr and faDirectory) = 0 then
    begin
      if (Bee_Common.FileNameMatch(RecName, Mask)) then
        if (Bee_Common.FileNameMatch(RecName, xOption) = False) then
        begin
          I := Archive.FindFile(cdOption + Bee_Common.DeleteFileDrive(RecName));
          if not (fOption xor uOption) then
          begin
            if I = -1 then
            begin
              Item := Archive.ZipArchive.CreateItem(RecName);
              Item.FileName := cdOption + RecName;
              Archive.ZipArchive.Add(Item);
            end else
            begin
              if Rec.Time > DateTimeToFileDate(
                Archive.Items[I].LastModTimeAsDateTime) then
              begin
                Archive.Items[I].DiskFileName := RecName;
                Archive.Items[I].Action := aaFreshen;
                Archive.ZipArchive.IsDirty := True;
              end;
            end;
          end else
          begin
            if uOption then
            begin
              I := Archive.FindFile(cdOption + RecName);
              if I = -1 then
              begin
                Item := Archive.ZipArchive.CreateItem(RecName);
                Item.FileName := cdOption + RecName;
                Archive.ZipArchive.Add(Item);
              end;
            end else
            begin
              I := Archive.FindFile(cdOption + RecName);
              if (I <> -1) and (Rec.Time > DateTimeToFileDate(
                Archive.Items[I].LastModTimeAsDateTime)) then
              begin
                Archive.Items[I].DiskFileName := RecName;
                Archive.Items[I].Action := aaFreshen;
                Archive.ZipArchive.IsDirty := True;
              end;
            end;
          end;
        end;
    end else
    begin
      if Recursive and (Rec.Name <> '.') and (Rec.Name <> '..')then
      begin
        ScanFileSystem(Bee_Common.IncludeTrailingBackSlash(RecName) +
          ExtractFileName(Mask) + '!');
      end;
    end;
    Error := FindNext(Rec);
  end;
  FindClose(Rec);
end;

 // ----------------------------------------------------------------------- //
 //                                                                         //
 //                                                                         //
 //                                                                         //
 //                                                                         //
 // ----------------------------------------------------------------------- //

procedure TZipApp.OnArchiveProgress(Sender: TObject; Progress: byte;
  var Abort: boolean);
begin
end;

procedure TZipApp.OnArchiveSaveProgress(Sender: TObject; Progress: byte;
  var Abort: boolean);
begin
end;

procedure TZipApp.OnArchiveItemProgress(Sender: TObject; Item: TAbArchiveItem;
  Progress: byte; var Abort: boolean);
begin
  if (CurItem <> Item) then
  begin
    CurItem := Item;
    case Item.Action of
      aaFailed:    AppInterface.cMsg := (msgFailed + Item.FileName);
      aaAdd:       AppInterface.cMsg := (msgAdding + Item.FileName);
      aaDelete:    AppInterface.cMsg := (msgDeleting + Item.FileName);
      aaFreshen:   AppInterface.cMsg := (msgFreshing + Item.FileName);
      aaMove:      AppInterface.cMsg := (msgMoving + Item.FileName);
      aaCopy:      AppInterface.cMsg := (msgCopying + Item.FileName);
      aaReplace:   AppInterface.cMsg := (msgReplacing + Item.FileName);
      aaStreamAdd: AppInterface.cMsg := (msgAdding + Item.FileName);
      aaRename: AppInterface.cMsg := (msgRenaming + Item.FileName);
      aaNone: case Command of
          'T': AppInterface.cMsg := (msgTesting + Item.FileName);
          'E': AppInterface.cMsg := (msgExtracting + Item.FileName);
          'X': AppInterface.cMsg := (msgExtracting + Item.FileName);
          else
            AppInterface.cMsg := 'Unknown message';
        end;
    end;
    Synchronize(AppInterface.OnClear);
    Synchronize(AppInterface.OnDisplay);

    AppInterface.cPercentage := MulDiv(100, CurItemPos, Archive.Count);
    Synchronize(AppInterface.OnTick);
    Inc(CurItemPos);
  end;
  Abort := Tick;
end;

procedure TZipApp.OnChange(Serder: TObject);
begin
end;

procedure TZipApp.OnConfirmProcessItem(Sender: TObject; Item: TAbArchiveItem;
  ProcessType: TAbProcessType; var Confirm: boolean);
begin
  Confirm := True;
end;

procedure TZipApp.OnConfirmOverwrite(var Name: string; var Confirm: boolean);
begin
  Confirm := False;
  case oOption of
    'A': Confirm := True;
    'S': Confirm := False;
    'Q': Terminate;
    else
    begin
      AppInterface.cFileName := Name;
      AppInterface.cFileSize := 0;
      AppInterface.cFileTime := 0;

      Synchronize(AppInterface.OnOverWrite);
      if Length(AppInterface.cMsg) = 1 then
      begin
        oOption := AppInterface.cMsg[1];
      end;

      case oOption of
        'Y': Confirm := True;
        'N': Confirm := False;
        'A': Confirm := True;
        'S': Confirm := False;
        'Q': Terminate;
      end;
    end;
  end;
end;

procedure TZipApp.OnNeedPassword(Sender: TObject; var NewPassword: string);
begin
  Synchronize(AppInterface.OnKey);
  NewPassword := AppInterface.cMsg;
end;

procedure TZipApp.OnProcessItemFailure(Sender: TObject; Item: TAbArchiveItem;
  ProcessType: TAbProcessType; ErrorClass: TAbErrorClass; ErrorCode: integer);
begin
  AppInterface.cMsg := IntToStr(ErrorCode);
  Synchronize(AppInterface.OnError);
end;

procedure TZipApp.OnConfirmSave(Sender: TObject; var Confirm: boolean);
begin
  Confirm := True;
end;

procedure TZipApp.OnLoad(Sender: TObject);
begin
end;

procedure TZipApp.OnRequestBlankDisk(Sender: TObject; var Confirm: boolean);
begin
  Confirm := True;
end;

procedure TZipApp.OnRequestImage(Sender: TObject; ImageNubmer: integer;
  var ImageName: string; var Confirm: boolean);
begin
end;

procedure TZipApp.OnRequestLastDisk(Sender: TObject; var Confirm: boolean);
begin
end;

procedure TZipApp.OnRequestNthDisk(Sender: TObject; DiskNumber: byte;
  var Confirm: boolean);
begin
end;

procedure TZipApp.OnSave(Sender: TObject);
begin
end;

// string sub-routines

function TZipApp.SizeToStr(Size: integer): string;
begin
  Result := Format('%u', [Size]);
end;

function TZipApp.RatioToStr(PackedSize, Size: integer): string;
begin
  if Size > 0 then
    Result := Format('%u%%', [MulDiv(PackedSize, 100, Size)])
  else
    Result := Format('%u%%', [100]);
end;

function TZipApp.AttrToStr(Attr: integer): string;
begin
  Result := '..RHSA';
  if Attr and faReadOnly = 0 then Result[3] := '.';
  if Attr and faHidden   = 0 then Result[4] := '.';
  if Attr and faSysFile  = 0 then Result[5] := '.';
  if Attr and faArchive  = 0 then Result[6] := '.';
end;

end.
