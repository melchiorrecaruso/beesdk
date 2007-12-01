(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

 {*********************************************************}
 {* ABBREVIA: AbCabTyp.pas 3.05                           *}
 {*********************************************************}
 {* ABBREVIA: Cabinet Archive                             *}
 {* Based on info from the FCI/FDI Library Description,   *}
 {* included in the Microsoft Cabinet SDK                 *}
 {*********************************************************}

{$I AbDefine.inc}

unit AbCabTyp;

interface

uses
  Windows, SysUtils, Classes, AbFciFdi, AbArcTyp,
  AbUtils;

type
  TAbCabItem = class(TAbArchiveItem)
  protected {private}
    FPartialFile: boolean;
  public
    property PartialFile: boolean Read FPartialFile Write FPartialFile;
  end;

type
  TAbCabCompressionType = (ctNone, ctMSZIP, ctLZX);
  TAbCabinetMode = (cmRead, cmWrite);
  TAbCabStatus = (csFile, csFolder, csCabinet);


const
  faExtractAndExecute = $040;
  AbDefCabSpanningThreshold = 0;
  AbDefFolderThreshold = 0;
  AbDefCompressionType = ctMSZIP;
  AbDefReserveHeaderSize = 0;
  AbDefReserveFolderSize = 0;
  AbDefReserveDataSize = 0;
  AbDefLZXWindowSize = 18;

  CompressionTypeMap: array[TAbCabCompressionType] of word = (0, 1, 4611);

type
  TAbCabArchive = class(TAbArchive)
  protected {private}
    {internal variables}
    FCabName: array[0..255] of char;
    FCabPath: array[0..255] of char;
    FFCICabInfo: FCICabInfo;
    FFCIContext: HFCI;
    FFDIContext: HFDI;
    FFDICabInfo: FDICabInfo;
    FErrors:  CabErrorRecord;
    FFileHandle: integer;
    FItemInProgress: TAbCabItem;
    FIIPName: string;
    FItemProgress: DWord;
    FNextCabinet: string;
    FNextDisk: string;
    FTempFileID: integer;

    {property variables}
    FCurrentCab: word;
    FCabSize: longint;
    FCompressionType: TAbCabCompressionType;
    FFileCount: word;
    FFolderThreshold: longint;
    FFolderCount: word;
    FHasPrev: boolean;
    FHasNext: boolean;
    FSetID: word;

    {internal methods}
    procedure CloseCabFile;
    procedure CreateCabFile;
    function CreateItem(const FileSpec: string): TAbArchiveItem;
      override;
    procedure DoCabItemProcessed;
    procedure DoCabItemProgress(BytesCompressed: DWord; var Abort: boolean);
    procedure DoConfirmOverwrite(var FileName: string; var Confirm: boolean);
      virtual;
    procedure DoGetNextCabinet(CabIndex: integer; var CabName: string;
      var Abort: boolean);
    procedure ExtractItemAt(Index: integer; const NewName: string);
      override;
    procedure ExtractItemToStreamAt(Index: integer; OutStream: TStream);
      override;
    function GetItem(ItemIndex: integer): TAbCabItem;
    procedure LoadArchive;
      override;
    procedure OpenCabFile;
    procedure PutItem(Index: integer; Value: TAbCabItem);
    procedure SaveArchive;
      override;
    procedure SetFolderThreshold(Value: longint);
    procedure SetSetID(Value: word);
    procedure SetSpanningThreshold(Value: longint);
      override;
    procedure TestItemAt(Index: integer);
      override;

  public {methods}
    constructor Create(const FileName: string; Mode: word);
      override;
    destructor Destroy;
      override;
    procedure Add(aItem: TAbArchiveItem);
      override;
    procedure NewCabinet;
    procedure NewFolder;

  public {properties}
    property CurrentCab: word Read FCurrentCab;
    property CabSize: longint Read FCabSize;
    property CompressionType: TAbCabCompressionType
      Read FCompressionType Write FCompressionType;
    property FolderThreshold: longint Read FFolderThreshold Write SetFolderThreshold;
    property FolderCount: word Read FFolderCount;
    property HasPrev: boolean Read FHasPrev;
    property HasNext: boolean Read FHasNext;
    property Items[Index: integer]: TAbCabItem Read GetItem Write PutItem; default;
    property ItemProgress: DWord Read FItemProgress Write FItemProgress;
    property SetID: word Read FSetID Write SetSetID;
  end;

function VerifyCab(const Fn: string): TAbArchiveType;

implementation

uses
  AbExcept;

type
  PWord = ^word;
  PInteger = ^integer;

function VerifyCab(const Fn: string): TAbArchiveType;
var
  CabArchive: TAbCabArchive;
begin
  Result := atCab;
  CabArchive := TAbCabArchive.Create(Fn, fmOpenRead or fmShareDenyNone);
  try
    try
      CabArchive.LoadArchive;
    except
      on EAbFDICreateError do
        Result := atUnknown;
    end;
  finally
    CabArchive.Free;
  end;
end;


{ == FDI/FCI Callback Functions - cdecl calling convention ================= }
function FXI_GetMem(uBytes: integer): Pointer;
  cdecl;
  {allocate memory}
begin
  Result := nil;
  if (uBytes > 0) then
    GetMem(Result, uBytes);
end;

{ -------------------------------------------------------------------------- }
procedure FXI_FreeMem(lpBuffer: Pointer);
  cdecl;
{free memory}
begin
  Dispose(lpBuffer);
end;


{ == FCI Callback Functions - cdecl calling convention ===================== }
function FCI_FileOpen(lpPathName: PChar; Flag, Mode: integer;
  PError: PInteger; Archive: TAbCabArchive): HFILE;
  cdecl;
  {open a file}
begin
  Result := _lcreat(lpPathName, 0);
  if (Result = HFILE_ERROR) then
    raise EAbFCIFileOpenError.Create;
end;

{ -------------------------------------------------------------------------- }
function FCI_FileRead(hFile: HFILE; lpBuffer: Pointer; uBytes: UINT;
  PError: PInteger; Archive: TAbCabArchive): HFILE;
  cdecl;
  {read from a file}
begin
  Result := _lread(hFile, lpBuffer, uBytes);
  if (Result = HFILE_ERROR) then
    raise EAbFCIFileReadError.Create;
end;

{ -------------------------------------------------------------------------- }
function FCI_FileWrite(hFile: HFILE; lpBuffer: Pointer; uBytes: UINT;
  PError: PInteger; Archive: TAbCabArchive): HFILE;
  cdecl;
  {write to a file}
begin
  Result := _lwrite(hFile, lpBuffer, uBytes);
  if (Result = HFILE_ERROR) then
    raise EAbFCIFileWriteError.Create;
end;

{ -------------------------------------------------------------------------- }
function FCI_FileClose(hFile: HFILE; PError: PInteger; Archive: TAbCabArchive): HFILE;
  cdecl;
  {close a file}
begin
  Result := _lclose(hFile);
  if (Result = HFILE_ERROR) then
    raise EAbFCIFileCloseError.Create;
end;

{ -------------------------------------------------------------------------- }
function FCI_FileSeek(hFile: HFILE; Offset: longint; Origin: integer;
  PError: PInteger; Archive: TAbCabArchive): longint;
  cdecl;
  {reposition file pointer}
begin
  Result := _llseek(hFile, Offset, Origin);
  if (Result = -1) then
    raise EAbFCIFileSeekError.Create;
end;

{ -------------------------------------------------------------------------- }
function FCI_FileDelete(lpFilename: PChar; PError: PInteger;
  Archive: TAbCabArchive): boolean;
  cdecl;
  {delete a file}
begin
  Result := SysUtils.DeleteFile(StrPas(lpFilename));
  if (Result = False) then
    raise EAbFCIFileDeleteError.Create;
end;

{ -------------------------------------------------------------------------- }
function FCI_GetNextCab(lpCCab: PFCICabInfo; PrevCab: longint;
  Archive: TAbCabArchive): boolean;
  cdecl;
  {get next cabinet filename}
var
  CabName: string;
  Abort: boolean;
begin
  Abort := False;
  with lpCCab^ do
  begin
    CabName := StrPas(szCab);                                            {!!.02}
    {obtain next cabinet.  Make index zero-based}
    Archive.DoGetNextCabinet(Pred(iCab), CabName, Abort);
    if not Abort then
      StrPCopy(szCab, CabName);                                          {!!.02}
  end;
  Result := not Abort;
end;

{ -------------------------------------------------------------------------- }
function FCI_FileDest(PCCab: PFCICabInfo; PFilename: PChar; cbFile: longint;
  Continuation: boolean; Archive: TAbCabArchive): integer;
  cdecl;
  {currently not used}
begin
  Result := 0;
end;

{ -------------------------------------------------------------------------- }
function FCI_GetOpenInfo(lpPathname: PChar; PDate, PTime, PAttribs: PWord;
  PError: PInteger; Archive: TAbCabArchive): integer;
  cdecl;
  {open a file and return date/attributes}
var
  DT: integer;
begin
  Result := _lopen(lpPathname, OF_READ or OF_SHARE_DENY_NONE);
  if (Result = -1) then
    raise EAbFCIFileOpenError.Create;
  PAttribs^ := AbFileGetAttr(StrPas(lpPathname));
  DT := FileGetDate(Result);
  PDate^ := DT shr 16;
  PTime^ := DT and $0FFFF;
  Archive.ItemProgress := 0;
end;

{ -------------------------------------------------------------------------- }
function FCI_Status(Status: word; cb1, cb2: DWord;
  Archive: TAbCabArchive): longint; cdecl;
  {keep archive informed}
var
  Abort: boolean;
begin
  Result := 0;
  if (Status = word(csCabinet)) then
  begin
    Archive.DoSave;
    Archive.FCabSize := cb2;
    Result := cb2;
  end else if (Status = word(csFolder)) then
    Archive.FCabSize := Archive.FCabSize + longint(cb2)
  else if (Status = word(csFile)) then
  begin
    Archive.DoCabItemProgress(cb2, Abort);
    Result := longint(Abort);
  end;
end;

{ -------------------------------------------------------------------------- }
function FCI_GetTempFile(lpTempName: PChar; TempNameSize: integer;
  Archive: TAbCabArchive): integer; cdecl;
  {obtain temporary filename}
var
  TempPath: array[0..255] of char;
  Prefix: array[0..10] of char;
begin
  Archive.FTempFileID := Archive.FTempFileID + 1;
  StrPCopy(Prefix, 'VMS');
  if (Archive.TempDirectory <> '') then
    StrPCopy(TempPath, Archive.TempDirectory)                            {!!.02}
  else
    GetTempPath(255, TempPath);                                          {!!.02}
  GetTempFileName(TempPath, Prefix, Archive.FTempFileID, lpTempName);    {!!.02}
  Result := 1;
end;

{ == FDI Callback Functions - cdecl calling convention ===================== }
function FDI_FileOpen(lpPathName: PChar; Flag, Mode: integer): integer;
  cdecl;
  {open a file}
begin
  Result := _lopen(lpPathName, Mode);
end;

{ -------------------------------------------------------------------------- }
function FDI_FileRead(hFile: HFILE; lpBuffer: Pointer; uBytes: UINT): UINT;
  cdecl;
  {read from a file}
begin
  Result := _lread(hFile, lpBuffer, uBytes);
end;

{ -------------------------------------------------------------------------- }
function FDI_FileWrite(hFile: HFILE; lpBuffer: Pointer; uBytes: UINT): UINT;
  cdecl;
  {write to a file}
begin
  Result := _lwrite(hFile, lpBuffer, uBytes);
end;

{ -------------------------------------------------------------------------- }
procedure FDI_FileClose(hFile: HFILE);
  cdecl;
{close a file}
begin
  _lclose(hFile);
end;

{ -------------------------------------------------------------------------- }
function FDI_FileSeek(hFile: HFILE; Offset: longint; Origin: integer): longint;
  cdecl;
  {reposition file pointer}
begin
  Result := _llseek(hFile, Offset, Origin);
end;

{ -------------------------------------------------------------------------- }
function FDI_EnumerateFiles(fdint: FDINOTIFICATIONTYPE;
  pfdin: PFDINotification): integer;
  cdecl;
  {Enumerate the files and build the archive file list}
var
  Item: TAbCabItem;
  Archive: TAbCabArchive;
begin
  Result  := 1;
  Archive := pfdin^.pv;
  with Archive do
    case fdint of
      FDINT_Cabinet_Info:
      begin
        FSetID := pfdin^.setID;
        FCurrentCab := pfdin^.iCabinet;
        FNextCabinet := StrPas(pfdin^.psz1);
        FNextDisk := StrPas(pfdin^.psz2);
        Result := 0;
      end;
      FDINT_Copy_File, FDINT_Partial_File:
      begin
        Item := TAbCabItem.Create;
        with Item do
        begin
          Filename := StrPas(pfdin^.psz1);
          UnCompressedSize := pfdin^.cb;
          LastModFileDate := pfdin^.date;
          LastModFileTime := pfdin^.time;
          ExternalFileAttributes := pfdin^.attribs;
          IsEncrypted := False;  {encryption not implemented at this time}
          PartialFile := (fdint = FDINT_Partial_File);
        end;
        FItemList.Add(Item);
        Result := 0;
      end;
    end;
end;

{ -------------------------------------------------------------------------- }
function FDI_ExtractFiles(fdint: FDINOTIFICATIONTYPE;
  pfdin: PFDINotification): integer;
  cdecl;
  {extract file from cabinet}
var
  Archive: TAbCabArchive;
  NewFilename: string;
  NextCabName: string;
  NewFilePath: string;
  Confirm: boolean;
begin
  Result  := 0;
  Archive := pfdin^.pv;
  case fdint of
    FDINT_Copy_File:
    begin
      NewFilename := StrPas(pfdin^.psz1);
      if (NewFilename = Archive.FItemInProgress.FileName) then
      begin
        if Archive.FIIPName <> '' then
          NewFilename := Archive.FIIPName
        else
        begin
          if not (eoRestorePath in Archive.ExtractOptions) then
            NewFilename := ExtractFileName(NewFileName);
          if (Archive.BaseDirectory <> '') then
            NewFilename := Archive.BaseDirectory + '\' + NewFilename;
        end;
        NewFilePath := ExtractFilePath(NewFilename);
        if (Length(NewFilePath) > 0) and
          (NewFilePath[Length(NewFilePath)] = '\') then
          System.Delete(NewFilePath, Length(NewFilePath), 1);
        if (Length(NewFilePath) > 0) and (not
          AbDirectoryExists(NewFilePath)) then
          if (eoCreateDirs in Archive.ExtractOptions) then
            AbCreateDirectory(NewFilePath)
          else
            raise EAbNoSuchDirectory.Create;
        if FileExists(NewFilename) then
        begin
          Archive.DoConfirmOverwrite(NewFilename, Confirm);
          if not Confirm then
            Result := 0 {skip file}
          else
            Result := FileOpen(NewFilename, fmOpenWrite or fmShareDenyNone);
        end else
          Result := FileCreate(NewFilename);
      end else
        Result := 0;   {skip file}
      //        Application.ProcessMessages;                                 {!!.04}
    end;
    FDINT_Next_Cabinet:
    begin
      Result := 1;
      NextCabName := StrPas(pfdin^.psz3) + StrPas(pfdin^.psz1);
    end;
    FDINT_Close_File_Info:
    begin
      _lclose(pfdin^.hf);
      // [ 880505 ]  Need to Set Attributes after File is closed {!!.05}
      AbFileSetAttr(NewFilename, pfdin^.attribs);
      // Need to test as Handle maybe invalid after _lclose
      FileSetDate(pfdin^.hf, longint(pfdin^.date) shl 16 + pfdin^.time);
      Archive.DoCabItemProcessed;
    end;
  end;
end;


{ == TAbCabArchive ========================================================= }
constructor TAbCabArchive.Create(const FileName: string; Mode: word);
begin
  {Mode is used to identify which interface to use: }
  {  fmOpenWrite - FCI, fmOpenRead - FDI}
  FMode := Mode and fmOpenWrite;
  FStatus := asInvalid;
  FArchiveName := FileName;
  BaseDirectory := ExtractFilePath(ParamStr(0));
  FItemList := TAbArchiveList.Create;
  FPadLock := TAbPadLock.Create;
  FStatus := asIdle;
  StrPCopy(FCabName, ExtractFileName(FileName));
  StrPCopy(FCabPath, ExtractFilePath(FileName));
  SpanningThreshold := AbDefCabSpanningThreshold;
  FFolderThreshold := AbDefFolderThreshold;
  FItemInProgress := nil;
  FItemProgress := 0;
end;

{ -------------------------------------------------------------------------- }
destructor TAbCabArchive.Destroy;
begin
  CloseCabFile;
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.Add(aItem: TAbArchiveItem);
{add a file to the cabinet}
var
  Confirm, DoExecute: boolean;
  FP, FN: array[0..255] of char;
  FH: HFILE;
  Item: TAbCabItem;
begin
  if (FMode <> fmOpenWrite) then
  begin
    DoProcessItemFailure(aItem, ptAdd, ecCabError, 0);
    Exit;
  end;
  CheckValid;
  DoExecute := False;
  if FItemList.IsActiveDupe(aItem.FileName) then
    raise EAbDuplicateName.Create;
  Item := TAbCabItem(aItem);
  DoConfirmProcessItem(Item, ptAdd, Confirm);
  if not Confirm then
    Exit;
  Item.Action := aaAdd;
  StrPCopy(FP, Item.Filename);                                           {!!.02}
  FH := _lopen(FP, OF_READ or OF_SHARE_DENY_NONE);                       {!!.02}
  if (FH <> HFILE_ERROR) then
  begin
    aItem.UncompressedSize := _llseek(FH, 0, 2);
    FItemInProgress := Item;
    FItemList.Add(Item);
    _lclose(FH);
  end else
    raise EAbFileNotFound.Create;


  StrPCopy(FN, ExtractFilename(Item.Filename));                          {!!.02}
  if not FCIAddFile(FFCIContext, FP, FN, DoExecute, @FCI_GetNextCab,
    @FCI_Status, @FCI_GetOpenInfo, CompressionTypeMap[FCompressionType]) then
    raise EAbFCIAddFileError.Create;

end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.CloseCabFile;
{Make sure the Cabinet DLL is shut down}
var
  Abort: boolean;
begin
  if (FFDIContext <> nil) then
  begin
    FDIDestroy(FFDIContext);
    FFDIContext := nil;
  end;
  if (FFCIContext <> nil) then
  begin
    FCIFlushCabinet(FFCIContext, False, @FCI_GetNextCab, @FCI_Status);
    FCIDestroy(FFCIContext);
    FFCIContext := nil;
  end;
  DoArchiveProgress(0, Abort);
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.CreateCabFile;
{create a new cabinet}
begin
  {set cabinet parameters}
  with FFCICabInfo do
  begin
    if (SpanningThreshold > 0) then
      cb := SpanningThreshold
    else
      cb := AbDefCabSpanningThreshold;
    if (FolderThreshold > 0) then
      cbFolderThresh := FolderThreshold
    else
      cbFolderThresh := AbDefFolderThreshold;
    cbReserveCFHeader := AbDefReserveHeaderSize;
    cbReserveCFFolder := AbDefReserveFolderSize;
    cbReserveCFData := AbDefReserveDataSize;
    iCab  := 1;
    iDisk := 0;
    fFailOnIncompressible := 0;
    setID := SetID;
    StrPCopy(szDisk, '');
    StrCopy(szCab, FCabName);
    StrCopy(szCabPath, FCabPath);
  end;

  {obtain an FCI context}
  FFCIContext := FCICreate(@FErrors, @FCI_FileDest, @FXI_GetMem,
    @FXI_FreeMem, @FCI_FileOpen, @FCI_FileRead, @FCI_FileWrite,
    @FCI_FileClose, @FCI_FileSeek, @FCI_FileDelete, @FCI_GetTempFile,
    @FFCICabInfo, Self);
  if (FFCIContext = nil) then
    if FErrors.ErrorPresent then
    begin
      CloseCabFile;
      raise EAbFCICreateError.Create;
    end;
end;

{ -------------------------------------------------------------------------- }
function TAbCabArchive.CreateItem(const FileSpec: string): TAbArchiveItem;
  {create a new item for the file list}
var
  Buff: array [0..255] of char;
begin
  Result := TAbCabItem.Create;
  with TAbCabItem(Result) do
  begin
    CompressedSize := 0;
    StrPCopy(Buff, ExpandFileName(FileSpec));
    AnsiToOEM(Buff, Buff);
    DiskFileName := StrPas(Buff);
    StrPCopy(Buff, FixName(FileSpec));
    AnsiToOEM(Buff, Buff);
    FileName := StrPas(Buff);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoCabItemProcessed;
{allow messages to be processed}
begin
  //  Application.ProcessMessages;                                       {!!.04}
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoCabItemProgress(BytesCompressed: DWord; var Abort: boolean);
{fire OnCabItemProgress event}
var
  Progress: byte;
begin
  Abort := False;
  if Assigned(FOnArchiveItemProgress) then
  begin
    Inc(FItemProgress, BytesCompressed);
    Progress := AbPercentage(FItemProgress, FItemInProgress.UnCompressedSize);
    FOnArchiveItemProgress(Self, FItemInProgress, Progress, Abort);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoConfirmOverwrite(var FileName: string;
  var Confirm: boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmOverwrite) then
    FOnConfirmOverwrite(FileName, Confirm);
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoGetNextCabinet(CabIndex: integer;
  var CabName: string; var Abort: boolean);
{fire OnRequestImage event}
begin
  Abort := False;
  if Assigned(FOnRequestImage) then
    FOnRequestImage(Self, CabIndex, CabName, Abort)
  else
    AbIncFilename(CabName, CabIndex);
end;

{----------------------------------------------------------------------------}
procedure TAbCabArchive.ExtractItemAt(Index: integer; const NewName: string);
{extract a file from the cabinet}
begin
  FItemInProgress := GetItem(Index);
  FIIPName := NewName;
  try
    if not FDICopy(FFDIContext, FCabName, FCabPath, 0, @FDI_ExtractFiles,
      nil, Self) then
      DoProcessItemFailure(FItemInProgress, ptExtract, ecCabError, 0);
  finally
    FIIPName := '';
  end;
end;

{----------------------------------------------------------------------------}
procedure TAbCabArchive.ExtractItemToStreamAt(Index: integer; OutStream: TStream);
begin
  {not implemented for cabinet archives}
end;

{----------------------------------------------------------------------------}
function TAbCabArchive.GetItem(ItemIndex: integer): TAbCabItem;
  {fetch an item from the file list}
begin
  Result := TAbCabItem(FItemList.Items[ItemIndex]);
end;

{----------------------------------------------------------------------------}
procedure TAbCabArchive.LoadArchive;
{Open existing cabinet or create a new one}
begin
  if (FMode = fmOpenRead) then
  begin
    FFDIContext := FDICreate(@FXI_GetMem, @FXI_FreeMem, @FDI_FileOpen,
      @FDI_FileRead, @FDI_FileWrite, @FDI_FileClose, @FDI_FileSeek,
      cpuDefault, @FErrors);
    if (FFDIContext = nil) then
      raise EAbFDICreateError.Create;
    OpenCabFile;
  end else
    CreateCabFile;
end;

{----------------------------------------------------------------------------}
procedure TAbCabArchive.NewCabinet;
{flush current cabinet and start a new one}
begin
  if not FCIFlushCabinet(FFCIContext, True, @FCI_GetNextCab, @FCI_Status) then
    raise EAbFCIFlushCabinetError.Create;
end;

{----------------------------------------------------------------------------}
procedure TAbCabArchive.NewFolder;
{flush current folder and start a new one}
begin
  if not FCIFlushFolder(FFCIContext, @FCI_GetNextCab, @FCI_Status) then
    raise EAbFCIFlushFolderError.Create;
end;

{----------------------------------------------------------------------------}
procedure TAbCabArchive.OpenCabFile;
{Open an existing cabinet}
var
  Abort: boolean;
begin
  {verify that the archive can be opened and is a cabinet}
  FFileHandle := FileOpen(FArchiveName, fmOpenRead or fmShareDenyNone);
  if (FFileHandle <= 0) then
    raise EAbReadError.Create;
  if not FDIIsCabinet(FFDIContext, FFileHandle, @FFDICabInfo) then
  begin
    CloseCabFile;
    raise EAbInvalidCabFile.Create;
  end;

  {store information about the cabinet}
  FileClose(FFileHandle);
  FCabSize := FFDICabInfo.cbCabinet;
  FFolderCount := FFDICabInfo.cFolders;
  FFileCount := FFDICabInfo.cFiles;
  FCurrentCab := FFDICabInfo.iCabinet;
  FHasPrev := FFDICabInfo.hasPrev;
  FHasNext := FFDICabInfo.hasNext;

  {Enumerate the files and build the file list}
  if not FDICopy(FFDIContext, FCabName, FCabPath, 0, @FDI_EnumerateFiles,
    nil, Self) then
  begin
    CloseCabFile;
    raise EAbFDICopyError.Create;
  end;
  DoArchiveProgress(100, Abort);
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.PutItem(Index: integer; Value: TAbCabItem);
{replace an existing item in the file list}
begin
  FItemList.Items[Index] := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SaveArchive;
{flush cabinet file}
begin
  if (FFCIContext <> nil) then
    FCIFlushCabinet(FFCIContext, False, @FCI_GetNextCab, @FCI_Status);
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetFolderThreshold(Value: longint);
{set maximum compression boundary}
begin
  if (Value > 0) then
    FFolderThreshold := Value
  else
    FFolderThreshold := AbDefFolderThreshold;
  FFCICabInfo.cbFolderThresh := FFolderThreshold;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetSetID(Value: word);
{set cabinet SetID}
begin
  FSetID := Value;
  FFCICabInfo.SetID := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetSpanningThreshold(Value: longint);
{set maximum cabinet size}
begin
  if (Value > 0) then
    FSpanningThreshold := Value
  else
    FSpanningThreshold := AbDefCabSpanningThreshold;
  FFCICabInfo.cb := FSpanningThreshold;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.TestItemAt(Index: integer);
begin
  {not implemented for cabinet archives}
end;

end.
