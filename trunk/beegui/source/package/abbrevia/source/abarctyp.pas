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
 {* ABBREVIA: AbArcTyp.pas 3.05                           *}
 {*********************************************************}
 {* ABBREVIA: TABArchive, TABArchiveItem classes          *}
 {*********************************************************}

{$I AbDefine.inc}

unit AbArcTyp;

interface

uses
  {$IFNDEF Linux}
  Windows,                                                 {!!.03}
  {$ENDIF Linux}
  Classes,
  AbUtils,
  SysUtils;

{ ===== TAbArchiveItem ====================================================== }
type
  TAbArchiveItem = class(TObject)
  private
    function GetLastModTimeAsDateTime: TDateTime;                        {!!.01}
    procedure SetLastModTimeAsDateTime(const Value: TDateTime);          {!!.01}
  protected {private}
    NextItem: TAbArchiveItem;
    FAction: TAbArchiveAction;
    FCompressedSize: longint;
    FCRC32:  longint;
    FDiskFileName: string;
    FExternalFileAttributes: longint;
    FFileName: string;
    FIsEncrypted: boolean;
    FLastModFileTime: word;
    FLastModFileDate: word;
    FTagged: boolean;
    FUncompressedSize: longint;

  protected {property methods}
    function GetCompressedSize: longint; virtual;
    function GetCRC32: longint; virtual;
    function GetDiskPath: string;
    function GetExternalFileAttributes: longint; virtual;
    function GetFileName: string; virtual;
    function GetIsEncrypted: boolean; virtual;
    function GetLastModFileDate: word; virtual;
    function GetLastModFileTime: word; virtual;
    function GetStoredPath: string;
    function GetUncompressedSize: longint; virtual;
    procedure SetCompressedSize(const Value: longint); virtual;
    procedure SetCRC32(const Value: longint); virtual;
    procedure SetExternalFileAttributes(Value: longint); virtual;
    procedure SetFileName(const Value: string); virtual;
    procedure SetIsEncrypted(Value: boolean); virtual;
    procedure SetLastModFileDate(const Value: word); virtual;
    procedure SetLastModFileTime(const Value: word); virtual;
    procedure SetUncompressedSize(const Value: longint); virtual;

  public {methods}
    constructor Create;
    destructor Destroy; override;
    function MatchesDiskName(const FileMask: string): boolean;
    function MatchesStoredName(const FileMask: string): boolean;
    function MatchesStoredNameEx(const FileMask: string): boolean;


  public {properties}
    property Action: TAbArchiveAction Read FAction Write FAction;
    property CompressedSize: longint Read GetCompressedSize Write SetCompressedSize;
    property CRC32: longint Read GetCRC32 Write SetCRC32;
    property DiskFileName: string Read FDiskFileName Write FDiskFileName;
    property DiskPath: string Read GetDiskPath;
    property ExternalFileAttributes: longint
      Read GetExternalFileAttributes Write SetExternalFileAttributes;
    property FileName: string Read GetFileName Write SetFileName;
    property IsEncrypted: boolean Read GetIsEncrypted Write SetIsEncrypted;
    property LastModFileDate: word Read GetLastModFileDate Write SetLastModFileDate;
    property LastModFileTime: word Read GetLastModFileTime Write SetLastModFileTime;
    property StoredPath: string Read GetStoredPath;
    property Tagged: boolean Read FTagged Write FTagged;
    property UncompressedSize: longint Read GetUncompressedSize
      Write SetUncompressedSize;

    property LastModTimeAsDateTime: TDateTime                           {!!.01}
      Read GetLastModTimeAsDateTime                                      {!!.01}
      Write SetLastModTimeAsDateTime;                                    {!!.01}
  end;


{ ===== TAbArchiveList ====================================================== }
type
  TAbArchiveList = class
  protected {private}
    FList: TList;
    HashTable: array[0..1020] of TAbArchiveItem;
  protected {methods}
    function GenerateHash(const S: string): longint;
    function GetCount: integer;
    procedure SetCount(NewCount: integer);
    function Get(Index: integer): TAbArchiveItem;
    procedure Put(Index: integer; Item: TAbArchiveItem);
  public {methods}
    constructor Create;
    destructor Destroy; override;
    function Add(Item: Pointer): integer;
    procedure Clear;
    procedure Delete(Index: integer);
    function Find(const FN: string): integer;
    function IsActiveDupe(const FN: string): boolean;
  public {properties}
    property Count: integer Read GetCount Write SetCount;
    property Items[Index: integer]: TAbArchiveItem Read Get Write Put; default;
  end;


{ ===== TAbArchive specific types =========================================== }
type
  TAbStoreOption  =
    (soStripDrive, soStripPath, soRemoveDots, soRecurse, soFreshen, soReplace);
  TAbStoreOptions =
    set of TAbStoreOption;

  TAbExtractOption  =
    (eoCreateDirs, eoRestorePath);
  TAbExtractOptions =
    set of TAbExtractOption;

  TAbArchiveStatus =
    (asInvalid, asIdle, asBusy);

  TAbArchiveEvent =
    procedure(Sender: TObject) of object;
  TAbArchiveConfirmEvent =
    procedure(Sender: TObject; var Confirm: boolean) of object;
  TAbArchiveProgressEvent =
    procedure(Sender: TObject; Progress: byte; var Abort: boolean) of object;
  TAbArchiveItemEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem) of object;
  TAbArchiveItemConfirmEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem;
    ProcessType: TAbProcessType; var Confirm: boolean) of object;
  TAbConfirmOverwriteEvent =
    procedure(var Name: string; var Confirm: boolean) of object;
  TAbArchiveItemFailureEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem;
    ProcessType: TAbProcessType; ErrorClass: TAbErrorClass;
    ErrorCode: integer) of object;
  TAbArchiveItemExtractEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem;
    const NewName: string) of object;
  TAbArchiveItemExtractToStreamEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem;
    OutStream: TStream) of object;
  TAbArchiveItemTestEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem) of object;
  TAbArchiveItemInsertEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem;
    OutStream: TStream) of object;
  TAbArchiveItemInsertFromStreamEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem;
    OutStream, InStream: TStream) of object;
  TAbArchiveItemProgressEvent =
    procedure(Sender: TObject; Item: TAbArchiveItem; Progress: byte;
    var Abort: boolean) of object;
  TAbProgressEvent =
    procedure(Progress: byte; var Abort: boolean) of object;
  TAbRequestDiskEvent =
    procedure(Sender: TObject; var Abort: boolean) of object;
  TAbRequestImageEvent =
    procedure(Sender: TObject; ImageNumber: integer; var ImageName: string;
    var Abort: boolean) of object;
  TAbRequestNthDiskEvent =
    procedure(Sender: TObject; DiskNumber: byte; var Abort: boolean) of object;


type
  TAbArchiveStreamHelper = class
  protected
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    procedure ExtractItemData(AStream: TStream); virtual; abstract;
    function FindFirstItem: boolean; virtual; abstract;
    function FindNextItem: boolean; virtual; abstract;
    procedure ReadHeader; virtual; abstract;
    procedure ReadTail; virtual; abstract;
    function SeekItem(Index: integer): boolean; virtual; abstract;
    procedure WriteArchiveHeader; virtual; abstract;
    procedure WriteArchiveItem(AStream: TStream); virtual; abstract;
    procedure WriteArchiveTail; virtual; abstract;
    function GetItemCount: integer; virtual; abstract;
  end;


{ ===== TAbArchive ========================================================== }
type
  TAbArchive = class(TObject)
  public
    FStream: TStream;
    FStatus: TAbArchiveStatus;

  protected {property variables}
    FArchiveName: string;
    FAutoSave: boolean;
    FBaseDirectory: string;
    FCurrentItem: TAbArchiveItem;
    FDOSMode: boolean;
    FExtractOptions: TAbExtractOptions;
    FImageNumber: word;
    FInStream: TStream;
    FIsDirty: boolean;
    FSpanningThreshold: longint;
    FItemList: TAbArchiveList;
    FLogFile: string;
    FLogging: boolean;
    FLogStream: TFileStream;
    FMode: word;
    FOwnsStream: boolean;
    FPadLock: TAbPadLock;
    FSpanned: boolean;
    FStoreOptions: TAbStoreOptions;
    FTempDir: string;

  protected {event variables}
    FOnProcessItemFailure: TAbArchiveItemFailureEvent;
    FOnArchiveProgress: TAbArchiveProgressEvent;
    FOnArchiveSaveProgress: TAbArchiveProgressEvent;                  {!!.04}
    FOnArchiveItemProgress: TAbArchiveItemProgressEvent;
    FOnConfirmProcessItem: TAbArchiveItemConfirmEvent;
    FOnConfirmOverwrite: TAbConfirmOverwriteEvent;
    FOnConfirmSave: TAbArchiveConfirmEvent;
    FOnLoad: TAbArchiveEvent;
    FOnProgress: TAbProgressEvent;
    FOnRequestImage: TAbRequestImageEvent;
    FOnSave: TAbArchiveEvent;

  protected {methods}
    procedure CheckValid;
    procedure FreshenAt(Index: integer);
    function FreshenRequired(Item: TAbArchiveItem): boolean;
    procedure GetFreshenTarget(Item: TAbArchiveItem);
    function GetItemCount: integer;
    procedure Init;
    procedure Lock;
    procedure MakeLogEntry(const FN: string; LT: TAbLogType);
    procedure ReplaceAt(Index: integer);
    procedure SaveIfNeeded(aItem: TAbArchiveItem);
    procedure SetBaseDirectory(Value: string);
    procedure SetLogFile(const Value: string);
    procedure SetLogging(Value: boolean);
    procedure Unlock;
    // !!0.7
    // protected {abstract methods}
    //   function CreateItem(const FileSpec : string): TAbArchiveItem;
    //    virtual; abstract;
  public
    function CreateItem(const FileSpec: string): TAbArchiveItem;
      virtual; abstract;
    // !!0.7
  protected
    procedure ExtractItemAt(Index: integer; const NewName: string);
      virtual; abstract;
    procedure ExtractItemToStreamAt(Index: integer; aStream: TStream);
      virtual; abstract;
    procedure LoadArchive;
      virtual; abstract;
    procedure SaveArchive;
      virtual; abstract;
    procedure TestItemAt(Index: integer);
      virtual; abstract;

  protected {virtual methods}
    procedure DoProcessItemFailure(Item: TAbArchiveItem;
      ProcessType: TAbProcessType; ErrorClass: TAbErrorClass;
      ErrorCode: integer);
      virtual;
    procedure DoArchiveSaveProgress(Progress: byte; var Abort: boolean); {!!.04}
      virtual;                                                             {!!.04}
    procedure DoArchiveProgress(Progress: byte; var Abort: boolean);
      virtual;
    procedure DoArchiveItemProgress(Item: TAbArchiveItem; Progress: byte;
      var Abort: boolean);
      virtual;
    procedure DoConfirmProcessItem(Item: TAbArchiveItem;
      const ProcessType: TAbProcessType; var Confirm: boolean);
      virtual;
    procedure DoConfirmSave(var Confirm: boolean);
      virtual;

    procedure DoLoad;
      virtual;
    procedure DoProgress(Progress: byte; var Abort: boolean);
      virtual;
    procedure DoSave;
      virtual;
    function FixName(const Value: string): string;
      virtual;
    function GetSpanningThreshold: longint;
      virtual;
    procedure SetSpanningThreshold(Value: longint);
      virtual;

  protected {properties and events}
    property InStream: TStream Read FInStream;

  public {methods}
    constructor Create(const FileName: string; Mode: word);
      virtual;
    constructor CreateFromStream(aStream: TStream; const aArchiveName: string);
      virtual; {!!.05 Added Virtual}
    destructor Destroy;
      override;
    procedure Add(aItem: TAbArchiveItem);
      virtual;
    procedure AddFiles(const FileMask: string; SearchAttr: integer);
    procedure AddFilesEx(const FileMask, ExclusionMask: string;
      SearchAttr: integer);
    procedure AddFromStream(const NewName: string; aStream: TStream);
    procedure ClearTags;
    procedure Delete(aItem: TAbArchiveItem);
    procedure DeleteAt(Index: integer);
    procedure DeleteFiles(const FileMask: string);
    procedure DeleteFilesEx(const FileMask, ExclusionMask: string);
    procedure DeleteTaggedItems;
    procedure Extract(aItem: TAbArchiveItem; const NewName: string);
    procedure ExtractAt(Index: integer; const NewName: string);
    procedure ExtractFiles(const FileMask: string);
    procedure ExtractFilesEx(const FileMask, ExclusionMask: string);
    procedure ExtractTaggedItems;
    procedure ExtractToStream(const aFileName: string; aStream: TStream);
    function FindFile(const aFileName: string): integer;
    function FindItem(aItem: TAbArchiveItem): integer;
    procedure Freshen(aItem: TAbArchiveItem);
    procedure FreshenFiles(const FileMask: string);
    procedure FreshenFilesEx(const FileMask, ExclusionMask: string);
    procedure FreshenTaggedItems;
    procedure Load; virtual;
    procedure Move(aItem: TAbArchiveItem; const NewStoredPath: string);
      virtual;
    procedure Replace(aItem: TAbArchiveItem);
    procedure Save;
      virtual;
    procedure TagItems(const FileMask: string);
    procedure TestTaggedItems;
    procedure UnTagItems(const FileMask: string);


    procedure DoDeflateProgress(aPercentDone: integer);
      virtual;
    procedure DoInflateProgress(aPercentDone: integer);
      virtual;
    procedure DoSpanningMediaRequest(Sender: TObject; ImageNumber: integer;
      var ImageName: string; var Abort: boolean); virtual;
  public {properties}
    property OnProgress: TAbProgressEvent Read FOnProgress Write FOnProgress;
    property ArchiveName: string Read FArchiveName;
    property AutoSave: boolean Read FAutoSave Write FAutoSave;
    property BaseDirectory: string Read FBaseDirectory Write SetBaseDirectory;
    property Count: integer Read GetItemCount;
    property DOSMode: boolean Read FDOSMode Write FDOSMode;
    property ExtractOptions: TAbExtractOptions
      Read FExtractOptions Write FExtractOptions;
    property IsDirty: boolean Read FIsDirty Write FIsDirty;
    property ItemList: TAbArchiveList Read FItemList;
    property LogFile: string Read FLogFile Write SetLogFile;
    property Logging: boolean Read FLogging Write SetLogging;
    property Mode: word Read FMode;
    property Spanned: boolean Read FSpanned;
    property SpanningThreshold: longint Read GetSpanningThreshold
      Write SetSpanningThreshold;
    property Status: TAbArchiveStatus Read FStatus;
    property StoreOptions: TAbStoreOptions Read FStoreOptions Write FStoreOptions;
    property TempDirectory: string Read FTempDir Write FTempDir;

  public {events}
    property OnProcessItemFailure: TAbArchiveItemFailureEvent
      Read FOnProcessItemFailure Write FOnProcessItemFailure;
    property OnArchiveProgress: TAbArchiveProgressEvent
      Read FOnArchiveProgress Write FOnArchiveProgress;
    property OnArchiveSaveProgress: TAbArchiveProgressEvent           {!!.04}
      Read FOnArchiveSaveProgress                                      {!!.04}
      Write FOnArchiveSaveProgress;                                    {!!.04}
    property OnArchiveItemProgress: TAbArchiveItemProgressEvent
      Read FOnArchiveItemProgress Write FOnArchiveItemProgress;
    property OnConfirmProcessItem: TAbArchiveItemConfirmEvent
      Read FOnConfirmProcessItem Write FOnConfirmProcessItem;
    property OnConfirmOverwrite: TAbConfirmOverwriteEvent
      Read FOnConfirmOverwrite Write FOnConfirmOverwrite;
    property OnConfirmSave: TAbArchiveConfirmEvent
      Read FOnConfirmSave Write FOnConfirmSave;
    property OnLoad: TAbArchiveEvent Read FOnLoad Write FOnLoad;
    property OnRequestImage: TAbRequestImageEvent
      Read FOnRequestImage Write FOnRequestImage;
    property OnSave: TAbArchiveEvent Read FOnSave Write FOnSave;
  end;


const
  AbDefAutoSave = False;
  AbDefExtractOptions = [eoCreateDirs];
  AbDefStoreOptions = [soStripDrive, soRemoveDots];
  AbBufferSize = 32768;
  AbLastDisk  = -1;
  AbLastImage = -1;


function AbConfirmPath(const BaseDirectory: string; var NewName: string;
  ExtractOptions: TAbExtractOptions;
  ConfirmOverwrite: TAbConfirmOverwriteEvent): boolean;

implementation

{.$R ABRES.R32}

uses
  AbExcept,
  AbSpanSt,
  AbDfBase,
  AbConst;

const
  CRLF = #13 + #10;
  ProcessTypeToLogType: array[TAbProcessType] of TAbLogType =
    (ltAdd, ltDelete, ltExtract, ltFreshen, ltMove, ltReplace, ltFoundUnhandled);


function AbConfirmPath(const BaseDirectory: string; var NewName: string;
  ExtractOptions: TAbExtractOptions;
  ConfirmOverwrite: TAbConfirmOverwriteEvent): boolean;
var
  FMessage: string;
  TestPath: string;
begin
  Result := True;
  TestPath := NewName;
  FMessage := BaseDirectory;

  {BaseDirectory is the drive:\directory\sub where we currently want files}
  {NewName is the optionalpath\sub\filename.ext where we want the file}
  AbUnfixName(TestPath);

  if (FMessage <> '') and (FMessage[Length(FMessage)] <> AbPathDelim) then
    FMessage := FMessage + AbPathDelim;
  if (eoRestorePath in ExtractOptions) then
    FMessage := FMessage + TestPath
  else
    FMessage := FMessage + ExtractFileName(TestPath);

  TestPath := ExtractFilePath(FMessage);
  if (Length(TestPath) > 0) and (TestPath[Length(TestPath)] = AbPathDelim) then
    System.Delete(TestPath, Length(TestPath), 1);
  if (Length(TestPath) > 0) and (not AbDirectoryExists(TestPath)) then
    if (eoCreateDirs in ExtractOptions) then
      AbCreateDirectory(TestPath)
    else
      raise EAbNoSuchDirectory.Create;

  if FileExists(FMessage) and Assigned(ConfirmOverwrite) then
    ConfirmOverwrite(FMessage, Result);

  if Result then
    NewName := FMessage;
end;


 { TAbArchiveItem implementation ============================================ }
 { TAbArchiveItem }
constructor TAbArchiveItem.Create;
begin
  inherited Create;
  FCompressedSize := 0;
  FUncompressedSize := 0;
  FFileName := '';
  FAction := aaNone;
  FLastModFileTime := 0;
  FLastModFileDate := 0;
end;

{ -------------------------------------------------------------------------- }
destructor TAbArchiveItem.Destroy;
begin
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetCompressedSize: longint;
begin
  Result := FCompressedSize;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetCRC32: longint;
begin
  Result := FCRC32;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetDiskPath: string;
begin
  Result := ExtractFilePath(DiskFileName);
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetExternalFileAttributes: longint;
begin
  Result := FExternalFileAttributes;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetFileName: string;
begin
  Result := FFileName;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetIsEncrypted: boolean;
begin
  Result := FIsEncrypted;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetLastModFileTime: word;
begin
  Result := FLastModFileTime;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetLastModFileDate: word;
begin
  Result := FLastModFileDate;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetStoredPath: string;
begin
  Result := ExtractFilePath(DiskFileName);
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetUnCompressedSize: longint;
begin
  Result := FUnCompressedSize;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.MatchesDiskName(const FileMask: string): boolean;
var
  DiskName, Mask: string;
begin
  DiskName := DiskFileName;
  AbUnfixName(DiskName);
  Mask := FileMask;
  AbUnfixName(Mask);
  Result := AbFileMatch(DiskName, Mask);
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.MatchesStoredName(const FileMask: string): boolean;
var
  Value: string;
  Drive, Dir, Name: string;
begin
  Value := FileMask;
  AbUnfixName(Value);
  AbParseFileName(Value, Drive, Dir, Name);
  Value := Dir + Name;
  Name  := FileName;
  AbUnfixName(Name);
  Result := AbFileMatch(Name, Value);
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveItem.MatchesStoredNameEx(const FileMask: string): boolean;
var
  I, J: integer;
  MaskPart: string;
begin
  Result := True;
  I := 1;
  while I <= Length(FileMask) do
  begin
    J := I;
    while (I <= Length(FileMask)) and (FileMask[I] <> PathSep {';'}) do
      Inc(I);
    MaskPart := Trim(Copy(FileMask, J, I - J));
    if (I <= Length(FileMask)) and (FileMask[I] = PathSep {';'}) then
      Inc(I);

    if MatchesStoredName(MaskPart) then
      Exit;
  end;
  Result := False;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetCompressedSize(const Value: longint);
begin
  FCompressedSize := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetCRC32(const Value: longint);
begin
  FCRC32 := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetExternalFileAttributes(Value: longint);
begin
  FExternalFileAttributes := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetIsEncrypted(Value: boolean);
begin
  FIsEncrypted := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetLastModFileDate(const Value: word);
begin
  FLastModFileDate := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetLastModFileTime(const Value: word);
begin
  FLastModFileTime := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetUnCompressedSize(const Value: longint);
begin
  FUnCompressedSize := Value;
end;

 { -------------------------------------------------------------------------- }
 {!!.01 -- Added }
function TAbArchiveItem.GetLastModTimeAsDateTime: TDateTime;
begin
  Result := AbDosFileDateToDateTime(LastModFileDate, LastModFileTime);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetLastModTimeAsDateTime(const Value: TDateTime);
var
  FileDate: integer;
begin
  FileDate := AbDateTimeToDosFileDate(Value);
  LastModFileTime := LongRec(FileDate).Lo;
  LastModFileDate := LongRec(FileDate).Hi;
end;

 { -------------------------------------------------------------------------- }
 {!!.01 -- End Added }

 { TAbArchiveList implementation ============================================ }
 { TAbArchiveList }
constructor TAbArchiveList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

{ -------------------------------------------------------------------------- }
destructor TAbArchiveList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveList.Add(Item: Pointer): integer;
var
  H: longint;
begin
  H := GenerateHash(TAbArchiveItem(Item).FileName);
  TAbArchiveItem(Item).NextItem := HashTable[H];
  HashTable[H] := TAbArchiveItem(Item);
  Result := FList.Add(Item);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveList.Clear;
begin
  FList.Clear;
  FillChar(HashTable, SizeOf(HashTable), #0);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveList.Delete(Index: integer);
var
  Look: TAbArchiveItem;
  Last: Pointer;
  FN: string;
begin
  FN := TAbArchiveItem(FList[Index]).FileName;
  Last := @HashTable[GenerateHash(FN)];
  Look := TAbArchiveItem(Last^);
  while Look <> nil do
  begin
    if CompareText(Look.FileName, FN) = 0 then
    begin
      Move(Look.NextItem, Last^, 4);
      Break;
    end;
    Last := @Look.NextItem;
    Look := TAbArchiveItem(Last^);
  end;
  TObject(FList[Index]).Free;
  FList.Delete(Index);
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveList.Find(const FN: string): integer;
var
  Look: TAbArchiveItem;
begin
  Look := HashTable[GenerateHash(FN)];
  while Look <> nil do
  begin
    if AnsiCompareFileName(Look.FileName, FN) = 0 then
    begin
      Result := FList.IndexOf(Look);
      Exit;
    end;
    Look := Look.NextItem;
  end;
  Result := -1;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveList.GenerateHash(const S: string): longint;
var
  G: longint;
  I: integer;
  U: string;
begin
{$Q-}
  Result := 0;
  U := AnsiUpperCase(S);
  for I := 1 to Length(U) do
  begin
    Result := (Result shl 4) + Ord(U[I]);
    G := longint(Result and $F0000000);
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
  Result := Result mod 1021;
{$Q+}
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveList.Get(Index: integer): TAbArchiveItem;
begin
  Result := TAbArchiveItem(FList[Index]);
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveList.GetCount: integer;
begin
  Result := FList.Count;
end;

{ -------------------------------------------------------------------------- }
function TAbArchiveList.IsActiveDupe(const FN: string): boolean;
var
  Look: TAbArchiveItem;
begin
  Look := HashTable[GenerateHash(FN)];
  while Look <> nil do
  begin
    if (CompareText(Look.FileName, FN) = 0) and (Look.Action <> aaDelete) then
    begin
      Result := True;
      Exit;
    end;
    Look := Look.NextItem;
  end;
  Result := False;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveList.Put(Index: integer; Item: TAbArchiveItem);
var
  H:  longint;
  Look: TAbArchiveItem;
  Last: Pointer;
  FN: string;
begin
  FN := TAbArchiveItem(FList[Index]).FileName;
  Last := @HashTable[GenerateHash(FN)];
  Look := TAbArchiveItem(Last^);
  { Delete old index }
  while Look <> nil do
  begin
    if CompareText(Look.FileName, FN) = 0 then
    begin
      Move(Look.NextItem, Last^, 4);
      Break;
    end;
    Last := @Look.NextItem;
    Look := TAbArchiveItem(Last^);
  end;
  { Free old instance }
  TObject(FList[Index]).Free;
  { Add new index }
  H := GenerateHash(TAbArchiveItem(Item).FileName);
  TAbArchiveItem(Item).NextItem := HashTable[H];
  HashTable[H] := TAbArchiveItem(Item);
  { Replace pointer }
  FList[Index] := Item;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchiveList.SetCount(NewCount: integer);
begin
  FList.Count := NewCount;
end;


 { TAbArchive implementation ================================================ }
 { TAbArchive }
constructor TAbArchive.Create(const FileName: string; Mode: word);
  {create an archive by opening a filestream on filename with the given mode}
begin
  inherited Create;
  FStatus := asInvalid;
  if AbDriveIsRemovable(FileName) then
    FStream := TAbSpanStream.Create(FileName, Mode, mtRemoveable, FSpanningThreshold)
  else
    FStream := TAbSpanStream.Create(FileName, Mode, mtLocal, FSpanningThreshold);
  // !!.05 The following is a test to use TFileStream directly if not spanning
  // This removes the headaches of not being able to seek() when not spanning.
  // Allowing features such as TestTagged, and Add then Extract to work without
  // having to close reopen the archive.
  //   FStream := TFileStream.Create(FileName,Mode);

  if (FStream is TAbSpanStream) then {!!.05}
  begin
    TAbSpanStream(FStream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress;   {!!.04}
  end;

  FLogStream := nil;
  FOwnsStream := True;
  FArchiveName := FileName;
  FOnProgress := DoProgress;
  FSpanned := False;
  FMode := Mode;
  BaseDirectory := ExtractFilePath(ParamStr(0));
  Init;
end;

{ -------------------------------------------------------------------------- }
constructor TAbArchive.CreateFromStream(aStream: TStream; const aArchiveName: string);
  {create an archive based on an existing stream}
begin
  inherited Create;
  FStatus := asInvalid;
  FLogStream := nil;
  FArchiveName := aArchiveName;
  FOnProgress := DoProgress;
  FOwnsStream := False;
  FSpanned := False;
  FStream := aStream;
  FMode := 0;
  Init;
end;

{ -------------------------------------------------------------------------- }
destructor TAbArchive.Destroy;
var
  i: integer;
begin
  if Assigned(FItemList) then
  begin
    if Count > 0 then
      for i := pred(Count) downto 0 do
        TObject(FItemList.Items[i]).Free;
    FItemList.Clear;
    FItemList.Free;
    FItemList := nil;
  end;
  FPadLock.Free;
  FPadLock := nil;
  if FOwnsStream then
  begin
    if Assigned(FStream) then
      {!!.05 avoid A/V if Nil (Only occurs if exception is raised)}
      FStream.Free;
    FStream := nil;
  end;
  if Assigned(FLogStream) then
  begin
    FLogStream.Free;
    FLogStream := nil;
  end;
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Add(aItem: TAbArchiveItem);
var
  Confirm: boolean;
begin
  CheckValid;
  if FItemList.IsActiveDupe(aItem.FileName) then
  begin
    if (soFreshen in StoreOptions) then
      Freshen(aItem)
    else if (soReplace in StoreOptions) then
      Replace(aItem)
    else
    begin
      DoProcessItemFailure(aItem, ptAdd, ecAbbrevia, AbDuplicateName);
      aItem.Free;
    end;
  end else
  begin
    DoConfirmProcessItem(aItem, ptAdd, Confirm);
    if not Confirm then
      Exit;
    Lock;
    try
      aItem.Action := aaAdd;
      FItemList.Add(aItem);
      FIsDirty := True;
      if AutoSave then
        Save;
    finally
      Unlock;
    end;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.AddFiles(const FileMask: string; SearchAttr: integer);
{Add files to the archive where the disk filespec matches}
begin
  AddFilesEx(FileMask, '', SearchAttr);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.AddFilesEx(const FileMask, ExclusionMask: string;
  SearchAttr: integer);
{Add files matching Filemask except those matching ExclusionMask}
var
  PathType: TAbPathType;
  IsWild: boolean;
  SaveDir: string;
  Mask:  string;
  MaskF: string;

  procedure CreateItems(Wild, Recursing: boolean);
  var
    i: integer;
    Files: TStrings;
    FilterList: TStringList;
    Item: TAbArchiveItem;
  begin
    FilterList := TStringList.Create;
    try
      if (MaskF <> '') then
        AbFindFilesEx(MaskF, SearchAttr and not faDirectory, FilterList, Recursing);

      Files := TStringList.Create;
      try

        AbFindFilesEx(Mask, SearchAttr and not faDirectory, Files, Recursing);
        if (Files.Count > 0) then
          for i := 0 to pred(Files.Count) do
            if FilterList.IndexOf(Files[i]) < 0 then
              if not Wild then
              begin
                if (Files[i] <> FArchiveName) then
                begin
                  Item := CreateItem(Files[i]);
                  Add(Item);
                end;
              end else
              begin
                if (AbAddBackSlash(FBaseDirectory) + Files[i]) <>
                  FArchiveName then
                begin
                  Item := CreateItem(Files[i]);
                  Add(Item);
                end;
              end;
      finally
        Files.Free;
      end;

    finally
      FilterList.Free;
    end;
  end;

begin
  CheckValid;
  IsWild := (Pos('*', FileMask) > 0) or (Pos('?', FileMask) > 0);
  PathType := AbGetPathType(FileMask);

  Mask := FileMask;
  AbUnfixName(Mask);
  MaskF := ExclusionMask;
  AbUnfixName(MaskF);

  case PathType of
    ptNone:
    begin
      GetDir(0, SaveDir);
      if BaseDirectory <> '' then
        ChDir(BaseDirectory);
      try
        CreateItems(IsWild, soRecurse in StoreOptions);
      finally
        if BaseDirectory <> '' then
          ChDir(SaveDir);
      end;
    end;
    ptRelative:
    begin
      GetDir(0, SaveDir);
      if BaseDirectory <> '' then
        ChDir(BaseDirectory);
      try
        CreateItems(IsWild, soRecurse in StoreOptions);
      finally
        if BaseDirectory <> '' then
          ChDir(SaveDir);
      end;
    end;
    ptAbsolute:
    begin
      CreateItems(IsWild, soRecurse in StoreOptions);
    end;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.AddFromStream(const NewName: string; aStream: TStream);
{Add an item to the archive directly from a TStream descendant}
var
  Confirm: boolean;
  Item: TAbArchiveItem;
  PT: TAbProcessType;
begin
  Item := CreateItem(NewName);
  CheckValid;

  PT := ptAdd;
  if FItemList.IsActiveDupe(NewName) then
  begin
    if ((soFreshen in StoreOptions) or (soReplace in StoreOptions)) then
    begin
      Item.Free;
      Item := FItemList[FItemList.Find(NewName)];
      PT := ptReplace;
    end else
    begin
      DoProcessItemFailure(Item, ptAdd, ecAbbrevia, AbDuplicateName);
      Item.Free;
      Exit;
    end;
  end;
  DoConfirmProcessItem(Item, PT, Confirm);

  if not Confirm then
    Exit;
  Lock;
  try
    FInStream := aStream;
    Item.Action := aaStreamAdd;
    if (PT = ptAdd) then
      FItemList.Add(Item);
    FIsDirty := True;
    Save;
    FInStream := nil;
  finally
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.CheckValid;
begin
  if Status = asInvalid then
    raise EAbNoArchive.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.ClearTags;
{Clear all tags from the archive}
var
  i: integer;
begin
  if Count > 0 then
    for i := 0 to pred(Count) do
      TAbArchiveItem(FItemList[i]).Tagged := False;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Delete(aItem: TAbArchiveItem);
{delete an item from the archive}
var
  Index: integer;
begin
  CheckValid;
  Index := FindItem(aItem);
  if Index <> -1 then
    DeleteAt(Index);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteAt(Index: integer);
{delete the item at the index from the archive}
var
  Confirm: boolean;
begin
  CheckValid;
  SaveIfNeeded(FItemList[Index]);
  Lock;
  try
    DoConfirmProcessItem(FItemList[Index], ptDelete, Confirm);
    if not Confirm then
      Exit;

    TAbArchiveItem(FItemList[Index]).Action := aaDelete;
    FIsDirty := True;
    if AutoSave then
      Save;
  finally
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteFiles(const FileMask: string);
{delete all files from the archive that match the file mask}
begin
  DeleteFilesEx(FileMask, '');
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteFilesEx(const FileMask, ExclusionMask: string);
{Delete files matching Filemask except those matching ExclusionMask}
var
  i: integer;
begin
  CheckValid;
  if Count > 0 then
  begin
    for i := pred(Count) downto 0 do
    begin
      with TAbArchiveItem(FItemList[i]) do
        if MatchesStoredNameEx(FileMask) then
          if not MatchesStoredNameEx(ExclusionMask) then
            DeleteAt(i);
    end;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteTaggedItems;
{delete all tagged items from the archive}
var
  i: integer;
begin
  CheckValid;
  if Count > 0 then
  begin
    for i := pred(Count) downto 0 do
    begin
      with TAbArchiveItem(FItemList[i]) do
        if Tagged then
          DeleteAt(i);
    end;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoProcessItemFailure(Item: TAbArchiveItem;
  ProcessType: TAbProcessType; ErrorClass: TAbErrorClass; ErrorCode: integer);
begin
  if Assigned(FOnProcessItemFailure) then
    FOnProcessItemFailure(Self, Item, ProcessType, ErrorClass, ErrorCode);
end;

 { -------------------------------------------------------------------------- }
 {!!.04 - Added }
procedure TAbArchive.DoArchiveSaveProgress(Progress: byte; var Abort: boolean);
begin
  Abort := False;
  if Assigned(FOnArchiveSaveProgress) then
    FOnArchiveSaveProgress(Self, Progress, Abort);
end;

 {!!.04 - Added end }
 { -------------------------------------------------------------------------- }
procedure TAbArchive.DoArchiveProgress(Progress: byte; var Abort: boolean);
begin
  Abort := False;
  if Assigned(FOnArchiveProgress) then
    FOnArchiveProgress(Self, Progress, Abort);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoArchiveItemProgress(Item: TAbArchiveItem;
  Progress: byte; var Abort: boolean);
begin
  Abort := False;
  if Assigned(FOnArchiveItemProgress) then
    FOnArchiveItemProgress(Self, Item, Progress, Abort);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoConfirmProcessItem(Item: TAbArchiveItem;
  const ProcessType: TAbProcessType; var Confirm: boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmProcessItem) then
    FOnConfirmProcessItem(Self, Item, ProcessType, Confirm);
  if (Confirm and FLogging) then
    MakeLogEntry(Item.Filename, ProcessTypeToLogType[ProcessType]);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoConfirmSave(var Confirm: boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmSave) then
    FOnConfirmSave(Self, Confirm);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoDeflateProgress(aPercentDone: integer);
var
  Abort: boolean;
begin
  DoProgress(aPercentDone, Abort);
  if Abort then
    raise EAbAbortProgress.Create(AbStrRes(AbUserAbort));
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoInflateProgress(aPercentDone: integer);
var
  Abort: boolean;
begin
  DoProgress(aPercentDone, Abort);
  if Abort then
    raise EAbAbortProgress.Create(AbStrRes(AbUserAbort));
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoLoad;
begin
  if Assigned(FOnLoad) then
    FOnLoad(Self);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoProgress(Progress: byte; var Abort: boolean);
begin
  Abort := False;
  DoArchiveItemProgress(FCurrentItem, Progress, Abort);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoSave;
begin
  if Assigned(FOnSave) then
    FOnSave(Self);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Extract(aItem: TAbArchiveItem; const NewName: string);
{extract an item from the archive}
var
  Index: integer;
begin
  CheckValid;
  Index := FindItem(aItem);
  if Index <> -1 then
    ExtractAt(Index, NewName);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractAt(Index: integer; const NewName: string);
{extract an item from the archive at Index}
var
  Confirm: boolean;
  ErrorClass: TAbErrorClass;
  ErrorCode: integer;
  TempNewName: string;
begin
  CheckValid;
  SaveIfNeeded(FItemList[Index]);
  Lock;
  try
    DoConfirmProcessItem(FItemList[Index], ptExtract, Confirm);
    if not Confirm then
      Exit;
    TempNewName := NewName;
    if (TempNewName = '') then
      TempNewName := TAbArchiveItem(FItemList[Index]).FileName;
    try
      FCurrentItem := FItemList[Index];
      ExtractItemAt(Index, TempNewName);
    except
      on E: Exception do
      begin
        AbConvertException(E, ErrorClass, ErrorCode);
        DoProcessItemFailure(FItemList[Index], ptExtract, ErrorClass,
          ErrorCode);
      end;
    end;
  finally
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractToStream(const aFileName: string;
  aStream: TStream);
{extract an item from the archive at Index directly to a stream}
var
  Confirm: boolean;
  ErrorClass: TAbErrorClass;
  ErrorCode: integer;
  Index: integer;
begin
  CheckValid;
  Index := FindFile(aFileName);
  if (Index = -1) then
    Exit;

  SaveIfNeeded(FItemList[Index]);
  Lock;
  try
    DoConfirmProcessItem(FItemList[Index], ptExtract, Confirm);
    if not Confirm then
      Exit;
    FCurrentItem := FItemList[Index];
    try
      ExtractItemToStreamAt(Index, aStream);
    except
      on E: Exception do
      begin
        AbConvertException(E, ErrorClass, ErrorCode);
        DoProcessItemFailure(FItemList[Index], ptExtract, ErrorClass,
          ErrorCode);
      end;
    end;
  finally
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractFiles(const FileMask: string);
{extract all files from the archive that match the mask}
begin
  ExtractFilesEx(FileMask, '');
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractFilesEx(const FileMask, ExclusionMask: string);
{Extract files matching Filemask except those matching ExclusionMask}
var
  i: integer;
  Abort: boolean;
{$IFNDEF Linux}
  //  Buff : array [0..MAX_PATH] of Char;                      {!!.03}
{$ENDIF}
begin
  {!!.03 - Added}
{$IFDEF Linux}
 { do nothing to BaseDirectory }
{$ELSE}
  // [ 752491 ] I question the need for the following code, it fails the unit test 
  // AbUnZperTest.pas -> TAbUnZipperTests.TestLocale2
  // I have removed this...  If you know your directory exists but
  // this routine says it does not, check to see if your directory
  // is using locale specific characters, if it is uncomment this section
  // if it works, then report it as a bug in the trackers at
  //  sf.net/projects/tpabbrevia  and include the directory name your using
  // the code pages and locale of your system.  Maybe then I might understand
  // why we had this here to begin with.
  //  if AreFileApisANSI then begin
  //    StrPCopy(Buff, BaseDirectory);
  //    OEMToAnsi(Buff, Buff);
  //    BaseDirectory := StrPas(Buff);
  //  end;
{$ENDIF}
  {!!.03 - End Added }

  CheckValid;
  if Count > 0 then
  begin
    for i := 0 to pred(Count) do
    begin
      with TAbArchiveItem(FItemList[i]) do
        if MatchesStoredNameEx(FileMask) then
          if not MatchesStoredNameEx(ExclusionMask) then
            ExtractAt(i, '');
      DoArchiveProgress(AbPercentage(succ(i), Count), Abort);
      if Abort then
        raise EAbUserAbort.Create;
    end;
    DoArchiveProgress(100, Abort);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractTaggedItems;
{extract all tagged items from the archive}
var
  i: integer;
  Abort: boolean;
begin
  CheckValid;
  if Count > 0 then
  begin
    for i := 0 to pred(Count) do
    begin
      with TAbArchiveItem(FItemList[i]) do
        if Tagged then
          ExtractAt(i, '');
      DoArchiveProgress(AbPercentage(succ(i), Count), Abort);
      if Abort then
        raise EAbUserAbort.Create;
    end;
    DoArchiveProgress(100, Abort);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.TestTaggedItems;
{test all tagged items in the archive}
var
  i: integer;
  Abort: boolean;
begin
  CheckValid;
  if Count > 0 then
  begin
    for i := 0 to pred(Count) do
    begin
      with TAbArchiveItem(FItemList[i]) do
        if Tagged then
        begin
          FCurrentItem := FItemList[i];
          TestItemAt(i);
        end;
      DoArchiveProgress(AbPercentage(succ(i), Count), Abort);
      if Abort then
        raise EAbUserAbort.Create;
    end;
    DoArchiveProgress(100, Abort);
  end;
end;

{ -------------------------------------------------------------------------- }
function TAbArchive.FindFile(const aFileName: string): integer;
  {find the index of the specified file}
begin
  Result := FItemList.Find(aFileName);
end;

{ -------------------------------------------------------------------------- }
function TAbArchive.FindItem(aItem: TAbArchiveItem): integer;
  {find the index of the specified item}
begin
  Result := FItemList.Find(aItem.FileName);
end;

{ -------------------------------------------------------------------------- }
function TAbArchive.FixName(const Value: string): string;
var
  lValue: string;
begin
  lValue := Value;
  {$IFDEF MSWINDOWS}
  if DOSMode then
  begin
    {Add the base directory to the filename before converting }
    {the file spec to the short filespec format. }
    if BaseDirectory <> '' then
    begin
      {Does the filename contain a drive or a leading backslash? }
      if not ((Pos(':', lValue) = 2) or (Pos(AbPathDelim, lValue) = 1)) then
        {If not, add the BaseDirectory to the filename.}
        lValue := AbAddBackSlash(BaseDirectory) + lValue;                {!!.04}
    end;
    lValue := AbGetShortFileSpec(lValue);
  end;
  {$ENDIF}

  {strip drive stuff}
  if soStripDrive in StoreOptions then
    AbStripDrive(lValue);

  {check for a leading backslash}
  if lValue[1] = AbPathDelim then
    System.Delete(lValue, 1, 1);

  if soStripPath in StoreOptions then
  begin
    lValue := ExtractFileName(lValue);
  end;

  if soRemoveDots in StoreOptions then
    AbStripDots(lValue);

  Result := lValue;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Freshen(aItem: TAbArchiveItem);
{freshen the item}
var
  Index: integer;
  //  temp : String;
begin
  CheckValid;
  Index := FindItem(aItem);

  if Index <> -1 then
  begin
    // [ 892830 ] freshing file it doesn't set the correct Item.DiskFileName
    if AbGetPathType(aItem.DiskFileName) = ptAbsolute then   {!!.05}
    begin
      FItemList[Index].DiskFileName := aItem.DiskFileName;  {!!.05}
      //       FItemList[Index].FileName     := aItem.DiskFileName;  {!!.05}
    end;
    FreshenAt(Index);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.FreshenAt(Index: integer);
{freshen item at index}
var
  Confirm: boolean;
  FR: boolean;
  ErrorClass: TAbErrorClass;
  ErrorCode: integer;
begin
  CheckValid;
  SaveIfNeeded(FItemList[Index]);
  Lock;
  try
    GetFreshenTarget(FItemList[Index]);
    FR := False;
    try
      FR := FreshenRequired(FItemList[Index]);
    except
      on E: Exception do
      begin
        AbConvertException(E, ErrorClass, ErrorCode);
        DoProcessItemFailure(FItemList[Index], ptFreshen, ErrorClass,
          ErrorCode);
      end;
    end;

    if not FR then
      Exit;

    DoConfirmProcessItem(FItemList[Index], ptFreshen, Confirm);

    if not Confirm then
      Exit;

    TAbArchiveItem(FItemList[Index]).Action := aaFreshen;
    FIsDirty := True;

    if AutoSave then
      Save;
  finally
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.FreshenFiles(const FileMask: string);
{freshen all items that match the file mask}
begin
  FreshenFilesEx(FileMask, '');
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.FreshenFilesEx(const FileMask, ExclusionMask: string);
{freshen all items that match the file mask}
var
  i: integer;
begin
  CheckValid;
  if Count > 0 then
  begin
    for i := pred(Count) downto 0 do
    begin
      with TAbArchiveItem(FItemList[i]) do
        if MatchesStoredNameEx(FileMask) then
          if not MatchesStoredNameEx(ExclusionMask) then
            FreshenAt(i);
    end;
  end;
end;

{ -------------------------------------------------------------------------- }
function TAbArchive.FreshenRequired(Item: TAbArchiveItem): boolean;
type
  DW = packed record
    Lo: word;
    Hi: word;
  end;
var
  FS: TFileStream;
  DateTime: longint;
  FileTime: word;
  FileDate: word;
  Matched: boolean;
  SaveDir: string;
begin
  GetDir(0, SaveDir);
  if BaseDirectory <> '' then
    ChDir(BaseDirectory);
  try
    FS := TFileStream.Create(Item.DiskFileName,
      fmOpenRead or fmShareDenyWrite);
    try
      DateTime := FileGetDate(FS.Handle);
      FileTime := DW(DateTime).Lo;
      FileDate := DW(DateTime).Hi;
      Matched := (Item.LastModFileDate = FileDate) and
        (Item.LastModFileTime = FileTime);
      Result := not Matched;
      // 887909 soFreshen isn't working (Specifically when date/time the same)
      if not Result then { Check for size change }
        Result := (FS.Size <> Item.UncompressedSize);
    finally
      FS.Free;
    end;
  finally
    if BaseDirectory <> '' then
      ChDir(SaveDir);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.FreshenTaggedItems;
{freshen all tagged items}
var
  i: integer;
begin
  CheckValid;
  if Count > 0 then
  begin
    for i := pred(Count) downto 0 do
    begin
      with TAbArchiveItem(FItemList[i]) do
        if Tagged then
          FreshenAt(i);
    end;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.GetFreshenTarget(Item: TAbArchiveItem);
var
  PathType: TAbPathType;
  Files: TStrings;
  SaveDir: string;
  DName: string;
begin
  PathType := AbGetPathType(Item.FileName);
  if (soRecurse in StoreOptions) and (PathType = ptNone) then
  begin
    GetDir(0, SaveDir);
    if BaseDirectory <> '' then
      ChDir(BaseDirectory);
    try
      Files := TStringList.Create;
      try
        AbFindFiles(Item.FileName, faAnyFile and not faDirectory, Files,
          True);
        if Files.Count > 0 then
        begin
          DName := AbAddBackSlash(BaseDirectory) + Files[0];           {!!.04}
          AbUnfixName(DName);
          Item.DiskFileName := DName;
        end
        else
          Item.DiskFileName := '';
      finally
        Files.Free;
      end;
    finally
      if BaseDirectory <> '' then
        ChDir(SaveDir);
    end;
  end
  else
  begin
    if (BaseDirectory <> '') then
      DName := AbAddBackSlash(BaseDirectory) + Item.FileName           {!!.04}
    else
    if AbGetPathType(Item.DiskFileName) = ptAbsolute then
      DName := Item.DiskFileName
    else
      DName := Item.FileName;
    AbUnfixName(DName);
    Item.DiskFileName := DName;
  end;
end;

{ -------------------------------------------------------------------------- }
function TAbArchive.GetSpanningThreshold: longint;
begin
  Result := FSpanningThreshold;
end;

{ -------------------------------------------------------------------------- }
function TAbArchive.GetItemCount: integer;
begin
  if Assigned(FItemList) then
    Result := FItemList.Count
  else
    Result := 0;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Init;
begin
  FIsDirty := False;
  FAutoSave := False;
  FItemList := TAbArchiveList.Create;
  FPadLock := TAbPadLock.Create;
  StoreOptions := [];
  ExtractOptions := [];
  FStatus := asIdle;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Load;
{load the archive}
begin
  Lock;
  try
    LoadArchive;
    FStatus := asIdle;
  finally
    DoLoad;
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Lock;
begin
  FPadLock.Locked := True;
  FStatus := asBusy;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.MakeLogEntry(const FN: string; LT: TAbLogType);
var
  Buf: array[0..255] of char;
begin
  if Assigned(FLogStream) then
  begin
    case LT of
      ltAdd: StrPCopy(Buf, FN + AbStrRes(AbLtAdd) +
          DateTimeToStr(Now) + CRLF);
      ltDelete: StrPCopy(Buf, FN + AbStrRes(AbLtDelete) +
          DateTimeToStr(Now) + CRLF);
      ltExtract: StrPCopy(Buf, FN + AbStrRes(AbLtExtract) +
          DateTimeToStr(Now) + CRLF);
      ltFreshen: StrPCopy(Buf, FN + AbStrRes(AbLtFreshen) +
          DateTimeToStr(Now) + CRLF);
      ltMove: StrPCopy(Buf, FN + AbStrRes(AbLtMove) +
          DateTimeToStr(Now) + CRLF);
      ltReplace: StrPCopy(Buf, FN + AbStrRes(AbLtReplace) +
          DateTimeToStr(Now) + CRLF);
      ltStart: StrPCopy(Buf, FN + AbStrRes(AbLtStart) +
          DateTimeToStr(Now) + CRLF);
      ltFoundUnhandled: StrPCopy(Buf, FN + AbStrRes(AbUnhandledEntity) +
          DateTimeToStr(Now) + CRLF);
    end;
    FLogStream.Write(Buf, StrLen(Buf));
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Move(aItem: TAbArchiveItem; const NewStoredPath: string);
var
  Confirm: boolean;
  Found: boolean;
  i: integer;
begin
  CheckValid;
  Found := False;
  if Count > 0 then
    for i := 0 to pred(Count) do
      with TAbArchiveItem(FItemList[i]) do
      begin
        if CompareText(FixName(NewStoredPath), FileName) = 0 then
          if Action <> aaDelete then
          begin
            Found := True;
            break;
          end;
      end;
  if Found then
  begin
    DoProcessItemFailure(aItem, ptMove, ecAbbrevia, AbDuplicateName);
    {even if something gets done in the AddItemFailure, we don't
     want to continue...}
    Exit;
  end;

  SaveIfNeeded(aItem);
  Lock;
  try
    DoConfirmProcessItem(aItem, ptMove, Confirm);
    if not Confirm then
      Exit;

    with aItem do
    begin
      FileName := FixName(NewStoredPath);
      Action := aaMove;
    end;
    FIsDirty := True;
    if AutoSave then
      Save;
  finally
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Replace(aItem: TAbArchiveItem);
{replace the item}
var
  Index: integer;
begin
  CheckValid;
  Index := FindItem(aItem);
  if Index <> -1 then
    ReplaceAt(Index);
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.ReplaceAt(Index: integer);
{replace item at Index}
var
  Confirm: boolean;
begin
  CheckValid;
  SaveIfNeeded(FItemList[Index]);
  Lock;
  try
    GetFreshenTarget(FItemList[Index]);
    DoConfirmProcessItem(FItemList[Index], ptReplace, Confirm);
    if not Confirm then
      Exit;

    TAbArchiveItem(FItemList[Index]).Action := aaReplace;
    FIsDirty := True;
    if AutoSave then
      Save;
  finally
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Save;
{save the archive}
var
  Confirm: boolean;
begin
  if Status = asInvalid then
    Exit;
  if (not FIsDirty) and (Count > 0) then
    Exit;
  Lock;
  try
    DoConfirmSave(Confirm);
    if not Confirm then
      Exit;

    SaveArchive;
    FIsDirty := False;
    DoSave;
  finally
    Unlock;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.SaveIfNeeded(aItem: TAbArchiveItem);
begin
  if (aItem.Action <> aaNone) then
    Save;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.SetBaseDirectory(Value: string);
begin
  if (Value <> '') then
    if Value[Length(Value)] = AbPathDelim then
      if (Length(Value) > 1) and (Value[Length(Value) - 1] <> ':') then
        System.Delete(Value, Length(Value), 1);
  if (Length(Value) = 0) or AbDirectoryExists(Value) then
    FBaseDirectory := Value
  else
    raise EAbNoSuchDirectory.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.SetSpanningThreshold(Value: longint);
begin
  FSpanningThreshold := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.SetLogFile(const Value: string);
begin
  FLogFile := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.SetLogging(Value: boolean);
begin
  FLogging := Value;
  if Assigned(FLogStream) then
  begin
    FLogStream.Free;
    FLogStream := nil;
  end;
  if FLogging and (FLogFile <> '') then
  begin
    try
      FLogStream := TFileStream.Create(FLogFile, fmCreate or fmOpenWrite);
      MakeLogEntry(FArchiveName, ltStart);
    except
      raise EAbException.Create(AbLogCreateErrorS);                      {!!.02}
    end;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.TagItems(const FileMask: string);
{tag all items that match the mask}
var
  i: integer;
begin
  if Count > 0 then
    for i := 0 to pred(Count) do
      with TAbArchiveItem(FItemList[i]) do
      begin
        if MatchesStoredNameEx(FileMask) then
          Tagged := True;
      end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.Unlock;
begin
  if FStatus = asBusy then
    FStatus := asIdle;
  FPadLock.Locked := False;
end;

{ -------------------------------------------------------------------------- }
procedure TAbArchive.UnTagItems(const FileMask: string);
{clear tags for all items that match the mask}
var
  i: integer;
begin
  if Count > 0 then
    for i := 0 to pred(Count) do
      with TAbArchiveItem(FItemList[i]) do
      begin
        if MatchesStoredNameEx(FileMask) then
          Tagged := False;
      end;
end;

{ ========================================================================== }

procedure TAbArchive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: integer; var ImageName: string; var Abort: boolean);
begin
  raise EAbSpanningNotSupported.Create;
end;

{ TAbArchiveStreamHelper }

constructor TAbArchiveStreamHelper.Create(AStream: TStream);
begin
  if Assigned(AStream) then
    FStream := AStream
  else
    raise Exception.Create('nil stream');
end;

end.
