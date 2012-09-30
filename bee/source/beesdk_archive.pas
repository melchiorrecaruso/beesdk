unit BeeSDK_Archive;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  Bee_Configuration,
  Bee_Common,
  Bee_Files,
  Bee_Types,
  Bee_Consts,
  Bee_MainPacker,

  BeeLib_Configuration;

const
  /// beex archive marker
  beexMARKER   = $1A656542;

  /// beex archive version
  beexVERSION  = 1;

  /// archive item type
  aitItem      = $01;
  aitBinding   = $02;
  aitLocator   = $7E;
  aitEnd       = $7F;

  /// archive compression type
  actNone      = $00;
  actMain      = $01;

  /// archive encryption type
  acrtNone     = $00;
  acrtMain     = $01;

type
  /// archive locator flag
  TArchiveLocatorFlag = (
    alfDisksNumber,
    alfDiskNumber);

  TArchiveLocatorFlags = set of TArchiveLocatorFlag;

  /// archive binding flag
  TArchiveBindingFlag = (
    abfID,
    abfCRC,
    abfSelfExtractorSize,
    abfComment);

  TArchiveBindingFlags = set of TArchiveBindingFlag;

  /// archive item flag
  TArchiveItemFlag = (
    aifSessionFlags,
    aifUncompressedSize,
    aifCreationTime,
    aifLastModifiedTime,
    aifLastAccessTime,
    aifAttributes,
    aifMode,
    aifCRC,
    aifDiskNumber,
    aifDiskSeek,
    aifUserID,
    aifUserName,
    aifGroupID,
    aifGroupName,
    aifComment,
    aifCompressionMethod,
    aifEncryptionMethod);

  TArchiveItemFlags = set of TArchiveItemFlag;

  /// archive compression flag
  TArchiveCompressionFlag = (
    acfCompressionLevel,
    acfDictionaryLevel,
    acfSolidCompression,
    acfCompressionTable);

  TArchiveCompressionFlags = set of TArchiveCompressionFlag;

type
  TArchiveLocator = class(TObject)
   private
     FFlags: TArchiveLocatorFlags;
     FDisksNumber: longword;
     FDiskNumber: longword;
     FDiskSeek: int64;
     procedure SetDisksNumber(Value: longword);
     procedure SetDiskNumber(Value: longword);
   public
     constructor Create;
     procedure Read(Stream: TFileReader);
     procedure Write(Stream: TFileWriter);
   public
     property Flags: TArchiveLocatorFlags read FFlags;
     property DisksNumber: longword read FDisksNumber write SetDisksNumber;
     property DiskNumber: longword read FDiskNumber write SetDiskNumber;
     property DiskSeek: int64 read FDiskSeek write FDiskSeek;
   end;

  TBeeArchiveBinding = class(TObject)
  private
    FFlags: TArchiveBindingFlags;
    FID: string;
    FCRC: longword;
    FSfxSize: longword;
    FComment: string;
    procedure SetID(const Value: string);
    procedure SetCRC(Value: longword);
    procedure SetSfxSize(Value: longword);
    procedure SetComment(const Value: string);
  public
    constructor Create;
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
  public
    property Flags: TArchiveBindingFlags read FFlags;
    property ID: string read FID write SetID;
    property CRC: longword read FCRC write SetCRC;
    property SfxSize: longword read FSfxSize write SetSfxSize;
    property Comment: string read FComment write SetComment;
  end;

  TArchiveItemTag = (aitNone, aitAdd, aitUpdate, aitDecode, aitDecodeAndUpdate);

  TArchiveItem = class(TObject)
  protected
    FFlags: TArchiveItemFlags;
    FUncompressedSize: int64;
    FCreationTime: longword;
    FLastModifiedTime: longword;
    FLastAccessTime: longword;
    FAttributes: longword;
    FMode: longword;
    FCRC: longword;
    FDiskNumber: longword;
    FDiskSeek: int64;
    FUserID: longword;
    FUserName: string;
    FGroupID: longword;
    FGroupName: string;
    FComment: string;
    FFileName: string;
    FCompressionMethod: longword;
    FCompressionFlags: TArchiveCompressionFlags;
    FCompressedSize: int64;
    FCompressionLevel: longword;
    FDictionaryLevel: longword;
    FCompressionTable: TTableParameters;
    FEncryptionMethod: longword;

    FPosition: longint;
    FTag: TArchiveItemTag;
    FExternalFileName: string;
    function GetSolidCompression: boolean;
    procedure SetUncompressedSize(const Value: int64);
    procedure SetCreationTime(Value: longword);
    procedure SetLastModifiedTime(Value: longword);
    procedure SetLastAccessTime(Value: longword);
    procedure SetAttributes(Value: longword);
    procedure SetMode(Value: longword);
    procedure SetCRC(Value: longword);
    procedure SetDiskNumber(Value: longword);
    procedure SetDiskSeek(const Value: int64);
    procedure SetUserID(Value: longword);
    procedure SetUserName(const Value: string);
    procedure SetGroupID(Value: longword);
    procedure SetGroupName(const Value: string);
    procedure SetComment(const Value: string);
    procedure SetCompressionMethod(Value: longword);
    procedure SetCompressionLevel(Value: longword);
    procedure SetDictionaryLevel(Value: longword);
    procedure SetSolidCompression(Value: boolean);
    procedure SetCompressionTable(Value: TTableParameters);
    procedure ClearCompressionTable;
    procedure SetEncryptionMethod(Value: longword);
  public {methods}
    constructor Create(SearchRec: TCustomSearchRec; UseFlags: TArchiveItemFlags;
      const UseFileName: string; const UseComment: string);
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
  public {property}
    property Flags: TArchiveItemFlags read FFlags;
    property UncompressedSize: int64 read FUncompressedSize;
    property CreationTime: longword read FCreationTime;
    property LastModifiedTime: longword read FLastModifiedTime;
    property LastAccessTime: longword read FLastAccessTime;
    property Attributes: longword read FAttributes;
    property Mode: longword read FMode;
    property CRC: longword read FCRC;
    property DiskNumber: longword read FDiskNumber;
    property DiskSeek: int64 read FDiskSeek;
    property UserID: longword read FUserID;
    property UserName: string read FUserName;
    property GroupID: longword read FGroupID;
    property GroupName: string read FGroupName;
    property Comment: string read FComment;
    property FileName: string read FFileName;

    property CompressionMethod: longword read FCompressionMethod;
    property CompressionFlags: TArchiveCompressionFlags read FCompressionFlags;
    property CompressedSize: int64 read FCompressedSize;
    property CompressionLevel: longword read FCompressionLevel;
    property DictionaryLevel: longword read FDictionaryLevel;
    property SolidCompression: boolean read GetSolidCompression;
    property CompressionTable: TTableParameters read FCompressionTable;
    property EncryptionMethod: longword read FEncryptionMethod;

    property Position: longint read FPosition;
  end;

  TBeeArchiveCustomItems = class(TObject)
  private {private}
    FItems: TList;
    FNames: TList;
  private { methods}
    function GetCount : longint;
    function GetItem(Index: longint): TArchiveItem;
    function GetNameIndex(const FileName: string): longint;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item : TArchiveItem);
    procedure Delete(Index: longint);
    procedure Clear;
    function Find(const FileName: string): longint;
  public {properties}
    property Count: longint read GetCount;
    property Items[Index: longint]: TArchiveItem read GetItem;
  end;

  TArchiveProgressEvent = procedure(Value: longint) of object;

  TArchiveMessageEvent = procedure(const Message: string) of object;

  TArchiveFailureEvent = procedure(
    const ErrorMessage: string; ErrorCode: longint) of object;

  TArchiveConfirm = (arcOk, arcCancel, arcAbort);

  TArchiveRenameEvent = procedure(Item: TArchiveItem;
    var RenameAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveExtractEvent = procedure(Item: TArchiveItem;
    var ExtractAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveEraseEvent = procedure(Item: TArchiveItem;
    var Confirm: TArchiveConfirm) of object;

  TArchiveUpdateEvent = procedure(Item: TArchiveItem;
    var Confirm: TArchiveConfirm) of object;

  // a class for each command

  TArchiveReaderBase = class(TObject)
  private
    FDecoder: THeaderDecoder;
    FTotalSize: int64;
    FProcessedSize: int64;
    FArchiveName: string;
    FArchiveReader: TFileReader;
    FSwapName: string;
    FSwapReader: TFileReader;
    FSwapWriter: TFileWriter;
    FSuspended:  boolean;
    FTerminated: boolean;
    FExitCode: byte;
    FOnFailure: TArchiveFailureEvent;
    FOnMessage: TArchiveMessageEvent;
    FOnProgress: TArchiveProgressEvent;
    FOnRequestImage: TFileReaderRequestImageEvent;
    FArchiveCustomItems: TBeeArchiveCustomItems;
    FArchiveBindingItem: TBeeArchiveBinding;
    FArchiveLocatorItem: TArchiveLocator;
    procedure InitDecoder (Item: TArchiveItem);
    procedure DecodeToSwap(Item: TArchiveItem);
    procedure DecodeToNil (Item: TArchiveItem);
    procedure DecodeToFile(Item: TArchiveItem);
    function Read(aStream: TFileReader): boolean;
    function GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetBackTear(Index: longint): longint;
    function GetNextTear(Index: longint): longint;
    function GetCount: longint;
    function GetItem(Index: longint): TArchiveItem;
    procedure SetArchiveName(const Value: string);
    procedure SetSuspended(Value: boolean);
    procedure SetTerminated(Value: boolean);
    procedure SetExitCode(Value: byte);
    procedure DoFailure(const ErrorMessage: string);
    procedure DoMessage(const Message: string);
    function DoProgress(Value: longint): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenArchive(const aArchiveName: string);
    procedure CloseArchive; virtual;
    procedure Terminate;

    function Find(const aFileName: string): longint;
  public
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property Items[Index: longint]: TArchiveItem read GetItem;
    property Count: longint read GetCount;

    property Suspended: boolean read FSuspended write SetSuspended;
    property Terminated: boolean read FTerminated;
    property ExitCode: byte read FExitCode;

    property OnFailure: TArchiveFailureEvent read FOnFailure write FOnFailure;
    property OnMessage: TArchiveMessageEvent read FOnMessage write FOnMessage;
    property OnProgress: TArchiveProgressEvent read FOnProgress write FOnProgress;
    property OnRequestImage: TFileReaderRequestImageEvent
      read FOnRequestImage write FOnRequestImage;
  end;

  TArchiveReader = class(TArchiveReaderBase)
  public
    procedure TagAll;
    procedure Tag(Index: longint); overload;
    procedure Tag(const FileMask: string; Recursive: TRecursiveMode); overload;
    procedure UnTagAll;
    procedure UnTag(Index: longint); overload;
    procedure UnTag(const FileMask: string; Recursive: TRecursiveMode); overload;
  end;

  TArchiveWriterBase = class(TArchiveReaderBase)
  private
    FEncoder: THeaderEncoder;
    FIsNeededToSave: boolean;
    FIsNeededToSwap: boolean;
    FThreshold: int64;
    FTempName: string;
    FTempWriter: TFileWriter;
    FWorkDirectory: string;
    FOnRequestBlankDisk: TFileWriterRequestBlankDiskEvent;
    procedure InitEncoder      (Item: TArchiveItem);
    procedure EncodeFromArchive(Item: TArchiveItem);
    procedure EncodeFromSwap   (Item: TArchiveItem);
    procedure EncodeFromFile   (Item: TArchiveItem);
  private
    procedure Write(aStream: TFileWriter);
    procedure Pack;
    function OpenSwap: longint;
    procedure SetWorkDirectory(const Value: string);
  public
    constructor Create;
    procedure CloseArchive; override;
  public
    property Threshold: int64 read FThreshold write FThreshold;
    property WorkDirectory: string read FWorkDirectory write SetWorkDirectory;
    property OnRequestBlankDisk: TFileWriterRequestBlankDiskEvent
      read FOnRequestBlankDisk write FOnRequestBlankDisk;
  end;

  TArchiveWriter = class(TArchiveWriterBase)
  public
    procedure TagAll;
    procedure Tag(Index: longint); overload;
    procedure Tag(const FileMask: string; Recursive: TRecursiveMode); overload;
    procedure UnTagAll;
    procedure UnTag(Index: longint); overload;
    procedure UnTag(const FileMask: string; Recursive: TRecursiveMode); overload;
  end;

  TBeeArchiveExtractor = class(TArchiveReader)
  private
    FIsNeededToExtract: boolean;
    FOnExtract: TArchiveExtractEvent;
    procedure CheckTags;
    procedure CheckSequences;
    procedure DoExtract(Item: TArchiveItem;
      var ExtractAs: string; var Confirm: TArchiveConfirm);
  public
    constructor Create;
    procedure ExtractTagged;
    procedure TestTagged;
  public
    property OnExtraction: TArchiveExtractEvent
      read FOnExtract write FOnExtract;
  end;

  TBeeArchiveRenamer = class(TArchiveWriter)
  private
    FOnRename: TArchiveRenameEvent;
    procedure CheckTags;
    procedure DoRename(Item: TArchiveItem;
      var RenameAs: string; var Confirm: TArchiveConfirm);
  public
    procedure RenameTagged;
  public
    property OnRenameEvent: TArchiveRenameEvent read FOnRename write FOnRename;
  end;

  TBeeArchiveEraser = class(TArchiveWriter)
  private
    FOnErase: TArchiveEraseEvent;
    procedure CheckTags;
    procedure CheckSequences;
    procedure DoErase(Item: TArchiveItem; var Confirm: TArchiveConfirm);
  public
    procedure EraseTagged;
  public
    property OnEraseEvent: TArchiveEraseEvent read FOnErase write FOnErase;
  end;

  TBeeArchiveUpdater = class(TArchiveWriterBase)
  private
    FCompressionMethod: longint;
    FCompressionLevel: longint;
    FDictionaryLevel: longint;
    FSolidCompression: boolean;
    FConfigurationName: string;
    FConfiguration: TConfiguration;
    FForceFileExtension: string;
    FOnUpdate: TArchiveUpdateEvent;
    procedure SetCompressionMethod(Value: longint);
    procedure SetCompressionLevel(Value: longint);
    procedure SetDictionaryLevel(Value: longint);
    procedure SetConfigurationName(const Value: string);
    procedure SetForceFileExtension(const Value: string);

    // procedure Configure;
    procedure CheckTags;
    procedure CheckSequences;

    //procedure DoUpdate(Item: TArchiveItem;
    //  var UseExternalFileName: string; var Confirm: TArchiveConfirm);
  public
    constructor Create;

    // procedure UpdateTagged;
    // procedure Update(SearchRec: TCustomSearchRec;
    //  const UseFileName: string; UseFlags: TArchiveItemFlags);
  public
    property CompressionMethod: longint
      read FCompressionLevel write SetCompressionLevel;
    property CompressionLevel: longint
      read FCompressionLevel write SetCompressionLevel;
    property DictionaryLevel: longint
      read FDictionaryLevel write SetDictionaryLevel;
    property SolidCompression: boolean
      read FSolidCompression write FSolidCompression;

    property ConfigurationName: string
      read FConfigurationName write SetConfigurationName;
    property ForceFileExtension: string
      read FForceFileExtension write FForceFileExtension;

    property OnUpdateEvent: TArchiveUpdateEvent read FOnUpdate write FOnUpdate;
  end;




implementation

// Read archive items routines

function ReadMagicSeek(Stream: TFileReader): int64;
begin


end;

// Write archive items routines

procedure WriteMagicSeek(Stream: TFileWriter; const MagicSeek: int64);
begin

end;

// TBeeArchiveLolatorItem class

constructor TArchiveLocator.Create;
begin
  inherited Create;
  FFlags := [];
  FDisksNumber := 0;
  FDiskNumber  := 0;
end;

procedure TArchiveLocator.Read(Stream: TFileReader);
begin
  FFlags := TArchiveLocatorFlags(longword(Stream.ReadInfWord));
  if (alfDisksNumber in FFlags) then
    FDisksNumber := Stream.ReadInfWord
  else
    FDisksNumber := 0;

  if (alfDiskNumber in FFlags) then
    FDiskNumber := Stream.ReadInfWord
  else
     FDiskNumber := 0;

  FDiskSeek := ReadMagicSeek(Stream);
end;

procedure TArchiveLocator.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));

  if (alfDisksNumber in FFlags) then
    Stream.WriteInfWord(FDisksNumber);

  if (alfDiskNumber in FFlags) then
    Stream.WriteInfWord(FDiskNumber);

  WriteMagicSeek(Stream, FDiskSeek);
end;

procedure TArchiveLocator.SetDisksNumber(Value: longword);
begin
  FDisksNumber := Value;
  if FDisksNumber <> 0 then
    Include(FFlags, alfDisksNumber)
  else
    Exclude(FFlags, alfDisksNumber);
end;

procedure TArchiveLocator.SetDiskNumber(Value: longword);
begin
  FDiskNumber := Value;
  if FDiskNumber <> 0 then
    Include(FFlags, alfDiskNumber)
  else
    Exclude(FFlags, alfDiskNumber);
end;

// TBeeArchiveBinding class

constructor TBeeArchiveBinding.Create;
begin
  inherited Create;
  FFlags   := [];
  FID      := '';
  FCRC     :=  0;
  FSfxSize :=  0;
  FComment := '';
end;

constructor TBeeArchiveBinding.Read(Stream: TFileReader);
begin
  inherited Create;
  FFlags := TArchiveBindingFlags(longword(Stream.ReadInfWord));
  if (abfID  in FFlags) then
    FID := Stream.ReadInfString
  else
    FID := '';

  if (abfCRC in FFlags) then
    FCRC := Stream.ReadInfWord
  else
    FCRC :=  0;

  if (abfSelfExtractorSize in FFlags) then
    FSfxSize := Stream.ReadInfWord
  else
    FSfxSize :=  0;

  if (abfComment in FFlags) then
    FComment := Stream.ReadInfString
  else
    FComment := '';
end;

procedure TBeeArchiveBinding.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));

  if (abfID in FFlags) then
    Stream.WriteInfString(FID);

  if (abfCRC in FFlags) then
    Stream.WriteInfWord(FCRC);

  if (abfSelfExtractorSize in FFlags) then
    Stream.WriteInfWord(FSfxSize);

  if (abfComment in FFlags) then
    Stream.WriteInfString(FComment);
end;

procedure TBeeArchiveBinding.SetID(const Value: string);
begin
  FID := Value;
  if FID <> '' then
    Include(FFlags, abfID)
  else
    Exclude(FFlags, abfID);
end;

procedure TBeeArchiveBinding.SetCRC(Value: longword);
begin
  FCRC := Value;
  if FCRC <> 0 then
    Include(FFlags, abfCRC)
  else
    Exclude(FFlags, abfCRC);
end;

procedure TBeeArchiveBinding.SetSfxSize(Value: longword);
begin
  FSfxSize := Value;
  if FSfxSize <> 0 then
  begin
    Include(FFlags, abfSelfExtractorSize)
  end else
    Exclude(FFlags, abfSelfExtractorSize);
end;

procedure TBeeArchiveBinding.SetComment(const Value: string);
begin
  FComment := Value;
  if FComment <> '' then
    Include(FFlags, abfComment)
  else
    Exclude(FFlags, abfComment);
end;

// TArchiveItem class

constructor TArchiveItem.Create(SearchRec: TCustomSearchRec;
  UseFlags: TArchiveItemFlags; const UseFileName: string;
  const UseComment: string);
begin
  inherited Create;
  FFlags := UseFlags;
  Exclude(FFlags, aifSessionFlags);

  if (aifUncompressedSize in FFLags) then FUncompressedSize := SearchRec.Size;
  if (aifCreationTime     in FFLags) then FCreationTime     := SearchRec.CreationTime;
  if (aifLastModifiedTime in FFLags) then FLastModifiedTime := SearchRec.LastModifiedTime;
  if (aifLastAccessTime   in FFLags) then FLastAccessTime   := SearchRec.LastAccessTime;
  if (aifAttributes       in FFLags) then FAttributes       := SearchRec.Attributes;
  if (aifMode             in FFLags) then FMode             := SearchRec.Mode;
//if (aifCRC              in FFLags) then FCRC              := 0;
//if (aifDiskNumber       in FFLags) then FDiskNumber       := 0;
//if (aifDiskSeek         in FFLags) then FDiskSeek         := 0;
  if (aifUserID           in FFLags) then FUserID           := SearchRec.UserID;
  if (aifUserName         in FFLags) then FUserName         := SearchRec.UserName;
  if (aifGroupID          in FFLags) then FGroupID          := SearchRec.GroupID;
  if (aifGroupName        in FFLags) then FGroupName        := SearchRec.GroupName;
  if (aifComment          in FFLags) then FComment          := UseComment;

  FFileName := UseFileName;

  // data descriptor
  FCompressionMethod := 0;
  FCompressionFlags  := [];
//FCompressedSize    := 0;
//FCompressionLevel  := 0;
//FDictionaryLevel   := 0;
//FCompressionTable  := nil;
  FEncryptionMethod  := 0;

//FPosition := -1;
  FTag := aitAdd;
  FExternalFileName := SearchRec.Name;
end;

constructor TArchiveItem.Read(Stream: TFileReader);
begin
  FFlags := TArchiveItemFlags(longword(Stream.ReadInfWord));

  if (aifUncompressedSize in FFlags) then FUncompressedSize := Stream.ReadInfWord;
  if (aifCreationTime     in FFlags) then FCreationTime     := Stream.ReadInfWord;
  if (aifLastModifiedTime in FFlags) then FLastModifiedTime := Stream.ReadInfWord;
  if (aifLastAccessTime   in FFlags) then FLastAccessTime   := Stream.ReadInfword;
  if (aifAttributes       in FFlags) then FAttributes       := Stream.ReadInfWord;
  if (aifMode             in FFlags) then FMode             := Stream.ReadInfWord;
  if (aifCRC              in FFlags) then FCRC              := Stream.ReadInfWord;
  if (aifDiskNumber       in FFlags) then FDiskNumber       := Stream.ReadInfWord;
  if (aifDiskSeek         in FFlags) then FDiskSeek         := Stream.ReadInfWord;
  if (aifUserID           in FFlags) then FUserID           := Stream.ReadInfWord;
  if (aifUserName         in FFlags) then FUserName         := Stream.ReadInfString;
  if (aifGroupID          in FFlags) then FGroupID          := Stream.ReadInfWord;
  if (aifGroupName        in FFlags) then FGroupName        := Stream.ReadInfString;
  if (aifComment          in FFlags) then FComment          := Stream.ReadInfString;

  FFileName:= Stream.ReadInfString;

  // data descryptor
  FCompressionMethod := longword(Stream.ReadInfWord);
  case FCompressionMethod of
    1: begin
         FCompressionFlags := TArchiveCompressionFlags(longword(Stream.ReadInfWord));
         if (acfCompressionLevel in FCompressionFlags) then
           FCompressionLevel := Stream.ReadInfWord;
         if (acfDictionaryLevel  in CompressionFlags) then
           FDictionaryLevel  := Stream.ReadInfWord;
         if (acfCompressionTable in FCompressionFlags) then
           Stream.Read(FCompressionTable, SizeOf(TTableParameters));
         FCompressedSize := Stream.ReadInfWord;
       end;
  end;
  FEncryptionMethod := longword(Stream.ReadInfWord);
end;

procedure TArchiveItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));

  if (aifUncompressedSize in FFlags) then Stream.WriteInfWord(FUncompressedSize);
  if (aifCreationTime     in FFlags) then Stream.WriteInfWord(FCreationTime);
  if (aifLastModifiedTime in FFlags) then Stream.WriteInfWord(FLastModifiedTime);
  if (aifLastAccessTime   in FFlags) then Stream.WriteInfWord(FLastAccessTime);
  if (aifAttributes       in FFlags) then Stream.WriteInfWord(FAttributes);
  if (aifMode             in FFlags) then Stream.WriteInfWord(FMode);
  if (aifCRC              in FFlags) then Stream.WriteInfWord(FCRC);
  if (aifDiskNumber       in FFlags) then Stream.WriteInfWord(FDiskNumber);
  if (aifDiskSeek         in FFlags) then Stream.WriteInfWord(FDiskSeek);
  if (aifUserID           in FFlags) then Stream.WriteInfWord(FUserID);
  if (aifUserName         in FFlags) then Stream.WriteInfString(FUserName);
  if (aifGroupID          in FFlags) then Stream.WriteInfWord(FGroupID);
  if (aifGroupName        in FFlags) then Stream.WriteInfString(FGroupName);
  if (aifComment          in FFlags) then Stream.WriteInfString(FComment);

  Stream.WriteInfString(FFileName);

  // data descriptor
  Stream.WriteInfWord(FCompressionMethod);
  case FCompressionMethod of
    1: begin
         Stream.WriteInfWord(longword(FCompressionFlags));
         if (acfCompressionLevel in FCompressionFlags) then
           Stream.WriteInfWord(FCompressionLevel);
         if (acfDictionaryLevel  in CompressionFlags)  then
           Stream.WriteInfWord(FDictionaryLevel);
         if (acfCompressionTable in FCompressionFlags) then
           Stream.Write(FCompressionTable, SizeOf(TTableParameters));

         Stream.WriteInfWord(FCompressedSize);
       end;
  end;
  Stream.WriteInfWord(FEncryptionMethod);
end;

function TArchiveItem.GetSolidCompression: boolean;
begin
  Result := acfSolidCompression in FCompressionFlags;
end;

procedure TArchiveItem.SetUncompressedSize(const Value: int64);
begin
  FUncompressedSize := Value;
  if Value <> 0 then
    Include(FFlags, aifUncompressedSize)
  else
    Exclude(FFlags, aifUncompressedSize);
end;

procedure TArchiveItem.SetCreationTime(Value: longword);
begin
  FCreationTime := Value;
  if Value <> 0 then
    Include(FFlags, aifCreationTime)
  else
    Exclude(FFlags, aifCreationTime);
end;

procedure TArchiveItem.SetLastModifiedTime(Value: longword);
begin
  FLastModifiedTime := Value;
  if Value <> 0 then
    Include(FFlags, aifLastModifiedTime)
  else
    Exclude(FFlags, aifLastModifiedTime);
end;

procedure TArchiveItem.SetLastAccessTime(Value: longword);
begin
  FLastAccessTime := Value;
  if Value <> 0 then
    Include(FFlags, aifLastAccessTime)
  else
    Exclude(FFlags, aifLastAccessTime);
end;

procedure TArchiveItem.SetAttributes(Value: longword);
begin
  FAttributes := Value;
  if Value <> 0 then
    Include(FFlags, aifAttributes)
  else
    Exclude(FFlags, aifAttributes);
end;

procedure TArchiveItem.SetMode(Value: longword);
begin
  FMode := Value;
  if Value <> 0 then
    Include(FFlags, aifMode)
  else
    Exclude(FFlags, aifMode);
end;

procedure TArchiveItem.SetCRC(Value: longword);
begin
  FCRC := Value;
  if Value <> 0 then
    Include(FFlags, aifCRC)
  else
    Exclude(FFlags, aifCRC);
end;

procedure TArchiveItem.SetDiskNumber(Value: longword);
begin
  FDiskNumber := Value;
  if Value <> 0 then
    Include(FFlags, aifDiskNumber)
  else
    Exclude(FFlags, aifDiskNumber);
end;

procedure TArchiveItem.SetDiskSeek(const Value: int64);
begin
  FDiskSeek := Value;
  if Value <> 0 then
    Include(FFlags, aifDiskSeek)
  else
    Exclude(FFlags, aifDiskSeek);
end;

procedure TArchiveItem.SetUserID(Value: longword);
begin
  FUserID := Value;
  if Value <> 0 then
    Include(FFlags, aifUserID)
  else
    Exclude(FFlags, aifUserID);
end;

procedure TArchiveItem.SetUserName(const Value: string);
begin
  FUserName := Value;
  if Value <> '' then
    Include(FFlags, aifUserName)
  else
    Exclude(FFlags, aifUserName);
end;

procedure TArchiveItem.SetGroupID(Value: longword);
begin
  FGroupID := Value;
  if Value <> 0 then
    Include(FFlags, aifGroupID)
  else
    Exclude(FFlags, aifGroupID);
end;

procedure TArchiveItem.SetGroupName(const Value: string);
begin
  FGroupName := Value;
  if Value <> '' then
    Include(FFlags, aifGroupName)
  else
    Exclude(FFlags, aifGroupName);
end;

procedure TArchiveItem.SetComment(const Value: string);
begin
  FComment := Value;
  if Value <> '' then
    Include(FFlags, aifComment)
  else
    Exclude(FFlags, aifComment);
end;

procedure TArchiveItem.SetCompressionMethod(Value: longword);
begin
  FCompressionMethod := Value;
  if Value <> 0 then
    Include(FFlags, aifCompressionMethod)
  else
    Exclude(FFlags, aifCompressionMethod);
end;

procedure TArchiveItem.SetCompressionLevel(Value: longword);
begin
  FCompressionLevel := Value;
  if Value <> 0 then
    Include(FCompressionFlags, acfCompressionLevel)
  else
    Exclude(FCompressionFlags, acfCompressionLevel);
end;

procedure TArchiveItem.SetDictionaryLevel(Value: longword);
begin
  FDictionaryLevel := Value;
  if Value <> 0 then
    Include(FCompressionFlags, acfDictionaryLevel)
  else
    Exclude(FCompressionFlags, acfDictionaryLevel);
end;

procedure TArchiveItem.SetSolidCompression(Value: boolean);
begin
  if Value then
    Include(FCompressionFlags, acfSolidCompression)
  else
    Exclude(FCompressionFlags, acfSolidCompression);
end;

procedure TArchiveItem.SetCompressionTable(Value: TTableParameters);
begin
  FCompressionTable := Value;
  Include(FCompressionFlags, acfCompressionTable);
end;

procedure TArchiveItem.ClearCompressionTable;
begin
  Exclude(FCompressionFlags, acfCompressionTable);
end;

procedure TArchiveItem.SetEncryptionMethod(Value: longword);
begin
  FEncryptionMethod := Value;
  if Value <> 0 then
    Include(FFlags, aifEncryptionMethod)
  else
    Exclude(FFlags, aifEncryptionMethod);
end;

// TBeeArchiveCustomItems class

constructor TBeeArchiveCustomItems.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FNames := TList.Create;
end;

destructor TBeeArchiveCustomItems.Destroy;
begin
  Clear;
  FItems.Destroy;
  FNames.Destroy;
  inherited Destroy;
end;

procedure TBeeArchiveCustomItems.Clear;
var
  I: longint;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    TArchiveItem(FItems[I]).Destroy;
  end;
end;

procedure TBeeArchiveCustomItems.Add(Item: TArchiveItem);
var
  Lo, Med, Hi, I: longint;
begin
  Item.FPosition := FItems.Add(Item);
  if FNames.Count <> 0 then
  begin
    Lo := 0;
    Hi := FNames.Count - 1;
    while Hi >= Lo do
    begin
      Med := (Lo + Hi) div 2;
      I := Bee_Common.CompareFileName(Item.FileName,
        TArchiveItem(FNames[Med]).FileName);

      if I > 0 then
        Lo := Med + 1
      else
        if I < 0 then
          Hi := Med - 1
        else
          Hi := -2;
    end;

    if Hi = -2 then
    begin
      FNames.Insert(Med + 1, Item);
    end else
    begin
      if I > 0 then
        FNames.Insert(Med + 1, Item)
      else
        FNames.Insert(Med, Item);
    end;
  end else
    FNames.Add(Item);
end;

function TBeeArchiveCustomItems.GetNameIndex(const FileName: string): longint;
var
  Lo, Med, Hi, I: longint;
begin
  Lo := 0;
  Hi := FNames.Count - 1;
  while Hi >= Lo do
  begin
    Med := (Lo + Hi) div 2;
    I := Bee_Common.CompareFileName(FileName,
      TArchiveItem(FNames[Med]).FileName);

    if I > 0 then
      Lo := Med + 1
    else
      if I < 0 then
        Hi := Med - 1
      else
        Hi := -2;
  end;

  if Hi = -2 then
    Result := Med
  else
    Result := -1;
end;

function TBeeArchiveCustomItems.Find(const FileName: string): longint;
begin
  Result := GetNameIndex(FileName);
  if Result <> -1 then
  begin
    Result := TArchiveItem(FNames[Result]).Position;
  end;
end;

procedure TBeeArchiveCustomItems.Delete(Index: longint);
var
  Item, Next: TArchiveItem;
begin
  Item := Items[Index];
  if Index < FItems.Count - 1 then
  begin
    Next := Items[Index + 1];

    if  (not(aifSessionFlags in Next.Flags)) then
    begin
      if    (aifUncompressedSize in Item.Flags) and
        (not(aifUncompressedSize in Next.Flags)) then
        Next.SetUncompressedSize(Item.UncompressedSize);

      if    (aifCreationTime in Item.Flags) and
        (not(aifCreationTime in Next.Flags)) then
        Next.SetCreationTime(Item.CreationTime);

      if    (aifLastModifiedTime in Item.Flags) and
        (not(aifLastModifiedTime in Next.Flags)) then
        Next.SetLastModifiedTime(Item.LastModifiedTime);

      if    (aifLastAccessTime in Item.Flags) and
        (not(aifLastAccessTime in Next.Flags)) then
        Next.SetLastAccessTime(Item.LastAccessTime);

      if    (aifAttributes in Item.Flags) and
        (not(aifAttributes in Next.Flags)) then
        Next.SetAttributes(Item.Attributes);

      if    (aifMode in Item.Flags) and
        (not(aifMode in Next.Flags)) then
        Next.SetMode(Item.Mode);

      if    (aifCRC in Item.Flags) and
        (not(aifCRC in Next.Flags)) then
        Next.SetCRC(Item.CRC);

      if    (aifDiskNumber in Item.Flags) and
        (not(aifDiskNumber in Next.Flags)) then
        Next.SetDiskNumber(Item.DiskNumber);

      if    (aifDiskSeek in Item.Flags) and
        (not(aifDiskSeek in Next.Flags)) then
        Next.SetDiskSeek(Item.DiskSeek);

      if    (aifUserID in Item.Flags) and
        (not(aifUserID in Next.Flags)) then
        Next.SetUserID(Item.UserID);

      if    (aifUserName in Item.Flags) and
        (not(aifUserName in Next.Flags)) then
        Next.SetUserName(Item.UserName);

      if    (aifGroupID in Item.Flags) and
        (not(aifGroupID in Next.Flags)) then
        Next.SetGroupID(Item.GroupID);

      if    (aifGroupName in Item.Flags) and
        (not(aifGroupName in Next.Flags)) then
        Next.SetGroupName(Item.GroupName);

      if    (aifComment in Item.Flags) and
        (not(aifComment in Next.Flags)) then
        Next.SetComment(Item.Comment);

      if    (aifCompressionMethod in Item.Flags) and
        (not(aifCompressionMethod in Next.Flags)) then
        Next.SetCompressionMethod(Item.CompressionMethod);

      if    (aifEncryptionMethod in Item.Flags) and
        (not(aifEncryptionMethod in Next.Flags)) then
        Next.SetEncryptionMethod(Item.EncryptionMethod);

      if    (acfCompressionLevel in Item.CompressionFlags) and
        (not(acfCompressionLevel in Item.CompressionFlags)) then
        Next.SetCompressionLevel(Item.CompressionLevel);

      if    (acfDictionaryLevel in Item.CompressionFlags) and
        (not(acfDictionaryLevel in Item.CompressionFlags)) then
        Next.SetDictionaryLevel(Item.DictionaryLevel);

      if    (acfSolidCompression in Item.CompressionFlags) and
        (not(acfSolidCompression in Item.CompressionFlags)) then
         Next.SetSolidCompression(TRUE);

      if    (acfCompressionTable in Item.CompressionFlags) and
        (not(acfCompressionTable in Item.CompressionFlags)) then
        Next.SetCompressionTable(Item.CompressionTable);
    end;
  end;
  FNames.Delete(GetNameIndex(Item.FileName));
  FItems.Delete(Item.Position);
  Item.Destroy;
end;

function TBeeArchiveCustomItems.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TBeeArchiveCustomItems.GetItem(Index: longint): TArchiveItem;
begin
  Result := TArchiveItem(FItems[Index]);
end;

// TArchiveReaderBase class

constructor TArchiveReaderBase.Create;
begin
  inherited Create;
  FExitCode := ccSuccesful;

  FArchiveCustomItems := TBeeArchiveCustomItems.Create;
  FArchiveBindingItem := TBeeArchiveBinding.Create;
  FArchiveLocatorItem := TArchiveLocator.Create;
end;

destructor TArchiveReaderBase.Destroy;
begin
  FArchiveCustomItems.Destroy;
  FArchiveBindingItem.Destroy;
  FArchiveLocatorItem.Destroy;
  inherited Destroy;
end;

procedure TArchiveReaderBase.Terminate;
begin
  SetExitCode(ccUserAbort);
end;

function TArchiveReaderBase.GetCount: longint;
begin
  Result := FArchiveCustomItems.Count;
end;

function TArchiveReaderBase.GetItem(Index: longint): TArchiveItem;
begin
  Result := FArchiveCustomItems.Items[Index];
end;

procedure TArchiveReaderBase.SetTerminated(Value: boolean);
begin
  if FTerminated = False then
  begin
    FTerminated := Value;
    if FTerminated = True then
    begin
      FSuspended := False;
    end;
  end;
end;

procedure TArchiveReaderBase.SetSuspended(Value: boolean);
begin
  if FTerminated = False then
  begin
    FSuspended := Value;
  end;
end;

procedure TArchiveReaderBase.SetExitCode(Value: byte);
begin
  if FTerminated = False then
  begin
    if FExitCode < Value then
    begin
      FExitCode := Value;
      if FExitCode >= ccError then
        SetTerminated(TRUE);
    end;
  end;
end;

procedure TArchiveReaderBase.SetArchiveName(const Value: string);
begin
  OpenArchive(Value);
end;

function TArchiveReaderBase.Read(aStream: TFileReader): boolean;
var
  T: longword;
begin
  Result := FALSE;
  T := longword(aStream.ReadInfWord);
  if T = aitLocator then
  begin
    if longword(aStream.ReadInfWord) <= beexVERSION then Exit;

    FArchiveLocatorItem.Read(aStream);
    if alfDisksNumber in FArchiveLocatorItem.FFlags then
      aStream.ImagesNumber := FArchiveLocatorItem.DisksNumber;
    if alfDiskNumber in FArchiveLocatorItem.FFlags then
      aStream.SeekImage(FArchiveLocatorItem.DiskNumber, FArchiveLocatorItem.DiskSeek);

    if aStream.ReadDWord = beexMARKER then
      repeat
        T := longword(aStream.ReadInfWord);
        if T <> aitEnd then
        begin
          if longword(aStream.ReadInfWord) <= beexVERSION then Exit;

          case T of
            aitItem:    FArchiveCustomItems.Add(TArchiveItem.Read(aStream));
            aitBinding: FArchiveBindingItem.Read(aStream);
          //aitLocator: already readed;
            else        Exit;
          end;
        end;
        Result := (T = aitEnd);
      until Result;
  end;
end;

procedure TArchiveReaderBase.InitDecoder(Item: TArchiveItem);
begin
  if acfDictionaryLevel in Item.FCompressionFlags then
    FDecoder.DictionaryLevel := Item.DictionaryLevel;

  if acfCompressionTable in Item.FCompressionFlags then
    FDecoder.TableParameters := Item.CompressionTable;

  FDecoder.Tear := Item.SolidCompression;
end;

procedure TArchiveReaderBase.DecodeToSwap(Item: TArchiveItem);
var
  CRC: longword;
begin
  if Assigned(FSwapWriter) then
  begin
    FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);

    Item.SetDiskNumber(FSwapWriter.CurrentImage);
    Item.SetDiskSeek(FSwapWriter.Seek(0, soCurrent));
    case Item.CompressionLevel of
      0: FDecoder.Copy  (FSwapWriter, Item.UncompressedSize, CRC);
    else FDecoder.Decode(FSwapWriter, Item.UncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not FSwapWriter   .IsValidStream then DoFailure(cmStrmWriteError);

    if not Item.Crc = CRC then
      DoFailure(Format(cmCrcError, [Item.FExternalFileName]));
  end else
    DoFailure(cmStrmWriteError);
end;

procedure TArchiveReaderBase.DecodeToNil(Item: TArchiveItem);
var
  CRC: longword;
  Stream: TFileWriter;
begin
  Stream := TNulWriter.Create;
  if Assigned(Stream) then
  begin
    FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);
    case Item.CompressionLevel of
      0: FDecoder.Copy  (Stream, Item.UncompressedSize, CRC);
    else FDecoder.Decode(Stream, Item.UncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not Stream        .IsValidStream then DoFailure(cmStrmWriteError);

    if not Item.Crc = CRC then
      DoFailure(Format(cmCrcError, [Item.FExternalFileName]));

    Stream.Destroy;
  end else
    DoFailure(cmStrmWriteError);
end;

procedure TArchiveReaderBase.DecodeToFile(Item: TArchiveItem);
var
  CRC: longword;
  Stream: TFileWriter;
begin
  Stream := TFileWriter.Create(Item.FExternalFileName, fmCreate);
  if Assigned(Stream) then
  begin
    FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);
    case Item.CompressionLevel of
      0: FDecoder.Copy  (Stream, Item.UncompressedSize, CRC);
    else FDecoder.Decode(Stream, Item.UncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not Stream        .IsValidStream then DoFailure(cmStrmWriteError);

    if not Item.Crc = CRC then
      DoFailure(Format(cmCrcError, [Item.FExternalFileName]));

    Stream.Destroy;
    if ExitCode < ccError then
    begin
      FileSetAttr(Item.FExternalFileName, Item.Attributes);
      FileSetDate(Item.FExternalFileName, Item.LastModifiedTime);
    end;
  end else
    DoFailure(cmStrmWriteError);
end;

function TArchiveReaderBase.GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index downto 0 do
    if FArchiveCustomItems.Items[Index].FTag = aTag then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiveReaderBase.GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index to FArchiveCustomItems.Count - 1 do
    if FArchiveCustomItems.Items[Index].FTag = aTag then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiveReaderBase.GetBackTear(Index: longint): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index downto 0 do
    if FArchiveCustomItems.Items[Index].SolidCompression then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiveReaderBase.GetNextTear(Index: longint): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index to FArchiveCustomItems.Count - 1 do
    if FArchiveCustomItems.Items[Index].SolidCompression then
    begin
      Result := I;
      Break;
    end;
end;

procedure TArchiveReaderBase.OpenArchive(const aArchiveName: string);
var
  MagicSeek: int64;
begin
  DoMessage(Format(Cr + cmOpening, [aArchiveName]));

  CloseArchive;
  if FileExists(aArchiveName) then
  begin
    FArchiveReader := TFileReader.Create(aArchiveName, 1);
    FArchiveReader.OnRequestImage := FOnRequestImage;
    if Assigned(FArchiveReader) then
    begin
      MagicSeek := ReadMagicSeek(FArchiveReader);
      if MagicSeek > 0 then
      begin
        FArchiveReader.Seek(MagicSeek, soFromBeginning);
        if Read(FArchiveReader) then
        begin
          FArchiveName := aArchiveName;
          if FArchiveCustomItems.Count = 0 then
            DoFailure(Format(cmArcTypeError, [aArchiveName]));
        end else
          DoFailure(Format(cmArcTypeError, [aArchiveName]));
      end else
        DoFailure(Format(cmArcTypeError, [aArchiveName]));
    end else
      DoFailure(Format(cmOpenArcError, [aArchiveName]));
  end;
end;

procedure TArchiveReaderBase.CloseArchive;
begin
  FArchiveName := '';
  if FArchiveReader <> nil then
    FreeAndNil(FArchiveReader);

  FArchiveCustomItems.Clear;
//FArchiveBindingItem.Clear;
//FArchiveLocatorItem.Clear;

  FProcessedSize := 0;
  FTotalSize     := 0;
end;

function TArchiveReaderBase.Find(const aFileName: string): longint;
begin
  Result := FArchiveCustomItems.Find(aFileName);
end;

procedure TArchiveReaderBase.DoFailure(const ErrorMessage: string);
begin
  SetExitCode(ccError);
  if Assigned(FOnFailure) then
    FOnFailure(ErrorMessage, ccError);
end;

procedure TArchiveReaderBase.DoMessage(const Message: string);
begin
  if Assigned(FOnMessage) then
  begin
    FOnMessage(Message);
  end;
end;

function TArchiveReaderBase.DoProgress(Value: longint): boolean;
begin
  while FSuspended do Sleep(250);

  Inc(FProcessedSize, Value);
  if Assigned(FOnProgress) then
  begin
    DoProgress(Round(FProcessedSize/FTotalSize * 100));
  end;
  Result := ExitCode < ccError;
end;

// TArchiveReader class

procedure TArchiveReader.TagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do Tag(I);
end;

procedure TArchiveReader.Tag(Index: longint);
begin
  FArchiveCustomItems.Items[Index].FTag := aitUpdate;
end;

procedure TArchiveReader.Tag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
    with FArchiveCustomItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then Tag(I);
end;

procedure TArchiveReader.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do UnTag(I);
end;

procedure TArchiveReader.UnTag(Index: longint);
begin
  FArchiveCustomItems.Items[Index].FTag := aitNone;
end;

procedure TArchiveReader.UnTag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
    with FArchiveCustomItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then UnTag(I);
end;

// TArchiveWriterBase class

constructor TArchiveWriterBase.Create;
begin
  inherited Create;
  FIsNeededToSave  := FALSE;
  FIsNeededToSwap  := FALSE;
  FThreshold       := 0;
  FWorkDirectory   := '';
end;

procedure TArchiveWriterBase.Write(aStream: TFileWriter);
var
  MagicSeek: int64;
  I: longword;
begin
  FArchiveLocatorItem.DiskNumber := aStream.CurrentImage;
  FArchiveLocatorItem.DiskSeek   := aStream.Seek(0, soFromCurrent);

  aStream.WriteDWord(beexMARKER);
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    aStream.WriteInfWord(aitItem);
    FArchiveCustomItems.Items[I].Write(aStream);
  end;
  aStream.WriteInfWord(aitBinding);
  FArchiveBindingItem.Write(aStream);

  if aStream.Threshold > 0 then aStream.CreateImage;

  // Copiare SFX module

  MagicSeek := aStream.Seek(0, soFromCurrent);
  aStream.WriteInfWord(aitLocator);
  FArchiveLocatorItem.Write(aStream);
  WriteMagicSeek(aStream, MagicSeek);
end;

procedure TArchiveWriterBase.Pack;
begin

end;

function TArchiveWriterBase.OpenSwap: longint;
var
  CRC: longword;
  I: longint;
  Item: TArchiveItem;
begin
  if (ExitCode < ccError) and (FIsNeededToSwap) then
  begin
    FSwapName   := GenerateFileName(FWorkDirectory);
    FSwapWriter := TFileWriter.Create(FSwapName, 0);
    if Assigned(FSwapWriter) then
    begin
      FDecoder := THeaderDecoder.Create(FArchiveReader);
      FDecoder.OnProgress := @DoProgress;

      for I := 0 to FArchiveCustomItems.Count - 1 do
        if ExitCode < ccError then
        begin
          Item := FArchiveCustomItems.Items[I];

          InitDecoder(Item);
          if Item.FTag in [aitDecode, aitDecodeAndUpdate] then
          begin
            case Item.FTag of
              aitDecode:          DoMessage(Format(cmSwapping, [Item.FileName]));
              aitDecodeAndUpdate: DoMessage(Format(cmDecoding, [Item.FileName]));
            end;

            case Item.FTag of
              aitDecode:          DecodeToSwap(Item);
              aitDecodeAndUpdate: DecodeToNil (Item);
            end;
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
          end;
        end;
      FDecoder.Destroy;
      FreeAndNil(FSwapWriter);

      if ExitCode < ccError then
      begin
        FSwapReader := TFileReader.Create(FSwapName, fmOpenRead + fmShareDenyWrite);
        if Assigned(FSwapReader) = False then
          DoFailure(cmOpenSwapError);
      end;
    end else
      DoFailure(cmCreateSwapError);
  end;
  Result := ExitCode;
end;

procedure TArchiveWriterBase.CloseArchive;
begin
  if Assigned(FArchiveReader) then FreeAndNil(FArchiveReader);
  if Assigned(FSwapWriter)    then FreeAndNil(FSwapWriter);
  if Assigned(FSwapReader)    then FreeAndNil(FSwapReader);
  if Assigned(FTempWriter)    then FreeAndNil(FTempWriter);

  if FIsNeededToSave then
  begin
    if ExitCode < ccError then
    begin
      SysUtils.DeleteFile(FSwapName);
      SysUtils.DeleteFile(FArchiveName);
      if RenameFile(FTempName, FArchiveName) = False then
        DoFailure(Format(cmRenameFileError, [FTempName, FArchiveName]));
    end else
    begin
      SysUtils.DeleteFile(FSwapName);
      SysUtils.DeleteFile(FTempName);
    end;
  end;
  FIsNeededToSave := FALSE;
  FArchiveName    := '';
  FSwapName       := '';
  FTempName       := '';

  FArchiveCustomItems.Clear;
  // FArchiveBindingItem.Clear;
  // FArchiveLocatorItem.Clear;

  FProcessedSize  := 0;
  FTotalSize      := 0;
end;


procedure TArchiveWriterBase.InitEncoder(Item: TArchiveItem);
begin
  if acfDictionaryLevel in Item.FCompressionFlags then
    FEncoder.DictionaryLevel := Item.DictionaryLevel;

  if acfCompressionTable in Item.FCompressionFlags then
    FEncoder.TableParameters := Item.CompressionTable;

  FEncoder.Tear := Item.SolidCompression;
end;

procedure TArchiveWriterBase.EncodeFromArchive(Item: TArchiveItem);
begin


end;

procedure TArchiveWriterBase.EncodeFromSwap(Item: TArchiveItem);
begin


end;

procedure TArchiveWriterBase.EncodeFromFile(Item: TArchiveItem);
begin


end;

procedure TArchiveWriterBase.SetWorkDirectory(const Value: string);
begin
  if (Value = '') or DirectoryExists(Value) then
  begin
    FWorkDirectory := IncludeTrailingBackSlash(Value);
  end;
end;

// TArchiveWriter class

procedure TArchiveWriter.TagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do Tag(I);
end;

procedure TArchiveWriter.Tag(Index: longint);
begin
  FArchiveCustomItems.Items[Index].FTag := aitUpdate;
end;

procedure TArchiveWriter.Tag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
    with FArchiveCustomItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then Tag(I);
end;

procedure TArchiveWriter.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do UnTag(I);
end;

procedure TArchiveWriter.UnTag(Index: longint);
begin
  FArchiveCustomItems.Items[Index].FTag := aitNone;
end;

procedure TArchiveWriter.UnTag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
    with FArchiveCustomItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then UnTag(I);
end;

// TBeeArchiveExtractor class

constructor TBeeArchiveExtractor.Create;
begin
  inherited Create;
  FIsNeededToExtract := FALSE;
end;

procedure TBeeArchiveExtractor.CheckTags;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  ExtractAs: string;
begin
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    Item := FArchiveCustomItems.Items[I];

    if Item.FTag = aitUpdate then
    begin
      repeat
        DoExtract(Item, ExtractAs, Confirm);
      until (Confirm <> arcOk) or (IsValidFileName(ExtractAs));

      case Confirm of
        arcOk:
        begin
          FIsNeededToExtract := TRUE;
          Item.FExternalFileName := ExtractAs;
        end;
        arcCancel: Item.FTag:= aitNone;
        arcAbort:
        begin
          FIsNeededToExtract := FALSE;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TBeeArchiveExtractor.CheckSequences;
var
  I, J, BackTear, NextTear: longint;
  Item: TArchiveItem;
begin
  // STEP2: find sequences and tag ...
  I := GetBackTag(FArchiveCustomItems.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FArchiveCustomItems.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitUpdate);
      for J := BackTear to NextTear do
      begin
        Item := FArchiveCustomItems.Items[J];
        case Item.FTag of
          aitNone:   Item.FTag := aitDecode;
        //aitUpdate: nothing to do
        //aitDecode: nothing to do
        end;
      end;
      I := BackTear;
    end;
    I := GetBackTag(I - 1, aitUpdate);
  end;

  // STEP2: calculate bytes to process ...
  for J := 0 to FArchiveCustomItems.Count - 1 do
  begin
    Item := FArchiveCustomItems.Items[J];
    case Item.FTag of
    //aitNone:   nothing to do
      aitUpdate: Inc(FTotalSize, Item.UncompressedSize);
      aitDecode: Inc(FTotalSize, Item.UncompressedSize);
    end;
  end;
end;

procedure TBeeArchiveExtractor.DoExtract(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnExtract) then
  begin
    Item.FExternalFileName := Item.FileName;
    FOnExtract(Item, ExtractAs, Confirm);
  end;
end;

procedure TBeeArchiveExtractor.ExtractTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  CheckSequences;
  if FIsNeededToExtract then
  begin
    FDecoder := THeaderDecoder.Create(FArchiveReader);
    FDecoder.OnProgress := @DoProgress;
    for I := 0 to FArchiveCustomItems.Count - 1 do
      if ExitCode < ccError then
      begin
        Item := FArchiveCustomItems.Items[I];

        InitDecoder(Item);
        if Item.FTag in [aitUpdate, aitDecode] then
        begin
          case Item.FTag of
            aitUpdate:          DoMessage(Format(cmExtracting, [Item.FExternalFileName]));
            aitDecode:          DoMessage(Format(cmDecoding,   [Item.FExternalFileName]));
            aitDecodeAndUpdate: DoMessage(Format(cmExtracting, [Item.FExternalFileName]));
          end;

          case Item.FTag of
            aitUpdate:          DecodeToFile(Item);
            aitDecode:          DecodeToNil   (Item);
            aitDecodeAndUpdate: DecodeToFile(Item);
          end;
          {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
        end;
      end;
    FDecoder.Destroy;
  end;
  CloseArchive;
end;

procedure TBeeArchiveExtractor.TestTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckSequences;
  FDecoder := THeaderDecoder.Create(FArchiveReader);
  FDecoder.OnProgress := @DoProgress;
  for I := 0 to FArchiveCustomItems.Count - 1 do
    if ExitCode < ccError then
    begin
      Item := FArchiveCustomItems.Items[I];

      InitDecoder(Item);
      if Item.FTag in [aitUpdate, aitDecode] then
      begin
        case Item.FTag of
          aitUpdate:          DoMessage(Format(cmTesting,  [Item.FExternalFileName]));
          aitDecode:          DoMessage(Format(cmDecoding, [Item.FExternalFileName]));
          aitDecodeAndUpdate: DoMessage(Format(cmTesting,  [Item.FExternalFileName]));
        end;

        case Item.FTag of
          aitUpdate:          DecodeToNil(Item);
          aitDecode:          DecodeToNil(Item);
          aitDecodeAndUpdate: DecodeToNil(Item);
        end;
        {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
      end;
    end;
  FDecoder.Destroy;
  CloseArchive;
end;

// TBeeArchiveRenamer class

procedure TBeeArchiveRenamer.CheckTags;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  RemaneAs: string;
begin
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    Item := FArchiveCustomItems.Items[I];

    Inc(FTotalSize, Item.CompressedSize);
    if Item.FTag in [aitUpdate] then
    begin
      repeat
        DoRename(Item, RemaneAs, Confirm);
      until (Confirm <> arcOk) or (FArchiveCustomItems.GetNameIndex(RemaneAs) = -1);

      case Confirm of
        arcOk:
        begin
          FIsNeededToSave := TRUE;
          Item.FFileName  := RemaneAs;
        end;
        arcCancel: Item.FTag:= aitNone;
        arcAbort:
        begin
          FIsNeededToSave := FALSE;
          Break;
        end;
      end;

    end; // if end
  end; // for end
end;

procedure TBeeArchiveRenamer.RenameTagged;
var
  I: longint;
  Encoder: THeaderEncoder;
  Item: TArchiveItem;
begin
  CheckTags;
  if FIsNeededToSave then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FThreshold);
    FTempWriter.OnRequestBlankDisk := FOnRequestBlankDisk;
    if Assigned(FTempWriter) then
    begin
      Encoder := THeaderEncoder.Create(FTempWriter);
      Encoder.OnProgress := @DoProgress;
      for I := 0 to FArchiveCustomItems.Count - 1 do
        if ExitCode < ccError  then
        begin
          Item := FArchiveCustomItems.Items[I];
          FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);

          Item.FDiskSeek   := FTempWriter.Seek(0, soCurrent);
          Item.FDiskNumber := FTempWriter.CurrentImage;
          case Item.FTag of
            aitNone:   DoMessage(Format(cmCopying,  [Item.FileName]));
            aitUpdate: DoMessage(Format(cmRenaming, [Item.FileName]));
          end;
          Encoder.Copy(FArchiveReader, Item.CompressedSize);

          if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
          if not FTempWriter   .IsValidStream then DoFailure(cmStrmWriteError);
        end;
      Encoder.Destroy;
      Write(FTempWriter);
      if not FTempWriter.IsValidStream then
        DoFailure(cmStrmWriteError);
    end else
      DoFailure(cmOpenTempError);
  end;
  CloseArchive;
end;

procedure TBeeArchiveRenamer.DoRename(Item: TArchiveItem;
  var RenameAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnRename) then
  begin
    RenameAs := Item.FileName;
    FOnRename(Item, RenameAs, Confirm);
  end;
end;

// TBeeArchiveEraser class

procedure TBeeArchiveEraser.DoErase(Item: TArchiveItem;
  var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnErase) then
  begin
    FOnErase(Item, Confirm);
  end;
end;

procedure TBeeArchiveEraser.CheckTags;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
begin
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    Item := FArchiveCustomItems.Items[I];
    if Item.FTag in [aitUpdate] then
    begin
      DoErase(Item, Confirm);
      case Confirm of
        arcOk:  FIsNeededToSave := TRUE;
        arcCancel: Item.FTag := aitNone;
        arcAbort:
        begin
          FIsNeededToSave := FALSE;
          Break;
        end;
      end;
    end;
  end;

  if FIsNeededToSave then CheckSequences;
end;

procedure TBeeArchiveEraser.CheckSequences;
var
  I, J, BackTear, NextTear: longint;
  Item: TArchiveItem;
begin
  // STEP1: find sequences and set tag ...
  I := GetBackTag(FArchiveCustomItems.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FArchiveCustomItems.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitNone);
      for J := BackTear to NextTear do
      begin
        Item := FArchiveCustomItems.Items[J];
        case Item.FTag of
          aitNone:   Item.FTag := aitDecode;
          aitUpdate: Item.FTag := aitDecodeAndUpdate;
        end;
        if Item.FTag in [aitDecode] then
          FIsNeededToSwap := TRUE;
      end;
      I := BackTear;
    end;
    I := GetBackTag(I - 1, aitUpdate);
  end;

  // STEP2: calculate bytes to process ...
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    Item := FArchiveCustomItems.Items[J];
    case Item.FTag of
      aitNone:            Inc(FTotalSize, Item.CompressedSize);
    //aitUpdate:          nothing to do
      aitDecode:          Inc(FTotalSize, Item.UncompressedSize * 2);
      aitDecodeAndUpdate: Inc(FTotalSize, Item.UncompressedSize);
    end;
  end;
end;

procedure TBeeArchiveEraser.EraseTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if FIsNeededToSave then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FThreshold);
    FTempWriter.OnRequestBlankDisk := FOnRequestBlankDisk;
    if Assigned(FTempWriter) then
    begin
      if OpenSwap < ccError  then
      begin
        for I := FArchiveCustomItems.Count - 1 downto 0 do
        begin
          Item := FArchiveCustomItems.Items[I];
          if Item.FTag in [aitUpdate, aitDecodeAndUpdate] then
          begin
            DoMessage(Format(cmDeleting, [Item.FileName]));
            FArchiveCustomItems.Delete(I);
          end;
        end;

        FEncoder := THeaderEncoder.Create(FTempWriter);
        FEncoder.OnProgress := @DoProgress;
        for I := 0 to FArchiveCustomItems.Count - 1 do
          if ExitCode < ccError then
          begin
            Item := FArchiveCustomItems.Items[I];
            case Item.FTag of
              aitNone:            DoMessage(Format(cmCopying,  [Item.FileName]));
            //aitUpdate:          DoMessage(Format(cmDeleting, [Item.FileName]));
              aitDecode:          DoMessage(Format(cmEncoding, [Item.FileName]));
            //aitDecodeAndUpdate: DoMessage(Format(cmDeleting, [Item.FileName]));
            end;

            InitDecoder(Item);
            case Item.FTag of
              aitNone:   EncodeFromArchive(Item);
              aitDecode: EncodeFromSwap   (Item);
            end;

            if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
            if not FSwapReader   .IsValidStream then DoFailure(cmStrmReadError);
            if not FTempWriter   .IsValidStream then DoFailure(cmStrmWriteError);
          end;
        FEncoder.Destroy;
        if ExitCode < ccError then
        begin
          Write(FTempWriter);
          if not FTempWriter.IsValidStream then
            DoFailure(cmStrmWriteError);
        end;
      end;
    end else
      DoFailure(cmOpenTempError);
  end;
end;

// TBeeArchiveUpdater class

constructor TBeeArchiveUpdater.Create;
begin
  inherited Create;
  FCompressionMethod  := 0;
  FCompressionLevel   := 0;;
  FDictionaryLevel    := 0;
  FSolidCompression   := FALSE;
  FConfigurationName  := '';
  FConfiguration      := nil;
  FForceFileExtension := '';
  FOnUpdate           := nil;
end;

procedure TBeeArchiveUpdater.SetCompressionMethod(Value: longint);
begin
  if Value in [0..1] then FCompressionMethod := Value;
end;

procedure TBeeArchiveUpdater.SetCompressionLevel(Value: longint);
begin
  if Value in [1..3] then FCompressionLevel := Value;
end;

procedure TBeeArchiveUpdater.SetDictionaryLevel(Value: longint);
begin
  if Value in [0..9] then FDictionaryLevel := Value;
end;

procedure TBeeArchiveUpdater.SetConfigurationName(const Value: string);
begin
  if FileExists(Value) then FConfigurationName := Value;
end;

procedure TBeeArchiveUpdater.SetForceFileExtension(const Value: string);
begin
  FForceFileExtension := Value;
end;

procedure TBeeArchiveUpdater.CheckTags;
begin


end;

procedure TBeeArchiveUpdater.CheckSequences;
var
  Item: TArchiveItem;
  I, J, BackTear, NextTear: longint;
begin
  DoMessage(Format(cmScanning, ['...']));
  // STEP1: configure new items ...
  // Configure;
  // STEP2: find sequences and tag ...
  I := GetBackTag(FArchiveCustomItems.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FArchiveCustomItems.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitNone);
      for J := BackTear to NextTear do
      begin
        Item := FArchiveCustomItems.Items[J];
        case Item.FTag of
          aitNone:            Item.FTag := aitDecode;
          aitUpdate:          Item.FTag := aitDecodeAndUpdate;
        //aitDecode:          nothing to do
        //aitDecodeAndUpdate: nothing to do
        end;
      end;
      I := BackTear;
    end;
    I := GetBackTag(I - 1, aitUpdate);
  end;

  (*
  // STEP3: calculate bytes to process ...
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    Item := FArchiveCustomItems.Items[I];
    case Item.FTag of
      aitNone:            Inc(FTotalSize, Item.CompressedSize);
      aitUpdate:          Inc(FTotalSize, Item.ExtSize);
      aitDecode:          Inc(FTotalSize, Item.Size + Item.Size);
      aitDecodeAndUpdate: Inc(FTotalSize, Item.Size + Item.ExtSize);
    end;
  end;

    if FCommandLine.sfxOption <> '' then
      FHeaders.LoadModule(FCommandLine.sfxOption);
  end else
    DoMessage(cmNoFilesWarning, ccWarning);

  *)
end;

(*




procedure TBeeArchiveAdder.ConfigureCoders;
var
  I: longint;
  CurrentItem: TArchiveItem;
  CurrentTable: TTableParameters;
  CurrentFileExt: string;
  PreviousFileExt: string;
begin
  if FCompressionCoder = actNONE then Exit;
  if FCompressionCoder = actMAIN then
  begin
    CurrentFileExt := '.';
    FConfiguration.Selector('\main');
    FConfiguration.CurrentSection.Values['Method'] := IntToStr(FCompressionLevel);
    FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(FDictionaryLevel);
    FConfiguration.Selector('\m' + FConfiguration.CurrentSection.Values['Method']);

    for I := 0 to FArchiveCustomItems.Count - 1 do
    begin
      CurrentItem := FArchiveCustomItems.Items[I];
      if CurrentItem.FTag = aitAdd then
      begin
        PreviousFileExt := CurrentFileExt;
        if FForceFileExtension = '' then
          CurrentFileExt := ExtractFileExt(CurrentItem.ExternalFileName)
        else
          CurrentFileExt := FForceFileExtension;

        CurrentItem.FCoder := TBeeArchiveMainCoder.Create;
        with TBeeArchiveMainCoder(CurrentItem.FCoder) do
        begin
          Method     := FCompressionLevel;
          Dictionary := FDictionaryLevel;

          if FConfiguration.GetTable(CurrentFileExt, CurrentTable) then
            Table := CurrentTable
          else
            Table := DefaultTableParameters;

          Tear := TRUE;
          if Solid then
            if CompareFileName(CurrentFileExt, PreviousFileExt) = 0 then
              Tear := FALSE;
        end;
      end;
    end;
  end;
  if FCompressionCoder = actROLOZ then
  begin
    // nothing to do
  end;
end;



procedure TBeeArchiveViewer.Add(SearchRec: TCustomSearchRec;
  const UseFileName: string; UseFlags: TArchiveItemFlags);
begin
  if FArchiveBusy then Exit;

  FArchiveBusy := TRUE;
  if Find(UseFileName) = -1 then
  begin
    FArchiveCustomItems.Add(TArchiveItem.Create(
      SearchRec, UseFileName, UseFlags));
    FNeededToSave := TRUE;
  end;
  FArchiveBusy  := FALSE;
end;

procedure TBeeArchiveViewer.Update(SearchRec: TCustomSearchRec;
  const UseFileName: string; UseFlags: TArchiveItemFlags);
var
  Index: longint;
begin
  if FArchiveBusy then Exit;

  FArchiveBusy := TRUE;
  Index := Find(UseFileName);
  if Index <> -1 then
    with FArchiveCustomItems.Items[Index] do
      if SearchRec.LastModifiedTime > LastModifiedTime then
      begin
        FTag := aitUpdate;
        // copiare tutti i dati
        FNeededToSave := TRUE;
      end;
  FArchiveBusy := FALSE;
end;

procedure TBeeArchiveViewer.Replace(SearchRec: TCustomSearchRec;
  const UseFileName: string; UseFlags: TArchiveItemFlags);
var
  Index: longint;
begin
  if FArchiveBusy then Exit;

  FArchiveBusy := TRUE;
  Index := Find(UseFileName);
  if Index <> -1 then
    with FArchiveCustomItems.Items[Index] do
    begin
      FTag := aitUpdate;

      // copiare tutti i dati


      FNeededToSave := TRUE;
    end;
  FArchiveBusy := FALSE;
end;


procedure TBeeArchiveViewer.Encode(Item: TArchiveItem);
var
  CRC: longword;
  Stream: TStream;
begin
  if Item.FExternalStream <> nil then
  begin
    Stream := Item.FExternalStream;
    Stream.Seek(Item.FExternalDiskSeek, soBeginning);
  end else
    Stream := CreateTFileReader(Item.ExternalFileName, fmOpenRead);

  if Stream <> nil then
  begin
    Item.DiskSeek := FTempWriter.Seek(0, soCurrent); // flush buffer
    // Item.DiskNumber := FTempWriter.DiskNumber;
    if Assigned(Item.Coder) then
    begin
      FBeeEncoder.CompressionMethod := TBeeArchiveMainCoder(Item.Coder).CompressionLevel;
      FBeeEncoder.DictionaryLevel   := TBeeArchiveMainCoder(Item.Coder).DictionaryLevel;
      FBeeEncoder.TableParameters   := TBeeArchiveMainCoder(Item.Coder).CompressionTable;
      FBeeEncoder.Tear              := TBeeArchiveMainCoder(Item.Coder).SolidCompression;

      Item.UncompressedSize     := FBeeEncoder.Encode(Stream, Item.ExternalUncompressedSize, CRC);
      Item.Coder.CompressedSize := FTempWriter.Seek(0, soCurrent) - Item.DiskSeek;

      // optimize compression ...
      if Item.Coder.CompressedSize >= Item.UncompressedSize then
      begin
        FreeAndNil(Item.FCoder);
        Stream.Seek(Item.FExternalDiskSeek, soBeginning);
        FTempWriter.Size      := Item.DiskSeek;
        Item.UncompressedSize := FTempWriter.Read(Stream, Item.ExternalUncompressedSize);
      end;
    end else
      Item.UncompressedSize := FBeeEncoder.Copy(Stream, Item.ExternalUncompressedSize, CRC);

    Item.CRC := CRC;

    if Item.FExternalStream = nil then Stream.Destroy;
  end;
end;

procedure TBeeArchiveViewer.Copy(Item: TArchiveItem);
var
  CRC: longword;
  Stream: TStream;
begin
  Stream := Item.FExternalStream;
  Stream.Seek(Item.FExternalDiskSeek, soBeginning);

  Item.DiskSeek := FTempWriter.Seek(0, soCurrent);
  FBeeEncoder.Copy(Stream, Item.ExternalUncompressedSize, CRC);
end;







     *)





end.

