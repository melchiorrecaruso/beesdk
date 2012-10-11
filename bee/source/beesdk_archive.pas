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

  /// archive compression method
  actNone      = $00;
  actMain      = $01;

  /// archive encryption method
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
    aifComment);

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
    FExternalFileSize: int64;
    function GetSolidCompression: boolean;
    procedure SetSolidCompression(Value: boolean);
  public {methods}
    constructor Create(DefaultFlags: TArchiveItemFlags);
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    procedure Update(SearchRec: TCustomSearchRec);
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
    property SolidCompression: boolean read GetSolidCompression write SetSolidCompression;
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

  TArchiveUpdateEvent = procedure(SearchRec: TCustomSearchRec;
    var UpdateAs; var Confirm: TArchiveConfirm) of object;

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
    FArchiveItems: TBeeArchiveCustomItems;
    FArchiveBinding: TBeeArchiveBinding;
    FArchiveLocator: TArchiveLocator;
    procedure UnPack;
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
    FSearchRecs: TList;
    FDefaultFlags: TArchiveItemFlags;
    FCompressionMethod: longint;
    FCompressionLevel: longint;
    FDictionaryLevel: longint;
    FSolidCompression: boolean;
    FEncryptionMethod: longword;
    FConfigurationName: string;
    FConfiguration: TConfiguration;
    FForceFileExtension: string;
    FOnUpdate: TArchiveUpdateEvent;
    procedure SetCompressionMethod(Value: longint);
    procedure SetCompressionLevel(Value: longint);
    procedure SetDictionaryLevel(Value: longint);
    procedure SetConfigurationName(const Value: string);
    procedure SetForceFileExtension(const Value: string);
    procedure ConfigureCrypter;
    procedure ConfigureCoder;
    procedure CheckTags;
    procedure CheckSequences;
    procedure DoUpdate(SearchRec: TCustomSearchRec;
      var UpdateAs: string; var Confirm: TArchiveConfirm);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateTagged;
    procedure Tag(SearchRec: TCustomSearchRec);
  public
    property CompressionMethod: longint
      read FCompressionLevel write SetCompressionLevel;
    property CompressionLevel: longint
      read FCompressionLevel write SetCompressionLevel;
    property DictionaryLevel: longint
      read FDictionaryLevel write SetDictionaryLevel;
    property SolidCompression: boolean
      read FSolidCompression write FSolidCompression;

    property EncrypMethod: longint
      read FCompressionLevel write SetCompressionLevel;


    property ConfigurationName: string
      read FConfigurationName write SetConfigurationName;
    property ForceFileExtension: string
      read FForceFileExtension write FForceFileExtension;

    property DefaultFlags: TArchiveItemFlags read FDefaultFlags write FDefaultFlags;

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

constructor TArchiveItem.Create(DefaultFlags: TArchiveItemFlags);
begin
  inherited Create;
  FFlags := DefaultFlags;
end;

procedure TArchiveItem.Update(SearchRec: TCustomSearchRec);
begin
//if (aifUncompressedSize in FFLags) then FUncompressedSize := SearchRec.Size;
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
//if (aifComment          in FFLags) then FComment          := UseComment;

//FFileName := SearchRec.Name;

// data descriptor
//FCompressionMethod := 0;
//FCompressionFlags  := [];
//FCompressedSize    := 0;
//FCompressionLevel  := 0;
//FDictionaryLevel   := 0;
//FCompressionTable  := nil;
//FEncryptionMethod  := 0;

//FPosition := -1;
  FExternalFileName := SearchRec.Name;
  FExternalFileSize := SearchRec.Size;
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

procedure TArchiveItem.SetSolidCompression(Value: boolean);
begin
  if Value then
    Include(FCompressionFlags, acfSolidCompression)
  else
    Exclude(FCompressionFlags, acfSolidCompression);
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
    (*
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


    end; *)
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
  Randomize;
  FExitCode := ccSuccesful;

  FArchiveItems := TBeeArchiveCustomItems.Create;
  FArchiveBinding := TBeeArchiveBinding.Create;
  FArchiveLocator := TArchiveLocator.Create;
end;

destructor TArchiveReaderBase.Destroy;
begin
  FArchiveItems.Destroy;
  FArchiveBinding.Destroy;
  FArchiveLocator.Destroy;
  inherited Destroy;
end;

procedure TArchiveReaderBase.Terminate;
begin
  SetExitCode(ccUserAbort);
end;

function TArchiveReaderBase.GetCount: longint;
begin
  Result := FArchiveItems.Count;
end;

function TArchiveReaderBase.GetItem(Index: longint): TArchiveItem;
begin
  Result := FArchiveItems.Items[Index];
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

    FArchiveLocator.Read(aStream);
    if alfDisksNumber in FArchiveLocator.FFlags then
      aStream.ImagesNumber := FArchiveLocator.DisksNumber;
    if alfDiskNumber in FArchiveLocator.FFlags then
      aStream.SeekImage(FArchiveLocator.DiskNumber, FArchiveLocator.DiskSeek);

    if aStream.ReadDWord = beexMARKER then
      repeat
        T := longword(aStream.ReadInfWord);
        if T <> aitEnd then
        begin
          if longword(aStream.ReadInfWord) <= beexVERSION then Exit;

          case T of
            aitItem:    FArchiveItems.Add(TArchiveItem.Read(aStream));
            aitBinding: FArchiveBinding.Read(aStream);
          //aitLocator: already readed;
            else        Exit;
          end;
        end;
        Result := (T = aitEnd);
      until Result;
  end;
end;

procedure TArchiveReaderBase.UnPack;
var
  I: longint;
  CurrentItem: TArchiveItem;
  PreviusItem: TArchiveItem;
begin














  (*
  PreviusItem := nil;
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    CurrentItem := FArchiveItems.Items[I];
    if not (aifSessionFlags in CurrentItem.Flags) then
    begin
      if not (aifUncompressedSize in CurrentItem.Flags) then
        CurrentItem.FUncompressedSize := PreviusItem.UncompressedSize;

      if not (aifCreationTime in CurrentItem.Flags) then
        CurrentItem.FCreationTime := PreviusItem.CreationTime;




    end;
    PreviusItem :=  CurrentItem;


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
    FExternalFileSize: int64;


  end; *)
end;

procedure TArchiveReaderBase.InitDecoder(Item: TArchiveItem);
begin
  if Item.CompressionMethod = actMain then
  begin
    if acfDictionaryLevel in Item.FCompressionFlags then
      FDecoder.DictionaryLevel := Item.DictionaryLevel;

    if acfCompressionTable in Item.FCompressionFlags then
      FDecoder.CompressionTable := Item.CompressionTable;

    FDecoder.FreshModeller(Item.SolidCompression);
  end;
end;

procedure TArchiveReaderBase.DecodeToSwap(Item: TArchiveItem);
var
  CRC: longword;
begin
  if Assigned(FSwapWriter) then
  begin
    FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);

    Item.FDiskNumber := FSwapWriter.CurrentImage;
    Item.FDiskSeek   := FSwapWriter.Seek(0, soCurrent);
    case Item.CompressionLevel of
      actMain: FDecoder.Decode(FSwapWriter, Item.FUncompressedSize, CRC);
      else     FDecoder.Copy  (FSwapWriter, Item.FUncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not FSwapWriter   .IsValidStream then DoFailure(cmStrmWriteError);

    if Item.FCrc <> CRC then
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
    FArchiveReader.SeekImage(Item.FDiskNumber, Item.FDiskSeek);
    case Item.CompressionLevel of
      actMain: FDecoder.Decode(Stream, Item.FUncompressedSize, CRC);
      else     FDecoder.Copy  (Stream, Item.FUncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not Stream        .IsValidStream then DoFailure(cmStrmWriteError);

    if Item.FCrc <> CRC then
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
    FArchiveReader.SeekImage(Item.FDiskNumber, Item.FDiskSeek);
    case Item.CompressionLevel of
      actMain: FDecoder.Decode(Stream, Item.FUncompressedSize, CRC);
      else     FDecoder.Copy  (Stream, Item.FUncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not Stream        .IsValidStream then DoFailure(cmStrmWriteError);

    if Item.FCrc <> CRC then
      DoFailure(Format(cmCrcError, [Item.FExternalFileName]));

    Stream.Destroy;
    if ExitCode < ccError then
    begin
      FileSetAttr(Item.FExternalFileName, Item.FAttributes);
      FileSetDate(Item.FExternalFileName, Item.FLastModifiedTime);
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
    if FArchiveItems.Items[Index].FTag = aTag then
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
  for  I := Index to FArchiveItems.Count - 1 do
    if FArchiveItems.Items[Index].FTag = aTag then
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
    if FArchiveItems.Items[Index].SolidCompression then
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
  for  I := Index to FArchiveItems.Count - 1 do
    if FArchiveItems.Items[Index].SolidCompression then
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
          if FArchiveItems.Count = 0 then
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

  FArchiveItems.Clear;
//FArchiveBinding.Clear;
//FArchiveLocator.Clear;

  FProcessedSize := 0;
  FTotalSize     := 0;
end;

function TArchiveReaderBase.Find(const aFileName: string): longint;
begin
  Result := FArchiveItems.Find(aFileName);
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
  for I := 0 to FArchiveItems.Count - 1 do Tag(I);
end;

procedure TArchiveReader.Tag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitUpdate;
end;

procedure TArchiveReader.Tag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do
    with FArchiveItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then Tag(I);
end;

procedure TArchiveReader.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do UnTag(I);
end;

procedure TArchiveReader.UnTag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitNone;
end;

procedure TArchiveReader.UnTag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do
    with FArchiveItems.Items[I] do
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
  FArchiveLocator.DiskNumber := aStream.CurrentImage;
  FArchiveLocator.DiskSeek   := aStream.Position;

  aStream.WriteDWord(beexMARKER);
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    aStream.WriteInfWord(aitItem);
    FArchiveItems.Items[I].Write(aStream);
  end;
  aStream.WriteInfWord(aitBinding);
  FArchiveBinding.Write(aStream);

  if aStream.Threshold > 0 then aStream.CreateImage;

  // Copiare SFX module

  MagicSeek := aStream.Position;
  aStream.WriteInfWord(aitLocator);
  FArchiveLocator.Write(aStream);
  WriteMagicSeek(aStream, MagicSeek);
end;

procedure TArchiveWriterBase.Pack;
var
  I: longint;
  CurrentItem: TArchiveItem;
  PreviusItem: TArchiveItem;
begin
  if FArchiveItems.Count > 0 then
  begin
    PreviusItem := FArchiveItems.Items[0];
    for I := 1 to FArchiveItems.Count - 1 do
      if not (aifSessionFlags in CurrentItem.FFlags) then
      begin
        CurrentItem := FArchiveItems.Items[I];

        GetBack


        if CurrentItem.FUncompressedSize




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







      end else
        PreviusItem := FArchiveItems.Items[I];
  end;
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

      for I := 0 to FArchiveItems.Count - 1 do
        if ExitCode < ccError then
        begin
          Item := FArchiveItems.Items[I];

          InitDecoder(Item);
          if Item.FTag in [aitDecode, aitDecodeAndUpdate] then
          begin
            case Item.FTag of
              aitDecode:          DoMessage(Format(cmSwapping, [Item.FFileName]));
              aitDecodeAndUpdate: DoMessage(Format(cmDecoding, [Item.FFileName]));
            end;

            case Item.FTag of
              aitDecode:          DecodeToSwap(Item);
              aitDecodeAndUpdate: DecodeToNil (Item);
            end;
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
            if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
            if not FSwapWriter   .IsValidStream then DoFailure(cmStrmWriteError);
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

  FArchiveItems.Clear;
  // FArchiveBinding.Clear;
  // FArchiveLocator.Clear;

  FProcessedSize  := 0;
  FTotalSize      := 0;
end;

procedure TArchiveWriterBase.InitEncoder(Item: TArchiveItem);
begin
  if Item.CompressionMethod = actMain then
  begin
    if acfDictionaryLevel in Item.FCompressionFlags then
      FEncoder.DictionaryLevel := Item.DictionaryLevel;

    if acfCompressionTable in Item.FCompressionFlags then
      FEncoder.CompressionTable := Item.CompressionTable;

    FEncoder.FreshModeller(Item.SolidCompression);
  end;
end;

procedure TArchiveWriterBase.EncodeFromArchive(Item: TArchiveItem);
var
  ABSPosition: int64;
  NulCRC:longword;
begin
  if Assigned(FArchiveReader) then
  begin
    FArchiveReader.SeekImage(Item.FDiskNumber, Item.FDiskSeek);

    ABSPosition      := FTempWriter.ABSPosition;
    Item.FDiskSeek   := FTempWriter.Position;
    Item.FDiskNumber := FTempWriter.CurrentImage;
    FEncoder.Copy(FArchiveReader, Item.FCompressedSize, NulCRC);

    Include(Item.FFlags, aifDiskSeek);
    Include(Item.FFlags, aifDiskNumber);

    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not FTempWriter   .IsValidStream then DoFailure(cmStrmWriteError);
  end else
    DoFailure(cmStrmReadError);
end;

procedure TArchiveWriterBase.EncodeFromSwap(Item: TArchiveItem);
var
  ABSPosition: int64;
begin
  if Assigned(FSwapReader) then
  begin
    FSwapReader.SeekImage(Item.FDiskNumber, Item.FDiskSeek);

    ABSPosition      := FTempWriter.ABSPosition;
    Item.FDiskSeek   := FTempWriter.Position;
    Item.FDiskNumber := FTempWriter.CurrentImage;
    case Item.FCompressionMethod of
      actMain: FEncoder.Encode(FSwapReader, Item.FUncompressedSize, Item.FCRC);
      else     FEncoder.Copy  (FSwapReader, Item.FUncompressedSize, Item.FCRC);
    end;
    Item.FCompressedSize := FTempWriter.ABSPosition - ABSPosition;

    Include(Item.FFlags, aifDiskSeek);
    Include(Item.FFlags, aifDiskNumber);

    if not FSwapReader.IsValidStream then DoFailure(cmStrmReadError);
    if not FTempWriter.IsValidStream then DoFailure(cmStrmWriteError);
  end else
    DoFailure(cmStrmReadError);
end;

procedure TArchiveWriterBase.EncodeFromFile(Item: TArchiveItem);
var
  ABSPosition: int64;
  Stream: TFileReader;
begin
  Stream := TFileReader.Create(Item.FExternalFileName, 0);
  if Stream <> nil then
  begin
    ABSPosition      := FTempWriter.ABSPosition;
    Item.FDiskSeek   := FTempWriter.Position;
    Item.FDiskNumber := FTempWriter.CurrentImage;
    case Item.CompressionMethod of
      actMain: FEncoder.Encode(Stream, Item.FExternalFileSize, Item.FCRC);
      else     FEncoder.Encode(Stream, Item.FExternalFileSize, Item.FCRC);
    end;
    Item.FCompressedSize := FTempWriter.ABSPosition - ABSPosition;

    Include(Item.FFlags, aifDiskSeek);
    Include(Item.FFlags, aifDiskNumber);

    if not Stream     .IsValidStream then DoFailure(cmStrmReadError);
    if not FTempWriter.IsValidStream then DoFailure(cmStrmWriteError);

    Stream.Destroy;
  end else
    DoFailure(Format(cmOpenFileError, [Item.FExternalFileName]));
end;

procedure TArchiveWriterBase.SetWorkDirectory(const Value: string);
begin
  if (Value = '') or DirectoryExists(Value) then
  begin
    FWorkDirectory := Value;
  end;
end;

// TArchiveWriter class

procedure TArchiveWriter.TagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do Tag(I);
end;

procedure TArchiveWriter.Tag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitUpdate;
end;

procedure TArchiveWriter.Tag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do
    with FArchiveItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then Tag(I);
end;

procedure TArchiveWriter.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do UnTag(I);
end;

procedure TArchiveWriter.UnTag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitNone;
end;

procedure TArchiveWriter.UnTag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do
    with FArchiveItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then UnTag(I);
end;

// TBeeArchiveExtractor class

constructor TBeeArchiveExtractor.Create;
begin
  inherited Create;
  FIsNeededToExtract := FALSE;
end;

procedure TBeeArchiveExtractor.DoExtract(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnExtract) then
  begin
    ExtractAs := Item.FileName;
    FOnExtract(Item, ExtractAs, Confirm);
  end;
end;

procedure TBeeArchiveExtractor.CheckTags;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  ExtractAs: string;
begin
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FArchiveItems.Count - 1 do
    if ExitCode < ccError then
    begin
      Item := FArchiveItems.Items[I];
      if Item.FTag = aitUpdate then
      begin
        repeat
          DoExtract(Item, ExtractAs, Confirm);
        until (Confirm <> arcOk) or (IsValidFileName(ExtractAs));

        case Confirm of
          arcOk: begin
            FIsNeededToExtract     := TRUE;
            Item.FExternalFileName := ExtractAs;
          end;
          arcCancel: Item.FTag:= aitNone;
          arcAbort:  DoFailure(cmUserAbort);
        end;
      end;
    end;

  if (ExitCode < ccError) and FIsNeededToExtract then CheckSequences;
end;

procedure TBeeArchiveExtractor.CheckSequences;
var
  I, J, BackTear, NextTear: longint;
  Item: TArchiveItem;
begin
  // STEP2: find sequences and tag ...
  I := GetBackTag(FArchiveItems.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FArchiveItems.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitUpdate);
      for J := BackTear to NextTear do
      begin
        Item := FArchiveItems.Items[J];
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
  for J := 0 to FArchiveItems.Count - 1 do
  begin
    Item := FArchiveItems.Items[J];
    case Item.FTag of
    //aitNone:   nothing to do
      aitUpdate: Inc(FTotalSize, Item.UncompressedSize);
      aitDecode: Inc(FTotalSize, Item.UncompressedSize);
    end;
  end;
end;

procedure TBeeArchiveExtractor.ExtractTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if (ExitCode < ccError) and FIsNeededToExtract then
  begin
    FDecoder := THeaderDecoder.Create(FArchiveReader);
    FDecoder.OnProgress := @DoProgress;
    for I := 0 to FArchiveItems.Count - 1 do
      if ExitCode < ccError then
      begin
        Item := FArchiveItems.Items[I];

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
            aitDecode:          DecodeToNil (Item);
            aitDecodeAndUpdate: DecodeToFile(Item);
          end;
          {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
          // if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
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
  for I := 0 to FArchiveItems.Count - 1 do
    if ExitCode < ccError then
    begin
      Item := FArchiveItems.Items[I];

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
        // if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
      end;
    end;
  FDecoder.Destroy;
  CloseArchive;
end;

// TBeeArchiveRenamer class

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

procedure TBeeArchiveRenamer.CheckTags;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  RemaneAs: string;
begin
  DoMessage(Format(cmScanning, ['...']));
  for I := 0 to FArchiveItems.Count - 1 do
    if ExitCode < ccError then
    begin
      Item := FArchiveItems.Items[I];

      Inc(FTotalSize, Item.CompressedSize);
      if Item.FTag in [aitUpdate] then
      begin
        repeat
          DoRename(Item, RemaneAs, Confirm);
        until (Confirm <> arcOk) or (FArchiveItems.GetNameIndex(RemaneAs) = -1);

        case Confirm of
          arcOk: begin
            FIsNeededToSave := TRUE;
            Item.FFileName  := RemaneAs;
          end;
          arcCancel: Item.FTag:= aitNone;
          arcAbort:  DoFailure(cmUserAbort);
        end;
      end; // if end
    end; // if end
end;

procedure TBeeArchiveRenamer.RenameTagged;
var
  I: longint;
  Encoder: THeaderEncoder;
  Item: TArchiveItem;
begin
  CheckTags;
  if (ExitCode < ccError) and FIsNeededToSave then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FThreshold);
    FTempWriter.OnRequestBlankDisk := FOnRequestBlankDisk;
    if Assigned(FTempWriter) then
    begin
      Encoder := THeaderEncoder.Create(FTempWriter);
      Encoder.OnProgress := @DoProgress;
      for I := 0 to FArchiveItems.Count - 1 do
        if ExitCode < ccError  then
        begin
          Item := FArchiveItems.Items[I];
          FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);

          Item.FDiskSeek   := FTempWriter.Seek(0, soCurrent);
          Item.FDiskNumber := FTempWriter.CurrentImage;
          case Item.FTag of
            aitNone:   DoMessage(Format(cmCopying,  [Item.FileName]));
            aitUpdate: DoMessage(Format(cmRenaming, [Item.FileName]));
          end;
          EncodeFromArchive(Item);
          {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
          //if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
          //if not FTempWriter   .IsValidStream then DoFailure(cmStrmWriteError);
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
  for I := 0 to FArchiveItems.Count - 1 do
    if ExitCode < ccError then

    begin
      Item := FArchiveItems.Items[I];
      if Item.FTag in [aitUpdate] then
      begin
        DoErase(Item, Confirm);
        case Confirm of
          arcOk:     FIsNeededToSave := TRUE;
          arcCancel: Item.FTag := aitNone;
          arcAbort:  DoFailure(cmUserAbort);
        end;
      end;
    end;

  if (ExitCode < ccError) and FIsNeededToSave then CheckSequences;
end;

procedure TBeeArchiveEraser.CheckSequences;
var
  I, J, BackTear, NextTear: longint;
  Item: TArchiveItem;
begin
  // STEP1: find sequences and set tag ...
  I := GetBackTag(FArchiveItems.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FArchiveItems.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitNone);
      for J := BackTear to NextTear do
      begin
        Item := FArchiveItems.Items[J];
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
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    Item := FArchiveItems.Items[J];
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
  if (ExitCode < ccError) and FIsNeededToSave then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FThreshold);
    FTempWriter.OnRequestBlankDisk := FOnRequestBlankDisk;
    if Assigned(FTempWriter) then
    begin
      if OpenSwap < ccError  then
      begin
        for I := FArchiveItems.Count - 1 downto 0 do
        begin
          Item := FArchiveItems.Items[I];
          if Item.FTag in [aitUpdate, aitDecodeAndUpdate] then
          begin
            DoMessage(Format(cmDeleting, [Item.FileName]));
            FArchiveItems.Delete(I);
          end;
        end;

        FEncoder := THeaderEncoder.Create(FTempWriter);
        FEncoder.OnProgress := @DoProgress;
        for I := 0 to FArchiveItems.Count - 1 do
          if ExitCode < ccError then
          begin
            Item := FArchiveItems.Items[I];
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
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
            //if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
            //if not FSwapReader   .IsValidStream then DoFailure(cmStrmReadError);
            //if not FTempWriter   .IsValidStream then DoFailure(cmStrmWriteError);
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
  CloseArchive;
end;

// TBeeArchiveUpdater class

constructor TBeeArchiveUpdater.Create;
begin
  inherited Create;
  FSearchRecs         := TList.Create;
  FOnUpdate           := nil;

  FCompressionMethod  := actNone;
  FCompressionLevel   := Ord(moFast);
  FDictionaryLevel    := Ord(do10MB);
  FSolidCompression   := FALSE;
  FEncryptionMethod   := acrtNone;

  FConfigurationName  := SelfPath + DefaultCfgName;
  FConfiguration      := nil;
  FForceFileExtension :=  '';
end;

destructor TBeeArchiveUpdater.Destroy;
var
  I: longint;
begin
  for I := 0 to FSearchRecs.Count - 1 do
    TCustomSearchRec(FSearchRecs[I]^).Destroy;
  FSearchRecs.Destroy;
  inherited destroy;
end;

procedure TBeeArchiveUpdater.DoUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnUpdate) then
  begin
    UpdateAs := SearchRec.Name;
    FOnUpdate(SearchRec, UpdateAs, Confirm);
  end;
end;

procedure TBeeArchiveUpdater.Tag(SearchRec: TCustomSearchRec);
var
  Csr: TCustomSearchRec;
begin
  Csr                  := TCustomSearchRec.Create;
  Csr.Name             := SearchRec.Name;
  Csr.Size             := SearchRec.Size;
  Csr.Attributes       := SearchRec.Attributes;
  Csr.CreationTime     := SearchRec.CreationTime;
  Csr.LastModifiedTime := SearchRec.LastModifiedTime;
  Csr.LastAccessTime   := SearchRec.LastAccessTime;
  Csr.Mode             := SearchRec.Mode;
  Csr.UserID           := SearchRec.UserID;
  Csr.UserName         := SearchRec.UserName;
  Csr.GroupID          := SearchRec.GroupID;
  Csr.GroupName        := SearchRec.GroupName;

  FSearchRecs.Add(Csr);
end;

procedure TBeeArchiveUpdater.ConfigureCrypter;
var
  I: longint;
  CurrentItem: TArchiveItem;
begin
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    CurrentItem := FArchiveItems.Items[I];
    if CurrentItem.FTag = aitAdd then
    begin
      CurrentItem.FEncryptionMethod := FEncryptionMethod;
      if CurrentItem.FEncryptionMethod = acrtMain then
      begin
        // nothing to do
      end;
    end;
  end;
end;

procedure TBeeArchiveUpdater.ConfigureCoder;
var
  I: longint;
  CurrentItem: TArchiveItem;
  CurrentTable: TTableParameters;
  CurrentFileExt: string;
  PreviousFileExt: string;
begin
  FConfiguration.Create;
  if FileExists(FConfigurationName) then
    FConfiguration.LoadFromFile(FConfigurationName)
  else
    DoFailure(Format(cmConfigError, [FConfigurationName]));

  if ExitCode < ccError then
  begin
    CurrentFileExt := '.';
    FConfiguration.Selector('\main');
    FConfiguration.CurrentSection.Values['Method'] := IntToStr(FCompressionLevel);
    FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(FDictionaryLevel);
    FConfiguration.Selector('\m' + FConfiguration.CurrentSection.Values['Method']);

    for I := 0 to FArchiveItems.Count - 1 do
    begin
      CurrentItem := FArchiveItems.Items[I];
      if CurrentItem.FTag = aitAdd then
      begin
        Exclude(CurrentItem.FCompressionFlags, acfCompressionLevel);
        Exclude(CurrentItem.FCompressionFlags, acfDictionaryLevel);
        Exclude(CurrentItem.FCompressionFlags, acfSolidCompression);
        Exclude(CurrentItem.FCompressionFlags, acfCompressionTable);

        CurrentItem.FCompressionMethod := FCompressionMethod;
        if CurrentItem.FCompressionMethod = acrtMain then
        begin
          CurrentItem.FCompressionLevel  := FCompressionLevel;
          CurrentItem.FDictionaryLevel   := FDictionaryLevel;

          Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
          Include(CurrentItem.FCompressionFlags, acfDictionaryLevel);

          PreviousFileExt := CurrentFileExt;
          if FForceFileExtension = '' then
            CurrentFileExt := ExtractFileExt(CurrentItem.FExternalFileName)
          else
            CurrentFileExt := FForceFileExtension;

          if FConfiguration.GetTable(CurrentFileExt, CurrentTable) then
            CurrentItem.FCompressionTable := CurrentTable
          else
            CurrentItem.FCompressionTable := DefaultTableParameters;

          if CompareFileName(CurrentFileExt, PreviousFileExt) = 0 then
          begin
            if SolidCompression then
              Include(CurrentItem.FCompressionFlags, acfSolidCompression);
          end else
            Include(CurrentItem.FCompressionFlags, acfCompressionTable);
        end;
      end;
    end;
  end;
  FreeAndNil(FConfiguration);
end;

function CompareCustomSearchRecExt(Item1, Item2: pointer): longint;
var
  Ext1, Ext2: string;
begin
  Ext1   := TCustomSearchRec(Item1^).Name;
  Ext2   := TCustomSearchRec(Item2^).Name;
  Result := Bee_Common.CompareFileName(Ext1, Ext2);
end;

procedure TBeeArchiveUpdater.CheckTags;
var
  I, J: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  Csr: TCustomSearchRec;
  UpdateAs: string;
begin
  // DoMessage(Format(cmScanning, ['...']));
  FSearchRecs.Sort(@CompareCustomSearchRecExt);
  for J := 0 to FSearchRecs.Count - 1 do
    if ExitCode < ccError then
    begin
      Csr := TCustomSearchRec(FSearchRecs[J]^);
      DoUpdate(Csr, UpdateAs, Confirm);
      case Confirm of
        arcOk:
        begin
          I := Find(UpdateAs);
          if I = -1 then
          begin
            Item           := TArchiveItem.Create(DefaultFlags);
            Item.FTag      := aitAdd;
            Item.FFileName := UpdateAs;
            FArchiveItems.Add(Item);
          end else
          begin
            Item := FArchiveItems.Items[I];
            if Item.FTag = aitNone then
            begin
              Item.FTag := aitUpdate;
            end;
          end;
          Item.Update(Csr);
          FIsNeededToSave := TRUE;
        end;
      //arcCancel: nothing to do
        arcAbort:  DoFailure(cmUserAbort);
      end;
    end;

  if (ExitCode < ccError) and FIsNeededToSave then CheckSequences;
end;

procedure TBeeArchiveUpdater.CheckSequences;
var
  Item: TArchiveItem;
  I, J, BackTear, NextTear: longint;
begin
  DoMessage(Format(cmScanning, ['...']));
  // STEP1: ConfigureCoder new items ...
  ConfigureCoder;
  // STEP2: find sequences and tag ...
  I := GetBackTag(FArchiveItems.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FArchiveItems.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitNone);
      for J := BackTear to NextTear do
      begin
        Item := FArchiveItems.Items[J];
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

  // STEP3: calculate bytes to process ...
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    Item := FArchiveItems.Items[I];
    case Item.FTag of
      aitNone:            Inc(FTotalSize, Item.CompressedSize);
      aitAdd:             Inc(FTotalSize, Item.FExternalFileSize);
      aitUpdate:          Inc(FTotalSize, Item.FExternalFileSize);
      aitDecode:          Inc(FTotalSize, Item.UncompressedSize + Item.UncompressedSize);
      aitDecodeAndUpdate: Inc(FTotalSize, Item.UncompressedSize + Item.FExternalFileSize);
    end;
  end;
end;

procedure TBeeArchiveUpdater.UpdateTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if (ExitCode < ccError) and  FIsNeededToSave then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FThreshold);
    FTempWriter.OnRequestBlankDisk := FOnRequestBlankDisk;
    if Assigned(FTempWriter) then
    begin
      if OpenSwap < ccError  then
      begin
        FEncoder := THeaderEncoder.Create(FTempWriter);
        FEncoder.OnProgress := @DoProgress;

        for I := 0 to FArchiveItems.Count - 1 do
          if (ExitCode < ccError) then
          begin
            Item := FArchiveItems.Items[I];
            case Item.FTag of
              aitNone:            DoMessage(Format(cmCopying,  [Item.FileName]));
              aitAdd:             DoMessage(Format(cmAdding,   [Item.FileName]));
              aitUpdate:          DoMessage(Format(cmUpdating, [Item.FileName]));
              aitDecode:          DoMessage(Format(cmEncoding, [Item.FileName]));
              aitDecodeAndUpdate: DoMessage(Format(cmUpdating, [Item.FileName]));
            end;

            InitEncoder(Item);
            case Item.FTag of
              aitNone:            EncodeFromArchive(Item);
              aitAdd:             EncodeFromFile   (Item);
              aitUpdate:          EncodeFromFile   (Item);
              aitDecode:          EncodeFromSwap   (Item);
              aitDecodeAndUpdate: EncodeFromFile   (Item);
            end;
            {$IFDEF CONSOLEAPPLICATION} DoClear; {$ENDIF}
            //if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
            //if not FSwapReader   .IsValidStream then DoFailure(cmStrmReadError);
            //if not FTempWriter   .IsValidStream then DoFailure(cmStrmWriteError);
          end;
        FEncoder.Destroy;
        if (ExitCode < ccError) then
        begin
          Write(FTempWriter);
          if not FTempWriter.IsValidStream then
            DoFailure(cmStrmWriteError);
        end;
      end;
    end;
  end;
  CloseArchive;
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
  FConfigurationName := Value;
end;

procedure TBeeArchiveUpdater.SetForceFileExtension(const Value: string);
begin
  FForceFileExtension := Value;
end;

end.

