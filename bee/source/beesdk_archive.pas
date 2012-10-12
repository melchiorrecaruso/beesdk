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

type
  /// archive compression method
  TArchiveCompressionMethod = (actNone, actMain);

  /// archive encryption method
  TArchiveEncryptionMethod = (acrtNone, acrtMain);


  /// archive locator flag
  TArchiveLocatorFlag = (
    alfDisksNumber,
    alfDiskNumber);

  TArchiveLocatorFlags = set of TArchiveLocatorFlag;

  /// archive binding flag
  TArchiveBindingFlag = (
    abfComment);

  TArchiveBindingFlags = set of TArchiveBindingFlag;

  /// archive item flag
  TArchiveItemFlag = (
    aifDefaultFlags,
    aifUncompressedSize,
    aifCreationTime,
    aifLastModifiedTime,
    aifLastAccessTime,
    aifAttributes,
    aifMode,
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
     FMagikSeek: int64;
   public
     constructor Create;
     procedure Read(Stream: TFileReader);
     procedure Write(Stream: TFileWriter);
   public
     property Flags: TArchiveLocatorFlags read FFlags;
     property DisksNumber: longword read FDisksNumber;
     property DiskNumber: longword read FDiskNumber;
     property DiskSeek: int64 read FDiskSeek;
     property MagikSeek: int64 read FMagikSeek;
   end;

  TArchiveBinding = class(TObject)
  private
    FFlags: TArchiveBindingFlags;
    FComment: string;
  public
    constructor Create;
    procedure Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
  public
    property Flags: TArchiveBindingFlags read FFlags;
    property Comment: string read FComment;
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
    FCRC32: longword;
    FDiskNumber: longword;
    FDiskSeek: int64;
    FUserID: longword;
    FUserName: string;
    FGroupID: longword;
    FGroupName: string;
    FComment: string;
    FFileName: string;
    FCompressionMethod: TArchiveCompressionMethod;
    FCompressionFlags: TArchiveCompressionFlags;
    FCompressedSize: int64;
    FCompressionLevel: TmOption;
    FDictionaryLevel: TdOption;
    FCompressionTable: TTableParameters;
    FEncryptionMethod: TArchiveEncryptionMethod;

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
    property CRC32: longword read FCRC32;
    property DiskNumber: longword read FDiskNumber;
    property DiskSeek: int64 read FDiskSeek;
    property UserID: longword read FUserID;
    property UserName: string read FUserName;
    property GroupID: longword read FGroupID;
    property GroupName: string read FGroupName;
    property Comment: string read FComment;
    property FileName: string read FFileName;

    property CompressionMethod: TArchiveCompressionMethod read FCompressionMethod;
    property CompressionFlags: TArchiveCompressionFlags read FCompressionFlags;
    property CompressedSize: int64 read FCompressedSize;
    property CompressionLevel: TmOption read FCompressionLevel;
    property DictionaryLevel: TdOption read FDictionaryLevel;
    property SolidCompression: boolean read GetSolidCompression write SetSolidCompression;
    property CompressionTable: TTableParameters read FCompressionTable;
    property EncryptionMethod: TArchiveEncryptionMethod read FEncryptionMethod;

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
    FCompressionMethod: TArchiveCompressionMethod;
    FCompressionLevel: TmOption;
    FDictionaryLevel: TdOption;
    FSolidCompression: boolean;
    FEncryptionMethod: TArchiveEncryptionMethod;
    FConfigurationName: string;
    FConfiguration: TConfiguration;
    FForceFileExtension: string;
    FOnUpdate: TArchiveUpdateEvent;
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
    property CompressionMethod: TArchiveCompressionMethod
      read FCompressionMethod write FCompressionMethod;

    property CompressionLevel: TmOption
      read FCompressionLevel write FCompressionLevel;

    property DictionaryLevel: TdOption
      read FDictionaryLevel  write FDictionaryLevel;

    property SolidCompression: boolean
      read FSolidCompression write FSolidCompression;

    property EncrypionMethod: TArchiveEncryptionMethod
      read FEncryptionMethod write FEncryptionMethod;

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
  FFlags       := [];
  FDisksNumber :=  0;
  FDiskNumber  :=  0;
  FMagikSeek   :=  0;
end;

procedure TArchiveLocator.Read(Stream: TFileReader);
begin
  FFlags := TArchiveLocatorFlags(longword(Stream.ReadInfWord));
  if (alfDisksNumber in FFlags) then
    FDisksNumber := Stream.ReadInfWord;

  if (alfDiskNumber  in FFlags) then
    FDiskNumber := Stream.ReadInfWord;

  FDiskSeek  := Stream.ReadInfWord;
  FMagikSeek := ReadMagicSeek(Stream);
end;

procedure TArchiveLocator.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));
  if (alfDisksNumber in FFlags) then
    Stream.WriteInfWord(FDisksNumber);

  if (alfDiskNumber  in FFlags) then
    Stream.WriteInfWord(FDiskNumber);

  Stream.WriteInfWord(FDiskSeek);
  WriteMagicSeek(Stream, FMagikSeek);
end;

// TArchiveBinding class

constructor TArchiveBinding.Create;
begin
  inherited Create;
  FFlags   := [];
  FComment := '';
end;

procedure TArchiveBinding.Read(Stream: TFileReader);
begin
  inherited Create;
  FFlags := TArchiveBindingFlags(longword(Stream.ReadInfWord));

  if (abfComment in FFlags) then
    FComment := Stream.ReadInfString;
end;

procedure TArchiveBinding.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));

  if (abfComment in FFlags) then
    Stream.WriteInfString(FComment);
end;

// TArchiveItem class

constructor TArchiveItem.Create(DefaultFlags: TArchiveItemFlags);
begin
  inherited Create;
  FFlags := DefaultFlags;
  FUncompressedSize  :=  0;
  FCreationTime      :=  0;
  FLastModifiedTime  :=  0;
  FLastAccessTime    :=  0;
  FAttributes        :=  0;
  FMode              :=  0;
  FUserID            :=  0;
  FUserName          := '';
  FGroupID           :=  0;
  FGroupName         := '';
  FComment           := '';
  FFileName          := '';

  // Data descriptor
  FCompressionMethod := actNone;
  FCompressionFlags  := [];
  FCRC32             :=  0;
  FDiskNumber        :=  0;
  FDiskSeek          :=  0;
  FCompressedSize    :=  0;
  FCompressionLevel  := moStore;
  FDictionaryLevel   := do2MB;
//FCompressionTable  := EmptyTableParameters;
  FEncryptionMethod  := acrtNone;

  FTag               := aitNone;
  FPosition          := -1;
  FExternalFileName  := '';
  FExternalFileSize  :=  0;
end;

procedure TArchiveItem.Update(SearchRec: TCustomSearchRec);
begin
//if (aifUncompressedSize in FFLags) then FUncompressedSize := SearchRec.Size;
  if (aifCreationTime     in FFLags) then FCreationTime     := SearchRec.CreationTime;
  if (aifLastModifiedTime in FFLags) then FLastModifiedTime := SearchRec.LastModifiedTime;
  if (aifLastAccessTime   in FFLags) then FLastAccessTime   := SearchRec.LastAccessTime;
  if (aifAttributes       in FFLags) then FAttributes       := SearchRec.Attributes;
  if (aifMode             in FFLags) then FMode             := SearchRec.Mode;
  if (aifUserID           in FFLags) then FUserID           := SearchRec.UserID;
  if (aifUserName         in FFLags) then FUserName         := SearchRec.UserName;
  if (aifGroupID          in FFLags) then FGroupID          := SearchRec.GroupID;
  if (aifGroupName        in FFLags) then FGroupName        := SearchRec.GroupName;
//if (aifComment          in FFLags) then FComment          := UseComment;

  FFileName := SearchRec.Name;

  // Data descriptor
  // FCompressionMethod := actNone;
  // FCompressionFlags  := [];
  // FCRC32             :=  0;
  // FDiskNumber        :=  0;
  // FDiskSeek          :=  0;
  // FCompressedSize    :=  0;
  // FCompressionLevel  :=  0;
  // FDictionaryLevel   :=  0;
  // FCompressionTable  :=  EmptyTableParameters;
  // FEncryptionMethod  :=  0;

  // FTag           := aitNone;
  // FPosition      := -1;
  FExternalFileName := SearchRec.Name;
  FExternalFileSize := SearchRec.Size;
end;

constructor TArchiveItem.Read(Stream: TFileReader);
begin
  FFileName := Stream.ReadInfString;
  FFlags    := TArchiveItemFlags(longword(Stream.ReadInfWord));

  if (aifUncompressedSize in FFlags) then FUncompressedSize := Stream.ReadInfWord;
  if (aifCreationTime     in FFlags) then FCreationTime     := Stream.ReadInfWord;
  if (aifLastModifiedTime in FFlags) then FLastModifiedTime := Stream.ReadInfWord;
  if (aifLastAccessTime   in FFlags) then FLastAccessTime   := Stream.ReadInfword;
  if (aifAttributes       in FFlags) then FAttributes       := Stream.ReadInfWord;
  if (aifMode             in FFlags) then FMode             := Stream.ReadInfWord;
  if (aifUserID           in FFlags) then FUserID           := Stream.ReadInfWord;
  if (aifUserName         in FFlags) then FUserName         := Stream.ReadInfString;
  if (aifGroupID          in FFlags) then FGroupID          := Stream.ReadInfWord;
  if (aifGroupName        in FFlags) then FGroupName        := Stream.ReadInfString;
  if (aifComment          in FFlags) then FComment          := Stream.ReadInfString;

  // Data descryptor
  if FUncompressedSize > 0 then
  begin
    FCompressionMethod := TArchiveCompressionMethod(longword(Stream.ReadInfWord));
    FDiskNumber        := Stream.ReadInfWord;
    FDiskSeek          := Stream.ReadInfWord;
    FCRC32             := Stream.ReadInfWord;

    if FCompressionMethod = actMain then
    begin
      FCompressionFlags := TArchiveCompressionFlags(longword(Stream.ReadInfWord));
      if (acfCompressionLevel in FCompressionFlags) then FCompressionLevel := TmOption(Stream.ReadInfWord);
      if (acfDictionaryLevel  in FCompressionFlags) then FDictionaryLevel  := TdOption(Stream.ReadInfWord);
      if (acfCompressionTable in FCompressionFlags) then Stream.Read(FCompressionTable, SizeOf(TTableParameters));
      FCompressedSize := Stream.ReadInfWord;
    end;
    FEncryptionMethod := TArchiveEncryptionMethod(longword(Stream.ReadInfWord));
  end;
end;

procedure TArchiveItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfString(FFileName);
  Stream.WriteInfWord(longword(FFlags));

  if (aifUncompressedSize in FFlags) then Stream.WriteInfWord(FUncompressedSize);
  if (aifCreationTime     in FFlags) then Stream.WriteInfWord(FCreationTime);
  if (aifLastModifiedTime in FFlags) then Stream.WriteInfWord(FLastModifiedTime);
  if (aifLastAccessTime   in FFlags) then Stream.WriteInfWord(FLastAccessTime);
  if (aifAttributes       in FFlags) then Stream.WriteInfWord(FAttributes);
  if (aifMode             in FFlags) then Stream.WriteInfWord(FMode);
  if (aifUserID           in FFlags) then Stream.WriteInfWord(FUserID);
  if (aifUserName         in FFlags) then Stream.WriteInfString(FUserName);
  if (aifGroupID          in FFlags) then Stream.WriteInfWord(FGroupID);
  if (aifGroupName        in FFlags) then Stream.WriteInfString(FGroupName);
  if (aifComment          in FFlags) then Stream.WriteInfString(FComment);

  // Data descriptor
  if FUncompressedSize > 0 then
  begin
    Stream.WriteInfWord(Ord(FCompressionMethod));
    Stream.WriteInfWord(FDiskNumber);
    Stream.WriteInfWord(FDiskSeek);
    Stream.WriteInfWord(FCRC32);

    if FCompressionMethod = actMain then
    begin
      Stream.WriteInfWord(longword(FCompressionFlags));
      if (acfCompressionLevel in FCompressionFlags) then Stream.WriteInfWord(Ord(FCompressionLevel));
      if (acfDictionaryLevel  in CompressionFlags)  then Stream.WriteInfWord(Ord(FDictionaryLevel));
      if (acfCompressionTable in FCompressionFlags) then Stream.Write(FCompressionTable, SizeOf(TTableParameters));
      Stream.WriteInfWord(FCompressedSize);
    end;
    Stream.WriteInfWord(Ord(FEncryptionMethod));
  end;
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
    TArchiveItem(FItems[I]).Destroy;
  FItems.Clear;
  FNames.Clear;
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
    if  (not(aifDefaultFlags in Next.Flags)) then
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

      if    (aifCRC32 in Item.Flags) and
        (not(aifCRC32 in Next.Flags)) then
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
  FExitCode     := ccSuccesful;
  FArchiveItems := TBeeArchiveCustomItems.Create;
end;

destructor TArchiveReaderBase.Destroy;
begin
  FArchiveItems.Destroy;
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
  Marker: longword;
  Locator: TArchiveLocator;
  Binding: TArchiveBinding;
begin
  Result  := FALSE;
  Marker  := longword(aStream.ReadInfWord);
  if Marker = aitLocator then
    if longword(aStream.ReadInfWord) <= beexVERSION then
    begin
      Locator := TArchiveLocator.Create;
      Locator.Read(aStream);

      aStream.ImagesNumber := Locator.DisksNumber;
      aStream.SeekImage(Locator.DiskNumber, Locator.DiskSeek);
      if aStream.ReadDWord = beexMARKER then
      begin
        Binding := TArchiveBinding.Create;
        repeat
          Marker := longword(aStream.ReadInfWord);
          case Marker of
            aitItem:    FArchiveItems.Add(TArchiveItem.Read(aStream));
            aitBinding: Binding.Read(aStream);
          //aitLocator: already readed;
            else        Marker := aitEnd;
          end;
          Result := (Marker = aitEnd);
        until Result;
        Binding.Destroy;
      end;
      Locator.Destroy;
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
    if not (aifDefaultFlags in CurrentItem.Flags) then
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
    aifCRC32,
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
      FDecoder.DictionaryLevel := Ord(Item.DictionaryLevel);

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
    case Item.CompressionMethod of
      actMain: FDecoder.Decode(FSwapWriter, Item.FUncompressedSize, CRC);
      else     FDecoder.Copy  (FSwapWriter, Item.FUncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not FSwapWriter   .IsValidStream then DoFailure(cmStrmWriteError);

    if Item.FCRC32 <> CRC then
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
    case Item.CompressionMethod of
      actMain: FDecoder.Decode(Stream, Item.FUncompressedSize, CRC);
      else     FDecoder.Copy  (Stream, Item.FUncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not Stream        .IsValidStream then DoFailure(cmStrmWriteError);

    if Item.FCRC32 <> CRC then
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
    case Item.CompressionMethod of
      actMain: FDecoder.Decode(Stream, Item.FUncompressedSize, CRC);
      else     FDecoder.Copy  (Stream, Item.FUncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not Stream        .IsValidStream then DoFailure(cmStrmWriteError);

    if Item.FCRC32 <> CRC then
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
    FOnMessage(Message);
end;

function TArchiveReaderBase.DoProgress(Value: longint): boolean;
begin
  Inc(FProcessedSize, Value);
  if Assigned(FOnProgress) then
    DoProgress(Round(FProcessedSize/FTotalSize * 100));

  while FSuspended do Sleep(250);
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
  FThreshold       :=  0;
  FWorkDirectory   := '';
end;

procedure TArchiveWriterBase.Write(aStream: TFileWriter);
var
  I: longword;
  Locator: TArchiveLocator;
  Binding: TArchiveBinding;
begin
  Locator := TArchiveLocator.Create;
  Locator.FDiskNumber := aStream.CurrentImage;
  if Locator.FDiskNumber > 0 then
    Include(Locator.FFlags,  alfDiskNumber);
  Locator.FDiskSeek := aStream.Position;

  aStream.WriteDWord(beexMARKER);
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    aStream.WriteInfWord(aitItem);
    FArchiveItems.Items[I].Write(aStream);
  end;
  aStream.WriteInfWord(aitBinding);
  Binding := TArchiveBinding.Create;
  Binding.Write(aStream);
  Binding.Destroy;

  if aStream.Threshold > 0 then aStream.CreateImage;

  Locator.FDisksNumber := aStream.CurrentImage;
  if Locator.FDisksNumber > 0 then
    Include(Locator.FFlags,  alfDisksNumber);

  Locator.FMagikSeek := aStream.Position;
  aStream.WriteInfWord(aitLocator);
  Locator.Write(aStream);
  Locator.Destroy;
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
    begin
      CurrentItem := FArchiveItems.Items[I];
      if (aifDefaultFlags in CurrentItem.FFlags) then
      begin
        Exclude(CurrentItem.FFlags, aifUncompressedSize);
        Exclude(CurrentItem.FFlags, aifCreationTime);
        Exclude(CurrentItem.FFlags, aifLastModifiedTime);
        Exclude(CurrentItem.FFlags, aifLastAccessTime);
        Exclude(CurrentItem.FFlags, aifAttributes);
        Exclude(CurrentItem.FFlags, aifMode);
        Exclude(CurrentItem.FFlags, aifUserID);
        Exclude(CurrentItem.FFlags, aifUserName);
        Exclude(CurrentItem.FFlags, aifGroupID);
        Exclude(CurrentItem.FFlags, aifGroupName);
        Exclude(CurrentItem.FFlags, aifComment);
        Exclude(CurrentItem.FCompressionFlags, acfCompressionLevel);
        Exclude(CurrentItem.FCompressionFlags, acfDictionaryLevel);
        // Exclude(CurrentItem.FCompressionFlags, acfCompressionTable);

        if CurrentItem.FUncompressedSize <> PreviusItem.FUncompressedSize then
          Include(CurrentItem.FFlags, aifUncompressedSize);

        if CurrentItem.FCreationTime <> PreviusItem.FCreationTime then
          Include(CurrentItem.FFlags, aifCreationTime);

        if CurrentItem.FLastModifiedTime <> PreviusItem.FLastModifiedTime then
          Include(CurrentItem.FFlags, aifLastModifiedTime);

        if CurrentItem.FLastAccessTime <> PreviusItem.FLastAccessTime then
          Include(CurrentItem.FFlags, aifLastAccessTime);

        if CurrentItem.FAttributes <> PreviusItem.FAttributes then
          Include(CurrentItem.FFlags, aifAttributes);

        if CurrentItem.FMode <> PreviusItem.FMode then
          Include(CurrentItem.FFlags, aifMode);

        if CurrentItem.FUserID <> PreviusItem.FUserID then
          Include(CurrentItem.FFlags, aifUserID);

        if CurrentItem.FUserName <> PreviusItem.FUserName then
          Include(CurrentItem.FFlags, aifUserName);

        if CurrentItem.FGroupID <> PreviusItem.FGroupID then
          Include(CurrentItem.FFlags, aifGroupID);

        if CurrentItem.FGroupName <> PreviusItem.FGroupName then
          Include(CurrentItem.FFlags, aifGroupName);

        if CurrentItem.FComment <> PreviusItem.FComment then
          Include(CurrentItem.FFlags, aifComment);

        // Data descriptor
        if CurrentItem.FCompressionLevel <> PreviusItem.FCompressionLevel then
          Include(CurrentItem.FCompressionFlags, acfCompressionLevel);

        if CurrentItem.FDictionaryLevel <> PreviusItem.FDictionaryLevel then
          Include(CurrentItem.FCompressionFlags, acfDictionaryLevel);

        //if CurrentItem.FCompressionTable <> PreviusItem.FCompressionTable then
        //  Include(CurrentItem.FCompressionFlags, acfCompressionTable);
      end;
      PreviusItem := CurrentItem;
    end;
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
      FEncoder.DictionaryLevel := Ord(Item.DictionaryLevel);

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
      actMain: FEncoder.Encode(FSwapReader, Item.FUncompressedSize, Item.FCRC32);
      else     FEncoder.Copy  (FSwapReader, Item.FUncompressedSize, Item.FCRC32);
    end;
    Item.FCompressedSize := FTempWriter.ABSPosition - ABSPosition;

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
    Item.FUncompressedSize := Item.FExternalFileSize;

    ABSPosition      := FTempWriter.ABSPosition;
    Item.FDiskSeek   := FTempWriter.Position;
    Item.FDiskNumber := FTempWriter.CurrentImage;
    case Item.CompressionMethod of
      actMain: FEncoder.Encode(Stream, Item.FUncompressedSize, Item.FCRC32);
      else     FEncoder.Encode(Stream, Item.FUncompressedSize, Item.FCRC32);
    end;
    Item.FCompressedSize   := FTempWriter.ABSPosition - ABSPosition;

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
  FCompressionLevel   := moFast;
  FDictionaryLevel    := do2MB;
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
    FConfiguration.CurrentSection.Values['Method'] := IntToStr(Ord(FCompressionLevel));
    FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(Ord(FDictionaryLevel));
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
        if CurrentItem.FCompressionMethod = actMain then
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

procedure TBeeArchiveUpdater.SetConfigurationName(const Value: string);
begin
  FConfigurationName := Value;
end;

procedure TBeeArchiveUpdater.SetForceFileExtension(const Value: string);
begin
  FForceFileExtension := Value;
end;

end.

