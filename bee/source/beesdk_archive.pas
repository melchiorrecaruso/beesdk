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
  aitItem      = $00;
  aitBinding   = $01;
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
    alifDisksNumber,
    alifDiskNumber);

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
    aifDefaultFlags,
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
    amcfCompressionLevel,
    amcfDictionaryLevel,
    amcfSolidCompression,
    amcfCompressionTable);

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
     procedure Clear;
     procedure Write(Stream: TFileWriter);
     function Read(Stream: TFileReader): boolean;
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
    procedure Clear;
    procedure Write(Stream: TFileWriter);
    function Read(Stream: TFileReader): boolean;
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
    FTag: TArchiveItemTag;
    FFlags: TArchiveItemFlags;
    FUncompressedSize: int64;
    FCreationTime: longword;
    FLastModifiedTime: longword;
    FLastAccessTime: longword;
    FAttributes: longword;
    FMode: longword;
    FCRC: longword;
    FDiskNumber: longword;
    FDiskSeek: qword;
    FUserID: longword;
    FGroupID: longword;
    FUserName: string;
    FGroupName: string;
    FComment: string;
    FFileName: string;

    FCompressionMethod: longint;
    FCompressionFlags: TArchiveCompressionFlags;
    FCompressedSize: qword;
    FCompressionLevel: longword;
    FDictionaryLevel: longword;
    FCompressionTable: TTableParameters;
    FEncryptionMethod: longint;

    FExternalFileName: string;
    FPosition: longint;
  protected {property methods}
    procedure SetUncompressedSize(const Value: int64);
    procedure SetCRC(Value: longword);

    procedure SetDiskNumber(Value: longword);
    procedure SetDiskSeek(const Value: qword);

    procedure SetUserID(Value: longword);
    procedure SetUserName(const Value: string);
    procedure SetGroupID(Value: longword);
    procedure SetGroupName(const Value: string);
    procedure SetComment(const Value: string);
    procedure SetFileName(const Value: string);


    procedure SetCompressionLevel(Value: longword);
    procedure SetDictionaryLevel(Value: longword);
    procedure SetCompressionTable(const Value: TTableParameters);
    function GetSolidCompression: boolean;
    procedure SetSolidCompression(Value: boolean);
    procedure SetCompressedSize(const Value: qword);
  public {methods}
    constructor Create;
    constructor Create(SearchRec: TCustomSearchRec; const UseFileName: string;
      UseDefaultFlags: TArchiveItemFlags);
    procedure Write(Stream: TFileWriter);
    function Read(Stream: TFileReader): boolean;
    procedure Update(SearchRec: TCustomSearchRec);
  public {property}
    property Flags: TArchiveItemFlags read FFlags;
    property UncompressedSize: int64 read FUncompressedSize write SetUncompressedSize;
    property CreationTime: longword read FCreationTime;
    property LastModifiedTime: longword read FLastModifiedTime;
    property LastAccessTime: longword read FLastAccessTime;
    property Attributes: longword read FAttributes;
    property Mode: longword read FMode;
    property CRC: longword read FCRC write SetCRC;
    property DiskNumber: longword read FDiskNumber write SetDiskNumber;
    property DiskSeek: qword read FDiskSeek write SetDiskSeek;
    property UserID: longword read FUserID write SetUserID;
    property UserName: string read FUserName write SetUserName;
    property GroupID: longword read FGroupID write SetGroupID;
    property GroupName: string read FGroupName write SetGroupName;
    property Comment: string read FComment write SetComment;
    property FileName: string read FFileName;
    property Position: longint read FPosition;

    property CompressionFlags: TArchiveCompressionFlags read FCompressionFlags;
    property CompressedSize: qword read FCompressedSize write SetCompressedSize;
    property CompressionLevel: longword read FCompressionLevel write SetCompressionLevel;
    property DictionaryLevel: longword read FDictionaryLevel write SetDictionaryLevel;
    property SolidCompression: boolean read GetSolidCompression write SetSolidCompression;
    property CompressionTable: TTableParameters read FCompressionTable write SetCompressionTable;
  end;





  TBeeArchiveCustomItems = class(TObject)
  private {private}
    FItems: TList;
    FNames: TList;
  private { methods}
    function FindName(const FileName: string): longint;
    function GetCount : longint;
    function GetItem(Index: longint): TArchiveItem;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item : TArchiveItem);
    function Find(const FileName: string): longint;
    procedure Delete(Index: longint);
    procedure Clear;
  public {properties}
    property Count: longint read GetCount;
    property Items[Index: longint]: TArchiveItem read GetItem;
  end;

  TBeeArchiveProgressEvent = procedure(Value: longint) of object;

  TBeeArchiveMessageEvent = procedure(const Message: string) of object;

  TBeeArchiveFailureEvent = procedure(
    const ErrorMessage: string; ErrorCode: longint) of object;

  TBeeArchiveConfirm = (arcOk, arcCancel, arcAbort);

  TBeeArchiveRenameEvent = procedure(Item: TArchiveItem;
    var RenameAs: string; var Confirm: TBeeArchiveConfirm) of object;

  TBeeArchiveExtractEvent = procedure(Item: TArchiveItem;
    var ExtractAs: string; var Confirm: TBeeArchiveConfirm) of object;

  TBeeArchiveEraseEvent = procedure(Item: TArchiveItem;
    var Confirm: TBeeArchiveConfirm) of object;

  TBeeStreamMode = (emToStream, emToFile,  emToNul);

  // a class for each command

  TBeeArchiveReader = class(TObject)
  private
    FDecoder: THeaderDecoder;
    FTotalSize: int64;
    FProcessedSize: int64;
    FArchiveName: string;
    FArchiveReader: TFileReader;
    FSuspended:  boolean;
    FTerminated: boolean;
    FExitCode: byte;
    FOnFailure: TBeeArchiveFailureEvent;
    FOnMessage: TBeeArchiveMessageEvent;
    FOnProgress: TBeeArchiveProgressEvent;
    FOnRequestImage: TFileReaderRequestImageEvent;
    FArchiveCustomItems: TBeeArchiveCustomItems;
    FArchiveBindingItem: TBeeArchiveBinding;
    FArchiveLocatorItem: TArchiveLocator;
    function Read(aStream: TFileReader): boolean;
    procedure UnPack;


    procedure Swap   (Item: TArchiveItem; Stream: TFileWriter);
    procedure Test   (Item: TArchiveItem);
    procedure Extract(Item: TArchiveItem);



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

    procedure TagAll;
    procedure Tag(Index: longint); virtual; overload;
    procedure Tag(const FileMask: string; Recursive: TRecursiveMode); overload;
    procedure UnTagAll;
    procedure UnTag(Index: longint); virtual; overload;
    procedure UnTag(const FileMask: string; Recursive: TRecursiveMode); overload;

    function IsTagged(Index: longint): boolean; virtual;

    function Find(const aFileName: string): longint;
    procedure Terminate;
  public
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property Items[Index: longint]: TArchiveItem read GetItem;
    property Count: longint read GetCount;

    property Suspended: boolean read FSuspended write SetSuspended;
    property Terminated: boolean read FTerminated;
    property ExitCode: byte read FExitCode;

    property OnFailure: TBeeArchiveFailureEvent read FOnFailure write FOnFailure;
    property OnMessage: TBeeArchiveMessageEvent read FOnMessage write FOnMessage;
    property OnProgress: TBeeArchiveProgressEvent read FOnProgress write FOnProgress;
    property OnRequestImage: TFileReaderRequestImageEvent
      read FOnRequestImage write FOnRequestImage;
  end;

  TBeeArchiveWriter = class(TBeeArchiveReader)
  private
    FEncoder: THeaderEncoder;
    FIsNeededToSave: boolean;
    FIsNeededToSwap: boolean;
    FThreshold: int64;
    FTempName: string;
    FTempWriter: TFileWriter;
    FSwapName: string;
    FSwapReader: TFileReader;
    FSwapWriter: TFileWriter;
    FWorkDirectory: string;
    FOnRequestBlankDisk: TFileWriterRequestBlankDiskEvent;
  private
    procedure Write(aStream: TFileWriter);
    procedure Pack;
    procedure OpenSwap;
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

  TBeeArchiveExtractor = class(TBeeArchiveReader)
  private
    FIsNeededToExtract: boolean;
    FOnExtract: TBeeArchiveExtractEvent;
    procedure CheckTags;
    procedure CheckSequences;
    procedure DoExtract(Item: TArchiveItem;
      var ExtractAs: string; var Confirm: TBeeArchiveConfirm);
  public
    constructor Create;
    procedure ExtractTagged;
    procedure TestTagged;
  public
    property OnExtraction: TBeeArchiveExtractEvent
      read FOnExtract write FOnExtract;
  end;

  TBeeArchiveRenamer = class(TBeeArchiveWriter)
  private
    FOnRename: TBeeArchiveRenameEvent;
    procedure CheckTags;
    procedure DoRename(Item: TArchiveItem;
      var RenameAs: string; var Confirm: TBeeArchiveConfirm);
  public
    procedure RenameTagged;
  public
    property OnRenameEvent: TBeeArchiveRenameEvent read FOnRename write FOnRename;
  end;

  TBeeArchiveEraser = class(TBeeArchiveWriter)
  private
    FOnErase: TBeeArchiveEraseEvent;
    procedure CheckTags;
    procedure CheckSequences;
    procedure DoErase(Item: TArchiveItem;
      var Confirm: TBeeArchiveConfirm);
  public
    procedure EraseTagged;
  public
    property OnEraseEvent: TBeeArchiveEraseEvent read FOnErase write FOnErase;
  end;

  TBeeArchiveAdder = class(TBeeArchiveWriter)
  private
  public
  public
  end;



  (*

  TBeeArchiveAdder = class(TBeeArchiveViewer)
  private
    FWorkDirectory: string;
    FNeededToSave: boolean;

    FSwapName: string;
    FSwapReader: TFileReader;
    FSwapWriter: TFileWriter;

    FTempName: string;
    FTempWriter: TFileWriter;

    FDefaultCustomItemFlags: TBeeArchiveCustomItemFlags;
    FDefaultBindingItemFlags: TBeeArchiveBindingItemFlags;

    FCompressionCoder: TBeeArchiveCoderType;
    FCrypterCoder: TBeeArchiveCrypterType;

    FCompressionLevel: longint;
    FDictionaryLevel: longint;
    FConfigurationName: string;
    FConfiguration: TConfiguration;

    FSolid: boolean;
    FForceFileExtension: string;

    FBeeEncoder: THeaderEncoder;
    FBeeDecoder: THeaderDecoder;


    procedure Encode(Item: TBeeArchiveCustomItem);
    procedure Copy(Item: TBeeArchiveCustomItem);

    function CreateSwap: boolean;
    procedure ConfigureCoders;
    procedure ConfigureCrypters;
    procedure SetCompressionLevel(Value: longint);
    procedure SetDictionaryLevel(Value: longint);
    procedure SetConfigurationName(const Value: string);

  public

    procedure Add(SearchRec: TCustomSearchRec; const UseFileName: string;
         UseFlags: TBeeArchiveCustomItemFlags);

       procedure Update(SearchRec: TCustomSearchRec; const UseFileName: string;
          UseFlags: TBeeArchiveCustomItemFlags);

       procedure Replace(SearchRec: TCustomSearchRec; const UseFileName: string;
          UseFlags: TBeeArchiveCustomItemFlags);
  public
    property DefaultCustomItemFlags: TBeeArchiveCustomItemFlags
      read FDefaultCustomItemFlags write FDefaultCustomItemFlags;
    property DefaultBindingItemFlags: TBeeArchiveBindingItemFlags
      read FDefaultBindingItemFlags write FDefaultBindingItemFlags;

    property CompressionCoder: TBeeArchiveCoderType
      read FCompressionCoder write FCompressionCoder;
    property CrypterCoder: TBeeArchiveCrypterType
      read FCrypterCoder write FCrypterCoder;

    property CompressionLevel: longint
      read FCompressionLevel write SetCompressionLevel;
    property DictionaryLevel: longint
      read FDictionaryLevel write SetDictionaryLevel;
    property ConfigurationName: string
      read FConfigurationName write SetConfigurationName;

    property ForceFileExtension: string
      read FForceFileExtension write FForceFileExtension;

    property Solid: boolean read FSolid write FSolid;

    property WorkDirectory: string read FWorkDirectory write FWorkDirectory;

  end;

  TBeeArchiveEraser = class(TBeeArchiveViewer)
  private
  public
     procedure Delete(Index: longint);
  end;

  TBeeArchiveExtractor = class(TBeeArchiveViewer)
  private
  public
  end;

  TBeeArchiveTester = class(TBeeArchiveViewer)
  private
  public
  end;


        *)

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
  Clear;
end;

procedure TArchiveLocator.Clear;
begin
  FFlags := [];
  FDisksNumber := 0;
  FDiskNumber := 0;
end;

procedure TArchiveLocator.SetDisksNumber(Value: longword);
begin
  FDisksNumber := Value;
  if FDisksNumber <> 0 then
    Include(FFlags, alifDisksNumber)
  else
    Exclude(FFlags, alifDisksNumber);
end;

procedure TArchiveLocator.SetDiskNumber(Value: longword);
begin
  FDiskNumber := Value;
  if FDiskNumber <> 0 then
    Include(FFlags, alifDiskNumber)
  else
    Exclude(FFlags, alifDiskNumber);
end;

procedure TArchiveLocator.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(beexVERSION));
  Stream.WriteInfWord(longword(FFlags));

  if (alifDisksNumber in FFlags) then
    Stream.WriteInfWord(FDisksNumber);

  if (alifDiskNumber in FFlags) then
    Stream.WriteInfWord(FDiskNumber);

  WriteMagicSeek(Stream, FDiskSeek);
end;

function TArchiveLocator.Read(Stream: TFileReader): boolean;
begin
  Result := Stream.ReadInfWord <= beexVERSION;

  Clear;
  if Result then
  begin
    FFlags := TArchiveLocatorFlags(longword(Stream.ReadInfWord));
    if (alifDisksNumber in FFlags) then
      FDisksNumber := Stream.ReadInfWord;

    if (alifDiskNumber in FFlags) then
      FDiskNumber := Stream.ReadInfWord;

    FDiskSeek := ReadMagicSeek(Stream);
  end;
end;

// TBeeArchiveBinding class

constructor TBeeArchiveBinding.Create;
begin
  inherited Create;
  Clear;
end;

procedure TBeeArchiveBinding.Clear;
begin
  FFlags   := [];
  FID      := '';
  FCRC     :=  0;
  FSfxSize :=  0;
  FComment := '';
end;

procedure TBeeArchiveBinding.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(beexVERSION));
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

function TBeeArchiveBinding.Read(Stream: TFileReader): boolean;
begin
  Result := Stream.ReadInfWord <= beexVERSION;

  Clear;
  if Result then
  begin
    FFlags := TArchiveBindingFlags(longword(Stream.ReadInfWord));
    if (abfID in FFlags) then
      FID := Stream.ReadInfString;

    if (abfCRC in FFlags) then
      FCRC := Stream.ReadInfWord;

    if (abfSelfExtractorSize in FFlags) then
      FSfxSize := Stream.ReadInfWord;

    if (abfComment in FFlags) then
      FComment := Stream.ReadInfString;
  end;
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

constructor TArchiveItem.Create;
begin
  inherited Create;
  FFlags := [];
end;

constructor TArchiveItem.Create(SearchRec: TCustomSearchRec;
  const UseFileName: string; UseDefaultFlags: TArchiveItemFlags);
begin
  inherited Create;
  FTag   := aitAdd;
  FFlags := UseDefaultFlags;

  Update(SearchRec);

  FComment  := '';
  FFileName := UseFileName;

  // data descriptor
  FCompressionMethod := 0;
  FCompressionFlags  := [];
  FEncryptionMethod  := 0;
end;

procedure TArchiveItem.Update(SearchRec: TCustomSearchRec);
begin
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

  FExternalFileName := SearchRec.Name;
end;

function TArchiveItem.Read(Stream: TFileReader): boolean;
begin
  Result := Stream.ReadInfWord <= beexVERSION;
  if Result then
  begin
    // header
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
           if (amcfCompressionLevel in FCompressionFlags) then
             FCompressionLevel := Stream.ReadInfWord;
           if (amcfDictionaryLevel  in CompressionFlags) then
             FDictionaryLevel  := Stream.ReadInfWord;
           if (amcfCompressionTable in FCompressionFlags) then
             Stream.Read(FCompressionTable, SizeOf(TTableParameters));
           FCompressedSize := Stream.ReadInfWord;
         end;
    end;
    FEncryptionMethod := longword(Stream.ReadInfWord);
  end;
end;

procedure TArchiveItem.SetUncompressedSize(const Value: int64);
begin
  FUncompressedSize := Value;
  if FUncompressedSize <> 0 then
    Include(FFlags, aifUncompressedSize)
  else
    Exclude(FFlags, aifUncompressedSize);
end;











procedure TArchiveItem.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TArchiveItem.SetCompressionLevel(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FCompressionLevel := Value;
    Include(FCompressionFlags, amcfCompressionLevel);
  end else
    Exclude(FCompressionFlags, amcfCompressionLevel);
end;

procedure TArchiveItem.SetDictionaryLevel(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FDictionaryLevel := Value;
    Include(FCompressionFlags, amcfDictionaryLevel);
  end else
    Exclude(FCompressionFlags, amcfDictionaryLevel);
end;

procedure TArchiveItem.SetCompressionTable(const Value: TTableParameters);
var
  I: longint;
  Sum: longint;
begin
  Sum := 0;
  for I := Low(Value) to High(Value) do
  begin
    Inc(Sum, Value[I]);
  end;

  if Sum <> 0 then
  begin
    FCompressionTable := Value;
    Include(FCompressionFlags, amcfCompressionTable);
  end else
    Exclude(FCompressionFlags, amcfCompressionTable);
end;



function TArchiveItem.GetSolidCompression: boolean;
begin
  Result := amcfSolidCompression in FCompressionFlags;
end;

procedure TArchiveItem.SetSolidCompression(Value: boolean);
begin
  if Value then
    Include(FCompressionFlags, amcfSolidCompression)
  else
    Exclude(FCompressionFlags, amcfSolidCompression);
end;

procedure TArchiveItem.SetCompressedSize(const Value: qword);
begin
  FCompressedSize := Value; ;
end;



procedure TArchiveItem.SetDiskNumber(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FDiskNumber := Value;
    Include(FFlags, aifDiskNumber);
  end else
    Exclude(FFlags, aifDiskNumber);
end;

procedure TArchiveItem.SetDiskSeek(const Value: qword);
begin
  if Value <> qword(-1) then
  begin
    FDiskSeek := Value;
    Include(FFlags, aifDiskSeek);
  end else
    Exclude(FFlags, aifDiskSeek);
end;

procedure TArchiveItem.SetUserID(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FUserID := Value;
    Include(FFlags, aifUserID);
  end else
    Exclude(FFlags, aifUserID);
end;

procedure TArchiveItem.SetUserName(const Value: string);
begin
  if Value <> '' then
  begin
    FUserName := Value;
    Include(FFlags, aifUserName);
  end else
    Exclude(FFlags, aifUserName);
end;

procedure TArchiveItem.SetGroupID(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FGroupID := Value;
    Include(FFlags, aifGroupID);
  end else
    Exclude(FFlags, aifGroupID);
end;

procedure TArchiveItem.SetGroupName(const Value: string);
begin
  if Value <> '' then
  begin
    FGroupName := Value;
    Include(FFlags, aifGroupName);
  end else
    Exclude(FFlags, aifGroupName);
end;

procedure TArchiveItem.SetComment(const Value: string);
begin
  if Value <> '' then
  begin
    FComment := Value;
    Include(FFlags, aifComment);
  end else
    Exclude(FFlags, aifComment);
end;

procedure TArchiveItem.SetCRC(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FCRC := Value;
    Include(FFlags, aifCRC);
  end else
    Exclude(FFlags, aifCRC);
end;

procedure TArchiveItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(beexVERSION));
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
         if (amcfCompressionLevel in FCompressionFlags) then
           Stream.WriteInfWord(FCompressionLevel);
         if (amcfDictionaryLevel  in CompressionFlags)  then
           Stream.WriteInfWord(FDictionaryLevel);
         if (amcfCompressionTable in FCompressionFlags) then
           Stream.Write(FCompressionTable, SizeOf(TTableParameters));

         Stream.WriteInfWord(FCompressedSize);
       end;
  end;
  Stream.WriteInfWord(FEncryptionMethod);
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

function TBeeArchiveCustomItems.FindName(const FileName: string): longint;
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
  Result := FindName(FileName);
  if Result <> -1 then
  begin
    Result := TArchiveItem(FNames[Result]).Position
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

    if (Item.SolidCompression) and (not(Next.SolidCompression)) then
    begin
      Next.SolidCompression := TRUE;
    end;
  end;

  FNames.Delete(FindName(Item.FileName));
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

// TBeeArchiveReader class

constructor TBeeArchiveReader.Create;
begin
  inherited Create;
  FExitCode := ccSuccesful;

  FArchiveCustomItems := TBeeArchiveCustomItems.Create;
  FArchiveBindingItem := TBeeArchiveBinding.Create;
  FArchiveLocatorItem := TArchiveLocator.Create;
end;

destructor TBeeArchiveReader.Destroy;
begin
  FArchiveCustomItems.Destroy;
  FArchiveBindingItem.Destroy;
  FArchiveLocatorItem.Destroy;
  inherited Destroy;
end;

procedure TBeeArchiveReader.Terminate;
begin
  SetExitCode(ccUserAbort);
end;

function TBeeArchiveReader.GetCount: longint;
begin
  Result := FArchiveCustomItems.Count;
end;

function TBeeArchiveReader.GetItem(Index: longint): TArchiveItem;
begin
  Result := FArchiveCustomItems.Items[Index];
end;

procedure TBeeArchiveReader.SetTerminated(Value: boolean);
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

procedure TBeeArchiveReader.SetSuspended(Value: boolean);
begin
  if FTerminated = False then
  begin
    FSuspended := Value;
  end;
end;

procedure TBeeArchiveReader.SetExitCode(Value: byte);
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

procedure TBeeArchiveReader.SetArchiveName(const Value: string);
begin
  OpenArchive(Value);
end;

function TBeeArchiveReader.Read(aStream: TFileReader): boolean;
var
  T: longword;
begin
  Result := FALSE;
  T := longword(aStream.ReadInfWord);
  if T = aitLocator then
  begin
    if FArchiveLocatorItem.Read(aStream) = FALSE then Exit;

    if alifDisksNumber in FArchiveLocatorItem.FFlags then
      aStream.ImagesNumber := FArchiveLocatorItem.DisksNumber;
    if alifDiskNumber in FArchiveLocatorItem.FFlags then
      aStream.SeekImage(FArchiveLocatorItem.DiskNumber, FArchiveLocatorItem.DiskSeek);

    if aStream.ReadDWord = beexMARKER then
      repeat
        T := longword(aStream.ReadInfWord);
        case T of
          aitItem:  FArchiveCustomItems.Add(TArchiveItem.Read(aStream));
          aitBinding: FArchiveBindingItem.Read(aStream);
          // aitLocator: already readed;
          else        Exit;
        end;
        Result := (T = aitEnd);
      until Result;
  end;
end;

procedure TBeeArchiveReader.UnPack;
begin

end;

procedure TBeeArchiveReader.Swap(Item: TArchiveItem; Stream: TFileWriter);
var
  CRC: longword;
begin
  if Assigned(Stream) then
  begin
    FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);

    Item.DiskNumber := Stream.CurrentImage;
    Item.DiskSeek   := Stream.Seek(0, soCurrent);
    case Item.CompressionLevel of
      0: FDecoder.Copy  (Stream, Item.UncompressedSize, CRC);
    else FDecoder.Decode(Stream, Item.UncompressedSize, CRC);
    end;
    if not FArchiveReader.IsValidStream then DoFailure(cmStrmReadError);
    if not Stream        .IsValidStream then DoFailure(cmStrmWriteError);

    if not Item.Crc = CRC then
      DoFailure(Format(cmCrcError, [Item.FExternalFileName]));
  end else
    DoFailure(cmStrmWriteError);
end;

procedure TBeeArchiveReader.Test(Item: TArchiveItem);
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

procedure TBeeArchiveReader.Extract(Item: TArchiveItem);
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

function TBeeArchiveReader.GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
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

function TBeeArchiveReader.GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
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

function TBeeArchiveReader.GetBackTear(Index: longint): longint;
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

function TBeeArchiveReader.GetNextTear(Index: longint): longint;
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

procedure TBeeArchiveReader.OpenArchive(const aArchiveName: string);
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

procedure TBeeArchiveReader.CloseArchive;
begin
  FArchiveName := '';
  if FArchiveReader <> nil then
    FreeAndNil(FArchiveReader);

  FArchiveCustomItems.Clear;
  FArchiveBindingItem.Clear;
  // FArchiveLocatorItem.Clear;

  FProcessedSize := 0;
  FTotalSize     := 0;
end;

procedure TBeeArchiveReader.TagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do Tag(I);
end;

procedure TBeeArchiveReader.Tag(Index: longint);
begin
  FArchiveCustomItems.Items[Index].FTag := aitUpdate;
end;

procedure TBeeArchiveReader.Tag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
    with FArchiveCustomItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then Tag(I);
end;

procedure TBeeArchiveReader.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do UnTag(I);
end;

procedure TBeeArchiveReader.UnTag(Index: longint);
begin
  FArchiveCustomItems.Items[Index].FTag := aitNone;
end;

procedure TBeeArchiveReader.UnTag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
    with FArchiveCustomItems.Items[I] do
      if FileNameMatch(FileName, FileMask, Recursive) then UnTag(I);
end;

function TBeeArchiveReader.IsTagged(Index: longint): boolean;
begin
  Result := FArchiveCustomItems.Items[Index].FTag <> aitNone;
end;

function TBeeArchiveReader.Find(const aFileName: string): longint;
begin
  Result := FArchiveCustomItems.FindName(aFileName);
end;

procedure TBeeArchiveReader.DoFailure(const ErrorMessage: string);
begin
  SetExitCode(ccError);
  if Assigned(FOnFailure) then
    FOnFailure(ErrorMessage, ccError);
end;

procedure TBeeArchiveReader.DoMessage(const Message: string);
begin
  if Assigned(FOnMessage) then
  begin
    FOnMessage(Message);
  end;
end;

function TBeeArchiveReader.DoProgress(Value: longint): boolean;
begin
  while FSuspended do Sleep(250);

  Inc(FProcessedSize, Value);
  if Assigned(FOnProgress) then
  begin
    DoProgress(Round(FProcessedSize/FTotalSize * 100));
  end;
  Result := ExitCode < ccError;
end;

// TBeeArchiveWriter class

constructor TBeeArchiveWriter.Create;
begin
  inherited Create;
  FIsNeededToSave  := FALSE;
  FIsNeededToSwap  := FALSE;
  FThreshold       := 0;
  FWorkDirectory   := '';
end;

procedure TBeeArchiveWriter.Write(aStream: TFileWriter);
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

procedure TBeeArchiveWriter.Pack;
begin

end;

procedure TBeeArchiveWriter.OpenSwap;
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

          FDecoder.CompressionMethod := Item.CompressionLevel;
          FDecoder.DictionaryLevel   := Item.DictionaryLevel;
          FDecoder.TableParameters   := Item.CompressionTable;
          FDecoder.Tear              := Item.SolidCompression;

          if Item.FTag in [aitDecode, aitDecodeAndUpdate] then
          begin
            case Item.FTag of
              aitDecode:          DoMessage(Format(cmSwapping, [Item.FileName]));
              aitDecodeAndUpdate: DoMessage(Format(cmDecoding, [Item.FileName]));
            end;

            case Item.FTag of
              aitDecode:          Swap(Item, FSwapWriter);
              aitDecodeAndUpdate: Test(Item);
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
end;

procedure TBeeArchiveWriter.CloseArchive;
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
  FArchiveBindingItem.Clear;
  // FArchiveLocatorItem.Clear;

  FProcessedSize  := 0;
  FTotalSize      := 0;
end;

procedure TBeeArchiveWriter.SetWorkDirectory(const Value: string);
begin
  if (Value = '') or DirectoryExists(Value) then
  begin
    FWorkDirectory := IncludeTrailingBackSlash(Value);
  end;
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
  Confirm: TBeeArchiveConfirm;
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
  // STEP2: find sequences and mark ...
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
  var ExtractAs: string; var Confirm: TBeeArchiveConfirm);
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
        if Item.FTag in [aitUpdate, aitDecode] then
        begin
          case Item.FTag of
            aitUpdate:          DoMessage(Format(cmExtracting, [Item.FExternalFileName]));
            aitDecode:          DoMessage(Format(cmDecoding,   [Item.FExternalFileName]));
            aitDecodeAndUpdate: DoMessage(Format(cmExtracting, [Item.FExternalFileName]));
          end;

          FDecoder.CompressionMethod := Item.CompressionLevel;
          FDecoder.DictionaryLevel   := Item.DictionaryLevel;
          FDecoder.TableParameters   := Item.CompressionTable;
          FDecoder.Tear              := Item.SolidCompression;

          case Item.FTag of
            aitUpdate:          Extract(Item);
            aitDecode:          Test   (Item);
            aitDecodeAndUpdate: Extract(Item);
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
      if Item.FTag in [aitUpdate, aitDecode] then
      begin
        case Item.FTag of
          aitUpdate:          DoMessage(Format(cmTesting,  [Item.FExternalFileName]));
          aitDecode:          DoMessage(Format(cmDecoding, [Item.FExternalFileName]));
          aitDecodeAndUpdate: DoMessage(Format(cmTesting,  [Item.FExternalFileName]));
        end;

        FDecoder.CompressionMethod := Item.CompressionLevel;
        FDecoder.DictionaryLevel   := Item.DictionaryLevel;
        FDecoder.TableParameters   := Item.CompressionTable;
        FDecoder.Tear              := Item.SolidCompression;

        case Item.FTag of
          aitUpdate:          Test(Item);
          aitDecode:          Test(Item);
          aitDecodeAndUpdate: Test(Item);
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
  Confirm: TBeeArchiveConfirm;
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
      until (Confirm <> arcOk) or (FArchiveCustomItems.FindName(RemaneAs) = -1);

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

          Item.DiskSeek   := FTempWriter.Seek(0, soCurrent);
          Item.DiskNumber := FTempWriter.CurrentImage;
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
  var RenameAs: string; var Confirm: TBeeArchiveConfirm);
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
  var Confirm: TBeeArchiveConfirm);
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
  Confirm: TBeeArchiveConfirm;
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
      OpenSwap;
      if ExitCode < ccError  then
      begin





      end;

             (*
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
             *)



    end else
      DoFailure(cmOpenTempError);
  end;






end;


(*




function TBeeArchiveAdder.CreateSwap: boolean;
var
  I: longint;
  Decoder: THeaderDecoder;
begin
  Result      := FALSE;
  FSwapName   := GenerateFileName(FWorkDirectory);
  FSwapWriter := CreateTFileWriter(FSwapName, fmCreate);
  if Assigned(FSwapWriter) then
  begin
  // trova le sequenze
  // e sposta sul file di swap i dati da ricodificare





    Result := FSwapWriter.Size <> 0;
    FSwapWriter.Destroy;
  end;
end;

procedure TBeeArchiveAdder.ConfigureCrypters;
begin
  // nothing to do
end;

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

procedure TBeeArchiveViewer.SaveArchive;
var
  I: longint;
begin
  if FArchiveBusy then Exit;
  if FNeededToSave then
  begin
    FArchiveBusy := TRUE;
    ConfigureCoders;

    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := CreateTFileWriter(FWorkDirectory, fmCreate);
    // ...
    FBeeEncoder := THeaderEncoder.Create(FTempWriter);
    FBeeDecoder := THeaderDecoder.Create(FArchiveReader);


    FBeeEncoder.Destroy;
    FBeeDecoder.Destroy;
    // ...
    FTempWriter.Destroy;

    FArchiveBusy := FALSE;
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

procedure TBeeArchiveViewer.Delete(Index: longint);
begin
  if FArchiveBusy then Exit;

  FArchiveBusy := TRUE;
  with FArchiveCustomItems.Items[Index] do
    begin
      FTag := aitDelete;
      FNeededToSave := TRUE;
    end;
  FArchiveBusy := FALSE;
end;



procedure TBeeArchiveViewer.SetCompressionLevel(Value: longint);
begin
  if Value in [1..3] then FCompressionLevel := Value;
end;

procedure TBeeArchiveViewer.SetDictionaryLevel(Value: longint);
begin
  if Value in [0..9] then FDictionaryLevel := Value;
end;

procedure TBeeArchiveViewer.SetConfigurationName(const Value: string);
begin
  FConfiguration.Clear;
  if FileExists(Value) then
  begin
    FConfigurationName := Value;
    FConfiguration.LoadFromFile(FConfigurationName);
  end;
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







   constructor TBeeArchiveAdder.Create;
begin
  inherited Create;
  FWorkDirectory := '';

  FTempName          := '';
  FTempWriter        := nil;

  FSwapName          := '';
  FSwapReader        := nil;
  FSwapWriter        := nil;

  FCompressionLevel  :=  1;
  FDictionaryLevel   :=  3;
  FConfigurationName := '';
  FConfiguration     := TConfiguration.Create;

  FSolid              := FALSE;
  FForceFileExtension := '.';

  FDefaultCustomItemFlags  := [];
  FDefaultBindingItemFlags := [];
end;



    function THeaderDecoder.ReadToNul(Item: THeader): boolean;
    var
      CRC: longword;
      Strm: TNulWriter;
    begin
      if foPassword in Item.Flags then
      begin
        if FStream is TFileReader then TFileReader(FStream).StartDecode(FPassword);
      end;

      Strm := TNulWriter.Create;
      FStream.Seek(Item.StartPos, soBeginning);
      case foMoved in Item.Flags of
        True:  Result := Copy  (Strm, Item.Size, CRC) = Item.Size;
        False: Result := Decode(Strm, Item.Size, CRC) = Item.Size;
      end;
      Result := Result and (Item.Crc = CRC);
      Strm.Free;

      if FStream is TFileReader then TFileReader(FStream).FinishDecode;
    end;



     *)





end.

