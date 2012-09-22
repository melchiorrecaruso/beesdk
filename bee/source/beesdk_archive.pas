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
  aitCustom    = $00;
  aitBinding   = $01;
  aitLocator   = $7E;
  aitEnd       = $7F;

  /// archive coder type
  actNone      = $00;
  actMain      = $01;

  /// archive crypter type
  acrtNone     = $00;
  acrtMain     = $01;

type
  /// archive locator item flag
  TBeeArchiveLocatorItemFlag = (
    alifVersionNeededToRead,
    alifDisksNumber,
    alifDiskNumber);

  TBeeArchiveLocatorItemFlags = set of TBeeArchiveLocatorItemFlag;

  /// archive binding item flag
  TBeeArchiveBindingItemFlag = (
    abifVersionNeededToRead,
    abifID,
    abifCRC,
    abifSfxSize,
    abifComment);

  TBeeArchiveBindingItemFlags = set of TBeeArchiveBindingItemFlag;

  /// archive custom item flag
  TBeeArchiveCustomItemFlag = (
    acifDefaultFlags,
    acifVersionNeededToExtract,
    acifUncompressedSize,
    acifCreationTime,
    acifLastModifiedTime,
    acifLastAccessTime,
    acifAttributes,
    acifMode,
    acifCRC,
    acifDiskNumber,
    acifDiskSeek,
    acifCoder,
    acifCrypter,
    acifUserID,
    acifUserName,
    acifGroupID,
    acifGroupName,
    acifComment);

  TBeeArchiveCustomItemFlags = set of TBeeArchiveCustomItemFlag;

  /// archive main coder flag
  TBeeArchiveMainCoderFlag = (
    amcfMethod,
    amcfDictionary,
    amcfTable,
    amcfTear,
    amcfCompressedSize);

  TBeeArchiveMainCoderFlags = set of TBeeArchiveMainCoderFlag;

type
  TBeeArchiveMainCoder = class
  protected {private}
    FFlags: TBeeArchiveMainCoderFlags;
    FMethod: longword;
    FDictionary: longword;
    FTable: TTableParameters;
    FCompressedSize: qword;
  protected {property methods}
    procedure SetMethod(Value: longword);
    procedure SetDictionary(Value: longword);
    procedure SetTable(const Value: TTableParameters);
    function GetTear: boolean;
    procedure SetTear(Value: boolean);
    procedure SetCompressedSize(const Value: qword);
  public {methods}
    constructor Create;
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    destructor Destroy; override;
  public {properties}
    property Flags: TBeeArchiveMainCoderFlags read FFlags;
    property Method: longword read FMethod write SetMethod;
    property Dictionary: longword read FDictionary write SetDictionary;
    property Table: TTableParameters read FTable write SetTable;
    property Tear: boolean read GetTear write SetTear;
    property CompressedSize: qword read FCompressedSize write SetCompressedSize;
  end;

  TBeeArchiveMainCrypter = class(TObject)
  protected {private}
  public {methods}
    constructor Create;
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    destructor Destroy; override;
  public {properties}
  end;

  TBeeArchiveItemTag = (
    aitNone,
    aitAdd,
    aitUpdate,
    aitDecode,
    aitDecodeAndUpdate);

  TBeeArchiveCustomItem = class(TObject)
  protected {private}
    FTag: TBeeArchiveItemTag;
    FFlags: TBeeArchiveCustomItemFlags;
    FDefaultFlags: TBeeArchiveCustomItemFlags;
    FVersionNeededToExtract: longword;
    FUncompressedSize: qword;
    FCreationTime: longword;
    FLastModifiedTime: longword;
    FLastAccessTime: longword;
    FAttributes: longword;
    FMode: longword;
    FCRC: longword;
    FCoder: TBeeArchiveMainCoder;
    FCrypter: TBeeArchiveMainCrypter;
    FDiskNumber: longword;
    FDiskSeek: qword;
    FUserID: longword;
    FGroupID: longword;
    FUserName: string;
    FGroupName: string;
    FComment: string;
    FFileName: string;

    FExternalFileName: string;
    FPosition: longint;
  protected {property methods}
    function GetCompressedSize: int64;
    procedure SetUncompressedSize(const Value: qword);
    procedure SetCRC(Value: longword);

    procedure SetDiskNumber(Value: longword);
    procedure SetDiskSeek(const Value: qword);

    procedure SetUserID(Value: longword);
    procedure SetUserName(const Value: string);
    procedure SetGroupID(Value: longword);
    procedure SetGroupName(const Value: string);
    procedure SetComment(const Value: string);
    procedure SetFileName(const Value: string);
  public {methods}
    constructor Create(SearchRec: TCustomSearchRec; const UseFileName: string;
      UseDefaultFlags: TBeeArchiveCustomItemFlags);

    procedure Update(SearchRec: TCustomSearchRec);
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    destructor Destroy; override;
  public {property}
    property Flags: TBeeArchiveCustomItemFlags read FFlags;
    property DefaultFlags: TBeeArchiveCustomItemFlags read FDefaultFlags write FDefaultFlags;
    property VersionNeededToExtract: longword read FVersionNeededToExtract;
    property UncompressedSize: qword read FUncompressedSize write SetUncompressedSize;
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

    property Coder: TBeeArchiveMainCoder read FCoder;
    property CompressedSize: int64 read GetCompressedSize;
  end;

  TBeeArchiveBindingItem = class(TObject)
  private
    FFlags: TBeeArchiveBindingItemFlags;
    FVersionNeededToRead: longword;
    FID: string;
    FCRC: longword;
    FSfxSize: longint;
    FComment: string;
    procedure SetVersionNeededToRead(Value: longword);
    procedure SetID(const Value: string);
    procedure SetCRC(Value: longword);
    procedure SetSfxSize(Value: longint);
    procedure SetComment(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    procedure Clear;
  public
    property Flags: TBeeArchiveBindingItemFlags read FFlags;
    property VersionNeedToRead: longword
       read FVersionNeededToRead write SetVersionNeededToRead;

    property ID: string read FID write SetID;
    property CRC: longword read FCRC write SetCRC;
    property SfxSize: longint read FSfxSize write SetSfxSize;
    property Comment: string read FComment write SetComment;
  end;

  TBeeArchiveLocatorItem = class(TObject)
  private
    FFlags: TBeeArchiveLocatorItemFlags;
    FVersionNeededToRead: longword;
    FDisksNumber: longword;
    FDiskNumber: longword;
    FDiskSeek: int64;
    procedure SetVersionNeededToRead(Value: longword);
    procedure SetDisksNumber(Value: longword);
    procedure SetDiskNumber(Value: longword);
    procedure SetDiskSeek(const Value: int64);
  public
    constructor Create;
    destructor Destroy; override;
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    procedure Clear;
  public
    property Flags: TBeeArchiveLocatorItemFlags read FFlags;
    property VersionNeededToRead: longword
       read FVersionNeededToRead write SetVersionNeededToRead;
    property DisksNumber: longword read FDisksNumber write SetDisksNumber;
    property DiskNumber: longword read FDiskNumber write SetDiskNumber;
    property DiskSeek: int64 read FDiskSeek write SetDiskSeek;
  end;

  TBeeArchiveCustomItems = class(TObject)
  private {private}
    FItems: TList;
    FNames: TList;
  private { methods}
    function FindName(const FileName: string): longint;
    function GetCount : longint;
    function GetItem(Index: longint): TBeeArchiveCustomItem;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item : TBeeArchiveCustomItem);
    function Find(const FileName: string): longint;
    procedure Delete(Index: longint);
    procedure Clear;
  public {properties}
    property Count: longint read GetCount;
    property Items[Index: longint]: TBeeArchiveCustomItem read GetItem;
  end;

  TBeeArchiveProgressEvent = procedure(Value: longint) of object;

  TBeeArchiveMessageEvent = procedure(const Message: string) of object;

  TBeeArchiveFailureEvent = procedure(
    const ErrorMessage: string; ErrorCode: longint) of object;

  TBeeArchiveConfirm = (arcOk, arcCancel, arcAbort);

  TBeeArchiveRenameEvent = procedure(Item: TBeeArchiveCustomItem;
    var RenameAs: string; var Confirm: TBeeArchiveConfirm) of object;

  TBeeArchiveExtractEvent = procedure(Item: TBeeArchiveCustomItem;
    var ExtractAs: string; var Confirm: TBeeArchiveConfirm) of object;

  TBeeArchiveEraseEvent = procedure(Item: TBeeArchiveCustomItem;
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
    FArchiveBindingItem: TBeeArchiveBindingItem;
    FArchiveLocatorItem: TBeeArchiveLocatorItem;
    function Read(aStream: TFileReader): boolean;
    procedure UnPack;


    procedure Swap   (Item: TBeeArchiveCustomItem; Stream: TFileWriter);
    procedure Test   (Item: TBeeArchiveCustomItem);
    procedure Extract(Item: TBeeArchiveCustomItem);



    function GetBackTag(Index: longint; aTag: TBeeArchiveItemTag): longint;
    function GetNextTag(Index: longint; aTag: TBeeArchiveItemTag): longint;
    function GetBackTear(Index: longint): longint;
    function GetNextTear(Index: longint): longint;
    function GetCount: longint;
    function GetItem(Index: longint): TBeeArchiveCustomItem;
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
    property Items[Index: longint]: TBeeArchiveCustomItem read GetItem;
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
    procedure DoExtract(Item: TBeeArchiveCustomItem;
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
    procedure DoRename(Item: TBeeArchiveCustomItem;
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
    procedure DoErase(Item: TBeeArchiveCustomItem;
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

// TBeeArchiveCoder class



// TBeeArchiveMainCoder class

constructor TBeeArchiveMainCoder.Create;
begin
  inherited Create;
  FFlags := [];
  // FFlags          := [amcfMethod, amcfDictionary, {amcfTable,}
  //                    amcfTear, amcfCompressedSize];
  //FMethod         := Ord(moFast);
  //FDictionary     := Ord(do5MB);
  // FTable       := EmptyTableParameters;
  // FCompressedSize := qword(-1);
end;

constructor TBeeArchiveMainCoder.Read(Stream: TFileReader);
begin
  inherited Create;
  FFlags := TBeeArchiveMainCoderFlags(longword(Stream.ReadInfWord));

  if (amcfMethod in FFlags) then
    FMethod := Stream.ReadInfWord;

  if (amcfDictionary in Flags) then
    FDictionary := Stream.ReadInfWord;

  if (amcfTable in FFlags) then
    Stream.Read(FTable, SizeOf(TTableParameters));

  if (amcfCompressedSize in Flags) then
    FCompressedSize := Stream.ReadInfWord;
end;

destructor TBeeArchiveMainCoder.Destroy;
begin
  inherited Destroy;
end;

procedure TBeeArchiveMainCoder.SetMethod(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FMethod := Value;
    Include(FFlags, amcfMethod);
  end else
    Exclude(FFlags, amcfMethod);
end;

procedure TBeeArchiveMainCoder.SetDictionary(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FDictionary := Value;
    Include(FFlags, amcfDictionary);
  end else
    Exclude(FFlags, amcfDictionary);
end;

procedure TBeeArchiveMainCoder.SetTable(const Value: TTableParameters);
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
    FTable := Value;
    Include(FFlags, amcfTable);
  end else
    Exclude(FFlags, amcfTable);
end;

function TBeeArchiveMainCoder.GetTear: boolean;
begin
  Result := amcfTear in FFlags;
end;

procedure TBeeArchiveMainCoder.SetTear(Value: boolean);
begin
  if Value then
    Include(FFlags, amcfTear)
  else
    Exclude(FFlags, amcfTear);
end;

procedure TBeeArchiveMainCoder.SetCompressedSize(const Value: qword);
begin
  if Value <> qword(-1) then
  begin
    FCompressedSize := Value; ;
    Include(FFlags, amcfCompressedSize);
  end else
    Exclude(FFlags, amcfCompressedSize);
end;

procedure TBeeArchiveMainCoder.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));

  if (amcfMethod in FFlags) then
    Stream.WriteInfWord(FMethod);

  if (amcfDictionary in Flags) then
    Stream.WriteInfWord(FDictionary);

  if (amcfTable in FFlags) then
    Stream.Write(FTable, SizeOf(TTableParameters));

  if (amcfCompressedSize in Flags) then
    Stream.WriteInfWord(FCompressedSize);
end;

// TBeeArchiveRolozCoder class









// TBeeArchiveMainCrypter class

constructor TBeeArchiveMainCrypter.Create;
begin
  inherited Create;
end;

constructor TBeeArchiveMainCrypter.Read(Stream: TFileReader);
begin
  inherited Create;
end;

destructor TBeeArchiveMainCrypter.Destroy;
begin
  inherited Destroy;
end;

procedure TBeeArchiveMainCrypter.Write(Stream: TFileWriter);
begin
  // nothing to do
end;

// TBeeArchiveCustomItem class

constructor TBeeArchiveCustomItem.Create(SearchRec: TCustomSearchRec;
  const UseFileName: string; UseDefaultFlags: TBeeArchiveCustomItemFlags);
begin
  inherited Create;
  FTag          := aitAdd;
  FDefaultFlags := UseDefaultFlags;
  FFlags        := FDefaultFlags;

  Update(SearchRec);

  // if (acifCRC in FFLags) then
  //   FCRC := longword(-1);

  FCoder   := nil;
  FCrypter := nil;

  // if (acifDiskNumber in FFLags) then
  //   FDiskNumber := longword(-1);

  // if (acifDiskSeek in FFLags) then
  //   FDiskSeek := qword(-1);

  FComment  := '';
  FFileName := UseFileName;
  // FPosition := -1;
end;

procedure TBeeArchiveCustomItem.Update(SearchRec: TCustomSearchRec);
begin
  if (acifVersionNeededToExtract in FFlags) then
    FVersionNeededToExtract := beexVERSION;

  if (acifUncompressedSize in FFLags) then
    FUncompressedSize := SearchRec.Size;

  if (acifCreationTime in FFLags) then
    FCreationTime := SearchRec.CreationTime;

  if (acifLastModifiedTime in FFLags) then
    FLastModifiedTime := SearchRec.LastModifiedTime;

  if (acifLastAccessTime in FFLags) then
    FLastAccessTime := SearchRec.LastAccessTime;

  if (acifAttributes in FFLags) then
    FAttributes := SearchRec.Attributes;

  if (acifMode in FFLags) then
    FMode := SearchRec.Mode;

  if (acifUserID in FFLags) then
    FUserID := SearchRec.UserID;

  if (acifUserName in FFLags) then
    FUserName := SearchRec.UserName;

  if (acifGroupID in FFLags) then
    FGroupID := SearchRec.GroupID;

  if (acifGroupName in FFLags) then
    FGroupName := SearchRec.GroupName;

  FExternalFileName := SearchRec.Name;
end;

constructor TBeeArchiveCustomItem.Read(Stream: TFileReader);
begin
  FFlags := TBeeArchiveCustomItemFlags(longword(Stream.ReadInfWord));

  if (acifDefaultFlags in FFlags) then
    FDefaultFlags := TBeeArchiveCustomItemFlags(longword(Stream.ReadInfWord));

  if (acifVersionNeededToExtract in FFlags) then
    FVersionNeededToExtract := Stream.ReadInfWord;

  if (acifUncompressedSize in FFlags) then
    FUncompressedSize := Stream.ReadInfWord;

  if (acifCreationTime in FFlags) then
    FCreationTime := Stream.ReadInfWord;

  if (acifLastModifiedTime in FFlags) then
    FLastModifiedTime := Stream.ReadInfWord;

  if (acifLastAccessTime in FFlags) then
    FLastAccessTime := Stream.ReadInfword;

  if (acifAttributes in FFlags) then
    FAttributes := Stream.ReadInfWord;

  if (acifMode in FFlags) then
    FMode := Stream.ReadInfWord;

  if (acifCRC in FFlags) then
    FCRC := Stream.ReadInfWord;

  if (acifDiskNumber in FFlags) then
    FDiskNumber := Stream.ReadInfWord;

  if (acifDiskSeek in FFlags) then
    FDiskSeek := Stream.ReadInfWord;

  FCoder := nil;
  if (acifCoder in FFlags) then
    case longword(Stream.ReadInfWord) of
      // actNone: nothing to do
      actMain:  FCoder := TBeeArchiveMainCoder.Read(Stream);
    end;
  if not Assigned(FCoder) then
    FCoder := TBeeArchiveMainCoder.Create;

  FCrypter := nil;
  if (acifCrypter in FFlags) then
    case longword(Stream.ReadInfWord) of
      // acrtNone: nothing to do
      acrtMain: FCrypter := TBeeArchiveMainCrypter.Read(Stream);
    end;
  if not Assigned(FCrypter) then
    FCrypter := TBeeArchiveMainCrypter.Create;

  if (acifUserID in FFlags) then
    FUserID := Stream.ReadInfWord;

  if (acifUserName in FFlags) then
    FUserName := Stream.ReadInfString;

  if (acifGroupID in FFlags) then
    FGroupID := Stream.ReadInfWord;

  if (acifGroupName in FFlags) then
    FGroupName := Stream.ReadInfString;

  if (acifComment in FFlags) then
    FComment :=Stream.ReadInfString;

  FFileName:= Stream.ReadInfString;
end;

destructor TBeeArchiveCustomItem.Destroy;
begin
  if Assigned(FCoder  ) then FreeAndNil(FCoder  );
  if Assigned(FCrypter) then FreeAndNil(FCrypter);
  inherited Destroy;
end;

function TBeeArchiveCustomItem.GetCompressedSize: int64;
begin
  if Assigned(FCoder) then
    Result := FCoder.CompressedSize
  else
    Result := FUnCompressedSize;
end;

procedure TBeeArchiveCustomItem.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TBeeArchiveCustomItem.SetUncompressedSize(const Value: qword);
begin
  if Value <> qword(-1) then
  begin
    FUncompressedSize := Value;
    Include(FFlags, acifUncompressedSize);
  end else
    Exclude(FFlags, acifUncompressedSize);
end;

procedure TBeeArchiveCustomItem.SetDiskNumber(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FDiskNumber := Value;
    Include(FFlags, acifDiskNumber);
  end else
    Exclude(FFlags, acifDiskNumber);
end;

procedure TBeeArchiveCustomItem.SetDiskSeek(const Value: qword);
begin
  if Value <> qword(-1) then
  begin
    FDiskSeek := Value;
    Include(FFlags, acifDiskSeek);
  end else
    Exclude(FFlags, acifDiskSeek);
end;

procedure TBeeArchiveCustomItem.SetUserID(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FUserID := Value;
    Include(FFlags, acifUserID);
  end else
    Exclude(FFlags, acifUserID);
end;

procedure TBeeArchiveCustomItem.SetUserName(const Value: string);
begin
  if Value <> '' then
  begin
    FUserName := Value;
    Include(FFlags, acifUserName);
  end else
    Exclude(FFlags, acifUserName);
end;

procedure TBeeArchiveCustomItem.SetGroupID(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FGroupID := Value;
    Include(FFlags, acifGroupID);
  end else
    Exclude(FFlags, acifGroupID);
end;

procedure TBeeArchiveCustomItem.SetGroupName(const Value: string);
begin
  if Value <> '' then
  begin
    FGroupName := Value;
    Include(FFlags, acifGroupName);
  end else
    Exclude(FFlags, acifGroupName);
end;

procedure TBeeArchiveCustomItem.SetComment(const Value: string);
begin
  if Value <> '' then
  begin
    FComment := Value;
    Include(FFlags, acifComment);
  end else
    Exclude(FFlags, acifComment);
end;

procedure TBeeArchiveCustomItem.SetCRC(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FCRC := Value;
    Include(FFlags, acifCRC);
  end else
    Exclude(FFlags, acifCRC);
end;

procedure TBeeArchiveCustomItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));

  if (acifDefaultFlags in  FFlags) then
    Stream.WriteInfWord(longword(FDefaultFlags));

  if (acifVersionNeededToExtract in  FFlags) then
    Stream.WriteInfWord(FVersionNeededToExtract);

  if (acifUncompressedSize in FFlags) then
    Stream.WriteInfWord(FUncompressedSize);

  if (acifCreationTime in FFlags) then
    Stream.WriteInfWord(FCreationTime);

  if (acifLastModifiedTime in FFlags) then
    Stream.WriteInfWord(FLastModifiedTime);

  if (acifLastAccessTime in FFlags) then
    Stream.WriteInfWord(FLastAccessTime);

  if (acifAttributes in FFlags) then
    Stream.WriteInfWord(FAttributes);

  if (acifMode in FFlags) then
    Stream.WriteInfWord(FMode);

  if (acifCRC in FFlags) then
    Stream.WriteInfWord(FCRC);

  if (acifDiskNumber in FFlags) then
    Stream.WriteInfWord(FDiskNumber);

  if (acifDiskSeek in FFlags) then
    Stream.WriteInfWord(FDiskSeek);

  if (acifCoder in FFlags) then
  begin
    Stream.WriteInfWord(actMain);
    FCoder.Write(Stream);
  end;

  if (acifCrypter in FFlags) then
  begin
    Stream.WriteInfWord(Ord(acrtMain));
    FCrypter.Write(Stream);
  end;

  if (acifUserID in FFlags) then
    Stream.WriteInfWord(FUserID);

  if (acifUserName in FFlags) then
    Stream.WriteInfString(FUserName);

  if (acifGroupID in FFlags) then
    Stream.WriteInfWord(FGroupID);

  if (acifGroupName in FFlags) then
    Stream.WriteInfString(FGroupName);

  if (acifComment in FFlags) then
    Stream.WriteInfString(FComment);

  Stream.WriteInfString(FFileName);
end;

// TBeeArchiveLolatorItem class

constructor TBeeArchiveLocatorItem.Create;
begin
  inherited Create;
  FFlags := [];
end;

constructor TBeeArchiveLocatorItem.Read(Stream: TFileReader);
begin
  inherited Create;
  FFlags := TBeeArchiveLocatorItemFlags(longword(Stream.ReadInfWord));

  if (alifVersionNeededToRead in FFlags) then
    FVersionNeededToRead := Stream.ReadInfWord;

  if (alifDisksNumber in FFlags) then
      FDisksNumber := Stream.ReadInfWord;

  if (alifDiskNumber in FFlags) then
    FDiskNumber := Stream.ReadInfWord;

  FDiskSeek := Stream.ReadInfWord;
end;

destructor TBeeArchiveLocatorItem.Destroy;
begin
  inherited Destroy;
end;

procedure TBeeArchiveLocatorItem.SetVersionNeededToRead(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FVersionNeededToRead := Value;
    Include(FFlags, alifVersionNeededToRead);
  end else
    Exclude(FFlags, alifVersionNeededToRead);
end;

procedure TBeeArchiveLocatorItem.SetDisksNumber(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FDisksNumber := Value;
    Include(FFlags, alifDisksNumber);
  end else
    Exclude(FFlags, alifDisksNumber);
end;

procedure TBeeArchiveLocatorItem.SetDiskNumber(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FDiskNumber := Value;
    Include(FFlags, alifDiskNumber);
  end else
    Exclude(FFlags, alifDiskNumber);
end;

procedure TBeeArchiveLocatorItem.SetDiskSeek(const Value: int64);
begin
  FDiskSeek := Value;
end;

procedure TBeeArchiveLocatorItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));

  if (alifVersionNeededToRead in FFlags) then
    Stream.WriteInfWord(FVersionNeededToRead);

  if (alifDisksNumber in FFlags) then
    Stream.WriteInfWord(FDisksNumber);

  if (alifDiskNumber in FFlags) then
    Stream.WriteInfWord(FDiskNumber);

  Stream.WriteInfWord(FDiskSeek);
end;

procedure TBeeArchiveLocatorItem.Clear;
begin
  FFlags := [];
end;

// TBeeArchiveBindingItem class

constructor TBeeArchiveBindingItem.Create;
begin
  inherited Create;
  FFlags := [];
end;

constructor TBeeArchiveBindingItem.Read(Stream: TFileReader);
begin
  inherited Create;
  FFlags := TBeeArchiveBindingItemFlags(longword(Stream.ReadInfWord));

  if (abifVersionNeededToRead in FFlags) then
    FVersionNeededToRead := Stream.ReadInfWord;

  if (abifID in FFlags) then
    FID := Stream.ReadInfString;

  if (abifCRC in FFlags) then
    FCRC := Stream.ReadInfWord;

  if (abifSfxSize in FFlags) then
    FSfxSize := Stream.ReadInfWord;

  if (abifComment in FFlags) then
    FComment := Stream.ReadInfString;
end;

destructor TBeeArchiveBindingItem.Destroy;
begin
  inherited Destroy;
end;

procedure TBeeArchiveBindingItem.SetVersionNeededToRead(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FVersionNeededToRead := Value;
    Include(FFlags, abifVersionNeededToRead);
  end else
    Exclude(FFlags, abifVersionNeededToRead);
end;

procedure TBeeArchiveBindingItem.SetID(const Value: string);
begin
  if Value <> '' then
  begin
    FID := Value;
    Include(FFlags, abifID);
  end else
    Exclude(FFlags, abifID);
end;

procedure TBeeArchiveBindingItem.SetCRC(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FCRC := Value;
    Include(FFlags, abifCRC);
  end else
    Exclude(FFlags, abifCRC);
end;

procedure TBeeArchiveBindingItem.SetSfxSize(Value: longint);
begin
  if Value <> 0 then
  begin
    FSfxSize := Value;
    Include(FFlags, abifSfxSize);
  end else
    Exclude(FFlags, abifSfxSize);
end;

procedure TBeeArchiveBindingItem.SetComment(const Value: string);
begin
  if Value <> '' then
  begin
    FComment := Value;
    Include(FFlags, abifComment);
  end else
    Exclude(FFlags, abifComment);
end;

procedure TBeeArchiveBindingItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfWord(longword(FFlags));

  if (abifVersionNeededToRead in FFlags) then
    Stream.WriteInfWord(FVersionNeededToRead);

  if (abifID in FFlags) then
    Stream.WriteInfString(FID);

  if (abifCRC in FFlags) then
    Stream.WriteInfWord(FCRC);

  if (abifSfxSize in FFlags) then
    Stream.WriteInfWord(FSfxSize);

  if (abifComment in FFlags) then
    Stream.WriteInfString(FComment);
end;

procedure TBeeArchiveBindingItem.Clear;
begin
  FFlags := [];
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
    TBeeArchiveCustomItem(FItems[I]).Destroy;
  end;
  FItems.Clear;
  FNames.Clear;
end;

procedure TBeeArchiveCustomItems.Add(Item: TBeeArchiveCustomItem);
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
        TBeeArchiveCustomItem(FNames[Med]).FileName);

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
      TBeeArchiveCustomItem(FNames[Med]).FileName);

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
    Result := TBeeArchiveCustomItem(FNames[Result]).Position
  end;
end;

procedure TBeeArchiveCustomItems.Delete(Index: longint);
var
  Item, Next: TBeeArchiveCustomItem;
begin
  Item := Items[Index];
  if Index < FItems.Count - 1 then
  begin
    Next := Items[Index + 1];

    if (Item.Coder.Tear) and (not(Next.Coder.Tear)) then
    begin
      Next.Coder.Tear := TRUE;
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

function TBeeArchiveCustomItems.GetItem(Index: longint): TBeeArchiveCustomItem;
begin
  Result := TBeeArchiveCustomItem(FItems[Index]);
end;

// TBeeArchiveReader class

constructor TBeeArchiveReader.Create;
begin
  inherited Create;
  FExitCode := ccSuccesful;

  FArchiveCustomItems := TBeeArchiveCustomItems.Create;
  FArchiveBindingItem := TBeeArchiveBindingItem.Create;
  FArchiveLocatorItem := TBeeArchiveLocatorItem.Create;
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

function TBeeArchiveReader.GetItem(Index: longint): TBeeArchiveCustomItem;
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
    FArchiveLocatorItem.Read(aStream);
    if alifVersionNeededToRead in FArchiveLocatorItem.FFlags then
      if FArchiveLocatorItem.VersionNeededToRead > beexVERSION then
        Exit;

    if alifDisksNumber in FArchiveLocatorItem.FFlags then
      aStream.ImagesNumber := FArchiveLocatorItem.DisksNumber;
    if alifDiskNumber in FArchiveLocatorItem.FFlags then
      aStream.SeekImage(FArchiveLocatorItem.DiskNumber, FArchiveLocatorItem.DiskSeek);

    if aStream.ReadDWord = beexMARKER then
      repeat
        T := longword(aStream.ReadInfWord);
        case T of
          aitCustom:  FArchiveCustomItems.Add(TBeeArchiveCustomItem.Read(aStream));
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

procedure TBeeArchiveReader.Swap(Item: TBeeArchiveCustomItem; Stream: TFileWriter);
var
  CRC: longword;
begin
  if Assigned(Stream) then
  begin
    FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);

    Item.DiskNumber := Stream.CurrentImage;
    Item.DiskSeek   := Stream.Seek(0, soCurrent);
    case Item.Coder.Method of
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

procedure TBeeArchiveReader.Test(Item: TBeeArchiveCustomItem);
var
  CRC: longword;
  Stream: TFileWriter;
begin
  Stream := TNulWriter.Create;
  if Assigned(Stream) then
  begin
    FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);
    case Item.Coder.Method of
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

procedure TBeeArchiveReader.Extract(Item: TBeeArchiveCustomItem);
var
  CRC: longword;
  Stream: TFileWriter;
begin
  Stream := TFileWriter.Create(Item.FExternalFileName, fmCreate);
  if Assigned(Stream) then
  begin
    FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);
    case Item.Coder.Method of
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

function TBeeArchiveReader.GetBackTag(Index: longint; aTag: TBeeArchiveItemTag): longint;
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

function TBeeArchiveReader.GetNextTag(Index: longint; aTag: TBeeArchiveItemTag): longint;
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
    if FArchiveCustomItems.Items[Index].Coder.Tear then
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
    if FArchiveCustomItems.Items[Index].Coder.Tear then
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
  FArchiveLocatorItem.Clear;

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
    aStream.WriteInfWord(aitCustom);
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
  Item: TBeeArchiveCustomItem;
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

          FDecoder.CompressionMethod := Item.Coder.Method;
          FDecoder.DictionaryLevel   := Item.Coder.Dictionary;
          FDecoder.TableParameters   := Item.Coder.Table;
          FDecoder.Tear              := Item.Coder.Tear;

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
  FArchiveLocatorItem.Clear;

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
  Item: TBeeArchiveCustomItem;
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
  Item: TBeeArchiveCustomItem;
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

procedure TBeeArchiveExtractor.DoExtract(Item: TBeeArchiveCustomItem;
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
  Item: TBeeArchiveCustomItem;
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

          FDecoder.CompressionMethod := Item.Coder.Method;
          FDecoder.DictionaryLevel   := Item.Coder.Dictionary;
          FDecoder.TableParameters   := Item.Coder.Table;
          FDecoder.Tear              := Item.Coder.Tear;

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
  Item: TBeeArchiveCustomItem;
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

        FDecoder.CompressionMethod := Item.Coder.Method;
        FDecoder.DictionaryLevel   := Item.Coder.Dictionary;
        FDecoder.TableParameters   := Item.Coder.Table;
        FDecoder.Tear              := Item.Coder.Tear;

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
  Item: TBeeArchiveCustomItem;
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
  Item: TBeeArchiveCustomItem;
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

procedure TBeeArchiveRenamer.DoRename(Item: TBeeArchiveCustomItem;
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

procedure TBeeArchiveEraser.DoErase(Item: TBeeArchiveCustomItem;
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
  Item: TBeeArchiveCustomItem;
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
  Item: TBeeArchiveCustomItem;
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
  Item: TBeeArchiveCustomItem;
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
  CurrentItem: TBeeArchiveCustomItem;
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
  const UseFileName: string; UseFlags: TBeeArchiveCustomItemFlags);
begin
  if FArchiveBusy then Exit;

  FArchiveBusy := TRUE;
  if Find(UseFileName) = -1 then
  begin
    FArchiveCustomItems.Add(TBeeArchiveCustomItem.Create(
      SearchRec, UseFileName, UseFlags));
    FNeededToSave := TRUE;
  end;
  FArchiveBusy  := FALSE;
end;

procedure TBeeArchiveViewer.Update(SearchRec: TCustomSearchRec;
  const UseFileName: string; UseFlags: TBeeArchiveCustomItemFlags);
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
  const UseFileName: string; UseFlags: TBeeArchiveCustomItemFlags);
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






procedure TBeeArchiveViewer.Encode(Item: TBeeArchiveCustomItem);
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
      FBeeEncoder.CompressionMethod := TBeeArchiveMainCoder(Item.Coder).Method;
      FBeeEncoder.DictionaryLevel   := TBeeArchiveMainCoder(Item.Coder).Dictionary;
      FBeeEncoder.TableParameters   := TBeeArchiveMainCoder(Item.Coder).Table;
      FBeeEncoder.Tear              := TBeeArchiveMainCoder(Item.Coder).Tear;

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

procedure TBeeArchiveViewer.Copy(Item: TBeeArchiveCustomItem);
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

