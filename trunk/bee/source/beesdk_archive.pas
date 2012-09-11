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
  beexMARKER  = $1A656542;

  /// beex archive version
  beexVERSION = 1;

  /// archive item type
  aitCustom  = $00;
  aitBinding = $01;
  aitLocator = $7E;
  aitEnd     = $7F;

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

  /// archive coder type
  TBeeArchiveCoderType = (
    actNone,
    actMain,
    actRoloz);

  /// archive main coder flag
  TBeeArchiveMainCoderFlag = (
    amcfMethod,
    amcfDictionary,
    amcfTable,
    amcfTear,
    amcfCompressedSize);

  TBeeArchiveMainCoderFlags = set of TBeeArchiveMainCoderFlag;

  /// archive crypter type
  TBeeArchiveCrypterType = (
    acrtNone,
    acrtMain);

type
  TBeeArchiveCoder = class(TObject)
  protected {private}
    FCompressedSize: qword;
    procedure SetCompressedSize(const Value: qword); virtual;
  public {methods}
  public {properties}
    property CompressedSize: qword read FCompressedSize write SetCompressedSize;
  end;

  TBeeArchiveMainCoder = class(TBeeArchiveCoder)
  protected {private}
    FFlags: TBeeArchiveMainCoderFlags;
    FMethod: longword;
    FDictionary: longword;
    FTable: TTableParameters;
  protected {property methods}
    procedure SetMethod(Value: longword);
    procedure SetDictionary(Value: longword);
    procedure SetTable(const Value: TTableParameters);
    function GetTear: boolean;
    procedure SetTear(Value: boolean);
    procedure SetCompressedSize(const Value: qword); override;
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
  end;

  TBeeArchiveRolozCoder = class(TBeeArchiveCoder)
  protected {private}
  public {methods}
    constructor Create;
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    destructor Destroy; override;
  public {properties}
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
    aitView,
    aitRename,

    aitAdd,
    aitUpdate,

    aitExtract,
    aitDelete,
    aitDecode,
    aitDecodeAndEncode);

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
    FCoder: TBeeArchiveCoder;
    FCrypter: TObject;
    FDiskNumber: longword;
    FDiskSeek: qword;

    FExternalFileName: string;
    FExternalAttributes: longword;
    FExternalUncompressedSize: qword;
    FExternalStream: TStream;
    FExternalDiskNumber: longint;
    FExternalDiskSeek: int64;


    FUserID: longword;
    FGroupID: longword;
    FUserName: string;
    FGroupName: string;
    FComment: string;
    FFileName: string;
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

    property ExternalFileName: string read FExternalFileName;
    property ExternalAttributes: longword read FExternalAttributes;
    property ExternalUncompressedSize: qword read FExternalUncompressedSize;

    property UserID: longword read FUserID write SetUserID;
    property UserName: string read FUserName write SetUserName;
    property GroupID: longword read FGroupID write SetGroupID;
    property GroupName: string read FGroupName write SetGroupName;
    property Comment: string read FComment write SetComment;
    property FileName: string read FFileName;
    property Position: longint read FPosition;

    property Coder: TBeeArchiveCoder read FCoder;
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
    function GetCount : longint;
    function GetItem(Index: longint): TBeeArchiveCustomItem;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item : TBeeArchiveCustomItem);
    function Find(const FileName: string): longint;
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
    var NewFileName: string; var Confirm: TBeeArchiveConfirm) of object;

  TBeeArchiveEraseEvent = procedure(Item: TBeeArchiveCustomItem;
    var Confirm: TBeeArchiveConfirm) of object;


  // a class for each command

  TBeeArchiveViewer = class(TObject)
  protected
    FOwnerTag: TBeeArchiveItemTag;
  private {private}
    FSuspended:  boolean;
    FTerminated: boolean;
    FExitCode: byte;

    FCurrentDirectory: string;
    FArchiveName: string;
    FArchiveReader: TFileReader;
    FArchiveCustomItems: TBeeArchiveCustomItems;
    FArchiveBindingItem: TBeeArchiveBindingItem;
    FArchiveLocatorItem: TBeeArchiveLocatorItem;
    function Read(aStream: TFileReader): boolean;


    procedure UnPack;
  private {event variables}
    FOnFailure: TBeeArchiveFailureEvent;
    FOnMessage: TBeeArchiveMessageEvent;

  private {event method}
    procedure DoFailure(const ErrorMessage: string; ErrorCode: longint);
    procedure DoMessage(const Message: string);


  private { methods}
    function GetItem(Index: longint): TBeeArchiveCustomItem;
    procedure SetArchiveName(const Value: string);
    procedure SetCurrentDirectory(const Value: string);

    procedure SetSuspended(Value: boolean);
    procedure SetTerminated(Value: boolean);
    procedure SetExitCode(Value: byte);
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure OpenArchive(const aArchiveName: string);
    procedure CloseArchive; virtual;
    function Find(const aFileName: string): longint;

    procedure TagAll; virtual;
    procedure Tag(const FileMask: string; Recursive: TRecursiveMode); virtual;
    procedure UnTagAll; virtual;
    procedure UnTag(const FileMask: string; Recursive: TRecursiveMode); virtual;

    procedure Terminate;
  public {properties}
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property Items[Index: longint]: TBeeArchiveCustomItem read GetItem;
    property CurrentDirectory: string
       read FCurrentDirectory write SetCurrentDirectory;

    property Suspended: boolean read FSuspended write SetSuspended;
    property Terminated: boolean read FTerminated;
    property ExitCode: byte read FExitCode;
  end;

  TBeeArchiveExtractor = class(TBeeArchiveViewer)
  private
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseArchive; override;
  public
  end;

  TBeeArchiveTester = class(TBeeArchiveExtractor)
  private
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseArchive; override;
  public
  end;

  TBeeArchiverBuilder = class(TBeeArchiveViewer)
  protected
    FTotalSize: int64;
    FProcessedSize: int64;
  private
    FThreshold: int64;
    FWorkDirectory: string;
    FOnProgress: TBeeArchiveProgressEvent;
  private
    procedure Write(aStream: TFileWriter);

    procedure SetWorkDirectory(const Value: string);
    function DoProgress(Value: longint): boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Pack;


  public
    property Threshold: int64 read FThreshold write FThreshold;
    property WorkDirectory: string read FWorkDirectory write SetWorkDirectory;


  end;

  TBeeArchiveRenamer = class(TBeeArchiverBuilder)
  private
    FOnRename: TBeeArchiveRenameEvent;
  private
    function IsNeededToSave: boolean;
    procedure DoRename(Item: TBeeArchiveCustomItem;
      var NewFileName: string; var Confirm: TBeeArchiveConfirm);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveArchive;
    procedure CloseArchive; override;
  public
    property OnRenameEvent: TBeeArchiveRenameEvent
      read FOnRename write FOnRename;
  end;

  TBeeArchiveEraser = class(TBeeArchiverBuilder)
  private
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveArchive;
    procedure CloseArchive; override;
  public
  end;

  TBeeArchiveAdder = class(TBeeArchiverBuilder)
  private
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveArchive;
    procedure CloseArchive; override;
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

procedure TBeeArchiveCoder.SetCompressedSize(const Value: qword);
begin
  FCompressedSize := Value;
end;

// TBeeArchiveMainCoder class

constructor TBeeArchiveMainCoder.Create;
begin
  inherited Create;
  FFlags := [];
  //FFlags          := [amcfMethod, amcfDictionary, {amcfTable,}
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
    inherited SetCompressedSize(Value);
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

constructor TBeeArchiveRolozCoder.Create;
begin
  inherited Create;
end;

constructor TBeeArchiveRolozCoder.Read(Stream: TFileReader);
begin
  inherited Create;
end;

procedure TBeeArchiveRolozCoder.Write(Stream: TFileWriter);
begin
  // nothing to do
end;

destructor TBeeArchiveRolozCoder.Destroy;
begin
  inherited Destroy;
end;

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

  FExternalFileName         := SearchRec.Name;
  FExternalAttributes       := SearchRec.Attributes;
  FExternalUncompressedSize := SearchRec.Size;
  FExternalStream           := nil;
  FExternalDiskSeek         := 0;
  FExternalDiskNumber       := 0;

  if (acifUserID in FFLags) then
    FUserID := SearchRec.UserID;

  if (acifUserName in FFLags) then
    FUserName := SearchRec.UserName;

  if (acifGroupID in FFLags) then
    FGroupID := SearchRec.GroupID;

  if (acifGroupName in FFLags) then
    FGroupName := SearchRec.GroupName;
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
    case TBeeArchiveCoderType(longword(Stream.ReadInfWord)) of
      // actNone: nothing to do
      actMain:  FCoder := TBeeArchiveMainCoder.Read(Stream);
      actRoloz: FCoder := TBeeArchiveRolozCoder.Read(Stream);
    end;

  FCrypter := nil;
  if (acifCrypter in FFlags) then
    case TBeeArchiveCrypterType(longword(Stream.ReadInfWord)) of
      // acrtNone: nothing to do
      acrtMain: FCrypter := TBeeArchiveMainCrypter.Read(Stream);
    end;

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
    if FCoder.ClassType = TBeeArchiveMainCoder then
      Stream.WriteInfWord(Ord(actMain))
    else
      if FCoder.ClassType = TBeeArchiveRolozCoder then
        Stream.WriteInfWord(Ord(actRoloz));
  end;

  if (acifCrypter in FFlags) then
  begin
    if FCrypter.ClassType = TBeeArchiveMainCrypter then
      Stream.WriteInfWord(Ord(acrtMain));
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

function TBeeArchiveCustomItems.Find(const FileName: string): longint;
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
    Result := TBeeArchiveCustomItem(FNames[Med]).Position
  else
    Result := -1;
end;

function TBeeArchiveCustomItems.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TBeeArchiveCustomItems.GetItem(Index: longint): TBeeArchiveCustomItem;
begin
  Result := TBeeArchiveCustomItem(FItems[Index]);
end;

// TBeeArchiveViewer class

constructor TBeeArchiveViewer.Create;
begin
  inherited Create;
  FOwnerTag := aitView;
  FExitCode := ccSuccesful;

  FArchiveName   := '';
  FArchiveReader := nil;
  FArchiveCustomItems := TBeeArchiveCustomItems.Create;
  FArchiveBindingItem := TBeeArchiveBindingItem.Create;
  FArchiveLocatorItem := TBeeArchiveLocatorItem.Create;
end;

destructor TBeeArchiveViewer.Destroy;
begin
  FArchiveCustomItems.Destroy;
  FArchiveBindingItem.Destroy;
  FArchiveLocatorItem.Destroy;
  inherited Destroy;
end;

procedure TBeeArchiveViewer.Terminate;
begin
  SetExitCode(ccUserAbort);
end;

function TBeeArchiveViewer.GetItem(Index: longint): TBeeArchiveCustomItem;
begin
  Result := FArchiveCustomItems.Items[Index];
end;

procedure TBeeArchiveViewer.SetTerminated(Value: boolean);
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

procedure TBeeArchiveViewer.SetSuspended(Value: boolean);
begin
  if FTerminated = False then
  begin
    FSuspended := Value;
  end;
end;

procedure TBeeArchiveViewer.SetExitCode(Value: byte);
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

procedure TBeeArchiveViewer.SetCurrentDirectory(const Value: string);
begin
  FCurrentDirectory := Value;
end;

procedure TBeeArchiveViewer.SetArchiveName(const Value: string);
begin
  OpenArchive(Value);
end;

function TBeeArchiveViewer.Read(aStream: TFileReader): boolean;
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

procedure TBeeArchiveViewer.UnPack;
begin

end;

procedure TBeeArchiveViewer.OpenArchive(const aArchiveName: string);
var
  MagicSeek: int64;
begin
  CloseArchive;
  if FileExists(aArchiveName) then
  begin
    FArchiveReader := TFileReader.Create(aArchiveName, 1);
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
            DoFailure(Format(cmArcTypeError, [aArchiveName]), ccError);
        end else
          DoFailure(Format(cmArcTypeError, [aArchiveName]), ccError);
      end else
        DoFailure(Format(cmArcTypeError, [aArchiveName]), ccError);
    end else
      DoFailure(Format(cmOpenArcError, [aArchiveName]), ccError);
  end;
end;

procedure TBeeArchiveViewer.CloseArchive;
begin
  FArchiveName := '';
  if FArchiveReader <> nil then
    FreeAndNil(FArchiveReader);

  FArchiveCustomItems.Clear;
  FArchiveBindingItem.Clear;
  FArchiveLocatorItem.Clear;
end;

function TBeeArchiveViewer.Find(const aFileName: string): longint;
begin
  Result := FArchiveCustomItems.Find(aFileName);
end;

procedure TBeeArchiveViewer.TagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    FArchiveCustomItems.Items[I].FTag := FOwnerTag;
  end;
end;

procedure TBeeArchiveViewer.Tag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
  Item: TBeeArchiveCustomItem;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    Item := FArchiveCustomItems.Items[I];
    if FileNameMatch(Item.FileName, FileMask, Recursive) then
    begin
      Item.FTag := FOwnerTag;
    end;
  end;
end;

procedure TBeeArchiveViewer.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    FArchiveCustomItems.Items[I].FTag := aitNone;
  end;
end;

procedure TBeeArchiveViewer.UnTag(const FileMask: string; Recursive: TRecursiveMode);
var
  I: longint;
  Item: TBeeArchiveCustomItem;
begin
  for I := 0 to FArchiveCustomItems.Count - 1 do
  begin
    Item := FArchiveCustomItems.Items[I];
    if FileNameMatch(Item.FileName, FileMask, Recursive) then
    begin
      Item.FTag := aitNone;
    end;
  end;
end;

procedure TBeeArchiveViewer.DoFailure(const ErrorMessage: string; ErrorCode: longint);
begin
  if Assigned(FOnFailure) then
  begin
    FOnFailure(ErrorMessage, ErrorCode);
  end;
end;

procedure TBeeArchiveViewer.DoMessage(const Message: string);
begin
  if Assigned(FOnMessage) then
  begin
    FOnMessage(Message);
  end;
end;

// TBeeArchiveBuilder class

constructor TBeeArchiverBuilder.Create;
begin
  inherited Create;
  FOwnerTag      := aitNone;
  FThreshold     :=  0;
  FWorkDirectory := '';
end;

destructor TBeeArchiverBuilder.Destroy;
begin
  inherited Destroy;
end;

procedure TBeeArchiverBuilder.SetWorkDirectory(const Value: string);
begin
  if (Value = '') or DirectoryExists(Value) then
  begin
    FWorkDirectory := IncludeTrailingBackSlash(Value);
  end;
end;

procedure TBeeArchiverBuilder.Write(aStream: TFileWriter);
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

procedure TBeeArchiverBuilder.Pack;
begin

end;

function TBeeArchiverBuilder.DoProgress(Value: longint): boolean;
begin
  while FSuspended do Sleep(250);

  Inc(FProcessedSize, Value);
  if Assigned(FOnProgress) then
  begin
    DoProgress(Round(FProcessedSize/FTotalSize * 100));
  end;
  Result := ExitCode < ccError;
end;

// TBeeArchiveRenamer class

constructor TBeeArchiveRenamer.Create;
begin
  inherited Create;
  FOwnerTag := aitRename;
end;

destructor TBeeArchiveRenamer.Destroy;
begin
  inherited Destroy;
end;

function TBeeArchiveRenamer.IsNeededToSave: boolean;
var
  I: longint;
  Item: TBeeArchiveCustomItem;
  Confirm: TBeeArchiveConfirm;
  NewFileName: string;
begin
  Result         := FALSE;
  FTotalSize     := 0;
  FProcessedSize := 0;
  for I := 0 to FArchiveCustomItems.Count -1 do
  begin
    Item := FArchiveCustomItems.Items[I];
    Item.FExternalStream := FArchiveReader;

    Inc(FTotalSize, Item.CompressedSize);
    if Item.FTag = aitRename then
    begin
      repeat
        DoRename(Item, NewFileName, Confirm);
      until (Confirm <> arcOk) or (FArchiveCustomItems.Find(NewFileName) = -1);

      case Confirm of
        arcOk:
        begin
          Result := TRUE;
          Item.FFileName := NewFileName;
        end;
        arcCancel: Item.FTag:= aitNone;
        arcAbort:
        begin
          Result := FALSE;
          Break;
        end;
      end;

    end; // if end
  end; // for end
end;

procedure TBeeArchiveRenamer.SaveArchive;
var
  I: longint;
  TempName: string;
  TempWriter: TFileWriter;
  Encoder: THeaderEncoder;
  Item: TBeeArchiveCustomItem;
begin
  if IsNeededToSave then
  begin
    TempName   := GenerateFileName(FWorkDirectory);
    TempWriter := TFileWriter.Create(TempName, FThreshold);
    if Assigned(TempWriter) then
    begin
      Encoder := THeaderEncoder.Create(TempWriter);
      Encoder.OnProgress := @DoProgress;

      for I := 0 to FArchiveCustomItems.Count - 1 do
        if ExitCode < ccError  then
        begin
          Item := FArchiveCustomItems.Items[I];
          FArchiveReader.SeekImage(Item.DiskNumber, Item.DiskSeek);

          Item.DiskSeek   := TempWriter.Seek(0, soCurrent);
          Item.DiskNumber := TempWriter.CurrentImage;
          case Item.FTag of
            aitNone:   DoMessage(Format(cmCopying,  [Item.FileName]));
            aitRename: DoMessage(Format(cmRenaming, [Item.FileName]));
          end;
          Encoder.Copy(FArchiveReader, Item.CompressedSize);

          if FArchiveReader.IsValidStream = False then SetExitCode(ccError);
          if TempWriter    .IsValidStream = False then SetExitCode(ccError);
        end;

      Encoder.Destroy;
      FreeandNil(TempWriter);
    end;
  end;
  CloseArchive;
end;

procedure TBeeArchiveRenamer.CloseArchive;
begin
  inherited CloseArchive;
  // rename ...
end;

procedure TBeeArchiveRenamer.DoRename(Item: TBeeArchiveCustomItem;
  var NewFileName: string; var Confirm: TBeeArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnRename) then
  begin
    NewFileName := Item.FileName;
    FOnRename(Item, NewFileName, Confirm);
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

  function THeaderDecoder.ReadToSwap(Item: THeader; Stream: TStream): boolean;
    var
      CRC: longword;
    begin
      if foPassword in Item.Flags then
      begin
        if FStream is TFileReader then TFileReader(FStream).StartDecode(FPassword);
        if  Stream is TFileWriter then TFileWriter( Stream).StartEncode(FPassword);
      end;

      FStream.Seek(Item.StartPos, soBeginning);
      Item.StartPos := Stream.Seek(0, soCurrent);
      case foMoved in Item.Flags of
        True:  Result := Copy  (Stream, Item.Size, CRC) = Item.Size;
        False: Result := Decode(Stream, Item.Size, CRC) = Item.Size;
      end;
      if Result then Result := Item.Crc = CRC;

      if FStream is TFileReader then TFileReader(FStream).FinishDecode;
      if  Stream is TFileWriter then TFileWriter( Stream).FinishEncode;
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

    function THeaderDecoder.ReadToFile(Item: THeader): boolean;
    var
      CRC: longword;
      Strm: TFileWriter;
    begin
      Result := False;
      Strm   := CreateTFileWriter(Item.ExtName, fmCreate);
      if Assigned(Strm) then
      begin
        if foPassword in Item.Flags then
           if FStream is TFileReader then
             TFileReader(FStream).StartDecode(FPassword);

        FStream.Seek(Item.StartPos, soBeginning);
        case foMoved in Item.Flags of
          True:  Result := Copy  (Strm, Item.Size, CRC) = Item.Size;
          False: Result := Decode(strm, Item.Size, CRC) = Item.Size;
        end;
        Strm.Free;

        if FStream is TFileReader then TFileReader(FStream).FinishDecode;

        Result := Result and (Item.Crc = CRC);
        if Result then
        begin
          FileSetAttr(Item.ExtName, Item.Attr);
          FileSetDate(Item.ExtName, Item.Time);
        end;
      end;
    end;

     *)





end.

