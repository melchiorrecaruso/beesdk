unit BeeSDK_Archive;

interface

uses
  Classes, SysUtils, Bee_Files;

type
  // Header type

  THeaderType = (
    htBINDING,
    htCUSTOM
  );

  // BINDING Header flag

  TBindingHeaderFlag = (
    bhfVERSION,
    bhfID,
    bhfDISK,
    bhfDISKs,
    bhfCOMMENT,
    bhfCRC
  );

  TBindingHeaderFlags = set of TBindingHeaderFlag;

  // CUSTOM Header flag

  TCustomHeaderFlag = (
    chfVERSION,
    chfNAME,
    chfSIZE,
    chfCTIME,
    chfMTIME,
    chfATIME,
    chfATTRIBUTES,
    chfMODE,
    chfCRC,
    chfCODER,
    chfCRYPTER,
    chfDISK,
    hcfSEEK,
    chfUID,
    chfGID,
    chfUNAME,
    chfGNAME,
    chfCOMMENT
  );

  TCustomHeaderFlags = set of TCustomHeaderFlag;

  /// Coder type

  TCoderType = (
    chtBEE,
    chtROLOZ,
    chtBLOWFISH
  );

  /// BEE Coder flag

  TBeeCoderFlag = (
    bcfVERSION,
    bcfMETHOD,
    bcfDICTIONARY,
    bcfTABLE,
    bcfTEAR
  );

   TBeeCoderFlags = set of TBeeCoderFlag;

  /// ROLOZ Coder flag

  TRolozCoderFlag = (
    rcfVERSION
  );

  TRolozCoderFlags = set of TRolozCoderFlag;

  /// BLOWFISH Coder type

  TBlowFishCoderFlag = (
    bfcfVERSION
  );

  TBlowFishCoderFlags = set of TBlowFishCoderFlag;

type
  TBeeCoder = class(TObject)
  public
    Flags: TBeeCoderFlags;
    Version: longword;
    Method: longword;
    Dictionary: longword;
    Table: longword;
    StoredSize: qword;
  end;

  TRolozCoder = class(TObject)
  public
    Flags: TRolozCoderFlags;
    Version: longword;
    StoredSize: qword;
  end;

  TBlowFishCoder = class(TObject)
  public
    Flags: TBlowFishCoderFlags;
    Version: longword;
  end;

type
  { Header actions }

  TAction = (haNone, haUpdate, haDecode, haDecodeAndUpdate);

type
  TCustomHeader = class(TObject)
  private
    Action: TAction;
    DiskName: string;
    DiskSize: qword;
  public
    Flags: TCustomHeaderFlags;
    Version: longword;
    NameLen: longword;
    Name: string;
    Size: qword;
    CreationTime: longword;
    ModificationTime: longword;
    AccesTime: longword;
    Attributes: longword;
    Mode: longword;
    CRC: longword;
    Coder: TObject;
    Crypter: TObject;
    Disk: longword;
    Seek: qword;
    UID: longword;
    GID: longword;
    UNameLen: longword;
    UName: string;
    GNameLen: longword;
    GName: string;
    CommentLen: longword;
    Comment: string;
  end;

  TBindingHeader = class(TObject)
  public
    Flags: TBindingHeaderFlags;
    Version: longword;
    ID: qword;
    DISK: longword;
    DISKs: longword;
    CommentLen: longword;
    Comment: string;
    CRC: longword;
    Seek: qword;
  end;

  THeaderList = class
  private
    FCustoms: TList;
    FBinding: TBindingHeader;
  public

  end;


implementation

  function OpenArchive(Archive: TStream): boolean;
  begin
    Result := False;


  end;



  function ReadCustomHeader(Item: TCustomHeader; Stream: TFileReader): boolean;
  var
    qw: qword;
  begin
    Result := False;
    try
      Stream.ReadInfint(qw);  Item.Flags := qw;




    except
      Result := False;
    end;
  end;

  function WriteGenericHeader(Item: TCustomHeader; Stream: TFileWriter): boolean;
  begin





  end;

  function ReadBindingHeader(Item: TBindingHeader; Stream: TFileReader): boolean;
  begin
  end;

  function WriteBindingHeader(Item: TBindingHeader; Stream: TFileWriter): boolean;
  begin
  end;







(*

{ ===== TAbArchiveList ====================================================== }

  TAbArchiveList = class
  protected {private}
    FList     : TList;
    FOwnsItems: Boolean;
    HashTable : array[0..1020] of TAbArchiveItem;
  protected {methods}
    function GenerateHash(const S : string) : LongInt;
    function GetCount : Integer;
    function Get(Index : Integer) : TAbArchiveItem;
    procedure Put(Index : Integer; Item : TAbArchiveItem);
  public {methods}
    constructor Create(AOwnsItems: Boolean);
    destructor Destroy; override;
    function Add(Item : Pointer): Integer;
    procedure Clear;
    procedure Delete(Index : Integer);
    function Find(const FN : string) : Integer;
    function GetEnumerator: TAbArchiveListEnumerator;
    function IsActiveDupe(const FN : string) : Boolean;
  public {properties}
    property Count : Integer
      read GetCount;
    property Items[Index : Integer] : TAbArchiveItem
      read Get
      write Put; default;
  end;


{ ===== TAbArchive specific types =========================================== }
type
  TAbStoreOption =
    (soStripDrive, soStripPath, soRemoveDots, soRecurse, soFreshen, soReplace);
  TAbStoreOptions =
    set of TAbStoreOption;

  TAbExtractOption =
    (eoCreateDirs, eoRestorePath);
  TAbExtractOptions =
    set of TAbExtractOption;

  TAbArchiveStatus =
    (asInvalid, asIdle, asBusy);

  TAbArchiveEvent =
    procedure(Sender : TObject) of object;
  TAbArchiveConfirmEvent =
    procedure (Sender : TObject; var Confirm : Boolean) of object;
  TAbArchiveProgressEvent =
    procedure(Sender : TObject; Progress : Byte; var Abort : Boolean) of object;
  TAbArchiveItemEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem) of object;
  TAbArchiveItemConfirmEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      ProcessType : TAbProcessType; var Confirm : Boolean) of object;
  TAbConfirmOverwriteEvent =
    procedure(var Name : string; var Confirm : Boolean) of object;
  TAbArchiveItemFailureEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      ProcessType : TAbProcessType; ErrorClass : TAbErrorClass;
      ErrorCode : Integer) of object;
  TAbArchiveItemExtractEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      const NewName : string) of object;
  TAbArchiveItemExtractToStreamEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream) of object;
  TAbArchiveItemTestEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem) of object;
  TAbArchiveItemInsertEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream) of object;
  TAbArchiveItemInsertFromStreamEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      OutStream, InStream : TStream) of object;
  TAbArchiveItemProgressEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem; Progress : Byte;
      var Abort : Boolean) of object;
  TAbProgressEvent =
    procedure(Progress : Byte; var Abort : Boolean) of object;
  TAbRequestDiskEvent =
    procedure(Sender : TObject; var Abort : Boolean) of object;
  TAbRequestImageEvent =
    procedure(Sender : TObject; ImageNumber : Integer;
      var ImageName : string; var Abort : Boolean) of object;
  TAbRequestNthDiskEvent =
    procedure(Sender : TObject; DiskNumber : Byte; var Abort : Boolean) of object;


type
  TAbArchiveStreamHelper = class
  protected
    FStream : TStream;
  public
    constructor Create(AStream : TStream);
    procedure ExtractItemData(AStream : TStream); virtual; abstract;
    function FindFirstItem : Boolean; virtual; abstract;
    function FindNextItem : Boolean; virtual; abstract;
    procedure ReadHeader; virtual; abstract;
    procedure ReadTail; virtual; abstract;
    function SeekItem(Index : Integer): Boolean; virtual; abstract;
    procedure WriteArchiveHeader; virtual; abstract;
    procedure WriteArchiveItem(AStream : TStream); virtual; abstract;
    procedure WriteArchiveTail; virtual; abstract;
    function GetItemCount : Integer; virtual; abstract;
  end;

type

  TBeeArchive = class (TObject)
   public
     FStream         : TStream;
     FStatus         : TAbArchiveStatus;
   protected
     FArchiveName    : string;
     FAutoSave       : Boolean;
     FBaseDirectory  : string;
     //FCurrentItem    : TAbArchiveItem;
     //FDOSMode        : Boolean;
     FExtractOptions : TAbExtractOptions;
     //FImageNumber    : Word;
     //FInStream       : TStream;
     //FIsDirty        : Boolean;
     //FSpanningThreshold      : Int64;
     FItemList       : TAbArchiveList;
     FLogFile        : string;
     FLogging        : Boolean;
     //FLogStream      : TFileStream;
     //FMode           : Word;
     //FOwnsStream     : Boolean;
     //FSpanned        : Boolean;
     FStoreOptions   : TAbStoreOptions;
     FTempDir        : string;

   protected
     FOnProcessItemFailure  : TAbArchiveItemFailureEvent;
     FOnArchiveProgress     : TAbArchiveProgressEvent;
     FOnArchiveSaveProgress : TAbArchiveProgressEvent;
     FOnArchiveItemProgress : TAbArchiveItemProgressEvent;
     FOnConfirmProcessItem  : TAbArchiveItemConfirmEvent;
     FOnConfirmOverwrite    : TAbConfirmOverwriteEvent;
     FOnConfirmSave         : TAbArchiveConfirmEvent;
     FOnLoad                : TAbArchiveEvent;
     FOnProgress            : TAbProgressEvent;
     FOnRequestImage        : TAbRequestImageEvent;
     FOnSave                : TAbArchiveEvent;

 protected {methods}
   constructor CreateInit;
   procedure CheckValid;
   function  ConfirmPath(Item : TAbArchiveItem; const NewName : string;
     out UseName : string) : Boolean;
   procedure FreshenAt(Index : Integer);
   function  FreshenRequired(Item : TAbArchiveItem) : Boolean;
   procedure GetFreshenTarget(Item : TAbArchiveItem);
   function  GetItemCount : Integer;
   procedure MakeLogEntry(const FN: string; LT : TAbLogType);
   procedure ReplaceAt(Index : Integer);
   procedure SaveIfNeeded(aItem : TAbArchiveItem);
   procedure SetBaseDirectory(Value : string);
   procedure SetLogFile(const Value : string);
   procedure SetLogging(Value : Boolean);

 protected {abstract methods}
   function CreateItem(const FileSpec : string): TAbArchiveItem;
     virtual; abstract;
   procedure ExtractItemAt(Index : Integer; const UseName : string);
     virtual; abstract;
   procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream);
     virtual; abstract;
   procedure LoadArchive;
     virtual; abstract;
   procedure SaveArchive;
     virtual; abstract;
   procedure TestItemAt(Index : Integer);
     virtual; abstract;

 protected {virtual methods}
   procedure DoProcessItemFailure(Item : TAbArchiveItem;
     ProcessType : TAbProcessType; ErrorClass : TAbErrorClass;
     ErrorCode : Integer);
     virtual;
   procedure DoArchiveSaveProgress(Progress : Byte; var Abort : Boolean);
     virtual;
   procedure DoArchiveProgress(Progress : Byte; var Abort : Boolean);
     virtual;
   procedure DoArchiveItemProgress(Item : TAbArchiveItem; Progress : Byte;
     var Abort : Boolean);
     virtual;
   procedure DoConfirmOverwrite(var FileName : string; var Confirm : Boolean);
     virtual;
   procedure DoConfirmProcessItem(Item : TAbArchiveItem;
     const ProcessType : TAbProcessType; var Confirm : Boolean);
     virtual;
   procedure DoConfirmSave(var Confirm : Boolean);
     virtual;

   procedure DoLoad;
     virtual;
   procedure DoProgress(Progress : Byte; var Abort : Boolean);
     virtual;
   procedure DoSave;
     virtual;
   function FixName(const Value : string) : string;
     virtual;
   function GetSpanningThreshold : Int64;
     virtual;
   function GetSupportsEmptyFolders : Boolean;
     virtual;
   procedure SetSpanningThreshold( Value : Int64 );
     virtual;

 protected {properties and events}
   property InStream : TStream
     read FInStream;

 public {methods}
   constructor Create(const FileName : string; Mode : Word);
     virtual;
   constructor CreateFromStream(aStream : TStream; const aArchiveName : string);
     virtual;
   destructor  Destroy;
     override;
   procedure Add(aItem : TAbArchiveItem);
     virtual;
   procedure AddFiles(const FileMask : string; SearchAttr : Integer);
   procedure AddFilesEx(const FileMask, ExclusionMask : string;
     SearchAttr : Integer);
   procedure AddFromStream(const NewName : string; aStream : TStream);
   procedure ClearTags;
   procedure Delete(aItem : TAbArchiveItem);
   procedure DeleteAt(Index : Integer);
   procedure DeleteFiles(const FileMask : string);
   procedure DeleteFilesEx(const FileMask, ExclusionMask : string);
   procedure DeleteTaggedItems;
   procedure Extract(aItem : TAbArchiveItem; const NewName : string);
   procedure ExtractAt(Index : Integer; const NewName : string);
   procedure ExtractFiles(const FileMask : string);
   procedure ExtractFilesEx(const FileMask, ExclusionMask : string);
   procedure ExtractTaggedItems;
   procedure ExtractToStream(const aFileName : string; aStream : TStream);
   function  FindFile(const aFileName : string): Integer;
   function  FindItem(aItem : TAbArchiveItem): Integer;
   procedure Freshen(aItem : TAbArchiveItem);
   procedure FreshenFiles(const FileMask : string);
   procedure FreshenFilesEx(const FileMask, ExclusionMask : string);
   procedure FreshenTaggedItems;
   procedure Load; virtual;
   procedure Move(aItem : TAbArchiveItem; const NewStoredPath : string);
     virtual;
   procedure Replace(aItem : TAbArchiveItem);
   procedure Save;
     virtual;
   procedure TagItems(const FileMask : string);
   procedure TestTaggedItems;
   procedure UnTagItems(const FileMask : string);


   procedure DoDeflateProgress(aPercentDone : integer);
     virtual;
   procedure DoInflateProgress(aPercentDone : integer);
     virtual;
   procedure DoSpanningMediaRequest(Sender : TObject; ImageNumber : Integer;
     var ImageName : string; var Abort : Boolean); virtual;
 public {properties}
   property OnProgress : TAbProgressEvent
     read FOnProgress write FOnProgress;
   property ArchiveName : string
     read FArchiveName;
   property AutoSave : Boolean
     read FAutoSave
     write FAutoSave;
   property BaseDirectory : string
     read FBaseDirectory
     write SetBaseDirectory;
   property Count : Integer
     read GetItemCount;
   property DOSMode : Boolean
     read FDOSMode
     write FDOSMode;
   property ExtractOptions : TAbExtractOptions
     read FExtractOptions
     write FExtractOptions;
   property IsDirty : Boolean
     read FIsDirty
     write FIsDirty;
   property ItemList : TAbArchiveList
     read FItemList;
   property LogFile : string
     read FLogFile
     write SetLogFile;
   property Logging : Boolean
     read FLogging
     write SetLogging;
   property Mode : Word
     read FMode;
   property Spanned : Boolean
     read FSpanned;
   property SpanningThreshold : Int64
     read  GetSpanningThreshold
     write SetSpanningThreshold;
   property Status : TAbArchiveStatus
     read FStatus;
   property StoreOptions : TAbStoreOptions
     read FStoreOptions
     write FStoreOptions;
   property SupportsEmptyFolders :  Boolean
     read GetSupportsEmptyFolders;
   property TempDirectory : string
     read FTempDir
     write FTempDir;

 public {events}
   property OnProcessItemFailure : TAbArchiveItemFailureEvent
     read FOnProcessItemFailure
     write FOnProcessItemFailure;
   property OnArchiveProgress : TAbArchiveProgressEvent
     read FOnArchiveProgress
     write FOnArchiveProgress;
   property OnArchiveSaveProgress : TAbArchiveProgressEvent
     read FOnArchiveSaveProgress
     write FOnArchiveSaveProgress;
   property OnArchiveItemProgress : TAbArchiveItemProgressEvent
     read FOnArchiveItemProgress
     write FOnArchiveItemProgress;
   property OnConfirmProcessItem : TAbArchiveItemConfirmEvent
     read FOnConfirmProcessItem
     write FOnConfirmProcessItem;
   property OnConfirmOverwrite : TAbConfirmOverwriteEvent
     read FOnConfirmOverwrite
     write FOnConfirmOverwrite;
   property OnConfirmSave : TAbArchiveConfirmEvent
     read FOnConfirmSave
     write FOnConfirmSave;
   property OnLoad : TAbArchiveEvent
     read FOnLoad
     write FOnLoad;
   property OnRequestImage : TAbRequestImageEvent
     read FOnRequestImage
     write FOnRequestImage;
   property OnSave : TAbArchiveEvent
     read FOnSave
     write FOnSave;
 end;

          *)

end.

