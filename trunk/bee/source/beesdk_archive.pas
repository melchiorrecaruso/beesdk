unit BeeSDK_Archive;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  HT0 = $00000;
  HT1 = $00001;

const
  HF1_VERSION     = $00001;
  HF1_NAME        = $00002;
  HF1_SIZE        = $00004;
  HF1_CODER       = $00008;
  HF1_CTIME       = $00010;
  HF1_MTIME       = $00020;
  HF1_ATIME       = $00040;
  HF1_ATTRIBUTES  = $00080;
  HF1_MODE        = $00100;
  HF1_CRC         = $00200;
  HF1_CRYPTER     = $00400;
  HF1_DISK        = $00800;
  HF1_SEEK        = $01000;
  HF1_UID         = $02000;
  HF1_GID         = $04000;
  HF1_UNAME       = $08000;
  HF1_GNAME       = $10000;
  HF1_COMMENT     = $20000;

type
  TArcHeaderTypeCustom = packed record
    Flags: longword;
    Version: longword;
    FileNameLen: longword;
    FileName: string;
    Size: qword;
    StoredSize: qword;
    CFileTime: longword;
    MFileTime: longword;
    AFileTime: longword;
    Attributes: longword;
    Mode: longword;
    CRC: longword;
    case longword of
      0: (Coder: longword);
      1: (Coder: longint);
    end;
    Crypter: packed record
      FType: longword;
      FFlags: longword;
      FVersion: longword;
    end;




  end;


type
  TArcHeader = class(TObject)
  private
    FType: longword;

    FData: pointer;
  end;

type
  TArcHeaderCustom =
  protected
    FFlags: longword;










    Method:  byte;



    FDiskFileName     : string;

    NextItem          : TBeeArchiveItem;
    FAction           : TBeeArchiveAction;








    FIsEncrypted      : Boolean;

    FTagged           : Boolean;


  protected
    function GetCompressedSize : Int64; virtual;
    function GetCRC32 : Longint; virtual;
    function GetDiskPath : string;
    function GetExternalFileAttributes : LongWord; virtual;
    function GetFileName : string; virtual;
    function GetIsDirectory: Boolean; virtual;
    function GetIsEncrypted : Boolean; virtual;
    function GetLastModFileDate : Word; virtual;
    function GetLastModFileTime : Word; virtual;
    function GetNativeFileAttributes : LongInt; virtual;
    function GetStoredPath : string;
    function GetUncompressedSize : Int64; virtual;
    procedure SetCompressedSize(const Value : Int64); virtual;
    procedure SetCRC32(const Value : Longint); virtual;
    procedure SetExternalFileAttributes( Value : LongWord ); virtual;
    procedure SetFileName(const Value : string); virtual;
    procedure SetIsEncrypted(Value : Boolean); virtual;
    procedure SetLastModFileDate(const Value : Word); virtual;
    procedure SetLastModFileTime(const Value : Word); virtual;
    procedure SetUncompressedSize(const Value : Int64); virtual;
    function GetLastModTimeAsDateTime: TDateTime; virtual;
    procedure SetLastModTimeAsDateTime(const Value: TDateTime); virtual;

  public {methods}
    constructor Create;
    destructor Destroy; override;
    function MatchesDiskName(const FileMask : string) : Boolean;
    function MatchesStoredName(const FileMask : string) : Boolean;
    function MatchesStoredNameEx(const FileMask : string) : Boolean;


  public {properties}
    property Action : TAbArchiveAction
      read FAction
      write FAction;
    property CompressedSize : Int64
      read GetCompressedSize
      write SetCompressedSize;
    property CRC32 : Longint
      read GetCRC32
      write SetCRC32;
    property DiskFileName : string
      read FDiskFileName
      write FDiskFileName;
    property DiskPath : string
      read GetDiskPath;
    property ExternalFileAttributes : LongWord
      read GetExternalFileAttributes
      write SetExternalFileAttributes;
    property FileName : string
      read GetFileName
      write SetFileName;
    property IsDirectory: Boolean
      read GetIsDirectory;
    property IsEncrypted : Boolean
      read GetIsEncrypted
      write SetIsEncrypted;
    property LastModFileDate : Word
      read GetLastModFileDate
      write SetLastModFileDate;
    property LastModFileTime : Word
      read GetLastModFileTime
      write SetLastModFileTime;
    property NativeFileAttributes : LongInt
      read GetNativeFileAttributes;
    property StoredPath : string
      read GetStoredPath;
    property Tagged : Boolean
      read FTagged
      write FTagged;
    property UncompressedSize : Int64
      read GetUncompressedSize
      write SetUncompressedSize;

    property LastModTimeAsDateTime : TDateTime
      read GetLastModTimeAsDateTime
      write SetLastModTimeAsDateTime;
  end;


{ ===== TAbArchiveListEnumerator ============================================ }
type
  TAbArchiveList = class;
  TAbArchiveListEnumerator = class
  private
    FIndex: Integer;
    FList: TAbArchiveList;
  public
    constructor Create(aList: TAbArchiveList);
    function GetCurrent: TAbArchiveItem;
    function MoveNext: Boolean;
    property Current: TAbArchiveItem read GetCurrent;
  end;


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



implementation

end.

