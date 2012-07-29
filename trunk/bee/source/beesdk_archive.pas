unit BeeSDK_Archive;

interface

uses
  Classes, SysUtils, Bee_Common, Bee_Files, BeeLib_Configuration;

const
  /// beex id marker
  beexMARKER =  $1A656542;

type
  /// archive item type
  TBeeArchiveItemType = (
    aitCUSTOM,
    aitBINDING);

  /// archive binding item flag
  TBeeArchiveBindingItemFlag = (
    abifVERSION,
    abifID,
    abifDISK,
    abifDISKs,
    abifCOMMENT,
    abifCRC);

  TBeeArchiveBindingItemFlags = set of TBeeArchiveBindingItemFlag;

  /// archive custom item flag
  TBeeArchiveCustomItemFlag = (
    acifVERSION,
    acifNAME,
    acifSIZE,
    acifCTIME,
    acifMTIME,
    acifATIME,
    acifATTRIBUTES,
    acifMODE,
    acifCRC,
    acifCODER,
    acifCRYPTER,
    acifDISK,
    acifSEEK,
    acifUID,
    acifUNAME,
    acifGID,
    acifGNAME,
    acifCOMMENT);

  TBeeArchiveCustomItemFlags = set of TBeeArchiveCustomItemFlag;

  /// CODER type

  cotBEE         =  $00001;
  cotROLOZ       =  $00002;

  /// BEE Coder flag

  bcofVERSION    =  $00001;
  bcofMETHOD     =  $00002;
  bcofDICTIONARY =  $00004;
  bcofTABLE      =  $00008;
  bcofTEAR       =  $00010;

  /// ROLOZ Coder flag

  rcofVERSION    =  $00001;

  /// CRYPTER type

  crtBLOWFISH    =  $00001;

  /// BLOWFISH Crypter flag

  bfcrfVERSION   =  $00001;

type
  TBeeCoder = class(TObject)
  protected {private}
    FFlags: longword;
    FMethod: longword;
    FDictionary: longword;
    FTable: TTableParameters;
    FCompressedSize: qword;
  protected {property methods}
    procedure SetMethod(Value: longword);
    procedure SetDictionary(Value: longword);
    procedure SetTable(Value: TTableParameters);
  public {methods}
    constructor Create;
    destructor Destroy; override;
  public {properties}
    property Flags: longword read FFlags;
    property Method: longword read FMethod write SetMethod;
    property Dictionary: longword read FDictionary write SetDictionary;
    property Table: TTableParameters read FTable write SetTable;
    property CompressedSize: qword read FCompressedSize write FCompressedSize;
  end;

  TRolozCoder = class(TObject)
  protected {private}
    FFlags: longword;
    FCompressedSize: qword;
  public {methods}
    constructor Create;
    destructor Destroy; override;
  public {properties}
    property Flags: longword read FFlags;
    property CompressedSize: qword read FCompressedSize write FCompressedSize;
  end;

  TBlowFishCrypter = class(TObject)
  protected {private}
    FFlags: longword;
  public {properties}
    property Flags: longword read FFlags;
  end;

  { Describe the pending action for an archive item }

  TBeeArchiveItemAction = (aaNone, aaUpdate, aaDecode, aaDecodeAndUpdate);

  TBeeArchiveItem = class(TObject)
  protected {private}
    NextItem : TBeeArchiveItem;
    FAction: TBeeArchiveItemAction;

    FFlags: longword;
    FVersionNeededToExtract: longword;
    FFileName: string;
    FUncompressedSize: qword;
    FCreationTime: longword;
    FLastModifiedTime: longword;
    FLastAccessTime: longword;
    FAttributes: longword;
    FMode: longword;
    FCRC: longword;
    FCoder: TObject;
    FCrypter: TObject;
    FDiskNumber: longword;
    FDiskSeek: qword;

    FExternalFileName: string;
    FExternalAttributes: longword;
    FExternalUncompressedSize: qword;

    FUserID: longword;
    FGroupID: longword;
    FUserName: string;
    FGroupName: string;
    FComment: string;
  protected {property methods}
    procedure SetFileName(const Value: string);

    procedure SetDiskNumber(const Value: longword);
    procedure SetDiskSeek(const Value: qword);

    procedure SetUserID(const Value: longword);
    procedure SetUserName(const Value: string);
    procedure SetGroupID(const Value: longword);
    procedure SetGroupName(const Value: string);
    procedure SetComment(const Value: string);
  public {methods}
    property Action: TBeeArchiveItemAction read FAction write FAction;
    property Flags: longword read FFlags;
    property VersionNeededToExtract: longword read FVersionNeededToExtract;
    property FileName: string read FFileName write SetFileName;
    property UncompressedSize: qword read FUncompressedSize;
    property CreationTime: longword read FCreationTime;
    property LastModifiedTime: longword read FLastModifiedTime;
    property LastAccessTime: longword read FLastAccessTime;
    property Attributes: longword read FAttributes;
    property Mode: longword read FMode;
    property CRC: longword read FCRC;
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
  end;

  TBeeBindingItem = class(TObject)
  protected {private}
    FFlags: longword;
    FVersionNeededToExtract: longword;
    FID: qword;
    FCRC: longword;
    FDiskNumber: longword;
    FDiskSeek: qword;
    FComment: string;
  protected {property methods}
    procedure SetVersionNeededToExtract(Value: longword);
    procedure SetID(const Value: qword);
    procedure SetCRC(Value: longword);
    procedure SetDiskNumber(const Value: longword);
    procedure SetDiskSeek(const Value: qword);
    procedure SetComment(const Value: string);
  public {methods}
    constructor Create;
    destructor Destroy; override;
    function IsAssigned(Flag: longword): boolean;
  public
    property Flags: longword read FFlags;
    property VersionNeedToExtract: longword
       read FVersionNeededToExtract write SetVersionNeededToExtract;

    property ID: qword read FID write SetID;
    property CRC: longword read FCRC write SetCRC;
    property DiskNumber: longword read FDiskNumber write SetDiskNumber;
    property DiskSeek: qword read FDiskSeek write SetDiskSeek;
    property Comment: string read FComment write SetComment;
  end;

  TBeeArchiveList = class(TObject)
  private {private}
    FItems: TList;
    FLastItem: TBeeArchiveItem;
  private { methods}
    function GetCount : longint;
    function GetItem(Index : longint): TBeeArchiveItem;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item : TBeeArchiveItem);
    function Find(const FileName: string): longint;
    procedure Clear;
  public {properties}
    property Count: longint read GetCount;
    property Items[Index : longint]: TBeeArchiveItem read GetItem;
    property LastItem: TBeeArchiveItem read FLastItem;
  end;

implementation

// TBeeBindingItem class

constructor TBeeBindingItem.Create;
begin
  inherited Create;
  FFlags :=  0;
end;

destructor TBeeBindingItem.Destroy;
begin
  inherited Destroy;
end;

function TBeeBindingItem.IsAssigned(Flag: longword): boolean;
begin
  Result := (FFlags and Flag) = Flag;
end;

procedure TBeeBindingItem.SetVersionNeededToExtract(Value: longword);
begin
  if Value <> longword(-1) then
  begin
    FVersionNeededToEstract := Value;
    FFlags := FFlags or bhfVERSION;
  end else
  begin


  end;
end;



// TBeeArchiveList class

constructor TBeeArchiveList.Create;
begin
  inherited Create;
  FLastItem := nil;
  FItems := TList.Create;
end;

destructor TBeeArchiveList.Destroy;
begin
  Clear;
  FItems.Destroy;
  inherited Destroy;
end;

procedure TBeeArchiveList.Add(Item: TBeeArchiveItem);
var
  Lo, Med, Hi, I: longint;
begin
  if FItems.Count = 0 then
  begin
    FItems.Add(Item);
  end else
  begin
    Lo := 0;
    Hi := FItems.Count - 1;
    while Hi >= Lo do
    begin
      Med := (Lo + Hi) div 2;
      I := Bee_Common.CompareFileName(Item.FileName,
        TBeeArchiveItem(FItems[Med]).FileName);

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
      FItems.Insert(Med + 1, Item);
    end else
    begin
      if I > 0 then
        FItems.Insert(Med + 1, Item)
      else
        FItems.Insert(Med, Item);
    end;
  end;
  Item.NextItem := FLastItem;
  FLastItem := Item;
end;

function TBeeArchiveList.Find(const FileName: string): longint;
var
  Lo, Med, Hi, I: longint;
begin
  Lo := 0;
  Hi := FItems.Count - 1;
  while Hi >= Lo do
  begin
    Med := (Lo + Hi) div 2;
    I := Bee_Common.CompareFileName(FileName,
      TBeeArchiveItem(FItems[Med]).FileName);

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

procedure TBeeArchiveList.Clear;
var
  I: longint;
begin
  for I := FItems.Count - 1 downto 0 do
  begin
    TBeeArchiveItem(FItems[I]).Destroy;
  end;
  FItems.Clear;
  FLastItem := nil;
end;

function TBeeArchiveList.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TBeeArchiveList.GetItem(Index: longint): TBeeArchiveItem;
begin
  Result := TBeeArchiveItem(FItems[Index]);
end;

//



function ReadMagicSeek(Stream: TFileReader): qword;
begin


end;

function ReadBeeCoder(Stream: TFileReader): TBeeCoder;
begin
  Result := TBeeCoder.Create;
  Result.Flags := Stream.ReadInfint;

  if (Result.Flags and bcofVERSION) > 0 then
    Result.Version := Stream.ReadInfint;

  if (Result.Flags and bcofMETHOD) > 0 then
    Result.Method := Stream.ReadInfint;

  if (Result.Flags and bcofDICTIONARY) > 0 then
    Result.Dictionary := Stream.ReadInfint;

  if (Result.Flags and bcofTABLE) > 0 then
    Stream.Read(Result.Table, SizeOf(Result.Table));
end;

function ReadRolozCoder(Stream: TFileReader): TRolozCoder;
begin
  Result := TRolozCoder.Create;
  Result.Flags := Stream.ReadInfint;

  if (Result.Flags and rcofVERSION) > 0 then
    Result.Version := Stream.ReadInfint;
end;

function ReadCoder(Stream: TFileReader): TObject;
begin
  case Stream.ReadInfint of
    cotBEE:   Result := ReadBeeCoder(Stream);
    cotROLOZ: Result := ReadRolozCoder(Stream);
    else      Result := nil;
  end;
end;

function ReadBlowFishCrypter(Stream: TFileReader): TBlowFishCrypter;
begin
  Result := TBlowFishCrypter.Create;
  Result.Flags := Stream.ReadInfint;

  if (Result.Flags and bfcrfVERSION) > 0 then
    Result.Version := Stream.ReadInfint;
end;

function ReadCrypter(Stream: TFileReader): TObject;
begin
  case Stream.ReadInfint of
    crtBLOWFISH: Result := ReadBlowFishCrypter(Stream);
    else         Result := nil;
  end;
end;

function ReadCustomHeader(Stream: TFileReader): TCustomHeader;
begin
  Result := TCustomHeader.Create;
  Result.Flags := Stream.ReadInfint;

  if (Result.Flags and chfVERSION) > 0 then
    Result.Version := Stream.ReadInfint;

  if (Result.Flags and chfNAME) > 0 then
    Result.Name := Stream.ReadInfString;

  if (Result.Flags and chfSIZE) > 0 then
    Result.Size := Stream.ReadInfint;

  if (Result.Flags and chfCTIME) > 0 then
    Result.CTime := Stream.ReadInfint;

  if (Result.Flags and chfMTIME) > 0 then
    Result.MTime := Stream.ReadInfint;

  if (Result.Flags and chfATIME) > 0 then
    Result.ATime := Stream.ReadInfint;

  if (Result.Flags and chfATTRIBUTES) > 0 then
    Result.Attributes := Stream.ReadInfint;

  if (Result.Flags and chfMODE) > 0 then
    Result.Mode := Stream.ReadInfint;

  if (Result.Flags and chfCRC) > 0 then
    Result.CRC := Stream.ReadInfint;

  if (Result.Flags and chfCODER) > 0 then
    Result.Coder := ReadCoder(Stream)
  else
    Result.Coder := nil;

  if (Result.Flags and chfCRYPTER) > 0 then
    Result.Crypter := ReadCrypter(Stream)
  else
    Result.Crypter := nil;

  if (Result.Flags and chfDISK) > 0 then
    Result.Disk := Stream.ReadInfint;

  if (Result.Flags and chfSEEK) > 0 then
    Result.Seek := Stream.ReadInfint;

  if (Result.Flags and chfUID) > 0 then
    Result.UID := Stream.ReadInfint;

  if (Result.Flags and chfUNAME) > 0 then
    Result.UName := Stream.ReadInfString;

  if (Result.Flags and chfGID) > 0 then
    Result.GID := Stream.ReadInfint;

  if (Result.Flags and chfGNAME) > 0 then
    Result.GName := Stream.ReadInfString;

  if (Result.Flags and chfCOMMENT) > 0 then
    Result.Comment := Stream.ReadInfString;
end;

function ReadBindingHeader(Stream: TFileReader): TBindingHeader;
begin
  Result := TBindingHeader.Create;
  Result.Flags := Stream.ReadInfint;

  if (Result.Flags and bhfVERSION) > 0 then
    Result.Version := Stream.ReadInfint;

  if (Result.Flags and bhfID) > 0 then
    Result.ID := Stream.ReadInfint;

  if (Result.Flags and bhfDISK) > 0 then
    Result.DISK := Stream.ReadInfint;

  if (Result.Flags and bhfDISKs) > 0 then
    Result.DISKs := Stream.ReadInfint;

  if (Result.Flags and bhfCOMMENT) > 0 then
    Result.Comment := Stream.ReadInfString;

  if (Result.Flags and bhfCRC) > 0 then
    Result.CRC := Stream.ReadInfint;

  Result.Seek := Stream.ReadInfint;
end;

function ReadHeader(Stream: TFileReader): TObject;
begin
end;

function ReadHeaders(Stream: TFileReader; List: TList): boolean;
var
  Marker: longword;
  HeaderType: longword;
begin
  Result := False;
  Stream.Seek(ReadMagicSeek(Stream), soFromBeginning);

  Marker := 0;
  Stream.Read(Marker, 4);
  if Marker = beexMarker then
  begin
    repeat
      HeaderType := Stream.ReadInfint;
      case HeaderType of
        htCUSTOM:  List.Add(ReadCustomHeader(Stream));
        htBINDING: List.Add(ReadBindingHeader(Stream));
        else       Break;
      end;
    until HeaderType = htBINDING;
    Result := HeaderType = htBINDING
  end;
end;


procedure WriteMagicSeek(Stream: TStream);
begin


end;

function WriteGenericHeader(Item: TCustomHeader; Stream: TFileWriter): boolean;
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

