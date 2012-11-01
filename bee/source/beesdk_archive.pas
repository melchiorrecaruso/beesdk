unit BeeSDK_Archive;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Bee_Files,
  Bee_Common,
  Bee_Consts,
  Bee_MainPacker,
  Bee_Configuration,
  Bee_CommandLine,
  BeeLib_Configuration;

const
  /// beex archive marker
  beexArchiveMarker = $1A656542;

  /// beex archive version
  beexVersionNeededToRead    = $00;
  beexVersionNeededToExtract = $00;

  /// archive item type
  aitItem      = $01;
  aitBinding   = $02;
  aitLocator   = $7E;
  aitEnd       = $7F;

type
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
    aifUncompressedSize,
    aifLastModifiedTime,
    aifAttributes,
    aifComment);

  TArchiveItemFlags = set of TArchiveItemFlag;

  // archive data descriptor flag
  TArchiveDataDescriptorFlag = (
    adfCompressedSize,
    adfDiskNumber,
    adfDiskSeek,
    adfCRC32);

  TArchiveDataDescriptorFlags = set of TArchiveDataDescriptorFlag;

  /// archive compression method
  TArchiveCompressionMethod = (actNone, actMain);

  /// archive compression flag
  TArchiveCompressionFlag = (
    acfCompressionMethod,
    acfCompressionLevel,
    acfDictionaryLevel,
    acfSolidCompression,
    acfCompressionTable);

  TArchiveCompressionFlags = set of TArchiveCompressionFlag;

  /// archive encryption method
  TArchiveEncryptionMethod = (acrtNone, acrtMain);

  /// archive encryption flag
  TArchiveEncryptionFlag = (
    aefEncryptionMethod);

  TArchiveEncryptionFlags = set of TArchiveEncryptionFlag;

  TArchiveItemTag = (aitNone, aitAdd, aitUpdate, aitDecode, aitDecodeAndUpdate);

  TArchiveItem = class(TObject)
  protected
    // Item property
    FFileName: string;
    FFlags: TArchiveItemFlags;
    FUncompressedSize: int64;
    FLastModifiedTime: longword;
    FAttributes: longword;
    FComment: string;
    // Data descriptor property
    FDataDescriptorFlags: TArchiveDataDescriptorFlags;
    FCompressedSize: int64;
    FDiskNumber: longword;
    FDiskSeek: int64;
    FCRC32: longword;
    // Compression property
    FCompressionFlags: TArchiveCompressionFlags;
    FCompressionMethod: TArchiveCompressionMethod;
    FCompressionLevel: TmOption;
    FDictionaryLevel: TdOption;
    FCompressionTable: TTableParameters;
    // Encryption property
    FEncryptionFlags: TArchiveEncryptionFlags;
    FEncryptionMethod: TArchiveEncryptionMethod;
  protected
    FPosition: longint;
    FTag: TArchiveItemTag;
    FExternalFileName: string;
    FExternalFileSize: int64;
    function GetSolidCompression: boolean;
    procedure SetSolidCompression(Value: boolean);
  public {methods}
    constructor Create;
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    procedure Update(SearchRec: TCustomSearchRec);
  public {property}
    // Item property
    property FileName: string read FFileName;
    property Flags: TArchiveItemFlags read FFlags;
    property UncompressedSize: int64 read FUncompressedSize;
    property LastModifiedTime: longword read FLastModifiedTime;
    property Attributes: longword read FAttributes;
    property Comment: string read FComment;
    // Data property
    property DadaDescriptorFlags: TArchiveDataDescriptorFlags read FDataDescriptorFlags;
    property CompressedSize: int64 read FCompressedSize;
    property DiskNumber: longword read FDiskNumber;
    property DiskSeek: int64 read FDiskSeek;
    property CRC32: longword read FCRC32;
    // Compression property
    property CompressionFlags: TArchiveCompressionFlags read FCompressionFlags;
    property CompressionMethod: TArchiveCompressionMethod read FCompressionMethod;
    property CompressionLevel: TmOption read FCompressionLevel;
    property DictionaryLevel: TdOption read FDictionaryLevel;
    property SolidCompression: boolean read GetSolidCompression write SetSolidCompression;
    property CompressionTable: TTableParameters read FCompressionTable;
    // Encryption property
    property EncryptionFlags: TArchiveEncryptionFlags read FEncryptionFlags;
    property EncryptionMethod: TArchiveEncryptionMethod read FEncryptionMethod;
  end;

  TArchiveCustomItems = class(TObject)
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

  TArchiveProgressEvent = procedure(Percentage: longint) of object;

  TArchiveClearProgressEvent = procedure of object;

  TArchiveMessageEvent = procedure(const Message: string) of object;

  TArchiveConfirm = (arcOk, arcCancel, arcQuit);

  TArchiveRenameEvent = procedure(Item: TArchiveItem;
    var RenameAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveExtractEvent = procedure(Item: TArchiveItem;
    var ExtractAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveEraseEvent = procedure(Item: TArchiveItem;
    var Confirm: TArchiveConfirm) of object;

  TArchiveUpdateEvent = procedure(SearchRec: TCustomSearchRec;
    var UpdateAs: string; var Confirm: TArchiveConfirm) of object;

  // a class for each command

  TArchiveReader = class(TObject)
  private
    FDecoder: THeaderDecoder;
    FTotalSize: int64;
    FProcessedSize: int64;
    FArchiveName: string;
    FArchivePassword: string;
    FArchiveComment: string;
    FArchiveReader: TFileReader;
    FSwapName: string;
    FSwapReader: TFileReader;
    FSwapWriter: TFileWriter;
    FSuspended:  boolean;
    FOnProgress: TArchiveProgressEvent;
    FOnMessage: TArchiveMessageEvent;
    FOnFailure: TArchiveMessageEvent;
    FOnClearProgress: TArchiveClearProgressEvent;
    FOnRequestImage: TFileReaderRequestImageEvent;
    FArchiveItems: TArchiveCustomItems;
    procedure InitDecoder (Item: TArchiveItem);
    procedure DecodeToSwap(Item: TArchiveItem);
    procedure DecodeToNil (Item: TArchiveItem);
    procedure DecodeToFile(Item: TArchiveItem);
    procedure UnPackCentralDirectory;
    procedure ReadCentralDirectory(aStream: TFileReader);
    function GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetBackTear(Index: longint): longint;
    function GetNextTear(Index: longint): longint;
    function GetCount: longint;
    function GetItem(Index: longint): TArchiveItem;
    procedure SetArchiveName(const Value: string);
    procedure SetExitCode(Value: longint);
    procedure DoRequestImage(ImageNumber: longint;
      var ImageName: string; var Abort: boolean);
    procedure DoFailure(const ErrorMessage: string);
    procedure DoMessage(const Message: string);
    function DoProgress(Value: longint): boolean;
    procedure DoClear;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenArchive(const aArchiveName: string);
    procedure CloseArchive; virtual;
    procedure Terminate;
    function Find(const aFileName: string): longint;
  public
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property ArchivePassword: string read FArchivePassword write FArchivePassword;
    property ArchiveComment: string read FArchiveComment write FArchiveComment;
    property Items[Index: longint]: TArchiveItem read GetItem;
    property Count: longint read GetCount;

    property Suspended: boolean read FSuspended write FSuspended;
    property OnProgress: TArchiveProgressEvent read FOnProgress write FOnProgress;
    property OnClearProgress: TArchiveClearProgressEvent read FOnClearProgress write FOnClearProgress;
    property OnMessage: TArchiveMessageEvent read FOnMessage write FOnMessage;
    property OnFailure: TArchiveMessageEvent read FOnFailure write FOnFailure;
    property OnRequestImage: TFileReaderRequestImageEvent
      read FOnRequestImage write FOnRequestImage;
  end;

  TCustomArchiveReader = class(TArchiveReader)
  public
    procedure Tag(Index: longint);
    procedure UnTag(Index: longint);
    procedure TagAll;
    procedure UnTagAll;
    function IsTagged(Index: longint): boolean;
  end;

  TArchiveWriter = class(TArchiveReader)
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
    procedure WriteCentralDirectory(aStream: TFileWriter);
    procedure PackCentralDirectory;
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

  TCustomArchiveWriter = class(TArchiveWriter)
  public
    procedure TagAll;
    procedure UnTagAll;
    procedure Tag(Index: longint);
    procedure UnTag(Index: longint);
  end;

  TArchiveExtractor = class(TCustomArchiveReader)
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

  TArchiveRenamer = class(TCustomArchiveWriter)
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

  TArchiveEraser = class(TCustomArchiveWriter)
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

  TArchiveUpdater = class(TArchiveWriter)
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

    property OnUpdate: TArchiveUpdateEvent read FOnUpdate write FOnUpdate;
  end;

implementation

uses
  Bee_Assembler;

// TArchiveItem class

constructor TArchiveItem.Create;
begin
  inherited Create;
  /// Item property ///
  FFileName := '';
  FFlags := [
    aifUncompressedSize,
    aifLastModifiedTime,
    aifAttributes,
    aifComment];

  FUncompressedSize    :=  0;
  FLastModifiedTime    :=  0;
  FAttributes          :=  0;
  FComment             := '';
  /// Data descriptor property ///
  FDataDescriptorFlags := [
    adfCompressedSize,
    adfDiskNumber,
    adfDiskSeek,
    adfCRC32];

  FCompressedSize      :=  0;
  FDiskNumber          :=  0;
  FDiskSeek            :=  0;
  FCRC32               :=  0;
  /// Compression property ///
  FCompressionFlags := [
    acfCompressionMethod,
    acfCompressionLevel,
    acfDictionaryLevel,
    acfSolidCompression,
    acfCompressionTable];

  FCompressionMethod   := actNone;
  FCompressionLevel    := moStore;
  FDictionaryLevel     := do2MB;
//FCompressionTable    := EmptyTableParameters;

  /// Encryption property ///
  FEncryptionFlags     := [];
  FEncryptionMethod  := acrtNone;
  /// Reserved property ///
  FTag               := aitNone;
  FPosition          := -1;
  FExternalFileName  := '';
  FExternalFileSize  :=  0;
end;

procedure TArchiveItem.Update(SearchRec: TCustomSearchRec);
begin
  /// Item property ///
//FFileName := SearchRec.Name;
//if (aifUncompressedSize in FFLags) then FUncompressedSize := SearchRec.Size;
  if (aifLastModifiedTime in FFLags) then FLastModifiedTime := SearchRec.LastModifiedTime;
  if (aifAttributes       in FFLags) then FAttributes       := SearchRec.Attributes;
//if (aifComment          in FFLags) then FComment          := UseComment;

  /// Data descriptor property ///
//FDataDescriptorFlags := [];
//FCompressedSize      :=  0;
//FDiskNumber          :=  0;
//FDiskSeek            :=  0;
//FCRC32               :=  0;

  /// Compression property ///
//FCompressionFlags  := [];
//FCompressionMethod := actNone;
//FCompressionLevel  :=  0;
//FDictionaryLevel   :=  0;
//FCompressionTable  :=  EmptyTableParameters;

  /// Encryption property ///
//FEncryptionFlags  := [];
//FEncryptionMethod :=  0;

  /// Reserved property ///
//FTag              := aitNone;
//FPosition         := -1;
  FExternalFileName := SearchRec.Name;
  FExternalFileSize := SearchRec.Size;
end;

constructor TArchiveItem.Read(Stream: TFileReader);
begin
  /// Item property ///
  FFileName := Stream.ReadInfString;
  FFlags    := TArchiveItemFlags(longword(Stream.ReadInfWord));
  if (aifUncompressedSize  in FFlags) then FUncompressedSize := Stream.ReadInfWord;
  if (aifLastModifiedTime  in FFlags) then FLastModifiedTime := Stream.ReadInfWord;
  if (aifAttributes        in FFlags) then FAttributes       := Stream.ReadInfWord;
  if (aifComment           in FFlags) then FComment          := Stream.ReadInfString;
  /// Data descryptor property ///
  FDataDescriptorFlags := TArchiveDataDescriptorFlags(longword(Stream.ReadInfWord));
  if (adfCompressedSize    in FDataDescriptorFlags) then FCompressedSize := Stream.ReadInfWord;
  if (adfDiskNumber        in FDataDescriptorFlags) then FDiskNumber     := Stream.ReadInfWord;
  if (adfDiskSeek          in FDataDescriptorFlags) then FDiskSeek       := Stream.ReadInfWord;
  if (adfCRC32             in FDataDescriptorFlags) then FCRC32          := Stream.ReadInfWord;
  /// Compression property ///
  FCompressionFlags := TArchiveCompressionFlags(longword(Stream.ReadInfWord));
  if (acfCompressionMethod in FCompressionFlags)    then FCompressionMethod := TArchiveCompressionMethod(longword(Stream.ReadInfWord));
  if (acfCompressionLevel  in FCompressionFlags)    then FCompressionLevel  := TmOption(Stream.ReadInfWord);
  if (acfDictionaryLevel   in FCompressionFlags)    then FDictionaryLevel   := TdOption(Stream.ReadInfWord);
  if (acfCompressionTable  in FCompressionFlags)    then Stream.Read(FCompressionTable, SizeOf(TTableParameters));
  /// Encryption property ///
  FEncryptionFlags := TArchiveEncryptionFlags(longword(Stream.ReadInfWord));
  if (aefEncryptionMethod  in FEncryptionFlags)     then FEncryptionMethod := TArchiveEncryptionMethod(longword(Stream.ReadInfWord));
end;

procedure TArchiveItem.Write(Stream: TFileWriter);
begin
  /// Item property ///
  Stream.WriteInfString(FFileName);
  Stream.WriteInfWord(longword(FFlags));
  if (aifUncompressedSize  in FFlags) then Stream.WriteInfWord(FUncompressedSize);
  if (aifLastModifiedTime  in FFlags) then Stream.WriteInfWord(FLastModifiedTime);
  if (aifAttributes        in FFlags) then Stream.WriteInfWord(FAttributes);
  if (aifComment           in FFlags) then Stream.WriteInfString(FComment);
  /// Data descriptor property ///
  Stream.WriteInfWord(longword(FDataDescriptorFlags));
  if (adfCompressedSize    in FDataDescriptorFlags) then Stream.WriteInfWord(FCompressedSize);
  if (adfDiskNumber        in FDataDescriptorFlags) then Stream.WriteInfWord(FDiskNumber);
  if (adfDiskSeek          in FDataDescriptorFlags) then Stream.WriteInfWord(FDiskSeek);
  if (adfCRC32             in FDataDescriptorFlags) then Stream.WriteInfWord(FCRC32);
  /// Compression property ///
  Stream.WriteInfWord(longword(FCompressionFlags));
  if (acfCompressionMethod in FCompressionFlags)    then Stream.WriteInfWord(Ord(FCompressionMethod));
  if (acfCompressionLevel  in FCompressionFlags)    then Stream.WriteInfWord(Ord(FCompressionLevel));
  if (acfDictionaryLevel   in CompressionFlags)     then Stream.WriteInfWord(Ord(FDictionaryLevel));
  if (acfCompressionTable  in FCompressionFlags)    then Stream.Write(FCompressionTable, SizeOf(TTableParameters));
  /// Encryption property ///
  Stream.WriteInfWord(longword(FEncryptionFlags));
  if (aefEncryptionMethod  in FEncryptionFlags)     then Stream.WriteInfWord(Ord(FEncryptionMethod));
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

// TArchiveCustomItems class

constructor TArchiveCustomItems.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FNames := TList.Create;
end;

destructor TArchiveCustomItems.Destroy;
begin
  Clear;
  FItems.Destroy;
  FNames.Destroy;
  inherited Destroy;
end;

procedure TArchiveCustomItems.Clear;
var
  I: longint;
begin
  for I := 0 to FItems.Count - 1 do
    TArchiveItem(FItems[I]).Destroy;
  FItems.Clear;
  FNames.Clear;
end;

procedure TArchiveCustomItems.Add(Item: TArchiveItem);
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
      I := AnsiCompareFileName(Item.FileName,
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

function TArchiveCustomItems.GetNameIndex(const FileName: string): longint;
var
  Lo, Med, Hi, I: longint;
begin
  Lo := 0;
  Hi := FNames.Count - 1;
  while Hi >= Lo do
  begin
    Med := (Lo + Hi) div 2;
    I := AnsiCompareFileName(FileName,
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

function TArchiveCustomItems.Find(const FileName: string): longint;
begin
  Result := GetNameIndex(FileName);
  if Result <> -1 then
  begin
    Result := TArchiveItem(FNames[Result]).FPosition;
  end;
end;

procedure TArchiveCustomItems.Delete(Index: longint);
var
  I: longint;
  Item, Next: TArchiveItem;
begin
  Item := Items[Index];
  if Index < FItems.Count - 1 then
  begin
    Next := Items[Index + 1];

    if    (acfSolidCompression in Item.CompressionFlags) and
      (not(acfSolidCompression in Next.CompressionFlags)) then
       Next.SetSolidCompression(TRUE);

    if    (acfCompressionTable in Item.CompressionFlags) and
      (not(acfCompressionTable in Next.CompressionFlags)) then
    begin
      Next.FCompressionTable :=  Item.FCompressionTable;
      Include(Next.FCompressionFlags, acfCompressionTable);
    end;
  end;

  FNames.Delete(GetNameIndex(Item.FileName));
  FItems.Delete(Item.FPosition);
  Item.Destroy;
end;

function TArchiveCustomItems.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TArchiveCustomItems.GetItem(Index: longint): TArchiveItem;
begin
  Result := TArchiveItem(FItems[Index]);
end;

// TArchiveReader class

constructor TArchiveReader.Create;
begin
  inherited Create;
  Randomize;
  FArchiveItems := TArchiveCustomItems.Create;
  ExitCode := ccSuccesful;
end;

destructor TArchiveReader.Destroy;
begin
  FArchiveItems.Destroy;
  inherited Destroy;
end;

procedure TArchiveReader.Terminate;
begin
  SetExitCode(ccUserAbort);
  FSuspended := False;
end;

function TArchiveReader.GetCount: longint;
begin
  Result := FArchiveItems.Count;
end;

function TArchiveReader.GetItem(Index: longint): TArchiveItem;
begin
  Result := FArchiveItems.Items[Index];
end;

procedure TArchiveReader.SetExitCode(Value: longint);
begin
  if ExitCode < Value then
    ExitCode := Value;
end;

procedure TArchiveReader.SetArchiveName(const Value: string);
begin
  OpenArchive(Value);
end;

procedure TArchiveReader.ReadCentralDirectory(aStream: TFileReader);
var
  Marker: longword;
  LocatorDisksNumber: longword;
  LocatorDiskNumber: longword;
  LocatorDiskSeek: longword;
  LocatorFlags: TArchiveLocatorFlags;
  BindingFlags: TArchiveBindingFlags;
begin
  // Read MagikSeek
  FArchiveReader.Seek(-SizeOf(longword), soFromEnd);
  FArchiveReader.Seek(-FArchiveReader.ReadDWord, soFromEnd);
  // Read Locator Marker
  if aStream.ReadInfWord = aitLocator then
    if aStream.ReadInfWord <= beexVersionNeededToRead then
    begin
      LocatorFlags := TArchiveLocatorFlags(longword(aStream.ReadInfWord));
      if (alfDisksNumber in LocatorFlags) then LocatorDisksNumber := aStream.ReadInfWord else LocatorDisksNumber := 1;
      if (alfDiskNumber  in LocatorFlags) then LocatorDiskNumber  := aStream.ReadInfWord else LocatorDiskNumber  := 1;
      LocatorDiskSeek := aStream.ReadInfWord;
      // Seek on CentralDirectory
      aStream.ImagesNumber := LocatorDisksNumber;
      aStream.SeekImage(LocatorDiskNumber, LocatorDiskSeek);
      if aStream.ReadDWord = beexArchiveMarker then
      begin
        while ExitCode < ccError do
        begin
          Marker := aStream.ReadInfWord;
          case Marker of
            aitItem: FArchiveItems.Add(TArchiveItem.Read(aStream));
            aitBinding: begin
              BindingFlags := TArchiveBindingFlags(longword(aStream.ReadInfWord));
              if (abfComment in BindingFlags) then FArchiveComment := aStream.ReadInfString else FArchiveComment := '';
            end;
            aitLocator: Break;
            aitEnd: Break;
            else DoFailure(cmArcTypeError);
          end;
        end;
      end else
        DoFailure(cmArcTypeError);

      if ExitCode < ccError then UnPackCentralDirectory;
    end else
      DoFailure(cmArcTypeError);
end;

procedure TArchiveReader.UnPackCentralDirectory;
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
      /// Item property ///
      if not(aifUncompressedSize  in CurrentItem.Flags) then CurrentItem.FUncompressedSize := PreviusItem.FUncompressedSize;
      if not(aifLastModifiedTime  in CurrentItem.Flags) then CurrentItem.FLastModifiedTime := PreviusItem.FLastModifiedTime;
      if not(aifAttributes        in CurrentItem.Flags) then CurrentItem.FAttributes       := PreviusItem.FAttributes;
      if not(aifComment           in CurrentItem.Flags) then CurrentItem.FComment          := PreviusItem.FComment;
      /// Data descryptor property ///
      if not(adfCompressedSize    in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCompressedSize := PreviusItem.FCompressedSize;
      if not(adfDiskNumber        in CurrentItem.FDataDescriptorFlags) then CurrentItem.FDiskNumber     := PreviusItem.FDiskNumber;
      if not(adfDiskSeek          in CurrentItem.FDataDescriptorFlags) then CurrentItem.FDiskSeek       := PreviusItem.FDiskSeek;
      if not(adfCRC32             in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCRC32          := PreviusItem.FCRC32;
      /// Compression property ///
      if not(acfCompressionMethod in CurrentItem.FCompressionFlags)    then CurrentItem.FCompressionMethod := PreviusItem.FCompressionMethod;
      if not(acfCompressionLevel  in CurrentItem.FCompressionFlags)    then CurrentItem.FCompressionLevel  := PreviusItem.FCompressionLevel;
      if not(acfDictionaryLevel   in CurrentItem.FCompressionFlags)    then CurrentItem.FDictionaryLevel   := PreviusItem.FDictionaryLevel;
      if not(acfCompressionTable  in CurrentItem.FCompressionFlags)    then CurrentItem.FCompressionTable  := PreviusItem.FCompressionTable;
      /// Encryption property ///
      if not(aefEncryptionMethod  in CurrentItem.FEncryptionFlags)     then CurrentItem.FEncryptionMethod := PreviusItem.FEncryptionMethod;

      PreviusItem :=  CurrentItem;
    end;
  end;
end;

procedure TArchiveReader.InitDecoder(Item: TArchiveItem);
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

procedure TArchiveReader.DecodeToSwap(Item: TArchiveItem);
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
    DoClear;

    if not FArchiveReader.IsValid then DoFailure(cmStrmReadError);
    if not FSwapWriter   .IsValid then DoFailure(cmStrmWriteError);

    if Item.FCRC32 <> CRC then
      DoFailure(Format(cmCrcError, [Item.FExternalFileName]));
  end else
    DoFailure(cmStrmWriteError);
end;

procedure TArchiveReader.DecodeToNil(Item: TArchiveItem);
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
    DoClear;

    if not FArchiveReader.IsValid then DoFailure(cmStrmReadError);
    if not Stream        .IsValid then DoFailure(cmStrmWriteError);

    if Item.FCRC32 <> CRC then
      DoFailure(Format(cmCrcError, [Item.FExternalFileName]));

    Stream.Destroy;
  end else
    DoFailure(cmStrmWriteError);
end;

procedure TArchiveReader.DecodeToFile(Item: TArchiveItem);
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
    DoClear;

    if not FArchiveReader.IsValid then DoFailure(cmStrmReadError);
    if not Stream        .IsValid then DoFailure(cmStrmWriteError);

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

function TArchiveReader.GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
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

function TArchiveReader.GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
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

function TArchiveReader.GetBackTear(Index: longint): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index downto 0 do
    if FArchiveItems.Items[Index].SolidCompression = FALSE then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiveReader.GetNextTear(Index: longint): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index to FArchiveItems.Count - 1 do
    if FArchiveItems.Items[Index].SolidCompression = FALSE then
    begin
      Result := I;
      Break;
    end;
end;

procedure TArchiveReader.OpenArchive(const aArchiveName: string);
begin
  CloseArchive;
  if FileExists(aArchiveName) then
  begin
    FArchiveReader := TFileReader.Create(aArchiveName);
    FArchiveReader.OnRequestImage := FOnRequestImage;
    if Assigned(FArchiveReader) then
    begin
      ReadCentralDirectory(FArchiveReader);
      if ExitCode < ccError then
      begin
        FArchiveName := aArchiveName;
        if FArchiveItems.Count = 0 then
          DoFailure(Format(cmArcTypeError, [aArchiveName]));
      end else
        DoFailure(Format(cmArcTypeError, [aArchiveName]));
    end else
      DoFailure(Format(cmOpenArcError, [aArchiveName]));
  end else
    FArchiveName := aArchiveName;
end;

procedure TArchiveReader.CloseArchive;
begin
  if Assigned(FArchiveReader) then FreeAndNil(FArchiveReader);
  if Assigned(FSwapReader)    then FreeAndNil(FSwapReader);
  if Assigned(FSwapWriter)    then FreeAndNil(FSwapWriter);

  FTotalSize     :=  0;
  FProcessedSize :=  0;
  FArchiveName   := '';
  FSwapName      := '';
  FSuspended     := FALSE;
  FArchiveItems.Clear;
end;

function TArchiveReader.Find(const aFileName: string): longint;
begin
  Result := FArchiveItems.Find(aFileName);
end;

procedure TArchiveReader.DoClear;
begin
  if Assigned(FOnClearProgress) then
    FOnClearProgress;
end;

function TArchiveReader.DoProgress(Value: longint): boolean;
begin
  Inc(FProcessedSize, Value);
  if Assigned(FOnProgress) then
    FOnProgress(MulDiv(100, FProcessedSize, FTotalSize));

  while FSuspended do Sleep(250);
  Result := ExitCode < ccError;
end;

procedure TArchiveReader.DoMessage(const Message: string);
begin
  if Assigned(FOnMessage) then FOnMessage(Message);
end;

procedure TArchiveReader.DoFailure(const ErrorMessage: string);
begin
  SetExitCode(ccError);
  if Assigned(FOnFailure) then
    FOnFailure(ErrorMessage);
end;

procedure TArchiveReader.DoRequestImage(ImageNumber: longint;
  var ImageName: string; var Abort: boolean);
begin
  Abort := True;
  if Assigned(FOnRequestImage) then
    FOnRequestImage(ImageNumber, ImageName, Abort);
end;

// TCustomArchiveReader class

procedure TCustomArchiveReader.Tag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitUpdate;
end;

procedure TCustomArchiveReader.UnTag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitNone;
end;

procedure TCustomArchiveReader.TagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do Tag(I);
end;

procedure TCustomArchiveReader.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do UnTag(I);
end;

function TCustomArchiveReader.IsTagged(Index: longint): boolean;
begin
  Result := FArchiveItems.Items[Index].FTag = aitUpdate;
end;

// TArchiveWriter class

constructor TArchiveWriter.Create;
begin
  inherited Create;
  FIsNeededToSave  := FALSE;
  FIsNeededToSwap  := FALSE;
  FThreshold       :=  0;
  FWorkDirectory   := '';
end;

procedure TArchiveWriter.WriteCentralDirectory(aStream: TFileWriter);
var
  I: longword;
  BindingFlags: TArchiveBindingFlags;
  LocatorFlags: TArchiveLocatorFlags;
  LocatorDisksNumber: longword;
  LocatorDiskNumber: longword;
  LocatorDiskSeek: int64;
  MagikSeek: int64;
begin
  LocatorFlags      := [];
  LocatorDiskSeek   := aStream.Position;
  LocatorDiskNumber := aStream.CurrentImage;
  if LocatorDiskNumber <> 1 then
    Include(LocatorFlags,  alfDiskNumber);

  PackCentralDirectory;
  aStream.WriteDWord(beexArchiveMarker);
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    aStream.WriteInfWord(aitItem);
    FArchiveItems.Items[I].Write(aStream);
  end;

  BindingFlags := [];
  if Length(FArchiveComment) > 0 then
    Include(BindingFlags,  abfComment);

  aStream.WriteInfWord(aitBinding);
  aStream.WriteInfWord(longword(BindingFlags));
  if (abfComment in BindingFlags) then
    aStream.WriteInfString(FArchiveComment);
  aStream.WriteInfWord(aitEnd);

  if aStream.Threshold > 0 then
    aStream.CreateImage;

  MagikSeek := aStream.Position;
  LocatorDisksNumber := aStream.CurrentImage;
  if LocatorDisksNumber <> 1 then
    Include(LocatorFlags,  alfDisksNumber);

  aStream.WriteInfWord(aitLocator);
  aStream.WriteInfWord(beexVersionneededToRead);
  aStream.WriteInfWord(longword(LocatorFlags));
  if (alfDisksNumber in LocatorFlags) then aStream.WriteInfWord(LocatorDisksNumber);
  if (alfDiskNumber  in LocatorFlags) then aStream.WriteInfWord(LocatorDiskNumber);
  aStream.WriteInfWord(LocatorDiskSeek);
  aStream.WriteInfWord(aitEnd);

  aStream.WriteDWord(longword(aStream.Position - MagikSeek + SizeOf(longword)));
end;

procedure TArchiveWriter.PackCentralDirectory;
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
      /// Item property ///
      Include(CurrentItem.FFlags, aifUncompressedSize);
      Include(CurrentItem.FFlags, aifLastModifiedTime);
      Include(CurrentItem.FFlags, aifAttributes);
      Include(CurrentItem.FFlags, aifComment);
      if CurrentItem.FUncompressedSize = PreviusItem.FUncompressedSize then Exclude(CurrentItem.FFlags, aifUncompressedSize);
      if CurrentItem.FLastModifiedTime = PreviusItem.FLastModifiedTime then Exclude(CurrentItem.FFlags, aifLastModifiedTime);
      if CurrentItem.FAttributes       = PreviusItem.FAttributes       then Exclude(CurrentItem.FFlags, aifAttributes);
      if CurrentItem.FComment          = PreviusItem.FComment          then Exclude(CurrentItem.FFlags, aifComment);
      /// Data descriptor property ///
      Include(CurrentItem.FDataDescriptorFlags, adfCompressedSize);
      Include(CurrentItem.FDataDescriptorFlags, adfDiskNumber);
      Include(CurrentItem.FDataDescriptorFlags, adfDiskSeek);
      Include(CurrentItem.FDataDescriptorFlags, adfCRC32);
      if CurrentItem.FCompressedSize = PreviusItem.FCompressedSize then Exclude(CurrentItem.FDataDescriptorFlags, adfCompressedSize);
      if CurrentItem.FDiskNumber     = PreviusItem.FDiskNumber     then Exclude(CurrentItem.FDataDescriptorFlags, adfDiskNumber);
      if CurrentItem.FDiskseek       = PreviusItem.FDiskSeek       then Exclude(CurrentItem.FDataDescriptorFlags, adfDiskSeek);
      if CurrentItem.FCRC32          = PreviusItem.FCRC32          then Exclude(CurrentItem.FDataDescriptorFlags, adfCRC32);
      /// Compression property ///
      Include(CurrentItem.FCompressionFlags, acfCompressionMethod);
      Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
      Include(CurrentItem.FCompressionFlags, acfDictionaryLevel);
    //Include(CurrentItem.FCompressionFlags, acfCompressionTable);
      if CurrentItem.FCompressionMethod = PreviusItem.FCompressionMethod then Exclude(CurrentItem.FCompressionFlags, acfCompressionMethod);
      if CurrentItem.FCompressionLevel  = PreviusItem.FCompressionLevel  then Exclude(CurrentItem.FCompressionFlags, acfCompressionLevel);
      if CurrentItem.FDictionaryLevel   = PreviusItem.FDictionaryLevel   then Exclude(CurrentItem.FCompressionFlags, acfDictionaryLevel);
    //if CurrentItem.FCompressionTable  = PreviusItem.FCompressionTable  then Exlude(CurrentItem.FCompressionFlags, acfCompressionTable);
      /// Encryption property ///
      Include(CurrentItem.FEncryptionFlags, aefEncryptionMethod);
      if CurrentItem.FEncryptionMethod  = PreviusItem.FEncryptionMethod  then Exclude(CurrentItem.FEncryptionFlags, aefEncryptionMethod);

      PreviusItem := CurrentItem;
    end;
  end;
end;

(*
function TBeeApp.CheckArchivePassword: longint;
var
  Item: THeader;
  Smaller, I: longint;
  Decoder: THeaderDecoder;
begin
  if (ArchiveExitCode < ccError) and (FHeaders.GetNext(0, foPassword) > -1) then
  begin
    // select smaller size item ...
    Smaller := 0;
    for I := 1 to FHeaders.Count - 1 do
    begin
      Item := FHeaders.Items[I];
      if (foTear in Item.Flags) and (Item.Size < FHeaders.Items[Smaller].Size)  then
      begin
        Smaller := I;
      end;
    end;
    Item := FHeaders.Items[Smaller];

    // test item ...
    DoMessage(Format(cmChecking, [Item.Name]));
    Decoder := THeaderDecoder.Create(FArchReader, DoTick);
    Decoder.Password := FCommandLine.pOption;

    for I := 0 to Smaller do
      Decoder.Initialize(FHeaders.Items[I]);

    if Decoder.ReadToNul(Item) = False then
      DoMessage(Format(cmTestPswError, [FCommandLine.ArchiveName]), ccError);

    Decoder.Destroy;
  end;
  Result := ArchiveExitCode;
end; *)

function TArchiveWriter.OpenSwap: longint;
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
      FSwapWriter.WriteDWord(beexArchiveMarker);

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
            if not FArchiveReader.IsValid then DoFailure(cmStrmReadError);
            if not FSwapWriter   .IsValid then DoFailure(cmStrmWriteError);
          end;
        end;
      FDecoder.Destroy;
      FreeAndNil(FSwapWriter);

      if ExitCode < ccError then
      begin
        FSwapReader := TFileReader.Create(FSwapName);
        if Assigned(FSwapReader) = False then
          DoFailure(cmOpenSwapError);
      end;
    end else
      DoFailure(cmCreateSwapError);
  end;
  Result := ExitCode;
end;

procedure TArchiveWriter.CloseArchive;
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
  FTempName       := '';
  inherited CloseArchive;
end;

procedure TArchiveWriter.InitEncoder(Item: TArchiveItem);
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

procedure TArchiveWriter.EncodeFromArchive(Item: TArchiveItem);
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
    DoClear;

    if not FArchiveReader.IsValid then DoFailure(cmStrmReadError);
    if not FTempWriter   .IsValid then DoFailure(cmStrmWriteError);
  end else
    DoFailure(cmStrmReadError);
end;

procedure TArchiveWriter.EncodeFromSwap(Item: TArchiveItem);
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
    DoClear;
    Item.FCompressedSize := FTempWriter.ABSPosition - ABSPosition;

    if not FSwapReader.IsValid then DoFailure(cmStrmReadError);
    if not FTempWriter.IsValid then DoFailure(cmStrmWriteError);
  end else
    DoFailure(cmStrmReadError);
end;

procedure TArchiveWriter.EncodeFromFile(Item: TArchiveItem);
var
  ABSPosition: int64;
  Stream: TFileReader;
begin
  Stream := TFileReader.Create(Item.FExternalFileName);
  if Stream <> nil then
  begin
    Item.FUncompressedSize := Item.FExternalFileSize;

    ABSPosition      := FTempWriter.ABSPosition;
    Item.FDiskSeek   := FTempWriter.Position;
    Item.FDiskNumber := FTempWriter.CurrentImage;
    case Item.CompressionMethod of
      actMain: FEncoder.Encode(Stream, Item.FUncompressedSize, Item.FCRC32);
      else     FEncoder.Copy  (Stream, Item.FUncompressedSize, Item.FCRC32);
    end;
    DoClear;
    Item.FCompressedSize := FTempWriter.ABSPosition - ABSPosition;

    if not Stream     .IsValid then DoFailure(cmStrmReadError);
    if not FTempWriter.IsValid then DoFailure(cmStrmWriteError);

    Stream.Destroy;
  end else
    DoFailure(Format(cmOpenFileError, [Item.FExternalFileName]));
end;

procedure TArchiveWriter.SetWorkDirectory(const Value: string);
begin
  FWorkDirectory := Value;
  if Length(FWorkDirectory) > 0 then
  begin
    FWorkDirectory := IncludeTrailingBackSlash(FWorkDirectory);
  end;
end;

// TCustomArchiveWriter class

procedure TCustomArchiveWriter.TagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do Tag(I);
end;

procedure TCustomArchiveWriter.Tag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitUpdate;
end;

procedure TCustomArchiveWriter.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do UnTag(I);
end;

procedure TCustomArchiveWriter.UnTag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitNone;
end;

// TArchiveExtractor class

constructor TArchiveExtractor.Create;
begin
  inherited Create;
  FIsNeededToExtract := FALSE;
end;

procedure TArchiveExtractor.DoExtract(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnExtract) then
  begin
    ExtractAs := Item.FileName;
    FOnExtract(Item, ExtractAs, Confirm);
  end;
end;

procedure TArchiveExtractor.CheckTags;
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
          arcQuit:  DoFailure(cmUserAbort);
        end;
      end;
    end;

  if (ExitCode < ccError) and FIsNeededToExtract then CheckSequences;
end;

procedure TArchiveExtractor.CheckSequences;
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
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    Item := FArchiveItems.Items[I];
    case Item.FTag of
    //aitNone:   nothing to do
      aitUpdate: Inc(FTotalSize, Item.UncompressedSize);
      aitDecode: Inc(FTotalSize, Item.UncompressedSize);
    end;
  end;
end;

procedure TArchiveExtractor.ExtractTagged;
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
        end;
      end;
    FDecoder.Destroy;
  end;
  CloseArchive;
end;

procedure TArchiveExtractor.TestTagged;
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
        Item.FExternalFileName := Item.FileName;
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
      end;
    end;
  FDecoder.Destroy;
  CloseArchive;
end;

// TArchiveRenamer class

procedure TArchiveRenamer.DoRename(Item: TArchiveItem;
  var RenameAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnRename) then
  begin
    RenameAs := Item.FileName;
    FOnRename(Item, RenameAs, Confirm);
  end;
end;

procedure TArchiveRenamer.CheckTags;
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
          arcQuit:  DoFailure(cmUserAbort);
        end;
      end; // if end
    end; // if end
end;

procedure TArchiveRenamer.RenameTagged;
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
    if Assigned(FTempWriter) then
    begin
      FTempWriter.OnRequestBlankDisk := FOnRequestBlankDisk;
      FTempWriter.WriteDWord(beexArchiveMarker);

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

        end;
      Encoder.Destroy;
      WriteCentralDirectory(FTempWriter);
      if not FTempWriter.IsValid then
        DoFailure(cmStrmWriteError);
    end else
      DoFailure(cmOpenTempError);
  end;
  CloseArchive;
end;

// TArchiveEraser class

procedure TArchiveEraser.DoErase(Item: TArchiveItem;
  var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnErase) then
  begin
    FOnErase(Item, Confirm);
  end;
end;

procedure TArchiveEraser.CheckTags;
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
          arcQuit:  DoFailure(cmUserAbort);
        end;
      end;
    end;

  if (ExitCode < ccError) and FIsNeededToSave then CheckSequences;
end;

procedure TArchiveEraser.CheckSequences;
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

procedure TArchiveEraser.EraseTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if (ExitCode < ccError) and FIsNeededToSave then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FThreshold);

    if Assigned(FTempWriter) then
    begin
      FTempWriter.OnRequestBlankDisk := FOnRequestBlankDisk;
      FTempWriter.WriteDWord(beexArchiveMarker);

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

          end;
        FEncoder.Destroy;
        if ExitCode < ccError then
        begin
          WriteCentralDirectory(FTempWriter);
          if not FTempWriter.IsValid then
            DoFailure(cmStrmWriteError);
        end;
      end;
    end else
      DoFailure(cmOpenTempError);
  end;
  CloseArchive;
end;

// TArchiveUpdater class

constructor TArchiveUpdater.Create;
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

destructor TArchiveUpdater.Destroy;
var
  I: longint;
begin
  for I := 0 to FSearchRecs.Count - 1 do
    TCustomSearchRec(FSearchRecs[I]).Destroy;
  FSearchRecs.Destroy;
  inherited Destroy;
end;

procedure TArchiveUpdater.DoUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnUpdate) then
  begin
    UpdateAs := SearchRec.Name;
    FOnUpdate(SearchRec, UpdateAs, Confirm);
  end;
end;

procedure TArchiveUpdater.Tag(SearchRec: TCustomSearchRec);
begin
  FSearchRecs.Add(TCustomSearchRec.CreateFrom(SearchRec));
end;

procedure TArchiveUpdater.ConfigureCrypter;
var
  I: longint;
  CurrentItem: TArchiveItem;
begin
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    CurrentItem := FArchiveItems.Items[I];
    if CurrentItem.FTag = aitAdd then
    begin
      Include(CurrentItem.FEncryptionFlags, aefEncryptionMethod);
      CurrentItem.FEncryptionMethod := FEncryptionMethod;
      //if CurrentItem.FEncryptionMethod = acrtMain then
      //begin
      //  nothing to do
      //end;
    end;
  end;
end;

procedure TArchiveUpdater.ConfigureCoder;
var
  I: longint;
  CurrentItem: TArchiveItem;
  CurrentTable: TTableParameters;
  CurrentFileExt: string;
  PreviousFileExt: string;
begin
  FConfiguration := TConfiguration.Create;
  if FileExists(FConfigurationName) then
    FConfiguration.LoadFromFile(FConfigurationName)
  else
    DoFailure(Format(cmConfigError, [FConfigurationName]));

  if ExitCode < ccError then
  begin
    CurrentFileExt := '.';
    FConfiguration.Selector('\main');
    FConfiguration.CurrentSection.Values['Method']     := IntToStr(Ord(FCompressionLevel));
    FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(Ord(FDictionaryLevel));
    FConfiguration.Selector('\m' + FConfiguration.CurrentSection.Values['Method']);

    for I := 0 to FArchiveItems.Count - 1 do
    begin
      CurrentItem := FArchiveItems.Items[I];
      if CurrentItem.FTag = aitAdd then
      begin
        Include(CurrentItem.FCompressionFlags, acfCompressionMethod);
        Exclude(CurrentItem.FCompressionFlags, acfCompressionLevel);
        Exclude(CurrentItem.FCompressionFlags, acfDictionaryLevel);
        Exclude(CurrentItem.FCompressionFlags, acfSolidCompression);
        Exclude(CurrentItem.FCompressionFlags, acfCompressionTable);

        CurrentItem.FCompressionMethod := FCompressionMethod;
        if CurrentItem.FCompressionMethod = actMain then
        begin
          Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
          Include(CurrentItem.FCompressionFlags, acfDictionaryLevel);

          CurrentItem.FCompressionLevel := FCompressionLevel;
          CurrentItem.FDictionaryLevel  := FDictionaryLevel;

          PreviousFileExt := CurrentFileExt;
          if FForceFileExtension = '' then
            CurrentFileExt := ExtractFileExt(CurrentItem.FExternalFileName)
          else
            CurrentFileExt := FForceFileExtension;

          if FConfiguration.GetTable(CurrentFileExt, CurrentTable) then
            CurrentItem.FCompressionTable := CurrentTable
          else
            CurrentItem.FCompressionTable := DefaultTableParameters;

          if AnsiCompareFileName(CurrentFileExt, PreviousFileExt) = 0 then
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

function CompareCustomSearchRec(Item1, Item2: pointer): longint;
begin
  Result := AnsiCompareFileName(
    ExtractFileExt(TCustomSearchRec(Item1).Name),
    ExtractFileExt(TCustomSearchRec(Item2).Name));

  if Result = 0 then
  begin
    if TCustomSearchRec(Item1).Size < TCustomSearchRec(Item2).Size then
      Result := -1
    else
      Result :=  1;
  end;
end;

procedure TArchiveUpdater.CheckTags;
var
  I, J: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  Csr: TCustomSearchRec;
  UpdateAs: string;
begin
  FSearchRecs.Sort(@CompareCustomSearchRec);
  for J := 0 to FSearchRecs.Count - 1 do
    if ExitCode < ccError then
    begin
      Csr := TCustomSearchRec(FSearchRecs.Items[J]);
      DoUpdate(Csr, UpdateAs, Confirm);
      case Confirm of
        arcOk:
        begin
          I := Find(UpdateAs);
          if I = -1 then
          begin
            Item           := TArchiveItem.Create;
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
        arcQuit: ExitCode := ccUserAbort;
      end;
    end;

  if (ExitCode < ccError) and FIsNeededToSave then CheckSequences;
end;

procedure TArchiveUpdater.CheckSequences;
var
  Item: TArchiveItem;
  I, J, BackTear, NextTear: longint;
begin
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

procedure TArchiveUpdater.UpdateTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if (ExitCode < ccError) and  FIsNeededToSave then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FThreshold);
    if Assigned(FTempWriter) then
    begin
      FTempWriter.OnRequestBlankDisk := FOnRequestBlankDisk;
      FTempWriter.WriteDWord(beexArchiveMarker);
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

          end;
        FEncoder.Destroy;
        if (ExitCode < ccError) then
        begin
          WriteCentralDirectory(FTempWriter);
          if not FTempWriter.IsValid then
            DoFailure(cmStrmWriteError);
        end;
      end;
    end;
  end;
  CloseArchive;
end;

procedure TArchiveUpdater.SetConfigurationName(const Value: string);
begin
  FConfigurationName := Value;
end;

procedure TArchiveUpdater.SetForceFileExtension(const Value: string);
begin
  FForceFileExtension := Value;
end;

end.

