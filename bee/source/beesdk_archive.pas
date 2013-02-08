unit BeeSDK_Archive;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  Bee_Files,
  Bee_Common,
  Bee_MainPacker,
  Bee_CommandLine,
  Bee_Configuration,
  BeeLib_Configuration;

const
  /// beex archive marker
  beexArchiveMarker = $1A656542;

  /// beex archive version
  beexVersionNeededToRead    = $50;
  beexVersionNeededToExtract = $50;

  /// archive item type
  aitItem    = $01;
  aitBinding = $02;
  aitLocator = $7E;
  aitEnd     = $7F;

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
    aifVersionNeededToExtract,
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
    acfCompressionBlock,
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
    FVersionNeededToExtract: longword;
    FUncompressedSize: int64;
    FLastModifiedTime: longint;
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
    FCompressionLevel: TclOption;
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
  public {methods}
    constructor Create(const aFileName: string);
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    procedure Update(SearchRec: TCustomSearchRec);
  public {property}
    // Item property
    property FileName: string read FFileName;
    property Flags: TArchiveItemFlags read FFlags;
    property VersionNeededToExtract: longword read FVersionNeededToExtract;
    property UncompressedSize: int64 read FUncompressedSize;
    property LastModifiedTime: longint read FLastModifiedTime;
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
    property CompressionLevel: TclOption read FCompressionLevel;
    property DictionaryLevel: TdOption read FDictionaryLevel;
    property SolidCompression: boolean read GetSolidCompression;
    property CompressionTable: TTableParameters read FCompressionTable;
    // Encryption property
    property EncryptionFlags: TArchiveEncryptionFlags read FEncryptionFlags;
    property EncryptionMethod: TArchiveEncryptionMethod read FEncryptionMethod;
  end;

  TArchiveItems = class(TObject)
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
    function Add(Item : TArchiveItem): longint;
    procedure Delete(Index: longint);
    procedure Clear;
    function Find(const FileName: string): longint;
  public {properties}
    property Count: longint read GetCount;
    property Items[Index: longint]: TArchiveItem read GetItem;
  end;

  TArchiveMessageEvent = procedure(const aMessage: string) of object;

  TArchiveConfirm = (arcOk, arcCancel, arcQuit);

  TArchiveRenameEvent = procedure(Item: TArchiveItem;
    var RenameAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveExtractEvent = procedure(Item: TArchiveItem;
    var ExtractAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveDeleteEvent = procedure(Item: TArchiveItem;
    var Confirm: TArchiveConfirm) of object;

  TArchiveUpdateEvent = procedure(SearchRec: TCustomSearchRec;
    var UpdateAs: string; var Confirm: TArchiveConfirm) of object;

  // a class for each command

  TArchiver = class(TObject)
  private
    FSuspended: boolean;
    FIsNeededToRun: boolean;
    FIsNeededToSwap: boolean;
    FIsNeededToSave: boolean;
    FProcessedSize: int64;
    FTotalSize: int64;

    FSearchRecs: TList;

    FConfigurationName: string;
    FCompressionMethod: TArchiveCompressionMethod;
    FCompressionLevel: TclOption;
    FCompressionBlock: int64;
    FDictionaryLevel: TdOption;
    FForceFileExtension: string;
    FEncryptionMethod: TArchiveEncryptionMethod;

    FArchiveName: string;
    FArchiveComment: string;
    FArchivePassword: string;
    FArchiveReader: TFileReader;
    FArchiveItems: TArchiveItems;

    FArchiveSFX: string;
    FArchiveSFXMod: TMemoryStream;


    FSwapName: string;
    FSwapReader: TFileReader;
    FSwapWriter: TFileWriter;

    FTempName: string;
    FTempWriter: TFileWriter;

    FWorkDirectory: string;
    FTestTempArchive: boolean;
    FThreshold: int64;

  private
    FEncoder: THeaderEncoder;
    procedure InitEncoder      (Item: TArchiveItem);
    procedure EncodeFromArchive(Item: TArchiveItem);
    procedure EncodeFromSwap   (Item: TArchiveItem);
    procedure EncodeFromFile   (Item: TArchiveItem);
  private
    FDecoder: THeaderDecoder;
    procedure InitDecoder      (Item: TArchiveItem);
    procedure DecodeToNul      (Item: TArchiveItem);
    procedure DecodeToSwap     (Item: TArchiveItem);
    procedure DecodeToFile     (Item: TArchiveItem);
  private
    procedure WriteCentralDirectory(aStream: TFileWriter);
    procedure ReadCentralDirectory(aStream: TFileReader);
    procedure PackCentralDirectory;
    procedure UnPackCentralDirectory;

    procedure OpenSwap;
    procedure TestTemporaryArchive;
    procedure SaveTemporaryArchive;
  private
    procedure SetArchiveName(const Value: string);
    procedure SetArchiveSFX(const Value: string);
    procedure SetWorkDirectory(const Value: string);

    function GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetBackTear(Index: longint): longint;
    function GetNextTear(Index: longint): longint;
    function GetItem(Index: longint): TArchiveItem;
    function GetCount: longint;

  private
    FOnRequestBlankImage: TFileWriterRequestBlankImageEvent;
    procedure DoRequestBlankImage(ImageNumber: longint;
     var Abort : Boolean);

  private
    FOnRequestImage: TFileReaderRequestImageEvent;
    procedure DoRequestImage(ImageNumber: longint;
      var ImageName: string; var Abort: boolean);

  private
    FOnMessage: TArchiveMessageEvent;
    procedure DoMessage(const Message: string);

  private
    FOnProgress: TArchiveProgressEvent;
    procedure DoProgress(Value: longint);

  private
    FOnExtract: TArchiveExtractEvent;
    procedure CheckTags4Test;
    procedure CheckTags4Extract;
    procedure CheckSequences4Extract;
    procedure DoExtract(Item: TArchiveItem;
      var ExtractAs: string; var Confirm: TArchiveConfirm);

  private
    FOnRename: TArchiveRenameEvent;
    procedure CheckTags4Rename;
    procedure CheckSequences4Rename;
    procedure DoRename(Item: TArchiveItem;
      var RenameAs: string; var Confirm: TArchiveConfirm);

  private
    FOnDelete: TArchiveDeleteEvent;
    procedure CheckTags4Delete;
    procedure CheckSequences4Delete;
    procedure DoDelete(Item: TArchiveItem;
      var Confirm: TArchiveConfirm);

  private
    FOnUpdate: TArchiveUpdateEvent;
    procedure CheckTags4Update;
    procedure CheckSequences4Update;
    procedure ConfigureCrypter;
    procedure ConfigureCoder;
    procedure DoUpdate(SearchRec: TCustomSearchRec;
      var UpdateAs: string; var Confirm: TArchiveConfirm);

  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenArchive(const aArchiveName: string);
    procedure CloseArchive;

    procedure Terminate;
    procedure Suspend(Value: boolean);

    procedure UnTagAll;
    procedure UnTag(Index: longint);
    procedure TagAll;
    procedure Tag(Index: longint); overload;
    procedure Tag(SearchRec: TCustomSearchRec); overload;
    function IsTagged(Index: longint): boolean;
    function Find(const aFileName: string): longint;

    procedure TestTagged;
    procedure ExtractTagged;
    procedure RenameTagged;
    procedure DeleteTagged;
    procedure UpdateTagged;

  public
    property ArchiveName: string
      read FArchiveName write SetArchiveName;
    property ArchiveComment: string
      read FArchiveComment write FArchiveComment;
    property ArchivePassword: string
      read FArchivePassword write FArchivePassword;
    property ArchiveSFX: string
      read FArchiveSFX write SetArchiveSFX;

    property CompressionMethod: TArchiveCompressionMethod
      read FCompressionMethod write FCompressionMethod;
    property CompressionLevel: TclOption
      read FCompressionLevel write FCompressionLevel;
    property DictionaryLevel: TdOption
      read FDictionaryLevel  write FDictionaryLevel;
    property CompressionBlock: int64
      read FCompressionBlock write FCompressionBlock;
    property EncrypionMethod: TArchiveEncryptionMethod
      read FEncryptionMethod write FEncryptionMethod;
    property ConfigurationName: string
      read FConfigurationName write FConfigurationName;
    property ForceFileExtension: string
      read FForceFileExtension write FForceFileExtension;

    property WorkDirectory: string
      read FWorkDirectory write SetWorkDirectory;
    property TestTempArchive: boolean
      read FTestTempArchive write FTestTempArchive;
    property Threshold: int64 read FThreshold write FThreshold;

    property OnExtract: TArchiveExtractEvent
      read FOnExtract write FOnExtract;
    property OnRename: TArchiveRenameEvent
      read FOnRename write FOnRename;
    property OnDelete: TArchiveDeleteEvent
      read FOnDelete write FOnDelete;
    property OnUpdate: TArchiveUpdateEvent
      read FOnUpdate write FOnUpdate;

    property OnRequestBlankImage: TFileWriterRequestBlankImageEvent
      read FOnRequestBlankImage write FOnRequestBlankImage;
    property OnRequestImage: TFileReaderRequestImageEvent
      read FOnRequestImage write FOnRequestImage;
    property OnProgress: TArchiveProgressEvent
      read FOnProgress write FOnProgress;
    property OnMessage: TArchiveMessageEvent
      read FOnMessage write FOnMessage;

    property Items[Index: longint]: TArchiveItem read GetItem;
    property Count: longint read GetCount;
  end;

function CompressionMethodToStr(Item: TArchiveItem): string;
function VersionToStr(Version: longword): string;
function RatioToStr(const PackedSize, Size: int64): string;
function SizeToStr(const Size: int64): string;
function AttrToStr(Attr: longint): string;

implementation

uses
  Bee_Assembler,
  Bee_Interface;

function CompressionMethodToStr(Item: TArchiveItem): string;
begin
  Result := 'm0a';
  if Item.CompressionMethod <> actNone then
  begin
    if Item.SolidCompression then
    begin
      Result[1] := 's';
    end;
    Result[2] := char(byte('0') + Ord(Item.CompressionLevel));
    Result[3] := char(byte('a') + Ord(Item.DictionaryLevel ));
  end;
end;

function VersionToStr(Version: longword): string;
begin
  case Version of
    80:  Result := '0.8.0';
    else Result := '?.?.?';
  end;
end;

function RatioToStr(const PackedSize, Size: int64): string;
begin
  if Size > 0 then
    Result := Format('%u%%', [Round((PackedSize / Size) * 100)])
  else
    Result := Format('%u%%', [100]);
end;

function SizeToStr(const Size: int64): string;
begin
  if Size > 0 then
    Result := Format('%u', [Size])
  else
    Result := Format('%u', [0]);
end;

function AttrToStr(Attr: longint): string;
begin
  Result := 'RHSVDAL';
  if Attr and faReadOnly  = 0 then Result[1] := '.';
  if Attr and faHidden    = 0 then Result[2] := '.';
  if Attr and faSysFile   = 0 then Result[3] := '.';
  if Attr and faVolumeId  = 0 then Result[4] := '.';
  if Attr and faDirectory = 0 then Result[5] := '.';
  if Attr and faArchive   = 0 then Result[6] := '.';
  if Attr and faSymLink   = 0 then Result[7] := '.';
end;

// TArchiveItem class

constructor TArchiveItem.Create(const aFileName: string);
begin
  inherited Create;
  FFileName := aFileName;
  /// Item property ///
  FFlags := [
    aifVersionNeededToExtract,
    aifUncompressedSize,
    aifLastModifiedTime,
    aifAttributes,
    aifComment];
  FVersionNeededToExtract :=  beexVersionNeededToExtract;
  FUncompressedSize       :=  0;
  FLastModifiedTime       :=  0;
  FAttributes             :=  0;
  FComment                := '';
  /// Data descriptor property ///
  FDataDescriptorFlags := [
    adfCompressedSize,
    adfDiskNumber,
    adfDiskSeek,
    adfCRC32];
  FCompressedSize    :=  0;
  FDiskNumber        :=  1;
  FDiskSeek          :=  0;
  FCRC32             :=  0;
  /// Compression property ///
  FCompressionFlags  := [];
  FCompressionMethod := actNone;
  FCompressionLevel  := moFast;
  FDictionaryLevel   := do2MB;
  FCompressionTable  := DefaultTableParameters;
  /// Encryption property ///
  FEncryptionFlags   := [];
  FEncryptionMethod  := acrtNone;
  /// Reserved property ///
  FTag               := aitAdd;
  FPosition          := -1;
  FExternalFileName  := '';
  FExternalFileSize  :=  0;
end;

procedure TArchiveItem.Update(SearchRec: TCustomSearchRec);
begin
  /// Item property ///
  FLastModifiedTime := SearchRec.LastModifiedTime;
  FAttributes       := SearchRec.Attributes;
  /// Reserved property ///
  FExternalFileName := SearchRec.Name;
  FExternalFileSize := SearchRec.Size;
end;

constructor TArchiveItem.Read(Stream: TFileReader);
begin
  FFileName := Stream.ReadInfString;
  /// Item property ///
  FFlags    := TArchiveItemFlags(longword(Stream.ReadInfWord));
  if (aifVersionNeededToExtract in FFlags) then FVersionNeededToExtract := Stream.ReadInfWord;
  if (aifUncompressedSize       in FFlags) then FUncompressedSize       := Stream.ReadInfWord;
  if (aifLastModifiedTime       in FFlags) then FLastModifiedTime       := Stream.ReadInfWord;
  if (aifAttributes             in FFlags) then FAttributes             := Stream.ReadInfWord;
  if (aifComment                in FFlags) then FComment                := Stream.ReadInfString;
  /// Data descryptor property ///
  FDataDescriptorFlags := TArchiveDataDescriptorFlags(longword(Stream.ReadInfWord));
  if (adfCompressedSize    in FDataDescriptorFlags) then FCompressedSize := Stream.ReadInfWord;
  if (adfDiskNumber        in FDataDescriptorFlags) then FDiskNumber     := Stream.ReadInfWord;
  if (adfDiskSeek          in FDataDescriptorFlags) then FDiskSeek       := Stream.ReadInfWord;
  if (adfCRC32             in FDataDescriptorFlags) then FCRC32          := Stream.ReadInfWord;
  /// Compression property ///
  FCompressionFlags := TArchiveCompressionFlags(longword(Stream.ReadInfWord));
  if (acfCompressionMethod in FCompressionFlags)    then FCompressionMethod := TArchiveCompressionMethod(Stream.ReadInfWord);
  if (acfCompressionLevel  in FCompressionFlags)    then FCompressionLevel  := TclOption(Stream.ReadInfWord);
  if (acfDictionaryLevel   in FCompressionFlags)    then FDictionaryLevel   := TdOption(Stream.ReadInfWord);
  if (acfCompressionTable  in FCompressionFlags)    then Stream.Read(@FCompressionTable, SizeOf(TTableParameters));
  /// Encryption property ///
  FEncryptionFlags := TArchiveEncryptionFlags(longword(Stream.ReadInfWord));
  if (aefEncryptionMethod  in FEncryptionFlags)     then FEncryptionMethod := TArchiveEncryptionMethod(Stream.ReadInfWord);
end;

procedure TArchiveItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfString(FFileName);
  /// Item property ///
  Stream.WriteInfWord(longword(FFlags));
  if (aifVersionNeededToExtract  in FFlags) then Stream.WriteInfWord(FVersionNeededToExtract);
  if (aifUncompressedSize        in FFlags) then Stream.WriteInfWord(FUncompressedSize);
  if (aifLastModifiedTime        in FFlags) then Stream.WriteInfWord(FLastModifiedTime);
  if (aifAttributes              in FFlags) then Stream.WriteInfWord(FAttributes);
  if (aifComment                 in FFlags) then Stream.WriteInfString(FComment);
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
  if (acfDictionaryLevel   in FCompressionFlags)    then Stream.WriteInfWord(Ord(FDictionaryLevel));
  if (acfCompressionTable  in FCompressionFlags)    then Stream.Write(@FCompressionTable, SizeOf(TTableParameters));
  /// Encryption property ///
  Stream.WriteInfWord(longword(FEncryptionFlags));
  if (aefEncryptionMethod  in FEncryptionFlags)     then Stream.WriteInfWord(Ord(FEncryptionMethod));
end;

function TArchiveItem.GetSolidCompression: boolean;
begin
  Result := acfCompressionBlock in FCompressionFlags;
end;

// TArchiveItems class

constructor TArchiveItems.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FNames := TList.Create;
end;

destructor TArchiveItems.Destroy;
begin
  Clear;
  FItems.Destroy;
  FNames.Destroy;
  inherited Destroy;
end;

procedure TArchiveItems.Clear;
var
  I: longint;
begin
  for I := 0 to FItems.Count - 1 do
    TArchiveItem(FItems[I]).Destroy;
  FItems.Clear;
  FNames.Clear;
end;

function TArchiveItems.Add(Item: TArchiveItem): longint;
var
  Lo, Med, Hi, I: longint;
begin
  Item.FPosition := FItems.Add(Item);
  if FNames.Count > 0 then
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
      SetExitStatus(esUnknowError);
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

   Result := Item.FPosition;
end;

function TArchiveItems.GetNameIndex(const FileName: string): longint;
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
  Result := -1;
  if Hi = -2 then
    Result := Med;
end;

function TArchiveItems.Find(const FileName: string): longint;
begin
  Result := GetNameIndex(FileName);
  if Result <> -1 then
  begin
    Result := TArchiveItem(FNames[Result]).FPosition;
  end;
end;

procedure TArchiveItems.Delete(Index: longint);
var
  I: longint;
  Item, Next: TArchiveItem;
begin
  Item := Items[Index];
  if Index < FItems.Count - 1 then
  begin
    Next := Items[Index + 1];

    if(not(acfCompressionBlock in Item.CompressionFlags)) and
      (   (acfCompressionBlock in Next.CompressionFlags)) then
      Exclude(Next.FCompressionFlags, acfCompressionBlock);

    if(   (acfCompressionTable in Item.CompressionFlags)) and
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

function TArchiveItems.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TArchiveItems.GetItem(Index: longint): TArchiveItem;
begin
  Result := TArchiveItem(FItems[Index]);
end;

// TArchiver class

constructor TArchiver.Create;
begin
  inherited Create;
  Randomize;

  FSuspended       := FALSE;
  FIsNeededToRun   := FALSE;
  FIsNeededToSwap  := FALSE;
  FIsNeededToSave  := FALSE;

  FSearchRecs         := TList.Create;
  FConfigurationName  := SelfPath + DefaultCfgName;
  FCompressionMethod  := actNone;
  FCompressionLevel   := moFast;
  FCompressionBlock   := 0;
  FDictionaryLevel    := do2MB;
  FForceFileExtension :=  '';
  FEncryptionMethod   := acrtNone;

  FArchiveName     := '';
  FArchiveComment  := '';
  FArchivePassword := '';
  FArchiveReader   := nil;
  FArchiveItems    := TArchiveItems.Create;

  FSwapName        := '';
  FSwapReader      := nil;
  FSwapWriter      := nil;

  FTempName        := '';
  FTempWriter      := nil;

  FWorkDirectory   := '';
  FTestTempArchive := FALSE;
  FThreshold       := 0;
end;

destructor TArchiver.Destroy;
var
  I: longint;
begin
  FArchiveItems.Destroy;
  for I := 0 to FSearchRecs.Count - 1 do
    TCustomSearchRec(FSearchRecs[I]).Destroy;
  FSearchRecs.Destroy;
  inherited Destroy;
end;

// TArchiver # ENCODE/DECODE #

procedure TArchiver.InitEncoder(Item: TArchiveItem);
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

procedure TArchiver.EncodeFromArchive(Item: TArchiveItem);
var
  NulCRC: longword;
begin
  FArchiveReader.Seek(Item.FDiskNumber, Item.FDiskSeek);

  Item.FDiskNumber := FTempWriter.CurrentImage;
  Item.FDiskSeek   := FTempWriter.SeekFromCurrent;
  begin
    FEncoder.Copy(FArchiveReader, Item.FCompressedSize, NulCRC);
  end;
end;

procedure TArchiver.EncodeFromSwap(Item: TArchiveItem);
begin
  FSwapReader.Seek(Item.FDiskNumber, Item.FDiskSeek);

  Item.FDiskNumber := FTempWriter.CurrentImage;
  Item.FDiskSeek   := FTempWriter.SeekFromCurrent;
  case Item.FCompressionMethod of
    actMain: FEncoder.Encode(FSwapReader, Item.FUncompressedSize, Item.FCRC32);
    else     FEncoder.Copy  (FSwapReader, Item.FUncompressedSize, Item.FCRC32);
  end;
  Item.FCompressedSize := FTempWriter.SeekFromCurrent - Item.FDiskSeek;
end;

procedure TArchiver.EncodeFromFile(Item: TArchiveItem);
var
  Stream: TFileReader;
begin
  Stream := TFileReader.Create(Item.FExternalFileName, FOnRequestImage);

  Item.FUncompressedSize := Item.FExternalFileSize;

  Item.FDiskNumber := FTempWriter.CurrentImage;
  Item.FDiskSeek   := FTempWriter.SeekFromCurrent;
  case Item.CompressionMethod of
    actMain: FEncoder.Encode(Stream, Item.FUncompressedSize, Item.FCRC32);
    else     FEncoder.Copy  (Stream, Item.FUncompressedSize, Item.FCRC32);
  end;
  Item.FCompressedSize := FTempWriter.SeekFromCurrent - Item.FDiskSeek;

  Stream.Destroy;
end;

procedure TArchiver.InitDecoder(Item: TArchiveItem);
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

procedure TArchiver.DecodeToSwap(Item: TArchiveItem);
var
  CRC: longword;
begin
  FArchiveReader.Seek(Item.DiskNumber, Item.DiskSeek);

  Item.FDiskNumber := FSwapWriter.CurrentImage;
  Item.FDiskSeek   := FSwapWriter.SeekFromCurrent;
  case Item.CompressionMethod of
    actMain: FDecoder.Decode(FSwapWriter, Item.FUncompressedSize, CRC);
    else     FDecoder.Copy  (FSwapWriter, Item.FUncompressedSize, CRC);
  end;

  if Item.FCRC32 <> CRC then
    SetExitStatus(esCrcError);
end;

procedure TArchiver.DecodeToNul(Item: TArchiveItem);
var
  CRC: longword;
  Stream: TFileWriter;
begin
  Stream := TNulWriter.Create;

  FArchiveReader.Seek(Item.FDiskNumber, Item.FDiskSeek);
  case Item.CompressionMethod of
    actMain: FDecoder.Decode(Stream, Item.FUncompressedSize, CRC);
    else     FDecoder.Copy  (Stream, Item.FUncompressedSize, CRC);
  end;
  Stream.Destroy;

  if Item.FCRC32 <> CRC then
    SetExitStatus(esCrcError);
end;

procedure TArchiver.DecodeToFile(Item: TArchiveItem);
var
  CRC: longword;
  Stream: TFileWriter;
begin
  Stream := TFileWriter.Create(Item.FExternalFileName, FOnRequestBlankImage, 0);

  FArchiveReader.Seek(Item.FDiskNumber, Item.FDiskSeek);
  case Item.CompressionMethod of
    actMain: FDecoder.Decode(Stream, Item.FUncompressedSize, CRC);
    else     FDecoder.Copy  (Stream, Item.FUncompressedSize, CRC);
  end;
  Stream.Destroy;
  if Item.FCRC32 <> CRC then
    SetExitStatus(esCrcError);

  if ExitStatus = esNoError then
  begin
    FileSetAttr(Item.FExternalFileName, Item.FAttributes);
    FileSetDate(Item.FExternalFileName, Item.FLastModifiedTime);
  end;
end;

// TArchiver # READ/WRITE CENTRAL DIRECTORY #

procedure TArchiver.WriteCentralDirectory(aStream: TFileWriter);
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
  LocatorDiskSeek   := aStream.SeekFromCurrent;
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
    Include(BindingFlags, abfComment);

  aStream.WriteInfWord(aitBinding);
  aStream.WriteInfWord(longword(BindingFlags));
  if (abfComment in BindingFlags) then
    aStream.WriteInfString(FArchiveComment);
  aStream.WriteInfWord(aitEnd);

  if aStream.Threshold > 0 then
    if (aStream.Threshold - aStream.SeekFromCurrent) < 512 then
    begin
      aStream.CreateNewImage;
    end;

  MagikSeek := aStream.SeekFromCurrent;
  LocatorDisksNumber := aStream.CurrentImage;
  if LocatorDisksNumber <> 1 then
    Include(LocatorFlags, alfDisksNumber);

  aStream.WriteInfWord(aitLocator);
  aStream.WriteInfWord(beexVersionNeededToRead);
  aStream.WriteInfWord(longword(LocatorFlags));
  if (alfDisksNumber in LocatorFlags) then aStream.WriteInfWord(LocatorDisksNumber);
  if (alfDiskNumber  in LocatorFlags) then aStream.WriteInfWord(LocatorDiskNumber);
  aStream.WriteInfWord(LocatorDiskSeek);
  aStream.WriteInfWord(aitEnd);

  aStream.WriteDWord(beexArchiveMarker);
  aStream.WriteDWord(longword(aStream.SeekFromCurrent - MagikSeek + SizeOf(longword)));
end;

procedure TArchiver.ReadCentralDirectory(aStream: TFileReader);
var
  Check: boolean;
  Marker: longword;
  LocatorDisksNumber: longword;
  LocatorDiskNumber: longword;
  LocatorDiskSeek: longword;
  LocatorFlags: TArchiveLocatorFlags;
  BindingFlags: TArchiveBindingFlags;
begin
  Check := FALSE;
  // Read Marker
  aStream.SeekFromEnd(-2*SizeOf(longword));
  if aStream.ReadDWord = beexArchiveMarker then
  begin
    // Read MagikSeek
    aStream.SeekFromEnd(-aStream.ReadDWord);
    // Read Locator Marker
    if aStream.ReadInfWord = aitLocator then
      if aStream.ReadInfWord <= beexVersionNeededToRead then
      begin
        LocatorFlags := TArchiveLocatorFlags(longword(aStream.ReadInfWord));
        if (alfDisksNumber in LocatorFlags) then LocatorDisksNumber := aStream.ReadInfWord else LocatorDisksNumber := 1;
        if (alfDiskNumber  in LocatorFlags) then LocatorDiskNumber  := aStream.ReadInfWord else LocatorDiskNumber  := 1;
        LocatorDiskSeek := aStream.ReadInfWord;
        // Read CentralDirectory
        aStream.ImagesNumber := LocatorDisksNumber;
        aStream.ImageNumber  := LocatorDiskNumber;
        aStream.SeekFromBeginning(LocatorDiskSeek);
        if aStream.ReadDWord = beexArchiveMarker then
          repeat
            Marker := aStream.ReadInfWord;
            case Marker of
              aitItem:    FArchiveItems.Add(TArchiveItem.Read(aStream));
              aitBinding: begin
                BindingFlags := TArchiveBindingFlags(longword(aStream.ReadInfWord));
                if (abfComment in BindingFlags) then FArchiveComment := aStream.ReadInfString;
              end;
              aitEnd: Check := TRUE;
              else    Break;
            end;
          until Marker = aitEnd;
      end;
  end;

  if Check = FALSE then
    SetExitStatus(esArchiveTypeError)
  else
    UnPackCentralDirectory;
end;

procedure TArchiver.PackCentralDirectory;
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
      if CurrentItem.FVersionNeededToExtract = PreviusItem.FVersionNeededToExtract then Exclude(CurrentItem.FFlags, aifVersionNeededToExtract) else Include(CurrentItem.FFlags, aifVersionNeededToExtract);
      if CurrentItem.FUncompressedSize       = PreviusItem.FUncompressedSize       then Exclude(CurrentItem.FFlags, aifUncompressedSize)       else Include(CurrentItem.FFlags, aifUncompressedSize);
      if CurrentItem.FLastModifiedTime       = PreviusItem.FLastModifiedTime       then Exclude(CurrentItem.FFlags, aifLastModifiedTime)       else Include(CurrentItem.FFlags, aifLastModifiedTime);
      if CurrentItem.FAttributes             = PreviusItem.FAttributes             then Exclude(CurrentItem.FFlags, aifAttributes)             else Include(CurrentItem.FFlags, aifAttributes);
      if CurrentItem.FComment                = PreviusItem.FComment                then Exclude(CurrentItem.FFlags, aifComment)                else Include(CurrentItem.FFlags, aifComment);
      /// Data descriptor property ///
      if CurrentItem.FCompressedSize   = PreviusItem.FCompressedSize then Exclude(CurrentItem.FDataDescriptorFlags, adfCompressedSize) else Include(CurrentItem.FDataDescriptorFlags, adfCompressedSize);
      if CurrentItem.FDiskNumber       = PreviusItem.FDiskNumber     then Exclude(CurrentItem.FDataDescriptorFlags, adfDiskNumber)     else Include(CurrentItem.FDataDescriptorFlags, adfDiskNumber);
      if CurrentItem.FDiskseek         = PreviusItem.FDiskSeek       then Exclude(CurrentItem.FDataDescriptorFlags, adfDiskSeek)       else Include(CurrentItem.FDataDescriptorFlags, adfDiskSeek);
      if CurrentItem.FCRC32            = PreviusItem.FCRC32          then Exclude(CurrentItem.FDataDescriptorFlags, adfCRC32)          else Include(CurrentItem.FDataDescriptorFlags, adfCRC32);
      /// Compression property ///
      if CurrentItem.FCompressionMethod = PreviusItem.FCompressionMethod then Exclude(CurrentItem.FCompressionFlags, acfCompressionMethod) else Include(CurrentItem.FCompressionFlags, acfCompressionMethod);
      if CurrentItem.FCompressionLevel  = PreviusItem.FCompressionLevel  then Exclude(CurrentItem.FCompressionFlags, acfCompressionLevel)  else Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
      if CurrentItem.FDictionaryLevel   = PreviusItem.FDictionaryLevel   then Exclude(CurrentItem.FCompressionFlags, acfDictionaryLevel)   else Include(CurrentItem.FCompressionFlags, acfDictionaryLevel);
      /// Encryption property ///
      if CurrentItem.FEncryptionMethod  = PreviusItem.FEncryptionMethod  then Exclude(CurrentItem.FEncryptionFlags, aefEncryptionMethod)   else Include(CurrentItem.FEncryptionFlags, aefEncryptionMethod);                  ;

      PreviusItem := CurrentItem;
    end;
  end;
end;

procedure TArchiver.UnPackCentralDirectory;
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
      if not(aifVersionNeededToExtract in CurrentItem.Flags) then CurrentItem.FVersionNeededToExtract := PreviusItem.FVersionNeededToExtract;
      if not(aifUncompressedSize       in CurrentItem.Flags) then CurrentItem.FUncompressedSize       := PreviusItem.FUncompressedSize;
      if not(aifLastModifiedTime       in CurrentItem.Flags) then CurrentItem.FLastModifiedTime       := PreviusItem.FLastModifiedTime;
      if not(aifAttributes             in CurrentItem.Flags) then CurrentItem.FAttributes             := PreviusItem.FAttributes;
      if not(aifComment                in CurrentItem.Flags) then CurrentItem.FComment                := PreviusItem.FComment;
      /// Data descryptor property ///
      if not(adfCompressedSize in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCompressedSize := PreviusItem.FCompressedSize;
      if not(adfDiskNumber     in CurrentItem.FDataDescriptorFlags) then CurrentItem.FDiskNumber     := PreviusItem.FDiskNumber;
      if not(adfDiskSeek       in CurrentItem.FDataDescriptorFlags) then CurrentItem.FDiskSeek       := PreviusItem.FDiskSeek;
      if not(adfCRC32          in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCRC32          := PreviusItem.FCRC32;
      /// Compression property ///
      if not(acfCompressionMethod in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionMethod := PreviusItem.FCompressionMethod;
      if not(acfCompressionLevel  in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionLevel  := PreviusItem.FCompressionLevel;
      if not(acfDictionaryLevel   in CurrentItem.FCompressionFlags) then CurrentItem.FDictionaryLevel   := PreviusItem.FDictionaryLevel;
      if not(acfCompressionTable  in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionTable  := PreviusItem.FCompressionTable;
      /// Encryption property ///
      if not(aefEncryptionMethod in CurrentItem.FEncryptionFlags) then CurrentItem.FEncryptionMethod := PreviusItem.FEncryptionMethod;

      PreviusItem := CurrentItem;
    end;
  end;
end;

// TArchiver # OPEN/CLOSE ARCHIVE #

procedure TArchiver.OpenArchive(const aArchiveName: string);
begin
  CloseArchive;
  if FileExists(aArchiveName) then
  begin
    DoMessage(Format(cmOpening, [aArchiveName]));
    FArchiveReader := TFileReader.Create(aArchiveName, FOnRequestImage);
    ReadCentralDirectory(FArchiveReader);
    if ExitStatus = esNoError then
    begin
      FArchiveName := aArchiveName;
      if FArchiveItems.Count = 0 then
        SetExitStatus(esArchiveTypeError);
    end;

  end else
  begin
    DoMessage(Format(cmCreating, [aArchiveName]));
    FArchiveName := aArchiveName;
  end;
end;

procedure TArchiver.OpenSwap;
var
  I: longint;
  CRC: longword;
  Item: TArchiveItem;
begin
  if ExitStatus = esNoError then
  begin
    FSwapName   := GenerateFileName(FWorkDirectory);
    FSwapWriter := TFileWriter.Create(FSwapName, FOnRequestBlankImage, 0);
    FSwapWriter.WriteDWord(beexArchiveMarker);

    FDecoder := THeaderDecoder.Create(FArchiveReader);
    FDecoder.OnProgress := DoProgress;
    for I := 0 to FArchiveItems.Count - 1 do
      if ExitStatus = esNoError then
      begin
        Item := FArchiveItems.Items[I];
        if Item.FTag in [aitDecode, aitDecodeAndUpdate] then
        begin
          case Item.FTag of
            aitDecode:          DoMessage(Format(cmSwapping, [Item.FFileName]));
            aitDecodeAndUpdate: DoMessage(Format(cmDecoding, [Item.FFileName]));
          end;

          InitDecoder(Item);
          case Item.FTag of
            aitDecode:          DecodeToSwap(Item);
            aitDecodeAndUpdate: DecodeToNul (Item);
          end;
        end;
      end;
    FreeAndNil(FDecoder);
    FreeAndNil(FSwapWriter);

    if ExitStatus = esNoError then
      FSwapReader := TFileReader.Create(FSwapName, FOnRequestImage);
  end;
end;

procedure TArchiver.TestTemporaryArchive;
var
  Tester: TArchiver;
begin
  if ExitStatus = esNoError then
  begin
    Tester := TArchiver.Create;
    Tester.OnRequestBlankImage := OnRequestBlankImage;
    Tester.OnRequestImage      := OnRequestImage;

    Tester.OnProgress          := OnProgress;
    Tester.OnMessage           := OnMessage;

    Tester.ArchivePassword := FArchivePassword;

    Tester.OpenArchive(FTempName);
    if ExitStatus = esNoError then
    begin
      Tester.TagAll;
      Tester.TestTagged;
    end;
    Tester.Destroy;
  end;
end;

procedure TArchiver.SaveTemporaryArchive;
var
  I: longint;
  Item: TArchiveItem;
begin
  SysUtils.DeleteFile(FSwapName);
  if ExitStatus = esNoError then
  begin
    if FThreshold > 0 then
    begin
      FTotalSize     := 0;
      FProcessedSize := 0;
      for I := 0 to FArchiveItems.Count - 1 do
      begin
        FArchiveItems.Items[I].FTag := aitUpdate;
        Inc(FTotalSize, FArchiveItems.Items[I].CompressedSize);
      end;

      FArchiveReader := TFileReader.Create(FTempName, FOnRequestImage);
      FTempWriter    := TFileWriter.Create(FArchiveName, FOnRequestBlankImage, FThreshold);
      FTempWriter.WriteDWord(beexArchiveMarker);

      FEncoder := THeaderEncoder.Create(FTempWriter);
      FEncoder.OnProgress := DoProgress;
      for I := 0 to FArchiveItems.Count - 1 do
        if ExitStatus = esNoError then
        begin
          Item := FArchiveItems.Items[I];

          case Item.FTag of
            aitUpdate: DoMessage(Format(cmSplitting, [Item.FileName]));
          end;
          EncodeFromArchive(Item);
        end;
      FreeAndNil(FEncoder);
      WriteCentralDirectory(FTempWriter);

      FreeAndNil(FTempWriter);
      FreeAndNil(FArchiveReader);
    end else
    begin
      SysUtils.DeleteFile(FArchiveName);
      if ExtractFilePath(FArchiveName) <> '' then
        ForceDirectories(ExtractFilePath(FArchiveName));
      if RenameFile(FTempName, FArchiveName) = FALSE then
        SetExitStatus(esRenameTempError);
    end;
  end;

  // ---
  if ExitStatus in [esNoError, esUserAbortError] then
    SysUtils.DeleteFile(FTempName);
end;

procedure TArchiver.CloseArchive;
begin
  if Assigned(FArchiveReader) then FreeAndNil(FArchiveReader);
  if Assigned(FSwapReader)    then FreeAndNil(FSwapReader);
  if Assigned(FSwapWriter)    then FreeAndNil(FSwapWriter);
  if Assigned(FTempWriter)    then FreeAndNil(FTempWriter);

  if FIsNeededToSave then
  begin
    if FTestTempArchive then
      TestTemporaryArchive;
    SaveTemporaryArchive;
  end;

  FSearchRecs.Clear;

  FArchiveName     := '';
  FArchiveComment  := '';
  FArchivePassword := '';
  FArchiveItems.Clear;

  FSwapName        := '';
  FTempName        := '';
end;

// TArchiver # FIND #

function TArchiver.Find(const aFileName: string): longint;
begin
  Result := FArchiveItems.Find(aFileName);
end;

// TArchiver # SUSPEND/TERMINATE #

procedure TArchiver.Terminate;
begin
  SetExitStatus(esUserAbortError);
  Suspend(FALSE);
end;

procedure TArchiver.Suspend(Value: boolean);
begin
  FSuspended := Value;
end;

// TArchiver # GET/SET #

function TArchiver.GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index downto 0 do
    if FArchiveItems.Items[I].FTag = aTag then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiver.GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index to FArchiveItems.Count - 1 do
    if FArchiveItems.Items[I].FTag = aTag then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiver.GetBackTear(Index: longint): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index downto 0 do
  begin
    if FArchiveItems.Items[I].SolidCompression = FALSE then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TArchiver.GetNextTear(Index: longint): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index to FArchiveItems.Count - 1 do
    if FArchiveItems.Items[I].SolidCompression = FALSE then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiver.GetCount: longint;
begin
  Result := FArchiveItems.Count;
end;

function TArchiver.GetItem(Index: longint): TArchiveItem;
begin
  Result := FArchiveItems.Items[Index];
end;

procedure TArchiver.SetArchiveName(const Value: string);
begin
  OpenArchive(Value);
end;

procedure TArchiver.SetArchiveSFX(const Value: string);
begin
  FArchiveSFX := Value;
end;

procedure TArchiver.SetWorkDirectory(const Value: string);
begin
  FWorkDirectory := Value;
  if FWorkDirectory <> '' then
  begin
    FWorkDirectory := IncludeTrailingBackSlash(FWorkDirectory);
  end;
end;

// TArchiver # DO EVENT #

procedure TArchiver.DoRequestBlankImage(ImageNumber: longint;
 var Abort : Boolean);
begin
  Abort := TRUE;
  if Assigned(FOnRequestImage) then
    FOnRequestBlankImage(ImageNumber, Abort);
end;

procedure TArchiver.DoRequestImage(ImageNumber: longint;
  var ImageName: string; var Abort: boolean);
begin
  Abort := TRUE;
  if Assigned(FOnRequestImage) then
    FOnRequestImage(ImageNumber, ImageName, Abort);
end;

procedure TArchiver.DoMessage(const Message: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Message);
end;

procedure TArchiver.DoProgress(Value: longint);
begin
  Inc(FProcessedSize, Value);
  if (FProcessedSize and $FFFF) = 0 then
  begin
    if Assigned(FOnProgress) then
      FOnProgress(Round((FProcessedSize/FTotalSize)*100));
  end;

  while FSuspended do Sleep(250);
end;

procedure TArchiver.DoExtract(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnExtract) then
  begin
    ExtractAs := Item.FileName;
    FOnExtract(Item, ExtractAs, Confirm);
  end;
end;

procedure TArchiver.DoRename(Item: TArchiveItem;
  var RenameAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnRename) then
  begin
    RenameAs := Item.FileName;
    FOnRename(Item, RenameAs, Confirm);
  end;
end;

procedure TArchiver.DoDelete(Item: TArchiveItem;
  var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnDelete) then
    FOnDelete(Item, Confirm);
end;

procedure TArchiver.DoUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs: string; var Confirm: TArchiveConfirm);
begin
  Confirm := arcCancel;
  if Assigned(FOnUpdate) then
  begin
    UpdateAs := SearchRec.Name;
    FOnUpdate(SearchRec, UpdateAs, Confirm);
  end;
end;

// TArchiveWriter class
(*
function TBeeApp.CheckArchivePassword: longint;
var
  Item: THeader;
  Smaller, I: longint;
  Decoder: THeaderDecoder;
begin
  if (ExitStatus = esNoError) and (FHeaders.GetNext(0, foPassword) > -1) then
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
  Result := ExitStatus;
end;
*)

// TArchiver # TAG #

procedure TArchiver.TagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do Tag(I);
end;

procedure TArchiver.Tag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitUpdate;
end;

procedure TArchiver.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FArchiveItems.Count - 1 do UnTag(I);
end;

procedure TArchiver.UnTag(Index: longint);
begin
  FArchiveItems.Items[Index].FTag := aitNone;
end;

procedure TArchiver.Tag(SearchRec: TCustomSearchRec);
begin
  FSearchRecs.Add(TCustomSearchRec.CreateFrom(SearchRec));
end;

function TArchiver.IsTagged(Index: longint): boolean;
begin
  Result := FArchiveItems.Items[Index].FTag = aitUpdate;
end;

// TArchiver # EXTRACT #

procedure TArchiver.CheckTags4Test;
var
  I: longint;
begin
  FSuspended       := FALSE;
  FIsNeededToRun   := FALSE;
  FIsNeededToSwap  := FALSE;
  FIsNeededToSave  := FALSE;

  for I := 0 to FArchiveItems.Count - 1 do
    if ExitStatus = esNoError then
      if FArchiveItems.Items[I].FTag = aitUpdate then
      begin
        FIsNeededToRun := TRUE;
        Break;
      end;
end;

procedure TArchiver.CheckTags4Extract;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  ExtractAs: string;
begin
  FSuspended       := FALSE;
  FIsNeededToRun   := FALSE;
  FIsNeededToSwap  := FALSE;
  FIsNeededToSave  := FALSE;

  for I := 0 to FArchiveItems.Count - 1 do
    if ExitStatus = esNoError then
    begin
      Item := FArchiveItems.Items[I];
      if Item.FTag = aitUpdate then
      begin
        repeat
          DoExtract(Item, ExtractAs, Confirm);
        until (Confirm <> arcOk) or (FileNameIsValid(ExtractAs));

        case Confirm of
          arcOk: begin
            FIsNeededToRun         := TRUE;
            Item.FExternalFileName := ExtractAs;
          end;
          arcCancel: Item.FTag:= aitNone;
          arcQuit: SetExitStatus(esUserAbortError);
        end;
      end;
    end;
end;

procedure TArchiver.CheckSequences4Extract;
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
          aitNone: Item.FTag := aitDecode;
        end;
      end;
      I := BackTear;
    end;
    I := GetBackTag(I - 1, aitUpdate);
  end;

  // STEP2: calculate bytes to process ...
  FTotalSize     := 0;
  FProcessedSize := 0;
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    Item := FArchiveItems.Items[I];
    case Item.FTag of
      aitUpdate: Inc(FTotalSize, Item.UncompressedSize);
      aitDecode: Inc(FTotalSize, Item.UncompressedSize);
    end;
  end;
end;

procedure TArchiver.ExtractTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags4Extract;
  if FIsNeededToRun then
  begin
    if ExitStatus = esNoError then
    begin
      CheckSequences4Extract;
      FDecoder := THeaderDecoder.Create(FArchiveReader);
      FDecoder.OnProgress := DoProgress;
      for I := 0 to FArchiveItems.Count - 1 do
        if ExitStatus = esNoError then
        begin
          Item := FArchiveItems.Items[I];
          InitDecoder(Item);
          case Item.FTag of
            aitUpdate: begin
                DoMessage(Format(cmExtracting, [Item.FExternalFileName]));
                DecodeToFile(Item);
              end;
            aitDecode: begin
                DoMessage(Format(cmDecoding, [Item.FExternalFileName]));
                DecodeToNul(Item);
              end;
            else SetExitStatus(esCaseError);
          end;
        end;
      FreeAndNil(FDecoder);
    end;
    UnTagAll;
  end;
end;

procedure TArchiver.TestTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags4Test;
  if FIsNeededToRun then
  begin
    if ExitStatus = esNoError then
    begin
      CheckSequences4Extract;
      FDecoder := THeaderDecoder.Create(FArchiveReader);
      FDecoder.OnProgress := DoProgress;
      for I := 0 to FArchiveItems.Count - 1 do
        if ExitStatus = esNoError then
        begin
          Item := FArchiveItems.Items[I];
          InitDecoder(Item);
          case Item.FTag of
            aitUpdate: begin
              DoMessage(Format(cmTesting, [Item.FileName]));
              DecodeToNul(Item);
            end;
            aitDecode: begin
              DoMessage(Format(cmDecoding, [Item.FileName]));
              DecodeToNul(Item);
            end;
            else SetExitStatus(esCaseError);
          end;
        end;
      FreeAndNil(FDecoder);
    end;
    UnTagAll;
  end;
end;

// TArchiver # RENAME #

procedure TArchiver.CheckTags4Rename;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  RemaneAs: string;
begin
  FSuspended       := FALSE;
  FIsNeededToRun   := FALSE;
  FIsNeededToSwap  := FALSE;
  FIsNeededToSave  := FALSE;

  for I := 0 to FArchiveItems.Count - 1 do
    if ExitStatus = esNoError then
    begin
      Item := FArchiveItems.Items[I];
      if Item.FTag in [aitUpdate] then
      begin
        repeat
          DoRename(Item, RemaneAs, Confirm);
        until (Confirm <> arcOk) or (FArchiveItems.GetNameIndex(RemaneAs) = -1);

        case Confirm of
          arcOk: begin
            FIsneededToRun  := TRUE;
            FIsNeededToSave := TRUE;
            Item.FFileName  := RemaneAs;
          end;
          arcCancel: Item.FTag := aitNone;
          arcQuit: SetExitStatus(esUserAbortError);
        end;
      end;
    end;
end;

procedure TArchiver.CheckSequences4Rename;
var
  I: longint;
begin
  // STEP2: calculate bytes to process ...
  FTotalSize     := 0;
  FProcessedSize := 0;
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    Inc(FTotalSize, FArchiveItems.Items[I].CompressedSize);
  end;
end;

procedure TArchiver.RenameTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags4Rename;
  if FIsNeededToRun then
  begin
    if ExitStatus = esNoError then
    begin
      FTempName   := GenerateFileName(FWorkDirectory);
      FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
      FTempWriter.WriteDWord(beexArchiveMarker);

      FEncoder := THeaderEncoder.Create(FTempWriter);
      FEncoder.OnProgress := DoProgress;
      for I := 0 to FArchiveItems.Count - 1 do
        if ExitStatus = esNoError then
        begin
          Item := FArchiveItems.Items[I];
          case Item.FTag of
            aitNone:   DoMessage(Format(cmCopying,  [Item.FileName]));
            aitUpdate: DoMessage(Format(cmRenaming, [Item.FileName]));
            else SetExitStatus(esCaseError);
          end;
          EncodeFromArchive(Item);
        end;
      FreeandNil(FEncoder);
      if ExitStatus = esNoError then
        WriteCentralDirectory(FTempWriter);
    end;
    UnTagAll;
  end;
end;

// TArchiver # DELETE #

procedure TArchiver.CheckTags4Delete;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
begin
  FSuspended       := FALSE;
  FIsNeededToRun   := FALSE;
  FIsNeededToSwap  := FALSE;
  FIsNeededToSave  := FALSE;

  for I := 0 to FArchiveItems.Count - 1 do
    if ExitStatus = esNoError then
    begin
      Item := FArchiveItems.Items[I];
      if Item.FTag in [aitUpdate] then
      begin
        DoDelete(Item, Confirm);
        case Confirm of
          arcOk: begin
            FIsNeededToRun  := TRUE;
            FIsNeededToSave := TRUE;
          end;
          arcCancel: Item.FTag := aitNone;
          arcQuit: SetExitStatus(esUserAbortError);
        end;
      end;
    end;
end;

procedure TArchiver.CheckSequences4Delete;
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
  FTotalSize     := 0;
  FProcessedSize := 0;
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

procedure TArchiver.DeleteTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags4Delete;
  if FIsNeededToRun then
  begin
    if ExitStatus = esNoError then
    begin
      CheckSequences4Delete;
      FTempName   := GenerateFileName(FWorkDirectory);
      FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
      FTempWriter.WriteDWord(beexArchiveMarker);

      if FIsNeededToSwap then OpenSwap;
      if ExitStatus = esNoError then
      begin
        for I := FArchiveItems.Count - 1 downto 0 do
        begin
          Item := FArchiveItems.Items[I];
          case Item.FTag of
            aitUpdate: begin
              DoMessage(Format(cmDeleting, [Item.FileName]));
              FArchiveItems.Delete(I);
            end;
            aitDecodeAndUpdate: begin
              DoMessage(Format(cmDeleting, [Item.FileName]));
              FArchiveItems.Delete(I);
            end;
          end;
        end;

        FEncoder := THeaderEncoder.Create(FTempWriter);
        FEncoder.OnProgress := DoProgress;
        for I := 0 to FArchiveItems.Count - 1 do
          if ExitStatus = esNoError then
          begin
            Item := FArchiveItems.Items[I];
            InitDecoder(Item);
            case Item.FTag of
              aitNone: begin
                DoMessage(Format(cmCopying, [Item.FileName]));
                EncodeFromArchive(Item);
              end;
              aitDecode: begin
                DoMessage(Format(cmEncoding, [Item.FileName]));
                EncodeFromSwap(Item);
              end
              else SetExitStatus(esCaseError);
            end;
          end;
        FreeandNil(FEncoder);
        if ExitStatus = esNoError then
          WriteCentralDirectory(FTempWriter);
      end;
    end;
    UnTagAll;
  end;
end;

// TArchiver # UPDATE #

function CompareCustomSearchRec(Item1, Item2: pointer): longint;
begin
  Result := AnsiCompareFileName(
    ExtractFileExt(TCustomSearchRec(Item1).Name),
    ExtractFileExt(TCustomSearchRec(Item2).Name));

  if Result = 0 then
  begin
    if TCustomSearchRec(Item1).Size < TCustomSearchRec(Item2).Size then Result := -1
    else if TCustomSearchRec(Item1).Size > TCustomSearchRec(Item2).Size then Result :=  1;
  end;
end;

procedure TArchiver.ConfigureCrypter;
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

procedure TArchiver.ConfigureCoder;
var
  I: longint;
  SolidBlock: int64;
  CurrentItem: TArchiveItem;
  CurrentTable: TTableParameters;
  CurrentFileExt: string;
  PreviousFileExt: string;
  Configuration: TConfiguration;
begin
  Configuration := TConfiguration.Create;
  if FileExists(FConfigurationName) then
    Configuration.LoadFromFile(FConfigurationName)
  else
    SetExitStatus(esLoadConfigError);

  if ExitStatus = esNoError then
  begin
    CurrentFileExt := '.';
    Configuration.Selector('\main');
    Configuration.CurrentSection.Values['Method']     := IntToStr(Ord(FCompressionLevel));
    Configuration.CurrentSection.Values['Dictionary'] := IntToStr(Ord(FDictionaryLevel));
    Configuration.Selector('\m' + Configuration.CurrentSection.Values['Method']);

    SolidBlock := FCompressionBlock;
    for I := 0 to FArchiveItems.Count - 1 do
    begin
      CurrentItem := FArchiveItems.Items[I];
      if CurrentItem.FTag = aitAdd then
      begin
        CurrentItem.FVersionNeededToExtract := beexVersionNeededToExtract;

        Include(CurrentItem.FCompressionFlags, acfCompressionMethod);
        Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
        Include(CurrentItem.FCompressionFlags, acfDictionaryLevel);
        Exclude(CurrentItem.FCompressionFlags, acfCompressionBlock);
        Exclude(CurrentItem.FCompressionFlags, acfCompressionTable);

        CurrentItem.FCompressionMethod := FCompressionMethod;
        CurrentItem.FCompressionLevel  := FCompressionLevel;
        CurrentItem.FDictionaryLevel   := FDictionaryLevel;
        if CurrentItem.FCompressionMethod = actMain then
        begin
          PreviousFileExt := CurrentFileExt;
          if FForceFileExtension = '' then
            CurrentFileExt := ExtractFileExt(CurrentItem.FExternalFileName)
          else
            CurrentFileExt := FForceFileExtension;

          if Configuration.GetTable(CurrentFileExt, CurrentTable) then
            CurrentItem.FCompressionTable := CurrentTable
          else
            CurrentItem.FCompressionTable := DefaultTableParameters;

          if AnsiCompareFileName(CurrentFileExt, PreviousFileExt) = 0 then
          begin
            Dec(SolidBlock, CurrentItem.UncompressedSize);
            if SolidBlock >= 0 then
              Include(CurrentItem.FCompressionFlags, acfCompressionBlock)
            else
              SolidBlock := CompressionBlock;
          end else
          begin
            Include(CurrentItem.FCompressionFlags, acfCompressionTable);
            SolidBlock := FCompressionBlock;
          end;

        end;
      end;
    end;
  end;
  FreeAndNil(Configuration);
end;

procedure TArchiver.CheckTags4Update;
var
  I, J: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  Csr: TCustomSearchRec;
  UpdateAs: string;
begin
  FSuspended       := FALSE;
  FIsNeededToRun   := FALSE;
  FIsNeededToSwap  := FALSE;
  FIsNeededToSave  := FALSE;

  FSearchRecs.Sort(@CompareCustomSearchRec);
  for J := 0 to FSearchRecs.Count - 1 do
    if ExitStatus = esNoError then
    begin
      Csr := TCustomSearchRec(FSearchRecs.Items[J]);
      DoUpdate(Csr, UpdateAs, Confirm);
      case Confirm of
        arcOk: begin
          I := Find(UpdateAs);
          if I = -1 then
          begin
            Item := FArchiveItems.Items[FArchiveItems.Add(
              TArchiveItem.Create(UpdateAs))];
          end else
          begin
            Item := FArchiveItems.Items[I];
            if Item.FTag = aitNone then
              Item.FTag := aitUpdate;
          end;
          Item.Update(Csr);
          FIsNeededToRun  := TRUE;
          FIsNeededToSave := TRUE;
        end;
      //arcCancel: nothing to do
        arcQuit: SetExitStatus(esUserAbortError);
      end;
    end;

  for J := 0 to FSearchRecs.Count - 1 do
    TCustomSearchRec(FSearchRecs[J]).Destroy;
  FSearchRecs.Clear;
  ConfigureCrypter;
  ConfigureCoder;
end;

procedure TArchiver.CheckSequences4Update;
var
  Item: TArchiveItem;
  I, J, BackTear, NextTear: longint;
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
  FTotalSize     := 0;
  FProcessedSize := 0;
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

procedure TArchiver.UpdateTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags4Update;
  if FIsNeededToRun then
  begin
    if ExitStatus = esNoError then
    begin
      CheckSequences4Update;
      FTempName   := GenerateFileName(FWorkDirectory);
      FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
      FTempWriter.WriteDWord(beexArchiveMarker);

      if FIsNeededToSwap then OpenSwap;
      if ExitStatus = esNoError then
      begin
        FEncoder := THeaderEncoder.Create(FTempWriter);
        FEncoder.OnProgress := DoProgress;
        for I := 0 to FArchiveItems.Count - 1 do
          if ExitStatus = esNoError then
          begin
            Item := FArchiveItems.Items[I];
            InitEncoder(Item);
            case Item.FTag of
              aitNone: begin
                DoMessage(Format(cmCopying, [Item.FileName]));
                EncodeFromArchive(Item);
              end;
              aitAdd: begin
                DoMessage(Format(cmAdding, [Item.FileName]));
                EncodeFromFile(Item);
              end;
              aitUpdate: begin
                DoMessage(Format(cmUpdating, [Item.FileName]));
                EncodeFromFile(Item);
              end;
              aitDecode: begin
                DoMessage(Format(cmEncoding, [Item.FileName]));
                EncodeFromSwap(Item);
              end;
              aitDecodeAndUpdate: begin
                DoMessage(Format(cmUpdating, [Item.FileName]));
                EncodeFromFile(Item);
              end;
            end;
          end;
        FreeAndNil(FEncoder);
        if ExitStatus = esNoError then
          WriteCentralDirectory(FTempWriter);
      end;
    end;
    UnTagAll;
  end;
end;

end.

