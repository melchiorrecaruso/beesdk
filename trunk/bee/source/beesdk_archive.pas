{
  Copyright (c) 2013 Melchiorre Caruso.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

   TArchiver, main class.

  Modifyed:

    v0.8.0 build 1895 - 2013.05.26 by Melchiorre Caruso.
}

unit BeeSDK_Archive;

{$I bee_compiler.inc}

interface

uses
  Math,
  Classes,
  SysUtils,
  DateUtils,

  Bee_Crc,
  Bee_BlowFish,
  Bee_Interface,
  Bee_MainPacker,

  Bee_Files,
  Bee_BufStream,
  Bee_Common,
  Bee_CommandLine,
  Bee_Configuration;

const
  /// archive markers
  ARCHIVE_MARKER                      = $78454542;
  ARCHIVE_CENTRALDIR_MARKER           = $78454542;
  ARCHIVE_CENTRALDIR_ITEM_MARKER      = $78454542;
  ARCHIVE_CENTRALDIR_SEEK_MARKER      = $78454542;
  ARCHIVE_CENTRALDIR_MAGIKSEEK_MARKER = $78454542;

type
  /// archive central directory flag
  TArchiveCentralDirectoryFlag = (
    acdfVersionNeededToRead,
    acdfLastModifiedTime,
    acdfComment);

  TArchiveCentralDirectoryFlags = set of TArchiveCentralDirectoryFlag;

  /// archive central directory seek flag
  TArchiveCentralDirectorySeekFlag = (
    acdsfVersionNeededToRead,
    acdsfDisksNumber,
    acdsfDiskNumber);

  TArchiveCentralDirectorySeekFlags = set of TArchiveCentralDirectorySeekFlag;

  /// archive item flag
  TArchiveItemFlag = (
    aifVersionNeededToRead,
    aifUncompressedSize,
    aifLastModifiedTime,
    aifLastStoredTime,
    aifAttributes,
    aifComment,
    aifLowLayer,
    aifHighLayer);

  TArchiveItemFlags = set of TArchiveItemFlag;

  /// archive data descriptor flag
  TArchiveDataDescriptorFlag = (
    addfCompressedSize,
    addfDiskNumber,
    addfDiskSeek,
    addfCheckMethod,
    addfCheckDigest,
    addfCheckMethodAux,
    addfCheckDigestAux);

  TArchiveDataDescriptorFlags = set of TArchiveDataDescriptorFlag;

  /// archive compression flag
  TArchiveCompressionFlag = (
    acfCompressionMethod,
    acfCompressionLevel,
    acfCompressionLevelAux,
    acfCompressionFilter,
    acfCompressionFilterAux,
    acfCompressionBlock);

  TArchiveCompressionFlags = set of TArchiveCompressionFlag;

  /// archive encryption flag
  TArchiveEncryptionFlag = (
    aefEncryptionMethod);

  TArchiveEncryptionFlags = set of TArchiveEncryptionFlag;

  /// archive item tag
  TArchiveItemTag = (aitNone, aitAdd, aitUpdate, aitDecode, aitDecAndUpd);

  /// archive item
  TArchiveItem = class(TObject)
  protected
    // item property
    FFileName: string;
    FFlags: TArchiveItemFlags;
    FVersionNeededToRead: longint;
    FUncompressedSize: int64;
    FLastModifiedTime: int64;
    FLastStoredTime: int64;
    FAttributes: longint;
    FComment: string;
    FLowLayer: longint;
    FHighLayer: longint;
    // data descriptor property
    FDataDescriptorFlags: TArchiveDataDescriptorFlags;
    FCompressedSize: int64;
    FDiskNumber: longint;
    FDiskSeek: int64;
    FCheckMethod: longword;
    FCheckDigest: string;
    FCheckMethodAux: longword;
    FCheckDigestAux: string;
    // compression property
    FCompressionFlags: TArchiveCompressionFlags;
    FCompressionMethod: longint;
    FCompressionLevel: longint;
    FCompressionLevelAux: longint;
    FCompressionFilter: string;
    FCompressionFilterAux: string;
    FCompressionBlock: int64;
    // encryption property
    FEncryptionFlags: TArchiveEncryptionFlags;
    FEncryptionMethod: longword;
  protected
    FIndex: longint;
    FTag: TArchiveItemTag;
    FExternalFileName: string;
    FExternalFileSize: int64;
  public {methods}
    constructor Create(const aFileName: string);
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    procedure Update(SearchRec: TCustomSearchRec);
  public {property}
    property FileName: string read FFileName;
    property Flags: TArchiveItemFlags read FFlags;
    property VersionNeededToRead: longint read FVersionNeededToRead;
    property UncompressedSize: int64 read FUncompressedSize;
    property LastModifiedTime: int64 read FLastModifiedTime;
    property LastStoredTime: int64 read FLastStoredTime;
    property Attributes: longint read FAttributes;
    property Comment: string read FComment;
    property LowLayer: longint read FLowLayer;
    property HighLayer: longint read FHighLayer;
    // data descriptor property
    property DadaDescriptorFlags: TArchiveDataDescriptorFlags read FDataDescriptorFlags;
    property CompressedSize: int64 read FCompressedSize;
    property DiskNumber: longint read FDiskNumber;
    property DiskSeek: int64 read FDiskSeek;
    property CheckMethod: longword read FCheckMethod;
    property CheckDigest: string read FCheckDigest;
    property CheckMethodAux: longword read FCheckMethodAux;
    property CheckDigestAux: string read FCheckDigestAux;
    // compression property
    property CompressionFlags: TArchiveCompressionFlags read FCompressionFlags;
    property CompressionMethod: longint read FCompressionMethod;
    property CompressionLevel: longint read FCompressionLevel;
    property CompressionLevelAux: longint read FCompressionLevelAux;
    property CompressionFilter: string read FCompressionFilter;
    property CompressionFilterAux: string read FCompressionFilterAux;
    property CompressionBlock: int64 read FCompressionBlock;
    // encryption property
    property EncryptionFlags: TArchiveEncryptionFlags read FEncryptionFlags;
    property EncryptionMethod: longword read FEncryptionMethod;
    //
    property Index: longint read FIndex;
  end;

  /// archive central directory
  TArchiveCentralDirectory = class(TObject)
  private
    FItems: TList;
    FItemsAux: TList;
    // central directory property
    FFlags: TArchiveCentralDirectoryFlags;
    FLastModifiedTime: int64;
    FComment: string;
    // central directory seek property
    FSeekFlags: TArchiveCentralDirectorySeekFlags;
    FDisksNumber: longint;
    FDiskNumber: longint;
    FDiskSeek: int64;
    // central directory magik seek property
    FMagikSeek: longint;
  private
    procedure Pack;
    procedure UnPack;
    function GetCount: longint;
    function GetItem(Index: longint): TArchiveItem;
    function GetIndexOf(const FileName: string): longint;
    function GetIndexAuxOf(const FileName: string): longint;
    function CompareItem(Item1, Item2: TArchiveItem): longint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    function Add(Item : TArchiveItem): longint;
    procedure Delete(Index: longint);
    procedure Clear;
    function IndexOf(const FileName: string): longint;
  public
    property Count: longint read GetCount;
    property Items[Index: longint]: TArchiveItem read GetItem;
    property LastModifiedTime: int64 read FLastModifiedTime;
    property Comment: string read FComment write FComment;
  end;

  /// ...

  TArchiveConfirm = (arcOk, arcCancel, arcQuit);

  /// archive events

  TArchiveCommentEvent = procedure (Item: TArchiveItem;
    var CommentAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveDeleteEvent = procedure(Item: TArchiveItem;
    var Confirm: TArchiveConfirm) of object;

  TArchiveExtractEvent = procedure(Item: TArchiveItem;
    var ExtractAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveMessageEvent = procedure(const Msg: string) of object;

  TArchiveProgressEvent = procedure(Percentage: longint) of object;

  TArchiveRenameEvent = procedure(Item: TArchiveItem;
    var RenameAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveUpdateEvent = procedure(SearchRec: TCustomSearchRec;
    var UpdateAs: string; var Confirm: TArchiveConfirm) of object;

  /// archive manager
  TArchiver = class(TObject)
  private
    FSuspended: boolean;
    FIsNeededToRun: boolean;
    FIsNeededToSwap: boolean;
    FIsNeededToSave: boolean;
    FProcessedSize: int64;
    FTotalSize: int64;
    // filenames and streams
    FArchiveName: string;
    FArchiveReader: TFileReader;
    FSwapName: string;
    FSwapReader: TFileReader;
    FSwapWriter: TFileWriter;
    FTempName: string;
    FTempWriter: TFileWriter;
    FSfxName: string;
    FSfxStream: TMemoryStream;
    // options
    FWorkDirectory: string;
    FCompressionParams: string;
    FEncryptionParams: string;
    FPassword: string;
    FCheckParams: string;
    FTestTempArchive: boolean;
    FVerboseMode: boolean;
    FVolumeSize: int64;
    FNewLayer: boolean;
    // central directory
    FCentralDirectory: TArchiveCentralDirectory;
    FCentralDirectoryNews: TList;
    // events
    FOnCommentItem: TArchiveCommentEvent;
    FOnDeleteItem: TArchiveDeleteEvent;
    FOnExtractItem: TArchiveExtractEvent;
    FOnMessage: TArchiveMessageEvent;
    FOnProgress: TArchiveProgressEvent;
    FOnRenameItem: TArchiveRenameEvent;
    FOnRequestBlankImage: TFileWriterRequestBlankImageEvent;
    FOnRequestImage: TFileReaderRequestImageEvent;
    FOnUpdateItem: TArchiveUpdateEvent;
  private
    function  DoCommentItem(Item: TArchiveItem): TArchiveConfirm;
    function  DoDeleteItem (Item: TArchiveItem): TArchiveConfirm;
    function  DoExtractItem(Item: TArchiveItem): TArchiveConfirm;
    procedure DoMessage(const Message: string);
    procedure DoProgress(Value: longint);
    function  DoRenameItem (Item: TArchiveItem): TArchiveConfirm;
    procedure DoRequestBlankImage(ImageNumber: longint; var Abort : Boolean);
    procedure DoRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
    function  DoUpdateItem (Item: TCustomSearchRec): TArchiveConfirm;

    procedure SetArchiveName(const Value: string);
    procedure SetComment(const Value: string);
    procedure SetSfxName(const Value: string);
    procedure SetWorkDirectory(const Value: string);

    function GetComment: string;
    function GetCount: longint;
    function GetItem(Index: longint): TArchiveItem;
    function GetLastModifiedTime: int64;
    function GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetBackTear(Index: longint): longint;
    function GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetNextTear(Index: longint): longint;

    procedure Encode(Reader: TBufStream; Writer: TBufStream; const Size: int64);
    procedure EncodeFromArchive(Item: TArchiveItem);
    procedure EncodeFromSwap   (Item: TArchiveItem);
    procedure EncodeFromFile   (Item: TArchiveItem);
    procedure DecodeToNul      (Item: TArchiveItem);
    procedure DecodeToSwap     (Item: TArchiveItem);
    procedure DecodeToFile     (Item: TArchiveItem);

    procedure Configure;
    procedure Swapping;
    procedure SaveTemporaryArchive;
    procedure TestTemporaryArchive;

    procedure CheckTags4Update;
    procedure CheckSequences4Update;
    procedure CheckTags4Delete;
    procedure CheckSequences4Delete;
    procedure CheckTags4Test;
    procedure CheckTags4Extract;
    procedure CheckSequences4Extract;
    procedure CheckTags4Rename;
    procedure CheckSequences4Rename;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CloseArchive;
    procedure ExtractTagged;
    procedure DeleteTagged;
    function IndexOf(const aFileName: string): longint;
    function IsTagged(Index: longint): boolean;
    procedure OpenArchive(const aArchiveName: string);
    procedure RenameTagged;
    procedure Suspend(Value: boolean);
    procedure Tag(SearchRec: TCustomSearchRec); overload;
    procedure Tag(Index: longint); overload;
    procedure TagAll;
    procedure Terminate;
    procedure TestTagged;
    procedure UnTag(Index: longint);
    procedure UnTagAll;
    procedure UpdateTagged;
  public
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property Comment: string read GetComment write SetComment;
    property Count: longint read GetCount;
    property CheckParams: string read FCheckParams write FCheckParams;
    property CompressionParams: string read FCompressionParams write FCompressionParams;
    property EncryptionParams: string read FEncryptionParams write FEncryptionParams;
    property Items[Index: longint]: TArchiveItem read GetItem;
    property LastModifiedTime: int64 read GetLastModifiedTime;
    property NewLayer: boolean read FNewLayer write FNewLayer;
    property Password: string read FPassword write FPassword;
    property SelfExtractor: string read FSfxName write FSfxName;
    property TestTempArchive: boolean read FTestTempArchive write FTestTempArchive;
    property VolumeSize: int64 read FVolumeSize write FVolumeSize;
    property VerboseMode: boolean read FVerboseMode write FVerboseMode;
    property WorkDirectory: string read FWorkDirectory write SetWorkDirectory;
  public
    property OnItemComment: TArchiveCommentEvent read FOnCommentItem write FOnCommentItem;
    property OnItemDelete: TArchiveDeleteEvent read FOnDeleteItem write FOnDeleteItem;
    property OnItemExtract: TArchiveExtractEvent read FOnExtractItem write FOnExtractItem;
    property OnMessage: TArchiveMessageEvent read FOnMessage write FOnMessage;
    property OnProgress: TArchiveProgressEvent read FOnProgress write FOnProgress;
    property OnItemRename: TArchiveRenameEvent read FOnRenameItem write FOnRenameItem;
    property OnRequestBlankImage: TFileWriterRequestBlankImageEvent read FOnRequestBlankImage write FOnRequestBlankImage;
    property OnRequestImage: TFileReaderRequestImageEvent read FOnRequestImage write FOnRequestImage;
    property OnItemUpdate: TArchiveUpdateEvent read FOnUpdateItem write FOnUpdateItem;
  end;

function CoderMethodToStr (Method: longint): string;
function HashMethodToStr  (Method: longint): string;
function CipherMethodToStr(Method: longint): string;

function VersionToStr(Version: longword): string;
function RatioToStr(const PackedSize, Size: int64): string;
function SizeToStr(const Size: int64): string;
function AttrToStr(Attr: longint): string;

implementation

// ---

function GetVersionNeededToRead(Item: TArchiveItem): longword; overload;
begin
  Result := 80;
end;

function GetVersionNeededToRead(Flags: TArchiveCentralDirectoryFlags): longword; overload;
begin
  Result := 80;
end;

function GetVersionNeededToRead(Flags: TArchiveCentralDirectorySeekFlags): longword; overload;
begin
  Result := 80;
end;

// ---

function CoderMethodToStr(Method: longint): string;
begin
  Result := '???';
  case Method of
    0:  Result := 'NONE';
    1:  Result := 'BEE';
    2:  Result := 'PPMD';
  end;
end;

function HashMethodToStr(Method: longint): string;
begin
  Result := '???';
  case Method of
    0: Result := 'NONE';
    1: Result := 'CRC32';
    2: Result := 'CRC64';
    3: Result := 'SHA1';
    4: Result := 'MD5';
  end;
end;

function CipherMethodToStr(Method: longint): string;
begin
  Result := '???';
  case Method of
    0: Result := 'NONE';
    1: Result := 'BLOWFISH';
    2: Result := 'IDEA';
  end;
end;

function VersionToStr(Version: longword): string;
begin
  Result := '?.?.?';
  case Version of
    80:  Result := '0.8.0';
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
  /// item property ///
  FFlags := [
    aifVersionNeededToRead,
    aifUncompressedSize,
    aifLastModifiedTime,
    aifLastStoredTime,
    aifAttributes,
    aifComment,
    aifLowLayer,
    aifHighLayer];
  FVersionNeededToRead :=  0;
  FUncompressedSize    :=  0;
  FLastModifiedTime    :=  0;
  FLastStoredTime      :=  0;
  FAttributes          :=  0;
  FComment             := '';
  FLowLayer            :=  0;
  FHighLayer           :=  0;
  /// data descriptor property ///
  FDataDescriptorFlags := [
    addfCompressedSize,
    addfDiskNumber,
    addfDiskSeek,
    addfCheckMethod,
    addfCheckDigest,
    addfCheckMethodAux,
    addfCheckDigestAux];
  FCompressedSize       := 0;
  FDiskNumber           := 0;
  FDiskSeek             := 0;
  FCheckMethod          := 0;
  FCheckDigest          := '';
  FCheckMethodAux       := 0;
  FCheckDigestAux       := '';
  /// compression property ///
  FCompressionFlags     := [
    acfCompressionMethod,
    acfCompressionLevel,
    acfCompressionLevelAux,
    acfCompressionFilter,
    acfCompressionFilterAux,
    acfCompressionBlock];
  FCompressionMethod    := 0;
  FCompressionLevel     := 0;
  FCompressionLevelAux  := 0;
  FCompressionFilter    := '';
  FCompressionFilterAux := '';
  FCompressionBlock     := 0;
  /// encryption property ///
  FEncryptionFlags      := [
    aefEncryptionMethod];
  FEncryptionMethod     := 0;
  /// reserved property ///
  FTag                  := aitAdd;
  FIndex                := -1;
  FExternalFileName     := '';
  FExternalFileSize     :=  0;
end;

procedure TArchiveItem.Update(SearchRec: TCustomSearchRec);
begin
  /// item property ///
  FLastModifiedTime  := SearchRec.LastModifiedTime;
  FAttributes        := SearchRec.Attributes;
  /// reserved property ///
  FExternalFileName  := SearchRec.Name;
  FExternalFileSize  := SearchRec.Size;
end;

constructor TArchiveItem.Read(Stream: TFileReader);
begin
  FFileName := Stream.ReadInfString;
  /// item property ///
  FFlags  := TArchiveItemFlags(longword(Stream.ReadInfWord));
  if (aifVersionNeededToRead in FFlags) then FVersionNeededToRead := Stream.ReadInfWord;
  if (aifUncompressedSize    in FFlags) then FUncompressedSize    := Stream.ReadInfWord;
  if (aifLastModifiedTime    in FFlags) then FLastModifiedTime    := Stream.ReadInfWord;
  if (aifLastStoredTime      in FFlags) then FLastStoredTime      := Stream.ReadInfWord;
  if (aifAttributes          in FFlags) then FAttributes          := Stream.ReadInfWord;
  if (aifComment             in FFlags) then FComment             := Stream.ReadInfString;
  if (aifLowLayer            in FFlags) then FLowLayer            := Stream.ReadInfWord;
  if (aifHighLayer           in FFlags) then FHighLayer           := Stream.ReadInfWord;
  /// data descryptor property ///
  FDataDescriptorFlags := TArchiveDataDescriptorFlags(longword(Stream.ReadInfWord));
  if (addfCompressedSize in FDataDescriptorFlags) then FCompressedSize := Stream.ReadInfWord;
  if (addfDiskNumber     in FDataDescriptorFlags) then FDiskNumber     := Stream.ReadInfWord;
  if (addfDiskSeek       in FDataDescriptorFlags) then FDiskSeek       := Stream.ReadInfWord;
  if (addfCheckMethod    in FDataDescriptorFlags) then FCheckMethod    := Stream.ReadInfWord;
  if (addfCheckDigest    in FDataDescriptorFlags) then FCheckDigest    := Stream.ReadInfArray;
  if (addfCheckMethodAux in FDataDescriptorFlags) then FCheckMethodAux := Stream.ReadInfWord;
  if (addfCheckDigestAux in FDataDescriptorFlags) then FCheckDigestAux := Stream.ReadInfArray;
  /// compression property ///
  FCompressionFlags := TArchiveCompressionFlags(longword(Stream.ReadInfWord));
  if (acfCompressionMethod    in FCompressionFlags) then FCompressionMethod    := Stream.ReadInfWord;
  if (acfCompressionLevel     in FCompressionFlags) then FCompressionLevel     := Stream.ReadInfWord;
  if (acfCompressionLevelAux  in FCompressionFlags) then FCompressionLevelAux  := Stream.ReadInfWord;
  if (acfCompressionFilter    in FCompressionFlags) then FCompressionFilter    := Stream.ReadInfArray;
  if (acfCompressionFilterAux in FCompressionFlags) then FCompressionFilterAux := Stream.ReadInfArray;
  if (acfCompressionBlock     in FCompressionFlags) then FCompressionBlock     := Stream.ReadInfWord;
  /// encryption property ///
  FEncryptionFlags := TArchiveEncryptionFlags(longword(Stream.ReadInfWord));
  if (aefEncryptionMethod  in FEncryptionFlags) then FEncryptionMethod := Stream.ReadInfWord;
end;

procedure TArchiveItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfString(FFileName);
  /// item property ///
  Stream.WriteInfWord(longword(FFlags));
  if (aifVersionNeededToRead in FFlags) then Stream.WriteInfWord(FVersionNeededToRead);
  if (aifUncompressedSize    in FFlags) then Stream.WriteInfWord(FUncompressedSize);
  if (aifLastModifiedTime    in FFlags) then Stream.WriteInfWord(FLastModifiedTime);
  if (aifLastStoredTime      in FFlags) then Stream.WriteInfWord(FLastStoredTime);
  if (aifAttributes          in FFlags) then Stream.WriteInfWord(FAttributes);
  if (aifComment             in FFlags) then Stream.WriteInfString(FComment);
  if (aifLowLayer            in FFlags) then Stream.WriteInfWord(FLowLayer);
  if (aifHighLayer           in FFlags) then Stream.WriteInfWord(FHighLayer);
  /// data descriptor property ///
  Stream.WriteInfWord(longword(FDataDescriptorFlags));
  if (addfCompressedSize in FDataDescriptorFlags) then Stream.WriteInfWord(FCompressedSize);
  if (addfDiskNumber     in FDataDescriptorFlags) then Stream.WriteInfWord(FDiskNumber);
  if (addfDiskSeek       in FDataDescriptorFlags) then Stream.WriteInfWord(FDiskSeek);
  if (addfCheckMethod    in FDataDescriptorFlags) then Stream.WriteInfWord(Ord(FCheckMethod));
  if (addfCheckDigest    in FDataDescriptorFlags) then Stream.WriteInfArray(FCheckDigest);
  if (addfCheckMethodAux in FDataDescriptorFlags) then Stream.WriteInfWord(Ord(FCheckMethodAux));
  if (addfCheckDigestAux in FDataDescriptorFlags) then Stream.WriteInfArray(FCheckDigestAux);
  /// compression property ///
  Stream.WriteInfWord(longword(FCompressionFlags));
  if (acfCompressionMethod    in FCompressionFlags) then Stream.WriteInfWord(Ord(FCompressionMethod));
  if (acfCompressionLevel     in FCompressionFlags) then Stream.WriteInfWord(FCompressionLevel);
  if (acfCompressionLevelAux  in FCompressionFlags) then Stream.WriteInfWord(FCompressionLevelAux);
  if (acfCompressionFilter    in FCompressionFlags) then Stream.WriteInfArray(FCompressionFilter);
  if (acfCompressionFilterAux in FCompressionFlags) then Stream.WriteInfArray(FCompressionFilterAux);
  if (acfCompressionBlock     in FCompressionFlags) then Stream.WriteInfWord(FCompressionBlock);
  /// encryption property ///
  Stream.WriteInfWord(longword(FEncryptionFlags));
  if (aefEncryptionMethod  in FEncryptionFlags) then Stream.WriteInfWord(Ord(FEncryptionMethod));
end;

// TArchiveCentralDirectory class

constructor TArchiveCentralDirectory.Create;
begin
  inherited Create;
  FItems    := TList.Create;
  FItemsAux := TList.Create;
  FLastModifiedTime :=  0;
  FComment  := '';
end;

destructor TArchiveCentralDirectory.Destroy;
begin
  Clear;
  FItems.Destroy;
  FItemsAux.Destroy;
  inherited Destroy;
end;

procedure TArchiveCentralDirectory.Clear;
var
  I: longint;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    TArchiveItem(FItems[I]).Destroy;
  end;
  FItems.Clear;
  FItemsAux.Clear;
  FLastModifiedTime := 0;
  FComment := '';
end;

procedure TArchiveCentralDirectory.Pack;
var
  I: longint;
  CurrentItem: TArchiveItem;
  PreviusItem: TArchiveItem;
begin
  if FItems.Count > 1 then
    for I := 1 to FItems.Count - 1 do
    begin
      PreviusItem := TArchiveItem(FItems[I - 1]);
      CurrentItem := TArchiveItem(FItems[I]);
      /// item property ///
      if CurrentItem.FVersionNeededToRead  = PreviusItem.FVersionNeededToRead  then Exclude(CurrentItem.FFlags, aifVersionNeededToRead) else Include(CurrentItem.FFlags, aifVersionNeededToRead);
      if CurrentItem.FUncompressedSize     = PreviusItem.FUncompressedSize     then Exclude(CurrentItem.FFlags, aifUncompressedSize)    else Include(CurrentItem.FFlags, aifUncompressedSize);
      if CurrentItem.FLastModifiedTime     = PreviusItem.FLastModifiedTime     then Exclude(CurrentItem.FFlags, aifLastModifiedTime)    else Include(CurrentItem.FFlags, aifLastModifiedTime);
      if CurrentItem.FLastStoredTime       = PreviusItem.FLastStoredTime       then Exclude(CurrentItem.FFlags, aifLastStoredTime)      else Include(CurrentItem.FFlags, aifLastStoredTime);
      if CurrentItem.FAttributes           = PreviusItem.FAttributes           then Exclude(CurrentItem.FFlags, aifAttributes)          else Include(CurrentItem.FFlags, aifAttributes);
      if CurrentItem.FComment              = PreviusItem.FComment              then Exclude(CurrentItem.FFlags, aifComment)             else Include(CurrentItem.FFlags, aifComment);
      if CurrentItem.FLowLayer             = PreviusItem.FLowLayer             then Exclude(CurrentItem.FFlags, aifLowLayer)            else Include(CurrentItem.FFlags, aifLowLayer);
      if CurrentItem.FHighLayer            = PreviusItem.FHighLayer            then Exclude(CurrentItem.FFlags, aifHighLayer)           else Include(CurrentItem.FFlags, aifHighLayer);
      /// data descriptor property ///
      if CurrentItem.FCompressedSize       = PreviusItem.FCompressedSize       then Exclude(CurrentItem.FDataDescriptorFlags, addfCompressedSize) else Include(CurrentItem.FDataDescriptorFlags, addfCompressedSize);
      if CurrentItem.FDiskNumber           = PreviusItem.FDiskNumber           then Exclude(CurrentItem.FDataDescriptorFlags, addfDiskNumber)     else Include(CurrentItem.FDataDescriptorFlags, addfDiskNumber);
      if CurrentItem.FDiskseek             = PreviusItem.FDiskSeek             then Exclude(CurrentItem.FDataDescriptorFlags, addfDiskSeek)       else Include(CurrentItem.FDataDescriptorFlags, addfDiskSeek);
      if CurrentItem.FCheckMethod          = PreviusItem.FCheckMethod          then Exclude(CurrentItem.FDataDescriptorFlags, addfCheckMethod)    else Include(CurrentItem.FDataDescriptorFlags, addfCheckMethod);
      if CurrentItem.FCheckDigest          = PreviusItem.FCheckDigest          then Exclude(CurrentItem.FDataDescriptorFlags, addfCheckDigest)    else Include(CurrentItem.FDataDescriptorFlags, addfCheckDigest);
      if CurrentItem.FCheckMethodAux       = PreviusItem.FCheckMethodAux       then Exclude(CurrentItem.FDataDescriptorFlags, addfCheckMethodAux) else Include(CurrentItem.FDataDescriptorFlags, addfCheckMethodAux);
      if CurrentItem.FCheckDigestAux       = PreviusItem.FCheckDigestAux       then Exclude(CurrentItem.FDataDescriptorFlags, addfCheckDigestAux) else Include(CurrentItem.FDataDescriptorFlags, addfCheckDigestAux);
      /// compression property ///
      if CurrentItem.FCompressionMethod    = PreviusItem.FCompressionMethod    then Exclude(CurrentItem.FCompressionFlags, acfCompressionMethod)    else Include(CurrentItem.FCompressionFlags, acfCompressionMethod);
      if CurrentItem.FCompressionLevel     = PreviusItem.FCompressionLevel     then Exclude(CurrentItem.FCompressionFlags, acfCompressionLevel)     else Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
      if CurrentItem.FCompressionLevelAux  = PreviusItem.FCompressionLevelAux  then Exclude(CurrentItem.FCompressionFlags, acfCompressionLevelAux)  else Include(CurrentItem.FCompressionFlags, acfCompressionLevelAux);
      if CurrentItem.FCompressionFilter    = PreviusItem.FCompressionFilter    then Exclude(CurrentItem.FCompressionFlags, acfCompressionFilter)    else Include(CurrentItem.FCompressionFlags, acfCompressionFilter);
      if CurrentItem.FCompressionFilterAux = PreviusItem.FCompressionFilterAux then Exclude(CurrentItem.FCompressionFlags, acfCompressionFilterAux) else Include(CurrentItem.FCompressionFlags, acfCompressionFilterAux);
      if CurrentItem.FCompressionBlock     = PreviusItem.FCompressionBlock     then Exclude(CurrentItem.FCompressionFlags, acfCompressionBlock)     else Include(CurrentItem.FCompressionFlags, acfCompressionBlock);
      /// encryption property ///
      if CurrentItem.FEncryptionMethod     = PreviusItem.FEncryptionMethod     then Exclude(CurrentItem.FEncryptionFlags, aefEncryptionMethod)      else Include(CurrentItem.FEncryptionFlags, aefEncryptionMethod);
    end;
end;

procedure TArchiveCentralDirectory.UnPack;
var
  I: longint;
  CurrentItem: TArchiveItem;
  PreviusItem: TArchiveItem;
begin
  if FItems.Count > 1 then
    for I := 1 to FItems.Count - 1 do
    begin
      PreviusItem := TArchiveItem(FItems[I - 1]);
      CurrentItem := TArchiveItem(FItems[I]);
      /// item property ///
      if not(aifVersionNeededToRead in CurrentItem.FFlags) then CurrentItem.FVersionNeededToRead := PreviusItem.FVersionNeededToRead;
      if not(aifUncompressedSize    in CurrentItem.FFlags) then CurrentItem.FUncompressedSize    := PreviusItem.FUncompressedSize;
      if not(aifLastModifiedTime    in CurrentItem.FFlags) then CurrentItem.FLastModifiedTime    := PreviusItem.FLastModifiedTime;
      if not(aifLastStoredTime      in CurrentItem.FFlags) then CurrentItem.FLastStoredTime      := PreviusItem.FLastStoredTime;
      if not(aifAttributes          in CurrentItem.FFlags) then CurrentItem.FAttributes          := PreviusItem.FAttributes;
      if not(aifComment             in CurrentItem.FFlags) then CurrentItem.FComment             := PreviusItem.FComment;
      if not(aifLowLayer            in CurrentItem.FFlags) then CurrentItem.FLowLayer            := PreviusItem.FLowLayer;
      if not(aifHighLayer           in CurrentItem.FFlags) then CurrentItem.FHighLayer           := PreviusItem.FHighLayer;
      /// data descryptor property ///
      if not(addfCompressedSize in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCompressedSize := PreviusItem.FCompressedSize;
      if not(addfDiskNumber     in CurrentItem.FDataDescriptorFlags) then CurrentItem.FDiskNumber     := PreviusItem.FDiskNumber;
      if not(addfDiskSeek       in CurrentItem.FDataDescriptorFlags) then CurrentItem.FDiskSeek       := PreviusItem.FDiskSeek;
      if not(addfCheckMethod    in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCheckMethod    := PreviusItem.FCheckMethod;
      if not(addfCheckDigest    in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCheckDigest    := PreviusItem.FCheckDigest;
      if not(addfCheckMethodAux in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCheckMethodAux := PreviusItem.FCheckMethodAux;
      if not(addfCheckDigestAux in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCheckDigestAux := PreviusItem.FCheckDigestAux;
      /// compression property ///
      if not(acfCompressionMethod    in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionMethod    := PreviusItem.FCompressionMethod;
      if not(acfCompressionLevel     in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionLevel     := PreviusItem.FCompressionLevel;
      if not(acfCompressionLevelAux  in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionLevelAux  := PreviusItem.FCompressionLevelAux;
      if not(acfCompressionFilter    in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionFilter    := PreviusItem.FCompressionFilter;
      if not(acfCompressionFilterAux in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionFilterAux := PreviusItem.FCompressionFilterAux;
      if not(acfCompressionBlock     in CurrentItem.FCompressionFlags) then CurrentItem.FCompressionBlock     := PreviusItem.FCompressionBlock;
      /// encryption property ///
      if not(aefEncryptionMethod in CurrentItem.FEncryptionFlags) then CurrentItem.FEncryptionMethod := PreviusItem.FEncryptionMethod;
    end;
end;

function TArchiveCentralDirectory.CompareItem(Item1, Item2: TArchiveItem): longint;
begin
  Result := AnsiCompareFileName(Item1.FileName, Item2.FileName);
  if Result = 0 then
  begin
    Result := Item1.LowLayer  - Item2.LowLayer;
    //Result := Item1.HighLayer - Item2.HighLayer;
  end;
end;

function TArchiveCentralDirectory.Add(Item: TArchiveItem): longint;
var
  Lo, Mid, Hi, I: longint;
begin
  Item.FIndex := FItems.Add(Item);
  if FItemsAux.Count <> 0 then
  begin
    Lo := 0;
    Hi := FItemsAux.Count - 1;
    while Hi >= Lo do
    begin
      Mid := (Lo + Hi) div 2;
      I := CompareItem(Item, TArchiveItem(FItemsAux[Mid]));

      if I > 0 then
        Lo := Mid + 1
      else
        if I < 0 then
          Hi := Mid - 1
        else
          Hi := -2;
    end;

    if Hi = -2 then
    begin
      SetExitStatus(esUnknowError);
    end else
    begin
      if I > 0 then
        FItemsAux.Insert(Mid + 1, Item)
      else
        FItemsAux.Insert(Mid, Item);
    end;
  end else
    FItemsAux.Add(Item);
  Result := Item.FIndex;
end;

function TArchiveCentralDirectory.GetIndexAuxOf(const FileName: string): longint;
var
  Lo, Mid, Hi, I: longint;
begin
  Lo := 0;
  Hi := FItemsAux.Count - 1;
  while Hi >= Lo do
  begin
    Mid := (Lo + Hi) div 2;

    I := AnsiCompareFileName(FileName, TArchiveItem(FItemsAux[Mid]).FileName);
    if I = 0 then
    begin
      I := 0 - TArchiveItem(FItemsAux[Mid]).LowLayer;
    end;

    if I > 0 then
      Lo := Mid + 1
    else
      if I < 0 then
        Hi := Mid - 1
      else
        Hi := -2;
  end;

  Result := -1;
  if Hi = -2 then
  begin
    Result := Mid;
  end;
end;

function TArchiveCentralDirectory.GetIndexOf(const FileName: string): longint;
begin
  Result := GetIndexAuxOf(FileName);
  if Result <> -1 then
  begin
    Result := TArchiveItem(FItemsAux[Result]).FIndex;
  end;
end;

function TArchiveCentralDirectory.IndexOf(const FileName: string): longint;
begin
  Result := GetIndexOf(FileName);
end;

procedure TArchiveCentralDirectory.Delete(Index: longint);
var
  I: longint;
  Item: TArchiveItem;
begin
  Item := Items[Index];
  if Item.CompressionBlock = 0 then
    if Index < FItems.Count - 1 then
      Items[Index + 1].FCompressionBlock := 0;

  FItemsAux.Delete(GetIndexAuxOf(Item.FileName));
  FItems.Delete(Item.FIndex);
  Item.Destroy;

  for I := 0 to FItems.Count - 1 do
    TArchiveItem(FItems[I]).FIndex := I;
end;

function TArchiveCentralDirectory.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TArchiveCentralDirectory.GetItem(Index: longint): TArchiveItem;
begin
  Result := TArchiveItem(FItems[Index]);
end;

procedure TArchiveCentralDirectory.Read(Stream: TFileReader);
const
  CDFULL  = [acdfVersionNeededToRead,  acdfLastModifiedTime, acdfComment];
  CDSFULL = [acdsfVersionNeededToRead, acdsfDisksNumber,     acdsfDiskNumber];
var
  MARKER: longword;
begin
  // [0] seek on read central directory magik seek
  if ExitStatus = esNoError then
    Stream.Seek(-2*SizeOf(DWord), fsFromEnd);

  // [1] read central directory magik seek marker
  if ExitStatus = esNoError then
    if Stream.ReadDWord <> ARCHIVE_CENTRALDIR_MAGIKSEEK_MARKER then
      SetExitStatus(esArchiveTypeError);

  // [2] seek on central directory marker (or seek marker)
  if ExitStatus = esNoError then
    Stream.Seek(-Stream.ReadDWord, fsFromEnd);

  // [3] read central directory marker (or seek marker)
  if ExitStatus = esNoError then
  begin
    MARKER := Stream.ReadDWord;
    if MARKER <> ARCHIVE_CENTRALDIR_MARKER then
      if MARKER <> ARCHIVE_CENTRALDIR_SEEK_MARKER then
        SetExitStatus(esArchiveTypeError);
  end;

  // [4] read central directory seek
  if ExitStatus = esNoError then
    if MARKER = ARCHIVE_CENTRALDIR_SEEK_MARKER then
    begin
      FSeekFlags := TArchiveCentralDirectorySeekFlags(longword(Stream.ReadInfWord));
      if (acdsfVersionNeededToRead in FSeekFlags) then
        if Stream.ReadInfWord > GetVersionNeededToRead(CDSFULL) then
          SetExitStatus(esArchiveVerError);

      if ExitStatus = esNoError then
      begin
        FDisksNumber := 1;
        if (acdsfDisksNumber in FSeekFlags) then
          FDisksNumber := Stream.ReadInfWord;

        FDiskNumber  := 1;
        if (acdsfDiskNumber  in FSeekFlags) then
          FDiskNumber := Stream.ReadInfWord;

        FDiskSeek := Stream.ReadInfWord;
        // [4.1] seek on central directory marker
        Stream.ImagesNumber := FDisksNumber;
        Stream.ImageNumber  := FDiskNumber;
        Stream.Seek           (FDiskSeek, fsFromBeginning);
        // [4.2] read central directory marker
        MARKER := Stream.ReadDWord;
        if MARKER <> ARCHIVE_CENTRALDIR_MARKER then
          SetExitStatus(esArchiveTypeError);
      end;
    end;

  // [5] read central directory
  if ExitStatus = esNoError then
    if MARKER = ARCHIVE_CENTRALDIR_MARKER then
    begin
      FFlags := TArchiveCentralDirectoryFlags(longword(Stream.ReadInfWord));
      if (acdfVersionNeededToRead in FFlags) then
        if Stream.ReadInfWord > GetVersionNeededToRead(CDFULL) then
          SetExitStatus(esArchiveVerError);

      if ExitStatus = esNoError then
      begin
        FLastModifiedTime := 0;
        if (acdfLastModifiedTime in FFlags) then
          FLastModifiedTime := Stream.ReadInfWord;

        FComment := '';
        if (acdfComment  in FFlags) then
          FComment := Stream.ReadInfString;
      end;
    end;;

  // [6] read central directory items
  if ExitStatus = esNoError then
  begin
    MARKER := Stream.ReadInfWord;
    while MARKER = ARCHIVE_CENTRALDIR_ITEM_MARKER do
    begin
      Add(TArchiveItem.Read(Stream));
      MARKER := Stream.ReadInfWord;
    end;
  end;

  // [7] check central directory seek marker
  if ExitStatus = esNoError then
    if MARKER <> ARCHIVE_CENTRALDIR_SEEK_MARKER then
      if MARKER <> ARCHIVE_CENTRALDIR_MAGIKSEEK_MARKER then
        SetExitStatus(esArchiveTypeError);

  // [8] unpack central directory
  if ExitStatus = esNoError then UnPack;
end;

procedure TArchiveCentralDirectory.Write(Stream: TFileWriter);
var
  I: longword;
begin
  // [0] store central directory seek
  FSeekFlags  := [acdsfVersionNeededToRead];
  FDiskSeek   := Stream.Seek(0, fsFromCurrent);
  FDiskNumber := Stream.CurrentImage;
  if FDiskNumber <> 1 then
    Include(FSeekFlags,  acdsfDiskNumber);

  // [1] write central directory
  FFlags := [acdfVersionNeededToRead, acdfLastModifiedTime];
  FLastModifiedTime := DateTimeToUnix(Now);
  if Length(FComment) > 0 then
    Include(FFlags, acdfComment);

  Stream.WriteDWord(ARCHIVE_CENTRALDIR_MARKER);
  Stream.WriteInfWord(longword(FFlags));
  if (acdfVersionNeededToRead in FFlags) then
    Stream.WriteInfWord(GetVersionNeededToRead(FFlags));
  if (acdfLastModifiedTime in FFlags) then
    Stream.WriteInfWord(FLastModifiedTime);
  if (acdfComment in FFlags) then
    Stream.WriteInfString(FComment);

  // [2] write central directory items
  Pack;
  if FItems.Count > 0 then
    for I := 0 to FItems.Count - 1 do
    begin
      Stream.WriteInfWord(ARCHIVE_CENTRALDIR_ITEM_MARKER);
      TArchiveItem(FItems[I]).Write(Stream);
    end;

  // [3] multi-spanning support
  if Stream.Threshold > 0 then
    if (Stream.Threshold - Stream.Seek(0, fsFromCurrent)) < 512 then
      Stream.CreateNewImage;

  FMagikSeek := Stream.Seek(0, fsFromCurrent);
  // [4] write central directory seek
  Stream.WriteDWord(ARCHIVE_CENTRALDIR_SEEK_MARKER);
  Stream.WriteInfWord(longword(FSeekFlags));
  if (acdsfVersionNeededToRead in FSeekFlags) then
    Stream.WriteInfWord(GetVersionNeededToRead(FSeekFlags));
  if (acdsfDisksNumber in FSeekFlags) then
    Stream.WriteInfWord(FDisksNumber);
  if (acdsfDiskNumber in FSeekFlags) then
    Stream.WriteInfWord(FDiskNumber);
  Stream.WriteInfWord(FDiskSeek);

  // [5] write magikseek
  Stream.WriteDWord(ARCHIVE_CENTRALDIR_MAGIKSEEK_MARKER);
  Stream.WriteDWord(longword(Stream.Seek(0, fsFromCurrent)
    - FMagikSeek + SizeOf(DWord)));
end;

// TArchiver class

constructor TArchiver.Create;
begin
  inherited Create;
  Randomize;
  FSuspended         := FALSE;
  FIsNeededToRun     := FALSE;
  FIsNeededToSwap    := FALSE;
  FIsNeededToSave    := FALSE;
  // files and streams
  FArchiveName       := '';
  FArchiveReader     := nil;
  FSwapName          := '';
  FSwapReader        := nil;
  FSwapWriter        := nil;
  FTempName          := '';
  FTempWriter        := nil;
  FSfxName           := '';
  FSfxStream         := nil;
  // options
  FWorkDirectory     := '';
  FCompressionParams := '';
  FEncryptionParams  := '';
  FCheckParams       := '';
  FTestTempArchive   := FALSE;
  FVolumeSize        := 0;
  FVerboseMode       := FALSE;
  FNewLayer          := FALSE;
  // central direcotry items
  FCentralDirectory     := TArchiveCentralDirectory.Create;
  FCentralDirectoryNews := TList.Create;
end;

destructor TArchiver.Destroy;
var
  I: longint;
begin
  FCentralDirectoryNews.Destroy;
  FCentralDirectory.Destroy;
  inherited Destroy;
end;

// TArchiver # ENCODE/DECODE #

procedure TArchiver.Encode(Reader: TBufStream; Writer: TBufStream; const Size: int64); inline;
var
  Count: int64;
  Buffer: TBuffer;
begin
  Reader.StartSession;
  Writer.StartSession;
  Count := Size div SizeOf(Buffer);
  while (Count <> 0) and (ExitStatus = esNoError) do
  begin
    Reader.Decode(@Buffer[0], SizeOf(Buffer));
    Writer.Encode(@Buffer[0], SizeOf(Buffer));
    DoProgress(SizeOf(Buffer));
    Dec(Count);
  end;
  Count := Size mod SizeOf(Buffer);
  if (Count <> 0) and (ExitStatus = esNoError) then
  begin
    Reader.Decode(@Buffer[0], Count);
    Writer.Encode(@Buffer[0], Count);
    DoProgress(Count);
  end;
  Writer.EndSession;
  Reader.EndSession;
end;

procedure TArchiver.EncodeFromArchive(Item: TArchiveItem);
begin
  FArchiveReader.Seek(Item.FDiskSeek, fsFromBeginning, Item.FDiskNumber);
  Item.FDiskNumber := FTempWriter.CurrentImage;
  Item.FDiskSeek   := FTempWriter.Seek(0, fsFromCurrent);

  FArchiveReader.HashMethod   := 0;
  FArchiveReader.CipherMethod := 0;
  FArchiveReader.CoderMethod  := 0;

  FTempWriter.HashMethod      := 0;
  FTempWriter.CipherMethod    := 0;
  FTempWriter.CoderMethod     := 0;
  begin
    Encode(FArchiveReader, FTempWriter, Item.FCompressedSize);
  end;
end;

procedure TArchiver.EncodeFromSwap(Item: TArchiveItem);
begin
  FSwapReader.Seek(Item.FDiskSeek, fsFromBeginning, Item.FDiskNumber);
  Item.FDiskSeek              := FTempWriter.Seek(0, fsFromCurrent);
  Item.FDiskNumber            := FTempWriter.CurrentImage;

  FSwapReader.HashMethod      := Item.CheckMethod;
  FSwapReader.CipherMethod    := Item.EncryptionMethod;
  FSwapReader.CipherKey       := FPassword;
  FSwapReader.CoderMethod     := 0;

  FTempWriter.HashMethod      := Item.CheckMethod;
  FTempWriter.CipherMethod    := Item.EncryptionMethod;
  FTempWriter.CipherKey       := FPassword;
  FTempWriter.CoderMethod     := Item.CompressionMethod;
  FTempWriter.CoderLevel      := Item.CompressionLevel;
  FTempWriter.CoderLevelAux   := Item.CompressionLevelAux;
  FTempWriter.CoderFilter     := Item.CompressionFilter;
  FTempWriter.CoderFilterAux  := Item.CompressionFilterAux;
  FTempWriter.CoderBlock      := Item.CompressionBlock;
  begin
    Encode(FSwapReader, FTempWriter, Item.FUnCompressedSize);
  end;
  //Item.FCheckDigest        := FSwapReader.HashDigest;
  //Item.FCheckDigestAux     := FTempWriter.HashDigest;
  Item.FCompressedSize       := FTempWriter.Seek(0, fsFromCurrent) - Item.FDiskSeek;
  //Item.FUncompressedSize   := Item.FExternalFileSize;
end;

procedure TArchiver.EncodeFromFile(Item: TArchiveItem);
var
  Source: TFileReader;
begin
  Source := TFileReader.Create(Item.FExternalFileName, nil);
  Item.FDiskSeek             := FTempWriter.Seek(0, fsFromCurrent);
  Item.FDiskNumber           := FTempWriter.CurrentImage;

  Source.HashMethod          := Item.CheckMethod;
  Source.CipherMethod        := 0;
  Source.CoderMethod         := 0;

  FTempWriter.HashMethod     := Item.CheckMethodAux;
  FTempWriter.CipherMethod   := Item.EncryptionMethod;
  FTempWriter.CipherKey      := ExtractEncryptionPassword(EncryptionParams);
  FTempWriter.CoderMethod    := Item.CompressionMethod;
  FTempWriter.CoderLevel     := Item.CompressionLevel;
  FTempWriter.CoderLevelAux  := Item.CompressionLevelAux;
  FTempWriter.CoderFilter    := Item.CompressionFilter;
  FTempWriter.CoderFilterAux := Item.CompressionFilterAux;
  FTempWriter.CoderBlock     := Item.CompressionBlock;
  begin
    Encode(Source, FTempWriter, Item.FExternalFileSize);
  end;
  Item.FCheckDigest          :=      Source.HashDigest;
  Item.FCheckDigestAux       := FTempWriter.HashDigest;
  Item.FCompressedSize       := FTempWriter.Seek(0, fsFromCurrent) - Item.FDiskSeek;
  Item.FUncompressedSize     := Item.FExternalFileSize;

  FreeAndNil(Source);
end;

procedure TArchiver.DecodeToSwap(Item: TArchiveItem);
begin
  FArchiveReader.Seek(Item.DiskSeek, fsFromBeginning, Item.DiskNumber);
  Item.FDiskSeek   := FSwapWriter.Seek(0, fsFromCurrent);
  Item.FDiskNumber := FSwapWriter.CurrentImage;

  FArchiveReader.HashMethod     := Item.CheckMethod;
  FArchiveReader.CipherMethod   := Item.EncryptionMethod;
  FArchiveReader.CipherKey      := FPassword;
  FArchiveReader.CoderMethod    := Item.CompressionMethod;
  FArchiveReader.CoderLevel     := Item.CompressionLevel;
  FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
  FArchiveReader.CoderFilter    := Item.CompressionFilter;
  FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
  FArchiveReader.CoderBlock     := Item.CompressionBlock;

  FSwapWriter.HashMethod        := Item.CheckMethod;
  FSwapWriter.CipherMethod      := Item.EncryptionMethod;
  FSwapWriter.CipherKey         := FPassword;
  FSwapWriter.CoderMethod       := 0;
  begin
    Encode(FArchiveReader, FSwapWriter, Item.UncompressedSize);
  end;

  if Item.CheckMethod <> 0 then
    if Item.CheckDigest <> FSwapWriter.HashDigest then
      SetExitStatus(esHashError);
end;

procedure TArchiver.DecodeToNul(Item: TArchiveItem);
var
  Count: int64;
  Buffer: TBuffer;
  Destination: TNulBufStream;
begin
  Destination := TNulBufStream.Create;
  FArchiveReader.Seek(Item.FDiskSeek, fsFromBeginning, Item.FDiskNumber);

  Destination.HashMethod        := Item.CheckMethod;
  Destination.CipherMethod      := 0;
  Destination.CoderMethod       := 0;

  FArchiveReader.HashMethod     := 0;
  FArchiveReader.CipherMethod   := Item.EncryptionMethod;
  FArchiveReader.CipherKey      := FPassword;
  FArchiveReader.CoderMethod    := Item.CompressionMethod;
  FArchiveReader.CoderLevel     := Item.CompressionLevel;
  FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
  FArchiveReader.CoderFilter    := Item.CompressionFilter;
  FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
  FArchiveReader.CoderBlock     := Item.CompressionBlock;
  begin
    Encode(FArchiveReader, Destination, Item.FUncompressedSize);
  end;

  if Item.CheckMethod <> 0 then
    if Item.FCheckDigest <> Destination.HashDigest then
      SetExitStatus(esHashError);
  FreeAndNil(Destination);
end;

procedure TArchiver.DecodeToFile(Item: TArchiveItem);
var
  Destination: TFileWriter;
begin
  Destination := TFileWriter.Create(Item.FExternalFileName, FOnRequestBlankImage, 0);
  FArchiveReader.Seek(Item.FDiskSeek, fsFromBeginning, Item.FDiskNumber);

  Destination.HashMethod        := Item.CheckMethod;
  Destination.CipherMethod      := 0;
  Destination.CoderMethod       := 0;

  FArchiveReader.HashMethod     := 0;
  FArchiveReader.CipherMethod   := Item.EncryptionMethod;
  FArchiveReader.CipherKey      := FPassword;
  FArchiveReader.CoderMethod    := Item.CompressionMethod;
  FArchiveReader.CoderLevel     := Item.CompressionLevel;
  FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
  FArchiveReader.CoderFilter    := Item.CompressionFilter;
  FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
  FArchiveReader.CoderBlock     := Item.CompressionBlock;
  begin
    Encode(FArchiveReader, Destination, Item.UncompressedSize);
  end;

  if Item.CheckMethod <> 0 then
    if Item.FCheckDigest <> Destination.HashDigest then
      SetExitStatus(esHashError);
  FreeAndNil(Destination);

  if ExitStatus = esNoError then
  begin
    FileSetDate(Item.FExternalFileName, Item.FLastModifiedTime);
    FileSetAttr(Item.FExternalFileName, Item.FAttributes);
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
    FCentralDirectory.Read(FArchiveReader);
    if ExitStatus = esNoError then
    begin
      FArchiveName := aArchiveName;
      //if FCentralDirectory.Count = 0 then
      //  SetExitStatus(esArchiveTypeError);
    end;

  end else
  begin
    DoMessage(Format(cmCreating, [aArchiveName]));
    FArchiveName := aArchiveName;
  end;
end;

procedure TArchiver.Swapping;
var
  I: longint;
  CRC: longword;
  Item: TArchiveItem;
begin
  if ExitStatus <> esNoError then Exit;

  FSwapName   := GenerateFileName(FWorkDirectory);
  FSwapWriter := TFileWriter.Create(FSwapName, FOnRequestBlankImage, 0);
  FSwapWriter.WriteDWord(ARCHIVE_MARKER);
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;
    Item := FCentralDirectory.Items[I];
    case Item.FTag of
      aitDecode:    DoMessage(Format(cmSwapping, [Item.FFileName]));
      aitDecAndUpd: DoMessage(Format(cmDecoding, [Item.FFileName]));
    end;

    case Item.FTag of
      aitDecode:    DecodeToSwap(Item);
      aitDecAndUpd: DecodeToNul(Item);
    end;
  end;
  FreeAndNil(FSwapWriter);

  if ExitStatus = esNoError then
    FSwapReader := TFileReader.Create(FSwapName, FOnRequestImage);
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

    Tester.EncryptionParams    := FEncryptionParams;

    Tester.OpenArchive(FTempName);
    if ExitStatus = esNoError then
    begin
      Tester.TagAll;
      Tester.TestTagged;
    end;
    Tester.CloseArchive;
    Tester.Destroy;
  end;
end;

procedure TArchiver.SaveTemporaryArchive;
var
  I: longint;
begin
  SysUtils.DeleteFile(FSwapName);
  if ExitStatus = esNoError then
  begin
    if FVolumeSize > 0 then
    begin
      FTotalSize     := 0;
      FProcessedSize := 0;
      for I := 0 to FCentralDirectory.Count - 1 do
        Inc(FTotalSize, FCentralDirectory.Items[I].CompressedSize);

      FArchiveReader := TFileReader.Create(FTempName, FOnRequestImage);
      FTempWriter    := TFileWriter.Create(FArchiveName, FOnRequestBlankImage, FVolumeSize);
      FTempWriter.WriteDWord(ARCHIVE_MARKER);

      for I := 0 to FCentralDirectory.Count - 1 do
      begin
        if ExitStatus <> esNoError then Break;

        DoMessage(Format(cmSplitting, [FCentralDirectory.Items[I].FileName]));
        EncodeFromArchive(FCentralDirectory.Items[I]);
      end;
      FCentralDirectory.Write(FTempWriter);

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

  if ExitStatus in [esNoError, esUserAbortError] then
    SysUtils.DeleteFile(FTempName);
end;

procedure TArchiver.CloseArchive;
var
  I: longint;
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
  for I := 0 to FCentralDirectoryNews.Count - 1 do
    TCustomSearchRec(FCentralDirectoryNews[I]).Destroy;
  FCentralDirectoryNews.Clear;
  FCentralDirectory.Clear;

  FArchiveName    := '';
  FSwapName       := '';
  FTempName       := '';

  FSuspended      := FALSE;
  FIsNeededToRun  := FALSE;
  FIsNeededToSwap := FALSE;
  FIsNeededToSave := FALSE;
end;

// TArchiver # FIND #

function TArchiver.IndexOf(const aFileName: string): longint;
begin
  Result := FCentralDirectory.GetIndexOf(aFileName);
end;

// TArchiver # SUSPEND/TERMINATE #

procedure TArchiver.Terminate;
begin
  SetExitStatus(esUserAbortError);
  FSuspended := FALSE;
end;

procedure TArchiver.Suspend(Value: boolean);
begin
  FSuspended := Value;
end;

// TArchiver # GET #

function TArchiver.GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index downto 0 do
    if FCentralDirectory.Items[I].FTag = aTag then
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
  for  I := Index to FCentralDirectory.Count - 1 do
    if FCentralDirectory.Items[I].FTag = aTag then
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
    if FCentralDirectory.Items[I].CompressionBlock = 0 then
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
  for  I := Index to FCentralDirectory.Count - 1 do
    if FCentralDirectory.Items[I].CompressionBlock = 0 then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiver.GetComment: string;
begin
  Result := FCentralDirectory.Comment;
end;

function TArchiver.GetCount: longint;
begin
  Result := FCentralDirectory.Count;
end;

function TArchiver.GetItem(Index: longint): TArchiveItem;
begin
  Result := FCentralDirectory.Items[Index];
end;

function TArchiver.GetLastModifiedTime: int64;
begin
  Result := FCentralDirectory.LastModifiedTime;
end;

// TArchiver # SET PROPERTY #

procedure TArchiver.SetArchiveName(const Value: string);
begin
  OpenArchive(Value);
end;

procedure TArchiver.SetComment(const Value: string);
begin
  FCentralDirectory.Comment := Value;
end;

procedure TArchiver.SetSfxName(const Value: string);
begin
  FSfxName := Value;
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

procedure TArchiver.DoRequestBlankImage(ImageNumber: longint; var Abort : Boolean);
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

procedure TArchiver.DoProgress(Value: longint); inline;
begin
  Inc(FProcessedSize, Value);
  if Assigned(FOnProgress) then
    if Word(FProcessedSize) = 0 then
    begin
      FOnProgress(Round((FProcessedSize/FTotalSize)*100));
    end;

  while FSuspended do Sleep(250);
end;

function TArchiver.DoCommentItem(Item: TArchiveItem): TArchiveConfirm;
var
  CommentAs: string;
begin
  Result := arcCancel;
  if Assigned(FOnCommentItem) then
  begin
    CommentAs := Item.Comment;
    FOnCommentItem(Item, CommentAs, Result);
    if Result = arcOk then
    begin
      Item.FComment := CommentAs;
    end;
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
  for I := 0 to FCentralDirectory.Count - 1 do Tag(I);
end;

procedure TArchiver.Tag(Index: longint);
begin
  FCentralDirectory.Items[Index].FTag := aitUpdate;
end;

procedure TArchiver.UnTagAll;
var
  I: longint;
begin
  for I := 0 to FCentralDirectory.Count - 1 do UnTag(I);
end;

procedure TArchiver.UnTag(Index: longint);
begin
  FCentralDirectory.Items[Index].FTag := aitNone;
end;

procedure TArchiver.Tag(SearchRec: TCustomSearchRec);
var
  Item: TCustomSearchRec;
begin
  Item                  := TCustomSearchRec.Create;
  Item.Name             := SearchRec.Name;
  Item.Size             := SearchRec.Size;
  Item.LastModifiedTime := SearchRec.LastModifiedTime;
  Item.Attributes       := Searchrec.Attributes;

  FCentralDirectoryNews.Add(Item);
end;

function TArchiver.IsTagged(Index: longint): boolean;
begin
  Result := FCentralDirectory.Items[Index].FTag = aitUpdate;
end;

// TArchiver # EXTRACT #

function TArchiver.DoExtractItem(Item: TArchiveItem): TArchiveConfirm;
begin


end;

procedure TArchiver.CheckTags4Extract;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  ExtractAs: string;
begin
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;
    Item := FCentralDirectory.Items[I];
    if Item.FTag = aitUpdate then
    begin
      Confirm := arcCancel;
      if Assigned(FOnExtractItem) then
      begin
        ExtractAs := Item.FFileName;
        FOnExtractItem(Item, ExtractAs, Confirm);
      end;

      case Confirm of
        arcOk: begin
          Item.FExternalFileName := ExtractAs;
          FIsNeededToRun         := TRUE;
        end;
        arcCancel: Item.FTag := aitNone;
        arcQuit:   SetExitStatus(esUserAbortError);
      end;
    end;
  end;
end;

procedure TArchiver.CheckTags4Test;
var
  I: longint;
begin
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;
    if FCentralDirectory.Items[I].FTag = aitUpdate then
    begin
      FIsNeededToRun := TRUE;
      Break;
    end;
  end;
end;

procedure TArchiver.CheckSequences4Extract;
var
  I, J, BackTear, NextTear: longint;
  Item: TArchiveItem;
begin
  // STEP2: find sequences and tag ...
  I := GetBackTag(FCentralDirectory.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FCentralDirectory.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitUpdate);
      for J := BackTear to NextTear do
      begin
        Item := FCentralDirectory.Items[J];
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
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    Item := FCentralDirectory.Items[I];
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
    CheckSequences4Extract;
    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;
      Item := FCentralDirectory.Items[I];
      case Item.FTag of
        aitUpdate: DoMessage(Format(cmExtracting, [Item.FExternalFileName]));
        aitDecode: DoMessage(Format(cmDecoding,   [Item.FFileName]));
      end;

      case Item.FTag of
        aitNone:;  // nothing to do
        aitUpdate: DecodeToFile(Item);
        aitDecode: DecodeToNul (Item);
        else       SetExitStatus(esCaseError);
      end;
    end;
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
    CheckSequences4Extract;
    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;
      Item := FCentralDirectory.Items[I];
      case Item.FTag of
        aitUpdate: DoMessage(Format(cmTesting,  [Item.FFileName]));
        aitDecode: DoMessage(Format(cmDecoding, [Item.FFileName]));
      end;

      case Item.FTag of
        aitUpdate: DecodeToNul(Item);
        aitDecode: DecodeToNul(Item);
        else       SetExitStatus(esCaseError);
      end;
    end;
  end;
end;

// TArchiver # RENAME #

function TArchiver.DoRenameItem(Item: TArchiveItem): TArchiveConfirm;
var
  RenameAs: string;
begin
  Result := arcCancel;
  if Assigned(FOnRenameItem) then
  begin
    RenameAs := Item.FFileName;
    FOnRenameItem(Item, RenameAs, Result);
    if Result = arcOk then
    begin
      Result := DoCommentItem(Item);
      if Result = arcOk then
        Item.FFileName := RenameAs;
    end;
  end;
end;

procedure TArchiver.CheckTags4Rename;
var
  I: longint;
  Item: TArchiveItem;
begin
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;
    Item := FCentralDirectory.Items[I];
    if Item.FTag in [aitUpdate] then
    begin
      case DoRenameItem(Item) of
        arcOk: begin
          FIsNeededToRun  := TRUE;
          FIsNeededToSave := TRUE;
        end;
        arcCancel: Item.FTag := aitNone;
        arcQuit:   SetExitStatus(esUserAbortError);
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
  for I := 0 to FCentralDirectory.Count - 1 do
    Inc(FTotalSize, FCentralDirectory.Items[I].CompressedSize);
end;

procedure TArchiver.RenameTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags4Rename;
  if FIsNeededToRun then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
    FTempWriter.WriteDWord(ARCHIVE_MARKER);

    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;
      Item := FCentralDirectory.Items[I];
      case Item.FTag of
        aitNone:   DoMessage(Format(cmCopying,  [Item.FileName]));
        aitUpdate: DoMessage(Format(cmRenaming, [Item.FileName]));
        else       SetExitStatus(esCaseError);
      end;

      EncodeFromArchive(Item);
    end;
    if ExitStatus = esNoError then
      FCentralDirectory.Write(FTempWriter);
  end;
end;

// TArchiver # DELETE #

function TArchiver.DoDeleteItem(Item: TArchiveItem): TArchiveConfirm;
begin
  Result := arcCancel;
  if Assigned(FOnDeleteItem) then
  begin
    FOnDeleteItem(Item, Result);
  end;
end;

procedure TArchiver.CheckTags4Delete;
var
  I: longint;
  Item: TArchiveItem;
begin
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;
    Item := FCentralDirectory.Items[I];
    if Item.FTag in [aitUpdate] then
    begin
      case DoDeleteItem(Item) of
        arcOk: begin
          FIsNeededToRun  := TRUE;
          FIsNeededToSave := TRUE;
        end;
        arcCancel: Item.FTag := aitNone;
        arcQuit:   SetExitStatus(esUserAbortError);
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
  I := GetBackTag(FCentralDirectory.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FCentralDirectory.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitNone);
      for J := BackTear to NextTear do
      begin
        Item := FCentralDirectory.Items[J];
        case Item.FTag of
          aitNone:   Item.FTag := aitDecode;
          aitUpdate: Item.FTag := aitDecAndUpd;
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
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    Item := FCentralDirectory.Items[J];
    case Item.FTag of
      aitNone:            Inc(FTotalSize, Item.CompressedSize);
    //aitUpdate:          nothing to do
      aitDecode:          Inc(FTotalSize, Item.UncompressedSize * 2);
      aitDecAndUpd: Inc(FTotalSize, Item.UncompressedSize);
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
    CheckSequences4Delete;
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
    FTempWriter.WriteDWord(ARCHIVE_MARKER);

    if FIsNeededToSwap then Swapping;
    for I := FCentralDirectory.Count - 1 downto 0 do
    begin
      if ExitStatus <> esNoError then Break;
      Item := FCentralDirectory.Items[I];
      case Item.FTag of
        aitUpdate:    DoMessage(Format(cmDeleting, [Item.FileName]));
        aitDecAndUpd: DoMessage(Format(cmDeleting, [Item.FileName]));
      end;

      case Item.FTag of
        aitUpdate:    FCentralDirectory.Delete(I);
        aitDecAndUpd: FCentralDirectory.Delete(I);
      end;
    end;

    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;
      Item := FCentralDirectory.Items[I];
      case Item.FTag of
        aitNone:   DoMessage(Format(cmCopying,  [Item.FileName]));
        aitDecode: DoMessage(Format(cmEncoding, [Item.FileName]));
      end;

      case Item.FTag of
        aitNone:   EncodeFromArchive(Item);
        aitDecode: EncodeFromSwap   (Item);
        else       SetExitStatus(esCaseError);
      end;
    end;
    if ExitStatus = esNoError then
      FCentralDirectory.Write(FTempWriter);
  end;
end;

// TArchiver # UPDATE #

function CompareCustomSearchRec(Item1, Item2: pointer): longint;
begin
  Result := AnsiCompareFileName(
    ExtractFileExt(TCustomSearchRec(Item1).Name),
    ExtractFileExt(TCustomSearchRec(Item2).Name));

  if Result = 0 then
    Result := AnsiCompareFileName(
      ExtractFileName(TCustomSearchRec(Item1).Name),
      ExtractFileName(TCustomSearchRec(Item2).Name));
end;

procedure TArchiver.Configure;
var
  I: longint;
  CurrentBlock: int64;
  CurrentItem: TArchiveItem;
  CurrentTable: TTableParameters;
  CurrentFileExt: string;
  PreviousFileExt: string;
  Configuration: TConfiguration;
begin
  Configuration := TConfiguration.Create;
  if FileExists(ExtractCompressionConfig(FCompressionParams)) then
    Configuration.LoadFromFile(ExtractCompressionConfig(FCompressionParams))
  else
    SetExitStatus(esConfigError);

  CurrentFileExt := '.';
  Configuration.Selector('\main');
  Configuration.CurrentSection.Values['Method']     := IntToStr(ExtractCompressionLevel   (FCompressionParams));
  Configuration.CurrentSection.Values['Dictionary'] := IntToStr(ExtractCompressionAuxLevel(FCompressionParams));
  Configuration.Selector('\m' + Configuration.CurrentSection.Values['Method']);

  CurrentBlock := ExtractCompressionBlock(FCompressionParams);
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    CurrentItem := FCentralDirectory.Items[I];
    if CurrentItem.FTag = aitAdd then
    begin
      // compression method
      Include(CurrentItem.FCompressionFlags, acfCompressionMethod);
      CurrentItem.FCompressionMethod := ExtractCompressionMethod(FCompressionParams);
      // compression level
      Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
      CurrentItem.FCompressionLevel := ExtractCompressionLevel(FCompressionParams);
      // dictionary level
      Include(CurrentItem.FCompressionFlags, acfCompressionLevelAux);
      CurrentItem.FCompressionLevelAux := ExtractCompressionAuxLevel(FCompressionParams);
      // compression CurrentBlock
      Include(CurrentItem.FCompressionFlags, acfCompressionBlock);
      CurrentItem.FCompressionBlock := ExtractCompressionBlock(FCompressionParams);
      // default compression table flag
      Exclude(CurrentItem.FCompressionFlags, acfCompressionFilter);
      CurrentItem.FCompressionFilter := '';
      Exclude(CurrentItem.FCompressionFlags, acfCompressionFilterAux);
      CurrentItem.FCompressionFilterAux := '';
      // force file extension option
      PreviousFileExt := CurrentFileExt;
      if ExtractCompressionFilter(FCompressionParams) = '' then
        CurrentFileExt := ExtractFileExt(CurrentItem.FExternalFileName)
      else
        CurrentFileExt := ExtractCompressionFilter(FCompressionParams);
      // compression block option
      if AnsiCompareFileName(CurrentFileExt, PreviousFileExt) = 0 then
      begin
        Dec(CurrentBlock, CurrentItem.FExternalFileSize);
        if CurrentBlock < 0 then
        begin
          CurrentBlock := ExtractCompressionBlock(FCompressionParams);
          CurrentItem.FCompressionBlock := 0;
        end;
      end else
      begin
        CurrentBlock := ExtractCompressionBlock(FCompressionParams);
        CurrentItem.FCompressionBlock := 0;
      end;
      // BEE compression method
      if CurrentItem.FCompressionMethod = 1 then
      begin
        Include(CurrentItem.FCompressionFlags, acfCompressionFilter);
        if Configuration.GetTable(CurrentFileExt, CurrentTable) then
          CurrentItem.FCompressionFilter := Hex(CurrentTable, SizeOf(CurrentTable))
        else
          CurrentItem.FCompressionFilter := Hex(DefaultTableParameters, SizeOf(CurrentTable));
      end;
      // encryption method
      CurrentItem.FEncryptionMethod    := ExtractEncryptionMethod(FEncryptionParams);
      // check method
      CurrentItem.FCheckMethod         := ExtractCheckMethod   (FCheckParams);
      CurrentItem.FCheckMethodAux      := ExtractCheckAuxMethod(FCheckParams);
      // version needed to read
      CurrentItem.FVersionNeededToRead := GetVersionNeededToRead(CurrentItem);
    end;
  end;
  FreeAndNil(Configuration);
end;

function TArchiver.DoUpdateItem(Item: TCustomSearchRec): TArchiveConfirm;
var
  I: longint;
  UpdateAs: string;
begin
  Result := arcCancel;
  if Assigned(FOnUpdateItem) then
  begin
    UpdateAs := Item.Name;
    FOnUpdateItem(Item, UpdateAs, Result);
    if Result = arcOk then
    begin
      I := IndexOf(UpdateAs);
      if I = -1 then
        I := FCentralDirectory.Add(TArchiveItem.Create(UpdateAs))
      else
        if FCentralDirectory.Items[I].FTag = aitNone then
          FCentralDirectory.Items[I].FTag := aitUpdate;

      FCentralDirectory.Items[I].Update(Item);
      Result := DoCommentItem(FCentralDirectory.Items[I]);
    end;
  end;

  case Result of
    arcOk: begin
      FIsNeededToRun  := TRUE;
      FIsNeededToSave := TRUE;
    end;
  //arcCancel: nothing to do
    arcQuit: SetExitStatus(esUserAbortError);
  end;
end;

procedure TArchiver.CheckTags4Update;
var
  I: longint;
  Item: TCustomSearchRec;
begin
  FCentralDirectoryNews.Sort(CompareCustomSearchRec);
  for I := 0 to FCentralDirectoryNews.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;
    Item := FCentralDirectoryNews.Items[I];
    case DoUpdateItem(Item) of
    //arcOk:     nothing to do
    //arcCancel: nothing to do
      arcQuit:   SetExitStatus(esUserAbortError);
    end;
  end;

  if ExitStatus <> esNoError then Configure;
end;

procedure TArchiver.CheckSequences4Update;
var
  Item: TArchiveItem;
  I, J, BackTear, NextTear: longint;
begin
  // STEP2: find sequences and tag ...
  I := GetBackTag(FCentralDirectory.Count - 1, aitUpdate);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FCentralDirectory.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, aitNone);
      for J := BackTear to NextTear do
      begin
        Item := FCentralDirectory.Items[J];
        case Item.FTag of
          aitNone:            Item.FTag := aitDecode;
          aitUpdate:          Item.FTag := aitDecAndUpd;
        //aitDecode:          nothing to do
        //aitDecAndUpd: nothing to do
        end;
      end;
      I := BackTear;
    end;
    I := GetBackTag(I - 1, aitUpdate);
  end;

  // STEP3: calculate bytes to process ...
  FTotalSize     := 0;
  FProcessedSize := 0;
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    Item := FCentralDirectory.Items[I];
    case Item.FTag of
      aitNone:      Inc(FTotalSize, Item.CompressedSize);
      aitAdd:       Inc(FTotalSize, Item.FExternalFileSize);
      aitUpdate:    Inc(FTotalSize, Item.FExternalFileSize);
      aitDecode:    Inc(FTotalSize, Item.UncompressedSize + Item.UncompressedSize);
      aitDecAndUpd: Inc(FTotalSize, Item.UncompressedSize + Item.FExternalFileSize);
    end;
  end;
end;

procedure TArchiver.UpdateTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags4Update;
  Writeln('Start1');

  if FIsNeededToRun then
  begin
    Writeln('Start2');
    CheckSequences4Update;
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
    FTempWriter.WriteDWord(ARCHIVE_MARKER);

    if FIsNeededToSwap then Swapping;
    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;
      Item := FCentralDirectory.Items[I];
      case Item.FTag of
        aitNone:      DoMessage(Format(cmCopying,  [Item.FileName]));
        aitAdd:       DoMessage(Format(cmAdding,   [Item.FileName]));
        aitUpdate:    DoMessage(Format(cmUpdating, [Item.FileName]));
        aitDecode:    DoMessage(Format(cmEncoding, [Item.FileName]));
        aitDecAndUpd: DoMessage(Format(cmUpdating, [Item.FileName]));
      end;

      case Item.FTag of
        aitNone:      EncodeFromArchive(Item);
        aitAdd:       EncodeFromFile   (Item);
        aitUpdate:    EncodeFromFile   (Item);
        aitDecode:    EncodeFromSwap   (Item);
        aitDecAndUpd: EncodeFromFile   (Item);
        else          SetExitStatus(esCaseError);
      end;
    end;
    if ExitStatus = esNoError then
      FCentralDirectory.Write(FTempWriter);
  end;
end;

end.
