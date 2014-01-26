{
  Copyright (c) 2012-2014 Melchiorre Caruso.

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

{
  Contains:

    Archiver class.

  Modifyed:

    v1.0.0 build 2210 - 2014.01.15 by Melchiorre Caruso.

}

unit bx_archiver;

{$I bx_compiler.inc}

interface

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  Classes,
  DateUtils,
  SysUtils,
  bx_cipher,
  bx_dirscanner,
  bx_filestream,
  bx_stream,
  libbx_bee_common;

const
  /// archive markers
  ARCHIVE_MARKER                      = $30305842;
  ARCHIVE_CENTRALDIR_MARKER           = $30315842;
  ARCHIVE_CENTRALDIR_ITEM_MARKER      = $30335842;
  ARCHIVE_CENTRALDIR_SEEK_MARKER      = $30355842;
  ARCHIVE_CENTRALDIR_MAGIKSEEK_MARKER = $30375842;

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
    acdsfImagesNumber,
    acdsfImageNumber);

  TArchiveCentralDirectorySeekFlags = set of TArchiveCentralDirectorySeekFlag;

  /// archive item flag
  TArchiveItemFlag = (
    aifVersionNeededToRead,
    aifUncompressedSize,
    aifLastModifiedTime,
    aifLastStoredTime,
    aifAttributes,
    aifComment,
    aifLink);

  TArchiveItemFlags = set of TArchiveItemFlag;

  /// archive data descriptor flag
  TArchiveDataDescriptorFlag = (
    addfCompressedSize,
    addfImageNumber,
    addfImageSeek,
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
  TArchiveItemTag = (
    aitUpdate,
    aitDecode);

  TArchiveItemTags = set of TArchiveItemTag;

  /// archive item
  TArchiveItem = class(TObject)
  protected
    FIndex: longint;
    FTags: TArchiveItemTags;
    // item property
    FFileName: string;
    FFlags: TArchiveItemFlags;
    FVersionNeededToRead: longint;
    FUncompressedSize: int64;
    FLastModifiedTime: int64;
    FLastStoredTime: int64;
    FAttributes: longint;
    FComment: string;
    FLink: string;
    // data descriptor property
    FDataDescriptorFlags: TArchiveDataDescriptorFlags;
    FCompressedSize: int64;
    FImageNumber: longint;
    FImageSeek: int64;
    FCheckMethod: longint;
    FCheckDigest: string;
    FCheckMethodAux: longint;
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
    FEncryptionMethod: longint;
    // external file property
    FExternalFileName: string;
    FExternalFileSize: int64;
    procedure InitFlags;
  public {methods}
    constructor Create(const aItemName: string);
    constructor Read(Stream: TFileReader);
    procedure Tag; overload;
    procedure Tag(Rec: TDirScannerItem); overload;
    function  Tagged: boolean;
    procedure UnTag;
    procedure Write(Stream: TFileWriter);
  public {property}
    property Index: longint read FIndex;
    // item property
    property FileName: string read FFileName write FFileName;
    property Flags: TArchiveItemFlags read FFlags;
    property VersionNeededToRead: longint read FVersionNeededToRead;
    property UncompressedSize: int64 read FUncompressedSize;
    property LastModifiedTime: int64 read FLastModifiedTime;
    property LastStoredTime: int64 read FLastStoredTime;
    property Attributes: longint read FAttributes;
    property Comment: string read FComment write FComment;
    property Link: string read FLink;
    // data descriptor property
    property DadaDescriptorFlags: TArchiveDataDescriptorFlags read FDataDescriptorFlags;
    property CompressedSize: int64 read FCompressedSize;
    property ImageNumber: longint read FImageNumber;
    property ImageSeek: int64 read FImageSeek;
    property CheckMethod: longint read FCheckMethod;
    property CheckDigest: string read FCheckDigest;
    property CheckMethodAux: longint read FCheckMethodAux;
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
    property EncryptionMethod: longint read FEncryptionMethod;
    // external file property
    property ExternalFileName: string read FExternalFileName write FExternalFileName;
    property ExternalFileSize: int64  read FExternalFileSize write FExternalFileSize;
  end;

  /// archive item finder
  TArchiveItemFinder = class(TObject)
  private
    FItems: TList;
    function GetCount: longint;
    function GetItem(Index: longint): TArchiveItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TArchiveItem): TArchiveItem;
    procedure Clear;
    procedure Delete(Index: longint);
    function Find(const FileName: string): longint;
  public
    property Count: longint read GetCount;
    property Items[Index: longint]: TArchiveItem read GetItem;
  end;

  /// archive central directory
  TArchiveCentralDirectory = class(TObject)
  private
    FItems: TList;
    // central directory property
    FFlags: TArchiveCentralDirectoryFlags;
    FLastModifiedTime: int64;
    FComment: string;
    // central directory seek property
    FSeekFlags: TArchiveCentralDirectorySeekFlags;
    FImagesNumber: longint;
    FImageNumber: longint;
    FImageSeek: int64;
    // central directory magik seek property
    FMagikSeek: longint;
  private
    function GetCount: longint;
    function GetItem(Index: longint): TArchiveItem;

    procedure Pack;
    procedure UnPack;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TArchiveItem);
    procedure Clear;
    procedure Delete(Index: longint);
    procedure Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
  public
    property Comment: string read FComment write FComment;
    property Count: longint read GetCount;
    property Items[Index: longint]: TArchiveItem read GetItem;
    property LastModifiedTime: int64 read FLastModifiedTime;
  end;

  /// archive events

  TMessageEvent    = procedure(const Msg: string) of object;

  TPercentageEvent = procedure(Percentage: longint) of object;

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
    FCompressionParams: string;
    FEncryptionParams: string;
    FHashingParams: string;
    FPassword: string;
    FTestTempArchive: boolean;
    FVerboseMode: boolean;
    FVolumeSize: int64;
    FWorkDirectory: string;
    // central directory
    FCentralDirectory: TArchiveCentralDirectory;
    // events
    FOnMessage: TMessageEvent;
    FOnPercentage: TPercentageEvent;
    FOnRequestBlankDisk: TFileWriterRequestBlankDiskEvent;
    FOnRequestImage: TFileReaderRequestImageEvent;
  private
    procedure DoMessage(const Message: string);
    procedure DoPercentage;
    procedure DoRequestBlankDisk(ImageNumber: longint; var Abort : Boolean);
    procedure DoRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);

    function GetComment: string;
    function GetCount: longint;
    function GetItem    (Index: longint): TArchiveItem;
    function GetLastModifiedTime: int64;

    procedure SetComment(const Value: string);
    procedure SetArchiveName(const Value: string);
    procedure SetSfxName(const Value: string);
    procedure SetWorkDirectory(const Value: string);

    function GetBackTag (Index: longint; aTags: TArchiveItemTags): longint;
    function GetBackTear(Index: longint): longint;
    function GetNextTag (Index: longint; aTags: TArchiveItemTags): longint;
    function GetNextTear(Index: longint): longint;

    procedure xxcode(Reader: TBufStream; Writer: TBufStream; const Size: int64); overload;
    procedure EncodeFromArchive(Item: TArchiveItem);
    procedure EncodeFromSwap   (Item: TArchiveItem);
    procedure EncodeFromFile   (Item: TArchiveItem);
    procedure DecodeToNul      (Item: TArchiveItem);
    procedure DecodeToSwap     (Item: TArchiveItem);
    procedure DecodeToFile     (Item: TArchiveItem);

    procedure CheckSequences4Delete;
    procedure CheckSequences4Extract;
    procedure CheckSequences4Rename;
    procedure CheckSequences4Update;
    procedure CheckTags;
    procedure Configure;
    procedure SaveTemporaryArchive;
    procedure Swapping;
    procedure TestTemporaryArchive;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenArchive(const aArchiveName: string);
    procedure CloseArchive;

    procedure ExtractTagged;
    procedure DeleteTagged;
    procedure RenameTagged;
    procedure TestTagged;
    procedure UpdateTagged;

    function Add(Item: TArchiveItem): TArchiveItem;

    procedure Suspend(Value: boolean);
    procedure Terminate;
  public
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property Comment: string read GetComment write SetComment;
    property Count: longint read GetCount;
    property CompressionParams: string read FCompressionParams write FCompressionParams;
    property EncryptionParams: string read FEncryptionParams write FEncryptionParams;
    property HashingParams: string read FHashingParams write FHashingParams;
    property Items[Index: longint]: TArchiveItem read GetItem;
    property LastModifiedTime: int64 read GetLastModifiedTime;
    property Password: string read FPassword write FPassword;
    property SelfExtractor: string read FSfxName write FSfxName;
    property TestTempArchive: boolean read FTestTempArchive write FTestTempArchive;
    property VolumeSize: int64 read FVolumeSize write FVolumeSize;
    property VerboseMode: boolean read FVerboseMode write FVerboseMode;
    property WorkDirectory: string read FWorkDirectory write SetWorkDirectory;
  public
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnPercentage: TPercentageEvent read FOnPercentage write FOnPercentage;
    property OnRequestBlankDisk: TFileWriterRequestBlankDiskEvent read FOnRequestBlankDisk write FOnRequestBlankDisk;
    property OnRequestImage: TFileReaderRequestImageEvent read FOnRequestImage write FOnRequestImage;
  end;

function HashingMethodToStr(Method: longint): string;
function CompressionMethodToStr (Method: longint): string;
function EncryptionMethodToStr(Method: longint): string;

function AttrToStr(Attr: longint): string;
function RatioToStr(const PackedSize, Size: int64): string;
function SizeToStr(const Size: int64): string;
function VersionToStr(Version: longword): string;

implementation

uses
  bx_CommandLine,
  bx_Common,
  bx_Messages;

function GetVersionNeededToRead(Item: TArchiveItem): longword; overload;
begin
  Result := 100;
end;

function GetVersionNeededToRead(Flags: TArchiveCentralDirectoryFlags): longword; overload;
begin
  Result := 100;
end;

function GetVersionNeededToRead(Flags: TArchiveCentralDirectorySeekFlags): longword; overload;
begin
  Result := 100;
end;

// ---

function HashingMethodToStr(Method: longint): string;
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

function CompressionMethodToStr(Method: longint): string;
begin
  Result := '???';
  case Method of
    0:  Result := 'STORE';
    1:  Result := 'BEE';
    2:  Result := 'PPMD';
  end;
end;

function EncryptionMethodToStr(Method: longint): string;
begin
  Result := '???';
  case Method of
    0: Result := 'NONE';
    1: Result := 'BLOWFISH';
    2: Result := 'IDEA';
  end;
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

function RatioToStr(const PackedSize, Size: int64): string;
begin
  if Size > 0 then
    Result := Format('%u%%', [Round((PackedSize / Size) * 100)])
  else
    Result := Format('%u%%', [100]);
end;

function SizeToStr(const Size: int64): string;
begin
  Result := Format('%u', [Size]);
end;

function VersionToStr(Version: longword): string;
begin
  Result := '?.?.?';
  case Version of
    100:  Result := '1.0.0';
  end;
end;

// TArchiveItem class

constructor TArchiveItem.Create(const aItemName: string);
begin
  inherited Create;
  FFileName             := aItemName;
  /// item property ///
  FVersionNeededToRead  :=  0;
  FUncompressedSize     :=  0;
  FLastModifiedTime     :=  0;
  FLastStoredTime       :=  0;
  FAttributes           :=  0;
  FComment              := '';
  FLink                 := '';
  /// data descriptor property ///
  FCompressedSize       :=  0;
  FImageNumber          :=  0;
  FImageSeek            :=  0;
  FCheckMethod          :=  0;
  FCheckDigest          := '';
  FCheckMethodAux       :=  0;
  FCheckDigestAux       := '';
  /// compression property ///
  FCompressionMethod    :=  0;
  FCompressionLevel     :=  0;
  FCompressionLevelAux  :=  0;
  FCompressionFilter    := '';
  FCompressionFilterAux := '';
  FCompressionBlock     :=  0;
  /// encryption property ///
  FEncryptionMethod     :=  0;
  /// reserved property ///
  FExternalFileName     := '';
  FExternalFileSize     :=  0;
  FTags                 := [];
end;

constructor TArchiveItem.Read(Stream: TFileReader);
begin
  FFileName := Stream.ReadInfString;
  /// item property ///
  FFlags  := TArchiveItemFlags(longword(Stream.ReadInfWord));
  if (aifVersionNeededToRead in FFlags) then FVersionNeededToRead := Stream.ReadInfWord;
  if (aifUncompressedSize    in FFlags) then FUncompressedSize    := Stream.ReadInfWord;
  if (aifLastModifiedTime    in FFlags) then FLastModifiedTime    := UnixToFileTime(Stream.ReadInfWord);
  if (aifLastStoredTime      in FFlags) then FLastStoredTime      := UnixToFileTime(Stream.ReadInfWord);
  if (aifAttributes          in FFlags) then FAttributes          := Stream.ReadInfWord;
  if (aifComment             in FFlags) then FComment             := Stream.ReadInfString;
  if (aifLink                in FFlags) then FLink                := Stream.ReadInfString;
  /// data descryptor property ///
  FDataDescriptorFlags := TArchiveDataDescriptorFlags(longword(Stream.ReadInfWord));
  if (addfCompressedSize in FDataDescriptorFlags) then FCompressedSize := Stream.ReadInfWord;
  if (addfImageNumber    in FDataDescriptorFlags) then FImageNumber    := Stream.ReadInfWord;
  if (addfImageSeek      in FDataDescriptorFlags) then FImageSeek      := Stream.ReadInfWord;
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
  if (aefEncryptionMethod in FEncryptionFlags) then FEncryptionMethod := Stream.ReadInfWord;
  /// reserved property ///
  FExternalFileName := '';
  FExternalFileSize :=  0;
  FTags             := [];
end;

procedure TArchiveItem.InitFlags;
begin
  FFlags := [
    aifVersionNeededToRead,
    aifUncompressedSize,
    aifLastModifiedTime,
    aifLastStoredTime,
    aifAttributes,
    aifComment,
    aifLink];

  FDataDescriptorFlags := [
    addfCompressedSize,
    addfImageNumber,
    addfImageSeek,
    addfCheckMethod,
    addfCheckDigest,
    addfCheckMethodAux,
    addfCheckDigestAux];

  FCompressionFlags := [
    acfCompressionMethod,
    acfCompressionLevel,
    acfCompressionLevelAux,
    acfCompressionFilter,
    acfCompressionFilterAux,
    acfCompressionBlock];

  FEncryptionFlags := [
    aefEncryptionMethod];
end;

procedure TArchiveItem.Tag;
begin
  Include(FTags, aitUpdate);
end;

procedure TArchiveItem.Tag(Rec: TDirScannerItem);
begin
  /// item property ///
  FLastModifiedTime := Rec.FileTime;
  FLastStoredTime   := DateTimeToFileDate(Now);
  FAttributes       := Rec.FileAttr;
  /// reserved property ///
  FExternalFileName := Rec.FileName;
  FExternalFileSize := Rec.FileSize;
  /// tag ///
  Include(FTags, aitUpdate);
end;

function TArchiveItem.Tagged: boolean;
begin
  Result := aitUpdate in FTags;
end;

procedure TArchiveItem.UnTag;
begin
  Exclude(FTags, aitUpdate);
end;

procedure TArchiveItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfString(FFileName);
  /// item property ///
  Stream.WriteInfWord(longword(FFlags));
  if (aifVersionNeededToRead in FFlags) then Stream.WriteInfWord(FVersionNeededToRead);
  if (aifUncompressedSize    in FFlags) then Stream.WriteInfWord(FUncompressedSize);
  if (aifLastModifiedTime    in FFlags) then Stream.WriteInfWord(FileTimeToUnix(FLastModifiedTime));
  if (aifLastStoredTime      in FFlags) then Stream.WriteInfWord(FileTimeToUnix(FLastStoredTime));
  if (aifAttributes          in FFlags) then Stream.WriteInfWord(FAttributes);
  if (aifComment             in FFlags) then Stream.WriteInfString(FComment);
  if (aifLink                in FFlags) then Stream.WriteInfString(FLink);
  /// data descriptor property ///
  Stream.WriteInfWord(longword(FDataDescriptorFlags));
  if (addfCompressedSize in FDataDescriptorFlags) then Stream.WriteInfWord(FCompressedSize);
  if (addfImageNumber    in FDataDescriptorFlags) then Stream.WriteInfWord(FImageNumber);
  if (addfImageSeek      in FDataDescriptorFlags) then Stream.WriteInfWord(FImageSeek);
  if (addfCheckMethod    in FDataDescriptorFlags) then Stream.WriteInfWord(FCheckMethod);
  if (addfCheckDigest    in FDataDescriptorFlags) then Stream.WriteInfArray(FCheckDigest);
  if (addfCheckMethodAux in FDataDescriptorFlags) then Stream.WriteInfWord(FCheckMethodAux);
  if (addfCheckDigestAux in FDataDescriptorFlags) then Stream.WriteInfArray(FCheckDigestAux);
  /// compression property ///
  Stream.WriteInfWord(longword(FCompressionFlags));
  if (acfCompressionMethod    in FCompressionFlags) then Stream.WriteInfWord(FCompressionMethod);
  if (acfCompressionLevel     in FCompressionFlags) then Stream.WriteInfWord(FCompressionLevel);
  if (acfCompressionLevelAux  in FCompressionFlags) then Stream.WriteInfWord(FCompressionLevelAux);
  if (acfCompressionFilter    in FCompressionFlags) then Stream.WriteInfArray(FCompressionFilter);
  if (acfCompressionFilterAux in FCompressionFlags) then Stream.WriteInfArray(FCompressionFilterAux);
  if (acfCompressionBlock     in FCompressionFlags) then Stream.WriteInfWord(FCompressionBlock);
  /// encryption property ///
  Stream.WriteInfWord(longword(FEncryptionFlags));
  if (aefEncryptionMethod in FEncryptionFlags) then Stream.WriteInfWord(FEncryptionMethod);
end;

// TArchiveItemFinder class

constructor TArchiveItemFinder.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TArchiveItemFinder.Destroy;
begin
  Clear;
  FItems.Destroy;
  inherited Destroy;
end;

procedure TArchiveItemFinder.Clear;
begin
  FItems.Clear;
end;

procedure TArchiveItemFinder.Delete(Index: longint);
begin
  FItems.Delete(Index);
end;

function TArchiveItemFinder.Add(Item: TArchiveItem): TArchiveItem;
var
  L, M, H, I: longint;
begin
  Result := Item;

  if FItems.Count <> 0 then
  begin
    L := 0;
    H := FItems.Count - 1;
    while H >= L do
    begin
      M := (L + H) div 2;
      I := AnsiCompareFileName(
        TArchiveItem(FItems[M]).FFileName, Item.FFileName);
      if I < 0 then
        L := M + 1
      else
        if I > 0 then
          H := M - 1
        else
          H := -2;
    end;

    if I < 0 then
      FItems.Insert(M + 1, Item)
    else
      if I > 0 then
        FItems.Insert(M, Item)
      else
        SetExitStatus(esArchiveCDError);

  end else
    FItems.Add(Item);
end;

function TArchiveItemFinder.Find(const FileName: string): longint;
var
  L, M, H, I: longint;
begin
  L := 0;
  H := FItems.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := AnsiCompareFileName(
      TArchiveItem(FItems[M]).FFileName, FileName);
    if I < 0 then
      L := M + 1
    else
      if I > 0 then
        H := M - 1
      else
        H := -2;
  end;

  if H = -2 then
    Result := M
  else
    Result := -1;
end;

function TArchiveItemFinder.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TArchiveItemFinder.GetItem(Index: longint): TArchiveItem;
begin
  Result := TArchiveItem(FItems[Index]);
end;

// TArchiveCentralDirectory class

constructor TArchiveCentralDirectory.Create;
begin
  inherited Create;
  FItems := TList.Create;
  Clear;
end;

destructor TArchiveCentralDirectory.Destroy;
begin
  Clear;
  FItems.Destroy;
  inherited Destroy;
end;

procedure TArchiveCentralDirectory.Clear;
var
  I: longint;
begin
  for I := 0 to FItems.Count - 1 do
    TArchiveItem(FItems[I]).Destroy;
  FItems.Clear;

  FLastModifiedTime :=  0;
  FComment := '';
end;

procedure TArchiveCentralDirectory.Pack;
var
  I: longint;
  CurrentItem: TArchiveItem;
  PreviusItem: TArchiveItem;
begin
  if FItems.Count > 0 then
  begin
    TArchiveItem(FItems[0]).InitFlags;
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
        if CurrentItem.FLink                 = PreviusItem.FLink                 then Exclude(CurrentItem.FFlags, aifLink)                else Include(CurrentItem.FFlags, aifLink);
        /// data descriptor property ///
        if CurrentItem.FCompressedSize       = PreviusItem.FCompressedSize       then Exclude(CurrentItem.FDataDescriptorFlags, addfCompressedSize) else Include(CurrentItem.FDataDescriptorFlags, addfCompressedSize);
        if CurrentItem.FImageNumber          = PreviusItem.FImageNumber          then Exclude(CurrentItem.FDataDescriptorFlags, addfImageNumber)    else Include(CurrentItem.FDataDescriptorFlags, addfImageNumber);
        if CurrentItem.FImageSeek            = PreviusItem.FImageSeek            then Exclude(CurrentItem.FDataDescriptorFlags, addfImageSeek)      else Include(CurrentItem.FDataDescriptorFlags, addfImageSeek);
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
      if not(aifLink                in CurrentItem.FFlags) then CurrentItem.FLink                := PreviusItem.FLink;
      /// data descryptor property ///
      if not(addfCompressedSize in CurrentItem.FDataDescriptorFlags) then CurrentItem.FCompressedSize := PreviusItem.FCompressedSize;
      if not(addfImageNumber    in CurrentItem.FDataDescriptorFlags) then CurrentItem.FImageNumber    := PreviusItem.FImageNumber;
      if not(addfImageSeek      in CurrentItem.FDataDescriptorFlags) then CurrentItem.FImageSeek      := PreviusItem.FImageSeek;
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

procedure TArchiveCentralDirectory.Add(Item: TArchiveItem);
begin
  Item.FIndex := FItems.Add(Item);
end;

procedure TArchiveCentralDirectory.Delete(Index: longint);
var
  Item: TArchiveItem;
begin
  Item := Items[Index];
  if Item.CompressionBlock = 0 then
    if Index < FItems.Count - 1 then
      Items[Index + 1].FCompressionBlock := 0;

  FItems.Delete(Index);
  FreeandNil(Item);
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
  CDSFULL = [acdsfVersionNeededToRead, acdsfImagesNumber,    acdsfImageNumber];
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
        FImagesNumber := 1;
        if (acdsfImagesNumber in FSeekFlags) then
          FImagesNumber := Stream.ReadInfWord;

        FImageNumber  := 1;
        if (acdsfImageNumber in FSeekFlags) then
          FImageNumber := Stream.ReadInfWord;

        FImageSeek := Stream.ReadInfWord;
        // [4.1] seek on central directory marker
        Stream.ImagesNumber := FImagesNumber;
        Stream.ImageNumber  := FImageNumber;
        Stream.Seek           (FImageSeek, fsFromBeginning);
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
    end;

  // [6] read central directory items
  if ExitStatus = esNoError then
  begin
    MARKER := Stream.ReadDWord;
    while MARKER = ARCHIVE_CENTRALDIR_ITEM_MARKER do
    begin
      Add(TArchiveItem.Read(Stream));
      MARKER := Stream.ReadDWord;
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
  FImageSeek   := Stream.Seek(0, fsFromCurrent);
  FImageNumber := Stream.CurrentImage;
  if FImageNumber <> 1 then
    Include(FSeekFlags,  acdsfImageNumber);

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
      Stream.WriteDWord(ARCHIVE_CENTRALDIR_ITEM_MARKER);
      TArchiveItem(FItems[I]).Write(Stream);
    end;

  // [3] multi-spanning support
  if Stream.Threshold > 0 then
    if (Stream.Threshold - Stream.Seek(0, fsFromCurrent)) < 1024 then
    begin
      Stream.CreateNewImage;
    end;

  FImagesNumber := Stream.CurrentImage;
  if FImagesNumber <> 1 then
    Include(FSeekFlags,  acdsfImagesNumber);

  FMagikSeek := Stream.Seek(0, fsFromCurrent);
  // [4] write central directory seek
  Stream.WriteDWord(ARCHIVE_CENTRALDIR_SEEK_MARKER);
  Stream.WriteInfWord(longword(FSeekFlags));
  if (acdsfVersionNeededToRead in FSeekFlags) then
    Stream.WriteInfWord(GetVersionNeededToRead(FSeekFlags));
  if (acdsfImagesNumber in FSeekFlags) then
    Stream.WriteInfWord(FImagesNumber);
  if (acdsfImageNumber in FSeekFlags) then
    Stream.WriteInfWord(FImageNumber);
  Stream.WriteInfWord(FImageSeek);

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
  FHashingParams       := '';
  FCompressionParams := '';
  FEncryptionParams  := '';
  FPassword          := '';
  FTestTempArchive   := FALSE;
  FVerboseMode       := FALSE;
  FVolumeSize        := 0;
  FWorkDirectory     := '';
  // central direcotry items
  FCentralDirectory  := TArchiveCentralDirectory.Create;
end;

destructor TArchiver.Destroy;
begin
  FCentralDirectory.Destroy;
  inherited Destroy;
end;

// TArchiver # ENCODE/DECODE #

procedure TArchiver.xxcode(Reader: TBufStream; Writer: TBufStream; const Size: int64);
var
  Buffer: TBuffer;
  Count_: int64;
  X: double;
begin
  X := Time;
  Reader.StartSession;
  Writer.StartSession;
  Count_ := Size div SizeOf(Buffer);
  while (Count_ > 0) and (ExitStatus = esNoError) do
  begin
    Reader.Decode(@Buffer[0], SizeOf(Buffer));
    Writer.Encode(@Buffer[0], SizeOf(Buffer));
    Inc(FProcessedSize, SizeOf(Buffer));

    if FVerboseMode = TRUE then
      if MilliSecondsBetween(X, Time) > 1000 then
      begin
        while FSuspended do Sleep(250);
        DoPercentage;
        X := Time;
      end;
    Dec(Count_);
  end;
  Count_ := Size mod SizeOf(Buffer);
  if Count_ > 0 then
  begin
    Reader.Decode(@Buffer[0], Count_);
    Writer.Encode(@Buffer[0], Count_);
    Inc(FProcessedSize, Count_);
  end;
  Writer.EndSession;
  Reader.EndSession;
end;

procedure TArchiver.EncodeFromArchive(Item: TArchiveItem);
begin
  if (Item.Attributes and (faDirectory or faSymLink or faVolumeID)) = 0 then
  begin
    FArchiveReader.Seek(Item.FImageSeek, fsFromBeginning, Item.FImageNumber);
    Item.FImageSeek             := FTempWriter.Seek(0, fsFromCurrent);
    Item.FImageNumber           := FTempWriter.CurrentImage;

    FArchiveReader.HashMethod   := 0;
    FArchiveReader.CipherMethod := 0;
    FArchiveReader.CoderMethod  := 0;

    FTempWriter.HashMethod      := 0;
    FTempWriter.CipherMethod    := 0;
    FTempWriter.CoderMethod     := 0;
    begin
      xxcode(FArchiveReader, FTempWriter, Item.FCompressedSize);
    end;
  end;
end;

procedure TArchiver.EncodeFromSwap(Item: TArchiveItem);
begin
  if (Item.Attributes and (faDirectory or faSymLink or faVolumeID)) = 0 then
  begin
    FSwapReader.Seek(Item.FImageSeek, fsFromBeginning, Item.FImageNumber);
    Item.FImageSeek            := FTempWriter.Seek(0, fsFromCurrent);
    Item.FImageNumber          := FTempWriter.CurrentImage;

    FSwapReader.HashMethod     := Item.CheckMethod;
    FSwapReader.CipherMethod   := Item.EncryptionMethod;
    FSwapReader.CipherPassword := FPassword;
    FSwapReader.CoderMethod    := 0;

    FTempWriter.HashMethod     := Item.CheckMethod;
    FTempWriter.CipherMethod   := Item.EncryptionMethod;
    FTempWriter.CipherPassword := FPassword;
    FTempWriter.CoderMethod    := Item.CompressionMethod;
    FTempWriter.CoderLevel     := Item.CompressionLevel;
    FTempWriter.CoderLevelAux  := Item.CompressionLevelAux;
    FTempWriter.CoderFilter    := Item.CompressionFilter;
    FTempWriter.CoderFilterAux := Item.CompressionFilterAux;
    FTempWriter.CoderBlock     := Item.CompressionBlock;
    begin
      xxcode(FSwapReader, FTempWriter, Item.FUnCompressedSize);
    end;
    //Item.FCheckDigest        := FSwapReader.HashDigest;
    //Item.FCheckDigestAux     := FTempWriter.HashDigest;
    Item.FCompressedSize       := FTempWriter.Seek(0, fsFromCurrent) - Item.FImageSeek;
    //Item.FUncompressedSize   := Item.FExternalFileSize;
  end;
end;

procedure TArchiver.EncodeFromFile(Item: TArchiveItem);
var
  Source: TFileReader;
begin
  if (Item.Attributes and (faDirectory or faSymLink or faVolumeID)) = 0 then
  begin
    Source := TFileReader.Create(Item.FExternalFileName, nil);
    Item.FImageSeek            := FTempWriter.Seek(0, fsFromCurrent);
    Item.FImageNumber          := FTempWriter.CurrentImage;

    Source.HashMethod          := Item.CheckMethod;
    Source.CipherMethod        := 0;
    Source.CoderMethod         := 0;

    FTempWriter.HashMethod     := Item.CheckMethodAux;
    FTempWriter.CipherMethod   := Item.EncryptionMethod;
    FTempWriter.CipherPassword := ExtractEncryptionPassword(EncryptionParams);
    FTempWriter.CoderMethod    := Item.CompressionMethod;
    FTempWriter.CoderLevel     := Item.CompressionLevel;
    FTempWriter.CoderLevelAux  := Item.CompressionLevelAux;
    FTempWriter.CoderFilter    := Item.CompressionFilter;
    FTempWriter.CoderFilterAux := Item.CompressionFilterAux;
    FTempWriter.CoderBlock     := Item.CompressionBlock;
    begin
      xxcode(Source, FTempWriter, Item.FExternalFileSize);
    end;
    Item.FCheckDigest          :=      Source.HashDigest;
    Item.FCheckDigestAux       := FTempWriter.HashDigest;
    Item.FCompressedSize       := FTempWriter.Seek(0, fsFromCurrent) - Item.FImageSeek;
    Item.FUncompressedSize     := Item.FExternalFileSize;

    FreeAndNil(Source);
  end else
    if (Item.FAttributes and (faDirectory)) = 0 then
    begin
      // nothing to do
    end else
      if (Item.FAttributes and (faSymLink)) = 0 then
      begin
        Item.FLink := fpReadLink(Item.FExternalFileName);
      end else
        SetExitStatus(esUnknowError);
end;

procedure TArchiver.DecodeToSwap(Item: TArchiveItem);
begin
  if (Item.Attributes and (faDirectory or faSymLink or faVolumeID)) = 0 then
  begin
    FArchiveReader.Seek(Item.ImageSeek, fsFromBeginning, Item.ImageNumber);
    Item.FImageSeek   := FSwapWriter.Seek(0, fsFromCurrent);
    Item.FImageNumber := FSwapWriter.CurrentImage;

    FArchiveReader.HashMethod     := Item.CheckMethod;
    FArchiveReader.CipherMethod   := Item.EncryptionMethod;
    FArchiveReader.CipherPassword := FPassword;
    FArchiveReader.CoderMethod    := Item.CompressionMethod;
    FArchiveReader.CoderLevel     := Item.CompressionLevel;
    FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
    FArchiveReader.CoderFilter    := Item.CompressionFilter;
    FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
    FArchiveReader.CoderBlock     := Item.CompressionBlock;

    FSwapWriter.HashMethod        := Item.CheckMethod;
    FSwapWriter.CipherMethod      := Item.EncryptionMethod;
    FSwapWriter.CipherPassword    := FPassword;
    FSwapWriter.CoderMethod       := 0;
    begin
      xxcode(FArchiveReader, FSwapWriter, Item.UncompressedSize);
    end;

    if Item.CheckMethod <> 0 then
      if Item.CheckDigest <> FSwapWriter.HashDigest then
        SetExitStatus(esHashError);
  end;
end;

procedure TArchiver.DecodeToNul(Item: TArchiveItem);
var
  Count_: int64;
  Buffer: TBuffer;
  Destination: TNulBufStream;
begin
  if (Item.Attributes and (faDirectory or faSymLink or faVolumeID)) = 0 then
  begin
    Destination := TNulBufStream.Create;
    FArchiveReader.Seek(Item.FImageSeek, fsFromBeginning, Item.FImageNumber);

    Destination.HashMethod        := Item.CheckMethod;
    Destination.CipherMethod      := 0;
    Destination.CoderMethod       := 0;

    FArchiveReader.HashMethod     := 0;
    FArchiveReader.CipherMethod   := Item.EncryptionMethod;
    FArchiveReader.CipherPassword := FPassword;
    FArchiveReader.CoderMethod    := Item.CompressionMethod;
    FArchiveReader.CoderLevel     := Item.CompressionLevel;
    FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
    FArchiveReader.CoderFilter    := Item.CompressionFilter;
    FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
    FArchiveReader.CoderBlock     := Item.CompressionBlock;
    begin
      xxcode(FArchiveReader, Destination, Item.FUncompressedSize);
    end;

    if Item.CheckMethod <> 0 then
      if Item.FCheckDigest <> Destination.HashDigest then
      begin
        SetExitStatus(esHashError);
      end;
    FreeAndNil(Destination);
  end;
end;

procedure TArchiver.DecodeToFile(Item: TArchiveItem);
var
  Destination: TFileWriter;
begin
  if (Item.Attributes and (faDirectory or faSymLink or faVolumeID)) = 0 then
  begin
    Destination := TFileWriter.Create(Item.FExternalFileName, FOnRequestBlankDisk, 0);
    FArchiveReader.Seek(Item.FImageSeek, fsFromBeginning, Item.FImageNumber);

    Destination.HashMethod        := Item.CheckMethod;
    Destination.CipherMethod      := 0;
    Destination.CoderMethod       := 0;

    FArchiveReader.HashMethod     := 0;
    FArchiveReader.CipherMethod   := Item.EncryptionMethod;
    FArchiveReader.CipherPassword := FPassword;
    FArchiveReader.CoderMethod    := Item.CompressionMethod;
    FArchiveReader.CoderLevel     := Item.CompressionLevel;
    FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
    FArchiveReader.CoderFilter    := Item.CompressionFilter;
    FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
    FArchiveReader.CoderBlock     := Item.CompressionBlock;
    begin
      xxcode(FArchiveReader, Destination, Item.UncompressedSize);
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
  end else
    if (Item.FAttributes and (faDirectory)) = 0 then
    begin
      ForceDirectories(Item.ExternalFileName);
    end else
      if (Item.FAttributes and (faSymLink)) = 0 then
      begin
        fpSymLink(PChar(Item.Link), PChar(Item.ExternalFileName));
      end else
        SetExitStatus(esUnknowError);
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
  FSwapWriter := TFileWriter.Create(FSwapName, FOnRequestBlankDisk, 0);
  FSwapWriter.WriteDWord(ARCHIVE_MARKER);
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;

    Item := FCentralDirectory.Items[I];
    if Item.FTags = [aitDecode] then
    begin
      DoMessage(Format(cmSwapping, [I + 1, Item.FFileName]));
      DecodeToSwap(Item);
    end else
      if Item.FTags = [aitDecode, aitUpdate] then
      begin
        DoMessage(Format(cmDecoding, [I + 1, Item.FFileName]));
        DecodeToNul(Item);
      end;
  end;
  FreeAndNil(FSwapWriter);

  if ExitStatus = esNoError then
    FSwapReader := TFileReader.Create(FSwapName, FOnRequestImage);
end;

procedure TArchiver.TestTemporaryArchive;
var
  I: longint;
  Tester: TArchiver;
begin
  if ExitStatus = esNoError then
  begin
    Tester := TArchiver.Create;
    Tester.OnRequestBlankDisk := OnRequestBlankDisk;
    Tester.OnRequestImage     := OnRequestImage;
    Tester.OnPercentage       := OnPercentage;
    Tester.OnMessage          := OnMessage;

    Tester.EncryptionParams   := FEncryptionParams;

    Tester.OpenArchive(FTempName);
    if ExitStatus = esNoError then
    begin
      for I := 0 to Tester.Count - 1 do
      begin
        Tester.Items[I].Tag;
      end;
      Tester.TestTagged;
    end;
    Tester.CloseArchive;
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
    if FVolumeSize > 0 then
    begin
      FTotalSize     := 0;
      FProcessedSize := 0;
      for I := 0 to FCentralDirectory.Count - 1 do
        Inc(FTotalSize, FCentralDirectory.Items[I].CompressedSize);

      FArchiveReader := TFileReader.Create(FTempName, FOnRequestImage);
      FTempWriter    := TFileWriter.Create(FArchiveName, FOnRequestBlankDisk, FVolumeSize);
      FTempWriter.WriteDWord(ARCHIVE_MARKER);
      for I := 0 to FCentralDirectory.Count - 1 do
      begin
        if ExitStatus <> esNoError then Break;

        Item := FCentralDirectory.Items[I];
        DoMessage(Format(cmSplitting, [I + 1, Item.FileName]));
        EncodeFromArchive(Item);
      end;
      if ExitStatus = esNoError then
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
  begin
    SysUtils.DeleteFile(FTempName);
  end;
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
  FCentralDirectory.Clear;

  FArchiveName    := '';
  FSwapName       := '';
  FTempName       := '';

  FSuspended      := FALSE;
  FIsNeededToRun  := FALSE;
  FIsNeededToSwap := FALSE;
  FIsNeededToSave := FALSE;
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

// TArchiver # GET PROPERTY #

function TArchiver.GetBackTag(Index: longint; aTags: TArchiveItemTags): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index downto 0 do
    if FCentralDirectory.Items[I].FTags = aTags then
    begin
      Result := I;
      Break;
    end;
end;

function TArchiver.GetNextTag(Index: longint; aTags: TArchiveItemTags): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index to FCentralDirectory.Count - 1 do
    if FCentralDirectory.Items[I].FTags = aTags then
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

procedure TArchiver.SetSfxName(const Value: string);
begin
  FSfxName := Value;
end;

procedure TArchiver.SetComment(const Value: string);
begin
  FCentralDirectory.Comment := Value;
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

procedure TArchiver.DoRequestBlankDisk(ImageNumber: longint; var Abort : Boolean);
begin
  Abort := TRUE;
  if Assigned(FOnRequestBlankDisk) then
    FOnRequestBlankDisk(ImageNumber, Abort);
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

procedure TArchiver.DoPercentage;
begin
  if Assigned(FOnPercentage) then
    FOnPercentage(Round((FProcessedSize/FTotalSize)*100));
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

function TArchiver.Add(Item: TArchiveItem): TArchiveItem;
begin
  Result := Item;
  begin
    FCentralDirectory.Add(Item);
  end;
end;

procedure TArchiver.CheckTags;
var
  I: longint;
  Item: TArchiveItem;
begin
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    Item := FCentralDirectory.Items[I];
    if aitUpdate in Item.FTags then
    begin
      FIsNeededToRun := TRUE;
    end;
    Exclude(Item.FTags, aitDecode);
  end;

end;

// TArchiver # EXTRACT #

procedure TArchiver.CheckSequences4Extract;
var
  I, J, BackTear, NextTear: longint;
  Item: TArchiveItem;
begin
  // STEP2: find sequences and tag ...
  I := GetBackTag(FCentralDirectory.Count - 1, [aitUpdate]);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FCentralDirectory.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, [aitUpdate]);
      for J := BackTear to NextTear do
      begin
        Item := FCentralDirectory.Items[J];
        if Item.FTags = [] then
          Include(Item.FTags, aitDecode);
      end;
      I := BackTear;
    end;
    I := GetBackTag(I - 1, [aitUpdate]);
  end;

  // STEP2: calculate bytes to process ...
  FTotalSize     := 0;
  FProcessedSize := 0;
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    Item := FCentralDirectory.Items[I];
    if Item.FTags = [aitUpdate] then
      Inc(FTotalSize, Item.UncompressedSize)
    else
      if Item.FTags = [aitDecode] then
        Inc(FTotalSize, Item.UncompressedSize)
      else
        if Item.FTags <> [] then
          SetExitStatus(esCaseError);
  end;
end;

procedure TArchiver.ExtractTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if FIsNeededToRun then
  begin
    CheckSequences4Extract;
    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;

      Item := FCentralDirectory.Items[I];
      if Item.FTags = [aitUpdate] then
      begin
        DoMessage(Format(cmExtracting, [I + 1, Item.FExternalFileName]));
        DecodeToFile(Item);
      end else
        if Item.FTags = [aitDecode] then
        begin
          DoMessage(Format(cmDecoding, [I + 1, Item.FFileName]));
          DecodeToNul(Item);
        end;
    end;
  end;
end;

procedure TArchiver.TestTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if FIsNeededToRun then
  begin
    CheckSequences4Extract;
    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;

      Item := FCentralDirectory.Items[I];
      if Item.FTags = [aitUpdate] then
      begin
        DoMessage(Format(cmTesting, [I + 1, Item.FFileName]));
        DecodeToNul(Item);
      end else
        if Item.FTags = [aitDecode] then
        begin
          DoMessage(Format(cmDecoding, [I + 1, Item.FFileName]));
          DecodeToNul(Item);
        end;
    end;
  end;
end;

// TArchiver # RENAME #

procedure TArchiver.CheckSequences4Rename;
var
  I: longint;
  Item: TArchiveItem;
begin
  // STEP2: calculate bytes to process ...
  FTotalSize     := 0;
  FProcessedSize := 0;
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    Item := FCentralDirectory.Items[I];
    if Item.FTags = [aitUpdate] then
      Inc(FTotalSize, Item.CompressedSize)
    else
      if Item.FTags = [] then
        Inc(FTotalSize, Item.CompressedSize)
      else
        SetExitStatus(esCaseError);
  end;
end;

procedure TArchiver.RenameTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if FIsNeededToRun then
  begin
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankDisk, 0);
    FTempWriter.WriteDWord(ARCHIVE_MARKER);

    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;

      Item := FCentralDirectory.Items[I];
      if Item.FTags = [aitUpdate] then
      begin
        DoMessage(Format(cmRenaming, [I + 1, Item.FileName]));
        EncodeFromArchive(Item);
      end else
        if Item.FTags = [] then
        begin
          DoMessage(Format(cmCopying, [I + 1, Item.FileName]));
          EncodeFromArchive(Item);
        end;
    end;
    if ExitStatus = esNoError then
      FCentralDirectory.Write(FTempWriter);

    FIsNeededToSave := TRUE;
  end;
end;

// TArchiver # DELETE #

procedure TArchiver.CheckSequences4Delete;
var
  I, J, BackTear, NextTear: longint;
  Item: TArchiveItem;
begin
  // STEP1: find sequences and set tag ...
  I := GetBackTag(FCentralDirectory.Count - 1, [aitUpdate]);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FCentralDirectory.Count;
    // little optimization
    if (NextTear - I) <> 1 then
      // if is solid sequences
      if (NextTear - BackTear) > 1 then
      begin
        NextTear := GetBackTag(NextTear - 1, []);
        for J := BackTear to NextTear do
        begin
          Item := FCentralDirectory.Items[J];
          Include(Item.FTags, aitDecode);

          if Item.FTags = [aitDecode] then
            FIsNeededToSwap := TRUE;
        end;
        I := BackTear;
      end;
    I := GetBackTag(I - 1, [aitUpdate]);
  end;

  // STEP2: calculate bytes to process ...
  FTotalSize     := 0;
  FProcessedSize := 0;
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    Item := FCentralDirectory.Items[I];
    if Item.FTags = [] then
      Inc(FTotalSize, Item.CompressedSize)
    else
      if Item.FTags = [aitDecode] then
        Inc(FTotalSize, 2 * Item.UncompressedSize)
      else
        if Item.FTags = [aitDecode, aitUpdate] then
          Inc(FTotalSize, Item.UncompressedSize);
  end;
end;

procedure TArchiver.DeleteTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if FIsNeededToRun then
  begin
    CheckSequences4Delete;
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankDisk, 0);
    FTempWriter.WriteDWord(ARCHIVE_MARKER);

    if FIsNeededToSwap then Swapping;
    for I := FCentralDirectory.Count - 1 downto 0 do
    begin
      Item := FCentralDirectory.Items[I];
      if aitUpdate in Item.FTags then
      begin
        DoMessage(Format(cmDeleting, [I + 1, Item.FileName]));
        FCentralDirectory.Delete(I);
      end;
    end;

    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;

      Item := FCentralDirectory.Items[I];
      if Item.FTags = [] then
      begin
        DoMessage(Format(cmCopying, [I + 1, Item.FileName]));
        EncodeFromArchive(Item);
      end else
        if Item.FTags = [aitDecode] then
        begin
          DoMessage(Format(cmEncoding, [I + 1, Item.FileName]));
          EncodeFromSwap(Item);
        end;
    end;
    if ExitStatus = esNoError then
      FCentralDirectory.Write(FTempWriter);

    FIsNeededToSave := TRUE;
  end;
end;

// TArchiver # UPDATE #

procedure TArchiver.Configure;
var
  I: longint;
  CurrentBlock: int64;
  CurrentItem: TArchiveItem;
  CurrentFileExt: string;
  PreviousFileExt: string;
  Configuration: TConfiguration;
begin
  Configuration := TConfiguration.Create(ExtractCompressionConfiguration(FCompressionParams), FALSE);
  if FileExists(ExtractCompressionConfiguration(FCompressionParams)) = FALSE then
  begin
    SetExitStatus(esConfigError);
  end;
  Configuration.CaseSensitive := FALSE;

  CurrentFileExt := '.';
  CurrentBlock   := ExtractCompressionBlock(FCompressionParams);
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    CurrentItem := FCentralDirectory.Items[I];
    if CurrentItem.FVersionNeededToRead = 0 then
    begin

      CurrentItem.FVersionNeededToRead := GetVersionNeededToRead(CurrentItem);              // version needed to read
      if (CurrentItem.Attributes and (faDirectory or faSymLink or faVolumeID)) = 0 then
      begin
        CurrentItem.FCompressionMethod   := ExtractCompressionMethod  (FCompressionParams); // compression method
        CurrentItem.FCompressionLevel    := ExtractCompressionLevel   (FCompressionParams); // compression level
        CurrentItem.FCompressionLevelAux := ExtractCompressionLevelAux(FCompressionParams); // compression aux level
        CurrentItem.FCompressionBlock    := ExtractCompressionBlock   (FCompressionParams); // compression block

        // force file extension option
        PreviousFileExt := CurrentFileExt;
        if ExtractCompressionFilter(FCompressionParams) = '' then
          CurrentFileExt := ExtractFileExt(CurrentItem.FExternalFileName)
        else
          CurrentFileExt := ExtractCompressionFilter(FCompressionParams);

        // compression block option
        if AnsiCompareText(CurrentFileExt, PreviousFileExt) = 0 then
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
          if Configuration.ValueExists('level-' + IntToStr(CurrentItem.FCompressionLevel), CurrentFileExt) = TRUE then
            CurrentItem.FCompressionFilter :=
              Configuration.ReadString('level-' + IntToStr(CurrentItem.FCompressionLevel), CurrentFileExt,
                Hex(DefaultTableParameters, SizeOf(DefaultTableParameters)))
          else
            CurrentItem.FCompressionFilter :=
              Configuration.ReadString('level-' + IntToStr(CurrentItem.FCompressionLevel), '.def',
                Hex(DefaultTableParameters, SizeOf(DefaultTableParameters)));
        end;
        CurrentItem.FEncryptionMethod := ExtractEncryptionMethod(FEncryptionParams); // encryption method
        CurrentItem.FCheckMethod      := ExtractHashingMethod   (FHashingParams);    // check method
        CurrentItem.FCheckMethodAux   := ExtractHashingAuxMethod(FHashingParams);    // check method aux
      end;
      {$IFDEF DEBUG}
      {$ENDIF}
    end;
  end;
  FreeAndNil(Configuration);
end;

procedure TArchiver.CheckSequences4Update;
var
  Item: TArchiveItem;
  I, J, BackTear, NextTear: longint;
begin
  // STEP2: find sequences and tag ...
  I := GetBackTag(FCentralDirectory.Count - 1, [aitUpdate]);
  while I > -1 do
  begin
    BackTear := GetBackTear(I);
    NextTear := GetNextTear(I + 1);

    if NextTear = -1 then
      NextTear := FCentralDirectory.Count;
    // if is solid sequences
    if (NextTear - BackTear) > 1 then
    begin
      NextTear := GetBackTag(NextTear - 1, []);
      for J := BackTear to NextTear do
      begin
        Item := FCentralDirectory.Items[J];
        Include(Item.FTags, aitDecode);

        if Item.FTags = [aitDecode] then
          FIsNeededToSwap := TRUE;
      end;
      I := BackTear;
    end;
    I := GetBackTag(I - 1, [aitUpdate]);
  end;

  // STEP3: calculate bytes to process ...
  FTotalSize     := 0;
  FProcessedSize := 0;
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    Item := FCentralDirectory.Items[I];
    if Item.FTags = [] then
      Inc(FTotalSize, Item.CompressedSize)
    else
      if Item.FTags = [aitUpdate] then
        Inc(FTotalSize, Item.FExternalFileSize)
      else
        if Item.FTags = [aitDecode] then
          Inc(FTotalSize, Item.UncompressedSize + Item.UncompressedSize)
        else
          if Item.FTags = [aitDecode, aitUpdate] then
            Inc(FTotalSize, Item.UncompressedSize + Item.FExternalFileSize);
  end;
end;

procedure TArchiver.UpdateTagged;
var
  I: longint;
  Item: TArchiveItem;
begin
  CheckTags;
  if FIsNeededToRun then
  begin
    Configure;
    CheckSequences4Update;
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankDisk, 0);
    FTempWriter.WriteDWord(ARCHIVE_MARKER);

    if FIsNeededToSwap then Swapping;
    for I := 0 to FCentralDirectory.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;

      Item := FCentralDirectory.Items[I];
      if Item.FTags = [] then
      begin
        DoMessage(Format(cmCopying, [I + 1, Item.FileName]));
        EncodeFromArchive(Item);
      end else
        if Item.FTags = [aitUpdate] then
        begin
          DoMessage(Format(cmAdding, [I + 1, Item.FileName]));
          EncodeFromFile(Item);
        end else
          if Item.FTags = [aitDecode] then
          begin
            DoMessage(Format(cmEncoding, [I + 1, Item.FileName]));
            EncodeFromSwap(Item);
          end else
          begin
            DoMessage(Format(cmUpdating, [I + 1, Item.FileName]));
            EncodeFromFile(Item);
          end;
    end;
    if ExitStatus = esNoError then
      FCentralDirectory.Write(FTempWriter);

    FIsNeededToSave := TRUE;
  end;
end;

end.
