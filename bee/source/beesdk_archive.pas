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

    v0.8.0 build 1895 - 2013.03.14 by Melchiorre Caruso.
}

unit BeeSDK_Archive;

{$I bee_compiler.inc}

interface

uses
  Classes,
  SysUtils,

  Bee_Crc,
  Bee_BlowFish,
  Bee_MainPacker,

  Bee_Files,
  Bee_BufStream,
  Bee_Common,
  Bee_CommandLine,
  Bee_Configuration;

const
  /// archive markers
  ARCHIVE_DATA_MARKER                 = $78454542;
  ARCHIVE_CENTRALDIR_MARKER           = $78454542;
  ARCHIVE_CENTRALDIR_END_MARKER       = $78454542;
  ARCHIVE_CENTRALDIR_SEEK_MARKER      = $78454542;
  ARCHIVE_CENTRALDIR_MAGIKSEEK_MARKER = $78454542;

  /// archive central directory item type
  acditFILE         = $01;
  acditEND          = $7F;

type
  /// archive central directory end flag
  TArchiveCentralDirectoryEndFlag = (
    acdefVersionNeededToRead,
    acdefLastModifiedTime,
    acdefComment);

  TArchiveCentralDirectoryEndFlags = set of TArchiveCentralDirectoryEndFlag;

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
    aifAttributes,
    aifComment,
    aifLayer,
    aifLayerModifiedTime);

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
    FVersionNeededToRead: longword;
    FUncompressedSize: int64;
    FLastModifiedTime: longint;
    FAttributes: longword;
    FComment: string;
    FLayer: longword;
    FLayerModifiedTime: longint;
    // data descriptor property
    FDataDescriptorFlags: TArchiveDataDescriptorFlags;
    FCompressedSize: int64;
    FDiskNumber: longword;
    FDiskSeek: int64;
    FCheckMethod: THashAlgorithm;
    FCheckDigest: string;
    FCheckMethodAux: THashAlgorithm;
    FCheckDigestAux: string;
    // compression property
    FCompressionFlags: TArchiveCompressionFlags;
    FCompressionMethod: TCoderAlgorithm;
    FCompressionLevel: longword;
    FCompressionLevelAux: longword;
    FCompressionFilter: string;
    FCompressionFilterAux: string;
    FCompressionBlock: int64;
    // encryption property
    FEncryptionFlags: TArchiveEncryptionFlags;
    FEncryptionMethod: TCipherAlgorithm;
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
    property VersionNeededToRead: longword read FVersionNeededToRead;
    property UncompressedSize: int64 read FUncompressedSize;
    property LastModifiedTime: longint read FLastModifiedTime;
    property Attributes: longword read FAttributes;
    property Comment: string read FComment;
    property Layer: longword read FLayer;
    property LayerModifiedTime: longint read FLayerModifiedTime;
    // data descriptor property
    property DadaDescriptorFlags: TArchiveDataDescriptorFlags read FDataDescriptorFlags;
    property CompressedSize: int64 read FCompressedSize;
    property DiskNumber: longword read FDiskNumber;
    property DiskSeek: int64 read FDiskSeek;
    property CheckMethod: THashAlgorithm read FCheckMethod;
    property CheckDigest: string read FCheckDigest;
    property CheckMethodAux: THashAlgorithm read FCheckMethodAux;
    property CheckDigestAux: string read FCheckDigestAux;
    // compression property
    property CompressionFlags: TArchiveCompressionFlags read FCompressionFlags;
    property CompressionMethod: TCoderAlgorithm read FCompressionMethod;
    property CompressionLevel: longword read FCompressionLevel;
    property CompressionLevelAux: longword read FCompressionLevelAux;
    property CompressionFilter: string read FCompressionFilter;
    property CompressionFilterAux: string read FCompressionFilterAux;
    property CompressionBlock: int64 read FCompressionBlock;
    // encryption property
    property EncryptionFlags: TArchiveEncryptionFlags read FEncryptionFlags;
    property EncryptionMethod: TCipherAlgorithm read FEncryptionMethod;
    //
    property Index: longint read FIndex;
  end;

  /// archive central directory
  TArchiveCentralDirectory = class(TObject)
  private
    FItems: TList;
    FItemsAux: TList;
    // central directory end property
    FCDE_Flags: TArchiveCentralDirectoryEndFlags;
    FCDE_LastModifiedTime: longint;
    FCDE_Comment: string;
    // central directory seek property
    FCDS_Flags: TArchiveCentralDirectorySeekFlags;
    FCDS_DisksNumber: longword;
    FCDS_DiskNumber: longword;
    FCDS_DiskSeek: int64;
    // central directory magik seek property
    FCDMS_DiskSeek: longint;
  private
    function GetCount: longint;
    function GetItem(Index: longint): TArchiveItem;
    function GetIndexAuxOf(const FileName: string; Layer: longword): longint;
    function GetIndexOf(const FileName: string; Layer: longword): longint;
  private
    procedure Pack;
    procedure UnPack;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    function Add(Item : TArchiveItem): longint;
    procedure Delete(Index: longint);
    procedure Clear;
    function IndexOf(const FileName: string; Layer: longword): longint;
  public
    property Items[Index: longint]: TArchiveItem read GetItem;
    property Count: longint read GetCount;
    property Comment: string read FCDE_Comment write FCDE_Comment;
    property LastModifiedTime: longint read FCDE_LastModifiedTime;
  end;

  /// ...
  TArchiveConfirm = (arcOk, arcCancel, arcQuit);

  /// archive events

  TArchiveProgressEvent = procedure(Percentage: longint) of object;

  TArchiveMessageEvent = procedure(const aMessage: string) of object;

  TArchiveRenameEvent = procedure(Item: TArchiveItem;
    var RenameAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveExtractEvent = procedure(Item: TArchiveItem;
    var ExtractAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveDeleteEvent = procedure(Item: TArchiveItem;
    var Confirm: TArchiveConfirm) of object;

  TArchiveCommentEvent = procedure(Item: TArchiveItem;
    var CommentAs: string; var Confirm: TArchiveConfirm) of object;

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
    FCheckParams: string;
    FTestTempArchive: boolean;
    FVerboseMode: boolean;
    FVolumeSize: int64;
    FLayer: longword;
    // items
    FCentralDirectory: TArchiveCentralDirectory;
    FSearchRecs: TList;
  private
    procedure SetComment(const Value: string);
    procedure SetArchiveName(const Value: string);
    procedure SetWorkDirectory(const Value: string);
    procedure SetSfxName(const Value: string);

    function GetBackTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetNextTag(Index: longint; aTag: TArchiveItemTag): longint;
    function GetBackTear(Index: longint): longint;
    function GetNextTear(Index: longint): longint;
    function GetItem(Index: longint): TArchiveItem;
    function GetComment: string;
    function GetCount: longint;
    function GetLastModifiedTime: longint;
  private
    procedure Encode           (Reader: TBufStream; Writer: TBufStream; const Size: int64);
    procedure EncodeFromArchive(Item: TArchiveItem);
    procedure EncodeFromSwap   (Item: TArchiveItem);
    procedure EncodeFromFile   (Item: TArchiveItem);
    procedure DecodeToNul      (Item: TArchiveItem);
    procedure DecodeToSwap     (Item: TArchiveItem);
    procedure DecodeToFile     (Item: TArchiveItem);
  private
    FOnRequestBlankImage: TFileWriterRequestBlankImageEvent;
    procedure DoRequestBlankImage(ImageNumber: longint; var Abort : Boolean);
  private
    FOnRequestImage: TFileReaderRequestImageEvent;
    procedure DoRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
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
  private
    FOnRename: TArchiveRenameEvent;
    function  DoRename(Item: TArchiveItem): TArchiveConfirm;
    procedure CheckTags4Rename;
    procedure CheckSequences4Rename;
  private
    FOnDelete: TArchiveDeleteEvent;
    function  DoDelete(Item: TArchiveItem): TArchiveConfirm;
    procedure CheckTags4Delete;
    procedure CheckSequences4Delete;
  private
    FOnUpdate: TArchiveUpdateEvent;
    function  DoUpdate(Item: TCustomSearchRec): TArchiveConfirm;
    procedure CheckTags4Update;
    procedure CheckSequences4Update;
    procedure Configure;
  private
    FOnComment: TArchiveCommentEvent;
    function DoComment(Item: TArchiveItem): TArchiveConfirm;
  private
    procedure Swapping;
    procedure TestTemporaryArchive;
    procedure SaveTemporaryArchive;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseArchive;
    procedure OpenArchive(const aArchiveName: string);
    procedure Suspend(Value: boolean);
    procedure Terminate;

    procedure UnTagAll;
    procedure UnTag(Index: longint);
    procedure TagAll;
    procedure Tag(Index: longint); overload;
    procedure Tag(SearchRec: TCustomSearchRec); overload;
    function IsTagged(Index: longint): boolean;
    function IndexOf(const aFileName: string): longint;

    procedure TestTagged;
    procedure ExtractTagged;
    procedure RenameTagged;
    procedure DeleteTagged;
    procedure UpdateTagged;
  public
    property OnRequestBlankImage: TFileWriterRequestBlankImageEvent read FOnRequestBlankImage write FOnRequestBlankImage;
    property OnRequestImage: TFileReaderRequestImageEvent read FOnRequestImage write FOnRequestImage;
    property OnMessage: TArchiveMessageEvent read FOnMessage write FOnMessage;
    property OnProgress: TArchiveProgressEvent read FOnProgress write FOnProgress;
    property OnExtract: TArchiveExtractEvent read FOnExtract write FOnExtract;
    property OnRename: TArchiveRenameEvent read FOnRename write FOnRename;
    property OnDelete: TArchiveDeleteEvent read FOnDelete write FOnDelete;
    property OnUpdate: TArchiveUpdateEvent read FOnUpdate write FOnUpdate;
    property OnComment: TArchiveCommentEvent read FOnComment write FOnComment;
  public
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property Comment: string read GetComment write SetComment;
    property SelfExtractor: string read FSfxName write FSfxName;
    property WorkDirectory: string read FWorkDirectory write SetWorkDirectory;
    property CompressionParams: string read FCompressionParams write FCompressionParams;
    property EncryptionParams: string read FEncryptionParams write FEncryptionParams;
    property CheckParams: string read FCheckParams write FCheckParams;
    property TestTempArchive: boolean read FTestTempArchive write FTestTempArchive;
    property VerboseMode: boolean read FVerboseMode write FVerboseMode;
    property VolumeSize: int64 read FVolumeSize write FVolumeSize;
    property Items[Index: longint]: TArchiveItem read GetItem;
    property Count: longint read GetCount;

    property LastModifiedTime: longint read GetLastModifiedTime;
  end;

function CoderMethodToStr(Method: TCoderAlgorithm ): string;
function HashMethodToStr(Method: THashAlgorithm): string;
function CipherMethodToStr(Method: TCipherAlgorithm): string;

function VersionToStr(Version: longword): string;
function RatioToStr(const PackedSize, Size: int64): string;
function SizeToStr(const Size: int64): string;
function AttrToStr(Attr: longint): string;

implementation

uses
  Bee_Interface;

// ---

function ExtractQWord(const Params: string; const K: string): qword;
var
  I: longint;
  S: string;
begin
  S := '';
  if Pos(K, UpCase(Params)) > 0 then
  begin
    for I := Pos(K, UpCase(Params)) + Length(K) to Length(Params) do
      if Params[I] <> ':' then
        S := S + Params[I]
      else
        Break;
  end;
  if TryStrWithMultToQWord(S, Result) = FALSE then Result := 0;
end;

function ExtractStr(const Params: string; const K: string): string;
var
  I: longint;
begin
  Result := '';
  if Pos(K, UpCase(Params)) > 0 then
  begin
    for I := Pos(K, UpCase(Params)) + Length(K) to Length(Params) do
      if Params[I] <> ':' then
        Result := Result + Params[I]
      else
        Break;
  end;
end;

function GetCoderAlgorithm(const Params: string): TCoderAlgorithm;
var
  S: string;
begin
  if Pos(':M=', UpCase(Params)) > 0 then
  begin
    S := Upcase(ExtractStr(Params, ':M='));
    if S = 'NONE' then Result := caStore else
    if S = 'BEE'  then Result := caBee   else
    if S = 'PPMD' then Result := caPpmd  else SetExitStatus(esCmdLineError);
  end else
    Result := caPpmd;
end;

function GetCoderLevel(const Params: string): longword;
begin
  if Pos(':L=', UpCase(Params)) > 0 then
  begin
    Result := ExtractQWord(Params, ':L=');
    case GetCoderAlgorithm(Params) of
      caStore: if ( 0 < Result) or (Result < 0) then SetExitStatus(esCmdLineError);
      caBee:   if ( 3 < Result) or (Result < 1) then SetExitStatus(esCmdLineError);
      caPpmd:  if (64 < Result) or (Result < 2) then SetExitStatus(esCmdLineError);
    end;
  end else
    // default level
    case GetCoderAlgorithm(Params) of
      caStore: Result := 0;
      caBee:   Result := 1;
      caPpmd:  Result := 3;
    end;
end;

function GetCoderLevelAux(Params: string): longword;
begin
  if Pos(':LA=', UpCase(Params)) > 0 then
  begin
    Result := ExtractQWord(Params, ':LA=');
    case GetCoderAlgorithm(Params) of
      caStore: if (        0 < Result) or (Result <    0) then SetExitStatus(esCmdLineError);
      caBee:   if (        9 < Result) or (Result <    0) then SetExitStatus(esCmdLineError);
      caPpmd:  if ($FFFFFFDB < Result) or (Result < $800) then SetExitStatus(esCmdLineError);
    end;
  end else
    // default level.aux
    case GetCoderAlgorithm(Params) of
      caStore: Result := 0;
      caBee:   Result := 2;
      caPpmd:  Result := $100000;
    end;
end;

function GetCoderFilter(const Params: string): string;
begin
  Result := ExtractStr(Params, ':F=');
end;

function GetCoderFilterAux(const Params: string): string;
begin
  Result := ExtractStr(Params, ':FA');
end;

function GetCoderBlock(Params: string): qword;
begin
  Result := ExtractQWord(Params, ':B=');
end;

function GetCoderConfiguration(const Params: string): string;
begin
  Result := ExtractStr(Params, ':C=');
  if Result = '' then
    Result := DefaultCfgName;
end;

function GetCipherKey(const Params: string): string;
begin
  Result := ExtractStr(Params, ':K=');
end;

function GetCipherAlgorithm(const Params: string): TCipherAlgorithm;
var
  S: string;
begin
  Result := caNul;
  if GetCipherKey(Params) <> '' then
    Result := caBlowFish;

  if Pos(':M=', UpCase(Params)) > 0 then
  begin
    S := UpCase(ExtractStr(Params, ':M='));
    if S = 'NONE'     then Result := caNul      else
    if S = 'BLOWFISH' then Result := caBlowFish else
    if S = 'IDEA'     then Result := caIdea     else SetExitStatus(esCmdLineError);
  end;

  if Result <> caNul then
    if Length(GetCipherKey(Params)) < 4 then
      SetExitStatus(esCmdLineError);
end;

function GetHashAlgorithm(Params: string): THashAlgorithm;
var
  S: string;
begin
  Result := haCRC32;
  if Pos(':M=', UpCase(Params)) > 0 then
  begin
    S := UpCase(ExtractStr(Params, ':M='));
    if S = 'NONE'  then Result := haNul   else
    if S = 'CRC32' then Result := haCRC32 else
    if S = 'CRC64' then Result := haCRC64 else
    if S = 'SHA1'  then Result := haSHA1  else
    if S = 'MD5'   then Result := haMD5   else SetExitStatus(esCmdLineError);
  end;
end;

function GetHashAlgorithmAux(Params: string): THashAlgorithm;
var
  S: string;
begin
  Result := haCRC32;
  if Pos(':MA=', UpCase(Params)) > 0 then
  begin
    S := UpCase(ExtractStr(Params, ':MA='));
    if S = 'NONE'  then Result := haNul   else
    if S = 'CRC32' then Result := haCRC32 else
    if S = 'CRC64' then Result := haCRC64 else
    if S = 'SHA1'  then Result := haSHA1  else
    if S = 'MD5'   then Result := haMD5   else SetExitStatus(esCmdLineError);
  end;
end;

// ---

function GetVersionNeededToRead(Item: TArchiveItem): longword; overload;
begin
  Result := 80;
end;

function GetVersionNeededToRead(Flags: TArchiveCentralDirectoryEndFlags): longword; overload;
begin
  Result := 80;
end;

function GetVersionNeededToRead(Flags: TArchiveCentralDirectorySeekFlags): longword; overload;
begin
  Result := 80;
end;

// ---

function CoderMethodToStr(Method: TCoderAlgorithm): string;
begin
  case Method of
    caStore: Result := 'NONE';
    caBee:   Result := 'BEE';
    caPpmd:  Result := 'PPMD';
    else     Result := '???';
  end;
end;

function HashMethodToStr(Method: THashAlgorithm): string;
begin
  case Method of
    haNul:   Result := 'NONE';
    haCRC32: Result := 'CRC32';
    haCRC64: Result := 'CRC64';
    haSHA1:  Result := 'SHA1';
    haMD5:   Result := 'MD5';
    else     Result := '???';
  end;
end;

function CipherMethodToStr(Method: TCipherAlgorithm): string;
begin
  case Method of
    caNul:      Result := 'NONE';
    caBlowFish: Result := 'BLOWFISH';
    caIdea:     Result := 'IDEA';
    else        Result := '???';
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
  /// item property ///
  FFlags := [
    aifVersionNeededToRead,
    aifUncompressedSize,
    aifLastModifiedTime,
    aifAttributes,
    aifComment,
    aifLayer,
    aifLayerModifiedTime];
  FVersionNeededToRead :=  0;
  FUncompressedSize    :=  0;
  FLastModifiedTime    :=  0;
  FAttributes          :=  0;
  FComment             := '';
  FLayer               :=  0;
  FLayerModifiedTime   :=  0;
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
  FCheckMethod          := haNul;
  FCheckDigest          := '';
  FCheckMethodAux       := haNul;
  FCheckDigestAux       := '';
  /// compression property ///
  FCompressionFlags     := [
    acfCompressionMethod,
    acfCompressionLevel,
    acfCompressionLevelAux,
    acfCompressionFilter,
    acfCompressionFilterAux,
    acfCompressionBlock];
  FCompressionMethod    := caStore;
  FCompressionBlock     := 0;
  FCompressionLevel     := 0;
  FCompressionLevelAux  := 0;
  FCompressionFilter    := '';
  FCompressionFilterAux := '';
  /// encryption property ///
  FEncryptionFlags      := [
    aefEncryptionMethod];
  FEncryptionMethod     := caNul;
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
  if (aifAttributes          in FFlags) then FAttributes          := Stream.ReadInfWord;
  if (aifComment             in FFlags) then FComment             := Stream.ReadInfString;
  if (aifLayer               in FFlags) then FLayer               := Stream.ReadInfWord;
  if (aifLayerModifiedTime   in FFlags) then FLayerModifiedTime   := Stream.ReadInfWord;
  /// data descryptor property ///
  FDataDescriptorFlags := TArchiveDataDescriptorFlags(longword(Stream.ReadInfWord));
  if (addfCompressedSize in FDataDescriptorFlags) then FCompressedSize := Stream.ReadInfWord;
  if (addfDiskNumber     in FDataDescriptorFlags) then FDiskNumber     := Stream.ReadInfWord;
  if (addfDiskSeek       in FDataDescriptorFlags) then FDiskSeek       := Stream.ReadInfWord;
  if (addfCheckMethod    in FDataDescriptorFlags) then FCheckMethod    := THashAlgorithm(Stream.ReadInfWord);
  if (addfCheckDigest    in FDataDescriptorFlags) then FCheckDigest    := Stream.ReadInfArray;
  if (addfCheckMethodAux in FDataDescriptorFlags) then FCheckMethodAux := THashAlgorithm(Stream.ReadInfWord);
  if (addfCheckDigestAux in FDataDescriptorFlags) then FCheckDigestAux := Stream.ReadInfArray;
  /// compression property ///
  FCompressionFlags := TArchiveCompressionFlags(longword(Stream.ReadInfWord));
  if (acfCompressionMethod    in FCompressionFlags) then FCompressionMethod    := TCoderAlgorithm(Stream.ReadInfWord);
  if (acfCompressionLevel     in FCompressionFlags) then FCompressionLevel     := Stream.ReadInfWord;
  if (acfCompressionLevelAux  in FCompressionFlags) then FCompressionLevelAux  := Stream.ReadInfWord;
  if (acfCompressionFilter    in FCompressionFlags) then FCompressionFilter    := Stream.ReadInfArray;
  if (acfCompressionFilterAux in FCompressionFlags) then FCompressionFilterAux := Stream.ReadInfArray;
  if (acfCompressionBlock     in FCompressionFlags) then FCompressionBlock     := Stream.ReadInfWord;
  /// encryption property ///
  FEncryptionFlags := TArchiveEncryptionFlags(longword(Stream.ReadInfWord));
  if (aefEncryptionMethod  in FEncryptionFlags) then FEncryptionMethod := TCipherAlgorithm(Stream.ReadInfWord);
end;

procedure TArchiveItem.Write(Stream: TFileWriter);
begin
  Stream.WriteInfString(FFileName);
  /// item property ///
  Stream.WriteInfWord(longword(FFlags));
  if (aifVersionNeededToRead in FFlags) then Stream.WriteInfWord(FVersionNeededToRead);
  if (aifUncompressedSize    in FFlags) then Stream.WriteInfWord(FUncompressedSize);
  if (aifLastModifiedTime    in FFlags) then Stream.WriteInfWord(FLastModifiedTime);
  if (aifAttributes          in FFlags) then Stream.WriteInfWord(FAttributes);
  if (aifComment             in FFlags) then Stream.WriteInfString(FComment);
  if (aifLayer               in FFlags) then Stream.WriteInfWord(FLayer);
  if (aifLayerModifiedTime   in FFlags) then Stream.WriteInfWord(FLayerModifiedTime);
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
  FItems := TList.Create;
  FItemsAux := TList.Create;
  // central direcotry end
  FCDE_LastModifiedTime := DateTimeToFileDate(Now);
  FCDE_Comment := '';
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
end;

function TArchiveCentralDirectory.Add(Item: TArchiveItem): longint;
var
  Lo, Med, Hi, I: longint;
begin
  Item.FIndex := FItems.Add(Item);
  if FItemsAux.Count <> 0 then
  begin
    Lo := 0;
    Hi := FItemsAux.Count - 1;
    while Hi >= Lo do
    begin
      Med := (Lo + Hi) div 2;
      I := AnsiCompareFileName(Item.FileName, TArchiveItem(FItemsAux[Med]).FileName);
      if I = 0 then
      begin
        I := Item.FLayer - TArchiveItem(FItemsAux[Med]).FLayer;
      end;

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
    end else
    begin
      if I > 0 then
        FItemsAux.Insert(Med + 1, Item)
      else
        FItemsAux.Insert(Med, Item);
    end;
  end else
    FItemsAux.Add(Item);
  Result := Item.FIndex;
end;

function TArchiveCentralDirectory.GetIndexAuxOf(const FileName: string; Layer: longword): longint;
var
  Lo, Med, Hi, I: longint;
begin
  Lo := 0;
  Hi := FItemsAux.Count - 1;
  while Hi >= Lo do
  begin
    Med := (Lo + Hi) div 2;
    I := AnsiCompareFileName(FileName, TArchiveItem(FItemsAux[Med]).FileName);
    if I = 0 then
    begin
      I := Layer - TArchiveItem(FItemsAux[Med]).FLayer;
    end;

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
  begin
    Result := Med;
  end;
end;

function TArchiveCentralDirectory.GetIndexOf(const FileName: string; Layer: longword): longint;
begin
  Result := GetIndexAuxOf(FileName, Layer);
  if Result <> -1 then
  begin
    Result := TArchiveItem(FItemsAux[Result]).FIndex;
  end;
end;

function TArchiveCentralDirectory.IndexOf(const FileName: string; Layer: longword): longint;
begin
  Result := GetIndexOf(FileName, Layer);
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

  FItemsAux.Delete(GetIndexAuxOf(Item.FileName, Item.FLayer));
  FItems.Delete(Item.FIndex);
  Item.Destroy;
end;

function TArchiveCentralDirectory.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TArchiveCentralDirectory.GetItem(Index: longint): TArchiveItem;
begin
  Result := TArchiveItem(FItems[Index]);
end;

procedure TArchiveCentralDirectory.Pack;
var
  I: longint;
  CurrentItem: TArchiveItem;
  PreviusItem: TArchiveItem;
begin
  if FItems.Count > 1 then
  begin
    PreviusItem := FItems.Items[0];
    for I := 1 to FItems.Count - 1 do
    begin
      CurrentItem := FItems.Items[I];
      /// item property ///
      if CurrentItem.FVersionNeededToRead  = PreviusItem.FVersionNeededToRead  then Exclude(CurrentItem.FFlags, aifVersionNeededToRead) else Include(CurrentItem.FFlags, aifVersionNeededToRead);
      if CurrentItem.FUncompressedSize     = PreviusItem.FUncompressedSize     then Exclude(CurrentItem.FFlags, aifUncompressedSize)    else Include(CurrentItem.FFlags, aifUncompressedSize);
      if CurrentItem.FLastModifiedTime     = PreviusItem.FLastModifiedTime     then Exclude(CurrentItem.FFlags, aifLastModifiedTime)    else Include(CurrentItem.FFlags, aifLastModifiedTime);
      if CurrentItem.FAttributes           = PreviusItem.FAttributes           then Exclude(CurrentItem.FFlags, aifAttributes)          else Include(CurrentItem.FFlags, aifAttributes);
      if CurrentItem.FComment              = PreviusItem.FComment              then Exclude(CurrentItem.FFlags, aifComment)             else Include(CurrentItem.FFlags, aifComment);
      if CurrentItem.FLayer                = PreviusItem.FLayer                then Exclude(CurrentItem.FFlags, aifLayer)               else Include(CurrentItem.FFlags, aifLayer);
      if CurrentItem.FLayerModifiedTime    = PreviusItem.FLayerModifiedTime    then Exclude(CurrentItem.FFlags, aifLayerModifiedTime)   else Include(CurrentItem.FFlags, aifLayerModifiedTime);
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

      PreviusItem := CurrentItem;
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
  begin
    PreviusItem := FItems.Items[0];
    for I := 1 to FItems.Count - 1 do
    begin
      CurrentItem := FItems.Items[I];
      /// item property ///
      if not(aifVersionNeededToRead in CurrentItem.FFlags) then CurrentItem.FVersionNeededToRead := PreviusItem.FVersionNeededToRead;
      if not(aifUncompressedSize    in CurrentItem.FFlags) then CurrentItem.FUncompressedSize    := PreviusItem.FUncompressedSize;
      if not(aifLastModifiedTime    in CurrentItem.FFlags) then CurrentItem.FLastModifiedTime    := PreviusItem.FLastModifiedTime;
      if not(aifAttributes          in CurrentItem.FFlags) then CurrentItem.FAttributes          := PreviusItem.FAttributes;
      if not(aifComment             in CurrentItem.FFlags) then CurrentItem.FComment             := PreviusItem.FComment;
      if not(aifLayer               in CurrentItem.FFlags) then CurrentItem.FLayer               := PreviusItem.FLayer;
      if not(aifLayerModifiedTime   in CurrentItem.FFlags) then CurrentItem.FLayerModifiedTime   := PreviusItem.FLayerModifiedTime;
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

      PreviusItem := CurrentItem;
    end;
  end;
end;

procedure TArchiveCentralDirectory.Read(Stream: TFileReader);
const
  CDSFULL = [acdsfVersionNeededToRead, acdsfDisksNumber,      acdsfDiskNumber];
  CDEFULL = [acdefVersionNeededToRead, acdefLastModifiedTime, acdefComment];
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
      FCDS_Flags := TArchiveCentralDirectorySeekFlags(longword(Stream.ReadInfWord));
      if (acdsfVersionNeededToRead in FCDS_Flags) then
        if Stream.ReadInfWord > GetVersionNeededToRead(CDSFULL) then
          SetExitStatus(esArchiveVerError);

      if ExitStatus = esNoError then
      begin
        FCDS_DisksNumber := 1;
        if (acdsfDisksNumber in FCDS_Flags) then
          FCDS_DisksNumber := Stream.ReadInfWord;

        FCDS_DiskNumber  := 1;
        if (acdsfDiskNumber  in FCDS_Flags) then
          FCDS_DiskNumber := Stream.ReadInfWord;

        FCDS_DiskSeek := Stream.ReadInfWord;
        // [4.1] seek on central directory marker
        Stream.ImagesNumber := FCDS_DisksNumber;
        Stream.ImageNumber  := FCDS_DiskNumber;
        Stream.Seek           (FCDS_DiskSeek, fsFromBeginning);
        // [4.2] read central directory marker
        MARKER := Stream.ReadDWord;
        if MARKER <> ARCHIVE_CENTRALDIR_MARKER then
          SetExitStatus(esArchiveTypeError);
      end;
    end;

  // [5] read central directory items
  if MARKER = ARCHIVE_CENTRALDIR_MARKER then
    while ExitStatus = esNoError do
    begin
      MARKER := Stream.ReadInfWord;
      case MARKER of
        acditFILE: Add(TArchiveItem.Read(Stream));
        acditEND:  Break;
        else       SetExitStatus(esArchiveTypeError);
      end;
    end;

  // [6] read central directory end marker
  if ExitStatus = esNoError then
    if Stream.ReadDWord <> ARCHIVE_CENTRALDIR_END_MARKER then
      SetExitStatus(esArchiveTypeError);

  // [7] read central directory end
  if ExitStatus = esNoError then
  begin
    FCDE_Flags := TArchiveCentralDirectoryEndFlags(longword(Stream.ReadInfWord));
    if (acdefVersionNeededToRead in FCDE_Flags) then
      if Stream.ReadInfWord > GetVersionNeededToRead(CDEFULL) then
        SetExitStatus(esArchiveVerError);

    if ExitStatus = esNoError then
    begin
      FCDE_LastModifiedTime := 0;
      if (acdefLastModifiedTime in FCDE_Flags) then
        FCDE_LastModifiedTime := Stream.ReadInfWord;

      FCDE_Comment := '';
      if (acdefComment  in FCDE_Flags) then
        FCDE_Comment := Stream.ReadInfString;
    end;
  end;

  // [8] unpack central directory
  if ExitStatus = esNoError then UnPack;
end;

procedure TArchiveCentralDirectory.Write(Stream: TFileWriter);
var
  I: longword;
begin
  // [0] store central directory seek
  FCDS_Flags      := [acdsfVersionNeededToRead];
  FCDS_DiskSeek   := Stream.Seek(0, fsFromCurrent);
  FCDS_DiskNumber := Stream.CurrentImage;
  if FCDS_DiskNumber <> 1 then
    Include(FCDS_Flags,  acdsfDiskNumber);

  // [1] write central directory items
  Stream.WriteDWord(ARCHIVE_CENTRALDIR_MARKER);
  Pack;

  if FItems.Count > 0 then
    for I := 0 to FItems.Count - 1 do
    begin
      Stream.WriteInfWord(acditFILE);
      TArchiveItem(FItems[I]).Write(Stream);
    end;
  Stream.WriteInfWord(acditEND);

  // [2] write central directory end
  FCDE_Flags := [acdefVersionNeededToRead, acdefLastModifiedTime];
  FCDE_LastModifiedTime := DateTimeToFileDate(Now);
  if Length(FCDE_Comment) > 0 then
    Include(FCDE_Flags, acdefComment);

  Stream.WriteDWord(ARCHIVE_CENTRALDIR_END_MARKER);
  Stream.WriteInfWord(longword(FCDE_Flags));
  if (acdefVersionNeededToRead in FCDE_Flags) then
    Stream.WriteInfWord(GetVersionNeededToRead(FCDE_Flags));
  if (acdefLastModifiedTime in FCDE_Flags) then
    Stream.WriteInfWord(FCDE_LastModifiedTime);
  if (acdefComment in FCDE_Flags) then
    Stream.WriteInfString(FCDE_Comment);

  // [3] multi-spanning support
  if Stream.Threshold > 0 then
    if (Stream.Threshold - Stream.Seek(0, fsFromCurrent)) < 512 then
      Stream.CreateNewImage;

  // [0.1] write central directory seek
  FCDMS_DiskSeek   := Stream.Seek(0, fsFromCurrent);
  FCDS_DisksNumber := Stream.CurrentImage;
  if FCDS_DisksNumber <> 1 then
    Include(FCDS_Flags, acdsfDisksNumber);

  Stream.WriteDWord(ARCHIVE_CENTRALDIR_SEEK_MARKER);
  Stream.WriteInfWord(longword(FCDS_Flags));
  if (acdsfVersionNeededToRead in FCDS_Flags) then
    Stream.WriteInfWord(GetVersionNeededToRead(FCDS_Flags));
  if (acdsfDisksNumber in FCDS_Flags) then
    Stream.WriteInfWord(FCDS_DisksNumber);
  if (acdsfDiskNumber in FCDS_Flags) then
    Stream.WriteInfWord(FCDS_DiskNumber);
  Stream.WriteInfWord(FCDS_DiskSeek);

  // [4] write magikseek
  Stream.WriteDWord(ARCHIVE_CENTRALDIR_MAGIKSEEK_MARKER);
  Stream.WriteDWord(longword(Stream.Seek(0, fsFromCurrent)
    - FCDMS_DiskSeek + SizeOf(DWord)));
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
  // items list
  FCentralDirectory  := TArchiveCentralDirectory.Create;
  FSearchRecs        := TList.Create;
end;

destructor TArchiver.Destroy;
var
  I: longint;
begin
  FCentralDirectory.Destroy;
  for I := 0 to FSearchRecs.Count - 1 do
  begin
    TCustomSearchRec(FSearchRecs[I]).Destroy;
  end;
  FSearchRecs.Destroy;
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

  FArchiveReader.HashMethod   := haNul;
  FArchiveReader.CipherMethod := caNul;
  FArchiveReader.CoderMethod  := caStore;

  FTempWriter.HashMethod      := haNul;
  FTempWriter.CipherMethod    := caNul;
  FTempWriter.CoderMethod     := caStore;
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
  FSwapReader.CipherKey       := GetCipherKey(EncryptionParams);
  FSwapReader.CoderMethod     := caStore;

  FTempWriter.HashMethod      := Item.CheckMethod;
  FTempWriter.CipherMethod    := Item.EncryptionMethod;
  FTempWriter.CipherKey       := GetCipherKey(EncryptionParams);
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
  Source.CipherMethod        := caNul;
  Source.CoderMethod         := caStore;

  FTempWriter.HashMethod     := Item.CheckMethodAux;
  FTempWriter.CipherMethod   := Item.EncryptionMethod;
  FTempWriter.CipherKey      := GetCipherKey(EncryptionParams);
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
  FArchiveReader.CipherKey      := GetCipherKey(EncryptionParams);
  FArchiveReader.CoderMethod    := Item.CompressionMethod;
  FArchiveReader.CoderLevel     := Item.CompressionLevel;
  FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
  FArchiveReader.CoderFilter    := Item.CompressionFilter;
  FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
  FArchiveReader.CoderBlock     := Item.CompressionBlock;

  FSwapWriter.HashMethod        := Item.CheckMethod;
  FSwapWriter.CipherMethod      := Item.EncryptionMethod;
  FSwapWriter.CipherKey         := GetCipherKey(EncryptionParams);
  FSwapWriter.CoderMethod       := caStore;
  begin
    Encode(FArchiveReader, FSwapWriter, Item.UncompressedSize);
  end;

  if Item.CheckMethod <> haNul then
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
  Destination.CipherMethod      := caNul;
  Destination.CoderMethod       := caStore;

  FArchiveReader.HashMethod     := haNul;
  FArchiveReader.CipherMethod   := Item.EncryptionMethod;
  FArchiveReader.CipherKey      := GetCipherKey(EncryptionParams);
  FArchiveReader.CoderMethod    := Item.CompressionMethod;
  FArchiveReader.CoderLevel     := Item.CompressionLevel;
  FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
  FArchiveReader.CoderFilter    := Item.CompressionFilter;
  FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
  FArchiveReader.CoderBlock     := Item.CompressionBlock;
  begin
    Encode(FArchiveReader, Destination, Item.FUncompressedSize);
  end;

  if Item.CheckMethod <> haNul then
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
  Destination.CipherMethod      := caNul;
  Destination.CoderMethod       := caStore;

  FArchiveReader.HashMethod     := haNul;
  FArchiveReader.CipherMethod   := Item.EncryptionMethod;
  FArchiveReader.CipherKey      := GetCipherKey(EncryptionParams);
  FArchiveReader.CoderMethod    := Item.CompressionMethod;
  FArchiveReader.CoderLevel     := Item.CompressionLevel;
  FArchiveReader.CoderLevelAux  := Item.CompressionLevelAux;
  FArchiveReader.CoderFilter    := Item.CompressionFilter;
  FArchiveReader.CoderFilterAux := Item.CompressionFilterAux;
  FArchiveReader.CoderBlock     := Item.CompressionBlock;
  begin
    Encode(FArchiveReader, Destination, Item.UncompressedSize);
  end;

  if Item.CheckMethod <> haNul then
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
  FSwapWriter.WriteDWord(ARCHIVE_DATA_MARKER);
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
      FTempWriter.WriteDWord(ARCHIVE_DATA_MARKER);

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
  FSearchRecs.Clear;

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
  Result := FCentralDirectory.IndexOf(aFileName, 0);
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

function TArchiver.GetLastModifiedTime: longint;
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

function TArchiver.DoComment(Item: TArchiveItem): TArchiveConfirm;
var
  CommentAs: string;
begin
  Result := arcCancel;
  if Assigned(FOnComment) then
  begin
    CommentAs := Item.Comment;
    FOnComment(Item, CommentAs, Result);
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
begin
  FSearchRecs.Add(TCustomSearchRec.CreateFrom(SearchRec));
end;

function TArchiver.IsTagged(Index: longint): boolean;
begin
  Result := FCentralDirectory.Items[Index].FTag = aitUpdate;
end;

// TArchiver # EXTRACT #

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
      if Assigned(FOnExtract) then
      begin
        ExtractAs := Item.FFileName;
        FOnExtract(Item, ExtractAs, Confirm);
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

function TArchiver.DoRename(Item: TArchiveItem): TArchiveConfirm;
var
  RenameAs: string;
begin
  Result := arcCancel;
  if Assigned(FOnRename) then
  begin
    RenameAs := Item.FFileName;
    FOnRename(Item, RenameAs, Result);
    if Result = arcOk then
    begin
      Result := DoComment(Item);
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
      case DoRename(Item) of
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
    FTempWriter.WriteDWord(ARCHIVE_DATA_MARKER);

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

function TArchiver.DoDelete(Item: TArchiveItem): TArchiveConfirm;
begin
  Result := arcCancel;
  if Assigned(FOnDelete) then
  begin
    FOnDelete(Item, Result);
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
      case DoDelete(Item) of
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
    FTempWriter.WriteDWord(ARCHIVE_DATA_MARKER);

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
  Block: int64;
  CurrentItem: TArchiveItem;
  CurrentTable: TTableParameters;
  CurrentFileExt: string;
  PreviousFileExt: string;
  Configuration: TConfiguration;
begin
  Configuration := TConfiguration.Create;
  if FileExists(GetCoderConfiguration(FCompressionParams)) then
    Configuration.LoadFromFile(GetCoderConfiguration(FCompressionParams))
  else
    SetExitStatus(esConfigError);

  CurrentFileExt := '.';
  Configuration.Selector('\main');
  Configuration.CurrentSection.Values['Method']     := IntToStr(Ord(GetCoderLevel   (FCompressionParams)));
  Configuration.CurrentSection.Values['Dictionary'] := IntToStr(Ord(GetCoderLevelAux(FCompressionParams)));
  Configuration.Selector('\m' + Configuration.CurrentSection.Values['Method']);

  Block := GetCoderBlock(FCompressionParams);
  for I := 0 to FCentralDirectory.Count - 1 do
  begin
    CurrentItem := FCentralDirectory.Items[I];
    if CurrentItem.FTag = aitAdd then
    begin
      // compression method
      Include(CurrentItem.FCompressionFlags, acfCompressionMethod);
      CurrentItem.FCompressionMethod := GetCoderAlgorithm(FCompressionParams);
      // compression level
      Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
      CurrentItem.FCompressionLevel := GetCoderLevel(FCompressionParams);
      // dictionary level
      Include(CurrentItem.FCompressionFlags, acfCompressionLevelAux);
      CurrentItem.FCompressionLevelAux := GetCoderLevelAux(FCompressionParams);
      // compression block
      Include(CurrentItem.FCompressionFlags, acfCompressionBlock);
      CurrentItem.FCompressionBlock := GetCoderBlock(FCompressionParams);
      // default compression table flag
      Exclude(CurrentItem.FCompressionFlags, acfCompressionFilter);
      CurrentItem.FCompressionFilter := '';
      Exclude(CurrentItem.FCompressionFlags, acfCompressionFilterAux);
      CurrentItem.FCompressionFilterAux := '';
      // force file extension option
      PreviousFileExt := CurrentFileExt;
      if GetCoderFilter(FCompressionParams) = '' then
        CurrentFileExt := ExtractFileExt(CurrentItem.FExternalFileName)
      else
        CurrentFileExt := GetCoderFilter(FCompressionParams);
      // compression block option
      if AnsiCompareFileName(CurrentFileExt, PreviousFileExt) = 0 then
      begin
        Dec(Block, CurrentItem.FExternalFileSize);
        if Block < 0 then
        begin
          Block := GetCoderBlock(FCompressionParams);
          CurrentItem.FCompressionBlock := 0;
        end;
      end else
      begin
        Block := GetCoderBlock(FCompressionParams);
        CurrentItem.FCompressionBlock := 0;
      end;
      // BEE compression method
      if CurrentItem.FCompressionMethod = caBee then
      begin
        Include(CurrentItem.FCompressionFlags, acfCompressionFilter);
        if Configuration.GetTable(CurrentFileExt, CurrentTable) then
          CurrentItem.FCompressionFilter := Hex(CurrentTable, SizeOf(CurrentTable))
        else
          CurrentItem.FCompressionFilter := Hex(DefaultTableParameters, SizeOf(CurrentTable));
      end;

      // encryption method
      CurrentItem.FEncryptionMethod    := GetCipherAlgorithm(FEncryptionParams);
      // check method
      CurrentItem.FCheckMethod         := GetHashAlgorithm   (FCheckParams);
      CurrentItem.FCheckMethodAux      := GetHashAlgorithmAux(FCheckParams);
      // version needed to read
      CurrentItem.FVersionNeededToRead := GetVersionNeededToRead(CurrentItem);
    end;
  end;
  FreeAndNil(Configuration);
end;

function TArchiver.DoUpdate(Item: TCustomSearchRec): TArchiveConfirm;
var
  I: longint;
  UpdateAs: string;
begin
  Result := arcCancel;
  if Assigned(FOnUpdate) then
  begin
    UpdateAs := Item.Name;
    FOnUpdate(Item, UpdateAs, Result);
    if Result = arcOk then
    begin
      I := IndexOf(UpdateAs);
      if I = -1 then
        I := FCentralDirectory.Add(TArchiveItem.Create(UpdateAs))
      else
        if FCentralDirectory.Items[I].FTag = aitNone then
          FCentralDirectory.Items[I].FTag := aitUpdate;
      FCentralDirectory.Items[I].Update(Item);
      Result := DoComment(FCentralDirectory.Items[I]);
    end;
  end;
end;

procedure TArchiver.CheckTags4Update;
var
  I: longint;
  Item: TCustomSearchRec;
begin
  FSearchRecs.Sort(@CompareCustomSearchRec);
  for I := 0 to FSearchRecs.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;
    Item := TCustomSearchRec(FSearchRecs.Items[I]);
    case DoUpdate(Item) of
      arcOk: begin
        FIsNeededToRun  := TRUE;
        FIsNeededToSave := TRUE;
      end;
    //arcCancel: nothing to do
      arcQuit: SetExitStatus(esUserAbortError);
    end;
  end;

  for I := 0 to FSearchRecs.Count - 1 do
    TCustomSearchRec(FSearchRecs[I]).Destroy;
  FSearchRecs.Clear;
  Configure;
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
      aitNone:            Inc(FTotalSize, Item.CompressedSize);
      aitAdd:             Inc(FTotalSize, Item.FExternalFileSize);
      aitUpdate:          Inc(FTotalSize, Item.FExternalFileSize);
      aitDecode:          Inc(FTotalSize, Item.UncompressedSize + Item.UncompressedSize);
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
  if FIsNeededToRun then
  begin
    CheckSequences4Update;
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
    FTempWriter.WriteDWord(ARCHIVE_DATA_MARKER);

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

