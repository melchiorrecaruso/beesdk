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

    v0.8.0 build 1864 - 2013.02.16 by Melchiorre Caruso.
}

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
  Bee_Configuration;

const
  /// beex archive marker
  beexArchiveMarker = $78454542;

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

  /// archive data descriptor flag
  TArchiveDataDescriptorFlag = (
    adfCompressedSize,
    adfDiskNumber,
    adfDiskSeek,
    adfCRC32);

  TArchiveDataDescriptorFlags = set of TArchiveDataDescriptorFlag;

  /// archive compression method
  TArchiveCompressionMethod = (acmNone, acmBee);

  /// archive compression level
  TArchiveCompressionLevel = (aclFast, aclNormal, aclMaximum);

  /// archive dictionary level
  TArchiveDictionaryLevel = (adl2MB, adl5MB, adl10MB, adl20MB, adl40MB, adl80MB,
    adl160MB, adl320MB, adl640MB, adl1280MB);

  /// archive compression flag
  TArchiveCompressionFlag = (
    acfCompressionMethod,
    acfCompressionLevel,
    acfDictionaryLevel,
    acfCompressionBlock,
    acfCompressionTable);

  TArchiveCompressionFlags = set of TArchiveCompressionFlag;

  /// archive encryption method
  TArchiveEncryptionMethod = (aemNone, aemBlowFish);

  /// archive encryption flag
  TArchiveEncryptionFlag = (
    aefEncryptionMethod);

  TArchiveEncryptionFlags = set of TArchiveEncryptionFlag;

  /// archive item tag
  TArchiveItemTag = (aitNone, aitAdd, aitUpdate, aitDecode, aitDecodeAndUpdate);

  /// archive item
  TArchiveItem = class(TObject)
  protected
    // item property
    FFileName: string;
    FFlags: TArchiveItemFlags;
    FVersionNeededToExtract: longword;
    FUncompressedSize: int64;
    FLastModifiedTime: longint;
    FAttributes: longword;
    FComment: string;
    // data descriptor property
    FDataDescriptorFlags: TArchiveDataDescriptorFlags;
    FCompressedSize: int64;
    FDiskNumber: longword;
    FDiskSeek: int64;
    FCRC32: longword;
    // compression property
    FCompressionFlags: TArchiveCompressionFlags;
    FCompressionMethod: TArchiveCompressionMethod;
    FCompressionLevel: TArchiveCompressionLevel;
    FDictionaryLevel: TArchiveDictionaryLevel;
    FCompressionTable: TTableParameters;
    // encryption property
    FEncryptionFlags: TArchiveEncryptionFlags;
    FEncryptionMethod: TArchiveEncryptionMethod;
  protected
    FPosition: longint;
    FTag: TArchiveItemTag;
    FExternalFileName: string;
    FExternalFileSize: int64;
    function GetCompressionBlock: boolean;
  public {methods}
    constructor Create(const aFileName: string);
    constructor Read(Stream: TFileReader);
    procedure Write(Stream: TFileWriter);
    procedure Update(SearchRec: TCustomSearchRec);
  public {property}
    property FileName: string read FFileName;
    property Flags: TArchiveItemFlags read FFlags;
    property VersionNeededToExtract: longword read FVersionNeededToExtract;
    property UncompressedSize: int64 read FUncompressedSize;
    property LastModifiedTime: longint read FLastModifiedTime;
    property Attributes: longword read FAttributes;
    property Comment: string read FComment;
    // data descriptor property
    property DadaDescriptorFlags: TArchiveDataDescriptorFlags read FDataDescriptorFlags;
    property CompressedSize: int64 read FCompressedSize;
    property DiskNumber: longword read FDiskNumber;
    property DiskSeek: int64 read FDiskSeek;
    property CRC32: longword read FCRC32;
    // compression property
    property CompressionFlags: TArchiveCompressionFlags read FCompressionFlags;
    property CompressionMethod: TArchiveCompressionMethod read FCompressionMethod;
    property CompressionLevel: TArchiveCompressionLevel read FCompressionLevel;
    property DictionaryLevel: TArchiveDictionaryLevel read FDictionaryLevel;
    property CompressionBlock: boolean read GetCompressionBlock;
    property CompressionTable: TTableParameters read FCompressionTable;
    // encryption property
    property EncryptionFlags: TArchiveEncryptionFlags read FEncryptionFlags;
    property EncryptionMethod: TArchiveEncryptionMethod read FEncryptionMethod;
  end;

  /// archive item list
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

  /// ...
  TArchiveConfirm = (arcOk, arcCancel, arcQuit);

  /// archive events
  TArchiveMessageEvent = procedure(const aMessage: string) of object;

  TArchiveRenameEvent = procedure(Item: TArchiveItem;
    var RenameAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveExtractEvent = procedure(Item: TArchiveItem;
    var ExtractAs: string; var Confirm: TArchiveConfirm) of object;

  TArchiveDeleteEvent = procedure(Item: TArchiveItem;
    var Confirm: TArchiveConfirm) of object;

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
    FArchiveComment: string;
    FTestTempArchive: boolean;
    FVolumeSize: int64;
    // items
    FArchiveItems: TArchiveItems;
    FSearchRecs: TList;
  private
    FEncoder: TStreamEncoder;
    procedure InitEncoder      (Item: TArchiveItem);
    procedure EncodeFromArchive(Item: TArchiveItem);
    procedure EncodeFromSwap   (Item: TArchiveItem);
    procedure EncodeFromFile   (Item: TArchiveItem);
  private
    FDecoder: TStreamDecoder;
    procedure InitDecoder      (Item: TArchiveItem);
    procedure DecodeToNul      (Item: TArchiveItem);
    procedure DecodeToSwap     (Item: TArchiveItem);
    procedure DecodeToFile     (Item: TArchiveItem);
  private
    procedure PackCentralDirectory;
    procedure WriteCentralDirectory(aStream: TFileWriter);
    procedure ReadCentralDirectory(aStream: TFileReader);
    procedure UnPackCentralDirectory;

    procedure Swapping;
    procedure TestTemporaryArchive;
    procedure SaveTemporaryArchive;
  private
    procedure SetArchiveName(const Value: string);
    procedure SetWorkDirectory(const Value: string);
    procedure SetSfxName(const Value: string);

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
  private
    FOnRename: TArchiveRenameEvent;
    procedure CheckTags4Rename;
    procedure CheckSequences4Rename;
  private
    FOnDelete: TArchiveDeleteEvent;
    procedure CheckTags4Delete;
    procedure CheckSequences4Delete;
  private
    FOnUpdate: TArchiveUpdateEvent;
    procedure CheckTags4Update;
    procedure CheckSequences4Update;
    procedure Configure;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenArchive(const aArchiveName: string);
    procedure CloseArchive;
    procedure Suspend(Value: boolean);
    procedure Terminate;

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
    property OnRequestBlankImage: TFileWriterRequestBlankImageEvent read FOnRequestBlankImage write FOnRequestBlankImage;
    property OnRequestImage: TFileReaderRequestImageEvent read FOnRequestImage write FOnRequestImage;
    property OnMessage: TArchiveMessageEvent read FOnMessage write FOnMessage;
    property OnProgress: TArchiveProgressEvent read FOnProgress write FOnProgress;
    property OnExtract: TArchiveExtractEvent read FOnExtract write FOnExtract;
    property OnRename: TArchiveRenameEvent read FOnRename write FOnRename;
    property OnDelete: TArchiveDeleteEvent read FOnDelete write FOnDelete;
    property OnUpdate: TArchiveUpdateEvent read FOnUpdate write FOnUpdate;

    property ArchiveName: string read FArchiveName write SetArchiveName;
    property ArchiveComment: string read FArchiveComment write FArchiveComment;
    property SelfExtractor: string read FSfxName write FSfxName;
    property WorkDirectory: string read FWorkDirectory write SetWorkDirectory;
    property CompressionParams: string read FCompressionParams write FCompressionParams;
    property EncryptionParams: string read FEncryptionParams write FEncryptionParams;
    property TestTempArchive: boolean read FTestTempArchive write FTestTempArchive;
    property VolumeSize: int64 read FVolumeSize write FVolumeSize;
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


function GetCompressionMethod(const Params: string): TArchiveCompressionMethod;
begin
  Result := acmNone;
end;

function GetCompressionBlock(const Params: string): int64;
begin
  Result := 0;
end;

function GetCompressionLevel(const Params: string): TArchiveCompressionLevel;
begin
  Result := aclFast;
end;

function GetDictionaryLevel(const Params: string): TArchiveDictionaryLevel;
begin
  Result := adl2MB;
end;

function GetForceFileExtension(const Params: string): string;
begin
  Result := '';
end;

function GetEncryptionMethod(const Params: string): TArchiveEncryptionMethod;
begin
  Result := aemNone;
end;

function GetConfigurationFileName(const Params: string): string;
begin
  Result := SelfPath + DefaultCfgName;
end;

function GetVersionNeededToExtract(Item: TArchiveItem): longword;
begin
  Result := beexVersionNeededToExtract;

end;







function CompressionMethodToStr(Item: TArchiveItem): string;
begin
  Result := 'm0a';
  if Item.CompressionMethod <> acmNone then
  begin
    if Item.CompressionBlock then
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
  FVersionNeededToExtract :=  0;
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
  FCompressionMethod := acmNone;
  FCompressionLevel  := aclFast;
  FDictionaryLevel   := adl2MB;
  FCompressionTable  := DefaultTableParameters;
  /// Encryption property ///
  FEncryptionFlags   := [];
  FEncryptionMethod  := aemNone;
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
  if (acfCompressionLevel  in FCompressionFlags)    then FCompressionLevel  := TArchiveCompressionLevel(Stream.ReadInfWord);
  if (acfDictionaryLevel   in FCompressionFlags)    then FDictionaryLevel   := TArchiveDictionaryLevel(Stream.ReadInfWord);
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

function TArchiveItem.GetCompressionBlock: boolean;
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
    I := AnsiCompareFileName(FileName, TArchiveItem(FNames[Med]).FileName);
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
  FArchiveComment    := '';
  FTestTempArchive   := FALSE;
  FVolumeSize        := 0;
  // items list
  FArchiveItems      := TArchiveItems.Create;
  FSearchRecs        := TList.Create;
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
  if Item.CompressionMethod = acmBee then
  begin
    if acfDictionaryLevel in Item.FCompressionFlags then
      FEncoder.DictionaryLevel := Ord(Item.DictionaryLevel);

    if acfCompressionTable in Item.FCompressionFlags then
      FEncoder.CompressionTable := Item.CompressionTable;

    FEncoder.FreshModeller(Item.CompressionBlock);
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
    acmBee: FEncoder.Encode(FSwapReader, Item.FUncompressedSize, Item.FCRC32);
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
    acmBee: FEncoder.Encode(Stream, Item.FUncompressedSize, Item.FCRC32);
    else     FEncoder.Copy  (Stream, Item.FUncompressedSize, Item.FCRC32);
  end;
  Item.FCompressedSize := FTempWriter.SeekFromCurrent - Item.FDiskSeek;

  Stream.Destroy;
end;

procedure TArchiver.InitDecoder(Item: TArchiveItem);
begin
  if Item.CompressionMethod = acmBee then
  begin
    if acfDictionaryLevel in Item.FCompressionFlags then
      FDecoder.DictionaryLevel := Ord(Item.DictionaryLevel);

    if acfCompressionTable in Item.FCompressionFlags then
      FDecoder.CompressionTable := Item.CompressionTable;

    FDecoder.FreshModeller(Item.CompressionBlock);
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
    acmBee: FDecoder.Decode(FSwapWriter, Item.FUncompressedSize, CRC);
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
    acmBee: FDecoder.Decode(Stream, Item.FUncompressedSize, CRC);
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
    acmBee: FDecoder.Decode(Stream, Item.FUncompressedSize, CRC);
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
      if not(aifVersionNeededToExtract in CurrentItem.FFlags) then CurrentItem.FVersionNeededToExtract := PreviusItem.FVersionNeededToExtract;
      if not(aifUncompressedSize       in CurrentItem.FFlags) then CurrentItem.FUncompressedSize       := PreviusItem.FUncompressedSize;
      if not(aifLastModifiedTime       in CurrentItem.FFlags) then CurrentItem.FLastModifiedTime       := PreviusItem.FLastModifiedTime;
      if not(aifAttributes             in CurrentItem.FFlags) then CurrentItem.FAttributes             := PreviusItem.FAttributes;
      if not(aifComment                in CurrentItem.FFlags) then CurrentItem.FComment                := PreviusItem.FComment;
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

procedure TArchiver.Swapping;
var
  I: longint;
  CRC: longword;
  Item: TArchiveItem;
begin
  if ExitStatus <> esNoError then Exit;

  FSwapName   := GenerateFileName(FWorkDirectory);
  FSwapWriter := TFileWriter.Create(FSwapName, FOnRequestBlankImage, 0);
  FSwapWriter.WriteDWord(beexArchiveMarker);

  FDecoder := TStreamDecoder.Create(FArchiveReader);
  FDecoder.OnProgress := DoProgress;
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;

    Item := FArchiveItems.Items[I];
    if Item.FTag in [aitDecode, aitDecodeAndUpdate] then
    begin
      InitDecoder(Item);
      case Item.FTag of
        aitDecode:          DoMessage(Format(cmSwapping, [Item.FFileName]));
        aitDecodeAndUpdate: DoMessage(Format(cmDecoding, [Item.FFileName]));
      end;

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
      for I := 0 to FArchiveItems.Count - 1 do
      begin
        FArchiveItems.Items[I].FTag := aitUpdate;
        Inc(FTotalSize, FArchiveItems.Items[I].CompressedSize);
      end;

      FArchiveReader := TFileReader.Create(FTempName, FOnRequestImage);
      FTempWriter    := TFileWriter.Create(FArchiveName, FOnRequestBlankImage, FVolumeSize);
      FTempWriter.WriteDWord(beexArchiveMarker);

      FEncoder := TStreamEncoder.Create(FTempWriter);
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
  FArchiveItems.Clear;
  FSearchRecs.Clear;

  FArchiveName := '';
  FSwapName    := '';
  FTempName    := '';

  FSuspended      := FALSE;
  FIsNeededToRun  := FALSE;
  FIsNeededToSwap := FALSE;
  FIsNeededToSave := FALSE;
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
    if FArchiveItems.Items[I].CompressionBlock = FALSE then
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
    if FArchiveItems.Items[I].CompressionBlock = FALSE then
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

// TArchiver # SET PROPERTY #

procedure TArchiver.SetArchiveName(const Value: string);
begin
  OpenArchive(Value);
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
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;
    if FArchiveItems.Items[I].FTag = aitUpdate then
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
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;

    Item := FArchiveItems.Items[I];
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
    CheckSequences4Extract;
    FDecoder := TStreamDecoder.Create(FArchiveReader);
    FDecoder.OnProgress := DoProgress;
    for I := 0 to FArchiveItems.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;
      Item := FArchiveItems.Items[I];
      InitDecoder(Item);
      case Item.FTag of
        aitUpdate: DoMessage(Format(cmExtracting, [Item.FExternalFileName]));
        aitDecode: DoMessage(Format(cmDecoding,   [Item.FFileName]));
      end;
      case Item.FTag of
        aitUpdate: DecodeToFile(Item);
        aitDecode: DecodeToNul (Item);
        else SetExitStatus(esCaseError);
      end;
    end;
    FreeAndNil(FDecoder);
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
    FDecoder := TStreamDecoder.Create(FArchiveReader);
    FDecoder.OnProgress := DoProgress;
    for I := 0 to FArchiveItems.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;
      Item := FArchiveItems.Items[I];
      InitDecoder(Item);
      case Item.FTag of
        aitUpdate: DoMessage(Format(cmTesting,  [Item.FFileName]));
        aitDecode: DoMessage(Format(cmDecoding, [Item.FFileName]));
      end;
      case Item.FTag of
        aitUpdate: DecodeToNul(Item);
        aitDecode: DecodeToNul(Item);
        else SetExitStatus(esCaseError);
      end;
    end;
    FreeAndNil(FDecoder);
  end;
end;

// TArchiver # RENAME #

procedure TArchiver.CheckTags4Rename;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
  RenameAs: string;
begin
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;

    Item := FArchiveItems.Items[I];
    if Item.FTag in [aitUpdate] then
    begin
      Confirm := arcCancel;
      if Assigned(FOnRename) then
      begin
        RenameAs := Item.FFileName;
        FOnRename(Item, RenameAs, Confirm);
      end;

      case Confirm of
        arcOk: begin
          Item.FFileName  := RenameAs;
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
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
    FTempWriter.WriteDWord(beexArchiveMarker);

    FEncoder := TStreamEncoder.Create(FTempWriter);
    FEncoder.OnProgress := DoProgress;
    for I := 0 to FArchiveItems.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;
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
end;

// TArchiver # DELETE #

procedure TArchiver.CheckTags4Delete;
var
  I: longint;
  Item: TArchiveItem;
  Confirm: TArchiveConfirm;
begin
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;

    Item := FArchiveItems.Items[I];
    if Item.FTag in [aitUpdate] then
    begin
      Confirm := arcCancel;
      if Assigned(FOnDelete) then
        FOnDelete(Item, Confirm);

      case Confirm of
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
    CheckSequences4Delete;
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
    FTempWriter.WriteDWord(beexArchiveMarker);

    if FIsNeededToSwap then Swapping;
    for I := FArchiveItems.Count - 1 downto 0 do
    begin
      if ExitStatus = esNoError then Break;

      Item := FArchiveItems.Items[I];
      if Item.FTag in [aitUpdate, aitDecodeAndUpdate] then
      begin
        DoMessage(Format(cmDeleting, [Item.FileName]));
        FArchiveItems.Delete(I);
      end;
    end;

    FEncoder := TStreamEncoder.Create(FTempWriter);
    FEncoder.OnProgress := DoProgress;
    for I := 0 to FArchiveItems.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;

      Item := FArchiveItems.Items[I];
      InitDecoder(Item);
      case Item.FTag of
        aitNone:   DoMessage(Format(cmCopying,  [Item.FileName]));
        aitDecode: DoMessage(Format(cmEncoding, [Item.FileName]));
      end;
      case Item.FTag of
        aitNone:   EncodeFromArchive(Item);
        aitDecode: EncodeFromSwap   (Item);
        else SetExitStatus(esCaseError);
      end;
    end;
    FreeandNil(FEncoder);
    if ExitStatus = esNoError then
      WriteCentralDirectory(FTempWriter);
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
    if TCustomSearchRec(Item1).Size < TCustomSearchRec(Item2).Size then
      Result := -1
    else
      if TCustomSearchRec(Item1).Size > TCustomSearchRec(Item2).Size then
        Result :=  1;
  end;
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
  if FileExists(GetConfigurationFileName(FCompressionParams)) then
    Configuration.LoadFromFile(GetConfigurationFileName(FCompressionParams))
  else
    SetExitStatus(esLoadConfigError);

  CurrentFileExt := '.';
  Configuration.Selector('\main');
  Configuration.CurrentSection.Values['Method']     := IntToStr(Ord(GetCompressionLevel(FCompressionParams)));
  Configuration.CurrentSection.Values['Dictionary'] := IntToStr(Ord(GetDictionaryLevel (FCompressionParams)));
  Configuration.Selector('\m' + Configuration.CurrentSection.Values['Method']);

  Block := GetCompressionBlock(FCompressionParams);
  for I := 0 to FArchiveItems.Count - 1 do
  begin
    CurrentItem := FArchiveItems.Items[I];
    if CurrentItem.FTag = aitAdd then
    begin
      // compression method
      Include(CurrentItem.FCompressionFlags, acfCompressionMethod);
      CurrentItem.FCompressionMethod := GetCompressionMethod(FCompressionParams);
      // compression level
      Include(CurrentItem.FCompressionFlags, acfCompressionLevel);
      CurrentItem.FCompressionLevel := GetCompressionLevel (FCompressionParams);
      // dictionary level
      Include(CurrentItem.FCompressionFlags, acfDictionaryLevel);
      CurrentItem.FDictionaryLevel := GetDictionaryLevel  (FCompressionParams);
      // default compression block flag
      Exclude(CurrentItem.FCompressionFlags, acfCompressionBlock);
      // default compression table flag
      Exclude(CurrentItem.FCompressionFlags, acfCompressionTable);

      // force file extension option
      PreviousFileExt := CurrentFileExt;
      if GetForceFileExtension(FCompressionParams) = '' then
        CurrentFileExt := ExtractFileExt(CurrentItem.FExternalFileName)
      else
        CurrentFileExt := GetForceFileExtension(FCompressionParams);

      // solid compression option
      if AnsiCompareFileName(CurrentFileExt, PreviousFileExt) = 0 then
      begin
        Dec(Block, CurrentItem.UncompressedSize);
        if Block < 0 then
          Block := GetCompressionBlock(FCompressionParams)
        else
          Include(CurrentItem.FCompressionFlags, acfCompressionBlock);
      end else
      begin
        // BEE compression method
        if CurrentItem.FCompressionMethod = acmBee then
        begin
          Include(CurrentItem.FCompressionFlags, acfCompressionTable);
          if Configuration.GetTable(CurrentFileExt, CurrentTable) then
            CurrentItem.FCompressionTable := CurrentTable
          else
            CurrentItem.FCompressionTable := DefaultTableParameters;
        end;
        Block := GetCompressionBlock(FCompressionParams);
      end;

      // encryption method
      if GetEncryptionMethod(FEncryptionParams) <> aemNone then
      begin
        Include(CurrentItem.FEncryptionFlags, aefEncryptionMethod);
        CurrentItem.FEncryptionMethod := GetEncryptionMethod(FEncryptionParams);
      end;
      CurrentItem.FVersionNeededToExtract := GetVersionNeededToExtract(CurrentItem);
    end;
  end;
  FreeAndNil(Configuration);
end;

procedure TArchiver.CheckTags4Update;
var
  I, J: longint;
  Item: TCustomSearchRec;
  Confirm: TArchiveConfirm;
  UpdateAs: string;
begin
  FSearchRecs.Sort(@CompareCustomSearchRec);
  for J := 0 to FSearchRecs.Count - 1 do
  begin
    if ExitStatus <> esNoError then Break;

    Item := TCustomSearchRec(FSearchRecs.Items[J]);

    Confirm := arcCancel;
    if Assigned(FOnUpdate) then
    begin
      UpdateAs := Item.Name;
      FOnUpdate(Item, UpdateAs, Confirm);
    end;

    case Confirm of
      arcOk: begin
        I := Find(UpdateAs);
        if I = -1 then
        begin
          I := FArchiveItems.Add(TArchiveItem.Create(UpdateAs));
        end else
        begin
          if FArchiveItems.Items[I].FTag = aitNone then
            FArchiveItems.Items[I].FTag := aitUpdate;
        end;
        FArchiveItems.Items[I].Update(Item);
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
  Configure;
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
    CheckSequences4Update;
    FTempName   := GenerateFileName(FWorkDirectory);
    FTempWriter := TFileWriter.Create(FTempName, FOnRequestBlankImage, 0);
    FTempWriter.WriteDWord(beexArchiveMarker);

    if FIsNeededToSwap then Swapping;
    FEncoder := TStreamEncoder.Create(FTempWriter);
    FEncoder.OnProgress := DoProgress;
    for I := 0 to FArchiveItems.Count - 1 do
    begin
      if ExitStatus <> esNoError then Break;

      Item := FArchiveItems.Items[I];
      InitEncoder(Item);
      case Item.FTag of
        aitNone:            DoMessage(Format(cmCopying,  [Item.FileName]));
        aitAdd:             DoMessage(Format(cmAdding,   [Item.FileName]));
        aitUpdate:          DoMessage(Format(cmUpdating, [Item.FileName]));
        aitDecode:          DoMessage(Format(cmEncoding, [Item.FileName]));
        aitDecodeAndUpdate: DoMessage(Format(cmUpdating, [Item.FileName]));
      end;
      case Item.FTag of
        aitNone:            EncodeFromArchive(Item);
        aitAdd:             EncodeFromFile   (Item);
        aitUpdate:          EncodeFromFile   (Item);
        aitDecode:          EncodeFromSwap   (Item);
        aitDecodeAndUpdate: EncodeFromFile   (Item);
        else SetExitStatus(esCaseError);
      end;
    end;
    FreeAndNil(FEncoder);
    if ExitStatus = esNoError then
      WriteCentralDirectory(FTempWriter);
  end;
end;

end.

