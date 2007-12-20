(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

 {*********************************************************}
 {* ABBREVIA: AbZipTyp.pas 3.05                           *}
 {*********************************************************}
 {* ABBREVIA: PKZip types                                 *}
 {* Based on information from Appnote.txt, shipped with   *}
 {* PKWare's PKZip for Windows 2.5                        *}
 {*********************************************************}

{$I AbDefine.inc}

unit AbZipTyp;

interface

uses
  Classes, AbArcTyp, AbUtils, AbSpanSt;

const
  { note  #$50 = 'P', #$4B = 'K'}
  Ab_ZipVersion = 21;
  Ab_ZipLocalFileHeaderSignature: longint = $04034B50;
  Ab_ZipCentralDirectoryFileHeaderSignature: longint = $02014B50;
  Ab_ZipCentralDirectoryTailSignature: longint = $06054B50;
  Ab_ZipSpannedSetSignature: longint = $08074B50;
  Ab_ZipPossiblySpannedSignature: longint = $30304B50;
  Ab_GeneralZipSignature: word = $4B50;       {!!.02}

  Ab_WindowsExeSignature: word = $5A4D;       {!!.02}
  Ab_LinuxExeSigWord1: word = $457F;       {!!.02}
  Ab_LinuxExeSigWord2: word = $464C;       {!!.02}

  Ab_iWindowSize = $8000;  {Inflate window size}
  Ab_iMaxCodeLen = 16;     {Maximum bit length of any code}
  Ab_iMaxCodes = 288;    {Maximum number of codes in any set}
  Size32K = 32768;
  AbDefZipSpanningThreshold = 0;
  AbDefPasswordRetries = 3;
  AbFileIsEncryptedFlag = $0001;
  AbHasDataDescriptorFlag = $0008;

var
  Ab_ZipEndCentralDirectorySignature: longint = $06054B50;

type
  PAbByteArray4K = ^TAbByteArray4K;
  TAbByteArray4K = array[1..4096] of byte;
  PAbByteArray8K = ^TAbByteArray8K;
  TAbByteArray8K = array[0..8192] of byte;
  PAbIntArray8K  = ^TAbIntArray8K;
  TAbIntArray8K  = array[0..8192] of smallint;

  PAbWordArray = ^TAbWordArray;
  TAbWordArray = array[0..65535 div SizeOf(word) - 1] of word;
  PAbByteArray = ^TAbByteArray;
  TAbByteArray = array[0..65535 - 1] of byte;
  PAbSmallIntArray = ^TAbSmallIntArray;
  TAbSmallIntArray = array[0..65535 div SizeOf(smallint) - 1] of smallint;

  PAbIntegerArray = ^TAbIntegerArray;
  TAbIntegerArray = array[0..65535 div sizeof(integer) - 1] of integer;

  PAbiSlide = ^TAbiSlide;
  TAbiSlide = array[0..Ab_iWindowSize] of byte;

  PPAbHuft = ^PAbHuft;
  PAbHuft  = ^TAbHuft;

  TAbHuft = packed record
    ExtraBits: byte;   {Number of extra bits}
    NumBits: byte;   {Number of bits in this code or subcode}
    Filler:  word;
    case byte of
      0: (N: word);  {Literal, length base, or distance base}
      1: (NextLevel: PAbHuft); {Pointer to next level of table}
  end;

  TAbFollower =                      {used to expand reduced files}
    packed record
    Size: byte;                 {size of follower set}
    FSet: array[0..31] of byte; {follower set}
  end;
  PAbFollowerSets = ^TAbFollowerSets;
  TAbFollowerSets = array[0..255] of TAbFollower;


  PAbSfEntry = ^TAbSfEntry;
  TAbSfEntry =                       {entry in a Shannon-Fano tree}
    packed record
    case byte of
      0: (Code: word;
        Value, BitLength: byte);
      1: (L: longint);
  end;
  PAbSfTree = ^TAbSfTree;

  TAbSfTree =
    packed record                        {a Shannon-Fano tree}
    Entries: smallint;
    MaxLength: smallint;
    Entry: array[0..256] of TAbSfEntry;
  end;

  PAbWord = ^word;

  TAbFCData = packed record
    case byte of
      0: (Freq: word);  {frequency count}
      1: (Code: word);  {bit string}
  end;

  TAbDLData = packed record
    case byte of
      0: (Dad: word);  {father node in Huffman tree}
      1: (Len: word);  {length of bit string}
  end;

  {Data structure describing a single value and its code string}
  TAbCTData = packed record
    FC: TAbFCData;
    Filler: word;
    DL: TAbDLData;
  end;
  PAbCTDataArray = ^TAbCTDataArray;
  TAbCTDataArray = array[0..65535 div SizeOf(TAbCTData) - 1] of TAbCTData;

  TAbTreeDescription = packed record
    DynamicTree: PAbCTDataArray;  {the dynamic tree}
    StaticTree: PAbCTDataArray;  {corresponding static tree or NULL}
    ExtraBits: PAbWordArray;    {extra bits for each code or NULL}
    ExtraBase: smallint;         {base index for ExtraBits}
    MaxElements: smallint;         {max number of elements in the tree}
    MaxLength: smallint;         {max bit length for the codes}
    MaxCode: smallint;         {largest code with non zero frequency}
  end;


type
  TAbFileType =
    (Binary, Ascii, Unknown);

  TAbZipCompressionMethod =
    (cmStored, cmShrunk, cmReduced1, cmReduced2, cmReduced3,
    cmReduced4, cmImploded, cmTokenized, cmDeflated,
    cmEnhancedDeflated, cmDCLImploded, cmBestMethod);

  TAbZipSupportedMethod =
    (smStored, smDeflated, smBestMethod);

  TAbZipHostOS =
    (hosDOS, hosAmiga, hosVAX, hosUnix, hosVMCMS, hosAtari,
    hosOS2, hosMacintosh, hosZSystem, hosCPM, hosNTFS);

  {for method 6 - imploding}
  TAbZipDictionarySize =
    (dsInvalid, ds4K, ds8K);

  {for method 8 - deflating}
  TAbZipDeflationOption =
    (doInvalid, doNormal, doMaximum, doFast, doSuperFast);

type
  TAbNeedPasswordEvent = procedure(Sender: TObject;
    var NewPassword: string) of object;

const
  AbDefCompressionMethodToUse = smBestMethod;
  AbDefDeflationOption = doNormal;


type
  TAbZipDataDescriptor = class(TObject)
  protected {private}
    FCRC32: longint;
    FCompressedSize: longint;
    FUncompressedSize: longint;
  public {methods}
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  public {properties}
    property CRC32: longint Read FCRC32 Write FCRC32;
    property CompressedSize: longint Read FCompressedSize Write FCompressedSize;
    property UncompressedSize: longint Read FUncompressedSize Write FUncompressedSize;
  end;

type
  { TAbZipFileHeader interface =============================================== }
  {ancestor class for ZipLocalFileHeader and DirectoryFileHeader}
  TAbZipFileHeader = class(TObject)
  protected {private}
    FValidSignature: longint;
    FSignature: longint;
    FVersionNeededToExtract: word;
    FGeneralPurposeBitFlag: word;
    FCompressionMethod: word;
    FLastModFileTime: word;
    FLastModFileDate: word;
    FCRC32: longint;
    FCompressedSize: longint;
    FUncompressedSize: longint;
    FFileNameLength: word;
    FExtraFieldLength: word;
    FFileName: PChar;
    FExtraField: PChar;
    FIsValid: boolean;
  protected {methods}
    function GetCompressionMethod: TAbZipCompressionMethod;
    function GetCompressionRatio: double;
    function GetDataDescriptor: boolean;
    function GetDeflationOption: TAbZipDeflationOption;
    function GetDictionarySize: TAbZipDictionarySize;
    function GetEncrypted: boolean;
    function GetShannonFanoTreeCount: byte;
    function GetValid: boolean;
    procedure SetCompressionMethod(Value: TAbZipCompressionMethod);
    procedure SetExtraField(Value: PChar);
    procedure SetFileName(Value: PChar);
  public {methods}
    constructor Create;
    destructor Destroy; override;
  public {properties}
    property Signature: longint Read FSignature Write FSignature;
    property VersionNeededToExtract: word
      Read FVersionNeededToExtract Write FVersionNeededToExtract;
    property GeneralPurposeBitFlag: word Read FGeneralPurposeBitFlag
      Write FGeneralPurposeBitFlag;
    property CompressionMethod: TAbZipCompressionMethod
      Read GetCompressionMethod Write SetCompressionMethod;
    property LastModFileTime: word Read FLastModFileTime Write FLastModFileTime;
    property LastModFileDate: word Read FLastModFileDate Write FLastModFileDate;
    property CRC32: longint Read FCRC32 Write FCRC32;
    property CompressedSize: longint Read FCompressedSize Write FCompressedSize;
    property UncompressedSize: longint Read FUncompressedSize Write FUncompressedSize;
    property FileNameLength: word Read FFileNameLength Write FFileNameLength;
    property ExtraFieldLength: word Read FExtraFieldLength Write FExtraFieldLength;
    property FileName: PChar Read FFileName Write SetFileName;
    property ExtraField: PChar Read FExtraField Write SetExtraField;

    property CompressionRatio: double Read GetCompressionRatio;
    property DeflationOption: TAbZipDeflationOption Read GetDeflationOption;
    property DictionarySize: TAbZipDictionarySize Read GetDictionarySize;
    property HasDataDescriptor: boolean Read GetDataDescriptor;
    property IsValid: boolean Read GetValid;
    property IsEncrypted: boolean Read GetEncrypted;
    property ShannonFanoTreeCount: byte Read GetShannonFanoTreeCount;
  end;

  { TAbZipLocalFileHeader interface ========================================== }
  TAbZipLocalFileHeader = class(TAbZipFileHeader)
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  { TAbZipDirectoryFileHeader interface ====================================== }
  TAbZipDirectoryFileHeader = class(TAbZipFileHeader)
  protected {private}
    FVersionMadeBy: word;
    FFileCommentLength: word;
    FDiskNumberStart: word;
    FInternalFileAttributes: word;
    FExternalFileAttributes: longint;
    FRelativeOffset: longint;
    FFileComment: PChar;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  public {properties}
    property VersionMadeBy: word Read FVersionMadeBy Write FVersionMadeBy;
    property FileCommentLength: word Read FFileCommentLength Write FFileCommentLength;
    property DiskNumberStart: word Read FDiskNumberStart Write FDiskNumberStart;
    property InternalFileAttributes: word
      Read FInternalFileAttributes Write FInternalFileAttributes;
    property ExternalFileAttributes: longint
      Read FExternalFileAttributes Write FExternalFileAttributes;
    property RelativeOffset: longint Read FRelativeOffset Write FRelativeOffset;
    property FileComment: PChar Read FFileComment Write FFileComment;
  end;

  { TAbZipDirectoryFileFooter interface ====================================== }
  TAbZipDirectoryFileFooter = class(TObject)
  protected {private}
    FValidSignature: longint;
    FSignature:  longint;
    FDiskNumber: word;
    FStartDiskNumber: word;
    FEntriesOnDisk: word;
    FTotalEntries: word;
    FDirectorySize: longint;
    FDirectoryOffset: longint;
    FZipfileCommentLength: word;
    FZipFileComment: PChar;
    function GetValid: boolean;
  public {methods}
    constructor Create;
    destructor Destroy;
      override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  public {properties}
    property Signature: longint Read FSignature Write FSignature;
    property DiskNumber: word Read FDiskNumber Write FDiskNumber;
    property EntriesOnDisk: word Read FEntriesOnDisk Write FEntriesOnDisk;
    property TotalEntries: word Read FTotalEntries Write FTotalEntries;
    property DirectorySize: longint Read FDirectorySize Write FDirectorySize;
    property DirectoryOffset: longint Read FDirectoryOffset Write FDirectoryOffset;
    property StartDiskNumber: word Read FStartDiskNumber Write FStartDiskNumber;
    property ZipfileCommentLength: word Read FZipfileCommentLength
      Write FZipfileCommentLength;
    property ZipfileComment: PChar Read FZipfileComment Write FZipfileComment;
    property IsValid: boolean Read GetValid;
  end;

  { TAbZipItem interface ===================================================== }
  TAbZipItem = class(TAbArchiveItem)
  protected {private}
    FItemInfo: TAbZipDirectoryFileHeader;
    FDecoder:  TObject;

  protected {methods}
    function GetCompressionMethod: TAbZipCompressionMethod;
    function GetCompressionRatio: double;
    function GetDeflationOption: TAbZipDeflationOption;
    function GetDictionarySize: TAbZipDictionarySize;
    function GetDiskNumberStart: word;
    function GetExtraField: string;
    function GetFileComment: string;
    function GetGeneralPurposeBitFlag: word;
    function GetInternalFileAttributes: word;
    function GetRelativeOffset: longint;
    function GetShannonFanoTreeCount: byte;
    function GetVersionMadeBy: word;
    function GetVersionNeededToExtract: word;
    procedure SaveCDHToStream(Stream: TStream);
    procedure SaveDDToStream(Stream: TStream);
    procedure SaveLFHToStream(Stream: TStream);
    procedure SetCompressionMethod(Value: TAbZipCompressionMethod);
    procedure SetDiskNumberStart(Value: word);
    procedure SetFileComment(const Value: string);
    procedure SetExtraField(const Value: string);
    procedure SetGeneralPurposeBitFlag(Value: word);
    procedure SetInternalFileAttributes(Value: word);
    procedure SetRelativeOffset(Value: longint);
    procedure SetVersionMadeBy(Value: word);
    procedure SetVersionNeededToExtract(Value: word);

  protected {redefined property methods}
    function GetCompressedSize: longint; override;
    function GetCRC32: longint; override;
    function GetExternalFileAttributes: longint; override;
    function GetFileName: string; override;
    function GetIsEncrypted: boolean; override;
    function GetLastModFileDate: word; override;
    function GetLastModFileTime: word; override;
    function GetUncompressedSize: longint; override;
    procedure SetCompressedSize(const Value: longint); override;
    procedure SetCRC32(const Value: longint); override;
    procedure SetExternalFileAttributes(Value: longint); override;
    procedure SetFileName(const Value: string); override;
    procedure SetLastModFileDate(const Value: word); override;
    procedure SetLastModFileTime(const Value: word); override;
    procedure SetUncompressedSize(const Value: longint); override;

  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);

  public {properties}
    property CompressionMethod: TAbZipCompressionMethod
      Read GetCompressionMethod Write SetCompressionMethod;
    property CompressionRatio: double Read GetCompressionRatio;
    property DeflationOption: TAbZipDeflationOption Read GetDeflationOption;
    property DictionarySize: TAbZipDictionarySize Read GetDictionarySize;
    property DiskNumberStart: word Read GetDiskNumberStart Write SetDiskNumberStart;
    property ExtraField: string Read GetExtraField Write SetExtraField;
    property FileComment: string Read GetFileComment Write SetFileComment;
    property InternalFileAttributes: word
      Read GetInternalFileAttributes Write SetInternalFileAttributes;
    property GeneralPurposeBitFlag: word
      Read GetGeneralPurposeBitFlag Write SetGeneralPurposeBitFlag;
    property RelativeOffset: longint Read GetRelativeOffset Write SetRelativeOffset;
    property ShannonFanoTreeCount: byte Read GetShannonFanoTreeCount;
    property VersionMadeBy: word Read GetVersionMadeBy Write SetVersionMadeBy;
    property VersionNeededToExtract: word
      Read GetVersionNeededToExtract Write SetVersionNeededToExtract;
  end;

  { TAbZipArchive interface ================================================== }
  TAbZipArchive = class(TAbArchive)
  protected {private}
    FCompressionMethodToUse: TAbZipSupportedMethod;
    FCurrentDisk: word;
    FDeflationOption: TAbZipDeflationOption;
    FDriveIsRemovable: boolean;
    FInfo: TAbZipDirectoryFileFooter;
    FIsExecutable: boolean;
    FPassword: string;
    FPasswordRetries: byte;
    FStubSize: longint;
    FAutoGen: boolean;                               {!!.02}

    FExtractHelper: TAbArchiveItemExtractEvent;
    FExtractToStreamHelper: TAbArchiveItemExtractToStreamEvent;
    FTestHelper: TAbArchiveItemTestEvent;
    FInsertHelper: TAbArchiveItemInsertEvent;
    FInsertFromStreamHelper: TAbArchiveItemInsertFromStreamEvent;
    FOnNeedPassword: TAbNeedPasswordEvent;
    FOnRequestLastDisk: TAbRequestDiskEvent;
    FOnRequestNthDisk: TAbRequestNthDiskEvent;
    FOnRequestBlankDisk: TAbRequestDiskEvent;

  protected {methods}


    procedure DoExtractHelper(Index: integer; const NewName: string);
    procedure DoExtractToStreamHelper(Index: integer; aStream: TStream);
    procedure DoTestHelper(Index: integer);
    procedure DoInsertHelper(Index: integer; OutStream: TStream);
    procedure DoInsertFromStreamHelper(Index: integer; OutStream: TStream);
    procedure DoRequestNextImage(ImageNumber: integer; var Stream: TStream;
      var Abort: boolean);
    function FindCDTail: longint;
    function GetItem(Index: integer): TAbZipItem;
    function GetZipFileComment: string;
    procedure PutItem(Index: integer; Value: TAbZipItem);
    procedure DoRequestLastDisk(var Abort: boolean);
      virtual;
    procedure DoRequestNthDisk(DiskNumber: byte; var Abort: boolean);
      virtual;
    procedure DoRequestBlankDisk(var Abort: boolean);
      virtual;
    procedure ExtractItemAt(Index: integer; const NewName: string);
      override;
    procedure ExtractItemToStreamAt(Index: integer; aStream: TStream);
      override;
    procedure TestItemAt(Index: integer);
      override;
    function FixName(const Value: string): string;
      override;
    procedure LoadArchive;
      override;
    procedure SaveArchive;
      override;
    procedure SetZipFileComment(const Value: string);

  protected {properties}
    property IsExecutable: boolean Read FIsExecutable Write FIsExecutable;

  public {protected}
    procedure DoRequestNthImage(ImageNumber: integer; var Stream: TStream;
      var Abort: boolean);
    procedure DoSpanningMediaRequest(Sender: TObject; ImageNumber: integer;
      var ImageName: string; var Abort: boolean); override;
    procedure DoRequestImage(Mode: TAbSpanMode; ImageNumber: integer;   {!!.01}
      var ImageName: string; var Abort: boolean);                        {!!.01}

  public {methods}
    constructor Create(const FileName: string; Mode: word);
      override;
    constructor CreateFromStream(aStream: TStream; const ArchiveName: string);
      override;
    destructor Destroy;
      override;
    function CreateItem(const FileSpec: string): TAbArchiveItem; override; {!!.05}

  public {properties}

    property CompressionMethodToUse: TAbZipSupportedMethod
      Read FCompressionMethodToUse Write FCompressionMethodToUse;
    property CurrentDisk: word Read FCurrentDisk Write FCurrentDisk;
    property DeflationOption: TAbZipDeflationOption
      Read FDeflationOption Write FDeflationOption;
    property DriveIsRemovable: boolean Read FDriveIsRemovable;
    property ExtractHelper: TAbArchiveItemExtractEvent
      Read FExtractHelper Write FExtractHelper;
    property ExtractToStreamHelper: TAbArchiveItemExtractToStreamEvent
      Read FExtractToStreamHelper Write FExtractToStreamHelper;
    property TestHelper: TAbArchiveItemTestEvent Read FTestHelper Write FTestHelper;
    property InsertHelper: TAbArchiveItemInsertEvent
      Read FInsertHelper Write FInsertHelper;
    property InsertFromStreamHelper: TAbArchiveItemInsertFromStreamEvent
      Read FInsertFromStreamHelper Write FInsertFromStreamHelper;
    property Password: string Read FPassword Write FPassword;
    property PasswordRetries: byte Read FPasswordRetries
      Write FPasswordRetries default AbDefPasswordRetries;
    property StubSize: longint Read FStubSize;
    property ZipFileComment: string Read GetZipFileComment Write SetZipFileComment;

    property Items[Index: integer]: TAbZipItem                      {!!.03}
      Read GetItem                                                    {!!.03} Write PutItem;
      default;                                         {!!.03}

  public {events}
    property OnNeedPassword: TAbNeedPasswordEvent
      Read FOnNeedPassword Write FOnNeedPassword;
    property OnRequestLastDisk: TAbRequestDiskEvent
      Read FOnRequestLastDisk Write FOnRequestLastDisk;
    property OnRequestNthDisk: TAbRequestNthDiskEvent
      Read FOnRequestNthDisk Write FOnRequestNthDisk;
    property OnRequestBlankDisk: TAbRequestDiskEvent
      Read FOnRequestBlankDisk Write FOnRequestBlankDisk;
  end;

{============================================================================}
procedure MakeSelfExtracting(StubStream, ZipStream, SelfExtractingStream: TStream);
    {-takes an executable stub, and a .zip format stream, and creates
     a SelfExtracting stream.  The stub should create a TAbZipArchive
     passing itself as the file, using a read-only open mode.  It should
     then perform operations as needed - like ExtractFiles( '*.*' ).
     This routine updates the RelativeOffset of each item in the archive}

function FindCentralDirectoryTail(aStream: TStream): longint;

function VerifyZip(Strm: TStream): TAbArchiveType;

function VerifySelfExtracting(Strm: TStream): TAbArchiveType;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  //  Dialogs,                                                           {!!.04}
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$IFNDEF NoQt}
  {$IFDEF UsingCLX}
  QControls,
  QDialogs,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  AbConst,
  AbExcept,
  AbVMStrm,
  SysUtils;

function VerifyZip(Strm: TStream): TAbArchiveType;
  { determine if stream appears to be in PkZip format }
var
  Footer: TAbZipDirectoryFileFooter;
  Sig: longint;                                                {!!.01}
  TailPosition: longint;
  StartPos: longint;
begin
  StartPos := Strm.Position;
  Result := atUnknown;

  Strm.Position := 0;                                                {!!.02}
  Strm.Read(Sig, SizeOf(longint));                                   {!!.02}
  if (Sig = Ab_ZipSpannedSetSignature) or                            {!!.02}
    (Sig = Ab_ZipPossiblySpannedSignature) then                     {!!.02}
    Result := atSpannedZip                                           {!!.02}
  else
  begin                                                         {!!.02}

    { attempt to find Central Directory Tail }
    TailPosition := FindCentralDirectoryTail(Strm);
    if TailPosition <> -1 then
    begin
      { check Central Directory Signature }
      Footer := TAbZipDirectoryFileFooter.Create;
      try
        Footer.LoadFromStream(Strm);
        if Footer.FSignature = AB_ZipCentralDirectoryTailSignature then
          Result := atZip;
      finally
        Footer.Free;
      end;
    end;
(* {!!.02}
  else begin  { may be a span }                                          {!!.01}
    Strm.Seek(0, soFromBeginning);                                       {!!.01}
    Strm.Read(Sig, SizeOf(LongInt));                                     {!!.01}
    if (Sig = Ab_ZipSpannedSetSignature)                                 {!!.01}
      or (Sig = Ab_ZipPossiblySpannedSignature)                          {!!.01}
    then                                                                 {!!.01}
      Result := atSpannedZip;                                            {!!.01}
*)   {!!.02}
  end;                                                                   {!!.01}
  Strm.Position := StartPos;
end;

function VerifySelfExtracting(Strm: TStream): TAbArchiveType;
  { determine if stream appears to be an executable with appended PkZip data }
var
  FileSignature: longint;
  StartPos: longint;
  IsWinExe, IsLinuxExe: boolean;                                        {!!.01}
begin
  StartPos := Strm.Position;
  { verify presence of executable stub }
  {check file type of stub stream}
  Strm.Position := 0;
  Strm.Read(FileSignature, sizeof(FileSignature));

  Result := atSelfExtZip;

  {!!.01 -- re-written Executable Type Detection to allow use of non-native stubs }
  IsLinuxExe := False;
  IsWinExe := LongRec(FileSignature).Lo = Ab_WindowsExeSignature;        {!!.02}
  if not IsWinExe then
  begin
    IsLinuxExe := FileSignature = Ab_LinuxExeSigWord1; { check 1st sig }
    if IsLinuxExe then
    begin
      Strm.Read(FileSignature, SizeOf(FileSignature)); { check 2nd sig }
      IsLinuxExe := FileSignature = Ab_LinuxExeSigWord2;
    end;
  end;

  if not (IsWinExe or IsLinuxExe) then
    Result := atUnknown;

  {!!.01 -- end re-written }
  { Check for central directory tail }
  if VerifyZip(Strm) <> atZip then
    Result := atUnknown;

  Strm.Position := StartPos;
end;

{============================================================================}
function FindCentralDirectoryTail(aStream: TStream): longint;
{ search end of aStream looking for ZIP Central Directory structure
  returns position in stream if found (otherwise returns -1),
  leaves stream positioned at start of structure or at original
  position if not found }
const
  StartBufSize = 512;
  CMaxBufSize  = 64 * 1024;                                               {!!.01}
var
  StartPos: longint;
  TailRec:  packed record
    trSig: longint;
    trMid: array [0..15] of byte;
    trLen: word;
  end;
  Buffer: PAnsiChar;
  Offset: longint;
  TestPos: PAnsiChar;
  Done: boolean;
  BytesRead: longint;
  BufSize: integer;
  MaxBufSize: integer;                                                   {!!.01}
  CommentLen: integer;
  SpanState: boolean;                                                   {!!.01}
begin
  { if spanning stream, don't want stream to read past beginning of current span}
  MaxBufSize := CMaxBufSize;                                             {!!.01}
  SpanState  := False;                                                    {!!.01}
  if aStream is TAbSpanStream then
  begin                                 {!!.01}
    if (TAbSpanStream(aStream).Size > 0) and                             {!!.01}
      (TAbSpanStream(aStream).Size < CMaxBufSize) then                   {!!.01}
      MaxBufSize := TAbSpanStream(aStream).Size;                         {!!.01}
    SpanState := TAbSpanStream(aStream).IgnoreSpanning;                  {!!.01}
    TAbSpanStream(aStream).IgnoreSpanning := True;                       {!!.01}
  end;                                                                   {!!.01}

  {save the starting position}
  StartPos := aStream.Seek(0, soFromCurrent);

  {start off with the majority case: no zip file comment, so the
   central directory tail is the last thing in the stream and it's a
   fixed size and doesn't indicate a zip file comment}
  Result := aStream.Seek(-sizeof(TailRec), soFromEnd);
  if (Result >= 0) then
  begin
    aStream.ReadBuffer(TailRec, sizeof(TailRec));
    if (TailRec.trSig = Ab_ZipEndCentralDirectorySignature) and
      (TailRec.trLen = 0) then
    begin
      aStream.Seek(Result, soFromBeginning);
      Exit;
    end;
  end;

  {the zip stream seems to have a comment, or it has null padding
   bytes from some flaky program, or it's not even a zip formatted
   stream; we need to search for the tail signature}

  {get a buffer}
  BufSize := StartBufSize;
  GetMem(Buffer, BufSize);
  try

    {start out searching backwards}
    Offset := -BufSize;

    {while there is still data to search ...}
    Done := False;
    while not Done do
    begin

      {seek to the search position}
      Result := aStream.Seek(Offset, soFromEnd);
      if (Result <= 0) then
      begin                                        {!!.01}
        Result := aStream.Seek(0, soFromBeginning);
        Done := True;
      end;

      {read a buffer full}
      BytesRead := aStream.Read(Buffer^, BufSize);

      if BytesRead < sizeOf(TailRec) then
      begin
        Result := -1;
        Exit;
      end;

      {search backwards through the buffer looking for the signature}
      TestPos := Buffer + BytesRead - sizeof(TailRec);
      while (TestPos <> Buffer) and (PLongint(TestPos)^
          <> Ab_ZipEndCentralDirectorySignature) do
        Dec(TestPos);

      {if we found the signature...}
      if (PLongint(TestPos)^ = Ab_ZipEndCentralDirectorySignature) then
      begin

        {get the tail record at this position}
        Move(TestPos^, TailRec, sizeof(TailRec));

        {if it's as valid a tail as we can check here...}
        CommentLen := -Offset - (TestPos - Buffer + sizeof(TailRec));
        if (TailRec.trLen <= CommentLen) then
        begin

          {calculate its position and exit}
          Result := Result + (TestPos - Buffer);
          aStream.Seek(Result, soFromBeginning);
          Exit;
        end;
      end;

      {otherwise move back one step, doubling the buffer}
      if (BufSize < MaxBufSize) then
      begin                               {!!.01}
        {        write('+');}{!!.01}
        FreeMem(Buffer);
        BufSize := BufSize * 2;
        if BufSize > MaxBufSize then                                     {!!.01}
          BufSize := MaxBufSize;                                         {!!.01}
        GetMem(Buffer, BufSize);
      end;
      {      else}{!!.01}
      {        write('.');}{!!.01}
      Dec(Offset, BufSize - SizeOf(longint));
    end;

    {if we reach this point, the CD tail is not present}
    Result := -1;
    aStream.Seek(StartPos, soFromBeginning);
  finally
    FreeMem(Buffer);
  end;

  { put SpanStream back the way it was }
  if aStream is TAbSpanStream then                                       {!!.01}
    TAbSpanStream(aStream).IgnoreSpanning := SpanState;                  {!!.01}
end;

{============================================================================}

{ TAbZipDataDescriptor implementation ====================================== }
procedure TAbZipDataDescriptor.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FCRC32, sizeof(FCRC32));
  Stream.Read(FCompressedSize, sizeof(FCompressedSize));
  Stream.Read(FUncompressedSize, sizeof(FUncompressedSize));
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipDataDescriptor.SaveToStream(Stream: TStream);
begin
  {!!.01 -- rewritten}
  Stream.Write(FCRC32, sizeof(FCRC32));
  Stream.Write(FCompressedSize, sizeof(FCompressedSize));
  Stream.Write(FUncompressedSize, sizeof(FUncompressedSize));
  {!!.01 -- end rewritten}
end;

{ -------------------------------------------------------------------------- }

{ TAbZipFileHeader implementation ========================================== }
constructor TAbZipFileHeader.Create;
begin
  inherited Create;
  FValidSignature := $0;
  FFileName := nil;
  FExtraField := nil;
end;

{ -------------------------------------------------------------------------- }
destructor TAbZipFileHeader.Destroy;
begin
  if Assigned(FFileName) then
    StrDispose(FFileName);
  if Assigned(FExtraField) then
    StrDispose(FExtraField);
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetCompressionMethod: TAbZipCompressionMethod;
begin
  Result := TAbZipCompressionMethod(FCompressionMethod);
end;

{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDataDescriptor: boolean;
begin
  Result := (CompressionMethod = cmDeflated) and
    ((FGeneralPurposeBitFlag and AbHasDataDescriptorFlag) <> 0);
end;

{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetCompressionRatio: double;
var
  CompSize: longint;
begin
  {adjust for encrypted headers - ensures we never get negative compression
  ratios for stored, encrypted files - no guarantees about negative
  compression ratios in other cases}
  if isEncrypted then
    CompSize := CompressedSize - 12
  else
    CompSize := CompressedSize;
  if UncompressedSize > 0 then
    Result := 100.0 * (1 - ((1.0 * CompSize) / UncompressedSize))
  else
    Result := 0.0;
end;

{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDeflationOption: TAbZipDeflationOption;
begin
  if CompressionMethod = cmDeflated then
    if ((FGeneralPurposeBitFlag and $02) <> 0) then
      if ((FGeneralPurposeBitFlag and $04) <> 0) then
        Result := doSuperFast
      else
        Result := doMaximum
    else
    if ((FGeneralPurposeBitFlag and $04) <> 0) then
      Result := doFast
    else
      Result := doNormal
  else
    Result := doInvalid;
end;

{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDictionarySize: TAbZipDictionarySize;
begin
  if CompressionMethod = cmImploded then
    if ((FGeneralPurposeBitFlag and $02) <> 0) then
      Result := ds8K
    else
      Result := ds4K
  else
    Result := dsInvalid;
end;

{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetEncrypted: boolean;
begin
  {bit 0 of the GeneralPurposeBitFlag}
  Result := ((FGeneralPurposeBitFlag and AbFileIsEncryptedFlag) <> 0);
end;

{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetShannonFanoTreeCount: byte;
begin
  if CompressionMethod = cmImploded then
    if ((FGeneralPurposeBitFlag and $04) <> 0) then
      Result := 3
    else
      Result := 2
  else
    Result := 0;
end;

{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetValid: boolean;
begin
  Result := (FValidSignature = FSignature);
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipFileHeader.SetCompressionMethod(
  Value: TAbZipCompressionMethod);
begin
  FCompressionMethod := Ord(Value);
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipFileHeader.SetExtraField(Value: PChar);
begin
  if Assigned(FExtraField) then
    StrDispose(FExtraField);
  FExtraField := nil;
  FExtraFieldLength := StrLen(Value);

  if FExtraFieldLength > 0 then
  begin
    FExtraField := StrAlloc(succ(FExtraFieldLength));
    StrCopy(FExtraField, Value);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipFileHeader.SetFileName(Value: PChar);
begin
  if Assigned(FFileName) then
    StrDispose(FFileName);
  FFileName := nil;
  FFileNameLength := StrLen(Value);
  FFileName := StrAlloc(succ(FFileNameLength));
  StrCopy(FFileName, Value);
end;

{ -------------------------------------------------------------------------- }

{ TAbZipLocalFileHeader implementation ===================================== }
constructor TAbZipLocalFileHeader.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipLocalFileHeaderSignature;
end;

{ -------------------------------------------------------------------------- }
destructor TAbZipLocalFileHeader.Destroy;
begin
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipLocalFileHeader.LoadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    Read(FSignature, sizeof(FSignature));
    Read(FVersionNeededToExtract, sizeof(FVersionNeededToExtract));
    Read(FGeneralPurposeBitFlag, sizeof(FGeneralPurposeBitFlag));
    Read(FCompressionMethod, sizeof(FCompressionMethod));
    Read(FLastModFileTime, sizeof(FLastModFileTime));
    Read(FLastModFileDate, sizeof(FLastModFileDate));
    Read(FCRC32, sizeof(FCRC32));
    Read(FCompressedSize, sizeof(FCompressedSize));
    Read(FUncompressedSize, sizeof(FUncompressedSize));
    Read(FFileNameLength, sizeof(FFileNameLength));
    Read(FExtraFieldLength, sizeof(FExtraFieldLength));

    FFileName := StrAlloc(succ(FFileNameLength));
    Read(FFileName^, FFileNameLength);
    FFileName[FFileNameLength] := #0;

    if FExtraFieldLength > 0 then
    begin
      FExtraField := StrAlloc(succ(FExtraFieldLength));
      Read(FExtraField^, FExtraFieldLength);
      FExtraField[FExtraFieldLength] := #0;
    end;
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipLocalFileHeader.SaveToStream(Stream: TStream);
begin
  with Stream do
  begin
    {write the valid signature from the constant}
    Write(FValidSignature, sizeof(FValidSignature));
    Write(FVersionNeededToExtract, sizeof(FVersionNeededToExtract));
    Write(FGeneralPurposeBitFlag, sizeof(FGeneralPurposeBitFlag));
    Write(FCompressionMethod, sizeof(FCompressionMethod));
    Write(FLastModFileTime, sizeof(FLastModFileTime));
    Write(FLastModFileDate, sizeof(FLastModFileDate));
    Write(FCRC32, sizeof(FCRC32));
    Write(FCompressedSize, sizeof(FCompressedSize));
    Write(FUncompressedSize, sizeof(FUncompressedSize));
    Write(FFileNameLength, sizeof(FFileNameLength));
    Write(FExtraFieldLength, sizeof(FExtraFieldLength));

    Write(FFileName^, FFileNameLength);
    Write(FExtraField^, FExtraFieldLength);
  end;
end;

{ -------------------------------------------------------------------------- }

{ TAbZipDirectoryFileHeader implementation ================================= }
constructor TAbZipDirectoryFileHeader.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipCentralDirectoryFileHeaderSignature;
  FileComment := nil;
end;

{ -------------------------------------------------------------------------- }
destructor TAbZipDirectoryFileHeader.Destroy;
begin
  if Assigned(FFileComment) then
    StrDispose(FileComment);
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileHeader.LoadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    Read(FSignature, sizeof(FSignature));
    Read(FVersionMadeBy, sizeof(FVersionMadeBy));
    Read(FVersionNeededToExtract, sizeof(FVersionNeededToExtract));
    Read(FGeneralPurposeBitFlag, sizeof(FGeneralPurposeBitFlag));
    Read(FCompressionMethod, sizeof(FCompressionMethod));
    Read(FLastModFileTime, sizeof(FLastModFileTime));
    Read(FLastModFileDate, sizeof(FLastModFileDate));
    Read(FCRC32, sizeof(FCRC32));
    Read(FCompressedSize, sizeof(FCompressedSize));
    Read(FUncompressedSize, sizeof(FUncompressedSize));
    Read(FFileNameLength, sizeof(FFileNameLength));
    Read(FExtraFieldLength, sizeof(FExtraFieldLength));
    Read(FFileCommentLength, sizeof(FFileCommentLength));
    Read(FDiskNumberStart, sizeof(FDiskNumberStart));
    Read(FInternalFileAttributes, sizeof(FInternalFileAttributes));
    Read(FExternalFileAttributes, sizeof(FExternalFileAttributes));
    Read(FRelativeOffset, sizeof(FRelativeOffset));

    FFileName := StrAlloc(succ(FFileNameLength));
    Read(FFileName^, FFileNameLength);
    FFileName[FFileNameLength] := #0;

    if FExtraFieldLength > 0 then
    begin
      FExtraField := StrAlloc(succ(FExtraFieldLength));
      Read(FExtraField^, FExtraFieldLength);
      FExtraField[FExtraFieldLength] := #0;
    end;

    if FFileCommentLength > 0 then
    begin
      FFileComment := StrAlloc(succ(FFileCommentLength));
      Read(FFileComment^, FFileCommentLength);
      FFileComment[FFileCommentLength] := #0;
    end;
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileHeader.SaveToStream(Stream: TStream);
begin
  with Stream do
  begin
    {write the valid signature from the constant}
    Write(FValidSignature, sizeof(FValidSignature));
    Write(FVersionMadeBy, sizeof(FVersionMadeBy));
    Write(FVersionNeededToExtract, sizeof(FVersionNeededToExtract));
    Write(FGeneralPurposeBitFlag, sizeof(FGeneralPurposeBitFlag));
    Write(FCompressionMethod, sizeof(FCompressionMethod));
    Write(FLastModFileTime, sizeof(FLastModFileTime));
    Write(FLastModFileDate, sizeof(FLastModFileDate));
    Write(FCRC32, sizeof(FCRC32));
    Write(FCompressedSize, sizeof(FCompressedSize));
    Write(FUncompressedSize, sizeof(FUncompressedSize));
    Write(FFileNameLength, sizeof(FFileNameLength));
    Write(FExtraFieldLength, sizeof(FExtraFieldLength));
    Write(FFileCommentLength, sizeof(FFileCommentLength));
    Write(FDiskNumberStart, sizeof(FDiskNumberStart));
    Write(FInternalFileAttributes, sizeof(FInternalFileAttributes));
    Write(FExternalFileAttributes, sizeof(FExternalFileAttributes));
    Write(FRelativeOffset, sizeof(FRelativeOffset));
    Write(FFileName^, FFileNameLength);
    Write(FExtraField^, FExtraFieldLength);
    Write(FFileComment^, FFileCommentLength);
  end;
end;

{ -------------------------------------------------------------------------- }

{ TAbZipDirectoryFileFooter implementation ================================= }
constructor TAbZipDirectoryFileFooter.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipEndCentralDirectorySignature;
  FZipfileComment := nil;
end;

{ -------------------------------------------------------------------------- }
destructor TAbZipDirectoryFileFooter.Destroy;
begin
  if Assigned(FZipfileComment) then
    StrDispose(FZipfileComment);
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
function TAbZipDirectoryFileFooter.GetValid: boolean;
begin
  Result := (FSignature = FValidSignature);
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.LoadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    Read(FSignature, sizeof(FSignature));
    Read(FDiskNumber, sizeof(FDiskNumber));
    Read(FStartDiskNumber, sizeof(FStartDiskNumber));
    Read(FEntriesOnDisk, sizeof(FEntriesOnDisk));
    Read(FTotalEntries, sizeof(FTotalEntries));
    Read(FDirectorySize, sizeof(FDirectorySize));
    Read(FDirectoryOffset, sizeof(FDirectoryOffset));
    Read(FZipfileCommentLength, sizeof(FZipfileCommentLength));

    if FZipfileCommentLength > 0 then
    begin
      FZipfileComment := StrAlloc(succ(FZipfileCommentLength));
      Read(FZipfileComment^, FZipfileCommentLength);
      FZipfileComment[FZipfileCommentLength] := #0;
    end;
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.SaveToStream(Stream: TStream);
begin
  with Stream do
  begin
    Write(FValidSignature, sizeof(FValidSignature));
    Write(FDiskNumber, sizeof(FDiskNumber));
    Write(FStartDiskNumber, sizeof(FStartDiskNumber));
    Write(FEntriesOnDisk, sizeof(FEntriesOnDisk));
    Write(FTotalEntries, sizeof(FTotalEntries));
    Write(FDirectorySize, sizeof(FDirectorySize));
    Write(FDirectoryOffset, sizeof(FDirectoryOffset));
    Write(FZipfileCommentLength, sizeof(FZipfileCommentLength));
    Write(FZipfileComment^, FZipfileCommentLength);
  end;
end;

{ -------------------------------------------------------------------------- }

{ TAbZipItem implementation ================================================ }
constructor TAbZipItem.Create;
begin
  inherited Create;
  FItemInfo := TAbZipDirectoryFileHeader.Create;
  FDecoder  := nil;
end;

{ -------------------------------------------------------------------------- }
destructor TAbZipItem.Destroy;
begin
  FItemInfo.Free;
  FItemInfo := nil;
  FDecoder.Free;
  FDecoder := nil;
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressedSize: longint;
begin
  Result := FItemInfo.CompressedSize;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressionMethod: TAbZipCompressionMethod;
begin
  Result := FItemInfo.CompressionMethod;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressionRatio: double;
begin
  Result := FItemInfo.CompressionRatio;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCRC32: longint;
begin
  Result := FItemInfo.CRC32;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetDeflationOption: TAbZipDeflationOption;
begin
  Result := FItemInfo.DeflationOption;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetDictionarySize: TAbZipDictionarySize;
begin
  Result := FItemInfo.DictionarySize;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetGeneralPurposeBitFlag: word;
begin
  Result := FItemInfo.GeneralPurposeBitFlag;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetDiskNumberStart: word;
begin
  Result := FItemInfo.DiskNumberStart;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetExternalFileAttributes: longint;
begin
  Result := FItemInfo.ExternalFileAttributes;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetExtraField: string;
begin
  if Assigned(FItemInfo) and (FItemInfo.ExtraField <> nil) then
    Result := StrPas(FItemInfo.ExtraField)
  else
    Result := '';
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetFileComment: string;
begin
  if Assigned(FItemInfo) and (FItemInfo.FileComment <> nil) then
    Result := StrPas(FItemInfo.FileComment)
  else
    Result := '';
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetFileName: string;
var
  Buff: array [0..MAX_PATH] of char;
begin
  if Assigned(FItemInfo) and (FItemInfo.FileName <> nil) then
  begin
    StrCopy(Buff, FItemInfo.FileName);
    {!!.03 - Added }
{$IFDEF Linux}
 { do nothing to Buff }
{$ELSE}
    if (Hi(VersionMadeBy) = 0) and AreFileApisANSI then
    begin
      OEMToAnsi(Buff, Buff);
    end;
{$ENDIF}
    {!!.03 - End Added }
    Result := StrPas(Buff);
  end else
    Result := '';
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetInternalFileAttributes: word;
begin
  Result := FItemInfo.InternalFileAttributes;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetIsEncrypted: boolean;
begin
  Result := FItemInfo.IsEncrypted;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetLastModFileDate: word;
begin
  Result := FItemInfo.LastModFileDate;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetLastModFileTime: word;
begin
  Result := FItemInfo.LastModFileTime;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetRelativeOffset: longint;
begin
  Result := FItemInfo.RelativeOffset;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetShannonFanoTreeCount: byte;
begin
  Result := FItemInfo.ShannonFanoTreeCount;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetUncompressedSize: longint;
begin
  Result := FItemInfo.UncompressedSize;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetVersionMadeBy: word;
begin
  Result := FItemInfo.VersionMadeBy;
end;

{ -------------------------------------------------------------------------- }
function TAbZipItem.GetVersionNeededToExtract: word;
begin
  Result := FItemInfo.VersionNeededToExtract;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.LoadFromStream(Stream: TStream);
begin
  FItemInfo.LoadFromStream(Stream);
  if FItemInfo.FileName <> nil then
    FFileName := StrPas(FItemInfo.FileName)
  else
    FFileName := '';
  LastModFileTime := FItemInfo.LastModFileTime;
  LastModFileDate := FItemInfo.LastModFileDate;
  DiskFileName := FileName;
  AbUnfixName(FDiskFileName);
  Action := aaNone;
  Tagged := False;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveLFHToStream(Stream: TStream);
var
  LFH:  TAbZipLocalFileHeader;
  Temp: PChar;
begin
  LFH := TAbZipLocalFileHeader.Create;
  try
    LFH.VersionNeededToExtract := VersionNeededToExtract;
    LFH.GeneralPurposeBitFlag := GeneralPurposeBitFlag;
    LFH.CompressionMethod := CompressionMethod;
    LFH.LastModFileTime := LastModFileTime;
    LFH.LastModFileDate := LastModFileDate;
    LFH.CRC32 := CRC32;
    LFH.CompressedSize := CompressedSize;
    LFH.UncompressedSize := UncompressedSize;
    if Length(FileName) > 0 then
    begin
      Temp := StrAlloc(succ(Length(FileName)));
      try
        StrPCopy(Temp, FileName);
        LFH.FileName := Temp;
      finally
        StrDispose(Temp);
      end;
    end;
    if Length(ExtraField) > 0 then
    begin
      Temp := StrAlloc(succ(Length(ExtraField)));
      try
        StrPCopy(Temp, ExtraField);
        LFH.ExtraField := Temp;
      finally
        StrDispose(Temp);
      end;
    end;
    LFH.SaveToStream(Stream);
  finally
    LFH.Free;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveCDHToStream(Stream: TStream);
{-Save a ZipCentralDirectorHeader entry to Stream}
begin
  FItemInfo.SaveToStream(Stream);
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveDDToStream(Stream: TStream);
var
  DD: TAbZipDataDescriptor;
begin
  DD := TAbZipDataDescriptor.Create;
  try
    DD.CRC32 := CRC32;
    DD.CompressedSize := CompressedSize;
    DD.UncompressedSize := UncompressedSize;
    DD.SaveToStream(Stream);
  finally
    DD.Free;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCompressedSize(const Value: longint);
begin
  FItemInfo.CompressedSize := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCompressionMethod(Value: TAbZipCompressionMethod);
begin
  FItemInfo.CompressionMethod := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCRC32(const Value: longint);
begin
  FItemInfo.CRC32 := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetDiskNumberStart(Value: word);
begin
  FItemInfo.DiskNumberStart := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetExternalFileAttributes(Value: longint);
begin
  FItemInfo.ExternalFileAttributes := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetExtraField(const Value: string);
begin
  FItemInfo.ExtraFieldLength := Length(Value);
  if Assigned(FItemInfo.FExtraField) then
    StrDispose(FItemInfo.FExtraField);
  FItemInfo.FExtraField := nil;
  if Length(Value) > 0 then
  begin
    FItemInfo.FExtraField := StrAlloc(succ(FItemInfo.FExtraFieldLength));
    StrPCopy(FItemInfo.FExtraField, Value);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetFileComment(const Value: string);
begin
  FItemInfo.FileCommentLength := Length(Value);
  if Assigned(FItemInfo.FFileComment) then
    StrDispose(FItemInfo.FFileComment);
  FItemInfo.FFileComment := nil;

  if Length(Value) > 0 then
  begin
    FItemInfo.FFileComment := StrAlloc(succ(FItemInfo.FFileCommentLength));
    StrPCopy(FItemInfo.FFileComment, Value);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetFileName(const Value: string);
begin
  FFileName := Value;
  FItemInfo.FileNameLength := Length(Value);
  if Assigned(FItemInfo.FFileName) then
    StrDispose(FItemInfo.FFileName);
  FItemInfo.FFileName := nil;
  if Length(Value) > 0 then
  begin
    FItemInfo.FFileName := StrAlloc(succ(FItemInfo.FFileNameLength));
    StrPCopy(FItemInfo.FFileName, Value);
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetGeneralPurposeBitFlag(Value: word);
begin
  FItemInfo.GeneralPurposeBitFlag := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetInternalFileAttributes(Value: word);
begin
  FItemInfo.InternalFileAttributes := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetLastModFileDate(const Value: word);
begin
  FItemInfo.LastModFileDate := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetLastModFileTime(const Value: word);
begin
  FItemInfo.LastModFileTime := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetRelativeOffset(Value: longint);
begin
  FItemInfo.RelativeOffset := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetUncompressedSize(const Value: longint);
begin
  inherited SetUncompressedSize(Value);
  FItemInfo.UncompressedSize := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetVersionMadeBy(Value: word);
begin
  FItemInfo.VersionMadeBy := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetVersionNeededToExtract(Value: word);
begin
  FItemInfo.VersionNeededToExtract := Value;
end;

{ -------------------------------------------------------------------------- }


{ TAbZipArchive implementation ============================================= }
constructor TAbZipArchive.Create(const FileName: string; Mode: word);
begin
  inherited Create(FileName, Mode);
  FCompressionMethodToUse := smBestMethod;
  FInfo := TAbZipDirectoryFileFooter.Create;
  StoreOptions := StoreOptions + [soStripDrive];
  FDeflationOption := doNormal;
  FPasswordRetries := AbDefPasswordRetries;
  FTempDir := '';
  SpanningThreshold := AbDefZipSpanningThreshold;
  FCurrentDisk := word(-1); {!!}
end;

{ -------------------------------------------------------------------------- }
constructor TAbZipArchive.CreateFromStream(aStream: TStream;
  const ArchiveName: string);
begin
  inherited CreateFromStream(aStream, ArchiveName);
  FInfo := TAbZipDirectoryFileFooter.Create;
  FPasswordRetries := AbDefPasswordRetries;
end;

{ -------------------------------------------------------------------------- }
destructor TAbZipArchive.Destroy;
begin
  FInfo.Free;
  FInfo := nil;
  inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
function TAbZipArchive.CreateItem(const FileSpec: string): TAbArchiveItem;
var
  Buff: array [0..MAX_PATH] of char;
  I: integer;
begin
  Result := TAbZipItem.Create;
  with TAbZipItem(Result) do
  begin
    CompressionMethod := cmDeflated;
    GeneralPurposeBitFlag := 0;
    CompressedSize := 0;
    CRC32 := 0;
    ExtraField := '';
    StrPCopy(Buff, ExpandFileName(FileSpec));
    {!!.03 - Added }
{$IFDEF Linux}
 { do nothing to Buff }
{$ELSE}
    if AreFileApisANSI then
    begin
      I := 0;
      while Ord(Buff[I]) <> 0 do
      begin
        if Ord(Buff[I]) >= 128 then
        begin
          VersionMadeBy := Lo(VersionMadeBy) or $0B00;
          Break;
        end;
        Inc(I);
      end;
    end;
{$ENDIF}
    {!!.03 - End Added }
    // Changed due to comment made by Jeff Rather.
    // No bug report, but after review I can see how
    // DiskFileName and FileName inconsitencies this case may
    // cause a problem.
    // Commented out Jeff Rather code, as it was causing problem
    //    StrPCopy(Buff, FixName(FileSpec));
    //    FileName := StrPas(Buff);
    //    DiskFileName := FileName;

    DiskFileName := StrPas(Buff);
    StrPCopy(Buff, FixName(FileSpec));
    FileName := StrPas(Buff);
    RelativeOffset := 0;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoExtractHelper(Index: integer; const NewName: string);
begin
  if Assigned(FExtractHelper) then
    FExtractHelper(Self, ItemList[Index], NewName)
  else
    raise EAbZipNoExtraction.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoExtractToStreamHelper(Index: integer;
  aStream: TStream);
begin
  if Assigned(FExtractToStreamHelper) then
    FExtractToStreamHelper(Self, ItemList[Index], aStream)
  else
    raise EAbZipNoExtraction.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoTestHelper(Index: integer);
begin
  if Assigned(FTestHelper) then
    FTestHelper(Self, ItemList[Index])
  else
    raise EAbZipNoExtraction.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoInsertHelper(Index: integer; OutStream: TStream);
begin
  if Assigned(FInsertHelper) then
    FInsertHelper(Self, ItemList[Index], OutStream)
  else
    raise EAbZipNoInsertion.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoInsertFromStreamHelper(Index: integer; OutStream: TStream);
begin
  if Assigned(FInsertFromStreamHelper) then
    FInsertFromStreamHelper(Self, ItemList[Index], OutStream, InStream)
  else
    raise EAbZipNoInsertion.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestLastDisk(var Abort: boolean);
var
  pMessage: string;
  pCaption: string;
begin
  Abort := False;
  if Assigned(FOnRequestLastDisk) then
    FOnRequestLastDisk(Self, Abort)
  else
  begin
    pMessage := AbStrRes(AbLastDiskRequest);
    pCaption := AbStrRes(AbDiskRequest);
{$IFDEF MSWINDOWS}
    Abort := Windows.MessageBox(0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL) = idCancel;
{$ENDIF}
{$IFDEF LINUX}
{$IFDEF NoQt}
    WriteLn(pMessage);
{$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
{$ENDIF}
{$ENDIF}
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestNthDisk(DiskNumber: byte;
  var Abort: boolean);
var
  pMessage: string;
  pCaption: string;
  FMessage: string;
begin
  Abort := False;
  if Assigned(FOnRequestNthDisk) then
    FOnRequestNthDisk(Self, DiskNumber, Abort)
  else
  begin
    pMessage := AbStrRes(AbDiskNumRequest);
    FMessage := Format(pMessage, [DiskNumber]);
    pMessage := FMessage;
    pCaption := AbStrRes(AbDiskRequest);
{$IFDEF MSWINDOWS}
    Abort := Windows.MessageBox(0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL) = idCancel;
{$ENDIF}
{$IFDEF LINUX}
{$IFDEF NoQt }
    WriteLn(pMessage);
{$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
{$ENDIF }
{$ENDIF}
  end;

  //  if not Abort and (FStream is TAbSpanStream) then                       {!!.01}
  //    TAbSpanStream(FStream).SpanNumber := DiskNumber;                     {!!.01}

end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestBlankDisk(var Abort: boolean);
var
  pMessage: string;
  pCaption: string;
begin
  Abort := False;
  FSpanned := True;

  if Assigned(FOnRequestBlankDisk) then
    FOnRequestBlankDisk(Self, Abort)
  else
  begin
    pMessage := AbStrRes(AbBlankDisk);
    pCaption := AbStrRes(AbDiskRequest);
{$IFDEF MSWINDOWS}
    Abort := Windows.MessageBox(0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL) = idCancel;
{$ENDIF}
{$IFDEF LINUX}
{$IFDEF NoQt}
    WriteLn(pMessage);
{$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
{$ENDIF}
{$ENDIF}
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestNthImage(ImageNumber: integer;
  var Stream: TStream; var Abort: boolean);
var
  ImageName: string;
  i: integer;
  Found: boolean;
  MediaType: TAbMediaType;
begin
  Abort := False;
  ImageName := FArchiveName;

  {--spanned disk set--}
  MediaType := mtLocal;
  if FDriveIsRemovable then
  begin
{$IFDEF LINUX}
    raise EAbException.Create('Floppy Spanning not supported on Linux'); {!!.01}
{$ENDIF}
    MediaType := mtRemoveable;

    if (ImageNumber > AbLastDisk) then
      DoRequestNthDisk(Succ(ImageNumber), Abort);
    if Abort then
      raise EAbUserAbort.Create;

    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);

    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;

    {!!.04 - changed}
    //    if FindCentralDirectoryTail(Stream) = -1 {not found} then
    if (CurrentDisk = word(-1)) and (FindCentralDirectoryTail(Stream) = -1)
    {not found} then
      {!!.04 - changed end}
      DoRequestLastDisk(Abort);
    if Abort then
      raise EAbUserAbort.Create;

    Exit;
  end;

  {--spanned image set--}
  { first check if the current image contains the CDT }{!!.03}
  // Removed {!!.05 if ImageNumber is looking for another disk it will never be opened.
  //  if FindCentralDirectoryTail(Stream) > -1 {not found} then begin        {!!.03}
  //    Exit;                                                                {!!.03}
  //  end;                                                                   {!!.03}

  {if OnRequestImage assigned, then fire event}
  if Assigned(FOnRequestImage) then
  begin

    FOnRequestImage(Self, ImageNumber, ImageName, Abort);
    if Abort then
      raise EAbUserAbort.Create;
    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
    //    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
    Exit;
  end;

  {if not last image requested, then simply auto-generate image name}
  if (ImageNumber > AbLastImage) then
  begin
    //    if (ImageNumber = 0) then
    //      ImageName := FArchiveName
    //    else
    AbIncFilename(ImageName, ImageNumber);
    if not FileExists(ImageName) then
      raise EAbFileNotFound.Create;
    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;   {!!.05 [ 714944 ]}
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
    //    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
    Exit;
  end;




  {search for last image, assuming images were auto-generated}
  FAutoGen := True;                                                  {!!.02}
  for i := 1 to 99 do
  begin
    AbIncFilename(ImageName, i);
    if not FileExists(ImageName) then
      raise EAbFileNotFound.Create;
    // 885670 (Moved Stream to avoid file corruption)      
    Stream.Free;                                                     {!!.04}
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
    //    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
    Found  := (FindCentralDirectoryTail(Stream) > -1);
    if Found then
      Break
    else
      Stream.Free;
  end;

  CurrentDisk := ImageNumber;                                            {!!.01}
  if not Found then
    raise EAbFileNotFound.Create;
end;

 { -------------------------------------------------------------------------- }
 {!!.01 -- Added}
procedure TAbZipArchive.DoRequestImage(Mode: TAbSpanMode; ImageNumber: integer;
  var ImageName: string; var Abort: boolean);
var
  pMessage: string;
  pCaption: string;
begin
  if Assigned(FOnRequestImage) then
    FOnRequestImage(self, ImageNumber, ImageName, Abort)
  else if FAutoGen then
  begin
    AbIncFilename(ImageName, ImageNumber);                            {!!.02}
    // if we are reading and the file does not exist
    // then we must be at last file in archive, change to .ZIP extention
    // as the last file is there.
    if (Mode = smReading) and not FileExists(ImageName) then         {!!.05}
      ImageName := ChangeFileExt(ImageName, '.ZIP');
  end
  else if Mode = smReading then
  begin

    pMessage := Format(AbStrRes(AbImageNumRequest), [ImageNumber]);
    pCaption := AbStrRes(AbImageRequest);
{$IFDEF MSWINDOWS}
    {!!.04}
    //    Abort := not InputQuery(pCaption, pMessage, ImageName);
    Abort := Windows.MessageBox(0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL) = idCancel;
    {!!.04}

{$ENDIF}
{$IFDEF LINUX}
{$IFDEF NoQt}
    WriteLn(pMessage);
{$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
{$ENDIF}
{$ENDIF}
  end;
end;

 { -------------------------------------------------------------------------- }
 {!!.01 -- End Added}
procedure TAbZipArchive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: integer; var ImageName: string; var Abort: boolean);
var
  SpanStrm: TAbSpanStream;
begin
  SpanStrm := Sender as TAbSpanStream;
  if SpanStrm.SpanMode = smWriting then
  begin
    if SpanStrm.MediaType = mtRemoveable then
    begin
      DoRequestBlankDisk(Abort);
      AbWriteVolumeLabel(Format('PKBACK# %3.3d',                         {!!.01}
        [Pred(ImageNumber)]), AbDrive(FArchiveName));                    {!!.01}
    end
    else
    begin
      DoRequestImage(SpanStrm.SpanMode, ImageNumber, ImageName, Abort);   {!!.01}
    end;
  end
  else
  begin  { SpanMode = smReading }
    if SpanStrm.MediaType = mtRemoveable then
    begin
      DoRequestNthDisk(ImageNumber, Abort);
    end
    else
    begin
      DoRequestImage(SpanStrm.SpanMode, ImageNumber, ImageName, Abort);  {!!.01}
    end;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestNextImage(ImageNumber: integer;
  var Stream: TStream; var Abort: boolean);
var
  ImageName: string;
  MediaType: TAbMediaType;
begin
  Abort := False;
  Stream.Free;
  Stream := nil;
  ImageName := FArchiveName;

  {if drive is removable then request a blank disk}
  if FDriveIsRemovable then
  begin
    DoRequestBlankDisk(Abort);
    MediaType := mtRemoveable;

    {otherwise we need the next image file name}
  end else
  begin
    MediaType := mtLocal;
    if Assigned(FOnRequestImage) then
      FOnRequestImage(Self, ImageNumber, ImageName, Abort)
    else {auto-generate the name}
      AbIncFilename(ImageName, ImageNumber);
  end;
  if Abort then
    raise EAbUserAbort.Create
  else
  begin
    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmCreate, MediaType, SpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
    //    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.ExtractItemAt(Index: integer; const NewName: string);
begin
  DoExtractHelper(Index, NewName);
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.ExtractItemToStreamAt(Index: integer;
  aStream: TStream);
begin
  DoExtractToStreamHelper(Index, aStream);
end;

{ -------------------------------------------------------------------------- }
function TAbZipArchive.FindCDTail: longint;
begin
  Result := FindCentralDirectoryTail(FStream);
end;

{ -------------------------------------------------------------------------- }
function TAbZipArchive.FixName(const Value: string): string;
  {-changes backslashes to forward slashes}
var
  i: smallint;
  lValue: string;
begin
  lValue := Value; {!!.05 [ 783583 ]}
  {$IFDEF MSWINDOWS}
  if DOSMode then
  begin
    {Add the base directory to the filename before converting }
    {the file spec to the short filespec format. }
    if BaseDirectory <> '' then
    begin
      {Does the filename contain a drive or a leading backslash? }
      if not ((Pos(':', lValue) = 2) or (Pos(AbPathDelim, lValue) = 1)) then
        {If not, add the BaseDirectory to the filename.}
        lValue := AbAddBackSlash(BaseDirectory) + lValue;                {!!.04}
    end;
    lValue := AbGetShortFileSpec(lValue);
  end;
  {$ENDIF MSWINDOWS}

  {Zip files Always strip the drive path}
  StoreOptions := StoreOptions + [soStripDrive];

  {strip drive stuff}
  if soStripDrive in StoreOptions then
    AbStripDrive(lValue);

  {check for a leading backslash}
  if (Length(lValue) > 1) and (lValue[1] = AbPathDelim) then {!!.05  - [ 799438 ]}
    System.Delete(lValue, 1, 1);

  if soStripPath in StoreOptions then
  begin
    lValue := ExtractFileName(lValue);
  end;

  if soRemoveDots in StoreOptions then
    AbStripDots(lValue);

  for i := 1 to Length(lValue) do
    if lValue[i] = '\' then
      lValue[i] := '/';
  Result := lValue;
end;

{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetItem(Index: integer): TAbZipItem;
begin
  Result := TAbZipItem(FItemList.Items[Index]);
end;

{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetZipFileComment: string;
begin
  Result := StrPas(FInfo.ZipFileComment);
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.LoadArchive;
var
  Abort: boolean;
  TailPosition: longint;
  Item: TAbZipItem;
  i: integer;
  Progress: byte;
  FileSignature: DWord;                                             {!!.02}
  IsZip: boolean;
  Lowest: longint;
begin
  Lowest := MaxLongint;
  Abort  := False;
  FAutoGen := False;                                                 {!!.02}
  if FStream.Size = 0 then
    Exit;

  {Get signature info}
  FStream.Position := 0;
  FStream.Read(FileSignature, sizeof(FileSignature));

  IsZip := (FileSignature and $0000FFFF) = Ab_GeneralZipSignature;   {!!.02}

{$IFDEF MSWINDOWS}
  // [ 719083 ] Windows exe signature check
  IsExecutable := (FileSignature and $0000FFFF) = Ab_WindowsExeSignature;
{$ENDIF}
{$IFDEF LINUX}
  if FileSignature = Ab_LinuxExeSigWord1 then begin
    FStream.Read( FileSignature, sizeof( FileSignature ) );
    IsExecutable := FileSignature = Ab_LinuxExeSigWord2;
  end;
{$ENDIF}


  {Check for spanning}
  if (FStream is TAbSpanStream) then
    FDriveIsRemovable := AbDriveIsRemovable(ArchiveName);


  { try to locate central directory tail }
  if (FileSignature = DWord(Ab_ZipSpannedSetSignature)) or           {!!.02}
    (FileSignature = DWord(Ab_ZipPossiblySpannedSignature)) then    {!!.02}
  begin
    if FDriveIsRemovable then        {!!.05}
      TailPosition := -1              {!!.02}
    else
      TailPosition := FindCDTail;  {!!.05}
  end
  else                                                               {!!.02}
  begin                                                              {!!.02}
    TailPosition := FindCDTail;
    // 885670 (Second Part Better Error Message)
    if TailPosition = -1 then
      raise EAbZipInvalid.Create;
  end;

  if (TailPosition = -1) then
  begin { not found so need different image }
    if IsZip then
    begin
      while (not Abort) and (TailPosition = -1) do
      begin
        DoRequestNthImage(AbLastDisk, FStream, Abort);
        TailPosition := FindCDTail;
      end;
      if Abort then
        Exit
      else
        FSpanned := True;
    end else
    begin
      FStatus := asInvalid;
      raise EAbZipInvalid.Create;
    end;
  end;

  { load the ZipDirectoryFileFooter }
  FInfo.LoadFromStream(FStream);
  CurrentDisk := FInfo.DiskNumber;
  { set spanning flag if current disk is not the first one }
  if (FInfo.DiskNumber > 0) then
    FSpanned := True;
  //Possible Bug, Remove Drives could be split instead of spanned.    
  if FSpanned and (not FDriveIsRemovable) then
    FAutoGen := True;

  { build Items list from central directory records }
  i := 0;
  FStream.Seek(FInfo.DirectoryOffset, soFromBeginning);
  {  while not((FStream.Position = TailPosition) and }{!!.01}
  {    (CurrentDisk = FInfo.DiskNumber)) do begin }{!!.01}
  while (FStream.Position < TailPosition) or                             {!!.01}
    (CurrentDisk <> FInfo.DiskNumber) do
  begin                           {!!.01}
    if Spanned then
    begin
      {see if we've got to switch disks}
      if (CurrentDisk < FInfo.DiskNumber) and (FInfo.EntriesOnDisk = i) or
        (FStream.Size = FStream.Position) then
      begin
        CurrentDisk := CurrentDisk + 1;
        DoRequestNthImage(CurrentDisk, FStream, Abort);
        if Abort then
          Exit;
      end;
    end;

    { create new Item }
    Item := TAbZipItem.Create;
    try
      Item.LoadFromStream(FStream);
    except {!!.05 [ 800130 ] ZIP - Potential Memory Leak }
      Item.Free;
      raise;
    end;
    if IsExecutable then
      if (Item.RelativeOffset < Lowest) then
        Lowest := Item.RelativeOffset;
    FItemList.Add(Item);
    ItemList[pred(Count)].Action := aaNone;
    Inc(i);
    Progress := (i * 100) div FInfo.TotalEntries;
    DoArchiveProgress(Progress, Abort);
    if Abort then
    begin
      FStatus := asInvalid;
      raise EAbUserAbort.Create;
    end;
  end;

  DoArchiveProgress(100, Abort);
  if IsExecutable then
    FStubSize := Lowest;
  FIsDirty := False;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.PutItem(Index: integer; Value: TAbZipItem);
begin
  FItemList.Items[Index] := Value;
end;

{ -------------------------------------------------------------------------- }

type
  TAbItemDataRec = record
    StreamOffset: longint;
  end;

{ -------------------------------------------------------------------------- }
const
  AB_SIZE_LOCAL_HEADER = 30;
  AB_SIZE_DATA_DESC = 16;
  AB_SIZE_CD_HEADER = 46;
  AB_SIZE_CD_TAIL = 22;

{ -------------------------------------------------------------------------- }

procedure TAbZipArchive.SaveArchive;
{builds a new archive and copies it to FStream}
var
  Abort: boolean;
  {BlockSize         : Longint;}{!!.01}
  {TotalBytesWritten : Longint;}{!!.01}
  CDHStream: TMemoryStream;
  HasDataDescriptor: boolean;
  i: integer;
  LFH: TAbZipLocalFileHeader;
  NewStream: TAbVirtualMemoryStream;
  WorkingStream: TAbVirtualMemoryStream;
  CurrItem: TAbZipItem;
  SCurrentImage: word;
  SCurrentOffset: longint;
  SSpanningThreshold: longint;
  CanSpan: boolean;
  MediaType: TAbMediaType;                                     {!!.01}
  BlockSize: longint;
  ByteBuf: byte;
begin
  {shouldn't be trying to overwrite an existing spanned archive}
  if Spanned then
  begin
    for i := 0 to Pred(Count) do
      if ItemList[i].Action <> aaFailed then
        ItemList[i].Action := aaNone;
    FIsDirty := False;
    raise EAbZipSpanOverwrite.Create;
  end;

  {!!.01 -- Modified to take better advantage of TAbSpanStream features }
  if FStream is TAbSpanStream then
  begin
    if TAbSpanStream(FStream).SpanMode = smWriting then
    begin
      MediaType := TAbSpanStream(FStream).MediaType;
      FStream.Free;
      FStream := TAbSpanstream.Create(ArchiveName, fmOpenRead or
        fmShareDenyWrite, MediaType, FSpanningThreshold);
      TAbSpanStream(FStream).OnRequestImage := DoSpanningMediaRequest;
      TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress; {!!.04}
    end;
  end;
  {!!.01 -- End Modified (see more below)}

  {can span if new archive and drive is removable, or
   can span if SpanningThreshold > 0 and drive is fixed}
  FDriveIsRemovable  := AbDriveIsRemovable(ArchiveName);
  SSpanningThreshold := AbGetDriveFreeSpace(ArchiveName);
  if FDriveIsRemovable then
    CanSpan := (FStream.Size = 0)
  else
  begin
    CanSpan := (SpanningThreshold > 0);
    if CanSpan then
      SSpanningThreshold := SpanningThreshold;
  end;
  if (SSpanningThreshold <= 0) then
    SSpanningThreshold := MaxLongint;

  {init new zip archive stream}
  NewStream := TAbVirtualMemoryStream.Create;
  try {NewStream}
    NewStream.SwapFileDirectory := ExtractFilePath(AbGetTempFile(FTempDir, False));

    {copy the executable stub over to the output}
    if IsExecutable then
      NewStream.CopyFrom(FStream, StubSize)
    else if CanSpan then
      NewStream.Write(Ab_ZipPossiblySpannedSignature,
        SizeOf(Ab_ZipPossiblySpannedSignature));

    {init central directory stream}
    CDHStream := TMemoryStream.Create;
    try {CDHStream}
      FInfo.EntriesOnDisk := 0;
      FInfo.TotalEntries := 0;
      SCurrentImage  := 0;
      SCurrentOffset := NewStream.Position;

      FStream.Position := 0;                                             {!!.01}

      {build new zip archive from existing archive}
      for i := 0 to pred(Count) do
      begin
        CurrItem := (ItemList[i] as TAbZipItem);
        FCurrentItem := ItemList[i];

        case CurrItem.Action of
          aaNone, aaMove, aaCopy, aaRename:
          begin
            // !!.07  Notify Item Progress
            DoArchiveItemProgress(CurrItem, 0, Abort);
            // end !! 0.7

            {just copy the file to new stream, and add CDH record}
            FStream.Position := CurrItem.RelativeOffset;
            CurrItem.DiskNumberStart := SCurrentImage;
            CurrItem.RelativeOffset := SCurrentOffset;
            {toss old local file header}
            LFH := TAbZipLocalFileHeader.Create;
            try {LFH}
              LFH.LoadFromStream(FStream);
            finally {LFH}
              LFH.Free;
            end; {LFH}
            {write out new local file header and append compressed data}

            CurrItem.SaveLFHToStream(NewStream);
            if (CurrItem.CompressedSize > 0) then
              NewStream.CopyFrom(FStream, CurrItem.CompressedSize);
            FInfo.EntriesOnDisk := FInfo.EntriesOnDisk + 1;
            FInfo.TotalEntries  := FInfo.TotalEntries + 1;
            CurrItem.SaveCDHToStream(CDHStream);

            // !!.07  Notify Item Progress
            DoArchiveItemProgress(CurrItem, 100, Abort);
            // end !! 0.7
          end;

          aaDelete:
          begin
            {doing nothing omits file from new stream}
            // !!.07  Notify Item Progress
            DoArchiveItemProgress(CurrItem, 100, Abort);
            // end !! 0.7
          end;

          aaAdd, aaFreshen, aaReplace, aaStreamAdd:
          begin
            // !!.07  Notify Item Progress
            DoArchiveItemProgress(CurrItem, 0, Abort);
            // end !! 0.7

            {compress the file, add it new stream, and add CDH record}
            CurrItem.DiskNumberStart := SCurrentImage;
            CurrItem.RelativeOffset := SCurrentOffset;
            WorkingStream := TAbVirtualMemoryStream.Create;
            try
              try {WorkingStream}

                WorkingStream.SwapFileDirectory := NewStream.SwapFileDirectory;
                if (CurrItem.Action = aaStreamAdd) then
                  DoInsertFromStreamHelper(i, WorkingStream)
                else
                  DoInsertHelper(i, WorkingStream);
                CurrItem.SaveLFHToStream(NewStream);
                NewStream.CopyFrom(WorkingStream, 0);
                if CurrItem.IsEncrypted then                               {!!.01}
                  CurrItem.SaveDDToStream(NewStream);                      {!!.01}

                FInfo.EntriesOnDisk := FInfo.EntriesOnDisk + 1;
                FInfo.TotalEntries  := FInfo.TotalEntries + 1;
                CurrItem.SaveCDHToStream(CDHStream);
              except
                on E: Exception do
                begin
                { Exception was caused by a User Abort and Item Failure should not be called
                  Question:  Do we want an New Event when this occurs or should the
                  exception just be re-raised }
                  if (E is EAbUserAbort) then {!!.05 [ 783614 ]}
                    raise;
                  CurrItem.Action := aaDelete;
                  DoProcessItemFailure(CurrItem, ptAdd, ecFileOpenError, 0);
                end;
              end;
            finally
              WorkingStream.Free;
            end;
            // !!.07  Notify Item Progress
            DoArchiveItemProgress(CurrItem, 100, Abort);
            // end !! 0.7
          end;
        end; { case }

        {Now add the data descriptor record to new stream}
        HasDataDescriptor := (CurrItem.CompressionMethod = cmDeflated) and
          ((CurrItem.GeneralPurposeBitFlag and AbHasDataDescriptorFlag) <> 0);
        if (CurrItem.Action <> aaDelete) and HasDataDescriptor then
          CurrItem.SaveDDToStream(NewStream);
        DoArchiveProgress(AbPercentage(9 * succ(i), 10 * Count), Abort);
        if Abort then
          raise EabUserAbort.Create;
        if (SSpanningThreshold > 0) then
          SCurrentImage := NewStream.Position div SSpanningThreshold;
        SCurrentOffset := NewStream.Position - (SCurrentImage * SSpanningThreshold);
      end;

      { if Spanning and pad archive so directory starts on last disk }
      if SSpanningThreshold > 0 then
      begin
        // is the disk space left on the disk enough, if not pad till it is
        BlockSize := (SSpanningThreshold - SCurrentOffset);
        if (BlockSize > 0) and (BlockSize < CDHStream.Size + 512) then
        begin
          bytebuf := 0;
          for i := 0 to BlockSize do
            NewStream.Write(bytebuf, sizeof(bytebuf));
          SCurrentImage  := NewStream.Position div SSpanningThreshold;
          SCurrentOffset :=
            NewStream.Position - (SCurrentImage * SSpanningThreshold);
        end;
      end;

      {append the central directory}
      FInfo.StartDiskNumber := SCurrentImage;
      FInfo.DirectoryOffset := SCurrentOffset;
      CDHStream.Position := 0;
      NewStream.CopyFrom(CDHStream, CDHStream.Size);

      { we're not sure if the CDH may span disks }
      if (SpanningThreshold > 0) then
        SCurrentImage := NewStream.Position div SSpanningThreshold;

      {append the central directory footer}
      FInfo.DirectorySize := CDHStream.Size;
      FInfo.DiskNumber := SCurrentImage;
      FInfo.SaveToStream(NewStream);
    finally {CDHStream}
      CDHStream.Free;
    end; {CDHStream}

    {check for spanning}
    FSpanned := (SCurrentImage > 0) and                                  {!!.03}
      (NewStream.Size > SSpanningThreshold) and CanSpan;                 {!!.03}
    if Spanned then
    begin
      NewStream.Position := 0;
      NewStream.Write(Ab_ZipSpannedSetSignature, SizeOf(Ab_ZipSpannedSetSignature));
    end;

    {copy new stream to FStream}
    NewStream.Position := 0;
    if (FStream is TMemoryStream) then
      TMemoryStream(FStream).LoadFromStream(NewStream)
    else
    begin
      { need new stream to write }
      FStream.Free;

      {!!.01 -- Modified to take better advantage of TAbSpanStream features }
      if Spanned and AbDriveIsRemovable(FArchiveName) then
      begin         {!!.03}
        {reset image number }
        SCurrentImage := 0;
        AbWriteVolumeLabel(Format('PKBACK# %3.3d', [Succ(SCurrentImage)]),
          AbDrive(FArchiveName));
        FStream := TAbSpanStream.Create(FArchiveName, fmOpenWrite or
          fmShareDenyWrite, mtRemoveable, FSpanningThreshold);
        {!!! write spanning signature here?}
      end
      else
      if SpanningThreshold > 0 then
      begin
        //NOTE: It is possible that a archive written through this method will not be
        //split.   I.e. The SpanningThreshold > TotalArchiveSize
        //First File of a Split Archive is 'ARCHIVENAME.Z01' however if not split
        //it should be 'ARCHIVENAME.ZIP'
        //To handle this problem the first file is created with. '.ZIP'
        //Later on in the process it is renamed to '.Z01' if spanned'
        //The last file in a split archive has .ZIP extension
        //It will be named .Z## initially.  Then renamed to .ZIP
        //So Split archive will have two renames, with last and first file.
        FStream := TAbSpanStream.Create(FArchiveName,
          fmOpenWrite or fmShareDenyWrite, mtLocal,
          FSpanningThreshold);
      end
      else
      begin
        // !!.05 Test to use TFileStream if not Spanned
        //        FStream := TAbSpanStream.Create(FArchiveName,
        //          fmOpenWrite or fmShareDenyWrite, mtLocal,
        //            FSpanningThreshold);
        FStream := nil; {!!.05 avoid A/V on free if Create Fails}
        FStream := TFileStream.Create(FArchiveName, fmOpenReadWrite or
          fmShareDenyWrite);
      end;

      try
        if (FStream is TAbSpanStream) then
        begin
          TAbSpanStream(FStream).OnRequestImage :=
            DoSpanningMediaRequest;
          TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress; {!!.04}
          //        TAbSpanStream(FStream).SpanNumber := SCurrentImage;              {!!.01}
        end;

        FStream.Size := 0;
        { copy temporary archive to the stream }
        if FStream.Position <> 0 then
          FStream.Position := 0;
        if (FStream is TAbSpanStream) then
          TAbSpanStream(FStream).ArchiveTotalSize := NewStream.Size;     {!!.04}
        FStream.CopyFrom(NewStream, NewStream.Size);
      except
        on E: Exception do
        begin
          if E is EAbUserAbort                      {!!.05 [783614] } then
            raise
          else
            raise EAbBadStream.CreateInner(E);
        end;
      end;
      {!!.01 -- End Modified }
    end;

    {rename if split archive}
    if (SCurrentImage > 0) then
    begin
      //Other archive types we hold the stream for the last file.
      //However, with a split archive we need to free it so we can rename it.
      FStream.Free;
      //Rename .ZIP to Z01
      RenameFile(FArchiveName, ChangeFileExt(FArchiveName, '.Z01'));
      //Rename .Z## (where ## is last image) to .ZIP
      if SCurrentImage < 9 then
        RenameFile(ChangeFileExt(FArchiveName, '.z0' +
          IntToStr(SCurrentImage + 1)), FArchiveName)
      else
        RenameFile(ChangeFileExt(FArchiveName, '.z' + IntToStr(SCurrentImage + 1)),
          FArchiveName);
      // SCurrentImage > 0 only if FStream is a Spanned Archive.
      MediaType := (FStream as TAbSpanStream).MediaType;
      // Open the Split archive for reading to duplicate behavior of single file archives.
      FStream := TAbSpanstream.Create(ArchiveName, fmOpenRead or
        fmShareDenyWrite, MediaType, FSpanningThreshold);
    end;


    {update Items list}
    for i := pred(Count) downto 0 do
    begin
      if FItemList[i].Action = aaDelete then
        FItemList.Delete(i)
      else if FItemList[i].Action <> aaFailed then
        FItemList[i].Action := aaNone;
    end;

    DoArchiveSaveProgress(100, Abort);                               {!!.04}
    DoArchiveProgress(100, Abort);
  finally {NewStream}
    NewStream.Free;
    //   FStream.Free;
  end;

end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.SetZipFileComment(const Value: string);
begin
  FInfo.ZipFileCommentLength := Length(Value);
  if Assigned(FInfo.FZipFileComment) then
    StrDispose(FInfo.FZipFileComment);
  FInfo.FZipFileComment := nil;
  if Length(Value) > 0 then
  begin
    FInfo.FZipFileComment := StrAlloc(succ(FInfo.FZipFileCommentLength));
    StrPCopy(FInfo.FZipFileComment, Value);
    FIsDirty := True;
  end                                                                    {!!.02}
  else
  begin  { if Value = '' then clear the ZIP Comment }               {!!.02}
    FInfo.FZipFileCommentLength := 0;                                    {!!.02}
    FInfo.FZipFileComment := nil;                                        {!!.02}
    FIsDirty := True;                                                    {!!.02}
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.TestItemAt(Index: integer);
begin
  DoTestHelper(Index);
end;

{ -------------------------------------------------------------------------- }
procedure MakeSelfExtracting(StubStream, ZipStream,
  SelfExtractingStream: TStream);
  {-takes an executable stub, and a .zip format stream, and creates
   a SelfExtracting stream.  The stub should create a TAbZipArchive
   passing itself as the file, using a read-only open mode.  It should
   then perform operations as needed - like ExtractFiles( '*.*' ).
   This routine updates the RelativeOffset of each item in the archive}
var
  DirectoryStart: longint;
  FileSignature: word;
  StubSize: longint;
  TailPosition: longint;
  ZDFF: TAbZipDirectoryFileFooter;
  ZipItem: TAbZipItem;
  IsWinExe, IsLinuxExe: boolean;                                        {!!.01}
begin
  {check file type of stub stream}
  StubStream.Position := 0;
  StubStream.Read(FileSignature, SizeOf(FileSignature));

  {!!.01 -- re-written executable Type Detection to allow use of non-native stubs }
  IsLinuxExe := False;
  IsWinExe := FileSignature = Ab_WindowsExeSignature;
  if not IsWinExe then
  begin
    IsLinuxExe := FileSignature = Ab_LinuxExeSigWord1; { check 1st sig }
    if IsLinuxExe then
    begin
      StubStream.Read(FileSignature, SizeOf(FileSignature)); { check 2nd sig }
      IsLinuxExe := FileSignature = Ab_LinuxExeSigWord2;
    end;
  end;

  if not (IsWinExe or IsLinuxExe) then
    raise EAbZipInvalidStub.Create;
  {!!.01 -- End Re-written}

  StubStream.Position := 0;
  StubSize := StubStream.Size;

  ZipStream.Position := 0;
  ZipStream.Read(FileSignature, sizeof(FileSignature));
  if FileSignature <> $4B50 then
    raise EAbZipInvalid.Create;
  ZipStream.Position := 0;

  {copy the stub into the selfex stream}
  SelfExtractingStream.Position := 0;
  SelfExtractingStream.CopyFrom(StubStream, 0);

  TailPosition := FindCentralDirectoryTail(ZipStream);
  if TailPosition = -1 then
    raise EAbZipInvalid.Create;
  {load the ZipDirectoryFileFooter}
  ZDFF := TAbZipDirectoryFileFooter.Create;
  try
    ZDFF.LoadFromStream(ZipStream);
    DirectoryStart := ZDFF.DirectoryOffset;
  finally
    ZDFF.Free;
  end;
  {copy everything up to the CDH into the SelfExtractingStream}
  ZipStream.Position := 0;
  SelfExtractingStream.CopyFrom(ZipStream, DirectoryStart);
  ZipStream.Position := DirectoryStart;
  repeat
    ZipItem := TAbZipItem.Create;
    try
      ZipItem.LoadFromStream(ZipStream);
      ZipItem.RelativeOffset := ZipItem.RelativeOffset + StubSize;
      {save the modified entry into the Self Extracting Stream}
      ZipItem.SaveCDHToStream(SelfExtractingStream);
    finally
      ZipItem.Free;
    end;
  until ZipStream.Position = TailPosition;

  {save the CDH Footer.}
  ZDFF := TAbZipDirectoryFileFooter.Create;
  try
    ZDFF.LoadFromStream(ZipStream);
    ZDFF.DirectoryOffset := ZDFF.DirectoryOffset + StubSize;
    ZDFF.SaveToStream(SelfExtractingStream);
  finally
    ZDFF.Free;
  end;
end;

end.
