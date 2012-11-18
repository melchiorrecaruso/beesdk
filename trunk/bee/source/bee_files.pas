{
  Copyright (c) 1999-2009 Andrew Filinsky and Melchiorre Caruso

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

    TFileReader class, bufferized TStream-similar input stream;
    TFileWriter class, bufferized TStream-similar output stream;
    TNulWriter  class, TStream-similar output stream, but works with 'nul' file.

  Modifyed:

    v0.7.8 build 0148 - 2005.06.23 by Andrew Filinsky;
    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;
  
    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_Files;

{$I compiler.inc}

interface

uses
  Math,
  Classes,
  SysUtils,
  Bee_Common,
  Bee_CommandLine,
  Bee_BufStream;

type
  { TFileReader }

  TFileReaderRequestImageEvent = procedure(ImageNumber: longint;
     var ImageName: string; var Abort: boolean) of object;

  TFileReader = class(TReadBufStream)
  private
    FFileName: string;
    FImageNumber: longword;
    FImagesNumber: longword;
    FOnRequestImage: TFileReaderRequestImageEvent;
    procedure GotoImage;
    function GetImageName: string;
    function GetIsValid: boolean;
    procedure SetImagesNumber(Value: longword);
    procedure SetImageNumber(Value: longword);
  public
    constructor Create(const aFileName: string;
       aRequestImage: TFileReaderRequestImageEvent);
    destructor Destroy; override;
    procedure Fill;

    function ReadDWord: dword;
    function ReadInfWord: qword;
    function ReadInfString: string;
    function Read(var Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64;override;
    procedure SeekImage(aImageNumber: longword; const Offset: int64);
  public
    property IsValid: boolean read GetIsValid;
    property ImagesNumber: longword read FImagesNumber write SetImagesNumber;
    property ImageNumber: longword read FImageNumber write SetImageNumber;
  end;

  { TFileWriter }

  TFileWriterRequestBlankDiskEvent = procedure(var Abort : Boolean) of object;

  TFileWriter = class(TWriteBufStream)
  private
    FFileName: string;
    FCurrentImage: longword;
    FCurrentImageSize: int64;
    FThreshold: int64;
    FOnRequestBlankDisk: TFileWriterRequestBlankDiskEvent;
    function GetIsValid: boolean; virtual;
    function GetImageName(ImageNumber: longword): string;
    function GetCurrentImage: longword;
    function GetABSPosition: int64;
    function GetPosition: int64;
    function GetSize: int64;
  public
    constructor Create(const aFileName: string; const aThreshold: int64;
      aRequestBlankDisk: TFileWriterRequestBlankDiskEvent);
    destructor Destroy; override;
    procedure Flush;

    procedure CreateNewImage(const aThreshold: int64);
    procedure WriteDWord(Data: dword);
    procedure WriteInfWord(Data: qword);
    procedure WriteInfString(const Data: string);
    function Write(const Data; Count: longint): longint; override;
    function WriteUnspanned(const Data; Count: longint): longint;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  public
    property IsValid: boolean read GetIsValid;
    property CurrentImage: longword read GetCurrentImage;
    property CurrentImageSize: int64 read FCurrentImageSize;
    property Threshold: int64 read FThreshold;

    property ABSPosition: int64 read GetABSPosition;
    property Position: int64 read GetPosition;
    property Size: int64 read GetSize write SetSize;
  end;

  { TNulWriter }

  TNulWriter = class(TFileWriter)
  private
    FNulPos: int64;
    FNulSize: int64;
  protected
    function GetIsValid: boolean; override;
    procedure FlushBuffer; override;
    procedure SetSize(NewSize: longint); override;
    procedure SetSize(const NewSize: int64); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Write(const Data; Count: longint): longint;  override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  end;

  { TCustomSearchRec }

  TCustomSearchRec = class
  public
    Name: string;
    Size: int64;
    Attributes: longint;
    LastModifiedTime: longint;
  public
    constructor CreateFrom(Item: TCustomSearchRec);
  end;

  { TFileScanner }

  TFileScanner = class
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: longint): TCustomSearchRec;
    procedure RecursiveScan(Mask: string; ExcludeMasks: TStringList; Recursive: TRecursiveMode);
    function CreateItem(const RecPath: string; const Rec: TSearchRec): TCustomSearchRec;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Scan(const Mask: string; ExcludeMasks: TStringList; Recursive: TRecursiveMode);
    procedure Clear;
    property Count: integer read GetCount;
    property Items[Index: longint]: TCustomSearchRec read GetItem;
  end;

function DoFill (Stream: pointer; Data: pointer;
   Size: longint): longint; {$IFDEF cppDLL} cdecl; {$ENDIF}
function DoFlush(Stream: pointer; Data: pointer;
   Size: longint): longint; {$IFDEF cppDLL} cdecl; {$ENDIF}

implementation

function DoFill(Stream: pointer; Data: pointer; Size: longint): longint;
begin
  Result := TFileReader(Stream).Read(Data^, Size);
end;

function DoFlush(Stream: pointer; Data: pointer; Size: longint): longint;
begin
  Result := TFileWriter(Stream).Write(Data^, Size);
end;

{ TFileReader class }

constructor TFileReader.Create(const aFileName: string;
  aRequestImage: TFileReaderRequestImageEvent);
begin
  inherited Create(nil);
  FFileName       := aFileName;
  FImagesNumber   := 1;
  FImageNumber    := 1;
  FOnRequestImage := aRequestImage;
  GotoImage;
end;

destructor TFileReader.Destroy;
begin
  if Assigned(FSource) then
     FreeAndNil(FSource);
  inherited Destroy;
end;

function TFileReader.GetImageName: string;
begin
  if FImageNumber <> FImagesNumber then
    Result := ChangeFileExt(FFileName, '.' + Format('%.3d', [FImageNumber]))
  else
    Result := FFileName;
end;

procedure TFileReader.GotoImage;
var
  Abort: boolean;
  ImageName: string;
begin
  if Assigned(FSource) then
    FreeAndNil(FSource);

  if FImageNumber >= 1 then
    if FImageNumber <= FImagesNumber then
    begin
      ImageName := GetImageName;
      while FileExists(ImageName) = FALSE do
      begin
        Abort := TRUE;
        if Assigned(FOnRequestImage) then
          FOnRequestImage(FImageNumber, ImageName, Abort);
        if Abort then Exit;
      end;

      try
        FSource := TFileStream.Create(ImageName, fmOpenRead or fmShareDenyWrite);
      except
        FSource := nil;
      end;
    end;
end;

function TFileReader.ReadDWord: dword;
begin
  Read(Result, SizeOf(Result));
end;

function TFileReader.ReadInfWord: qword;
var
  Last: byte;
  Temp: qword;
  Count: longword;
begin
  Result := 0;
  Count  := 0;
  while Read(Last, 1) = 1 do
  begin
    Temp   := Last and $7F;
    Temp   := Temp shl (7 * Count);
    Result := Result or Temp;

    if (Last and $80) = 0 then Break;
    Inc(Count);
  end;
end;

function TFileReader.ReadInfString: string;
var
  Len: longint;
begin
  Len := ReadInfWord;
  if Assigned(FSource) then
  begin
    SetLength(Result, Len);
    if Len > 0 then
      Read(Result[1], Len);
  end;
end;

function TFileReader.Read(var Data; Count: longint): longint;
var
  Readed: longint;
  Bytes: PByte;
begin
  Result := 0;
  Bytes := @Data;
  while Assigned(FSource) and (Count > 0) do
  begin
    Readed := inherited Read(Bytes[Result], Count);
    Inc(Result, Readed);
    Dec(Count,  Readed);

    if Count > 0 then
    begin
      if FImageNumber < FImagesNumber then
      begin
        Inc(FImageNumber);
        GotoImage;
      end else
        Break;
    end;
  end;
end;

procedure TFileReader.Fill;
begin
  if Assigned(FSource) then
     FillBuffer;
end;

function TFileReader.Seek(Offset: longint; Origin: word): longint;
begin
  if Assigned(FSource) then
    inherited Seek(Offset, Origin);
end;

function TFileReader.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if Assigned(FSource) then
    inherited Seek(Offset, Origin);
end;

procedure TFileReader.SeekImage(aImageNumber: longword; const Offset: int64);
begin
  SetImageNumber(aImageNumber);
  Seek(Offset, soBeginning);
end;

function TFileReader.GetIsValid: boolean;
begin
  Result := Assigned(FSource);
end;

procedure TFileReader.SetImagesNumber(Value: longword);
begin
  FImagesNumber := Value;
  GotoImage;
end;

procedure TFileReader.SetImageNumber(Value: longword);
begin
  FImageNumber := Value;
  GotoImage;
end;

{ TFileWriter class }

constructor TFileWriter.Create(const aFileName: string;
  const aThreshold: int64; aRequestBlankDisk: TFileWriterRequestBlankDiskEvent);
begin
  inherited Create(nil);
  FFileName := aFileName;
  FCurrentImage       := 0;
  FCurrentImageSize   := 0;
  FOnRequestBlankDisk := aRequestBlankDisk;
  CreateNewImage(Max(0, aThreshold));
end;

destructor TFileWriter.Destroy;
begin
  if Assigned(FSource) then
  begin
    FlushBuffer;
    FreeAndNil(FSource);
  end;
  inherited Destroy;
end;

procedure TFileWriter.Flush;
begin
  FlushBuffer;
end;

function TFileWriter.GetCurrentImage: longword;
begin
  FlushBuffer;
  Result := FCurrentImage;
end;

function TFileWriter.GetABSPosition: int64;
begin
  Result := (FCurrentImage * FThreshold) + Seek(0, soCurrent);
end;

function TFileWriter.GetPosition: int64;
begin
  Result := Seek(0, soCurrent);
end;

function TFileWriter.GetSize: int64;
var
  I: int64;
begin
  I := Seek(0, soCurrent);
  Result := Seek(0, soEnd);
  Seek(I, soBeginning);
end;

procedure TFileWriter.CreateNewImage(const aThreshold: int64);
var
  Abort: boolean;
begin
  if Assigned(FSource) then
  begin
    FlushBuffer;
    FreeAndNil(FSource);
  end;
  FThreshold := aThreshold;
  FCurrentImageSize := 0;

  if FThreshold > 0 then
      // while GetDriveFreeSpace(FFileName) > 0 do
      begin
        Abort := TRUE;
        if Assigned(FOnRequestBlankDisk) then
          FOnRequestBlankDisk(Abort);
        if Abort then Exit;
      end;

  RenameFile(FFileName, GetImageName(FCurrentImage));
  try
    if ExtractFilePath(FFileName) <> '' then
      ForceDirectories(ExtractFilePath(FFileName));
    FSource := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);

    Inc(FCurrentImage);
  except
    FSource := nil;
  end;
end;

procedure TFileWriter.WriteDWord(Data: dword);
begin
  Write(Data, SizeOf(Data));
end;

procedure TFileWriter.WriteInfWord(Data: qword);
var
  LocalBuffer: array[0..9] of byte;
  LastByte: byte;
  Count: longint;
begin
  Count := 0;
  repeat
    LastByte := Data and $7F;
    Data := Data shr 7;
    if Data <> 0 then
    begin
      LastByte := LastByte or $80
    end;
    LocalBuffer[Count] := LastByte;
    Inc(Count);
  until Data = 0;
  Write(LocalBuffer[0], Count);
end;

procedure TFileWriter.WriteInfString(const Data: string);
var
  Len: longint;
begin
  Len := Length(Data);
  WriteInfWord(Len);
  if Len > 0 then
  begin
    Write(Data[1], Len);
  end;
end;

function TFileWriter.GetImageName(ImageNumber: longword): string;
begin
  if ImageNumber <> 0 then
    Result := ChangeFileExt(FFileName, '.' + Format('%.3d', [ImageNumber]))
  else
    Result := FFileName;
end;

function TFileWriter.GetIsValid: boolean;
begin
  Result := Assigned(FSource);
end;

function TFileWriter.Write(const Data; Count: longint): longint;
var
  Writed: longint;
  Bytes: PByte;
begin
  if FThreshold = 0 then
  begin
    Result := WriteUnspanned(Data, Count);
  end else
  begin
    Result := 0;
    Bytes := @Data;
    while Assigned(FSource) and (Count > 0) do
    begin
      Writed := inherited Write(Bytes[Result], Min(Count, FThreshold - FCurrentImageSize));
      Inc(FCurrentImageSize, Writed);
      Inc(Result, Writed);
      Dec(Count,  Writed);

      if Count > 0 then CreateNewImage(FThreshold);
    end;
  end;
end;

function TFileWriter.WriteUnspanned(const Data; Count: longint): longint;
var
  Writed: longint;
  Bytes: PByte;
begin
  Result := 0;
  Bytes := @Data;
  while Assigned(FSource) and (Count > 0) do
  begin
    Writed := inherited Write(Bytes[Result], Count);
    Inc(FCurrentImageSize, Writed);
    Inc(Result, Writed);
    Dec(Count,  Writed);

    if Writed = 0 then FreeAndNil(FSource);
  end;
  FThreshold := 0;
end;

function TFileWriter.Seek(Offset: longint; Origin: word): longint;
begin
  if Assigned(FSource) then
    Result := inherited Seek(OffSet, Origin);
end;

function TFileWriter.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if Assigned(FSource) then
    Result := inherited Seek(OffSet, Origin);
end;

{ TNulWriter class }

constructor TNulWriter.Create;
begin
  FNulPos  := 0;
  FNulSize := 0;
end;

destructor TNulWriter.Destroy;
begin
  // nothing to do
end;

procedure TNulWriter.FlushBuffer;
begin
  // nothing to do
end;

procedure TNulWriter.SetSize(NewSize: longint);
begin
  FNulPos  := NewSize;
  FNulSize := NewSize;
end;

procedure TNulWriter.SetSize(const NewSize: int64);
begin
  FNulPos  := NewSize;
  FNulSize := NewSize;
end;

function TNulWriter.Write(const Data; Count: longint): longint;
begin
  Inc(FNulPos, Count);
  if FNulPos > FNulSize then
  begin
    FNulSize := FNulPos;
  end;
  Result := Count;
end;

function TNulWriter.Seek(Offset: longint; Origin: word): longint;
begin
  case Origin of
    soFromBeginning: FNulPos := OffSet;
    soFromCurrent:   FNulPos := Min(FNulSize, FNulPos + Offset);
    soFromEnd:       FNulPos := Max(0, FNulPos - Offset);
  end;
  Result := FNulPos;
end;

function TNulWriter.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  case Origin of
    soBeginning: FNulPos := OffSet;
    soCurrent:   FNulPos := Min(FNulSize, FNulPos + Offset);
    soEnd:       FNulPos := Max(0, FNulPos - Offset);
  end;
  Result := FNulPos;
end;

function TNulWriter.GetIsValid: boolean;
begin
  Result := TRUE;
end;

{ TCustomSearchRec class }

constructor TCustomSearchRec.CreateFrom(Item: TCustomSearchRec);
begin
  inherited Create;
  Name             := Item.Name;
  Size             := Item.Size;
  Attributes       := Item.Attributes;
  LastModifiedTime := Item.LastModifiedTime;
end;

{ TFileScanner class }

constructor TFileScanner.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TFileScanner.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TFileScanner.Clear;
var
  I: longint;
begin
  for I := 0 to FList.Count - 1 do
  begin
    TCustomSearchRec(FList[I]).Destroy;
  end;
  FList.Clear;
end;

function TFileScanner.CreateItem(const RecPath: string; const Rec: TSearchRec): TCustomSearchRec;
begin
  Result                  := TCustomSearchRec.Create;
  Result.Name             := RecPath + Rec.Name;
  Result.Size             := Rec.Size;
  Result.Attributes       := Rec.Attr;
  Result.LastModifiedTime := Rec.Time;
end;

procedure TFileScanner.RecursiveScan(Mask: string; ExcludeMasks: TStringList; Recursive: TRecursiveMode);
var
  Error: longint;
  Rec: TSearchRec;
  RecName: string;
  RecPath: string;
begin
  // directory and recursive mode ...
  Mask := ExcludeTrailingBackSlash(Mask);
  if DirectoryExists(Mask) then
  begin
    Recursive := rmFull;
    Mask := IncludeTrailingBackSlash(Mask) + '*';
  end;
  RecPath := ExtractFilePath(Mask);

  //  recursive rmWildCard mode...
  if Recursive = rmWildCard then
  begin
    if FileNameHasWildCards(Mask) then
      Recursive := rmFull
    else
      Recursive := rmNone;
  end;

  // search filemask ...
  Error := FindFirst(RecPath + '*', faAnyFile, Rec);
  while Error = 0 do
  begin
    RecName := RecPath + Rec.Name;
    if (Rec.Attr and faDirectory) = 0 then
    begin
      if FileNameMatch(RecName, Mask, Recursive) then
        if not FileNameMatch(RecName, ExcludeMasks, Recursive) then
          FList.Add(CreateItem(RecPath, Rec));
    end else
      if (Recursive <> rmNone) and (Rec.Name <> '.') and (Rec.Name <> '..') then
        RecursiveScan(IncludeTrailingBackSlash(RecName) + ExtractFileName(Mask), ExcludeMasks, Recursive);

    Error := FindNext(Rec);
  end; // end while error ...
  FindClose(Rec);
end;

procedure TFileScanner.Scan(const Mask: string; ExcludeMasks: TStringList; Recursive: TRecursiveMode);
var
  I: longint;
  Masks: TStringList;
begin
  Masks := TStringList.Create;
  ExpandFileMask(Mask, Masks, Recursive);
  for I := 0 to Masks.Count - 1 do
  begin
    RecursiveScan(Masks[I], ExcludeMasks, Recursive);
  end;
  Masks.Free;
end;

function TFileScanner.GetCount: longint;
begin
  Result := FList.Count;
end;

function TFileScanner.GetItem(Index:longint): TCustomSearchRec;
begin
  Result := TCustomSearchRec(FList.Items[Index]);
end;

end.
