{
  Copyright (c) 1999-2013 Andrew Filinsky and Melchiorre Caruso.

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
  
    v0.8.0 build 1864 - 2013.02.15 by Melchiorre Caruso.
}

unit Bee_Files;

{$I bee_compiler.inc}

interface

uses
  Math,
  Classes,
  DateUtils,
  SysUtils,

  Bee_Common,
  Bee_CommandLine,
  Bee_BufStream;

type
  TFileReaderRequestImageEvent = procedure(ImageNumber: longint;
     var ImageName: string; var Abort: boolean) of object;

  { TFileReader }

  TFileReader = class(TReadBufStream)
  private
    FFileName: string;
    FImageNumber: longint;
    FImagesNumber: longint;
    FOnRequestImage: TFileReaderRequestImageEvent;
    procedure DoOpenImage(Value: longint);
    function DoRequestImage(Value: longint): string;
    function GetImageName(Value: longint): string;
    procedure SetImageNumber(Value: longint);
    procedure SetImagesNumber(Value: longint);
  public
    constructor Create(const aFileName: string;
       aRequestImage: TFileReaderRequestImageEvent);
    destructor Destroy; override;
    function ReadDWord: dword;
    function ReadInfWord: qword;
    function ReadInfString: string;
    function ReadInfArray: string;
    function Read(Data: PByte; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: longint; ImageNum: longint): int64; overload;
  public
    property ImagesNumber: longint read FImagesNumber write SetImagesNumber;
    property ImageNumber: longint read FImageNumber write SetImageNumber;
  end;

  { TFileWriter }

  TFileWriterRequestBlankImageEvent = procedure(aImageNumber: longint;
     var Abort : Boolean) of object;

  TFileWriter = class(TWriteBufStream)
  protected
    FFileName: string;
    FThreshold: int64;
    FCurrentImage: longint;
    FCurrentImageSize: int64;
    FOnRequestBlankImage: TFileWriterRequestBlankImageEvent;
    function DoRequestImage(Value: longint): string;
    function GetImageName(Value: longint): string;
  public
    constructor Create(const aFileName: string; aRequestBlankImage:
      TFileWriterRequestBlankImageEvent; const aThreshold: int64);
    destructor Destroy; override;
    procedure CreateNewImage;

    procedure WriteDWord(Data: dword);
    procedure WriteInfWord(Data: qword);
    procedure WriteInfString(const Data: string);
    procedure WriteInfArray(const Data: string);
    function Write(Data: PByte; Count: longint): longint; override;
  public
    property Threshold: int64 read FThreshold;
    property CurrentImage: longint read FCurrentImage;
    property CurrentImageSize: int64 read FCurrentImageSize;
  end;

  { TCustomSearchRec }

  TCustomSearchRec = class(TObject)
    Name: string;
    Size: int64;
    LastModifiedTime: int64;
    Attributes: longint;
  end;

  { TSearchRecList }

  TFileScanner = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: longint): TCustomSearchRec;
    procedure RecursiveScan(Mask: string; Recursive: boolean);
    function New(const RecPath: string; const Rec: TSearchRec): TCustomSearchRec;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Mask: string; Recursive: boolean);
    procedure Delete(const Mask: string; Recursive: boolean);
    procedure Clear;
    property Count: integer read GetCount;
    property Items[Index: longint]: TCustomSearchRec read GetItem;
  end;

implementation

uses
  Bee_Interface;

{ TFileReader class }

constructor TFileReader.Create(const aFileName: string;
  aRequestImage: TFileReaderRequestImageEvent);
begin
  inherited Create(THandle(-1));
  FFileName       := aFileName;
  FImageNumber    := 1;
  FImagesNumber   := 1;
  FOnRequestImage := aRequestImage;

  if ExitStatus = esNoError then
  begin
    FHandle := FileOpen(FFileName, fmOpenRead or fmShareDenyWrite);
    if FHandle = -1 then
      SetExitStatus(esOpenStreamError);
  end;
end;

destructor TFileReader.Destroy;
begin
  FileClose(FHandle);
  inherited Destroy;
end;

function TFileReader.GetImageName(Value: longint): string;
begin
  if Value = FImagesNumber then
    Result := FFileName
  else
    Result := ChangeFileExt(FFileName, '.' + Format('%.3d', [Value]));
end;

function TFileReader.DoRequestImage(Value: longint): string;
var
  Abort: boolean;
begin
  Result := GetImageName(Value);
  while FileExists(Result) = FALSE do
  begin
    Abort := TRUE;
    if Assigned(FOnRequestImage) then
      FOnRequestImage(Value, Result, Abort);

    if Abort then
      SetExitStatus(esUserAbortError);

    if ExitStatus <> esNoError then Break;
  end;
end;

procedure TFileReader.DoOpenImage(Value: longint);
var
  ImageName: string;
begin
  ClearBuffer;
  FileClose(FHandle);

  FImageNumber := Value;
  ImageName := GetImageName(FImageNumber);
  if ExitStatus = esNoError then
  begin
    FHandle := FileOpen(ImageName, fmOpenRead or fmShareDenyWrite);
    if FHandle = -1 then
      SetExitStatus(esOpenStreamError);
  end;
end;

function TFileReader.ReadDWord: dword;
begin
  Read(@Result, SizeOf(Result));
end;

function TFileReader.ReadInfWord: qword;
var
  Last: byte;
  Temp: qword;
  Count: longword;
begin
  Result := 0;
  Count  := 0;
  repeat
    Read(@Last, 1);
    Temp   := Last and $7F;
    Temp   := Temp shl (7 * Count);
    Result := Result or Temp;

    if (Last and $80) = 0 then Break;
    Inc(Count);
  until FALSE;
end;

function TFileReader.ReadInfString: string;
var
  Len: longint;
begin
  Len := ReadInfWord;
  SetLength(Result, Len);
  if Len > 0 then
  begin
    Read(@Result[1], Len);
  end;
end;

function TFileReader.ReadInfArray: string;
var
  ArrLen: longint;
  Arr: TByteArray;
begin
  ArrLen := ReadInfWord;
  if ArrLen > 0 then
  begin
    Read(@Arr[0], ArrLen);
    Result := Hex(Arr, ArrLen);
  end else
    Result := '';
end;

function TFileReader.Read(Data: PByte; Count: longint): longint;
begin
  Result := 0;
  repeat
    Inc(Result, inherited Read(@Data[Result], Count - Result));

    if Result < Count then
    begin
      if FImageNumber < FImagesNumber then
      begin
        DoOpenImage(FImageNumber + 1);
        if ExitStatus <> esNoError then Break;
      end else
        Break;
    end;
  until Result = Count;
end;

function TFileReader.Seek(const Offset: int64; Origin: longint; ImageNum: longint): int64;
begin
  SetImageNumber(ImageNum);
  Result := Seek(Offset, Origin);
end;

procedure TFileReader.SetImagesNumber(Value: longint);
begin
  FImagesNumber := Value;
end;

procedure TFileReader.SetImageNumber(Value: longint);
begin
  if FImageNumber <> Value then
    DoOpenImage(Value);
end;

{ TFileWriter class }

constructor TFileWriter.Create(const aFileName: string; aRequestBlankImage:
  TFileWriterRequestBlankImageEvent; const aThreshold: int64);
var
  ImageName: string;
begin
  inherited Create(THandle(-1));
  FFileName            := aFileName;
  FThreshold           := Max(0, aThreshold);
  FCurrentImage        := 1;
  FCurrentImageSize    := 0;
  FOnRequestBlankImage := aRequestBlankImage;

  ImageName := DoRequestImage(FCurrentImage);
  if ExitStatus = esNoError then
  begin
    if ExtractFilePath(ImageName) <> '' then
      ForceDirectories(ExtractFilePath(ImageName));
    FHandle := FileCreate(ImageName);
    if FHandle = -1 then
      SetExitStatus(esCreateStreamError);
  end;
end;

destructor TFileWriter.Destroy;
begin
  FlushBuffer;
  FileClose(FHandle);
  inherited Destroy;
end;

procedure TFileWriter.WriteDWord(Data: dword);
begin
  Write(@Data, SizeOf(Data));
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
  Write(@LocalBuffer[0], Count);
end;

procedure TFileWriter.WriteInfString(const Data: string);
var
  Len: longint;
begin
  Len := Length(Data);
  WriteInfWord(Len);
  if Len > 0 then
  begin
    Write(@Data[1], Len);
  end;
end;

procedure TFileWriter.WriteInfArray(const Data: string);
var
  ArrLen: longint;
  Arr: TByteArray;
begin
  ArrLen := Length(Data) div 2;
  WriteInfWord(ArrLen);
  if ArrLen > 0 then
    if HexToData(Data, Arr, ArrLen) then
    begin
      Write(@Arr[0], ArrLen);
    end;
end;

function TFileWriter.GetImageName(Value: longint): string;
begin
  Result := ChangeFileExt(FFileName, '.' + Format('%.3d', [Value]));
end;

function TFileWriter.DoRequestImage(Value: longint): string;
var
  Abort: boolean;
begin
  Result := FFileName;
  if FThreshold > 0 then
    while GetDriveFreeSpace(Result) <= FThreshold do
    begin
      Abort := TRUE;
      if Assigned(FOnRequestBlankImage) then
        FOnRequestBlankImage(Value, Abort);
      if Abort then
        SetExitStatus(esUserAbortError);

      if ExitStatus <> esNoError then Break;
    end;
end;

procedure TFileWriter.CreateNewImage;
var
  ImageName: string;
begin
  FlushBuffer;
  FileClose(FHandle);
  if RenameFile(FFileName, GetImageName(FCurrentImage)) = FALSE then
    SetExitStatus(esRenameTempError);

  if ExitStatus = esNoError then
  begin
    Inc(FCurrentImage);
    ImageName := DoRequestImage(FCurrentImage);

    if ExitStatus = esNoError then
    begin
      if ExtractFilePath(ImageName) <> '' then
        ForceDirectories(ExtractFilePath(ImageName));
      FHandle := FileCreate(ImageName);
      if FHandle = -1 then
        SetExitStatus(esCreateStreamError);
    end;
  end;
  FCurrentImageSize := 0;
  ClearBuffer;
end;

function TFileWriter.Write(Data: PByte; Count: longint): longint;
var
  I: longint;
begin
  if FThreshold = 0 then
  begin
    Result := inherited Write(Data, Count);
  end else
  begin
    Result := 0;
    repeat
      if FCurrentImageSize = FThreshold then
      begin
        CreateNewImage;
        if ExitStatus <> esNoError then Break;
      end;
      I := Min(Count - Result, FThreshold - FCurrentImageSize);

      inherited Write(@Data[Result], I);
      Inc(FCurrentImageSize, I);
      Inc(Result, I);
    until Result = Count;
  end;
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
   TCustomSearchRec(FList[I]).Destroy;
  FList.Clear;
end;

function TFileScanner.New(const RecPath: string; const Rec: TSearchRec): TCustomSearchRec;
begin
  Result                  := TCustomSearchRec.Create;
  Result.Name             := RecPath + Rec.Name;
  Result.Size             := Rec.Size;
  Result.Attributes       := Rec.Attr;
  {$IFDEF MSWINDOWS}
  Result.LastModifiedTime := DateTimeToUnix(DosDateTimeToDateTime(Rec.Time));
  {$ENDIF}
  {$IFDEF MAC}
  Result.LastModifiedTime := DateTimeToUnix(MacTimeToDateTime(Rec.Time));
  {$ENDIF}
  {$IFDEF UNIX}
  Result.LastModifiedTime := Rec.Time;
  {$ENDIF}
end;

procedure TFileScanner.RecursiveScan(Mask: string; Recursive: boolean);
var
  RecName: string;
  RecPath: string;
  Rec: TSearchRec;
  Error: longint;
begin
  // directory and recursive mode ...
  Mask := ExcludeTrailingBackSlash(Mask);
  if DirectoryExists(Mask) then
  begin
    Recursive := TRUE;
    Mask := IncludeTrailingBackSlash(Mask) + '*';
  end;
  RecPath := ExtractFilePath(Mask);

  // search filemask ...
  Error := FindFirst(RecPath + '*', faAnyFile, Rec);
  while Error = 0 do
  begin
    RecName := RecPath + Rec.Name;
    if (Rec.Attr and faDirectory) = 0 then
    begin
      if FileNameMatch(RecName, Mask, Recursive) then
        FList.Add(New(RecPath, Rec));
    end else
      if (Recursive) and (Rec.Name <> '.') and (Rec.Name <> '..') then
        RecursiveScan(IncludeTrailingBackSlash(RecName) + ExtractFileName(Mask), Recursive);

    Error := FindNext(Rec);
  end; // end while error ...
  FindClose(Rec);
end;

procedure TFileScanner.Add(const Mask: string; Recursive: boolean);
var
  I: longint;
  Masks: TStringList;
begin
  Masks := TStringList.Create;
  ExpandFileMask(Mask, Masks, Recursive);
  for I := 0 to Masks.Count - 1 do
    RecursiveScan(Masks[I], Recursive);
  Masks.Free;
end;

procedure TFileScanner.Delete(const Mask: string; Recursive: boolean);
var
  I, J: longint;
  Masks: TStringList;
begin
  Masks := TStringList.Create;
  ExpandFileMask(Mask, Masks, Recursive);
  for I := 0 to Masks.Count - 1 do
    for J := Count - 1 downto 0 do
      if FileNameMatch(Items[J].Name, Masks[I], Recursive) then
      begin
        FList.Delete(I);
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