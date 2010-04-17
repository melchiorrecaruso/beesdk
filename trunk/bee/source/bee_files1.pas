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
  
    v0.8.0 build 1110 - 2010.04.07 by Melchiorre Caruso.
}

unit Bee_Files;

{$I compiler.inc}

interface

uses
  Math,
  Classes,
  SysUtils,
  Bee_Types,
  Bee_Common,
  Bee_BufStream;

type
  { TFileReader }

  TFileReader = class(TReadBufStream2)
  private
    FFileStream: TFileStream;
  public
    constructor Create(const AFileName: string; AMode: word);
    destructor Destroy; override;
  end;

  { TFileWriter }

  TFileWriter = class(TWriteBufStream2)
  private
    FFileStream: TFileStream;
  public
    constructor Create(const AFileName: string; AMode: word);
    destructor Destroy; override;
  end;

  { TNulWriter }

  TNulWriter = class(TFileWriter)
  private
    FNulPos: int64;
    FNulSize: int64;
  public
    constructor Create;
    destructor Destroy; override;
    function Write(const Data; Count: longint): longint;
    function Seek(Offset: longint; Origin: word): longint; overload;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; overload;
  end;

  { TCustomSearchRec }

  TCustomSearchRec = class
  public
    FileName: string;
    FileSize: int64;
    FileTime: longint;
    FileAttr: longint;
    FileLink: string;
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

function CreateTFileReader(const AFileName: string; AMode: word): TFileReader;
function CreateTFileWriter(const AFileName: string; AMode: word): TFileWriter;

implementation

function CreateTFileReader(const AFileName: string; AMode: word): TFileReader;
begin
  try
    Result := TFileReader.Create(AFileName, AMode);
  except
    Result := nil;
  end;
end;

function CreateTFileWriter(const AFileName: string; AMode: word): TFileWriter;
begin
  try
    Result := TFileWriter.Create(AFileName, AMode);
  except
    Result := nil;
  end;
end;

{ TFileReader class }

constructor TFileReader.Create(const AFileName: string; AMode: word);
begin
  FFileStream := TFileStream.Create(AFileName, AMode);
  inherited Create(FFileStream);
end;

destructor TFileReader.Destroy;
begin
  inherited Destroy;
  FFileStream.Free;
end;

{ TFileWriter class }

constructor TFileWriter.Create(const AFileName: string; AMode: word);
begin
  if AMode = fmCreate then
  begin
    Bee_Common.ForceDirectories(ExtractFilePath(AFileName));
  end;
  FFileStream := TFileStream.Create(AFileName, AMode);
  inherited Create(FFileStream);
end;

destructor TFileWriter.Destroy;
begin
  inherited Destroy;
  FFileStream.Free;
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
    soFromCurrent: FNulPos := Min(FNulSize, FNulPos + Offset);
    soFromEnd: FNulPos := Max(0, FNulPos - Offset);
  end;
  Result := FNulPos;
end;

function TNulWriter.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  case Origin of
    soBeginning: FNulPos := OffSet;
    soCurrent: FNulPos := Min(FNulSize, FNulPos + Offset);
    soEnd: FNulPos := Max(0, FNulPos - Offset);
  end;
  Result := FNulPos;
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
    FreeMem(FList.Items[I]);
  end;
  FList.Clear;
end;

function TFileScanner.CreateItem(const RecPath: string; const Rec: TSearchRec): TCustomSearchRec;
begin
  Result := TCustomSearchRec.Create;
  Result.FileName := DeleteFileDrive(RecPath) + Rec.Name;
  Result.FileSize := Rec.Size;
  Result.FileTime := Rec.Time;
  Result.FileAttr := Rec.Attr;
  Result.FileLink := RecPath + Rec.Name;
end;

procedure TFileScanner.RecursiveScan(Mask: string; ExcludeMasks: TStringList; Recursive: TRecursiveMode);
var
  Error: longint;
  Rec: TSearchRec;
  RecName: string;
  RecPath: string;
begin
  // directory and recursive mode...
  Mask := ExcludeTrailingBackSlash(Mask);
  if DirectoryExists(Mask) then
  begin
    Recursive := rmFull;
    Mask := IncludeTrailingBackSlash(Mask) + '*';
  end;
  RecPath := ExtractFilePath(Mask);

  // search filemask...
  Error := FindFirst(RecPath + '*', faAnyFile, Rec);
  while Error = 0 do
  begin
    RecName := RecPath + Rec.Name;

    if FileNameMatch(RecName, Mask, Recursive) then
      if not FileNameMatch(RecName, ExcludeMasks, Recursive) then
      begin
        if (Rec.Attr and faDirectory) = 0 then
          FList.Add(CreateItem(RecPath, Rec))
        else
          { TODO : Verify Recursive <> rmNone }
          if (Recursive <> rmNone) and (Rec.Name <> '.') and (Rec.Name <> '..') then
            RecursiveScan(IncludeTrailingBackSlash(RecName) + ExtractFileName(Mask), ExcludeMasks, Recursive);
      end;
    Error := FindNext(Rec);
  end; // end while error...
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
