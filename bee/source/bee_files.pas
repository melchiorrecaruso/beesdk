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
  Bee_BlowFish;

type
  { TFileReader }

  TFileReader = class(TFileStream)
  private
    FBlowFish: TBlowFish;
    BufferSize: longint;
    BufferReaded: longint;
    Buffer: array [0..$1FFFF] of byte;
  public
    constructor Create(const FileName: string; Mode: word);
    destructor Destroy; override;
    function Read(var Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
    procedure BlowFish(const AKey: string); overload;
    procedure BlowFish; overload;
  end;

  { TFileWriter }

  TFileWriter = class(TFileStream)
  private
    FBlowFish: TBlowFish;
    BufferSize: longint;
    Buffer: array [0..$1FFFF] of byte;
  public
    constructor Create(const FileName: string; Mode: word);
    destructor Destroy; override;
    procedure Flush;
    function Write(const Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
    procedure BlowFish(const AKey: string); overload;
    procedure BlowFish; overload;
  end;

  { TNulWriter }

  TNulWriter = class(TFileWriter)
  private
    FSize:     int64;
    FPosition: int64;
  protected
    procedure SetSize(NewSize: longint); override;
    procedure SetSize(const NewSize: int64); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Data; Count: longint): longint; override;
    function Write(const Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
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

function CreateTFileReader(const FileName: string; Mode: word): TFileReader;
function CreateTFileWriter(const FileName: string; Mode: word): TFileWriter;

implementation

uses
  Bee_Assembler;

{ TFileReader class }

constructor TFileReader.Create(const FileName: string; Mode: word);
begin
  if Mode = fmCreate then
  begin
    ForceDirectories(ExtractFilePath(FileName));
  end;
  BufferSize   := 0;
  BufferReaded := 0;
  FBlowFish     := TBlowFish.Create;
  inherited Create(FileName, Mode);
end;

destructor TFileReader.Destroy;
begin
  FBlowFish.Free;
  inherited Destroy;
end;

function TFileReader.Read(var Data; Count: longint): longint;
var
  Bytes: array [0..$FFFFFFF] of byte absolute Data;
  S:     longint;
begin
  if (Count = 1) and (BufferReaded < BufferSize) then
  begin
    byte(Data) := Buffer[BufferReaded];
    Inc(BufferReaded);
    Result := 1;
  end else
  begin
    Result := 0;
    repeat
      if BufferReaded = BufferSize then
      begin
        BufferReaded := 0;
        BufferSize   := inherited Read(Buffer, SizeOf(Buffer));

        if BufferSize = 0 then Exit; // This causes Result < Count

        if FBlowFish.Started then
          FBlowFish.Decode(Buffer, BufferSize);
      end;
      S := Count - Result;

      if S > BufferSize - BufferReaded then
        S := BufferSize - BufferReaded;

      CopyBytes(Buffer[BufferReaded], Bytes[Result], S);

      Inc(Result, S);
      Inc(BufferReaded, S);
    until Result = Count;
  end;
end;

function TFileReader.Seek(Offset: longint; Origin: word): longint;
begin
  if (Origin = soFromCurrent) and (OffSet = 0) then
    Result := inherited Seek(Offset - (BufferSize - BufferReaded), Origin)
  else
    Result := inherited Seek(Offset, Origin);

  BufferSize   := 0;
  BufferReaded := 0;
end;

function TFileReader.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if (Origin = soCurrent) and (OffSet = 0) then
    Result := inherited Seek(Offset + BufferSize - BufferReaded, Origin)
  else
    Result := inherited Seek(Offset, Origin);

  BufferSize   := 0;
  BufferReaded := 0;
end;

procedure TFileReader.BlowFish(const AKey: string);
begin
  FBlowFish.Start(AKey);
end;

procedure TFileReader.BlowFish;
begin
  FBlowFish.Finish;
end;

function CreateTFileReader(const FileName: string; Mode: word): TFileReader;
begin
  try
    Result := TFileReader.Create(FileName, Mode);
  except
    Result := nil;
  end;
end;

{ TFileWriter class }

constructor TFileWriter.Create(const FileName: string; Mode: word);
begin
  if Mode = fmCreate then
  begin
    ForceDirectories(ExtractFilePath(FileName));
  end;
  BufferSize := 0;
  FBlowFish   := TBlowFish.Create;
  inherited Create(FileName, Mode);
end;

procedure TFileWriter.Flush;
begin
  if FBlowFish.Started then
  begin
    BufferSize := FBlowFish.Encode(Buffer, BufferSize);
  end;
  inherited Write(Buffer, BufferSize);
  BufferSize := 0;
end;

function TFileWriter.Write(const Data; Count: longint): longint;
var
  Bytes: array [0..$FFFFFFF] of byte absolute Data;
  S:     longint;
begin

  if (Count = 1) and (SizeOf(Buffer) > BufferSize) then
  begin
    Buffer[BufferSize] := byte(Data);
    Inc(BufferSize);
    Result := 1;
  end else
    if Count > (SizeOf(Buffer) - BufferSize) then
    begin
      Result := 0;
      repeat
        S := SizeOf(Buffer) - BufferSize;
        CopyBytes(Bytes[Result], Buffer[BufferSize], S);
        Inc(Result, S);
        Inc(BufferSize, S);
        Flush;
      until ((Count - Result) <= SizeOf(Buffer));

      CopyBytes(Bytes[Result], Buffer[BufferSize], Count - Result);
      Inc(BufferSize, Count - Result);
      Inc(Result, Count - Result);
    end else
    begin
      CopyBytes(Data, Buffer[BufferSize], Count);
      Inc(BufferSize, Count);
      Result := Count;
    end;

  (*

  if Count > (SizeOf(Buffer) - BufferSize) then
  begin
    Result := 0;
    repeat
      S := SizeOf(Buffer) - BufferSize;
      CopyBytes(Bytes[Result], Buffer[BufferSize], S);
      Inc(Result, S);
      Inc(BufferSize, S);
      Flush;
    until ((Count - Result) <= SizeOf(Buffer));

    CopyBytes(Bytes[Result], Buffer[BufferSize], Count - Result);
    Inc(BufferSize, Count - Result);
    Inc(Result, Count - Result);
  end else
    if Count > 1 then
    begin
      CopyBytes(Data, Buffer[BufferSize], Count);
      Inc(BufferSize, Count);
      Result := Count;
    end else
    begin
      Buffer[BufferSize] := byte(Data);
      Inc(BufferSize);
      Result := Count;
    end;
  *)
end;

function TFileWriter.Seek(Offset: longint; Origin: word): longint;
begin
  if BufferSize > 0 then
  begin
    Flush;
  end;
  Result := inherited Seek(Offset, Origin);
end;

function TFileWriter.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if BufferSize > 0 then
  begin
    Flush;
  end;
  Result := inherited Seek(Offset, Origin);
end;

destructor TFileWriter.Destroy;
begin
  if BufferSize > 0 then
  begin
    Flush;
  end;
  FBlowFish.Free;
  inherited Destroy;
end;

procedure TFileWriter.BlowFish(const AKey: string);
begin
  FBlowFish.Start(AKey);
end;

procedure TFileWriter.BlowFish;
begin
  FBlowFish.Finish;
end;

function CreateTFileWriter(const FileName: string; Mode: word): TFileWriter;
begin
  try
    Result := TFileWriter.Create(FileName, Mode);
  except
    Result := nil;
  end;
end;

{ TNulWriter class }

constructor TNulWriter.Create;
begin
  { inherited Create; }
  FBlowFish  := TBlowFish.Create;
  FPosition := 0;
  FSize     := 0;
end;

destructor TNulWriter.Destroy;
begin
  FBlowFish.Free;
  { inherited Destroy; }
end;

function TNulWriter.Read(var Data; Count: longint): longint;
begin
  Result := 0;
end;

function TNulWriter.Write(const Data; Count: longint): longint;
begin
  Inc(FPosition, Count);
  if FPosition > FSize then
  begin
    FSize := FPosition;
  end;
  Result := Count;
end;

function TNulWriter.Seek(Offset: longint; Origin: word): longint;
begin
  case Origin of
    soFromBeginning: FPosition := OffSet;
    soFromCurrent: FPosition := Min(FSize, FPosition + Offset);
    soFromEnd: FPosition := Max(0, FPosition - Offset);
  end;
  Result := FPosition;
end;

function TNulWriter.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  case Origin of
    soBeginning: FPosition := OffSet;
    soCurrent: FPosition := Min(FSize, FPosition + Offset);
    soEnd: FPosition := Max(0, FPosition - Offset);
  end;
  Result := FPosition;
end;

procedure TNulWriter.SetSize(NewSize: longint);
begin
  FSize     := NewSize;
  FPosition := FSize;
end;

procedure TNulWriter.SetSize(const NewSize: int64);
begin
  FSize     := NewSize;
  FPosition := FSize;
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
