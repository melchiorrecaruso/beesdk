{
  Copyright (c) 2010-2013 Melchiorre Caruso.

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

    TFileScanner class.

  First release:

    v1.0 build 2165 - 2013.12.26 by Melchiorre Caruso.

  Modifyed:

}

unit bx_FileScanner;

{$I bx_compiler.inc}

interface

uses
  Classes,
  SysUtils;

type
  { TFileScannerItem }

  TFileScannerItem = class(TObject)
  private
    FFileName: string;
    FFileSize: int64;
    FFileTime: int64;
    FFileAttr: longint;
  public
    constructor Create(const RecPath: string; const Rec: TSearchRec);
  public
    property FileName: string read FFileName;
    property FileSize: int64 read FFileSize;
    property FileTime: int64 read FFileTime;
    property FileAttr: longint read FFileAttr;
  end;

  { TFileScanner }

  TFileScanner = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: longint): TFileScannerItem;
    procedure Scan(FileMask: string; Recursive: boolean);
    procedure AddItem(const RecPath: string; const Rec: TSearchRec);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const FileMask: string; Recursive: boolean);
    procedure Delete(const FileMask: string; Recursive: boolean);
    function Find(const FileName: string): longint;
    procedure Sort(Compare: TListSortCompare);
    procedure Clear;
  public
    property Count: integer read GetCount;
    property Items[Index: longint]: TFileScannerItem read GetItem;
  end;

implementation

uses
  bx_Common,
  DateUtils;

{ TFileScannerItem class }

constructor TFilescannerItem.Create(const RecPath: string; const Rec: TSearchRec);
begin
  inherited Create;
  FFileName := RecPath + Rec.Name;
  FFileSize := Rec.Size;
  FFileTime := DateTimeToUnix(FileDateToDateTime(Rec.Time));
  FFileAttr := Rec.Attr;
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
    TFileScannerItem(FList[I]).Destroy;
  FList.Clear;
end;

procedure TFileScanner.AddItem(const RecPath: string; const Rec: TSearchRec);
begin
  if Find(RecPath + Rec.Name) = -1 then
  begin
    FList.Add(TFileScannerItem.Create(RecPath, Rec));
  end;
end;

function TFilescanner.Find(const FileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := 0 to GetCount - 1 do
    if AnsiCompareFileName(FileName, Items[I].FileName) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

procedure TFileScanner.Scan(FileMask: string; Recursive: boolean);
var
  RecName: string;
  RecPath: string;
  Rec: TSearchRec;
  Error: longint;
begin
  if FileMask = '' then Exit;
  // directory and recursive mode ...
  if DirectoryExists(FileMask) then
  begin
    Recursive := TRUE;
    FileMask  := IncludeTrailingPathDelimiter(FileMask) + '*';
  end else
    if FileMask[Length(FileMask)] = PathDelim then
    begin
      Recursive := TRUE;
      FileMask  := IncludeTrailingPathDelimiter(FileMask) + '*';
    end;

  RecPath := ExtractFilePath(FileMask);
  while FileMaskHasWildcards(RecPath) do
  begin
    RecPath := ExtractFilePath(ExcludeTrailingPathDelimiter(RecPath));
  end;

  // search filemask ...
  Error := FindFirst(
    RecPath + '*',
    faReadOnly  or
    faHidden    or
    faSysFile   or
    faVolumeId  or
    faDirectory or
    faArchive   or
    faSymLink   or
    faAnyFile,
    Rec);

  while Error = 0 do
  begin
    RecName := RecPath + Rec.Name;
    if (Rec.Attr and faDirectory) = faDirectory then
    begin
      if (Rec.Attr and faSymLink) = 0 then
        if (Recursive) and (Rec.Name <> '.') and (Rec.Name <> '..') then
          Scan(IncludeTrailingBackSlash(RecName) +
            ExtractFileName(FileMask), Recursive);
    end else
      if FileNameMatch(RecName, FileMask, Recursive) then
      begin
        AddItem(RecPath, Rec);
      end;

    Error := FindNext(Rec);
  end; // end while error ...
  FindClose(Rec);
end;

procedure TFileScanner.Add(const FileMask: string; Recursive: boolean);
begin
  Scan(FileMask, Recursive);
end;

procedure TFileScanner.Delete(const FileMask: string; Recursive: boolean);
var
  I: longint;
begin
  for I := Count - 1 downto 0 do
    if FileNameMatch(Items[I].FileName, FileMask, Recursive) then
    begin
      FList.Delete(I);
    end;
end;

procedure TFileScanner.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

function TFileScanner.GetCount: longint;
begin
  Result := FList.Count;
end;

function TFileScanner.GetItem(Index:longint): TFileScannerItem;
begin
  Result := TFileScannerItem(FList.Items[Index]);
end;

end.
