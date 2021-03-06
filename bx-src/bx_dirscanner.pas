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

    Directory scanner class.

  Modifyed:

    v1.0 build 2225 - 2014.01.26 by Melchiorre Caruso.

}

unit bx_dirscanner;

{$I bx_compiler.inc}

interface

uses
  Classes,
  SysUtils;

type
  { TDirScannerItem }

  TDirScannerItem = class(TObject)
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

  { TDirScanner }

  TDirScanner = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: longint): TDirScannerItem;
    procedure Scan(const FilePath, FileMask:string; Recursive: boolean);
    procedure AddItem(const RecPath: string; const Rec: TSearchRec);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(FileMask: string; Recursive: boolean);
    procedure Delete(const FileMask: string; Recursive: boolean);
    function Find(const FileName: string): longint;
    procedure Sort(Compare: TListSortCompare);
    procedure Clear;
  public
    property Count: integer read GetCount;
    property Items[Index: longint]: TDirScannerItem read GetItem;
  end;

implementation

uses
  bx_common,
  DateUtils;

{ TDirScannerItem class }

constructor TDirScannerItem.Create(const RecPath: string; const Rec: TSearchRec);
begin
  inherited Create;
  FFileName := RecPath + Rec.Name;
  FFileSize := Rec.Size;
  FFileTime := Rec.Time;
  FFileAttr := Rec.Attr;
end;

{ TDirScanner class }

constructor TDirScanner.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDirScanner.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TDirScanner.Clear;
var
  I: longint;
begin
  for I := 0 to FList.Count - 1 do
  begin
    TDirScannerItem(FList[I]).Destroy;
  end;
  FList.Clear;
end;

procedure TDirScanner.AddItem(const RecPath: string; const Rec: TSearchRec);
var
  L, M, H, I: longint;
begin
  //if (Rec.Attr and faReadOnly ) > 0 then Exit;
  //if (Rec.Attr and faHidden   ) > 0 then Exit;
  //if (Rec.Attr and faSysFile  ) > 0 then Exit;
  if (Rec.Attr and faVolumeId ) > 0 then Exit;
  //if (Rec.Attr and faDirectory) > 0 then Exit;
  //if (Rec.Attr and faArchive  ) > 0 then Exit;
  //if (Rec.Attr and faSymLink  ) > 0 then Exit;
  //if (Rec.Attr and faAnyFile  ) > 0 then Exit;

  if FList.Count <> 0 then
  begin
    L := 0;
    H := FList.Count - 1;
    while H >= L do
    begin
      M := (L + H) div 2;
      I := AnsiCompareFileName(
        TDirScannerItem(FList[M]).FFileName, RecPath + Rec.Name);
      if I < 0 then
        L := M + 1
      else
        if I > 0 then
          H := M - 1
        else
          H := -2;
    end;

    if I < 0 then
      FList.Insert(M + 1, TDirScannerItem.Create(RecPath, Rec))
    else
      if I > 0 then
        FList.Insert(M, TDirScannerItem.Create(RecPath, Rec));

  end else
    FList.Add(TDirScannerItem.Create(RecPath, Rec));
end;

function TDirScanner.Find(const FileName: string): longint;
var
  L, M, H, I: longint;
begin
  L := 0;
  H := FList.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := AnsiCompareFileName(
      TDirScannerItem(FList[M]).FileName, FileName);
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

procedure TDirScanner.Scan(const FilePath, FileMask: string; Recursive: boolean);
var
  Error: longint;
  Rec: TSearchRec;
  RecPath: string;
begin
  if FilePath <> '' then
    RecPath := IncludeTrailingPathDelimiter(FilePath)
  else
    RecPath := FilePath;

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
    if (Rec.Attr and faDirectory) = faDirectory then
    begin
      if (Rec.Name <> '.') and (Rec.Name <> '..') then
      begin
        Scan(RecPath + Rec.Name, FileMask, Recursive);
        if FileNameMatch(RecPath + Rec.Name, FileMask, Recursive) then
        begin
          AddItem(RecPath, Rec);
        end;

      end;
    end else
      if FileNameMatch(RecPath + Rec.Name, FileMask, Recursive) then
      begin
        AddItem(RecPath, Rec);
      end;

    Error := FindNext(Rec);
  end; // end while error ...
  FindClose(Rec);
end;

procedure TDirScanner.Add(FileMask: string; Recursive: boolean);
var
  FilePath: string;
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
      FileMask  := FileMask + '*';
    end;

  FilePath := ExtractFilePath(FileMask);
  while FileMaskHasWildcards(FilePath) do
  begin
    FilePath := ExtractFilePath(ExcludeTrailingPathDelimiter(FilePath));
  end;
  Scan(FilePath, FileMask, Recursive);
end;

procedure TDirScanner.Delete(const FileMask: string; Recursive: boolean);
var
  I: longint;
begin
  for I := Count - 1 downto 0 do
    if FileNameMatch(Items[I].FileName, FileMask, Recursive) then
    begin
      FList.Delete(I);
    end;
end;

procedure TDirScanner.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

function TDirScanner.GetCount: longint;
begin
  Result := FList.Count;
end;

function TDirScanner.GetItem(Index:longint): TDirScannerItem;
begin
  Result := TDirScannerItem(FList.Items[Index]);
end;

end.
