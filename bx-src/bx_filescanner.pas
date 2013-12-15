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

    v1.0 build 2153 - 2013.12.15 by Melchiorre Caruso.

  Modifyed:

}

unit bx_FileScanner;

{$I bx_compiler.inc}

interface

uses
  Classes,
  DateUtils,
  SysUtils;

type
  { PFileScannerItem }

  PFileScannerItem = ^TFileScannerItem;

  { TFileScannerItem }

  TFileScannerItem = packed record
    ItemName: string;
    ItemSize: int64;
    ItemTime: int64;
    ItemAttr: longint;
  end;

  { TFileScanner }

  TFileScanner = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: longint): PFileScannerItem;
    function AddItem(const RecPath: string; const Rec: TSearchRec): PFileScannerItem;
    procedure Scan(FileMask: string; Recursive: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const FileMask: string; Recursive: boolean);
    procedure Clear;
    procedure Delete(const FileMask: string; Recursive: boolean);
    procedure Sort(Compare: TListSortCompare);
  public
    property Count: integer read GetCount;
    property Items[Index: longint]: PFileScannerItem read GetItem;
  end;

implementation

uses
  bx_Common;

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
    FreeMem(FList[I]);
  FList.Clear;
end;

function TFileScanner.AddItem(const RecPath: string; const Rec: TSearchRec): PFileScannerItem;
begin
  GetMem(Result, SizeOf(TFileScannerItem));
  Result^.ItemName := RecPath + Rec.Name;
  Result^.ItemSize := Rec.Size;
  Result^.ItemTime := DateTimeToUnix(FileDateToDateTime(Rec.Time));
  Result^.ItemAttr := Rec.Attr;
end;

procedure TFileScanner.Scan(FileMask: string; Recursive: boolean);
var
  RecName: string;
  RecPath: string;
  Rec: TSearchRec;
  Error: longint;
begin
  // directory and recursive mode ...
  if FileMask <> '' then
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
        FList.Add(AddItem(RecPath, Rec));

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
    if FileNameMatch(Items[I]^.ItemName, FileMask, Recursive) then
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

function TFileScanner.GetItem(Index:longint): PFileScannerItem;
begin
  Result := FList.Items[Index];
end;

end.

