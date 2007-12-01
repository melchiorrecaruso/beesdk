{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

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


unit BeeGui_ArchiveTreeView;

interface

uses
  Math,
  Classes,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  SysUtils,
  Controls,
  ComCtrls,
  BeeGui_IconList;

{ TArchiveTreeView }

type
  TArchiveTreeView = class(TTReeView)
  private
    fArchiveFiles: TArchiveList;
    FArchiveFilesUpdate: boolean;
    FArchiveFolders: TArchiveList;
    FArchiveFileDetails: TArchiveListDetails;
  private
    //procedure ClearFiles;
    procedure Expand(Node: TTreeNode);
    function GetArchiveFiles: TArchiveList;
    function GetArchiveFolder: string;
    function AndString(const str1, str2: string): string;
    //function IndexOf(const Name: string): integer;
    procedure QuickSort(List: TArchiveList; L, R: integer);
    function Compare(List: TArchiveList; I1, I2: integer): integer;
    procedure AddFile(ParentNode: TTreeNode; Item: TArchiveListItem);
    procedure AddFolder(ParentNode: TTreeNode; Item: TArchiveListItem);
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
    procedure Initialize;
    procedure Clear;
    // ---
    procedure ExpandAll;
    procedure CollapseAll;
    procedure ExpandOne;
    procedure CollapseOne;
  public
    property ArchiveFiles: TArchiveList Read GetArchiveFiles;
    property ArchiveFileDetails: TArchiveListDetails Read FArchiveFileDetails;
    property ArchiveFolder: string Read GetArchiveFolder;
  end;

implementation

uses
  BeeGui_SysUtils;

{ TArchiveTreeManager }

constructor TArchiveTreeManager.Create(ATreeView: TTreeView);
begin
  inherited Create;
  FArchiveTreeView := ATreeView;
  // Include(Options, tvoAllowMultiSelect);
  // RightClickSelect := False;
  // ---
  FArchiveFilesUpdate := False;
  FArchiveFiles := TArchiveList.Create;
  FArchiveFolders := TArchiveList.Create;
  FArchiveFileDetails := TArchiveListDetails.Create;
  // ---
  FArchiveTreeImages := TIconList.Create(True);
end;

destructor TArchiveTreeManager.Destroy;
begin
  FArchiveFiles.Destroy;
  FArchiveFolders.Destroy;
  FArchiveFileDetails.Destroy;
  // ---
  FArchiveTreeImages.Destroy;
  // ---
  inherited Destroy;
end;

function TArchiveTreeManager.Compare(List: TArchiveList; I1, I2: integer): integer;
begin
  Result := AnsiCompareFileName(TArchiveListItem(List.Items[I1]).FileName,
    TArchiveListItem(List.Items[I2]).FileName);
end;

procedure TArchiveTreeManager.QuickSort(List: TArchiveList; L, R: integer);
var
  I, J, Pivot: longint;
begin
  if (R > L) then
  begin
    repeat
      I := L;
      J := R;
      Pivot := (L + R) div 2;
      repeat
        while Compare(List, I, Pivot) < 0 do
          Inc(I);
        while Compare(List, J, Pivot) > 0 do
          Dec(J);
        if I <= J then
        begin
          List.Exchange(I, J);
          if Pivot = I then
            Pivot := J
          else
          if Pivot = J then
            Pivot := I;
          Inc(I);
          Dec(j);
        end;
      until I > J;
      if L < J then
        QuickSort(List, L, J);
      L := I;
    until I >= R;
  end;
end;

procedure TArchiveTreeManager.Initialize;
var
  I: integer;
  CurrFolderPath: string;
  CurrFolderName: string;
  Node: TArchiveListItem;
begin
  if FArchiveFilesUpdate then
  begin
    FArchiveFilesUpdate := False;

    FArchiveFolders.Clear;
    FArchiveFileDetails.Clear;
    for I := 0 to FArchiveFiles.Count - 1 do
    begin
      CurrFolderName := ExtractFileName(ExcludeTrailingBackSlash(
        TArchiveListItem(FArchiveFiles.Items[I]).FilePath));
      CurrFolderPath := ExtractFilePath(ExcludeTrailingBackSlash(
        TArchiveListItem(FArchiveFiles.Items[I]).FilePath));
      while (Length(CurrFolderName) > 0) do
      begin
        if FArchiveFolders.IndexOf(CurrFolderPath, CurrFolderName) = -1 then
        begin
          Node := TArchiveListItem.Create;
          Node.FileName := CurrFolderName;
          Node.FilePath := CurrFolderPath;
          Node.FilePacked := 0;
          Node.FileSize := 0;
          Node.FileRatio := 0;
          Node.FileTime := DateTimeToFileDate(Now);
          Node.FileAttr := faDirectory;
          Node.FileCRC := 0;
          Node.FilePosition := -1;
          FArchiveFolders.Add(Node);
        end;
        CurrFolderName := ExtractFileName(ExcludeTrailingBackSlash(CurrFolderPath));
        CurrFolderPath := ExtractFilePath(ExcludeTrailingBackSlash(CurrFolderPath));
      end;
      FArchiveFileDetails.Update(TArchiveListItem(FArchiveFiles.Items[I]));
    end;
    QuickSort(FArchiveFiles, 0, FArchiveFiles.Count - 1);
    QuickSort(FArchiveFolders, 0, FArchiveFolders.Count - 1);

    if Assigned(FArchiveTreeView) then
    begin
      FArchiveTreeView.Items.BeginUpdate;
      FArchiveTreeView.Items.Clear;
      for I := 0 to FArchiveFolders.Count - 1 do
      begin
        if Length(TArchiveListItem(FArchiveFolders.Items[I]).FilePath) = 0 then
        begin
          AddFolder(nil, TArchiveListItem(FArchiveFolders.Items[I]));
        end;
      end;
      for I := 0 to FArchiveFiles.Count - 1 do
      begin
        if Length(TArchiveListItem(FArchiveFiles.Items[I]).FilePath) = 0 then
        begin
          AddFile(nil, TArchiveListItem(FArchiveFiles.Items[I]));
        end;
      end;
      FArchiveTreeView.Items.EndUpdate;
    end;
  end;
end;

procedure TArchiveTreeManager.AddFile(ParentNode: TTreeNode;
  Item: TArchiveListItem);
var
  Node: TTreeNode;
  FileInfo: TFileInfo;
begin
  if Assigned(FArchiveTreeView) then
  begin
    with TArchiveListItem(Item) do
    begin
      Node := FArchiveTreeView.Items.AddChild(ParentNode, FileName);
      if Assigned(FArchiveTreeImages) then
      begin
        FileInfo  := FArchiveTreeImages.GetFileInfo(FileName, FileAttr);
        Node.SelectedIndex := FileInfo.FileOpenIconIndex;
        Node.ImageIndex := FileInfo.FileIconIndex;
        Node.StateIndex := FileInfo.FileIconIndex;
        Node.Data := Item;
      end;
    end;
  end;
end;

procedure TArchiveTreeManager.AddFolder(ParentNode: TTreeNode; Item: TArchiveListItem);
var
  Node: TTreeNode;
  FileInfo: TFileInfo;
begin
  if Assigned(FArchiveTreeView) then
  begin
    with TArchiveListItem(Item) do
    begin
      Node := FArchiveTreeView.Items.AddChild(ParentNode, FileName);
      if Assigned(FArchiveTreeImages) then
      begin
        FileInfo  := FArchiveTreeImages.GetFileInfo(FileName, FileAttr);
        Node.SelectedIndex := FileInfo.FileOpenIconIndex;
        Node.ImageIndex := FileInfo.FileIconIndex;
        Node.StateIndex := FileInfo.FileIconIndex;
        Node.Data := Item;
      end;
      Expand(Node);
    end;
  end;
end;

procedure TArchiveTreeManager.Expand(Node: TTreeNode);
var
  I: integer;
  CurrFolder: string;
  Start: TTreeNode;
begin
  Start := Node;
  CurrFolder := IncludeTrailingBackslash(Start.Text);
  if Start.Level > 0 then
  begin
    repeat
      Start := Start.Parent;
      CurrFolder := IncludeTrailingBackslash(Start.Text) + CurrFolder;
    until Start.Level = 0;
  end;
  for I := 0 to FArchiveFolders.Count - 1 do
  begin
    if AnsiCompareText(CurrFolder, TArchiveListItem(
      FArchiveFolders.Items[I]).FilePath) = 0 then
    begin
      AddFolder(Node, TArchiveListItem(FArchiveFolders.Items[I]));
    end;
  end;
  for I := 0 to FArchiveFiles.Count - 1 do
  begin
    if AnsiCompareText(CurrFolder, TArchiveListItem(
      FArchiveFiles.Items[I]).FilePath) = 0 then
    begin
      AddFile(Node, TArchiveListItem(FArchiveFiles.Items[I]));
    end;
  end;
end;

procedure TArchiveTreeManager.Clear;
begin
  if Assigned(FArchiveTreeView) then
  begin
    FArchiveTreeView.Items.BeginUpdate;
    FArchiveTreeView.Items.Clear;
    FArchiveTreeView.Items.EndUpdate;
  end;
  // ---
  FArchiveFilesUpdate := False;
  FArchiveFolders.Clear;
  FArchiveFiles.Clear;
end;

function TArchiveTreeManager.GetArchiveFiles: TArchiveList;
begin
  FArchiveFilesUpdate := True;
  Result := FArchiveFiles;
  Result.Clear;
end;

function TArchiveTreeManager.AndString(const str1, str2: string): string;
var
  J: integer;
begin
  Result := '';
  for J := 1 to Min(Length(str1), Length(str2)) do
    if AnsiCompareFileName(str1[J], str2[J]) = 0 then
      Result := Result + str1[J]
    else
    begin
      if not (AnsiCompareFileName(IncludeTrailingBackslash(Result), Result) = 0) then
        Result := ExtractFilePath(Result);
      Exit;
    end;
end;

function TArchiveTreeManager.GetArchiveFolder: string;
var
  I: integer;
begin
  Result := '';
  if Assigned(FArchiveTreeView) then
  begin
    I := 0;
    with FArchiveTreeView do
    begin
      while I < Items.Count do
      begin
        if Items[I].Selected then
        begin
          Result := TArchiveListItem(Items[I].Data).FilePath;
          Break;
        end;
        Inc(I);
      end;
      while I < Items.Count do
      begin
        if Items[I].Selected then
        begin
          Result := AndString(Result, TArchiveListItem(Items[I].Data).FilePath);
        end;
        Inc(I);
      end;
    end;
  end;
end;

procedure TArchiveTreeManager.ExpandAll;
var
  I: integer;
begin
  if Assigned(FArchiveTreeView) then
  begin
    for I := 0 to FArchiveTreeView.Items.Count - 1 do
    begin
      if FArchiveTreeView.Items[I].HasChildren then
        FArchiveTreeView.Items[I].Expanded := True;
    end;
  end;
end;

procedure TArchiveTreeManager.CollapseAll;
var
  I: integer;
begin
  if Assigned(FArchiveTreeView) then
  begin
    for I := 0 to FArchiveTreeView.Items.Count - 1 do
    begin
      if FArchiveTreeView.Items[I].HasChildren then
        FArchiveTreeView.Items[I].Expanded := False;
    end;
  end;
end;

procedure TArchiveTreeManager.ExpandOne;
var
  I: integer;
begin
  if Assigned(FArchiveTreeView) then
  begin
    for I := FArchiveTreeView.Items.Count - 1 downto 0 do
    begin
      if FArchiveTreeView.Items[I].Expanded = False then
      begin
        if not Assigned(FArchiveTreeView.Items[I].Parent) then
          FArchiveTreeView.Items[I].Expanded := True
        else
        if FArchiveTreeView.Items[I].Parent.Expanded then
          FArchiveTreeView.Items[I].Expanded := True;
      end;
    end;
  end;
end;

procedure TArchiveTreeManager.CollapseOne;
var
  I, J:  integer;
  Check: boolean;
begin
  if Assigned(FArchiveTreeView) then
  begin
    for I := 0 to FArchiveTreeView.Items.Count - 1 do
    begin
      Check := True;
      if FArchiveTreeView.Items[I].Expanded = True then
      begin
        for J := 0 to FArchiveTreeView.Items[I].Count - 1 do
        begin
          if FArchiveTreeView.Items[I].Item[J].Expanded then
          begin
            Check := False;
            Break;
          end;
        end;
        if Check then
        begin
          FArchiveTreeView.Items[I].Expanded := False;
        end;
      end;
    end;
  end;
end;

end.
