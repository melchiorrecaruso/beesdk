{
    Copyright (c) 2014 Melchiorre Caruso

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

{   Contains:

    TArchiveListItem    class;
    TArchiveList        class;
    TArchiveListDetails class;
    TArchiveTreeView    class.
  
    Modifyed:

    v1.0.0 build 0020 - 2006.11.25 by Melchiorre Caruso.
}

unit bxm_archivetreeviewmgr;

{$I bxm_compiler.inc}

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
  GraphType,
  LResources;

type
{ TArchiveListItem }

  TArchiveListItem = class
  public
    FileName: string;
    FilePath: string;
    FileType: string;
    FileSize: integer;
    FilePacked: integer;
    FileRatio: integer;
    FileAttr: integer;
    FileTime: integer;
    FileComm: string;
    FileCrc: cardinal;
    FileMethod: string;
    FileVersion: string;
    FilePassword: string;
    FilePosition: integer;
    FileIconIndex: integer;
  end;

  { TArchiveList }

  TArchiveList = class(TList)
  public
    procedure Clear; override;
    destructor Destroy; override;
    function IndexOf(const Path: string; const Name: string): integer;
  end;

  { TArchiveListDetails }

  TArchiveListDetails = class
  public
    FilesCount: integer;
    FilesSize: integer;
    FilesPacked: integer;
    FilesCrypted: integer;
    DirectoriesCount: integer;
    Version: extended;
    LastTime: integer;
  public
    procedure Clear;
    procedure Update(Item: TArchiveListItem);
  end;
  
  { TArchiveTreeView }

  TArchiveTreeView = class(TTreeView)
  private
    FArchiveFiles: TArchiveList;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize;
    procedure Clear;
    // ---
    procedure ExpandAll;
    procedure CollapseAll;
    procedure ExpandOne;
    procedure CollapseOne;
  public
    property ArchiveFiles: TArchiveList read GetArchiveFiles;
    property ArchiveFileDetails: TArchiveListDetails read FArchiveFileDetails;
    property ArchiveFolder: string read GetArchiveFolder;
  end;
  
 { Register }

  procedure Register;

implementation

uses
  bxm_iconlist;

  { TArchiveListDetails }
  
  procedure TArchiveListDetails.Clear;
  begin
    FilesCount := 0;
    FilesSize := 0;
    FilesPacked := 0;
    FilesCrypted := 0;
    DirectoriesCount := 0;
    Version := 0;
    LastTime := 0;
  end;

  procedure TArchiveListDetails.Update(Item: TArchiveListItem);
  begin
    if (Item.FileAttr and faDirectory) = 0 then
    begin
      Inc(FilesCount);
      Inc(FilesSize, Item.FileSize);
      Inc(FilesPacked, Item.FilePacked);
      if CompareText(Item.FilePassword, 'Yes') = 0 then Inc(FilesCrypted);
      LastTime := Max(LastTime, Item.FileTime);
      try
        Version := Max(Version, StrToCurr(Item.FileVersion));
      except
        Version := 0;
      end;
    end
    else
      Inc(DirectoriesCount);
  end;
  
  { TArchiveList }

  destructor TArchiveList.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;

  procedure TArchiveList.Clear;
  var
    I: integer;
  begin
    for I := Count - 1 downto 0 do
    begin
      TArchiveListItem(Items[I]).Destroy;
    end;
    inherited Clear;
  end;
  
  function TArchiveList.IndexOf(const Path: string; const Name: string): integer;
  var
    I: integer;
  begin
    Result := -1;
    for I := 0 to Count -1 do
    begin
      if AnsiCompareFileName(TArchiveListItem(Items[I]).FileName, Name) = 0 then
        if AnsiCompareFileName(TArchiveListItem(Items[I]).FilePath, Path) = 0 then
        begin
          Result := I;
          Break;
        end;
    end;
  end;

  { TArchiveTreeView }

  constructor TArchiveTreeView.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    // Include(Options, tvoAllowMultiselect);
    RightClickSelect := False;
    // ---
    FArchiveFilesUpdate := False;
    FArchiveFiles := TArchiveList.Create;
    FArchiveFolders := TArchiveList.Create;
    FArchiveFileDetails := TArchiveListDetails.Create;
  end;
  
  destructor TArchiveTreeView.Destroy;
  begin
    FArchiveFiles.Destroy;
    FArchiveFolders.Destroy;
    FArchiveFileDetails.Destroy;
    // ---
    inherited Destroy;
  end;
  
  function TArchiveTreeView.Compare(List: TArchiveList; I1, I2: integer): integer;
  begin
    Result := AnsiCompareFileName
      (TArchiveListItem(List.Items[I1]).FileName,
       TArchiveListItem(List.Items[I2]).FileName);
  end;

  procedure TArchiveTreeView.QuickSort(List: TArchiveList; L, R: integer);
  var
    I,J, Pivot : Longint;
  begin
    if (R > L) then
    begin
      repeat
        I := L;
        J := R;
        Pivot := (L + R) div 2;
        repeat
          while Compare(List, I, Pivot) < 0 do Inc(I);
          while Compare(List, J, Pivot) > 0 do Dec(J);
          if I <= J then
          begin
            List.Exchange (I, J);
            if Pivot = I then
              Pivot := J
            else
              if Pivot = J then
                Pivot := I;
          Inc(I);
          Dec(j);
          end;
      until I > J;
      if L < J then QuickSort(List, L, J);
      L := I;
      until I >= R;
    end;
  end;

  procedure TArchiveTreeView.Initialize;
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
        CurrFolderName := ExtractFileName(ExcludeTrailingBackSlash(TArchiveListItem(FArchiveFiles.Items[I]).FilePath));
        CurrFolderPath := ExtractFilePath(ExcludeTrailingBackSlash(TArchiveListItem(FArchiveFiles.Items[I]).FilePath));
        while (Length(CurrFolderName) > 0) do
        begin
          if FArchiveFolders.IndexOf(CurrFolderPath, CurrFolderName) = -1 then
          begin
            Node := TArchiveListItem.Create;
            Node.FileName := CurrFolderName;
            Node.FilePath := CurrFolderPath;
            Node.FilePacked   := 0;
            Node.FileSize     := 0;
            Node.FileRatio    := 0;
            Node.FileTime     := DateTimeToFileDate(Now);
            Node.FileAttr     := faDirectory;
            Node.FileCRC      := 0;
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
      
      Items.BeginUpdate;
      Items.Clear;
      for I := 0 to FArchiveFolders.Count - 1 do
      begin
        if Length(TArchiveListItem(FArchiveFolders.Items[I]).FilePath) = 0 then
          AddFolder(nil, TArchiveListItem(FArchiveFolders.Items[I]));
      end;
      for I := 0 to FArchiveFiles.Count - 1 do
      begin
        if Length(TArchiveListItem(FArchiveFiles.Items[I]).FilePath) = 0 then
          AddFile(nil, TArchiveListItem(FArchiveFiles.Items[I]));
      end;
      Items.EndUpdate;
    end;
  end;
  
  procedure TArchiveTreeView.AddFile(ParentNode: TTreeNode; Item: TArchiveListItem);
  var
    Node: TTreeNode;
  begin
    with Item do
    begin
      Node := Items.AddChild(ParentNode, FileName);
      // if Assigned(Images) and (Images.ClassType = TIconList) then
      begin
        Node.SelectedIndex := 0;
        Node.ImageIndex    := 0;
      end;
      Node.Data := Item;
    end;
  end;

  procedure TArchiveTreeView.AddFolder(ParentNode: TTreeNode; Item: TArchiveListItem);
  var
    Node: TTreeNode;
  begin
    with Item do
    begin
      Node := Items.AddChild(ParentNode, FileName);
      // if Assigned(Images) and (Images.ClassType = TIconList) then
      begin
        Node.SelectedIndex := 7;
        Node.ImageIndex    := 6;
      end;
      Node.Data := Item;
    end;
    Expand(Node);
  end;

  procedure TArchiveTreeView.Expand(Node: TTreeNode);
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

    for I := 0 to FArchiveFolders.Count -1 do
    begin
      if AnsiCompareText(CurrFolder, TArchiveListItem(FArchiveFolders.Items[I]).FilePath) = 0 then
      begin
        AddFolder(Node, TArchiveListItem(FArchiveFolders.Items[I]));
      end;
    end;

    for I := 0 to FArchiveFiles.Count -1 do
    begin
      if AnsiCompareText(CurrFolder, TArchiveListItem(FArchiveFiles.Items[I]).FilePath) = 0 then
      begin
        AddFile(Node, TArchiveListItem(FArchiveFiles.Items[I]));
      end;
    end;
  end;

  procedure TArchiveTreeView.Clear;
  begin
    FArchiveFilesUpdate := False;
    FArchiveFolders.Clear;
    FArchiveFiles.Clear;
    BeginUpdate;
    Items.Clear;
    EndUpdate;
  end;

  function TArchiveTreeView.GetArchiveFiles: TArchiveList;
  begin
    FArchiveFilesUpdate :=  True;
    Result := FArchiveFiles;
    Result.Clear;
  end;
  
  function TArchiveTreeView.AndString(const str1, str2: string): string;
  var
    J : integer;
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
  
  function TArchiveTreeView.GetArchiveFolder: string;
  var
    I: integer;
  begin
    Result := '';  I := 0;
    while I < Items.Count do
    begin
      if Items[I].MultiSelected then
      begin
        Result := TArchiveListItem(Items[I].Data).FilePath;
        Break;
      end;
      Inc(I);
    end;
    while I < Items.Count do
    begin
      if Items[I].MultiSelected then
      begin
        Result := AndString(Result, TArchiveListItem(Items[I].Data).FilePath);
      end;
      Inc(I);
    end;
  end;
  
  procedure TArchiveTreeView.ExpandAll;
  var
    I: integer;
  begin
    for I := 0 to Items.Count -1 do
    begin
      if Items[I].HasChildren then
      begin
        Items[I].Expanded := True;
      end;
    end;
  end;
  
  procedure TArchiveTreeView.CollapseAll;
  var
    I: integer;
  begin
    for I := 0 to Items.Count -1 do
    begin
      if Items[I].HasChildren then
      begin
        Items[I].Expanded := False;
      end;
    end;
  end;
  
  procedure TArchiveTreeView.ExpandOne;
  var
    I: integer;
  begin
    for I := Items.Count -1 downto 0 do
    begin
      if Items[I].Expanded = False then
      begin
        if not Assigned(Items[I].Parent) then
          Items[I].Expanded := True
        else
          if Items[I].Parent.Expanded then
            Items[I].Expanded := True;
      end;
    end;
  end;
  
  procedure TArchiveTreeView.CollapseOne;
  var
    I, J: integer;
    Check: boolean;
  begin
    for I := 0 to Items.Count -1 do
    begin
      Check := True;
      if Items[I].Expanded = True then
      begin
        for J := 0 to Items[I].Count -1 do
        begin
          if Items[I].Items[J].Expanded then
          begin
            Check := False;
            Break;
          end;
        end;
        if Check then
        begin
          Items[I].Expanded := False;
        end;
      end;
    end;
  end;
  
  { Register }

  procedure Register;
  begin
    RegisterComponents('BX Manager', [TArchiveTreeView]);
  end;
  
initialization

  {$I bxm_archivetreeviewmgr.lrs }

end.