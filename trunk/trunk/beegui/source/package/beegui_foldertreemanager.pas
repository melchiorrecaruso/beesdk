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

unit BeeGui_FolderTreeManager;

{$I Compiler.inc}

interface

uses
  Classes,
  ComCtrls,
  Dialogs,
  SysUtils,
  StdCtrls,
  /// ---
  BeeGui_IconList,
  BeeGui_SysUtils;

type
  TFolderTreeManager = class
  private
    FFolderEdit: TEdit;
    FCurrFolder: string;
    FFolderTree: TTreeView;
    function SetFolder: boolean;
    procedure SetFolderEdit(Value: TEdit);
    procedure SetCurrFolder(Value: string);
    procedure OnTreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: boolean);
    procedure OnTreeDblClick(Sender: TObject);
    procedure OnTreeClick(Sender: TObject);
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
    procedure Initialize;
  published
    property CurrentFolder: string Read FCurrFolder Write SetCurrFolder;
    property FolderEdit: TEdit Read FFolderEdit Write SetFolderEdit default nil;
  end;

implementation

{ TFolderTreeManager }

constructor TFolderTreeManager.Create(ATreeView: TTreeView);
begin
  inherited Create;
  FFolderTree := ATreeView;
  if Assigned(FFolderTree) then
  begin
    FFolderTree.Images := TIconList.Create(True);
  end;
end;

destructor TFolderTreeManager.Destroy;
begin
  if Assigned(FFolderTree) then
  begin
    FFolderTree.Images.Free;
    FFolderTree := nil;
  end;
  inherited Destroy;
end;

procedure TFolderTreeManager.Initialize;
var
  I: integer;
  Node: TTreeNode;
  FileInfo: TFileInfo;
  Drives: TStringList;
begin
  if Assigned(FFolderTree) then
  begin
    with FFolderTree do
    begin
      Items.BeginUpdate;
      Drives := TStringList.Create;
      try
        Items.Clear;
        GetDrivers(Drives);
        for I := 0 to Drives.Count - 1 do
        begin
          Node := Items.Add(Items.GetFirstNode, Drives.Strings[I]);
          if Assigned(Images) and (Images.ClassType = TIconList) then
          begin
            FileInfo := TIconList(Images).GetFileInfo(Drives.Strings[I], faDirectory);
            Node.SelectedIndex := FileInfo.FileOpenIconIndex;
            Node.ImageIndex := FileInfo.FileIconIndex;
            Node.StateIndex := FileInfo.FileIconIndex;
          end;
          Node := Items.AddChild(Node, '');
          Node.SelectedIndex := -1;
          Node.ImageIndex := -1;
          Node.StateIndex := -1;
        end;
      finally
        Items.EndUpdate;
        Drives.Free;
      end;
      OnExpanding := OnTreeExpanding;
      OnDblClick := OnTreeDblClick;
      OnClick := OnTreeClick;
    end;
  end;
end;

function TFolderTreeManager.SetFolder: boolean;
var
  I: integer;
  cDir, tDir: string;
  Start: TTreeNode;
begin
  Result := False;
  if Assigned(FFolderTree) then
  begin
    SetCurrentDir(FCurrFolder);
    cDir := IncludeTrailingBackslash(GetCurrentDir);

    I := 0;
    while I < FFolderTree.Items.Count do
    begin
      Start := FFolderTree.Items[I];
      tDir  := IncludeTrailingBackslash(Start.Text);

      if Start.Level > 0 then
        repeat
          Start := Start.Parent;
          tDir  := IncludeTrailingBackslash(Start.Text) + tDir;
        until Start.Level = 0;

      if AnsiPosText(tDir, cDir) > 0 then
      begin
        FFolderTree.Items[I].Expand(False);
        if AnsiCompareText(tDir, cDir) = 0 then
        begin
          Result := True;
          I := FFolderTree.Items.Count;
        end;
      end;
      Inc(I);
    end;
  end;
end;

procedure TFolderTreeManager.SetCurrFolder(Value: string);
begin
  FCurrFolder := Value;
  SetFolder;
end;

procedure TFolderTreeManager.SetFolderEdit(Value: TEdit);
begin
  FFolderEdit := Value;
end;

procedure TFolderTreeManager.OnTreeExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: boolean);
var
  Start, TMP: TTreeNode;
  Folders: TStringList;
  FileInfo: TFileInfo;
  Path: string;
  I: integer;
begin
  if Assigned(FFolderTree) then
  begin
    Start := Node;
    FFolderTree.Selected := Node;

    if Start.HasChildren = False then
      Exit;
    if Start.GetFirstChild.ImageIndex <> -1 then
      Exit;

    Path := IncludeTrailingBackslash(Start.Text);
    if Start.Level > 0 then
      repeat
        Start := Start.Parent;
        Path  := IncludeTrailingBackslash(Start.Text) + Path;
      until Start.Level = 0;

    FFolderTree.Items.BeginUpdate;
    Folders := TStringList.Create;
    try
      if GetDirectories(Path, Folders) then
        if Assigned(FFolderEdit) then
          FFolderEdit.Text := Path;

      Start := Node;
      while Start.HasChildren do
        Start.GetFirstChild.Delete;

      for I := 0 to Folders.Count - 1 do
        with FFolderTree do
        begin
          TMP := Items.AddChild(Start, Folders.Strings[I]);
          if Assigned(Images) and (Images.ClassType = TIconList) then
          begin
            FileInfo := TIconList(Images).GetFileInfo(Folders.Strings[I], faDirectory);
            TMP.SelectedIndex := FileInfo.FileOpenIconIndex;
            TMP.ImageIndex := FileInfo.FileIconIndex;
            TMP.StateIndex := FileInfo.FileIconIndex;
          end;
          TMP := Items.AddChild(TMP, '');
          TMP.SelectedIndex := -1;
          TMP.ImageIndex := -1;
          TMP.StateIndex := -1;
        end;

    finally
      Folders.Free;
    end;
    FFolderTree.Items.EndUpdate;
  end;
end;

procedure TFolderTreeManager.OnTreeDblClick(Sender: TObject);
begin
  if Assigned(FFolderTree) and Assigned(FFolderTree.Selected) then
  begin
    FFolderTree.Selected.Expand(False);
  end;
end;

procedure TFolderTreeManager.OnTreeClick(Sender: TObject);
var
  Start: TTreeNode;
  Path:  string;
begin
  if Assigned(FFolderTree) and Assigned(FFolderTree.Selected) then
  begin
    Start := FFolderTree.Selected;
    Path  := IncludeTrailingBackslash(Start.Text);
    if Start.Level > 0 then
    begin
      repeat
        Start := Start.Parent;
        Path  := IncludeTrailingBackslash(Start.Text) + Path;
      until Start.Level = 0;
    end;
    if SetCurrentDir(Path) then
    begin
      if Assigned(FFolderEdit) then
        FFolderEdit.Text := Path;
    end;
  end;
end;

end.
