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

{   Contains:

    TFolderTreeView class.

    Modifyed:

    v1.0.3 build 0020 - 2006/11/25 by Melchiorre Caruso.
}

unit BeeGui_FolderTreeView;

interface

uses
  Classes,
  ComCtrls,
  Dialogs,
  SysUtils,
  StdCtrls,
  LResources,
  /// ---
  Bee_Common,
  /// ---
  BeeGui_IconList,
  BeeGui_SysUtils;

type
  TFolderTreeView = class(TTreeView)
  private
    FFolderEdit: TEdit;
    FCurrPath: string;
    function SetPath: boolean;
    procedure SetFolderEdit(Value: TEdit);
    procedure SetCurrPath(Value: string);
    procedure OnTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure OnTreeDblClick(Sender: TObject);
    procedure OnTreeClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize;
  published
    property CurrDir: string read FCurrPath write SetCurrPath;
    property FolderEdit: TEdit read FFolderEdit write SetFolderEdit default nil;
  end;
  
 { Register }

  procedure Register;

implementation

  { TFolderTreeView }

  constructor TFolderTreeView.Create(AOwner: TComponent);  
  begin
    inherited Create(AOwner);
  end;

  destructor TFolderTreeView.Destroy;
  begin
    inherited Destroy;
  end;
  
  procedure TFolderTreeView.Initialize;
  var
    I: integer;
    Drives: TStringList;
    Node: TTreeNode;
  begin
    Drives := TStringList.Create;
    Items.BeginUpdate;
    try
      Items.Clear;
      GetDrivers(Drives);
      for I := 0 to Drives.Count - 1 do
      begin
        Node := Items.Add(Items.GetFirstNode, Drives.Strings[I]);
        if Assigned(Images) and (Images.ClassType = TIconList) then
        begin
          Node.SelectedIndex := TIconList(Images).GetIconIndex('harddrive');
          Node.ImageIndex := Node.SelectedIndex;
          Node.StateIndex := Node.SelectedIndex;
        end;
        Node := Items.AddChild(Node, '');
        Node.SelectedIndex := - 1;
        Node.ImageIndex := - 1;
        Node.StateIndex := - 1;
      end;
    finally
      Items.EndUpdate;
      Drives.Free;
    end;
    OnExpanding := OnTreeExpanding;
    OnDblClick := OnTreeDblClick;
    OnClick := OnTreeClick;
  end;
  
  function TFolderTreeView.SetPath: boolean;
  var
    I: integer;
    cDir, tDir: string;
    Start: TTreeNode;
  begin
    Result := False;

    SetCurrentDir(FCurrPath);
    cDir := IncludeTrailingBackslash(GetCurrentDir);

    I := 0;
    while I <  Items.Count do
    begin
      Start := Items[I];
      tDir  := IncludeTrailingBackslash(Start.Text);
      if Start.Level > 0 then
        repeat
          Start := Start.Parent;
          tDir  := IncludeTrailingBackslash(Start.Text) + tDir;
        until Start.Level = 0;

      if FileNamePos(tDir, cDir) > 0 then
      begin
        Items[I].Expand(False);
        if AnsiCompareText(tDir, cDir) = 0 then
        begin
          Result := True;
          I := Items.Count
        end;
      end;
      Inc (I);
    end;
  end;

  procedure TFolderTreeView.SetCurrPath(Value: string);
  begin
    FCurrPath := Value;
    SetPath;
  end;

  procedure TFolderTreeView.SetFolderEdit(Value: TEdit);
  begin
    FFolderEdit := Value;
  end;

  procedure TFolderTreeView.OnTreeExpanding;
  var
    Start, TMP: TTreeNode;
    Folders: TStringList;
    Path: string;
    I: integer;
  begin
    Start := Node;
    Selected := Node;

    if Start.HasChildren = False then Exit;
    if Start.GetFirstChild.ImageIndex <> -1 then Exit;

    Path := IncludeTrailingBackslash(Start.Text);
    if Start.Level > 0 then
      repeat
        Start := Start.Parent;
        Path := IncludeTrailingBackslash(Start.Text) + Path;
      until Start.Level = 0;

    Items.BeginUpdate;
    Folders := TStringList.Create;
    try
      if GetDirectories(Path, Folders) then
        if Assigned(FFolderEdit) then
          FFolderEdit.Text := Path;

      Start := Node;
      while Start.HasChildren do Start.GetFirstChild.Delete;

      for I := 0 to Folders.Count - 1 do
      begin
        TMP := Items.AddChild(Start, Folders.Strings[I]);
        if Assigned(Images) and (Images.ClassType = TIconList) then
        begin
          TMP.ImageIndex := TIconList(Images).GetIconIndex('folderclose');
          TMP.SelectedIndex := TIconList(Images).GetIconIndex('folderopen');
          TMP.StateIndex := TMP.ImageIndex;
        end;
        TMP := Items.AddChild(TMP, '');
        TMP.SelectedIndex := - 1;
        TMP.ImageIndex := - 1;
        TMP.StateIndex := - 1;
      end;
    finally
      Folders.Free;
    end;
    Items.EndUpdate;
  end;

  procedure TFolderTreeView.OnTreeDblClick;
  begin
    if Selected <> nil then Selected.Expand(False);
  end;

  procedure TFolderTreeView.OnTreeClick;
  var
    Start: TTreeNode;
    Path: string;
  begin
    if Selected <> nil then
    begin
      Start := Selected;
      Path := IncludeTrailingBackslash(Start.Text);
      if Start.Level > 0 then
        repeat
          Start := Start.Parent;
          Path := IncludeTrailingBackslash(Start.Text) + Path;
        until Start.Level = 0;

      if SetCurrentDir(Path) then
        if Assigned(FFolderEdit) then
          FFolderEdit.Text := Path;
    end;
  end;
  
  { Register }

  procedure Register;
  begin
    RegisterComponents('BeeGui', [TFolderTreeView]);
  end;
  
initialization

  {$I beegui_foldertreeview.lrs }  

end.

