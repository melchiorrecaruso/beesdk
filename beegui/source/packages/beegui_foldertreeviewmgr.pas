{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

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

    TFolderTreeViewMgr class.

  Modifyed:

}

unit BeeGui_FolderTreeViewMgr;

{$I compiler.inc}

interface

uses
  Classes,
  Controls,
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

  { TFolderTreeViewMgr }

  TFolderTreeViewMgr = class(TComponent)
  private
    FFolderTree: TTreeView;
    FFolderIcons: TIconList;
    FFolderName: string;
    FFolderEdit: TEdit;
    procedure SetFolderName(Value: string);
    procedure SetFolderEdit(Value: TEdit);
    procedure SetFolderTree(Value: TTreeView);
    procedure SetFolderIcons(Value: TIconList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Expand(Node: TTreeNode);
    procedure Initialize;
    procedure DblClick;
    procedure Click;
  published
    property FolderName: string read FFolderName write SetFolderName;
    property FolderEdit: TEdit read FFolderEdit write SetFolderEdit default nil;
    property FolderTree: TTreeView read FFolderTree write SetFolderTree default nil;
    property FolderIcons: TIconList read FFolderIcons write SetFolderIcons default nil;
  end;
  
 { Register }

  procedure Register;

implementation

  { TFolderTreeViewMgr }

  constructor TFolderTreeViewMgr.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FFolderIcons := nil;
    FFolderTree  := nil;
    FFolderEdit  := nil;
  end;

  procedure TFolderTreeViewMgr.Initialize;
  var
    I: integer;
    Drives: TStringList;
    Node: TTreeNode;
  begin
    if Assigned(FFolderTree) then
    begin
      Drives := TStringList.Create;
      FFolderTree.Items.BeginUpdate;
      try
        GetDrivers(Drives);
        FFolderTree.Items.Clear;
        for I := 0 to Drives.Count - 1 do
        begin
          Node := FFolderTree.Items.Add(FFolderTree.Items.GetFirstNode, Drives.Strings[I]);
          if Assigned(FFolderIcons) then
          begin
            Node.SelectedIndex := FFolderIcons.FileIcon('.@harddrive', faDirectory);
            Node.ImageIndex := Node.SelectedIndex;
            Node.StateIndex := Node.SelectedIndex;
           end;
          Node := FFolderTree.Items.AddChild(Node, '');
          Node.SelectedIndex := - 1;
          Node.ImageIndex := - 1;
          Node.StateIndex := - 1;
        end;
      finally
        FFolderTree.Items.EndUpdate;
        Drives.Free;
      end;
    end;
  end;

  destructor TFolderTreeViewMgr.Destroy;
  begin
    FFolderIcons := nil;
    FFolderTree  := nil;
    FFolderEdit  := nil;
    inherited Destroy;
  end;
  
  procedure TFolderTreeViewMgr.SetFolderName(Value: string);
  var
    i: integer;
    iFolderName: string;
    Start: TTreeNode;
  begin
    if Assigned(FFolderTree) then
    begin
      if DirectoryExists(IncludeTrailingBackSlash(Value)) then
      begin
        FFolderName := IncludeTrailingBackSlash(Value);
        SetCurrentDir(FFolderName);

        i := 0;
        while i < FFolderTree.Items.Count do
        begin
          Start := FFolderTree.Items[i];
          iFolderName  := IncludeTrailingBackslash(Start.Text);
          if Start.Level > 0 then
            repeat
              Start := Start.Parent;
              iFolderName  := IncludeTrailingBackslash(Start.Text) + iFolderName;
            until Start.Level = 0;

            if FileNamePos(iFolderName, FFolderName) > 0 then
            begin
              FFolderTree.Items[i].Expand(False);
              if CompareFileName(iFolderName, FFolderName) = 0 then
              begin
                i := FFolderTree.Items.Count
              end;
            end;
          Inc (i);
        end;
      end;
    end;
  end;

  procedure TFolderTreeViewMgr.SetFolderEdit(Value: TEdit);
  begin
    if Assigned(Value) then
    begin
      FFolderEdit := Value;
    end;
  end;
  
  procedure TFolderTreeViewMgr.SetFolderTree(Value: TTreeView);
  begin
    if Assigned(Value) then
    begin
      FFolderTree := Value;
    end;
  end;
  
  procedure TFolderTreeViewMgr.SetFolderIcons(Value: TIconList);
  begin
    if Assigned(Value) then
    begin
      FFolderIcons := Value;
    end;
  end;

  procedure TFolderTreeViewMgr.Expand(Node: TTreeNode);
  var
    Start, TMP: TTreeNode;
    Folders: TStringList;
    Folder: string;
    I: integer;
  begin
    if Assigned(FFolderTree) then
    begin
      Start := Node;
      FFolderTree.Selected := Node;

      if Start.HasChildren = False then Exit;
      if Start.GetFirstChild.ImageIndex <> -1 then Exit;

      Folder := IncludeTrailingBackslash(Start.Text);
      if Start.Level > 0 then
        repeat
          Start := Start.Parent;
          Folder := IncludeTrailingBackslash(Start.Text) + Folder;
        until Start.Level = 0;

      FFolderTree.Items.BeginUpdate;
      Folders := TStringList.Create;
      try
        if GetDirectories(Folder, Folders) then
          if Assigned(FFolderEdit) then
            FFolderEdit.Text := Folder;

        Start := Node;
        while Start.HasChildren do Start.GetFirstChild.Delete;

        for I := 0 to Folders.Count - 1 do
        begin
          TMP := FFolderTree.Items.AddChild(Start, Folders.Strings[I]);
          if Assigned(FFolderIcons) then
          begin
            TMP.ImageIndex := FFolderIcons.FileIcon('.@folderclose', faDirectory);
            TMP.SelectedIndex := FFolderIcons.FileIcon('.@folderopen', faDirectory);
            TMP.StateIndex := TMP.ImageIndex;
          end;
          TMP := FFolderTree.Items.AddChild(TMP, '');
          TMP.SelectedIndex := - 1;
          TMP.ImageIndex := - 1;
          TMP.StateIndex := - 1;
        end;
      finally
        Folders.Free;
      end;
      FFolderTree.Items.EndUpdate;
    end;
  end;

  procedure TFolderTreeViewMgr.DblClick;
  begin
    if Assigned(FFolderTree) then
    begin
      if FFolderTree.Selected <> nil then
        FFolderTree.Selected.Expand(False);
    end;
  end;

  procedure TFolderTreeViewMgr.Click;
  var
    Start: TTreeNode;
    Folder: string;
  begin
    if Assigned(FFolderTree) then
    begin
      if FFolderTree.Selected <> nil then
      begin
        Start := FFolderTree.Selected;
        Folder := IncludeTrailingBackslash(Start.Text);
        if Start.Level > 0 then
          repeat
            Start := Start.Parent;
            Folder := IncludeTrailingBackslash(Start.Text) + Folder;
          until Start.Level = 0;

        if SetCurrentDir(Folder) then
          if Assigned(FFolderEdit) then
            FFolderEdit.Text := Folder;
       end;
    end;
  end;
  
  { Register }

  procedure Register;
  begin
    RegisterComponents('BeePackage', [TFolderTreeViewMgr]);
  end;
  
initialization

  {$I beegui_foldertreeviewmgr.lrs }

end.

