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

    BeeGui Select-DeSelect form.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;
  
    v1.0.3 build 0360 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_SeDeseFrm;

interface

uses
  Forms,
  Buttons,
  Classes,
  Dialogs,
  StdCtrls,
  Controls,
  Sysutils,
  ComCtrls,
  LResources,
  XMLPropStorage,
  // ---
  BeeGui_SysUtils,
  BeeGui_ArchiveTreeView;

type
  { TSeDeSeFrm }

  TSeDeSeFrm = class(TForm)
    SeDeseFrm_Storage: TXMLPropStorage;
    SeDeseFrm_Mask_: TLabel;
    SeDeseFrm_Mask: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private    
    procedure SeDeSeItems(Tree: TArchiveTreeView; Value: boolean);
  public
    procedure SelectAll(Tree: TArchiveTreeView);
    procedure SelectItems(Tree: TArchiveTreeView);
    procedure DeSelectAll(Tree: TArchiveTreeView);
    procedure DeselectItems(Tree: TArchiveTreeView);
    procedure InvertSelection(Tree: TArchiveTreeView);
  end;

implementation

  { TSeDeseFrm class }

  procedure TSeDeseFrm.SeDeSeItems(Tree: TArchiveTreeView; Value: boolean);
  var
    I: integer;
    Ext, iExt: string;
    Name, iName: string;
    Folder, iFolder: string;
    Notify: TNotifyEvent;
    Node: TTreeNode;
  begin
    Notify := Tree.OnSelectionChanged;
    Tree.OnSelectionChanged := nil;
    try
      Ext := ExtractFileExt(SeDeseFrm_Mask.Text);
      Name := ChangeFileExt(ExtractFileName(SeDeseFrm_Mask.Text), '');
      Folder := ExtractFilePath(SeDeseFrm_Mask.Text);
          
      for I := 0 to Tree.Items.Count -1 do
      begin
        Node := Tree.Items[I];

        iExt    := ExtractFileExt(TArchiveListItem(Node.Data).FileName    );
        iName   :=  ChangeFileExt(TArchiveListItem(Node.Data).FileName, '');
        iFolder :=               (TArchiveListItem(Node.Data).FilePath    );

        if ((Ext    = '.*') or (AnsiCompareFileName(Ext,    iExt   ) = 0)) and
           ((Name   =  '*') or (AnsiCompareFileName(Name,   iName  ) = 0)) and
           ((Folder =  '*' + PathDelim) or (AnsiCompareFileName(Folder, iFolder) = 0)) then
        begin
          Node.MultiSelected := Value;
          if Value then
          begin
            while Assigned(Node.Parent) do
            begin
              Node.Parent.Expanded := True;
              Node := Node.Parent;
            end;
          end;
        end;
      end;
    finally
      Tree.OnSelectionChanged := Notify;
    end;
  end;
  
  procedure TSeDeseFrm.SelectAll(Tree: TArchiveTreeView);
  var
    I: integer;
    Notify: TNotifyEvent;
  begin
    Notify := Tree.OnSelectionChanged;
    Tree.OnSelectionChanged := nil;
    try
      for I := 0 to Tree.Items.Count -1 do
      begin
        Tree.Items[I].MultiSelected := True;
        if Assigned(Tree.Items[I].Parent) then
        begin
          Tree.Items[I].Parent.Expanded := True;
        end;
      end;
    finally
      Tree.OnSelectionChanged := Notify;
    end;
  end;
  
  procedure TSeDeseFrm.SelectItems(Tree: TArchiveTreeView);
  begin
    SeDeSeItems(Tree, True);
  end;

  procedure TSeDeseFrm.DeSelectAll(Tree: TArchiveTreeView);
  var
    I: integer;
    Notify: TNotifyEvent;
  begin
    Notify := Tree.OnSelectionChanged;
    Tree.OnSelectionChanged := nil;
    try
      for I := 0 to Tree.Items.Count -1 do
      begin
        Tree.Items[I].MultiSelected := False;
      end;
    finally
      Tree.OnSelectionChanged := Notify;
    end;
  end;

  procedure TSeDeseFrm.DeselectItems(Tree: TArchiveTreeView);
  begin
    SeDeSeItems(Tree, False);
  end;

  procedure TSeDeseFrm.InvertSelection(Tree: TArchiveTreeView);
  var
    I: integer;
    Node: TTreeNode;
    Notify: TNotifyEvent;
  begin
    Notify := Tree.OnSelectionChanged;
    Tree.OnSelectionChanged := nil;
    try
      for I := 0 to Tree.Items.Count -1 do
      begin
        Node := Tree.Items[I];
        Node.MultiSelected := not Node.MultiSelected;
        if Node.MultiSelected then
        begin
          while Assigned(Node.Parent) do
          begin
            Node.Parent.Expanded := True;
            Node := Node.Parent;
          end;
        end;
      end;
    finally
      Tree.OnSelectionChanged := Notify;
    end;
  end;

  procedure TSeDeSeFrm.FormShow(Sender: TObject);
  begin
    SeDeseFrm_Mask.SetFocus;
  end;

  procedure TSeDeSeFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      SeDeseFrm_Storage.FileName := CfgFolder + ('sedesefrm.xml');
    end;
    {$I beegui_sedesefrm.inc}
    SeDeseFrm_Storage.Restore;
  end;
  
initialization

  {$I beegui_sedesefrm.lrs}

end.
