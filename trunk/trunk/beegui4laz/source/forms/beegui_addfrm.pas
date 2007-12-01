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

    BeeGui Add form.

    Modifyed:

    v1.0.1 build 9155 - 2005.07.15 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso.
  
    v1.0.3 build 0360 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_AddFrm;

interface

uses
  Forms,
  Menus,
  Buttons,
  Dialogs,
  Classes,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ComCtrls,
  LResources,
  // --
  BeeGui_AddTreeView,
  XMLPropStorage;

type
  { TAddFrm class }

  TAddFrm = class(TForm)
    AddFrm_Storage: TXMLPropStorage;
    AddFrm_Pages_General: TTabSheet;
    AddFrm_Pages_Files: TTabSheet;
    AddFrm_Pages: TPageControl;
    // Add Page
    AddFrm_MethodGB: TGroupBox;
    AddFrm_ActionGB: TGroupBox;
    AddFrm_FilesTree: TAddTreeView;
    BtnDown: TBitBtn;
    BtnUp: TBitBtn;
    BtnFiles: TBitBtn;
    BtnView: TBitBtn;
    BtnModify: TBitBtn;
    BtnDelete: TBitBtn;
    BtnType: TBitBtn;
    BtnDir: TBitBtn;
    ConfigFrm_PriorityGB: TGroupBox;
    AddFrm_DictionaryGB: TGroupBox;
    AddFrm_Dictionary: TComboBox;
    AddFrm_Method: TComboBox;
    AddFrm_Action: TComboBox;
    ConfigFrm_Priority: TComboBox;
    AddFrm_OptionsGB: TGroupBox;
    AddFrm_sOption: TCheckBox;
    AddFrm_tOption: TCheckBox;
    AddFrm_rOption: TCheckBox;
    AddFrm_kOption: TCheckBox;
    // Files page
    AddFrm_FilesTitle: TLabel;
    AddFrm_FilesImages: TImageList;
    AddFrm_PMenu: TPopupMenu;
    AddFrm_PMenu_AddDir: TMenuItem;
    AddFrm_PMenu_N2: TMenuItem;
    AddFrm_PMenu_AddFiles: TMenuItem;
    AddFrm_PMenu_N1: TMenuItem;
    AddFrm_PMenu_View: TMenuItem;
    AddFrm_PMenu_Type: TMenuItem;
    AddFrm_PMenu_Modify: TMenuItem;
    AddFrm_PMenu_Delete: TMenuItem;
    AddFrm_Root: TEdit;
    // MSGs
    AddFrm_MSGs: TComboBox;
    // Dialog
    AddFrm_OpenDialog: TOpenDialog;
    AddFrm_ForceExtGB: TGroupBox;
    AddFrm_eOption: TEdit;
    AddFrm_Root_: TLabel;
    AddFrm_aOption: TCheckBox;
    
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    
    procedure BtnDownClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure AddFrm_PMenu_AddDirClick(Sender: TObject);
    procedure AddFrm_PMenu_AddFilesClick(Sender: TObject);
    procedure AddFrm_PMenu_ViewClick(Sender: TObject);
    procedure AddFrm_PMenu_DeleteClick(Sender: TObject);
    procedure AddFrm_PMenu_TypeClick(Sender: TObject);
    procedure AddFrm_PMenu_ModifyClick(Sender: TObject);
    procedure AddFrm_FilesTreeSelectionChanged(Sender: TObject);
  public
    procedure EnableFilesList(Value: boolean);
  private
  end;

implementation

uses
  BeeGui_PasswordFrm,
  BeeGui_ConfigFrm,
  BeeGui_RenameFrm,
  BeeGui_SysUtils;

const
  AddFrm_MSG_SelectDir  = 0;
  AddFrm_MSG_ModifyItem = 1;

 { TAddFrm class }

  procedure TAddFrm.AddFrm_PMenu_AddDirClick(Sender: TObject);
  var
    DirName: string;
  begin
    DirName := '';
    if SelectDirectory(AddFrm_MSGs.Items[AddFrm_MSG_SelectDir], '', DirName) then
    begin
      AddFrm_FilesTree.AddDirName (DirName);
    end;
    AddFrm_FilesTree.Selected := nil;
  end;
  
  procedure TAddFrm.AddFrm_PMenu_AddFilesClick (Sender: TObject);
  var
    I: integer;
  begin
    if AddFrm_OpenDialog.Execute then
      for I := 0 to AddFrm_OpenDialog.Files.Count - 1 do
      begin
        AddFrm_FilesTree.AddFileName (AddFrm_OpenDialog.Files [I]);
      end;
    AddFrm_FilesTree.Selected := nil;
  end;

  procedure TAddFrm.AddFrm_PMenu_ViewClick (Sender: TObject);
  var
    I: integer;
  begin
    for I := 0 to AddFrm_FilesTree.Items.Count -1 do
    begin
      if AddFrm_FilesTree.Items [I].Selected then
      begin
        //BeeGui_ShellExecute (AddFrm_FilesTree.FilePath.Strings [I], '');
      end;
    end;
  end;
  
  procedure TAddFrm.AddFrm_PMenu_TypeClick (Sender: TObject);
  var
    I: integer;
  begin
    for I := AddFrm_FilesTree.Items.Count - 1 downto 0 do
    begin
      if AddFrm_FilesTree.Items [I].Selected then
      begin
        if AddFrm_FilesTree.Items [I].ImageIndex = ATV_SELECTED then
        begin
          AddFrm_FilesTree.Items [I].ImageIndex    := ATV_EXCLUDED;
          AddFrm_FilesTree.Items [I].StateIndex    := ATV_EXCLUDED;
          AddFrm_FilesTree.Items [I].SelectedIndex := ATV_EXCLUDED;
        end else
        begin
          AddFrm_FilesTree.Items [I].ImageIndex    := ATV_SELECTED;
          AddFrm_FilesTree.Items [I].ImageIndex    := ATV_SELECTED;
          AddFrm_FilesTree.Items [I].SelectedIndex := ATV_SELECTED;
        end;
      end;
    end;
    AddFrm_FilesTree.UpdateItems;
  end;

  procedure TAddFrm.AddFrm_PMenu_ModifyClick (Sender: TObject);
  var
    f: TRenameFrm;
    NewName: string;
  begin
    if AddFrm_FilesTree.Selected <> nil then
    begin
      f := TRenameFrm.Create (Self);
      f.Caption := AddFrm_MSGs.Items [AddFrm_MSG_ModifyItem];
      f.RenameFrm_To.Text := AddFrm_FilesTree.FileMask.Strings [AddFrm_FilesTree.Selected.Index];

      if f.ShowModal = mrOk then
      begin
        NewName := ExcludeTrailingBackslash (f.RenameFrm_To.Text);
        while Pos ('\', NewName) = 1 do Delete (NewName, 1, 1);

        if Length (NewName) > 0 then
        begin
          AddFrm_FilesTree.FileMask [AddFrm_FilesTree.Selected.Index] := NewName;
          AddFrm_FilesTree.UpdateItems;
        end;
      end;
      f.Free;
    end;
  end;

  procedure TAddFrm.AddFrm_PMenu_DeleteClick (Sender: TObject);
  var
    I: integer;
  begin
    for I := AddFrm_FilesTree.Items.Count - 1 downto 0 do
    begin
      if AddFrm_FilesTree.Items [I].Selected then
      begin
        AddFrm_FilesTree.DeleteItem (I);
      end;
    end;
    AddFrm_FilesTree.UpdateItems;
  end;

  procedure TAddFrm.FormCreate (Sender: TObject);
  var
    CfgFolder: string;
  begin
    AddFrm_Pages.ActivePage := AddFrm_Pages_General;
    //---
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      AddFrm_Storage.FileName := CfgFolder + ('addfrm.xml');
    end;
    {$I beegui_addfrm.inc}
    AddFrm_Storage.Restore;
  end;

  procedure TAddFrm.BtnDownClick(Sender: TObject);
  var
    NewSpin: integer;
  begin
    NewSpin := AddFrm_FilesTree.Spin;

    Dec (NewSpin);

    if AnsiCompareFileName (AddFrm_FilesTree.GetNewRoot (NewSpin), AddFrm_Root.Text) <> 0 then
      AddFrm_FilesTree.Spin := NewSpin;

    AddFrm_FilesTree.UpdateItems;
  end;

  procedure TAddFrm.AddFrm_FilesTreeSelectionChanged (Sender: TObject);
  begin
    if AddFrm_FilesTree.Selected = nil then
    begin
      AddFrm_PMenu_AddDir   .Enabled := True;
      AddFrm_PMenu_AddFiles .Enabled := True;
      // --
      AddFrm_PMenu_View     .Enabled := False;
      AddFrm_PMenu_Type     .Enabled := False;
      AddFrm_PMenu_Modify   .Enabled := False;
      // --
      AddFrm_PMenu_Delete   .Enabled := False;
    end else
    begin
      AddFrm_PMenu_AddDir   .Enabled := True;
      AddFrm_PMenu_AddFiles .Enabled := True;
      // --
      AddFrm_PMenu_View     .Enabled := True;
      AddFrm_PMenu_Type     .Enabled := True;
      AddFrm_PMenu_Modify   .Enabled := True;
      // --
      AddFrm_PMenu_Delete   .Enabled := True;
    end;

    BtnView  .Enabled := AddFrm_PMenu_View  .Enabled;
    BtnType  .Enabled := AddFrm_PMenu_Type  .Enabled;
    BtnModify.Enabled := AddFrm_PMenu_Modify.Enabled;
    BtnDelete.Enabled := AddFrm_PMenu_Delete.Enabled;
  end;

  procedure TAddFrm.BtnUpClick (Sender: TObject);
  var
    NewSpin: integer;
  begin
    NewSpin := AddFrm_FilesTree.Spin;

    Inc (NewSpin);

    if AnsiCompareFileName (AddFrm_FilesTree.GetNewRoot (NewSpin), AddFrm_Root.Text) <> 0 then
      AddFrm_FilesTree.Spin := NewSpin;

    AddFrm_FilesTree.UpdateItems;
  end;

  procedure TAddFrm.FormDestroy (Sender: TObject);
  begin
    // DragAcceptFiles (AddFrm_FilesList.Handle, False);
  end;

  (*
  procedure TAddFrm.AddFrm_DragFilesTrgDrop (Sender: TObject);
  var
    I: integer;
  begin
    for I := 0 to AddFrm_DragFilesTrg.FileCount -1 do
    begin
      if FileExists (AddFrm_DragFilesTrg.FileList.Strings [I]) then
        AddFrm_FilesList.AddFile (AddFrm_DragFilesTrg.FileList.Strings [I])
      else
        AddFrm_FilesList.AddDirectory (AddFrm_DragFilesTrg.FileList.Strings [I]);
    end;
    AddFrm_FilesList.UpdateItems;
  end;
  *)
  
  procedure TAddFrm.EnableFilesList (Value: boolean);
  begin
    BtnDir          .Enabled := Value;
    BtnFiles        .Enabled := Value;
    BtnUp           .Enabled := Value;
    BtnDown         .Enabled := Value;
    AddFrm_FilesTree.Enabled := Value;

    if Value then
      AddFrm_FilesTree.Color := clWindow
    else
      AddFrm_FilesTree.Color := clBtnFace;
  end;
  
initialization

  {$I beegui_addfrm.lrs}

end.
