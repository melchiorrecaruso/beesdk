{
    Copyright (c) 2005-2007 Andrew Filinsky and Melchiorre Caruso

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
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_AddFrm;

{$R-,Q-,S-}

interface

uses
  Spin,
  Math,
  Forms,
  Menus,
  ImgList,
  Buttons,
  Dialogs,
  Classes,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ComCtrls,
  FileCtrl,
  Messages,
  ShellApi,

  uDragFilesTrg, 

  BeeGui_Components;

type
  TAddCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type
  TAddFrm = class (TAddCustomFrm)
    AddFrm_Pages_General: TTabSheet;
    AddFrm_Pages_Files: TTabSheet;
    AddFrm_Pages: TPageControl;
    // Add Page
    AddFrm_MethodGB: TGroupBox;
    AddFrm_ActionGB: TGroupBox;
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
    BtnUpDown: TUpDown;
    // MSGs
    AddFrm_MSGs: TComboBox;
    // Dialog
    AddFrm_OpenDialog: TOpenDialog;
    AddFrm_cOption: TCheckBox;
    BtnDir: TSpeedButton;
    BtnFile: TSpeedButton;
    AddFrm_ForceExtGB: TGroupBox;
    AddFrm_eOption: TEdit;
    AddFrm_Root_: TLabel;
    AddFrm_aOption: TCheckBox;
    AddFrm_DragFilesTrg: TDragFilesTrg;
    AddFrm_FilesList: TBeeGui_ListView_Add;

    BtnView: TSpeedButton;
    BtnDelete: TSpeedButton;
    BtnModify: TSpeedButton;
    BtnType: TSpeedButton;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    
    procedure FormCreate (Sender: TObject);
    procedure FormDestroy (Sender: TObject);

    procedure AddFrm_FilesListSelectItem (Sender: TObject; Item: TListItem; Selected: Boolean);

    procedure AddFrm_PMenu_AddDirClick (Sender: TObject);
    procedure AddFrm_PMenu_AddFilesClick (Sender: TObject);
    procedure AddFrm_PMenu_ViewClick (Sender: TObject);
    procedure AddFrm_PMenu_DeleteClick (Sender: TObject);
    procedure AddFrm_PMenu_TypeClick (Sender: TObject);
    procedure AddFrm_PMenu_ModifyClick (Sender: TObject);

    procedure BtnUpDownClick (Sender: TObject; Button: TUDBtnType);
    procedure AddFrm_DragFilesTrgDrop (Sender: TObject);

  public
    procedure EnableFilesList (Value: boolean);
  private
    { Private declarations }
  end;

var
  AddFrm: TAddFrm;

implementation

{$R *.DFM}

uses
  BeeGui_PasswordFrm,
  BeeGui_ConfigFrm,
  BeeGui_RenameFrm,
  BeeGui_Common,
  BeeGui_WinOS;

const
  AddFrm_MSG_SelectDir  = 0;
  AddFrm_MSG_ModifyItem = 1;

 // TBeeGuiAddForm

  constructor TAddCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TAddCustomFrm.Destroy;
  var
    I: integer;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
    begin
      if AnsiCompareText (TControl (Self.Components [I]).Name, 'AddFrm_FilesList') = 0 then
      begin
        BeeGui_Configuration.Save_TControl (TControl (Self.Components [I]));
      end;
    end;
    inherited Destroy;
  end;

  procedure TAddCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

 // TAddFrm class

  procedure TAddFrm.AddFrm_PMenu_AddDirClick (Sender: TObject);
  var
    Dir: ansistring;
  begin
    if SelectDirectory (AddFrm_MSGs.Items [AddFrm_MSG_SelectDir], '', Dir) then
      AddFrm_FilesList.AddDirectory (Dir);
  end;

  procedure TAddFrm.AddFrm_PMenu_AddFilesClick (Sender: TObject);
  var
    I: integer;
  begin
    if AddFrm_OpenDialog.Execute then
      for I := 0 to AddFrm_OpenDialog.Files.Count - 1 do
        AddFrm_FilesList.AddFile (AddFrm_OpenDialog.Files [I]);
  end;

  procedure TAddFrm.AddFrm_PMenu_ViewClick (Sender: TObject);
  var
    I: integer;
  begin
    for I := 0 to AddFrm_FilesList.Items.Count -1 do
      if AddFrm_FilesList.Items [I].Selected then
        BeeGui_ShellExecute (AddFrm_FilesList.Items [I].SubItems [0], '');
  end;
  
  procedure TAddFrm.AddFrm_PMenu_TypeClick (Sender: TObject);
  var
    I: integer;
  begin
    for I := AddFrm_FilesList.Items.Count - 1 downto 0 do
      if AddFrm_FilesList.Items [I].Selected then
      begin
        if AddFrm_FilesList.Items [I].ImageIndex = S_Index then
          AddFrm_FilesList.Items [I].ImageIndex := E_Index
        else
          AddFrm_FilesList.Items [I].ImageIndex := S_Index;
      end;
    AddFrm_FilesList.UpdateItems;
  end;

  procedure TAddFrm.AddFrm_PMenu_ModifyClick (Sender: TObject);
  var
    f: TRenameFrm;
    NewName: string;
  begin
    if AddFrm_FilesList.Selected <> nil then
    begin
      f := TRenameFrm.Create (Self);
      f.Caption := AddFrm_MSGs.Items [AddFrm_MSG_ModifyItem];
      f.RenameFrm_To.Text := AddFrm_FilesList.Selected.SubItems [2];

      if f.ShowModal = mrOk then
      begin
        NewName := ExcludeTrailingBackslash (f.RenameFrm_To.Text);
        while Pos ('\', NewName) = 1 do Delete (NewName, 1, 1);

        if Length (NewName) > 0 then
        begin
          AddFrm_FilesList.Selected.SubItems [2] := NewName;
          AddFrm_FilesList.UpdateItems;
        end;
      end;

      f.Free;
    end;
  end;

  procedure TAddFrm.BtnUpDownClick (Sender: TObject; Button: TUDBtnType);
  var
    NewSpin: integer;
  begin
    NewSpin := AddFrm_FilesList.Spin;
    case Button of
      btPrev: Inc (NewSpin);
      btNext: Dec (NewSpin);
    end;

    if CompareText (AddFrm_FilesList.GetNewRoot (NewSpin), AddFrm_Root.Text) <> 0 then
      AddFrm_FilesList.Spin := NewSpin;

    AddFrm_FilesList.UpdateItems;
  end;

  procedure TAddFrm.AddFrm_PMenu_DeleteClick (Sender: TObject);
  var
    I: integer;
  begin
    for I := AddFrm_FilesList.Items.Count - 1 downto 0 do
      if AddFrm_FilesList.Items [I].Selected then
        AddFrm_FilesList.Items.Delete (I);

    AddFrm_FilesList.UpdateItems;
  end;

  procedure TAddFrm.FormCreate (Sender: TObject);
  begin
    AddFrm_Pages.ActivePage := AddFrm_Pages_General;
    DragAcceptFiles (AddFrm_FilesList.Handle, True);
  end;

  procedure TAddFrm.FormDestroy (Sender: TObject);
  begin
    DragAcceptFiles (AddFrm_FilesList.Handle, False);
  end;

  procedure TAddFrm.AddFrm_FilesListSelectItem (Sender: TObject; Item: TListItem; Selected: Boolean);
  begin
    if AddFrm_FilesList.SelCount = 0 then
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

  procedure TAddFrm.EnableFilesList (Value: boolean);
  begin
    BtnDir          .Enabled := Value;
    BtnFile         .Enabled := Value;
    BtnUpDown       .Enabled := Value;
    AddFrm_FilesList.Enabled := Value;

    if Value then
      AddFrm_FilesList.Color := clWindow
    else
      AddFrm_FilesList.Color := clBtnFace;
  end;

end.
