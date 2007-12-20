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

unit BeeGui_AddFrm;

{$I Compiler.inc}

interface

uses
  Spin,
  Math,
  Menus,
  Forms,
  ImgList,
  Buttons,
  Dialogs,
  Classes,
  Windows,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ComCtrls,
  FileCtrl,
  Messages,
  ShellApi,
  // ---
  BeeGui_ConfigFrm,
  BeeGui_FormStorage,
  BeeGui_DragFilesTrg,
  BeeGui_AddListManager;

type
  TAddFrm = class(TForm)
    AddFrm_FileImages: TImageList;
    AddFrm_Messages: TComboBox;
    // --
    AddFrm_PMenu: TPopupMenu;
    AddFrm_PMenu_AddDir: TMenuItem;
    AddFrm_PMenu_N2: TMenuItem;
    AddFrm_PMenu_AddFiles: TMenuItem;
    AddFrm_PMenu_N1: TMenuItem;
    AddFrm_PMenu_View: TMenuItem;
    AddFrm_PMenu_Type: TMenuItem;
    AddFrm_PMenu_Modify: TMenuItem;
    AddFrm_PMenu_Delete: TMenuItem;
    // --
    AddFrm_OpenDialog: TOpenDialog;
    BtnCancel: TBitBtn;
    BtnOk:  TBitBtn;
    AddFrm_Pages: TPageControl;
    AddFrm_Pages_General: TTabSheet;
    AddFrm_OptionsGB: TGroupBox;
    AddFrm_rOption: TCheckBox;
    AddFrm_sOption: TCheckBox;
    AddFrm_tOption: TCheckBox;
    AddFrm_kOption: TCheckBox;
    AddFrm_cOption: TCheckBox;
    AddFrm_aOption: TCheckBox;
    AddFrm_MethodGB: TGroupBox;
    AddFrm_Method: TComboBox;
    AddFrm_ActionGB: TGroupBox;
    AddFrm_Action: TComboBox;
    AddFrm_DictionaryGB: TGroupBox;
    AddFrm_Dictionary: TComboBox;
    AddFrm_PriorityGB: TGroupBox;
    General_Priority: TComboBox;
    AddFrm_eOptionGB: TGroupBox;
    AddFrm_eOption: TEdit;
    AddFrm_Pages_Files: TTabSheet;
    AddFrm_FileListLabel: TLabel;
    BtnDir: TSpeedButton;
    BtnFile: TSpeedButton;
    BtnView: TSpeedButton;
    BtnDelete: TSpeedButton;
    BtnModify: TSpeedButton;
    BtnType: TSpeedButton;
    AddFrm_RootLabel: TLabel;
    AddFrm_Root: TEdit;
    BtnUpDown: TUpDown;
    AddFrm_FileList: TListView;
    AddFrm_ZipCompressionLevelGB: TGroupBox;
    AddFrm_ZipCompressionLevel: TComboBox;
    AddFrm_ZipMethodGB: TGroupBox;
    AddFrm_ZipMethod: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    // --
    procedure AddFrm_PMenu_AddDirClick(Sender: TObject);
    procedure AddFrm_PMenu_AddFilesClick(Sender: TObject);
    procedure AddFrm_PMenu_ViewClick(Sender: TObject);
    procedure AddFrm_PMenu_DeleteClick(Sender: TObject);
    procedure AddFrm_PMenu_TypeClick(Sender: TObject);
    procedure AddFrm_PMenu_ModifyClick(Sender: TObject);
    // --
    procedure BtnUpDownClick(Sender: TObject; Button: TUDBtnType);
    // --
    procedure AddFrm_DragFilesTrgDrop(Sender: TObject);
    procedure AddFrm_FilesListSelectItem(Sender: TObject;
      Item: TListItem; Selected: boolean);
  protected
    procedure Initialize;
  private
    FcOption: string;
    FArcName: string;
    FFormStorage: TFormStorage;
    FDragFilesTrg: TDragFilesTrg;
    FAddListManager: TAddListManager;
  public
    constructor Create(AOwner: TComponent;
      const ArcName: string; const cOption: string);
    destructor Destroy; override;
    // --
    procedure LoadOptions(aConfigFrm: TConfigFrm);
    procedure SetAppParams(Params: TStringList);
    procedure AddToFileList(const Name: string; IsFolder: boolean);
    procedure EnableFileList(Value: boolean);
  end;

var
  AddFrm: TAddFrm;

implementation

{$R *.dfm}

uses
  BeeGui_PasswordFrm,
  BeeGui_RenameFrm,
  BeeGui_AddFrmRes,
  // ---
  BeeGui_SysUtils;

// TAddFrm class

constructor TAddFrm.Create(AOwner: TComponent;
  const ArcName: string; const cOption: string);
begin
  inherited Create(AOwner);
  FArcName := ArcName;
  FcOption := cOption;
end;

destructor TAddFrm.Destroy;
begin
  SetLength(FArcName, 0);
  SetLength(FcOption, 0);
  inherited Destroy;
end;

procedure TAddFrm.FormCreate(Sender: TObject);
begin
  FAddListManager := TAddListManager.Create;
  FAddListManager.List := AddFrm_FileList;
  FAddListManager.Root := AddFrm_Root;
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SAddFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SAddFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SAddFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
  FDragFilesTrg := TDragFilesTrg.Create(Self);
  FDragFilesTrg.OnDrop := AddFrm_DragFilesTrgDrop;
  FDragFilesTrg.Target := AddFrm_FileList;
  FDragFilesTrg.Enabled := True;
  // ---
  Initialize;
end;

procedure TAddFrm.Initialize;
var
  FArcExt: string;
begin
  AddFrm_MethodGB.Visible := False;
  AddFrm_Method.Enabled := False;

  AddFrm_DictionaryGB.Visible := False;
  AddFrm_Dictionary.Enabled := False;

  AddFrm_ZipMethodGB.Visible := False;
  AddFrm_ZipMethod.Enabled := False;

  AddFrm_ZipCompressionLevelGB.Visible := False;
  AddFrm_ZipCompressionLevel.Enabled := False;

  AddFrm_eOptionGB.Visible := True;
  AddFrm_eOption.Enabled := False;
  AddFrm_eOption.Color := clBtnFace;

  AddFrm_aOption.Enabled := False;
  AddFrm_kOption.Enabled := False;
  AddFrm_sOption.Enabled := False;
  AddFrm_tOption.Enabled := False;

  FArcExt := ExtractFileExt(FArcName);
  if (AnsiCompareText(FArcExt, '.zip') = 0) then
  begin
    AddFrm_ZipMethodGB.Visible := True;
    AddFrm_ZipMethod.Enabled := True;
    AddFrm_ZipCompressionLevelGB.Visible := True;
    AddFrm_ZipCompressionLevel.Enabled := True;

    AddFrm_kOption.Enabled := True;
    AddFrm_tOption.Enabled := True;
  end else
    if (AnsiCompareText(FArcExt, '.tar') = 0) then
    begin
      AddFrm_ZipMethodGB.Visible := True;
      AddFrm_ZipMethod.ItemIndex := -1;
      AddFrm_ZipMethod.Color := clBtnFace;
      AddFrm_ZipCompressionLevelGB.Visible := True;
      AddFrm_ZipCompressionLevel.ItemIndex := -1;
      AddFrm_ZipCompressionLevel.Color := clBtnFace;
    end else
      if (AnsiCompareText(FArcExt, '.bee') = 0) or
         (AnsiCompareText(FArcExt, '.exe') = 0) then
      begin   
        AddFrm_MethodGB.Visible := True;
        AddFrm_Method.Enabled := True;
        AddFrm_DictionaryGB.Visible := True;
        AddFrm_Dictionary.Enabled := True;

        AddFrm_aOption.Enabled := True;
        AddFrm_kOption.Enabled := True;
        AddFrm_sOption.Enabled := True;
        AddFrm_tOption.Enabled := True;

        AddFrm_eOption.Enabled := True;
        AddFrm_eOption.Color := clWindow;
      end;

  AddFrm_aOption.Checked := AddFrm_aOption.Enabled;
  AddFrm_kOption.Checked := AddFrm_kOption.Enabled;
  AddFrm_sOption.Checked := AddFrm_sOption.Enabled;
  AddFrm_tOption.Checked := AddFrm_tOption.Enabled;
end;

procedure TAddFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SAddFrm_PropertyFull
  else
    FFormStorage.Properties := _SAddFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
  // --
  FDragFilesTrg.Enabled := False;
  FDragFilesTrg.Free;
  // --
  FAddListManager.Free;
end;

procedure TAddFrm.AddFrm_PMenu_AddDirClick(Sender: TObject);
var
  S: string;
begin
  if SelectDirectory(AddFrm_Messages.Items[_CAddFrm_SelFolderToAdd], '', S) then
  begin
    FAddListManager.Add(S, True);
  end;
end;

procedure TAddFrm.AddFrm_PMenu_AddFilesClick(Sender: TObject);
var
  I: integer;
begin
  if AddFrm_OpenDialog.Execute then
  begin
    for I := 0 to AddFrm_OpenDialog.Files.Count - 1 do
    begin
      FAddListManager.Add(AddFrm_OpenDialog.Files[I], False);
    end;
  end;
end;

procedure TAddFrm.AddFrm_PMenu_ViewClick(Sender: TObject);
var
  I: integer;
  S: string;
begin
  for I := 0 to FAddListManager.Count - 1 do
  begin
    if FAddListManager.IsSel[I] then
    begin
      S := ExtractFilePath(FAddListManager.Paths[I]);
      ShellExecute(0, 'open', PChar(S), nil, PChar(''), SW_SHOW);
    end;
  end;
end;

procedure TAddFrm.AddFrm_PMenu_TypeClick(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to FAddListManager.Count - 1 do
  begin
    if FAddListManager.IsSel[I] then
    begin
      if FAddListManager.Icons[I] = 0 then
        FAddListManager.Icons[I] := 1
      else
        FAddListManager.Icons[I] := 0;
    end;
  end;
end;

procedure TAddFrm.AddFrm_PMenu_ModifyClick(Sender: TObject);
var
  I: integer;
  f: TRenameFrm;
  NewName: string;
begin
  for I := 0 to FAddListManager.Count - 1 do
  begin
    if FAddListManager.IsSel[I] then
    begin
      f := TRenameFrm.Create(Self);
      f.Caption := AddFrm_Messages.Items[_CAddFrm_ModifyMask];
      f.RenameFrm_To.Text := FAddListManager.Masks[I];
      // --
      if f.ShowModal = mrOk then
      begin
        NewName := ExcludeTrailingBackslash(f.RenameFrm_To.Text);
        while Pos(PathDelim, NewName) = 1 do
        begin
          Delete(NewName, 1, 1);
        end;
        if Length(NewName) > 0 then
        begin
          FAddListManager.Masks[I] := NewName;
        end;
      end;
      f.Free;
    end;
  end;
end;

procedure TAddFrm.BtnUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btPrev: FAddListManager.Spin := FAddListManager.Spin + 1;
    btNext: FAddListManager.Spin := FAddListManager.Spin - 1;
  end;
end;

procedure TAddFrm.AddFrm_PMenu_DeleteClick(Sender: TObject);
var
  I: integer;
begin
  for I := FAddListManager.Count - 1 downto 0 do
  begin
    if FAddListManager.IsSel[I] then
    begin
      FAddListManager.Delete(I);
    end;
  end;
end;

procedure TAddFrm.AddFrm_FilesListSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
begin
  if Selected then
  begin
    AddFrm_PMenu_AddDir.Enabled := True;
    AddFrm_PMenu_AddFiles.Enabled := True;
    // --
    AddFrm_PMenu_View.Enabled := True;
    AddFrm_PMenu_Type.Enabled := True;
    AddFrm_PMenu_Modify.Enabled := True;
    // --
    AddFrm_PMenu_Delete.Enabled := True;
  end else
  begin
    AddFrm_PMenu_AddDir.Enabled := True;
    AddFrm_PMenu_AddFiles.Enabled := True;
    // --
    AddFrm_PMenu_View.Enabled := False;
    AddFrm_PMenu_Type.Enabled := False;
    AddFrm_PMenu_Modify.Enabled := False;
    // --
    AddFrm_PMenu_Delete.Enabled := False;
  end;
  BtnView.Enabled := AddFrm_PMenu_View.Enabled;
  BtnType.Enabled := AddFrm_PMenu_Type.Enabled;
  BtnModify.Enabled := AddFrm_PMenu_Modify.Enabled;
  BtnDelete.Enabled := AddFrm_PMenu_Delete.Enabled;
end;

procedure TAddFrm.AddFrm_DragFilesTrgDrop(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to FDragFilesTrg.FileCount - 1 do
  begin
    if FileExists(FDragFilesTrg.FileList.Strings[I]) then
    begin
      FAddListManager.Add(FDragFilesTrg.FileList.Strings[I], False);
    end else
    begin
      FAddListManager.Add(FDragFilesTrg.FileList.Strings[I], True);
    end;
  end;
end;

procedure TAddFrm.EnableFileList(Value: boolean);
begin
  BtnDir.Enabled  := Value;
  BtnFile.Enabled := Value;
  BtnUpDown.Enabled := Value;
  // --
  AddFrm_FileList.Enabled := Value;
  if Value then
  begin
    AddFrm_FileList.Color := clWindow;
  end else
  begin
    AddFrm_FileList.Color := clBtnFace;
  end;
end;

procedure TAddFrm.SetAppParams(Params: TStringList);
var
  I: integer;
begin
  with Params do
  begin
    Add('a');
    Add('-l');
    Add('-y' + GetApplicationTempDir);

    if AddFrm_rOption.Checked then Add('-r');
    if AddFrm_sOption.Checked then Add('-s');
    if AddFrm_tOption.Checked then Add('-t');
    if AddFrm_aOption.Checked then Add('-a');
    if AddFrm_kOption.Checked then Add('-k');

    if Length(AddFrm_eOption.Text) > 0 then
      Add('-e' + AddFrm_eOption.Text);

    if AddFrm_MethodGB.Visible and (AddFrm_Method.ItemIndex <> -1) then
      Add('-m' + AddFrm_Method.Items[AddFrm_Method.ItemIndex][1]);

    if AddFrm_ZipMethodGB.Visible and (AddFrm_ZipMethod.ItemIndex <> -1) then
      Add('-m' + AddFrm_ZipMethod.Items[AddFrm_ZipMethod.ItemIndex][1]);

    if AddFrm_DictionaryGB.Visible and (AddFrm_Dictionary.ItemIndex <> -1) then
      Add('-d' + AddFrm_Dictionary.Items[AddFrm_Dictionary.ItemIndex][1]);

    if AddFrm_ZipCompressionLevelGB.Visible and (AddFrm_ZipCompressionLevel.ItemIndex <> -1) then
      Add('-d' + AddFrm_ZipCompressionLevel.Items[AddFrm_ZipCompressionLevel.ItemIndex][1]);

    Add('-pri' + General_Priority.Items[General_Priority.ItemIndex][1]);

    if (FcOption <> '') and AddFrm_cOption.Enabled and AddFrm_cOption.Checked then
    begin
      Add('-c' + FcOption);
    end;

    Add(FArcName);
    for I := 0 to FAddListManager.Count - 1 do
    begin
      if FAddListManager.Icons[I] = 0 then
        Add(FAddListManager.List.Items[I].Caption)
      else
        Add('-x' + FAddListManager.List.Items[I].Caption);
    end;
  end;
end;

procedure TAddFrm.AddToFileList(const Name: string; IsFolder: boolean);
begin
  FAddListManager.Add(Name, IsFolder);
end;

procedure TAddFrm.LoadOptions(aConfigFrm: TConfigFrm);
begin
  AddFrm_Dictionary.ItemIndex := aConfigFrm.AddFrm_Dictionary.ItemIndex;
  AddFrm_Method.ItemIndex := aConfigFrm.AddFrm_Method.ItemIndex;
  // ---
  AddFrm_aOption.Checked  := aConfigFrm.AddFrm_aOption.Checked;
  AddFrm_cOption.Checked  := aConfigFrm.AddFrm_cOption.Checked;
  AddFrm_kOption.Checked  := aConfigFrm.AddFrm_kOption.Checked;
  AddFrm_rOption.Checked  := aConfigFrm.AddFrm_rOption.Checked;
  AddFrm_sOption.Checked  := aConfigFrm.AddFrm_sOption.Checked;
  AddFrm_tOption.Checked  := aConfigFrm.AddFrm_tOption.Checked;
  // ---
  General_Priority.ItemIndex := aConfigFrm.General_Priority.ItemIndex;
end;

end.
