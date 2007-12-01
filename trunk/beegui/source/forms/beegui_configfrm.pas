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

unit BeeGui_ConfigFrm;

{$I Compiler.inc}

interface

uses
  Forms,
  Menus,
  ImgList,
  Buttons,
  Classes,
  Dialogs,
  Windows,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  FileCtrl,
  BeeGui_FormStorage;

type
  TConfigFrm = class(TForm)
    ConfigFrm_Tree: TTreeView;
    // add goup box
    AddFrm_OptionsGB: TGroupBox;
    AddFrm_aOption: TCheckBox;
    AddFrm_cOption: TCheckBox;
    AddFrm_rOption: TCheckBox;
    AddFrm_sOption: TCheckBox;
    AddFrm_tOption: TCheckBox;
    AddFrm_kOption: TCheckBox;
    AddFrm_MethodGB: TGroupBox;
    AddFrm_Method: TComboBox;
    AddFrm_DictionaryGB: TGroupBox;
    AddFrm_Dictionary: TComboBox;
    // extract group box
    ExtractFrm_cOption: TCheckBox;
    ExtractFrm_OverWriteGB: TGroupBox;
    ExtractFrm_OverWrite: TComboBox;
    ExtractFrm_OptionsGB: TGroupBox;
    ExtractFrm_xCommand: TCheckBox;
    ToolBar_SkinGB: TGroupBox;
    ToolBar_Skin_Image: TImage;
    ToolBar_Skin: TComboBox;
    ToolBar_ButtonsLabel: TLabel;
    ToolBar_Buttons: TListView;
    // general group box
    General_PriorityGB: TGroupBox;
    General_Priority: TComboBox;
    General_OptionsGB: TGroupBox;
    General_Options_UpAsExit: TCheckBox;
    General_Options_HideMainFrm: TCheckBox;
    General_Options_MaskKey: TCheckBox;
    // notebook
    ConfigFrm_Notebook: TNotebook;
    ConfigFrm_Notebook_Add4Bee: TGroupBox;
    ConfigFrm_Notebook_ToolBar: TGroupBox;
    ConfigFrm_Notebook_General: TGroupBox;
    ConfigFrm_Notebook_Extract: TGroupBox;
    // buttons
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    function ShowModal: integer; override;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToolBar_SkinClick(Sender: TObject);
    procedure ToolBar_ButtonsClick(Sender: TObject);
    procedure ConfigFrm_TreeChange(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    FPopup: TPopupMenu;
    FFormStorage: TFormStorage;
    function GetButtons: TPopupMenu;
    function GetSkinFileName: string;
    procedure SetButtons(Value: TPopupMenu);
    procedure ToolBar_Skin_Image_Load;
    procedure ToolBar_Skin_Initialize;
  public
    { Public declarations }
    procedure ToolBar_Skin_Buttons_Load(var Buttons: array of TControl);
    function GetDictionaryStr: string;
    function GetPriorityStr: string;
    function GetMethodStr: string;
    property Buttons: TPopupMenu Read GetButtons Write SetButtons;
  end;

var
  ConfigFrm: TConfigFrm;

implementation

{$R *.dfm}

uses
  BeeGui_ConfigFrmRes,
  BeeGui_SysUtils,
  // ---
  IniFiles;

// TConfigFrm

procedure TConfigFrm.FormCreate(Sender: TObject);
 // var
 // Item: TTreeNode;
begin
  // Inizialize ToolBar_Skin
  ToolBar_Skin_Initialize;
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SConfigFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SConfigFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SConfigFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
  ConfigFrm_Tree.Items.AddChild(nil, ConfigFrm_Notebook_Add4Bee.Caption);
  ConfigFrm_Tree.Items.AddChild(nil, ConfigFrm_Notebook_Extract.Caption);
  ConfigFrm_Tree.Items.AddChild(nil, ConfigFrm_Notebook_ToolBar.Caption);
  ConfigFrm_Tree.Items.AddChild(nil, ConfigFrm_Notebook_General.Caption);
  if ConfigFrm_Tree.Items.Count > 0 then
  begin
    ConfigFrm_Tree.Items[0].Selected := True;
  end;
end;

procedure TConfigFrm.FormDestroy(Sender: TObject);
begin
  if WindowState = wsNormal then
    FFormStorage.Properties := _SConfigFrm_PropertyFull
  else
    FFormStorage.Properties := _SConfigFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
end;

function TConfigFrm.ShowModal: integer;
begin
  ToolBar_Skin_Image_Load;
  // ---
  if WindowState = wsNormal then
    FFormStorage.Properties := _SConfigFrm_PropertyFull
  else
    FFormStorage.Properties := _SConfigFrm_Property;
  FFormStorage.Save(GetApplicationTmpSettingFileName);
  // ---
  Result := inherited ShowModal;
  if Result = mrCancel then
  begin
    if WindowState = wsNormal then
      FFormStorage.Properties := _SConfigFrm_PropertyFull
    else
      FFormStorage.Properties := _SConfigFrm_Property;
    FFormStorage.Load(GetApplicationTmpSettingFileName);
  end;
end;

procedure TConfigFrm.ConfigFrm_TreeChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(ConfigFrm_Tree.Selected) then
  begin
    case Node.AbsoluteIndex of
      0: ConfigFrm_Notebook.ActivePage := 'Add4Bee';
      1: ConfigFrm_Notebook.ActivePage := 'Extract';
      2: ConfigFrm_Notebook.ActivePage := 'Skin';
      3: ConfigFrm_Notebook.ActivePage := 'General';
    end;
  end;
end;

procedure TConfigFrm.ToolBar_SkinClick(Sender: TObject);
begin
  ToolBar_Skin_Image_Load;
end;

procedure TConfigFrm.ToolBar_Skin_Image_Load;
var
  b1, b2:  TBitmap;
  x, y, i: integer;
begin
  if FileExists(GetSkinFileName) then
  begin
    b1 := TBitmap.Create;
    b2 := TBitmap.Create;
    try
      b1.LoadFromFile(GetSkinFileName);
      b2.Height := b1.Height;
      b2.Width  := b2.Height * 8;
      for i := 0 to 7 do
      begin
        for y := 0 to b1.Height - 1 do
          for x := 0 to b1.Height - 1 do
          begin
            b2.Canvas.Pixels[i * b1.Height + x, y] :=
              b1.Canvas.Pixels[i * 2 * b1.Height + x, y];
          end;
      end;
      ToolBar_Skin_Image.Width  := b2.Width;
      ToolBar_Skin_Image.Height := b2.Height;
      ToolBar_Skin_Image.Canvas.Draw(0, 0, b2);
    finally
      b1.Destroy;
      b2.Destroy;
    end;
  end;
end;

procedure TConfigFrm.SetButtons(Value: TPopupMenu);
var
  I: integer;
  P: integer;
  lvItem: TListItem;
  newCaption: string;
begin
  ToolBar_Buttons.Items.Clear;

  FPopup := Value;
  for I := 0 to FPopup.Items.Count - 1 do
  begin
    newCaption := FPopup.Items[I].Caption;
    P := Pos('&', newCaption);
    while P > 0 do
    begin
      Delete(newCaption, P, 1);
      P := Pos('&', newCaption);
    end;
    lvItem := ToolBar_Buttons.Items.Add;
    lvItem.Caption := newCaption;
    lvItem.Checked := FPopup.Items[I].Checked;
  end;
end;

function TConfigFrm.GetButtons: TPopupMenu;
var
  I: integer;
begin
  if Assigned(FPopup) then
  begin
    for I := 0 to FPopup.Items.Count - 1 do
    begin
      FPopup.Items[I].Checked :=
        ToolBar_Buttons.Items[I].Checked;
    end;
    Result := FPopup;
  end;
end;

procedure TConfigFrm.ToolBar_ButtonsClick(Sender: TObject);
begin
  if Assigned(ToolBar_Buttons.Selected) then
  begin
    ToolBar_Buttons.Selected.Checked := not ToolBar_Buttons.Selected.Checked;
  end;
end;

procedure TConfigFrm.ToolBar_Skin_Buttons_Load(var Buttons: array of TControl);
var
  I, J:  integer;
  Image: TBitmap;
  ImageList: TImageList;
begin
  if FileExists(GetSkinFileName) then
  begin
    Image := TBitmap.Create;
    ImageList := TImageList.Create(nil);
    try
      Image.LoadFromFile(GetSkinFileName);
      ImageList.Height := Image.Height;
      ImageList.Width  := Image.Height * 2;
      ImageList.BkColor := clFuchsia;
      ImageList.Add(Image, nil);

      I := 0;
      for J := 0 to High(Buttons) do
        if (Buttons[J].ClassType = TSpeedButton) then
        begin
          TSpeedButton(Buttons[J]).Glyph.FreeImage;
          TSpeedButton(Buttons[J]).Glyph.Width  := 0;
          TSpeedButton(Buttons[J]).Glyph.Height := 0;
          ImageList.GetBitmap(I, TSpeedButton(Buttons[J]).Glyph);
          Inc(I);
        end;
    finally
      ImageList.Free;
      Image.Free;
    end;
  end;
end;

procedure TConfigFrm.ToolBar_Skin_Initialize;
var
  I: integer;
  REC: TSearchRec;
  SkinFileName: string;
begin
  I := FindFirst(GetApplicationSkinFolderName + PathDelim + '*.*', faAnyFile, REC);
  while I = 0 do
  begin
    if ((REC.Attr and faDirectory) = 0) and (REC.Name[1] <> '.') then
    begin
      ToolBar_Skin.Items.Add(ExtractFileName(REC.Name));
    end;
    I := FindNext(REC);
  end;
  FindClose(REC);
end;

function TConfigFrm.GetSkinFileName: string;
begin
  if ToolBar_Skin.Items.Count > 0 then
  begin
    if ToolBar_Skin.ItemIndex < 0 then
    begin
      ToolBar_Skin.ItemIndex := 0;
    end;
    Result := GetApplicationSkinFolderName + PathDelim +
      ToolBar_Skin.Items[ToolBar_Skin.ItemIndex];
  end else
    Result := 'nil';
end;

function TConfigFrm.GetMethodStr: string;
begin
  case ConfigFrm.AddFrm_Method.ItemIndex of
    0: Result := '-m0';
    1: Result := '-m1';
    2: Result := '-m2';
    3: Result := '-m3';
    else
      Result := '-m3';
  end;
end;

function TConfigFrm.GetDictionaryStr: string;
begin
  case ConfigFrm.AddFrm_Method.ItemIndex of
    0: Result := '-d0';
    1: Result := '-d1';
    2: Result := '-d2';
    3: Result := '-d3';
    4: Result := '-d4';
    5: Result := '-d5';
    6: Result := '-d6';
    7: Result := '-d7';
    8: Result := '-d8';
    9: Result := '-d9';
    else
      Result := '-d5';
  end;
end;

function TConfigFrm.GetPriorityStr: string;
begin
  case General_Priority.ItemIndex of
    0: Result := '-pri0';
    1: Result := '-pri1';
    2: Result := '-pri2';
    3: Result := '-pri3';
    else
      Result := '-pri1';
  end;
end;

end.
