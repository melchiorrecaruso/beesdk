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

    BeeGui Config form.

    Modifyed:

    v1.0.1 build 9156 - 2005.07.16 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_ConfigFrm;

{$R-,Q-,S-}

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


  BeeGui_Config,
  BeeGui_Components;

const
  UPBTN2EXIT_CLASS    = 'TCheckBox';
  HIDEMAINFRM_CLASS   = 'TCheckBox';
  PPriority_CLASS     = 'TComboBox';
  CHECKFILETYPE_CLASS = 'TCheckBox';

  SKIN_IDENT          = 'ConfigFrm_Skin_Select';
  PPriority_IDENT     = 'ConfigFrm_Priority.ItemIndex';
  UPBTN2EXIT_IDENT    = 'ConfigFrm_General_Options_UpAsExit.Checked';
  HIDEMAINFRM_IDENT   = 'ConfigFrm_General_Options_HideMainFrm.Checked';
  CHECKFILETYPE_IDENT = 'ConfigFrm_General_Options_CheckFileType.Checked';

type
  TConfigCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SaveAndDestroy;
    procedure   LoadLanguage;
  end;

type
  TConfigFrm = class (TConfigCustomFrm)
    ConfigFrm_Tree: TTreeView;
    // add goup box
    AddFrm_OptionsGB: TGroupBox;
    AddFrm_cOption: TCheckBox;
    AddFrm_rOption: TCheckBox;
    AddFrm_sOption: TCheckBox;
    AddFrm_tOption: TCheckBox;
    AddFrm_pOption: TCheckBox;
    AddFrm_MethodGB: TGroupBox;
    AddFrm_Method: TComboBox;
    AddFrm_DictionaryGB: TGroupBox;
    AddFrm_Dictionary: TComboBox;
    // extract group box
    ExtractFrm_cOption: TCheckBox;
    ExtractFrm_ExtrOverWrite_: TGroupBox;
    ExtractFrm_ExtrOverWrite: TComboBox;
    ExtractFrm_ExtrOptions: TGroupBox;
    ExtractFrm_ExtrDirName: TCheckBox;
    // skin group box
    ConfigFrm_Skin_SelectGB: TGroupBox;
    ConfigFrm_Skin_Select_Image: TImage;
    // general group box
    ConfigFrm_General_OptionsGB: TGroupBox;
    ConfigFrm_General_Options_UpAsExit: TCheckBox;
    ConfigFrm_PriorityGB: TGroupBox;
    ConfigFrm_Priority: TComboBox;
    ConfigFrm_Btns_ListView: TListView;
    ConfigFrm_Btns_Images: TImageList;
    ConfigFrm_Btns_SelectLabel: TLabel;
    ConfigFrm_General_Options_HideMainFrm: TCheckBox;
    PasswordFrm_MaskKey: TCheckBox;
    // notebook
    ConfigFrm_Notebook: TNotebook;
    ConfigFrm_Notebook_Add: TGroupBox;
    ConfigFrm_Notebook_Skin: TGroupBox;
    ConfigFrm_Notebook_General: TGroupBox;
    ConfigFrm_Notebook_Extract: TGroupBox;
    // buttons
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;

    ConfigFrm_General_Options_CheckFileType: TCheckBox;
    AddFrm_aOption: TCheckBox;
    ConfigFrm_LanguageGB: TGroupBox;
    ConfigFrm_Skin_Select: TBeeGui_ComboBox;
    ConfigFrm_Language: TBeeGui_ComboBox;
    procedure FormCreate (Sender: TObject);
    procedure ConfigFrm_Skin_SelectClick (Sender: TObject);
    procedure ConfigFrm_Btns_ListViewClick (Sender: TObject);
    procedure ConfigFrm_TreeChange (Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    Popup:    TPopupMenu;
    procedure Load_SelectIMAGE (const fname: ansistring);
  public
    { Public declarations }
    procedure Get_Btns_PopupMenu;
    procedure Set_Btns_PopupMenu  (aPopup: TPopupMenu);
  end;

  procedure LoadBtnsSkin (var BTNs: array of TControl);

var
  ConfigFrm: TConfigFrm;

var
  BeeGui_Configuration: TBeeGui_Configuration;
  BeeGui_Language: TBeeGui_Language;

  procedure BeeGui_OpenRegistry;
  procedure BeeGui_CloseRegistry;
  procedure BeeGui_UpdateLanguage;

implementation

uses
  BeeGui_WinOS,
  BeeGui_Common;

{$R *.DFM}

/// TConfigCustomFrm

  constructor TConfigCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TConfigCustomFrm.Destroy;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    inherited Destroy;
  end;

  procedure TConfigCustomFrm.SaveAndDestroy;
  var
    I: integer;
  begin
    for I := 0 to ComponentCount - 1 do
       BeeGui_Configuration.Save_TControl (TControl (Components [I]));
    Destroy;
  end;

  procedure TConfigCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TConfigFrm

  procedure TConfigFrm.FormCreate (Sender: TObject);
  begin
    ConfigFrm_Tree.Items [0].Selected := True;
    ConfigFrm_Skin_SelectClick (Self);
  end;

  procedure TConfigFrm.ConfigFrm_TreeChange (Sender: TObject; Node: TTreeNode);
  begin
    ConfigFrm_Notebook.PageIndex := ConfigFrm_Tree.Selected.AbsoluteIndex;
  end;

  procedure TConfigFrm.ConfigFrm_Skin_SelectClick (Sender: TObject);
  var
    I: integer;
    T: TSearchRec;
  begin
    I := 0;
    if FindFirst (ExtractFilePath (ParamStr (0)) + SKIN_FOLDER_MASK, faAnyFile - faDirectory, T) = 0 then
      repeat
        if I = ConfigFrm_Skin_Select.ItemIndex then
          Load_SelectIMAGE (ExtractFilePath (ParamStr (0)) + IncludeTrailingBackslash (SKIN_FOLDER) + T.Name);

        Inc (I);
      until (not (FindNext (T) = 0));
    FindClose (T);
  end;

  procedure TConfigFrm.Load_SelectIMAGE;
  var
    b1, b2 : TBitmap;
    x, y, i: integer;
  begin
    b1 := TBitmap.Create;
    b2 := TBitmap.Create;
    try
      b1.LoadFromFile (fname);
      b2.Height := b1.Height;
      b2.Width  := b2.Height * 8;

      for i := 0 to 7 do
        for y := 0 to b1.Height -1 do
          for x := 0 to b1.Height -1 do
            b2.Canvas.Pixels [i * b1.Height + x, y] := b1.Canvas.Pixels [i * 2 * b1.Height + x, y];

      ConfigFrm_Skin_Select_Image.Width  := b2.Width;
      ConfigFrm_Skin_Select_Image.Height := b2.Height;
      ConfigFrm_Skin_Select_Image.Canvas.Draw (0, 0, b2);
    finally
      b1.Destroy;
      b2.Destroy;
    end;
  end;

  procedure TConfigFrm.Set_Btns_PopupMenu;
  var
    I: integer;
    P: integer;
    lvItem: TListItem;
    newCaption: string;
  begin
    Popup := aPopup;
    for I := 0 to Popup.Items.Count - 1 do
    begin
      newCaption := Popup.Items [I].Caption;

      P :=  Pos ('&', newCaption);
      while P > 0 do
      begin
        Delete (newCaption, P, 1);
        P :=  Pos ('&', newCaption);
      end;

      lvItem := ConfigFrm_Btns_ListView.Items.Add;
      lvItem.Caption := newCaption;
      if Popup.Items [I].Checked then
        lvItem.ImageIndex := 0
      else
        lvItem.ImageIndex := 1;
    end;
  end;

  procedure TConfigFrm.Get_Btns_PopupMenu;
  var
    I: integer;
  begin
    if Assigned (Popup) then
      for I := 0 to Popup.Items.Count - 1 do
      begin
        Popup.Items [I].Checked := (ConfigFrm_Btns_ListView.Items [I].ImageIndex = 0);
      end;
  end;

  procedure TConfigFrm.ConfigFrm_Btns_ListViewClick (Sender: TObject);
  begin
    if ConfigFrm_Btns_ListView.SelCount = 0 then Exit;

    if ConfigFrm_Btns_ListView.Selected.ImageIndex = 1 then
      ConfigFrm_Btns_ListView.Selected.ImageIndex := 0
    else
      ConfigFrm_Btns_ListView.Selected.ImageIndex := 1;
  end;

/// functions and procedures

  procedure LoadBtnsSkin;
  var
    fName: ansistring;
    fIndex: integer;
    I, J: integer;
    Image: TBitmap;
    ImageList: TImageList;
    T: TSearchRec;
  begin
    fIndex := BeeGui_Configuration.GetIntegerValue (TBeeGui_ComboBox.ClassName, SKIN_IDENT + '.ItemIndex', 0);
    fName  := '';

    I := 0;
    if FindFirst (ExtractFilePath (ParamStr (0)) + SKIN_FOLDER_MASK, faAnyFile - faDirectory, T) = 0 then
      repeat
        if I = fIndex then
          fName := ExtractFilePath (ParamStr (0)) + IncludeTrailingBackslash (SKIN_FOLDER) + T.Name;

        Inc (I);
      until (not (FindNext (T) = 0));
    FindClose (T);

    if not FileExists (fname) then
    begin
      if FindFirst (ExtractFilePath (ParamStr (0)) + SKIN_FOLDER_MASK, faAnyFile - faDirectory, T) = 0 then
        fname  := ExtractFilePath (ParamStr (0)) + IncludeTrailingBackslash (SKIN_FOLDER) + T.Name;
    end;

    if FileExists (fname) then
    begin
      Image := TBitmap.Create;
      ImageList := TImageList.Create (nil);
      try
        Image.LoadFromFile (fname);
        ImageList.Height := Image.Height;
        ImageList.Width := Image.Height * 2;
        ImageList.BkColor:= clFuchsia;
        ImageList.Add (Image, nil);

        I := 0;
        for J := 0 to High (Btns) do
        if (Btns [J].ClassType = TSpeedButton)then
        begin
          TSpeedButton (Btns [J]).Glyph.FreeImage;
          TSpeedButton (Btns [J]).Glyph.Width  := 0;
          TSpeedButton (Btns [J]).Glyph.Height := 0;
          ImageList.GetBitmap (I, TSpeedButton (Btns [J]).Glyph);
          Inc (I);
        end;
      finally
        ImageList.Free;
        Image.Free;
      end;
    end;
  end;

/// BeeGui Registry

  procedure BeeGui_OpenRegistry;
  var
    I: integer;
    T: TSearchRec;
    FileIndex: integer;
    FileName: string;
  begin
    FileName :=
      IncludeDelimiter (BeeGui_GetWin_AppDataDir) +
        IncludeDelimiter ('BeeGui')      +
          IncludeDelimiter (CONFIGURATION_FOLDER) +
            'configuration.ini';
    // ---
    if (not FileExists(FileName)) and FileExists(ExtractFilePath(ParamStr(0)) + 'BeeGui.ini') then
    begin
      CopyFile (PChar(ExtractFilePath(ParamStr(0)) + 'BeeGui.ini'), PChar(FileName), False);
    end;
    // ---
    BeeGui_Configuration := TBeeGui_Configuration.Create (FileName);

    FileIndex := BeeGui_Configuration.GetIntegerValue (TBeeGui_ComboBox.ClassName, 'ConfigFrm_Language.ItemIndex', 0);
    FileName  := '';

    I := 0;
    if FindFirst (ExtractFilePath (ParamStr (0)) + LANGUAGE_FOLDER_MASK, faAnyFile - faDirectory, T) = 0 then
      repeat
        if I = FileIndex then
        begin
          FileName := ExtractFilePath (ParamStr (0)) + IncludeTrailingBackslash (LANGUAGE_FOLDER) + T.Name;
          Break;
        end;

        Inc (I);
      until (FindNext (T) <> 0);
    FindClose (T);

    if Length (FileName) = 0 then
      FileName := ExtractFilePath (ParamStr (0)) + IncludeTrailingBackslash (LANGUAGE_FOLDER) + 'English.ini';

    BeeGui_Language := TBeeGui_Language.Create (FileName);
  end;

  procedure BeeGui_CloseRegistry;
  begin
    if BeeGui_Configuration <> nil then
      FreeAndNil (BeeGui_Configuration);

    if BeeGui_Language <> nil then
      FreeAndNil (BeeGui_Language);
  end;

  procedure BeeGui_UpdateLanguage;
  var
    I: integer;
    T: TSearchRec;
    FileIndex: integer;
    FileName: string;
  begin
    FileIndex := BeeGui_Configuration.GetIntegerValue (TBeeGui_ComboBox.ClassName, 'ConfigFrm_Language.ItemIndex', 0);
    FileName  := '';

    I := 0;
    if FindFirst (ExtractFilePath (ParamStr (0)) + LANGUAGE_FOLDER_MASK, faAnyFile - faDirectory, T) = 0 then
      repeat
        if I = FileIndex then
        begin
          FileName := ExtractFilePath (ParamStr (0)) + IncludeTrailingBackslash (LANGUAGE_FOLDER) + T.Name;
          Break;
        end;

        Inc (I);
      until (FindNext (T) <> 0);
    FindClose (T);

    if Length (FileName) = 0 then
      FileName := ExtractFilePath (ParamStr (0)) + IncludeTrailingBackslash (LANGUAGE_FOLDER) + 'English.ini';

    BeeGui_Language.Rename (FileName, True);
  end;

initialization
  BeeGui_OpenRegistry;

finalization
  BeeGui_CloseRegistry;

end.
