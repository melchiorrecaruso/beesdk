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

    Forms routines

    Modifyed:

    v1.0.1 build 9155 - 2005.07.15 Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 Melchiorre Caruso;
    v1.0.2 build 0250 - 2005.11.13 Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 Melchiorre Caruso;
}

unit BeeGui_Config;

{$R-,Q-,S-}

interface

uses
  Forms,
  Menus,
  Classes,
  Dialogs,
  Buttons,
  IniFiles,
  STDCtrls,
  SysUtils,
  ExtCTRLS,
  COMCtrls,
  Controls,
  Graphics,

  BeeGui_WinOS,
  BeeGui_Common,
  BeeGui_Components;

type

  TBeeGui_Configuration = class (TMemIniFile)
  private
    procedure   Load_TListView (List:TListView);
    procedure   Save_TListView (List:TListView);
    procedure   Load_TFontDialog (Dialog: TFontDialog);
    procedure   Save_TFontDialog (Dialog: TFontDialog);
    procedure   Load_TOpenDialog (Dialog: TOpenDialog);
    procedure   Save_TOpenDialog (Dialog: TOpenDialog);
    procedure   Load_TSaveDialog (Dialog: TSaveDialog);
    procedure   Save_TSaveDialog (Dialog: TSaveDialog);
    procedure   Load_TComboBox (CB: TComboBox);
    procedure   Save_TComboBox (CB: TComboBox);
    procedure   Load_TBeeGui_ComboBox (CB: TBeeGui_ComboBox);
    procedure   Save_TBeeGui_ComboBox (CB: TBeeGui_ComboBox);
    procedure   Load_TRadioButton (RB: TRadioButton);
    procedure   Save_TRadioButton (RB: TRadioButton);
    procedure   Load_TCheckBox (CB: TCheckBox);
    procedure   Save_TCheckBox (CB: TCheckBox);
    procedure   Load_TEdit (Edit: TEdit);
    procedure   Save_TEdit (Edit: TEdit);
    //procedure   Load_TMainMenu (Menu: TMainMenu);
    //procedure   Save_TMainMenu (Menu: TMainMenu);
    procedure   Load_TMenuItem (Item: TMenuItem);
    procedure   Save_TMenuItem (Item: TMenuItem);
    procedure   Load_TBeeGui_ListView (List: TBeeGui_ListView);
    procedure   Save_TBeeGui_ListView (List: TBeeGui_ListView);
    procedure   Load_TBeeGui_ListView_Add (List: TBeeGui_ListView_Add);
    procedure   Save_TBeeGui_ListView_Add (List: TBeeGui_ListView_Add);
  public
    constructor Create (const FileName: string);
    procedure   Save;
    procedure   Save_TForm (Form: TForm);
    procedure   Load_TForm (Form: TForm);
    procedure   Save_TControl (Control:TControl);
    procedure   Load_TControl (Control:TControl);
    function    GetStrValue (const Section,Ident: string; Default: string): string;
    function    GetBoolValue (const Section,Ident: string; Default: boolean): boolean;
    function    GetIntegerValue (const Section,Ident: string; Default: integer): integer;
  end;

  TBeeGui_Language = class (TMemIniFile)
  private
    procedure   Load_TBitBtn (Btn: TBitBtn);
    procedure   Load_TSpeedButton (Btn: TSpeedButton);
    procedure   Load_TListView (List: TListView);
    procedure   Load_TPageControl (PageC: TPageControl);
    procedure   Load_TTreeView (Tree: TTreeView);
    procedure   Load_TLabel (L: TLabel);
    procedure   Load_TGroupBox (GB: TGroupBox);
    procedure   Load_TOpenDialog (Dialog: TOpenDialog);
    procedure   Load_TSaveDialog (Dialog: TSaveDialog);
    procedure   Load_TComboBox (CB: TComboBox);
    procedure   Load_TRadioButton (RB: TRadioButton);
    procedure   Load_TCheckBox (CB: TCheckBox);
    //procedure   Load_TMainMenu (Menu: TMainMenu);
    procedure   Load_TMenuItem (Item: TMenuItem);
    procedure   Load_TBeeGui_ListView (List: TBeeGui_ListView);
    procedure   Load_TBeeGui_ListView_Add (List: TBeeGui_ListView_Add);
  public
    constructor Create (const FileName: string);
    procedure   Save;
    procedure   Load_TForm (Form: TForm);
    procedure   Load_TControl (Control:TControl);
    function    GetStrValue (const Section,Ident: string; Default: string): string;
    function    GetBoolValue (const Section,Ident: string): boolean;
    function    GetIntegerValue (const Section,Ident: string): integer;
  end;

implementation

/// TBeeGui_Configuration

  constructor TBeeGui_Configuration.Create;
  begin
    ForceDirectories (ExtractFilePath (FileName));
    inherited Create (FileName);
  end;

  procedure TBeeGui_Configuration.Save;
  var
    S: TStringList;
  begin
    S := TStringList.Create;
    try
      GetStrings (S);
      S.SaveToFile (FileName);
    finally
      S.Free;
    end;
  end;

  procedure TBeeGui_Configuration.Load_TBeeGui_ListView (List: TBeeGui_ListView);
  var
    I: integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
      List.Columns [I].Width := ReadInteger (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Width', List.Columns [I].Width);
    List.ColumnSort := TBeeGui_ListView_Sort (ReadInteger (List.ClassName, List.Name + '.ColumnSort', Integer (List.ColumnSort)));
    List.DirectionSort := ReadBool (List.ClassName, List.Name + '.DirectionSort', List.DirectionSort);
    List.ListMode := ReadBool (List.ClassName, List.Name + '.ListMode', List.ListMode);
  end;

  procedure TBeeGui_Configuration.Save_TBeeGui_ListView (List: TBeeGui_ListView);
  var
    I: Integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
      WriteInteger (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Width', List.Columns [I].Width);
    WriteInteger (List.ClassName, List.Name + '.ColumnSort', Integer (List.ColumnSort));
    WriteBool (List.ClassName, List.Name + '.DirectionSort', List.DirectionSort);
    WriteBool (List.ClassName, List.Name + '.ListMode', List.ListMode);
  end;

  procedure TBeeGui_Configuration.Save_TMenuItem (Item: TMenuItem);
  begin
    WriteBool (TMenuItem.ClassName, Item.Name + '.Checked', Item.Checked);
  end;

  procedure TBeeGui_Configuration.Load_TMenuItem (Item: TMenuItem);
  begin
    Item.Checked := ReadBool (TMenuItem.ClassName, Item.Name + '.Checked', Item.Checked);
  end;

  //procedure TBeeGui_Configuration.Load_TMainMenu (Menu: TMainMenu);
  //var
  //  I: integer;
  //begin
  //  for I := 0 to Menu.Items.Count - 1 do
  //    Load_TMenuItem (Menu.Items [I]);
  //end;

  //procedure TBeeGui_Configuration.Save_TMainMenu (Menu: TMainMenu);
  //var
  //  I: integer;
  //begin
  //  for I := 0 to Menu.Items.Count - 1 do
  //    Save_TMenuItem (Menu.Items [I]);
  //end;

  procedure TBeeGui_Configuration.Save_TEdit (Edit: TEdit);
  begin
    WriteString (Edit.ClassName, Edit.Name + '.Text', Edit.Text);
  end;

  procedure TBeeGui_Configuration.Load_TEdit (Edit: TEdit);
  begin
    Edit.Text := ReadString (Edit.ClassName, Edit.Name + '.Text', Edit.Text);
  end;

  procedure TBeeGui_Configuration.Save_TCheckBox (CB: TCheckBox);
  begin
    WriteBool (CB.ClassName, CB.Name + '.Checked', CB.Checked);
  end;

  procedure TBeeGui_Configuration.Load_TCheckBox (CB: TCheckBox);
  begin
    CB.Checked := ReadBool (CB.ClassName, CB.Name + '.Checked', CB.Checked);
  end;

  procedure TBeeGui_Configuration.Save_TRadioButton (RB: TRadioButton);
  begin
    WriteBool (RB.ClassName, RB.Name + '.Checked', RB.Checked);
  end;

  procedure TBeeGui_Configuration.Load_TRadioButton (RB: TRadioButton);
  begin
    RB.Checked := ReadBool (RB.ClassName, RB.Name + '.Checked', RB.Checked);
  end;

  procedure TBeeGui_Configuration.Save_TBeeGui_ComboBox (CB: TBeeGui_ComboBox);
  begin
    WriteInteger (CB.ClassName, CB.Name + '.ItemIndex',  CB.ItemIndex);
  end;

  procedure TBeeGui_Configuration.Load_TBeeGui_ComboBox (CB: TBeeGui_ComboBox);
  begin
    CB.ItemIndex := ReadInteger (CB.ClassName, CB.Name + '.ItemIndex', 0);
  end;

  procedure TBeeGui_Configuration.Save_TComboBox (CB: TComboBox);
  begin
    WriteInteger (CB.ClassName, CB.Name + '.ItemIndex',  CB.ItemIndex);
  end;

  procedure TBeeGui_Configuration.Load_TComboBox (CB: TComboBox);
  begin
    CB.ItemIndex := ReadInteger (CB.ClassName, CB.Name + '.ItemIndex', 2);
  end;

  procedure TBeeGui_Configuration.Save_TSaveDialog (Dialog: TSaveDialog);
  begin
    WriteString (TCommonDialog.ClassName, Dialog.Name + '.InitialDir', Dialog.InitialDir);
  end;

  procedure TBeeGui_Configuration.Load_TSaveDialog (Dialog: TSaveDialog);
  begin
    Dialog.InitialDir := ReadString (TCommonDialog.ClassName, Dialog.Name + '.InitialDir', Dialog.InitialDir);
  end;

  procedure TBeeGui_Configuration.Save_TOpenDialog (Dialog: TOpenDialog);
  begin
    WriteString (TCommonDialog.ClassName, Dialog.Name + '.InitialDir', Dialog.InitialDir);
  end;

  procedure TBeeGui_Configuration.Load_TOpenDialog (Dialog: TOpenDialog);
  begin
    Dialog.InitialDir := ReadString (TCommonDialog.ClassName, Dialog.Name + '.InitialDir', Dialog.InitialDir);
  end;

  procedure TBeeGui_Configuration.Save_TFontDialog (Dialog: TFontDialog);
  begin
    WriteInteger (Dialog.ClassName, Dialog.Name + '.Charset', Integer (Dialog.Font.Charset));
    WriteInteger (Dialog.ClassName, Dialog.Name + '.Color'  , Integer (Dialog.Font.Color  ));
    WriteString  (Dialog.ClassName, Dialog.Name + '.Name'   ,          Dialog.Font.Name    );
    WriteInteger (Dialog.ClassName, Dialog.Name + '.Height' ,          Dialog.Font.Height  );
    WriteInteger (Dialog.ClassName, Dialog.Name + '.Pitch'  , Integer (Dialog.Font.Pitch  ));
    WriteInteger (Dialog.ClassName, Dialog.Name + '.Size'   ,          Dialog.Font.Size    );

    if (fsBold in Dialog.Font.Style) then
      WriteBool (Dialog.ClassName, Dialog.Name + '.Style[fsBold]', True)
    else
      WriteBool (Dialog.ClassName, Dialog.Name + '.Style[fsBold]', False);

    if (fsItalic in Dialog.Font.Style) then
      WriteBool (Dialog.ClassName, Dialog.Name + '.Style[fsItalic]', True)
    else
      WriteBool (Dialog.ClassName, Dialog.Name + '.Style[fsItalic]', False);

    if (fsUnderline in Dialog.Font.Style) then
      WriteBool (Dialog.ClassName, Dialog.Name + '.Style[fsUnderline]', True )
    else
      WriteBool (Dialog.ClassName, Dialog.Name + '.Style[fsUnderline]', False);

    if (fsStrikeOut in Dialog.Font.Style) then
      WriteBool (Dialog.ClassName, Dialog.Name + '.Style[fsStrikeOut]', True)
    else
      WriteBool (Dialog.ClassName, Dialog.Name + '.Style[fsStrikeOut]', False);
  end;

  procedure TBeeGui_Configuration.Load_TFontDialog (Dialog: TFontDialog);
  begin
    Dialog.Font.Charset := TFontCharSet (ReadInteger (Dialog.ClassName, Dialog.Name + '.Charset', Dialog.Font.Charset));
    Dialog.Font.Color   := TColor       (ReadInteger (Dialog.ClassName, Dialog.Name + '.Color', Dialog.Font.Color));
    Dialog.Font.Name    :=               ReadString  (Dialog.ClassName, Dialog.Name + '.Name', Dialog.Font.Name);
    Dialog.Font.Height  :=               ReadInteger (Dialog.ClassName, Dialog.Name + '.Height', Dialog.Font.Height);
    Dialog.Font.Pitch   := TFontPitch   (ReadInteger (Dialog.ClassName, Dialog.Name +'.Pitch', Integer (Dialog.Font.Pitch)));
    Dialog.Font.Size    :=               ReadInteger (Dialog.ClassName, Dialog.Name + '.Size', Dialog.Font.Size);

    if ReadBool (Dialog.ClassName, Dialog.Name + '.Style[fsBold]', False) then
      Dialog.Font.Style := Dialog.Font.Style + [fsBold]
    else
      Dialog.Font.Style := Dialog.Font.Style - [fsBold];

    if ReadBool (Dialog.ClassName, Dialog.Name + '.Style[fsItalic]', False) then
      Dialog.Font.Style := Dialog.Font.Style + [fsItalic]
    else
      Dialog.Font.Style := Dialog.Font.Style - [fsItalic];

    if ReadBool (Dialog.ClassName, Dialog.Name + '.Style[fsUnderline]', False) then
      Dialog.Font.Style := Dialog.Font.Style + [fsUnderline]
    else
      Dialog.Font.Style := Dialog.Font.Style - [fsUnderline];

    if ReadBool (Dialog.ClassName, Dialog.Name + '.Style[fsStrikeOut]', False) then
      Dialog.Font.Style := Dialog.Font.Style + [fsStrikeOut]
    else
      Dialog.Font.Style := Dialog.Font.Style - [fsStrikeOut];
  end;

  procedure TBeeGui_Configuration.Save_TListView (List: TListView);
  var
    I: Integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
      WriteInteger (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Width', List.Columns [I].Width);
  end;

  procedure TBeeGui_Configuration.Load_TListView (List: TListView);
  var
    I: integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
      List.Columns [I].Width := ReadInteger (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Width', List.Columns [I].Width);
  end;

  procedure TBeeGui_Configuration.Save_TBeeGui_ListView_Add (List: TBeeGui_ListView_Add);
  var
    I: integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
      WriteInteger (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Width', List.Columns [I].Width);
  end;

  procedure TBeeGui_Configuration.Load_TBeeGui_ListView_Add (List: TBeeGui_ListView_Add);
  var
    I: integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
      List.Columns [I].Width := ReadInteger (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Width', List.Columns [I].Width);
  end;

  function TBeeGui_Configuration.GetStrValue (const Section,Ident: ansistring; Default: ansistring): ansistring;
  begin
    Result := ReadString (Section, Ident, '')
  end;

  function TBeeGui_Configuration.GetBoolValue (const Section,Ident: ansistring; Default: boolean): boolean;
  begin
    Result := ReadBool (Section, Ident, False)
  end;

  function TBeeGui_Configuration.GetIntegerValue (const Section,Ident: ansistring; Default: integer): integer;
  begin
    Result := ReadInteger (Section, Ident, -1)
  end;

  procedure TBeeGui_Configuration.Save_TForm (Form: TForm);
  begin
    if (Form.WindowState = wsNormal) then
    begin
      if (Form.BorderStyle = bsSizeable) then
      begin
        WriteInteger (TForm.ClassName, Form.Name + '.Heigth', Form.Height);
        WriteInteger (TForm.ClassName, Form.Name + '.Width', Form.Width);
      end;
      WriteInteger (TForm.ClassName, Form.Name + '.Left', Form.Left);
      WriteInteger (TForm.ClassName, Form.Name + '.Top', Form.Top);
    end;
    WriteInteger (TForm.ClassName, Form.Name + '.WState', Integer (Form.WindowState));
  end;

  procedure TBeeGui_Configuration.Load_TForm (Form: TForm);
  begin
    if (Form.BorderStyle = bsSizeable) then
    begin
      Form.Height := ReadInteger (TForm.ClassName, Form.Name + '.Heigth', Form.Height);
      Form.Width  := ReadInteger (TForm.ClassName, Form.Name + '.Width', Form.Width);
      Form.WindowState := TWindowState (ReadInteger (TForm.ClassName, Form.Name + '.WState', 0));
    end;
    Form.Left := ReadInteger (TForm.ClassName, Form.Name + '.Left', Form.Left);
    Form.Top := ReadInteger (TForm.ClassName, Form.Name + '.Top', Form.Top);
  end;

  procedure TBeeGui_Configuration.Save_TControl (Control:TControl);
  begin
    if Control.ClassType = TMenuItem             then Save_TMenuItem            (TMenuItem            (Control)) else
    if Control.ClassType = TSaveDialog           then Save_TSaveDialog          (TSaveDialog          (Control)) else
    if Control.ClassType = TOpenDialog           then Save_TOpenDialog          (TOpenDialog          (Control)) else
    if Control.ClassType = TFontDialog           then Save_TFontDialog          (TFontDialog          (Control)) else
    if Control.ClassType = TCheckBox             then Save_TCheckBox            (TCheckBox            (Control)) else
    if Control.ClassType = TBeeGui_ComboBox      then Save_TBeeGui_ComboBox     (TBeeGui_ComboBox     (Control)) else
    if Control.ClassType = TComboBox             then Save_TComboBox            (TComboBox            (Control)) else
    if Control.ClassType = TEdit                 then Save_TEdit                (TEdit                (Control)) else
    if Control.ClassType = TRadioButton          then Save_TRadioButton         (TRadioButton         (Control)) else
    if Control.ClassType = TListView             then Save_TListView            (TListView            (Control)) else
    if Control.ClassType = TBeeGui_ListView      then Save_TBeeGui_ListView     (TBeeGui_ListView     (Control)) else
    if Control.ClassType = TBeeGui_ListView_Add  then Save_TBeeGui_ListView_Add (TBeeGui_ListView_Add (Control)) ;
  end;

  procedure TBeeGui_Configuration.Load_TControl (Control:TControl);
  begin
    if Control.ClassType = TMenuItem             then Load_TMenuItem            (TMenuItem            (Control)) else
    if Control.ClassType = TSaveDialog           then Load_TSaveDialog          (TSaveDialog          (Control)) else
    if Control.ClassType = TOpenDialog           then Load_TOpenDialog          (TOpenDialog          (Control)) else
    if Control.ClassType = TFontDialog           then Load_TFontDialog          (TFontDialog          (Control)) else
    if Control.ClassType = TCheckBox             then Load_TCheckBox            (TCheckBox            (Control)) else
    if Control.ClassType = TRadioButton          then Load_TRadioButton         (TRadioButton         (Control)) else
    if Control.ClassType = TBeeGui_ComboBox      then Load_TBeeGui_ComboBox     (TBeeGui_ComboBox     (Control)) else
    if Control.ClassType = TComboBox             then Load_TComboBox            (TComboBox            (Control)) else
    if Control.ClassType = TEdit                 then Load_TEdit                (TEdit                (Control)) else
    if Control.ClassType = TListView             then Load_TListView            (TListView            (Control)) else
    if Control.ClassType = TBeeGui_ListView      then Load_TBeeGui_ListView     (TBeeGui_ListView     (Control)) else
    if Control.ClassType = TBeeGui_ListView_Add  then Load_TBeeGui_ListView_Add (TBeeGui_ListView_Add (Control)) ;
  end;

/// TBeeGui_Language

  constructor TBeeGui_Language.Create;
  begin
    ForceDirectories (ExtractFilePath (FileName));
    inherited Create (FileName);
  end;

  procedure TBeeGui_Language.Save;
  var
    S: TStringList;
  begin
    S := TStringList.Create;
    try
      GetStrings (S);
      S.SaveToFile (FileName);
    finally
      S.Free;
    end;
  end;

  procedure TBeeGui_Language.Load_TBeeGui_ListView (List: TBeeGui_ListView);
  var
    I: integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
    begin
      if ValueExists (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption') then
        List.Columns [I].Caption := ReadString (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption', List.Columns [I].Caption)
      else
        WriteString (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption', List.Columns [I].Caption);
     end;
  end;

  procedure TBeeGui_Language.Load_TMenuItem (Item: TMenuItem);
  begin
    if ValueExists (TMenuItem.ClassName, Item.Name + '.Caption') then
      Item.Caption := ReadString (TMenuItem.ClassName, Item.Name + '.Caption', Item.Caption)
    else
      WriteString (TMenuItem.ClassName, Item.Name + '.Caption', Item.Caption);
  end;

  //procedure TBeeGui_Language.Load_TMainMenu (Menu: TMainMenu);
  //var
  //  I: integer;
  //begin
  //  for I := 0 to Menu.Items.Count - 1 do
  //    Load_TMenuItem (Menu.Items [I]);
  //end;

  procedure TBeeGui_Language.Load_TCheckBox (CB: TCheckBox);
  begin
    if ValueExists (CB.ClassName, CB.Name + '.Caption') then
      CB.Caption := ReadString (CB.ClassName, CB.Name + '.Caption', CB.Caption)
    else
      WriteString (CB.ClassName, CB.Name + '.Caption', CB.Caption);
  end;

  procedure TBeeGui_Language.Load_TRadioButton (RB: TRadioButton);
  begin
    if ValueExists (RB.ClassName, RB.Name + '.Caption') then
      RB.Caption := ReadString (RB.ClassName, RB.Name + '.Caption', RB.Caption)
    else
      WriteString (RB.ClassName, RB.Name + '.Caption',  RB.Caption);
  end;

  procedure TBeeGui_Language.Load_TComboBox (CB: TComboBox);
  var
    I: integer;
  begin
    for I := 0 to CB.Items.Count - 1 do
      if ValueExists (CB.ClassName, CB.Name + '.Items[' + IntToStr(I) + ']') then
        CB.Items [I] := ReadString (CB.ClassName, CB.Name + '.Items[' + IntToStr(I) + ']', CB.Items [I])
      else
        WriteString (CB.ClassName, CB.Name + '.Items[' + IntToStr(I) + ']', CB.Items [I]);
  end;

  procedure TBeeGui_Language.Load_TSaveDialog (Dialog: TSaveDialog);
  begin
    if ValueExists (TCommonDialog.ClassName, Dialog.Name + '.Title') then
      Dialog.Title := ReadString (TCommonDialog.ClassName, Dialog.Name + '.Title', Dialog.Title)
    else
      WriteString (TCommonDialog.ClassName, Dialog.Name + '.Title', Dialog.Title);
  end;

  procedure TBeeGui_Language.Load_TOpenDialog (Dialog: TOpenDialog);
  begin
    if ValueExists (TCommonDialog.ClassName, Dialog.Name + '.Title') then
      Dialog.Title := ReadString (TCommonDialog.ClassName, Dialog.Name + '.Title', Dialog.Title)
    else
      WriteString (TCommonDialog.ClassName, Dialog.Name + '.Title', Dialog.Title);
  end;

  procedure TBeeGui_Language.Load_TGroupBox (GB: TGroupBox);
  begin
    if ValueExists (GB.ClassName, GB.Name + '.Caption') then
     GB.Caption := ReadString (GB.ClassName, GB.Name + '.Caption', GB.Caption)
    else
     WriteString (GB.ClassName, GB.Name + '.Caption', GB.Caption);
  end;

  procedure TBeeGui_Language.Load_TLabel (L: TLabel);
  begin
    if ValueExists (TLabel.ClassName, L.Name + '.Caption') then
      L.Caption := ReadString (TLabel.ClassName, L.Name + '.Caption', L.Caption)
    else
      WriteString (TLabel.ClassName, L.Name + '.Caption', L.Caption);
  end;

  procedure TBeeGui_Language.Load_TTreeView (Tree: TTreeView);
  var
    I: integer;
  begin
    for I := 0 to Tree.Items.Count - 1 do
      if ValueExists (Tree.ClassName, Tree.Name + '.Items[' + IntToStr (I) + '].Text') then
        Tree.Items [I].Text := ReadString (Tree.ClassName, Tree.Name + '.Items[' + IntToStr (I) + '].Text', Tree.Items [I].Text)
      else
        WriteString (Tree.ClassName, Tree.Name + '.Items[' + IntToStr (I) + '].Text', Tree.Items [I].Text);
  end;

  procedure TBeeGui_Language.Load_TPageControl (PageC: TPageControl);
  var
    I: Integer;
  begin
    for I := 0 to PageC.PageCount - 1 do
      if ValueExists (TPageControl.ClassName, PageC.Pages [i].Name + '.Caption') then
        PageC.Pages [I].Caption := ReadString (TPageControl.ClassName, PageC.Pages [i].Name + '.Caption', PageC.Pages [i].Caption)
      else
        WriteString (TPageControl.ClassName, PageC.Pages [i].Name + '.Caption', PageC.Pages [i].Caption);
  end;

  procedure TBeeGui_Language.Load_TListView (List: TListView);
  var
    I: integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
    begin
      if ValueExists (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption') then
        List.Columns [I].Caption := ReadString (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption', List.Columns [I].Caption)
      else
        WriteString (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption', List.Columns [I].Caption);
    end;
  end;

  procedure TBeeGui_Language.Load_TBeeGui_ListView_Add (List: TBeeGui_ListView_Add);
  var
    I: integer;
  begin
    for I := 0 to List.Columns.Count - 1 do
    begin
      if ValueExists (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption') then
        List.Columns [I].Caption := ReadString (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption', List.Columns [I].Caption)
      else
        WriteString (List.ClassName, List.Name + '.Column[' + IntToStr(I) + '].Caption', List.Columns [I].Caption);
    end;
  end;

  procedure TBeeGui_Language.Load_TBitBtn (Btn: TBitBtn);
  begin
    if ValueExists (TBitBtn.ClassName, Btn.Name + '.Caption') then
      Btn.Caption := ReadString (TBitBtn.ClassName, Btn.Name + '.Caption', Btn.Caption)
    else
      WriteString(TBitBtn.ClassName, Btn.Name + '.Caption', Btn.Caption);
  end;

  procedure TBeeGui_Language.Load_TSpeedButton (Btn: TSpeedButton);
  begin
    if ValueExists (TSpeedButton.ClassName, Btn.Name + '.Caption') then
      Btn.Caption := ReadString (TSpeedButton.ClassName, Btn.Name + '.Caption', Btn.Caption)
    else
      WriteString(TSpeedButton.ClassName, Btn.Name + '.Caption', Btn.Caption);
  end;

  function TBeeGui_Language.GetStrValue (const Section,Ident: string; Default: string): string;
  begin
    Result := ReadString (Section, Ident, '')
  end;

  function TBeeGui_Language.GetBoolValue (const Section,Ident: ansistring): boolean;
  begin
    Result := ReadBool (Section, Ident, False)
  end;

  function TBeeGui_Language.GetIntegerValue (const Section,Ident: ansistring): integer;
  begin
    Result := ReadInteger (Section, Ident, -1)
  end;

  procedure TBeeGui_Language.Load_TForm (Form: TForm);
  begin
    if ValueExists (TForm.ClassName, Form.Name + '.Caption') then
      Form.Caption := ReadString (TForm.ClassName, Form.Name + '.Caption', Form.Caption)
    else
      WriteString (TForm.ClassName, Form.Name + '.Caption', Form.Caption);
  end;

  procedure TBeeGui_Language.Load_TControl (Control:TControl);
  begin
    if Control.ClassType = TSpeedButton          then Load_TSpeedButton         (TSpeedButton         (Control)) else
    if Control.ClassType = TBitBtn               then Load_TBitBtn              (TBitBtn              (Control)) else
    if Control.ClassType = TMenuItem             then Load_TMenuItem            (TMenuItem            (Control)) else
    if Control.ClassType = TLabel                then Load_TLabel               (TLabel               (Control)) else
    if Control.ClassType = TSaveDialog           then Load_TSaveDialog          (TSaveDialog          (Control)) else
    if Control.ClassType = TOpenDialog           then Load_TOpenDialog          (TOpenDialog          (Control)) else
    if Control.ClassType = TGroupBox             then Load_TGroupBox            (TGroupBox            (Control)) else
    if Control.ClassType = TCheckBox             then Load_TCheckBox            (TCheckBox            (Control)) else
    if Control.ClassType = TRadioButton          then Load_TRadioButton         (TRadioButton         (Control)) else
    if Control.ClassType = TComboBox             then Load_TComboBox            (TComboBox            (Control)) else
    if Control.ClassType = TTreeView             then Load_TTreeView            (TTreeView            (Control)) else
    if Control.ClassType = TPageControl          then Load_TPageControl         (TPageControl         (Control)) else
    if Control.ClassType = TListView             then Load_TListView            (TListView            (Control)) else
    if Control.ClassType = TBeeGui_ListView      then Load_TBeeGui_ListView     (TBeeGui_ListView     (Control)) else
    if Control.ClassType = TBeeGui_ListView_Add  then Load_TBeeGui_ListView_Add (TBeeGui_ListView_Add (Control)) ;
  end;

end.
