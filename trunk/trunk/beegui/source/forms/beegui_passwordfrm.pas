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

unit BeeGui_PasswordFrm;

{$I Compiler.inc}

interface

uses
  Forms,
  Classes,
  Dialogs,
  Buttons,
  SysUtils,
  StdCtrls,
  Graphics,
  ExtCtrls,
  Controls,
  BeeGui_FormStorage;

type
  TPasswordFrm = class(TForm)
    PasswordFrm_Messages: TComboBox;
    PasswordFrm_Image: TImage;
    // --
    PasswordFrm_Panel: TPanel;
    PasswordFrm_Key: TEdit;
    PasswordFrm_KeyLabel: TLabel;
    PasswordFrm_Confirm_KeyLabel: TLabel;
    PasswordFrm_Confirm_Key: TEdit;
    PasswordFrm_MaskKey: TCheckBox;
    // --
    BtnCancel: TBitBtn;
    BtnClear: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure PasswordFrm_MaskKeyClick(Sender: TObject);
    procedure PasswordFrm_KeyChange(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
  private
    { Private declarations }
    FFormStorage: TFormStorage;
    procedure ConfirmKey(Value: boolean);
  public
    { Public declarations }
    procedure SetKey(const Value: string);
  end;

var
  PasswordFrm: TPasswordFrm;

implementation

{$R *.dfm}

uses
  BeeGui_PasswordFrmRes,
  BeeGui_SysUtils;

// TPasswordFrm class

procedure TPasswordFrm.FormCreate(Sender: TObject);
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SPasswordFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SPasswordFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SPasswordFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
end;

procedure TPasswordFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SPasswordFrm_PropertyFull
  else
    FFormStorage.Properties := _SPasswordFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
end;

procedure TPasswordFrm.FormShow(Sender: TObject);
begin
  ConfirmKey(PasswordFrm_MaskKey.Checked);
end;

procedure TPasswordFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    if not PasswordFrm_MaskKey.Checked then
      CanClose := True
    else
    if CompareStr(PasswordFrm_Key.Text, PasswordFrm_Confirm_Key.Text) = 0 then
      CanClose := True
    else
    begin
      MessageDlg(PasswordFrm_Messages.Items[_CPasswordFrm_PassNotConfirm],
        mtWarning, [mbOK], 0);
      CanClose := False;
    end;
  end;
end;

procedure TPasswordFrm.PasswordFrm_MaskKeyClick(Sender: TObject);
begin
  ConfirmKey(PasswordFrm_MaskKey.Checked);
end;

procedure TPasswordFrm.PasswordFrm_KeyChange(Sender: TObject);
begin
  PasswordFrm_Confirm_Key.Text := '';
end;

procedure TPasswordFrm.ConfirmKey(Value: boolean);
begin
  if Value then
  begin
    PasswordFrm_Key.PasswordChar  := '*';
    PasswordFrm_Confirm_Key.Color := clWindow;
  end else
  begin
    PasswordFrm_Key.PasswordChar  := #0;
    PasswordFrm_Confirm_Key.Text  := '';
    PasswordFrm_Confirm_Key.Color := clBtnFace;
  end;
  PasswordFrm_Confirm_Key.Enabled := Value;
end;

procedure TPasswordFrm.SetKey(const Value: string);
begin
  PasswordFrm_Key.Text := Value;
  PasswordFrm_Confirm_Key.Text := Value;
end;

procedure TPasswordFrm.BtnClearClick(Sender: TObject);
begin
  PasswordFrm_Key.Clear;
  PasswordFrm_Confirm_Key.Clear;
end;

end.
