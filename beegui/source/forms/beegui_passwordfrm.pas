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

    BeeGui Password form.

  Modifyed:
  
}

unit BeeGui_PasswordFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Classes,
  Dialogs,
  Buttons,
  IniFiles,
  SysUtils,
  StdCtrls,
  Graphics,
  ExtCtrls,
  Controls,
  LResources;

type

  { TPasswordFrm }

  TPasswordFrm = class(TForm)
    ConfirmPasswordLabel: TLabel;
    NoteLabel: TLabel;
    Password:  TEdit;
    ConfirmPassword: TEdit;
    MaskPassword: TCheckBox;
    Bevel:     TBevel;
    BtnClear:  TBitBtn;
    BtnOk:     TBitBtn;
    BtnCancel: TBitBtn;
    PasswordLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MaskPasswordClick(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
  private
    { Private declarations }
    procedure SetConfirmPassword(Value: boolean);
  public
    { Public declarations }
    procedure SetPassword(const Value: string);
  public
    { Public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  end;

var
  PasswordFrm: TPasswordFrm;

implementation

uses
  BeeGui_Consts,
  BeeGui_Messages,
  BeeGui_SysUtils;

{ TPasswordFrm }

  {$I beegui_passwordfrm_saveproperty.inc}
  {$I beegui_passwordfrm_loadproperty.inc}
  {$I beegui_passwordfrm_savelanguage.inc}
  {$I beegui_passwordfrm_loadlanguage.inc}

procedure TPasswordFrm.FormCreate(Sender: TObject);
begin
  LoadLanguage;
  LoadProperty;
end;

procedure TPasswordFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    {$IFDEF SAVELANGUAGE}
  SaveLanguage;
    {$ENDIF}
  SaveProperty;
end;

procedure TPasswordFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    if not MaskPassword.Checked then
      CanClose := True
    else
    if CompareStr(Password.Text, ConfirmPassword.Text) = 0 then
      CanClose := True
    else
    begin
      MessageDlg(rsWarning, rseConfirmPassword, mtWarning, [mbOK], 0);
      CanClose := False;
    end;
  end
  else
    CanClose := True;
end;

procedure TPasswordFrm.FormShow(Sender: TObject);
begin
  SetConfirmPassword(MaskPassword.Checked);
end;

procedure TPasswordFrm.MaskPasswordClick(Sender: TObject);
begin
  SetConfirmPassword(MaskPassword.Checked);
end;

procedure TPasswordFrm.PasswordChange(Sender: TObject);
begin
  ConfirmPassword.Text := '';
end;

procedure TPasswordFrm.SetConfirmPassword;
begin
  if Value then
  begin
    Password.PasswordChar := '*';
    ConfirmPassword.Color := clWindow;
  end
  else
  begin
    Password.PasswordChar := #0;
    ConfirmPassword.Text  := '';
    ConfirmPassword.Color := clBtnFace;
  end;
  ConfirmPassword.Enabled := Value;
  if ConfirmPassword.CanFocus then
    ConfirmPassword.SetFocus;
  if Password.CanFocus then
    Password.SetFocus;
end;

procedure TPasswordFrm.SetPassword;
begin
  Password.Text := Value;
  ConfirmPassword.Text := Value;
end;

procedure TPasswordFrm.BtnClearClick(Sender: TObject);
begin
  Password.Clear;
  ConfirmPassword.Clear;
end;

initialization

  {$I beegui_passwordfrm.lrs}

end.
