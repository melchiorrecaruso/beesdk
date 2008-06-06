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
    PasswordImage: TImage;
    PasswordLabel: TLabel;
    Password: TEdit;
    ConfirmPasswordLabel: TLabel;
    ConfirmPassword: TEdit;
    MaskPassword: TCheckBox;
    Bevel: TBevel;
    BtnClear: TBitBtn;
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MaskPasswordClick (Sender: TObject);
    procedure PasswordChange (Sender: TObject);
    procedure BtnClearClick (Sender: TObject);
  private
    { Private declarations }
    procedure SetConfirmPassword(Value: boolean);
  public
    { Public declarations }    
    procedure SetPassword(const Value: string);
  end;

implementation

uses
  BeeGui_Messages,
  BeeGui_SysUtils;

  procedure TPasswordFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_passwordfrm_loadlanguage.inc}
    {$I beegui_passwordfrm_loadproperty.inc}
  end;

  procedure TPasswordFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$IFDEF DEBUG}
      {$I beegui_passwordfrm_savelanguage.inc}
    {$ENDIF}
    {$I beegui_passwordfrm_saveproperty.inc}
  end;
  
  procedure TPasswordFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
          MessageDlg(rsWarning, rsPasswordNotConfirmed, mtWarning, [mbOk], 0);
          CanClose := False;
        end;
    end else
      CanClose := True;
  end;
  
  procedure TPasswordFrm.FormShow(Sender: TObject);
  begin
    SetConfirmPassword(MaskPassword.Checked);
    // Fix Laz bug
    if Constraints.MaxWidth = 0 then
    begin
      Constraints.MaxWidth  := Width;
      Constraints.MaxHeight := Height;
      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;
    end;
  end;

  procedure TPasswordFrm.MaskPasswordClick (Sender: TObject);
  begin
    SetConfirmPassword(MaskPassword.Checked);
  end;

  procedure TPasswordFrm.PasswordChange (Sender: TObject);
  begin
    ConfirmPassword.Text := '';
  end;

  procedure TPasswordFrm.SetConfirmPassword;
  begin
    if Value then
    begin
      Password.PasswordChar := '*';
      ConfirmPassword.Color := clWindow;
    end else
    begin
      Password.PasswordChar := #0;
      ConfirmPassword.Text  := '';
      ConfirmPassword.Color := clBtnFace;
    end;
    ConfirmPassword.Enabled := Value;
  end;

  procedure TPasswordFrm.SetPassword;
  begin
    Password.Text := Value;
    ConfirmPassword.Text := Value;
  end;

  procedure TPasswordFrm.BtnClearClick (Sender: TObject);
  begin
    Password.Clear;
    ConfirmPassword.Clear;
  end;
  
initialization

  {$I beegui_passwordfrm.lrs}

end.
