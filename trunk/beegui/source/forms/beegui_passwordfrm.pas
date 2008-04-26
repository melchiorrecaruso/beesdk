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
    Bevel: TBevel;
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    ConfirmKey: TEdit;
    ConfirmKeyLabel: TLabel;
    Key: TEdit;
    KeyImage: TImage;
    KeyLabel: TLabel;
    MaskKey: TCheckBox;
    BtnClear: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnClearClick (Sender: TObject);
    procedure MaskKeyClick (Sender: TObject);
    procedure KeyChange (Sender: TObject);
  private
    { Private declarations }
    procedure SetConfirmKey(Value: boolean);
  public
    { Public declarations }    
    procedure SetKey(const Value: string);
  end;
  
var
  PasswordFrm: TPasswordFrm;

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
    {$I beegui_passwordfrm_savelanguage.inc}
    {$I beegui_passwordfrm_saveproperty.inc}
  end;
  
  procedure TPasswordFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    if ModalResult = mrOk then
    begin
      if not MaskKey.Checked then
        CanClose := True
      else
        if CompareStr(Key.Text, ConfirmKey.Text) = 0 then
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
    SetConfirmKey(MaskKey.Checked);
    if Constraints.MaxHeight = 0 then
    begin
      Constraints.MaxHeight := Height;
      Constraints.MinHeight := Height;
      Constraints.MaxWidth := Width;
      Constraints.MinWidth := Width
    end;
  end;

  procedure TPasswordFrm.MaskKeyClick (Sender: TObject);
  begin
    SetConfirmKey(MaskKey.Checked);
  end;

  procedure TPasswordFrm.KeyChange (Sender: TObject);
  begin
    ConfirmKey.Text := '';
  end;

  procedure TPasswordFrm.SetConfirmKey;
  begin
    if Value then
    begin
      Key.PasswordChar := '*';
      ConfirmKey.Color := clWindow;
    end else
    begin
      Key.PasswordChar := #0;
      ConfirmKey.Text  := '';
      ConfirmKey.Color := clBtnFace;
    end;
    ConfirmKey.Enabled := Value;
  end;

  procedure TPasswordFrm.SetKey;
  begin
    Key.Text := Value;
    ConfirmKey.Text := Value;
  end;

  procedure TPasswordFrm.BtnClearClick (Sender: TObject);
  begin
    Key.Clear;
    ConfirmKey.Clear;
  end;
  
initialization

  {$I beegui_passwordfrm.lrs}

end.
