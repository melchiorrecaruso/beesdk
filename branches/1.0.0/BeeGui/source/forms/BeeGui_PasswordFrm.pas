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

    BeeGui PassWord form.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_PasswordFrm;

{$R-,Q-,S-}

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
  Controls;

type
  TPasswordCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type 
  TPasswordFrm = class (TPasswordCustomFrm)
    PasswordFrm_Confirm_Key_: TLabel;
    PasswordFrm_MaskKey: TCheckBox;
    PasswordFrm_Confirm_Key: TEdit;
    PasswordFrm_MSGs: TComboBox;
    PasswordFrm_Panel: TPanel;
    PasswordFrm_Image: TImage;
    PasswordFrm_Key_: TLabel;
    PasswordFrm_Key: TEdit;
    BtnCancel: TBitBtn;
    BtnClear: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormShow (Sender: TObject);
    procedure FormCloseQuery (Sender: TObject; var CanClose: Boolean);
    procedure PasswordFrm_MaskKeyClick (Sender: TObject);
    procedure PasswordFrm_KeyChange (Sender: TObject);
    procedure BtnClearClick (Sender: TObject);
  private
    { Private declarations }
    procedure ConfirmKey (Value: boolean);
  public
    { Public declarations }    
    procedure SetKey (const Value: string);
  end;

var
  PasswordFrm: TPasswordFrm;

implementation

{$R *.DFM}

uses
  BeeGui_ConfigFrm;

/// TPasswordCustomFrm class

  constructor TPasswordCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TPasswordCustomFrm.Destroy;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    inherited Destroy;
  end;

  procedure TPasswordCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TPasswordFrm class

  procedure TPasswordFrm.FormShow(Sender: TObject);
  begin
    ConfirmKey (PasswordFrm_MaskKey.Checked);
  end;

  procedure TPasswordFrm.FormCloseQuery (Sender: TObject; var CanClose: Boolean);
  begin
    if ModalResult = mrOk then
    begin
      if not PasswordFrm_MaskKey.Checked then
        CanClose := True
      else
        if CompareStr (PasswordFrm_Key.Text, PasswordFrm_Confirm_Key.Text) = 0 then
          CanClose := True
        else
        begin
          MessageDlg (PasswordFrm_MSGs.Items [0], mtWarning, [mbOk], 0);
          CanClose := False;
        end;
    end;
  end;

  procedure TPasswordFrm.PasswordFrm_MaskKeyClick (Sender: TObject);
  begin
    ConfirmKey (PasswordFrm_MaskKey.Checked);
  end;

  procedure TPasswordFrm.PasswordFrm_KeyChange (Sender: TObject);
  begin
    PasswordFrm_Confirm_Key.Text := '';
  end;

  procedure TPasswordFrm.ConfirmKey;
  begin
    if Value then
    begin
      PasswordFrm_Key.PasswordChar := '*';
      PasswordFrm_Confirm_Key.Color := clWindow;
    end else
    begin
      PasswordFrm_Key.PasswordChar := #0;
      PasswordFrm_Confirm_Key.Text := '';
      PasswordFrm_Confirm_Key.Color := clBtnFace;
    end;
    PasswordFrm_Confirm_Key.Enabled := Value;
  end;

  procedure TPasswordFrm.SetKey;
  begin
    PasswordFrm_Key.Text := Value;
    PasswordFrm_Confirm_Key.Text := Value;
  end;

  procedure TPasswordFrm.BtnClearClick (Sender: TObject);
  begin
    PasswordFrm_Key.Clear;
    PasswordFrm_Confirm_Key.Clear;
  end;

  
end.
