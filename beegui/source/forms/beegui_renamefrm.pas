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

unit BeeGui_RenameFrm;

{$I Compiler.inc}

interface

uses
  Forms,
  Classes,
  Dialogs,
  Buttons,
  Messages,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  // ---
  BeeGui_FormStorage;

type
  TRenameFrm = class(TForm)
    RenameFrm_FromLabel: TLabel;
    RenameFrm_From: TLabel;
    RenameFrm_ToLabel: TLabel;
    RenameFrm_To: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FFormStorage: TFormStorage;
  public
    { Public declarations }
  end;

var
  RenameFrm: TRenameFrm;

implementation

{$R *.dfm}

uses
  BeeGui_RenameFrmRes,
  BeeGui_SysUtils;

/// TRenameFrm class

procedure TRenameFrm.FormCreate(Sender: TObject);
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SRenameFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SRenameFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SRenameFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
end;

procedure TRenameFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SRenameFrm_PropertyFull
  else
    FFormStorage.Properties := _SRenameFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
end;

procedure TRenameFrm.FormShow(Sender: TObject);
begin
  RenameFrm_From.Caption := RenameFrm_To.Text;
  RenameFrm_To.SetFocus;
end;

end.
