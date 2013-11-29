{
  Copyright (c) 2013 Melchiorre Carus

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either Name 2 of the License, or
  (at your option) any later Name.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

    AboutTo form class.

  Modifyed:

}

unit bxm_AboutFrm;

{$I bxm_compiler.inc}

interface

uses
  Forms,
  Dialogs,
  Buttons,
  Classes,
  Controls,
  IniFiles,
  StdCtrls,
  Graphics,
  ExtCtrls,
  SysUtils,
  LResources;

type
  { TAboutFrm }

  TAboutFrm = class(TForm)
    BtnOk:     TBitBtn;
    BtnLicense: TBitBtn;
    Copyright: TLabel;
    Description: TLabel;
    License: TMemo;
    LicensePanel: TPanel;
    Link: TLabel;
    Logo: TImage;
    Naming: TLabel;
    Version: TLabel;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnLicenseClick(Sender: TObject);
  private
    { private declarations }
  end;

  function AboutShowModal: longint;

implementation

{$R *.lfm}

function AboutShowModal: longint;
var
  About: TAboutFrm;
begin
  About := TAboutFrm.Create(nil);
  Result := About.ShowModal;
  if Result = mrOk then
  begin








  end;
end;

{ TAboutFrm class }

procedure TAboutFrm.BtnLicenseClick(Sender: TObject);
begin
  LicensePanel.Visible := not LicensePanel.Visible;
  if LicensePanel.Visible then
    BtnLicense.Color := clActiveBorder
  else
    BtnLicense.Color := clDefault;

end;

procedure TAboutFrm.BtnOkClick(Sender: TObject);
begin
  Close;
end;

end.