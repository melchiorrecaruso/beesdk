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

unit BeeGui_AboutFrm;

{$I Compiler.inc}

interface

uses
  Forms,
  Buttons,
  Classes,
  Controls,
  StdCtrls,
  Graphics,
  ExtCtrls,
  SysUtils,
  BeeGui_FormStorage;

type
  TAboutFrm = class(TForm)
    AboutFrm_Copyright: TLabel;
    AboutFrm_Authors: TLabel;
    AboutFrm_Build_: TLabel;
    AboutFrm_Build: TLabel;
    AboutFrm_Title: TLabel;
    AboutFrm_Ver_: TLabel;
    AboutFrm_Ver: TLabel;
    AboutFrm_Web: TLabel;
    AboutFrm_Link: TLabel;
    AboutFrm_Logo: TImage;
    BtnLicense: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure AboutFrm_LinkClick(Sender: TObject);
    procedure AboutFrm_LinkMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure BtnLicenseClick(Sender: TObject);
  private
    { Private declarations }
    FFormStorage: TFormStorage;
  public
    { Public declarations }
  end;

var
  AboutFrm: TAboutFrm;

implementation

{$R *.dfm}

uses
  BeeGui_AboutFrmRes,
  BeeGui_SysUtils;

// TAboutFrm class

procedure TAboutFrm.FormCreate(Sender: TObject);
var
  Version: ansistring;
  Major, Minor, Release, Build: integer;
begin
  // Load Executable Version Info
  Version := ExecutableVersion(ParamStr(0));
  ExpandExecutableVersion(Version, Major, Minor, Release, Build);
  AboutFrm_Build_.Caption := IntToStr(Build);
  AboutFrm_Ver_.Caption :=
    IntToStr(Major) + '.' + IntToStr(Minor) +
    '.' + IntToStr(Release);
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SAboutFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SAboutFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SAboutFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
end;

procedure TAboutFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SAboutFrm_PropertyFull
  else
    FFormStorage.Properties := _SAboutFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
end;

procedure TAboutFrm.BtnLicenseClick(Sender: TObject);
begin
  ShellExec(_SAboutFrm_LicFilename, ExtractFilePath(ParamStr(0)));
end;

procedure TAboutFrm.AboutFrm_LinkClick(Sender: TObject);
begin
  ShellExec(AboutFrm_Link.Caption, '');
end;

procedure TAboutFrm.AboutFrm_LinkMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  AboutFrm_Link.Font.Style := [fsUnderline];
  AboutFrm_Link.Font.Color := clBlue;
end;

procedure TAboutFrm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  AboutFrm_Link.Font.Style := [];
  AboutFrm_Link.Font.Color := clBlue;
end;

end.
