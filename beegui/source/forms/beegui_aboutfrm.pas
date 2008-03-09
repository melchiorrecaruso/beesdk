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

    BeeGui AboutTo form.

  Modifyed:

}

unit BeeGui_AboutFrm;

{$I compiler.inc}

interface

uses
  Forms,
  GetText,
  Process,
  Dialogs,
  Buttons,
  Classes,
  Controls,
  StdCtrls,
  Graphics,
  ExtCtrls,
  SysUtils,
  LResources,
  Translations,
  XMLPropStorage;

type
  { TAboutFrm }

  TAboutFrm = class(TForm)
    BtnLicense: TBitBtn;
    BtnOk: TBitBtn;
    Storage: TXMLPropStorage;
    Process: TProcess;
    // ---
    Logo: TImage;
    Version: TLabel;
    Build: TLabel;
    VersionValue: TLabel;
    // ---
    Title: TLabel;
    MoreInfo: TLabel;
    Copyright: TLabel;
    Authors: TLabel;
    BuildValue: TLabel;
    // ---
    Web: TLabel;
    Link: TLabel;
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // ---
    procedure LinkClick (Sender: TObject);
    procedure LinkMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // ---
    procedure BtnOkClick(Sender: TObject);
    procedure BtnLicenseClick(Sender: TObject);
  public
    procedure LoadProperties;
    procedure SaveProperties;
  private
    procedure LoadLanguage;
    procedure SaveLanguage;
  end;

var
  AboutFrm: TAboutFrm;

implementation

uses
  BeeGui_SysUtils;

  { TAboutFrm class }

  {$I beegui_aboutfrm.inc}

  procedure TAboutFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir('BeeGui'));
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder + ('aboutfrm.xml');
    end;
    SessionProperties := 'WindowState;';
    if WindowState = wsNormal then
    begin
      SessionProperties :=
        SessionProperties + 'Top;' + 'Left;' + 'Width;' + 'Height;';
    end;
    Storage.Restore;
    // ---
    VersionValue.Caption   := '1.0.5';
    BuildValue.Caption := '126';
  end;
  
  procedure TAboutFrm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  begin
    Link.Font.Style := [];
    Link.Font.Color := clBlack;
  end;
  
  procedure TAboutFrm.LinkMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
  begin
    Link.Font.Style := [fsUnderline];
    Link.Font.Color := clBlue;
  end;
  
  procedure TAboutFrm.LinkClick(Sender: TObject);
  begin
    ShellExec(Link.Caption, '');
  end;
  
  procedure TAboutFrm.BtnLicenseClick(Sender: TObject);
  begin
    ShellExec(ExtractFilePath(ParamStr(0))
      + IncludeTrailingBackSlash('docs') + 'license.htm', '');
  end;

  procedure TAboutFrm.BtnOkClick(Sender: TObject);
  begin
    Close;
  end;

initialization

  {$I beegui_aboutfrm.lrs}
  
end.
