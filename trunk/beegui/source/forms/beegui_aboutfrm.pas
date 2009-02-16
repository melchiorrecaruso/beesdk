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

    AboutTo form.

  Modifyed:

}

unit BeeGui_AboutFrm;

{$I compiler.inc}

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
    Logo: TImage;
    Version: TLabel;
    MoreInfo: TLabel;
    Copyright: TLabel;
    Link: TLabel;
    Bevel: TBevel;
    BtnOk: TBitBtn;
    BtnLicense: TBitBtn;
    procedure BtnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BtnLicenseClick(Sender: TObject);
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  private
    { private declarations }
  end;

var
  AboutFrm: TAboutFrm;

implementation

uses
  BeeGui_Consts,
  BeeGui_Messages,
  BeeGui_SysUtils;

  { TAboutFrm class }
  
  {$I beegui_aboutfrm_saveproperty.inc}
  {$I beegui_aboutfrm_loadproperty.inc}
  {$I beegui_aboutfrm_savelanguage.inc}
  {$I beegui_aboutfrm_loadlanguage.inc}

  procedure TAboutFrm.FormCreate(Sender: TObject);
  begin
    LoadLanguage;
    LoadProperty;

    Version.Caption := cApplicationName + ' 1.0.5 [build 0645]';
    Copyright.Caption := '(C) 2003-2009 Andrew Filinsky and  Melchiorre Caruso';
  end;

  procedure TAboutFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    {$IFDEF SAVELANGUAGE}
    SaveLanguage;
    {$ENDIF}
    SaveProperty;
  end;
  
  procedure TAboutFrm.BtnLicenseClick(Sender: TObject);
  var
    FLicenseFile: string;
  begin
    FLicenseFile := ExtractFilePath(ParamStr(0)) +
      IncludeTrailingBackSlash(cApplicationDocsFolder) + cApplicationLicenseFile;

    if GetOSWebBrowser <> '' then
    begin
      ShellExec(FLicenseFile, GetOSWebBrowser);
    end;
  end;
  
  procedure TAboutFrm.BtnOkClick(Sender: TObject);
  begin
    Close;
  end;

initialization

  {$I beegui_aboutfrm.lrs}
  
end.
