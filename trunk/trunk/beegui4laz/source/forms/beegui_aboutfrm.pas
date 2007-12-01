{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public BtnLicense as published by
    the Free Software Foundation; either version 2 of the BtnLicense, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public BtnLicense for more details.

    You should have received a copy of the GNU General Public BtnLicense
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{   Contains:

    BeeGui AboutTo form.

    Modifyed:

    v1.0.1 build 9143 - 2005.07.09 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;
  
    v1.0.3 build 0360 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_AboutFrm;

interface

uses
  Forms,
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
  XMLPropStorage;

type
  { TAboutFrm }

  TAboutFrm = class(TForm)
    AboutFrm_Logo: TImage;
    AboutFrm_Ver: TLabel;
    AboutFrm_Ver_: TLabel;
    // ---
    AboutFrm_Title: TLabel;
    AboutFrm_MoreInfo: TLabel;
    AboutFrm_Copyright: TLabel;
    AboutFrm_Authors: TLabel;
    AboutFrm_Link: TLabel;
    // ---
    AboutFrm_Storage: TXMLPropStorage;
    AboutFrm_Process: TProcess;
    AboutFrm_Web: TLabel;
    BtnOk: TButton;
    BtnLicense: TButton;
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // ---
    procedure AboutFrm_LinkClick (Sender: TObject);
    procedure AboutFrm_LinkMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // ---
    procedure BtnLicenseClick(Sender: TObject);
  private
  public
  end;

implementation

uses
  BeeGui_SysUtils;

  { TAboutFrm class }

  procedure TAboutFrm.BtnLicenseClick(Sender: TObject);
  begin
    ShellExec(ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('docs') + 'license.htm', '');
  end;

  procedure TAboutFrm.AboutFrm_LinkClick(Sender: TObject);
  begin
    ShellExec(AboutFrm_Link.Caption, '');
  end;

  procedure TAboutFrm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  begin
    AboutFrm_Link.Font.Style := [];
    AboutFrm_Link.Font.Color := clBlack;
  end;

  procedure TAboutFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      AboutFrm_Storage.FileName := CfgFolder + ('aboutfrm.xml');
    end;
    {$I beegui_aboutfrm.inc}
    AboutFrm_Storage.Restore;
    // ---
    AboutFrm_Ver_.Caption   := '1.0.3.427';
  end;

  procedure TAboutFrm.FormDestroy(Sender: TObject);
  begin
    AboutFrm_Storage.Destroy;
  end;

  procedure TAboutFrm.AboutFrm_LinkMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
  begin
    AboutFrm_Link.Font.Style := [fsUnderline];
    AboutFrm_Link.Font.Color := clBlue;
  end;
  
initialization

  {$I beegui_aboutfrm.lrs}
  
end.
