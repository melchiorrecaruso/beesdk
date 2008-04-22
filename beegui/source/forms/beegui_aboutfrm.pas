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
  Process,
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
    Version: TLabel;
    Process: TProcess;
    // ---
    Logo: TImage;
    MoreInfo: TLabel;
    Copyright: TLabel;
    Link: TLabel;
    // ---
    BtnLicense: TBitBtn;
    BtnOk: TBitBtn;
    // ---
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // ---
    procedure LinkClick (Sender: TObject);
    procedure LinkMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // ---
    procedure BtnOkClick(Sender: TObject);
    procedure BtnLicenseClick(Sender: TObject);
  public
    { public declarations }
  private
    { private declarations }
  end;

implementation

uses
  BeeGui_SysUtils;

  { TAboutFrm class }

  procedure TAboutFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_aboutfrm_loadlanguage.inc}
    {$I beegui_aboutfrm_loadproperty.inc}
    // ---
    Logo.Transparent := True;
    Version.Caption   := 'BeeGui 1.0.5 [build 211]';
  end;

  procedure TAboutFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_aboutfrm_savelanguage.inc}
    {$I beegui_aboutfrm_saveproperty.inc}
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
