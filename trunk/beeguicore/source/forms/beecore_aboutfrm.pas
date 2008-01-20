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

    BeeCore AboutTo form.

  Modifyed:

}

unit BeeCore_AboutFrm;

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
    Storage: TXMLPropStorage;
    Process: TProcess;
    // ---
    Logo: TImage;
    Ver: TLabel;
    VerValue: TLabel;
    // ---
    Title: TLabel;
    MoreInfo: TLabel;
    Copyright: TLabel;
    Authors: TLabel;
    // ---
    Web: TLabel;
    Link: TLabel;
    // ---
    BtnOk: TButton;
    BtnLicense: TButton;
    // ---
    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // ---
    procedure LinkClick (Sender: TObject);
    procedure LinkMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // ---
    procedure BtnLicenseClick(Sender: TObject);
  public
    { public declarations }
  private
    { private declarations }
  end;
  
var
  AboutFrm: TAboutFrm;

implementation

uses
  BeeCore_SysUtils;

  { TAboutFrm class }

  procedure TAboutFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder + ('aboutfrm.xml');
    end;
    {$i beecore_aboutfrm.inc}
    Storage.Restore;
    // ---
    VerValue.Caption   := '0.1.0.40';
  end;

  procedure TAboutFrm.FormDestroy(Sender: TObject);
  begin
    Storage.Destroy;
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

  {$I beecore_aboutfrm.lrs}
  
end.
