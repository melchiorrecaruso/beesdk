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

    BeeGui AboutTo form.

    Modifyed:

    v1.0.1 build 9143 - 2005.07.09 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_AboutFrm;

{$R-,Q-,S-}

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

  BeeGui_Components;

type
  TAboutCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type
  TAboutFrm = class (TAboutCustomFrm)
    AboutFrm_Copyright: TBeeGui_Label;
    AboutFrm_Authors: TBeeGui_Label;
    AboutFrm_Build_: TBeeGui_Label;
    AboutFrm_Title: TBeeGui_Label;
    AboutFrm_Ver_: TBeeGui_Label;
    AboutFrm_Link: TBeeGui_Label;
    AboutFrm_Build: TLabel;
    AboutFrm_Logo: TImage;
    AboutFrm_Ver: TLabel;
    AboutFrm_Web: TLabel;
    BtnLicense: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BtnLicenseClick (Sender: TObject);
    procedure AboutFrm_LinkClick (Sender: TObject);
    procedure FormMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure AboutFrm_LinkMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutFrm: TAboutFrm;

implementation

{$R *.DFM}

uses
  BeeGui_WinOS,
  BeeGui_Common,
  BeeGui_ConfigFrm;

/// TAboutCustomFrm class

  constructor TAboutCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TAboutCustomFrm.Destroy;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    inherited Destroy;
  end;

  procedure TAboutCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TAboutFrm class

  procedure TAboutFrm.BtnLicenseClick (Sender: TObject);
  begin
    BeeGui_ShellExecute (LIC_FILENAME, ExtractFilePath (ParamStr(0)));
  end;

  procedure TAboutFrm.AboutFrm_LinkClick(Sender: TObject);
  begin
    BeeGui_ShellExecute (AboutFrm_Link.Caption, '');
  end;

  procedure TAboutFrm.AboutFrm_LinkMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
  begin
    AboutFrm_Link.Font.Style := [fsUnderline];
    AboutFrm_Link.Font.Color := clBlue;
  end;

  procedure TAboutFrm.FormMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
  begin
    AboutFrm_Link.Font.Style := [];
    AboutFrm_Link.Font.Color := clBlue;
  end;

  procedure TAboutFrm.FormCreate(Sender: TObject);
  var
    Version: ansistring;
    Major, Minor, Release, Build: integer;
  begin
    Version := BeeGui_FileVersion (ParamStr (0));

    BeeGui_ExpandVersion (Version, Major, Minor, Release, Build);

    AboutFrm_Ver_.Caption   := IntToStr (Major) + '.' + IntToStr (Minor) + '.' + IntToStr (Release);
    AboutFrm_Build_.Caption := IntToStr (Build);
  end;

end.
