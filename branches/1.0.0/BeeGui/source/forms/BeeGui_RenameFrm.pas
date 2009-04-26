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

    BeeGui Rename form.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_RenameFrm;

{$R-,Q-,S-}

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

  BeeGui_Config;

type
  TRenameCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type
  TRenameFrm = class (TRenameCustomFrm)
    RenameFrm_From_: TLabel;
    RenameFrm_From: TLabel;
    RenameFrm_To_: TLabel;
    RenameFrm_To: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormShow (Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RenameFrm: TRenameFrm;

implementation

uses
  BeeGui_ConfigFrm;

{$R *.DFM}

/// TRenameCustomFrm class

  constructor TRenameCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TRenameCustomFrm.Destroy;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    inherited Destroy;
  end;

  procedure TRenameCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TRenameFrm class

  procedure TRenameFrm.FormShow (Sender: TObject);
  begin
    RenameFrm_From.Caption := RenameFrm_To.Text;
    RenameFrm_To.SetFocus;
  end;


end.
