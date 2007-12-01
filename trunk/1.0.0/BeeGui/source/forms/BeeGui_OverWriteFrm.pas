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

    BeeGui OverWrite form.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_OverWriteFrm;

{$R-,Q-,S-}

interface

uses
  Forms,
  Buttons,
  Dialogs,
  Classes,
  Messages,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls;

type
  TOverwriteCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type
  TOverwriteFrm = class (TOverwriteCustomFrm)
    OverWriteFrm_OldImage: TImage;
    OverWriteFrm_NewImage: TImage;
    OverWriteFrm_OldDate: TLabel;
    OverWriteFrm_NewDate: TLabel;
    OverWriteFrm_OldSize: TLabel;
    OverWriteFrm_NewSize: TLabel;
    OverWriteFrm_Would: TLabel;
    OverWriteFrm_Image: TImage;
    OverWriteFrm_With: TLabel;
    OverWriteFrm_The: TLabel;
    BtnYesAll: TBitBtn;
    BtnAbort: TBitBtn;
    BtnNoAll: TBitBtn;
    BtnYes: TBitBtn;
    BtnNo: TBitBtn;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OverWriteFrm: TOverwriteFrm;

implementation

{$R *.DFM}

uses
  BeeGui_ConfigFrm;

/// TOverwriteCustomFrm class

  constructor TOverwriteCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TOverwriteCustomFrm.Destroy;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    inherited Destroy;
  end;

  procedure TOverwriteCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TOverwriteFrm class

  procedure TOverwriteFrm.FormShow (Sender: TObject);
  begin
    Width := OverWriteFrm_The.Left + OverWriteFrm_The.Width + 20;
  end;


end.
