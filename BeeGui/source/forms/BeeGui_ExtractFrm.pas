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

    BeeGui Exctract form.

    Modifyed:

    v1.0.1 build 9155 - 2005.07.15 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_ExtractFrm;

{$R-,Q-,S-}

interface

uses
  Forms,
  Buttons,
  Classes,
  ComCtrls,
  StdCtrls,
  Controls,
  SysUtils,

  BeeGui_Components;

type
  TExtractCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type
  TExtractFrm = class (TExtractCustomFrm)
    ExtractFrm_ExtrOverWrite_: TGroupBox;
    ExtractFrm_ExtrOverWrite: TComboBox;
    ExtractFrm_ExtrOptions: TGroupBox;
    ExtractFrm_ExtrDirName: TCheckBox;
    ConfigFrm_PriorityGB: TGroupBox;
    ConfigFrm_Priority: TComboBox;
    ExtractFrm_cOption: TCheckBox;
    ExtractFrm_ExtrDir_: TLabel;
    ExtractFrm_ExtrDir: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    ExtractFrm_Tree: TBeeGui_TreeView;
    procedure FormCreate (Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExtractFrm: TExtractFrm;

implementation

uses
  BeeGui_ConfigFrm;

{$R *.DFM}

/// TBeeGuiExtractForm

  constructor TExtractCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TExtractCustomFrm.Destroy;
  var
    I: integer;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
    begin
      if AnsiCompareText(TControl (Self.Components [I]).Name, 'ExtractFrm_ExtrDir') = 0 then
        BeeGui_Configuration.Save_TControl (TControl (Self.Components [I]));
    end;
    inherited Destroy;
  end;

  procedure TExtractCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TExtractFrm

  procedure TExtractFrm.FormCreate;
  begin
    ExtractFrm_Tree.Initialize;
    ExtractFrm_Tree.CurrDir := ExtractFrm_ExtrDir.Text;
  end;

  
end.
