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

    BeeGui Select-UnSelect form.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0312 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_SelectFrm;

{$R-,Q-,S-}

interface

uses
  Forms,
  Buttons,
  Classes,
  StdCtrls,
  Controls,
  Sysutils,
  ComCtrls;

type
  TSelectCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type
  TSelectFrm = class (TSelectCustomFrm)
    SelectFrm_FrmCaptionCB: TComboBox;
    SelectFrm_Mask_: TLabel;
    SelectFrm_Mask: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormShow(Sender: TObject);
  private    
    { Private declarations }
    procedure SeDeSeItems (List: TListView; Value: boolean);
  public
    { Public declarations }
    procedure SelectAll (List: TListView);
    procedure SelectItems (List: TListView);
    procedure DeSelectAll (List: TListView);
    procedure DeselectItems (List: TListView);
    procedure InvertSelection (List: TListView);
  end;

var
  SelectFrm: TSelectFrm;

implementation

uses
  BeeGui_ConfigFrm;

{$R *.DFM}

/// TSelectCustomFrm

  constructor TSelectCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TSelectCustomFrm.Destroy;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    inherited Destroy;
  end;

  procedure TSelectCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TSelectFrm

  procedure TSelectFrm.SeDeSeItems;
  var
    I: integer;
    Ext, iExt: ansistring;
    Name, iName: ansistring;
  begin
    Ext := ExtractFileExt (SelectFrm_Mask.Text);
    Name := ChangeFileExt (SelectFrm_Mask.Text, '');

    for I := 0 to List.Items.Count -1 do
    begin
      iExt := ExtractFileExt (List.Items [I].Caption);
      iName := ChangeFileExt (List.Items [I].Caption, '');

      if (Ext = '.*') or (AnsiCompareText (Ext, iExt) = 0) then
        if (Name = '*') or (AnsiCompareText (Name, iName) = 0) then
          List.Items [I].Selected := Value;
    end;
  end;

  procedure TSelectFrm.SelectAll;
  var
    I: integer;
  begin
    for I := 0 to List.Items.Count -1 do
      List.Items [I].Selected := True;
  end;

  procedure TSelectFrm.SelectItems;
  begin
    SeDeSeItems (List, True);
  end;

  procedure TSelectFrm.DeSelectAll;
  var
    I: integer;
  begin
    for I := 0 to List.Items.Count -1 do
      List.Items [I].Selected := False;
  end;

  procedure TSelectFrm.DeselectItems;
  begin
    SeDeSeItems (List, False);
  end;

  procedure TSelectFrm.InvertSelection;
  var
    I: integer;
  begin
    for I := 0 to List.Items.Count -1 do
      List.Items [I].Selected := not List.Items [I].Selected;
  end;

  procedure TSelectFrm.FormShow  (Sender: TObject);
  begin
    SelectFrm_Mask.SetFocus;
  end;

end.
