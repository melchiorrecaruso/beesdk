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

unit BeeGui_SelectFrm;

{$I Compiler.inc}

interface

uses
  Forms,
  Buttons,
  Classes,
  StdCtrls,
  Controls,
  SysUtils,
  ComCtrls,
  BeeGui_FormStorage;

type
  TSelectFrm = class(TForm)
    SelectFrm_Messages: TComboBox;
    SelectFrm_MaskLabel: TLabel;
    SelectFrm_Mask: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FFormStorage: TFormStorage;
    procedure SeDeSeItems(List: TListView; Value: boolean);
  public
    { Public declarations }
    procedure IsSelectFrm;
    procedure IsUnselectFrm;
    procedure SelectAll(List: TListView);
    procedure SelectItems(List: TListView);
    procedure DeSelectAll(List: TListView);
    procedure DeselectItems(List: TListView);
    procedure InvertSelection(List: TListView);
  end;

var
  SelectFrm: TSelectFrm;

implementation

{$R *.DFM}

uses
  BeeGui_SelectFrmRes,
  BeeGui_SysUtils;

// TSelectFrm

procedure TSelectFrm.FormCreate(Sender: TObject);
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SSelectFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SSelectFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SSelectFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
end;

procedure TSelectFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SSelectFrm_PropertyFull
  else
    FFormStorage.Properties := _SSelectFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
end;

procedure TSelectFrm.IsSelectFrm;
begin
  Caption := SelectFrm_Messages.Items[_CSelectFrm_SelectFrmCaption];
end;

procedure TSelectFrm.IsUnselectFrm;
begin
  Caption := SelectFrm_Messages.Items[_CSelectFrm_UnselectFrmCaption];
end;

procedure TSelectFrm.SeDeSeItems;
var
  I: integer;
  Ext, iExt: ansistring;
  Name, iName: ansistring;
begin
  Ext  := ExtractFileExt(SelectFrm_Mask.Text);
  Name := ChangeFileExt(SelectFrm_Mask.Text, '');

  for I := 0 to List.Items.Count - 1 do
  begin
    iExt  := ExtractFileExt(List.Items[I].Caption);
    iName := ChangeFileExt(List.Items[I].Caption, '');

    if (Ext = '.*') or (AnsiCompareText(Ext, iExt) = 0) then
      if (Name = '*') or (AnsiCompareText(Name, iName) = 0) then
        List.Items[I].Selected := Value;
  end;
end;

procedure TSelectFrm.SelectAll;
var
  I: integer;
begin
  for I := 0 to List.Items.Count - 1 do
  begin
    List.Items[I].Selected := True;
  end;
end;

procedure TSelectFrm.SelectItems;
begin
  SeDeSeItems(List, True);
end;

procedure TSelectFrm.DeSelectAll;
var
  I: integer;
begin
  for I := 0 to List.Items.Count - 1 do
  begin
    List.Items[I].Selected := False;
  end;
end;

procedure TSelectFrm.DeselectItems;
begin
  SeDeSeItems(List, False);
end;

procedure TSelectFrm.InvertSelection;
var
  I: integer;
begin
  for I := 0 to List.Items.Count - 1 do
  begin
    List.Items[I].Selected := not List.Items[I].Selected;
  end;
end;

procedure TSelectFrm.FormShow(Sender: TObject);
begin
  SelectFrm_Mask.SetFocus;
end;

end.
