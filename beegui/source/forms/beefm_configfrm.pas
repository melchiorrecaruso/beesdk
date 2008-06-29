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

{   Contains:

      BeeGui Config form.

    Modifyed:
}

unit BeeFM_ConfigFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Menus,
  Buttons,
  Classes,
  Dialogs,
  IniFiles,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  LResources;
  
type
  { TConfigFrm }

  TConfigFrm = class(TForm)
    AddFrm_Dictionary1: TComboBox;
    AddFrm_DictionaryGB1: TGroupBox;
    AddFrm_Method1: TComboBox;
    AddFrm_MethodGB1: TGroupBox;
    BtnOk: TBitBtn;
    // ---
    ConfigFrm_Tree: TTreeView;
    ConfigFrm_AddGB: TGroupBox;
    ConfigFrm_ExtractGB: TGroupBox;
    ConfigFrm_GeneralGB: TGroupBox;
    dOption: TComboBox;
    dOptionLabel: TLabel;
    ConfigFrm_OptionsGB: TGroupBox;
    kOption: TCheckBox;
    lOption: TCheckBox;
    mOption: TComboBox;
    mOptionLabel: TLabel;
    oOption: TComboBox;
    oOptionLabel: TLabel;
    Options: TGroupBox;
    Options1: TGroupBox;
    rOption: TCheckBox;
    sOption: TCheckBox;
    tOption: TCheckBox;
    cdOption: TCheckBox;
    xCommand1: TCheckBox;
    // ---
    procedure FormCreate(Sender: TObject);
    procedure ConfigFrm_TreeChange(Sender: TObject; Node: TTreeNode);
  private
    procedure SetPageIndex(PageIndex: integer);
  public
    function AddOptions: string;
    function DeleteOptions: string;
    function ExtractOptions(const Folder: string): string;
  end;

var
  ConfigFrm: TConfigFrm;

implementation

uses
  BeeGui_SysUtils;

  { TConfigFrm }

  procedure TConfigFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
  begin

  end;
  
  procedure TConfigFrm.ConfigFrm_TreeChange(Sender: TObject; Node: TTreeNode);
  begin
    SetPageIndex(ConfigFrm_Tree.Selected.AbsoluteIndex);
  end;

  procedure TConfigFrm.SetPageIndex(PageIndex: integer);
  begin
    ConfigFrm_AddGB    .Left := 152;
    ConfigFrm_ExtractGB.Left := 152;
    ConfigFrm_GeneralGB.Left := 152;
    // ---
    ConfigFrm_AddGB    .Top  := 10;
    ConfigFrm_ExtractGB.Top  := 10;
    ConfigFrm_GeneralGB.Top  := 10;
    // ---
    Height := 400;
    Width  := 501;
    // ---
    ConfigFrm_AddGB    .Visible := False;
    ConfigFrm_ExtractGB.Visible := False;
    ConfigFrm_GeneralGB.Visible := False;
    case PageIndex of
      0: ConfigFrm_AddGB    .Visible := True;
      1: ConfigFrm_ExtractGB.Visible := True;
      2: ConfigFrm_GeneralGB.Visible := True;
    end;
  end;
  
  function TConfigFrm.AddOptions: string;
  begin
    Result :=
      ' -m' + IntToStr(mOption.ItemIndex) +
      ' -d' + IntToStr(dOption.ItemIndex);
      
    if rOption.Checked then Result := Result + ' -r+' else Result := Result + ' -r-';
    if sOption.Checked then Result := Result + ' -s+' else Result := Result + ' -s-';
    if tOption.Checked then Result := Result + ' -t+' else Result := Result + ' -t-';
    if kOption.Checked then Result := Result + ' -k+' else Result := Result + ' -k-';
    if lOption.Checked then Result := Result + ' -l+' else Result := Result + ' -l-';
  end;
  
  function TConfigFrm.DeleteOptions: string;
  begin
    Result := ' -l+';
  end;
  
  function TConfigFrm.ExtractOptions(const Folder: string): string;
  begin
    if cdOption.Checked then
      Result := ' x'
    else
      Result := ' e';
      
    case oOption.ItemIndex of
      0:   Result := Result + ' -oS'; // Skip all
      1:   Result := Result + ' -oA'; // OverWrite all
      else Result := Result + ' -oY'; //
    end;
    
    if cdOption.Checked then
    begin
      Result := Result + ' -cd' + Folder;
    end;
  end;
    
initialization

  {$I beefm_configfrm.lrs}
  
end.
