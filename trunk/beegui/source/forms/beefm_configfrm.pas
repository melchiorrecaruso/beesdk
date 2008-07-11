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
    BtnOk: TBitBtn;
    cdAOption: TCheckBox;
    Tree: TTreeView;
    AddPage: TGroupBox;
    ExtractPage: TGroupBox;
    dOption: TComboBox;
    dOptionLabel: TLabel;
    kOption: TCheckBox;
    lOption: TCheckBox;
    mOption: TComboBox;
    mOptionLabel: TLabel;
    AddingOptions: TGroupBox;
    ExtractingOptions: TGroupBox;
    oOption: TComboBox;
    oOptionLabel: TLabel;
    rOption: TCheckBox;
    sOption: TCheckBox;
    tOption: TCheckBox;
    cdEOption: TCheckBox;
    xCommand: TCheckBox;
    // ---
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
  private
    procedure SetPageIndex(PageIndex: integer);
  public
    function DeleteOptions: string;
    function AddOptions(const Folder: string): string;
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
    Storage: TMemIniFile;
  begin
    {$I beefm_configfrm_loadlanguage.inc}
    {$I beefm_configfrm_loadproperty.inc}
  end;

  procedure TConfigFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$IFDEF DEBUG}
      {$I beefm_configfrm_savelanguage.inc}
    {$ENDIF}
    {$I beefm_configfrm_saveproperty.inc}
  end;
  
  procedure TConfigFrm.TreeChange(Sender: TObject; Node: TTreeNode);
  begin
    SetPageIndex(Tree.Selected.AbsoluteIndex);
  end;

  procedure TConfigFrm.SetPageIndex(PageIndex: integer);
  var
    I: integer;
  begin
    AddPage    .Visible := False;
    ExtractPage.Visible := False;
    case PageIndex of
      0: AddPage    .Visible := True;
      1: ExtractPage.Visible := True;
    end;
    Tree.Items[PageIndex].Selected := True;
  end;
  
  function TConfigFrm.AddOptions(const Folder: string): string;
  begin
    Result :=
      ' -m' + IntToStr(mOption.ItemIndex) +
      ' -d' + IntToStr(dOption.ItemIndex);
      
    if rOption.Checked then Result := Result + ' -r+' else Result := Result + ' -r-';
    if sOption.Checked then Result := Result + ' -s+' else Result := Result + ' -s-';
    if tOption.Checked then Result := Result + ' -t+' else Result := Result + ' -t-';
    if kOption.Checked then Result := Result + ' -k+' else Result := Result + ' -k-';
    if lOption.Checked then Result := Result + ' -l+' else Result := Result + ' -l-';

    if cdAOption.Checked then
    begin
      Result := Result + ' -cd' + Folder;
    end;
  end;
  
  function TConfigFrm.DeleteOptions: string;
  begin
    Result := ' -l+';
  end;
  
  function TConfigFrm.ExtractOptions(const Folder: string): string;
  begin
    if cdEOption.Checked then
      Result := ' x'
    else
      Result := ' e';
      
    case oOption.ItemIndex of
      0:   Result := Result + ' -oS'; // Skip all
      1:   Result := Result + ' -oA'; // OverWrite all
      else Result := Result + ' -oY'; //
    end;
    
    if cdEOption.Checked then
    begin
      Result := Result + ' -cd' + Folder;
    end;
  end;
    
initialization

  {$I beefm_configfrm.lrs}
  
end.
