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

      BeeFm Configuration form.

    Modifyed:
    
      v1.0.5 build 0462 - 2008.07.12 by Melchiorre Caruso.
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
  LResources,

  BeeGui_CommandLine;
  
type
  { TConfigFrm }

  TConfigFrm = class(TForm)
    Tree: TTreeView;
    AddPage: TGroupBox;
    ExtractPage: TGroupBox;
    AddingOptions: TGroupBox;
    ExtractingOptions: TGroupBox;
    mOptionLabel: TLabel;
    dOptionLabel: TLabel;
    oOptionLabel: TLabel;
    dOption: TComboBox;
    mOption: TComboBox;
    oOption: TComboBox;
    rOption: TCheckBox;
    sOption: TCheckBox;
    tOption: TCheckBox;
    kOption: TCheckBox;
    lOption: TCheckBox;
    cdAOption: TCheckBox;
    xCommand: TCheckBox;
    cdEOption: TCheckBox;
    BtnOk: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
    procedure SetPageIndex(PageIndex: integer);
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  public
    { public declarations }
    procedure AddOptions(const Folder: string);
    procedure ExtractOptions(const Folder: string);
  end;

var
  ConfigFrm: TConfigFrm;

implementation

uses
  BeeGui_Consts,
  BeeGui_Messages,
  BeeGui_SysUtils;

  { TConfigFrm }

  {$I beefm_configfrm_saveproperty.inc}
  {$I beefm_configfrm_loadproperty.inc}
  {$I beefm_configfrm_savelanguage.inc}
  {$I beefm_configfrm_loadlanguage.inc}

  procedure TConfigFrm.FormCreate(Sender: TObject);
  begin
    LoadLanguage;
    LoadProperty;
  end;

  procedure TConfigFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    {$IFDEF DEBUG}
      SaveLanguage;
    {$ENDIF}
  end;
  
  procedure TConfigFrm.TreeChange(Sender: TObject; Node: TTreeNode);
  begin
    SetPageIndex(Tree.Selected.AbsoluteIndex);
  end;

  procedure TConfigFrm.SetPageIndex(PageIndex: integer);
  var
    I: integer;
  begin
    AddPage.Visible := False;
    ExtractPage.Visible := False;
    case PageIndex of
      0: AddPage    .Visible := True;
      1: ExtractPage.Visible := True;
    end;
    Tree.Items[PageIndex].Selected := True;
  end;
  
  procedure TConfigFrm.AddOptions(const Folder: string);
  begin
    CommandLine.mOption := mOption.ItemIndex;
    CommandLine.dOption := dOption.ItemIndex;

    CommandLine.rOption := rOption.Checked;
    CommandLine.sOption := sOption.Checked;
    CommandLine.tOption := tOption.Checked;
    CommandLine.kOption := kOption.Checked;
    CommandLine.lOption := lOption.Checked;

    if cdAOption.Checked then
    begin
      CommandLine.cdOption := Folder;
    end;
  end;
  
  procedure TConfigFrm.ExtractOptions(const Folder: string);
  begin
    if xCommand.Checked then
      CommandLine.Command := 'X'
    else
      CommandLine.Command := 'E';

    case oOption.ItemIndex of
      0:   CommandLine.oOption := 'S';
      1:   CommandLine.oOption := 'A';
      else CommandLine.oOption := 'Y';
    end;
    
    if cdEOption.Checked then
    begin
      CommandLine.cdOption := Folder;
    end;
  end;
    
initialization

  {$I beefm_configfrm.lrs}
  
end.
