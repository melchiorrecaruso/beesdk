{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

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

    v1.0.1 build 9156 - 2005.07.16 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0360 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_ConfigFrm;

interface

uses
  Forms,
  Menus,
  Buttons,
  Classes,
  Dialogs,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  LResources,
  XMLPropStorage,
  // ---
  BeeGui_AddFrm,
  BeeGui_ExtractFrm,
  BeeGui_PasswordFrm;

type
  { TConfigFrm }

  TConfigFrm = class(TForm)
    // ---
    ConfigFrm_Tree: TTreeView;
    ConfigFrm_AddGB: TGroupBox;
    ConfigFrm_ExtractGB: TGroupBox;
    ConfigFrm_GeneralGB: TGroupBox;
    ConfigFrm_Storage: TXMLPropStorage;
    // ---
    AddFrm_DictionaryGB: TGroupBox;
    AddFrm_Dictionary: TComboBox;
    AddFrm_MethodGB: TGroupBox;
    AddFrm_Method: TComboBox;
    AddFrm_OptionsGB: TGroupBox;
    AddFrm_kOption: TCheckBox;
    AddFrm_rOption: TCheckBox;
    AddFrm_sOption: TCheckBox;
    AddFrm_tOption: TCheckBox;
    // ---
    ExtractFrm_OptionsGB: TGroupBox;
    ExtractFrm_xOption: TCheckBox;
    ExtractFrm_OverwriteGB: TGroupBox;
    ExtractFrm_Overwrite: TComboBox;
    // ---
    ConfigFrm_PriorityGB: TGroupBox;
    ConfigFrm_Priority: TComboBox;
    ConfigFrm_OptionsGB: TGroupBox;
    ConfigFrm_mkOption: TCheckBox;
    BtnOk: TBitBtn;
    // ---
    procedure ConfigFrm_TreeChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure SetPageIndex(PageIndex: integer);
    function PasswordFrmCreate: TPasswordFrm;
    function ExtractFrmCreate: TExtractFrm;
    function AddFrmCreate: TAddFrm;
  end;

var
  ConfigFrm: TConfigFrm;

implementation

uses
  BeeGui_SysUtils;

  { TConfigFrm }

  procedure TConfigFrm.FormCreate(Sender: TObject);
  var
    I: integer;
    CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      ConfigFrm_Storage.FileName := CfgFolder + ('configfrm.xml');
    end;
    {$I beegui_configfrm.inc}
    ConfigFrm_Storage.Restore;
    //---
    with ConfigFrm.AddFrm_Method        do if ItemIndex = -1 then ItemIndex := 2;
    with ConfigFrm.AddFrm_Dictionary    do if ItemIndex = -1 then ItemIndex := 2;
    with ConfigFrm.ExtractFrm_Overwrite do if ItemIndex = -1 then ItemIndex := 2;
    with ConfigFrm.ConfigFrm_Priority   do if ItemIndex = -1 then ItemIndex := 2;
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
  
  function TConfigFrm.ExtractFrmCreate: TExtractFrm;
  begin
    Result := TExtractFrm.Create(Self);
    // ---
    Result.ConfigFrm_Priority      .ItemIndex := ConfigFrm.ConfigFrm_Priority  .ItemIndex;
    Result.ExtractFrm_ExtrOverWrite.ItemIndex := ConfigFrm.ExtractFrm_Overwrite.ItemIndex;
    Result.ExtractFrm_xOption      .Checked   := ConfigFrm.ExtractFrm_xOption  .Checked;
  end;
  
  function TConfigFrm.AddFrmCreate: TAddFrm;
  begin
    Result := TAddFrm.Create(Self);
    //
    Result.AddFrm_Action.ItemIndex := 2;
    
    Result.ConfigFrm_Priority.ItemIndex := ConfigFrm.ConfigFrm_Priority.ItemIndex;
    Result.AddFrm_Method     .ItemIndex := ConfigFrm.AddFrm_Method     .ItemIndex;
    Result.AddFrm_Dictionary .ItemIndex := ConfigFrm.AddFrm_Dictionary .ItemIndex;
    // options
    Result.AddFrm_rOption.Checked := ConfigFrm.AddFrm_rOption.Checked;
    Result.AddFrm_sOption.Checked := ConfigFrm.AddFrm_sOption.Checked;
    Result.AddFrm_tOption.Checked := ConfigFrm.AddFrm_tOption.Checked;
    Result.AddFrm_kOption.Checked := ConfigFrm.AddFrm_kOption.Checked;
  end;
  
  function  TConfigFrm.PasswordFrmCreate: TPasswordFrm;
  begin
    Result := TPasswordFrm.Create(Self);
    Result.PasswordFrm_MaskKey.Checked := ConfigFrm.ConfigFrm_mkOption.Checked;
  end;
    
initialization

  {$I beegui_configfrm.lrs}
  
end.
