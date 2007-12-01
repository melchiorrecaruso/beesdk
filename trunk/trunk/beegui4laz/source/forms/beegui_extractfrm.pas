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

    BeeGui Exctract form.

    Modifyed:

    v1.0.1 build 9155 - 2005.07.15 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;
  
    v1.0.3 build 0360 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_ExtractFrm;

interface

uses
  Forms,
  Buttons,
  Classes,
  ComCtrls,
  StdCtrls,
  Controls,
  SysUtils,
  LResources,
  XMLPropStorage,
  // ---
  BeeGui_IconList,
  BeeGui_FolderTreeView;

type
  { TExtractFrm }

  TExtractFrm = class(TForm)
    ExtractFrm_OverWriteGB: TGroupBox;
    ExtractFrm_ExtrOverWrite: TComboBox;
    ExtractFrm_Storage: TXMLPropStorage;
    ExtractFrm_OptionsBG: TGroupBox;
    ExtractFrm_xOption: TCheckBox;
    ExtractFrm_Tree: TFolderTreeView;
    ConfigFrm_PriorityGB: TGroupBox;
    ExtractFrm_IconList: TIconList;    
    ConfigFrm_Priority: TComboBox;
    ExtractFrm_FolderEdit: TEdit;
    ExtractFrm_ExtrDir_: TLabel;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

uses
  BeeGui_SysUtils;

  { TExtractFrm class}

  procedure TExtractFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      ExtractFrm_Storage.FileName := CfgFolder+ ('extractfrm.xml');
    end;
    {$I beegui_extractfrm.inc}
    ExtractFrm_Storage.Restore;
    // ---
    ExtractFrm_IconList.Initialize(ExtractFilePath(ParamStr(0)) + 'smallicons');
    ExtractFrm_Tree.Initialize;
    // ---
    ExtractFrm_Tree.CurrDir := ExtractFrm_FolderEdit.Text;
  end;

  
initialization

  {$I beegui_extractfrm.lrs}

end.
