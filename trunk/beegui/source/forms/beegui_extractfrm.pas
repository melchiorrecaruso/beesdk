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

    BeeCore Exctract form.

    Modifyed:

}

unit BeeGui_ExtractFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Dialogs,
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
  BeeGui_FolderTreeViewMgr;

type

  { TExtractFrm }

  TExtractFrm = class(TForm)
    BtnOpen2: TBitBtn;
    cfgOptionStr: TComboBox;
    FoldersMgr: TFolderTreeViewMgr;
    Icons: TIconList;
    Storage: TXMLPropStorage;
    FolderLabel: TLabel;
    Folder: TEdit;
    priOptionLabel: TLabel;
    priOption: TComboBox;
    oOptionLabel: TLabel;
    oOption: TComboBox;
    Options: TGroupBox;
    xCommand: TCheckBox;
    cdOption: TCheckBox;
    Folders: TTreeView;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FoldersClick(Sender: TObject);
    procedure FoldersDblClick(Sender: TObject);
    procedure FoldersExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
  public
    { public declarations }
  private
    { private declarations }
  end;
  
implementation

uses
  BeeGui_SysUtils,
  Bee_Common;

  { TExtractFrm class}

  procedure TExtractFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := IncludeTrailingBackSlash(GetApplicationConfigDir('BeeGui'));
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder+ ('extractfrm.xml');
    end;
    {$I beegui_extractfrm.inc}
    Storage.Restore;
    // ---
    Icons.IconFolder := ExtractFilePath(ParamStr(0))
      + IncludeTrailingBackSlash('icons') + 'smallicons';

    FoldersMgr.Initialize;
    FoldersMgr.FolderName := Folder.Text;
  end;

  procedure TExtractFrm.FoldersClick(Sender: TObject);
  begin
    FoldersMgr.Click;
  end;

  procedure TExtractFrm.FoldersDblClick(Sender: TObject);
  begin
    FoldersMgr.DblClick;
  end;

  procedure TExtractFrm.FoldersExpanding(Sender: TObject; Node: TTreeNode;
    var AllowExpansion: Boolean);
  begin
    AllowExpansion := True;
    FoldersMgr.Expand(Node);
  end;

initialization

  {$I beegui_extractfrm.lrs}

end.
