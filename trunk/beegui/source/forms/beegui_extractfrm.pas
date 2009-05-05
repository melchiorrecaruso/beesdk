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

{ Contains:

    BeeGui Exctract form.

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
  IniFiles,
  ComCtrls,
  StdCtrls,
  Controls,
  Graphics,
  SysUtils,
  LResources,
  // ---
  BeeGui_IconList,
  BeeGui_FolderTreeViewMgr;

type

  { TExtractFrm }

  TExtractFrm = class(TForm)
    FolderLabel: TLabel;
    Folders:   TTreeView;
    Folder:    TEdit;
    FoldersMgr: TFolderTreeViewMgr;
    Icons:     TIconList;
    Options:   TGroupBox;
    cdOption:  TEdit;
    xCommand:  TCheckBox;
    oOptionLabel: TLabel;
    oOption:   TComboBox;
    cdOptionCheck: TCheckBox;
    BtnCancel: TBitBtn;
    BtnOk:     TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FoldersClick(Sender: TObject);
    procedure FoldersDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FoldersExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: boolean);
    procedure cdOptionCheckChange(Sender: TObject);
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  private
    { private declarations }
  end;

var
  ExtractFrm: TExtractFrm;

implementation

uses
  Bee_Common,
  BeeGui_Consts,
  BeeGui_Messages,
  BeeGui_SysUtils;

{ TExtractFrm class}

  {$I beegui_extractfrm_saveproperty.inc}
  {$I beegui_extractfrm_loadproperty.inc}
  {$I beegui_extractfrm_savelanguage.inc}
  {$I beegui_extractfrm_loadlanguage.inc}

procedure TExtractFrm.FormCreate(Sender: TObject);
begin
  Icons.IconFolder := GetApplicationSmallIconsDir;

  LoadLanguage;
  LoadProperty;

  FoldersMgr.Initialize;
  if Folder.Text = '' then
  begin
    Folder.Text := GetUserDir;
  end;
  FoldersMgr.FolderName := Folder.Text;
end;

procedure TExtractFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    {$IFDEF SAVELANGUAGE}
  SaveLanguage;
    {$ENDIF}
  SaveProperty;
end;

procedure TExtractFrm.cdOptionCheckChange(Sender: TObject);
begin
     cdOption.Enabled := False;
  // cdOption.Enabled := cdOptionCheck.Checked;

  if cdOption.Enabled then
    cdOption.Color := clWindow
  else
    cdOption.Color := clInactiveCaptionText;
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
  var AllowExpansion: boolean);
begin
  AllowExpansion := True;
  FoldersMgr.Expand(Node);
end;

initialization

  {$I beegui_extractfrm.lrs}

end.
