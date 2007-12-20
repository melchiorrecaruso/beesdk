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

unit BeeGui_ExtractFrm;

{$I Compiler.inc}

interface

uses
  Forms,
  Buttons,
  Classes,
  ComCtrls,
  StdCtrls,
  Controls,
  SysUtils,
  //---
  BeeGui_ConfigFrm,
  BeeGui_FormStorage,
  BeeGui_FolderTreeManager;

type
  TExtractFrm = class(TForm)
    ExtractFrm_FolderTree: TTreeView;
    ExtractFrm_OverWriteGB: TGroupBox;
    ExtractFrm_OverWrite: TComboBox;
    ExtractFrm_OptionsGB: TGroupBox;
    ExtractFrm_xCommand: TCheckBox;
    ConfigFrm_PriorityGB: TGroupBox;
    General_Priority: TComboBox;
    ExtractFrm_cOption: TCheckBox;
    ExtractFrm_FolderLabel: TLabel;
    ExtractFrm_Folder: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FFormStorage: TFormStorage;
    FFolderTreeManager: TFolderTreeManager;
  public
    { Public declarations }
    function GetCommandStr: string;
    function GetPriorityStr: string;
    function GetOverWriteModestr: string;
    procedure GetOptions(aConfigFrm: TConfigFrm);
  end;

var
  ExtractFrm: TExtractFrm;

implementation

{$R *.dfm}

uses
  BeeGui_ExtractFrmRes,
  BeeGui_SysUtils;

// TExtractFrm

procedure TExtractFrm.FormCreate;
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SExtractFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SExtractFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SExtractFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
  FFolderTreeManager := TFolderTreeManager.Create(ExtractFrm_FolderTree);
  FFolderTreeManager.FolderEdit := ExtractFrm_Folder;
  FFolderTreeManager.Initialize;
  // --
  FFolderTreeManager.CurrentFolder := ExtractFrm_Folder.Text;
end;

procedure TExtractFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SExtractFrm_PropertyFull
  else
    FFormStorage.Properties := _SExtractFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
  // --
  FFolderTreeManager.Free;
end;

function TExtractFrm.GetCommandStr: string;
begin
  case ExtractFrm_xCommand.Checked of
    True: Result := 'x';
    else
      Result := 'e';
  end;
end;

function TExtractFrm.GetPriorityStr: string;
begin
  case General_Priority.ItemIndex of
    0: Result := '-pri0';
    1: Result := '-pri1';
    2: Result := '-pri2';
    3: Result := '-pri3';
    else
      Result := '-pri1';
  end;
end;

function TExtractFrm.GetOverWriteModestr: string;
begin
  case ExtractFrm_OverWrite.ItemIndex of
    0: Result := '-oQ';
    1: Result := '-oA';
    2: Result := '-oS';
    else
      Result := '-oQ';
  end;
end;

procedure TExtractFrm.GetOptions(aConfigFrm: TConfigFrm);
begin
  ExtractFrm_OverWrite.ItemIndex := aConfigFrm.ExtractFrm_OverWrite.ItemIndex;
  ExtractFrm_xCommand.Checked := aConfigFrm.ExtractFrm_xCommand.Checked;
  ExtractFrm_cOption.Checked := aConfigFrm.ExtractFrm_cOption.Checked;
  General_Priority.ItemIndex := aConfigFrm.General_Priority.ItemIndex;
end;

end.
