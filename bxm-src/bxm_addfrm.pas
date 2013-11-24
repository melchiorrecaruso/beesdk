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

{ Contains:

    BeeGui Add form.

  Modifyed:
}

unit bxm_AddFrm;

{$I bxm_compiler.inc}

interface

uses
  Forms,
  Menus,
  Buttons,
  Dialogs,
  Classes,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ComCtrls,
  IniFiles,
  LResources, EditBtn, ExtCtrls, Arrow, DividerBevel,
  // ---
  bxm_AddTreeViewMgr;

type

  { TAddFrm class }

  TAddFrm = class(TForm)
    AdvancedOptionsArrow: TArrow;
    ArchiveWithPassword: TEdit;
    ArchiveWithPasswordCheck: TCheckBox;
    CompressionMethod: TComboBox;
    CompressionMethodLabel: TLabel;
    AdvancedOptionsPanel: TPanel;
    RecurseSubdirectories: TCheckBox;
    Root: TEdit;
    RootPanel: TPanel;
    RootArrow: TArrow;
    BtnHelp: TBitBtn;

    BtnCancel: TBitBtn;
    BtnDelete: TBitBtn;
    BtnFiles:  TBitBtn;
    BtnFolder: TBitBtn;
    BtnOk:     TBitBtn;
    BtnPlusMinus: TBitBtn;
    BtnSave: TBitBtn;


    ArchiveNameLabel: TLabel;
    ArchiveNameComboBox: TComboBox;
    FilesLabel: TLabel;
    FilesPanel: TPanel;
    FilesMgr: TAddTreeViewMgr;
    Files: TTreeView;
    AndvancedOptionsLabel: TLabel;
    RootLabel: TLabel;
    UpdateMethod: TComboBox;
    UpdateMethodLabel: TLabel;
    UpDown1: TUpDown;











    OpenDialog: TOpenDialog;





    SelectFilesDialog: TOpenDialog;
    SaveDialog: TSaveDialog;





    FilesImages: TImageList;
    // ---
    PopupMenu: TPopupMenu;
    PopupMenu_AddFolder: TMenuItem;
    PopupMenu_N2: TMenuItem;
    PopupMenu_AddFiles: TMenuItem;
    PopupMenu_N1: TMenuItem;
    PopupMenu_PlusMinus: TMenuItem;
    PopupMenu_Delete: TMenuItem;
    UseCurrentArchiveDirectory: TCheckBox;

    procedure AdvancedOptionsArrowClick(Sender: TObject);
    procedure ArchiveWithPasswordCheckClick(Sender: TObject);



    procedure FormCreate(Sender: TObject);
    procedure FilesSelectionChanged(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);







    procedure PopupMenu_AddFolderClick(Sender: TObject);
    procedure PopupMenu_AddFilesClick(Sender: TObject);

    procedure PopupMenu_PlusMinusClick(Sender: TObject);

    procedure PopupMenu_DeleteClick(Sender: TObject);
    procedure RootArrowClick(Sender: TObject);





    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure BtnSaveClick(Sender: TObject);
  public
    { public declarations }
  private
    { private declarations }
    FArchivePath: string;
    function GetArchiveName: string;
    procedure SetArchiveName(Value: string);
  public
    property ArchiveName: string Read GetArchiveName Write SetArchiveName;
  end;

var
  AddFrm: TAddFrm;

implementation

uses
  bxm_Consts,
  bxm_SysUtils,
  bxm_Messages;

{ TAddFrm class }

procedure TAddFrm.FormCreate(Sender: TObject);
begin
  SetLength(FArchivePath, 0);
  // ---
  RootArrow.ArrowType            := atRight;
  AdvancedOptionsArrow.ArrowType := atRight;
  Height := 320;
end;

procedure TAddFrm.AdvancedOptionsArrowClick(Sender: TObject);
begin
  AdvancedOptionsPanel.Visible := not AdvancedOptionsPanel.Visible;
  if AdvancedOptionsPanel.Visible then
  begin
    AdvancedOptionsArrow.ArrowType := atDown;
    Height := Height + AdvancedOptionsPanel.Height;
  end else
  begin
    AdvancedOptionsArrow.ArrowType := atRight;
    Height := Height - AdvancedOptionsPanel.Height;
  end;
end;

procedure TAddFrm.ArchiveWithPasswordCheckClick(Sender: TObject);
begin
  ArchiveWithPassword.Enabled := ArchiveWithPasswordCheck.Checked;
end;

procedure TAddFrm.BtnSaveClick(Sender: TObject);
var
  S: string;
begin
  SaveDialog.FileName := ArchiveName;
  if SaveDialog.Execute then
  begin
    S := SaveDialog.FileName;
    case SaveDialog.FilterIndex of
      1: S := ChangeFileExt(S, '.bee');
      2: S := ChangeFileExt(S, '.exe');
    end;
    ArchiveName := S;
  end;
end;

procedure TAddFrm.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btNext: FilesMgr.Spin := FilesMgr.Spin - 1;
    btPrev: FilesMgr.Spin := FilesMgr.Spin + 1;
  end;
end;

procedure TAddFrm.PopupMenu_AddFolderClick(Sender: TObject);
var
  FolderName: string;
begin
  SetLength(FolderName, 0);
  if SelectDirectory(rsSelectFolder, '', FolderName) then
  begin
    FilesMgr.AddFolder(FolderName);
    if (FilesMgr.Count = 1) and (FilesMgr.Spin = 0) then
    begin
      FilesMgr.Spin := 1;
    end;
  end;
  Files.Selected := nil;
end;

procedure TAddFrm.PopupMenu_AddFilesClick(Sender: TObject);
var
  I: longint;
begin
  if SelectFilesDialog.Execute then
  begin
    for I := 0 to SelectFilesDialog.Files.Count - 1 do
    begin
      FilesMgr.AddFile(SelectFilesDialog.Files[I]);
    end;
    Files.Selected := nil;
  end;
end;

procedure TAddFrm.PopupMenu_PlusMinusClick(Sender: TObject);
var
  I: longint;
begin
  for I := Files.Items.Count - 1 downto 0 do
    if Files.Items[I].Selected then
    begin
      FilesMgr.PlusMinus(I);
    end;
end;

procedure TAddFrm.PopupMenu_DeleteClick(Sender: TObject);
var
  I: longint;
begin
  for I := FilesMgr.Count - 1 downto 0 do
    if FilesMgr.MultiSelected[I] then
    begin
      FilesMgr.DeleteFile(I);
    end;
end;

procedure TAddFrm.RootArrowClick(Sender: TObject);
begin
  RootPanel.Visible := not RootPanel.Visible;
  if RootPanel.Visible then
  begin
    RootArrow.ArrowType := atDown;
    Height := Height + RootPanel.Height;
  end else
  begin
    RootArrow.ArrowType := atRight;
    Height := Height - RootPanel.Height;
  end;
end;

procedure TAddFrm.FilesSelectionChanged(Sender: TObject);
begin
  if Files.Selected = nil then
  begin
    PopupMenu_AddFolder.Enabled := TRUE;
    PopupMenu_AddFiles.Enabled  := TRUE;
    PopupMenu_PlusMinus.Enabled := FALSE;
    PopupMenu_Delete.Enabled    := FALSE;
  end else
  begin
    PopupMenu_AddFolder.Enabled := TRUE;
    PopupMenu_AddFiles.Enabled  := TRUE;
    PopupMenu_PlusMinus.Enabled := TRUE;
    PopupMenu_Delete.Enabled    := TRUE;
  end;

  BtnPlusMinus.Enabled := PopupMenu_PlusMinus.Enabled;
  BtnDelete.Enabled    := PopupMenu_Delete.Enabled;
end;

procedure TAddFrm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: longint;
begin
  for I := Low(FileNames) to High(FileNames) do
  begin
    if DirectoryExists(FileNames[I]) then
      FilesMgr.AddFolder(FileNames[I])
    else
      FilesMgr.AddFile(FileNames[I]);
  end;
end;

procedure TAddFrm.SetArchiveName(Value: string);
begin
  FArchivePath := ExtractFilePath(Value);
  ArchiveNameComboBox.Text := ExtractFileName(Value);
end;

function TAddFrm.GetArchiveName: string;
begin
  Result := FArchivePath + ArchiveNameComboBox.Text;
end;

initialization

  {$i bxm_addfrm.lrs}

end.