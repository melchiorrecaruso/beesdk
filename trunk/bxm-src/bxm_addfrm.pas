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

    Add form class.

  Modifyed:
}

unit bxm_AddFrm;

{$I bxm_compiler.inc}

interface

uses
  Arrow,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  IniFiles,
  LResources,
  Menus,
  StdCtrls,
  SysUtils,
  // ---

  bxm_AddTreeViewMgr,
  bxm_Plugins;

type

  { TAddFrm class }

  TAddFrm = class(TForm)
    AndvancedOptionsLabel: TLabel;
    AdvancedOptionsArrow: TArrow;
    AdvancedOptionsPanel: TPanel;
    ArchiveNameComboBox: TComboBox;
    ArchiveNameLabel: TLabel;
    ArchiveWithPassword: TEdit;
    ArchiveWithPasswordCheck: TCheckBox;
    BtnCancel: TBitBtn;
    BtnDelete: TBitBtn;
    BtnFiles: TBitBtn;
    BtnFolder: TBitBtn;
    BtnHelp: TBitBtn;
    BtnOk: TBitBtn;
    BtnPlusMinus: TBitBtn;
    BtnSave: TBitBtn;
    CompressionMethod: TComboBox;
    CompressionMethodLabel: TLabel;
    Files: TTreeView;
    FilesImages: TImageList;
    FilesLabel: TLabel;
    FilesMgr: TAddTreeViewMgr;
    FilesPanel: TPanel;
    FilesPopupMenu: TPopupMenu;
    OpenDialog: TOpenDialog;
    PopupMenu_AddFiles: TMenuItem;
    PopupMenu_AddFolder: TMenuItem;
    PopupMenu_Delete: TMenuItem;
    PopupMenu_N1: TMenuItem;
    PopupMenu_N2: TMenuItem;
    PopupMenu_PlusMinus: TMenuItem;
    RecurseSubdirectories: TCheckBox;
    Root: TEdit;
    RootArrow: TArrow;
    RootLabel: TLabel;
    RootPanel: TPanel;
    SaveDialog: TSaveDialog;
    SelectFilesDialog: TOpenDialog;
    UpdateMethod: TComboBox;
    UpdateMethodLabel: TLabel;
    UpDown: TUpDown;
    UseCurrentArchiveDirectory: TCheckBox;
    procedure AdvancedOptionsArrowClick(Sender: TObject);
    procedure ArchiveWithPasswordCheckClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FilesSelectionChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure PopupMenu_AddFilesClick(Sender: TObject);
    procedure PopupMenu_AddFolderClick(Sender: TObject);
    procedure PopupMenu_DeleteClick(Sender: TObject);
    procedure PopupMenu_PlusMinusClick(Sender: TObject);
    procedure RootArrowClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
  private
    { private declarations }
    FArchivePath: string;
  public
    { public declarations }
  end;

  function AddShowModal(PCL: TParserCommandLine): longint;

implementation

{$R *.lfm}

uses
  bxm_Consts,
  bxm_Messages,
  bxm_SysUtils;

function AddShowModal(PCL: TParserCommandLine): longint;
var
  Add: TAddFrm;
  i: longint;
begin
  Add := TAddFrm.Create(nil);
  Add.FArchivePath := ExtractFilePath(PCL.ArchiveName);
  Add.ArchiveNameComboBox.Text := ExtractFileName(PCL.ArchiveName);

  Result := Add.ShowModal;
  if Result = mrOk then
  begin
    PCL.Command := cAdd;
    PCL.CompressionMode := TCompressionMode(
      Ord(Add.CompressionMethod.ItemIndex));

    for i := 0 to Add.Files.Items.Count - 1 do
      if Add.Files.Items[i].ImageIndex = 1 then
        PCL.ExcludedFileMasks.Add(Add.Files.Items[i].Text);

    if Add.ArchiveWithPasswordCheck.Checked then
      PCL.Password := Add.ArchiveWithPassword.Text;

    PCL.Recursive := Add.RecurseSubdirectories.Checked;
    PCL.UpdateMode := TUpdateMode(Ord(Add.UpdateMethod.ItemIndex));
    PCL.ArchiveName := Add.FArchivePath + Add.ArchiveNameComboBox.Text;

    for i := 0 to Add.Files.Items.Count - 1 do
      if Add.Files.Items[i].ImageIndex = 0 then
        PCL.FileMasks.Add(Add.Files.Items[i].Text);
  end;
  Add.Destroy;
end;

{ TAddFrm class }

procedure TAddFrm.FormCreate(Sender: TObject);
begin
  RootArrow.ArrowType            := atRight;
  AdvancedOptionsArrow.ArrowType := atRight;

  {$IFDEF MSWINDOWS}
  Constraints.MinHeight := 300;
  Constraints.MaxHeight := 300;
  {$ELSE}
  Constraints.MinHeight := 320;
  Constraints.MaxHeight := 320;
  {$ENDIF}
  Constraints.MinWidth  := 500;
  Constraints.MaxWidth  := 500;
end;

procedure TAddFrm.BtnSaveClick(Sender: TObject);
var
  S: string;
begin
  SaveDialog.FileName := FArchivePath + ArchiveNameComboBox.Text;
  if SaveDialog.Execute then
  begin
    S := SaveDialog.FileName;
    case SaveDialog.FilterIndex of
      1: S := ChangeFileExt(S, '.bee');
      2: S := ChangeFileExt(S, '.exe');
    end;
    FArchivePath := ExtractFilePath(S);
    ArchiveNameComboBox.Text := ExtractFileName(S);
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
  BtnDelete.Enabled := PopupMenu_Delete.Enabled;
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

procedure TAddFrm.RootArrowClick(Sender: TObject);
begin
  RootPanel.Visible := not RootPanel.Visible;
  if RootPanel.Visible then
  begin
    RootArrow.ArrowType := atDown;
    Constraints.MaxHeight := Constraints.MaxHeight + RootPanel.Height;
    Constraints.MinHeight := Constraints.MaxHeight;
  end else
  begin
    RootArrow.ArrowType := atRight;
    Constraints.MinHeight := Constraints.MinHeight - RootPanel.Height;
    Constraints.MaxHeight := Constraints.MinHeight;
  end;
  Height := Constraints.MaxHeight;
end;

procedure TAddFrm.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btNext: FilesMgr.Spin := FilesMgr.Spin - 1;
    btPrev: FilesMgr.Spin := FilesMgr.Spin + 1;
  end;
end;

procedure TAddFrm.AdvancedOptionsArrowClick(Sender: TObject);
begin
  AdvancedOptionsPanel.Visible := not AdvancedOptionsPanel.Visible;
  if AdvancedOptionsPanel.Visible then
  begin
    AdvancedOptionsArrow.ArrowType := atDown;
    Constraints.MaxHeight := Constraints.MaxHeight + AdvancedOptionsPanel.Height;
    Constraints.MinHeight := Constraints.MaxHeight;
  end else
  begin
    AdvancedOptionsArrow.ArrowType := atRight;
    Constraints.MinHeight := Constraints.MinHeight - AdvancedOptionsPanel.Height;
    Constraints.MaxHeight := Constraints.MinHeight;
  end;
  Height := Constraints.MaxHeight;
end;

procedure TAddFrm.ArchiveWithPasswordCheckClick(Sender: TObject);
begin
  ArchiveWithPassword.Enabled := ArchiveWithPasswordCheck.Checked;
end;

end.