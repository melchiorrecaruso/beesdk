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

unit BeeGui_AddFrm;

{$I compiler.inc}

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
  LResources,
  // ---
  BeeGui_AddTreeViewMgr;

type

  { TAddFrm class }

  TAddFrm = class(TForm)
    FilesMgr: TAddTreeViewMgr;
    cdOptionCheck: TCheckBox;
    aOptionCheck: TCheckBox;
    aOption: TComboBox;
    ArchiveNameComboBox: TComboBox;
    ArchiveNameLabel: TLabel;

    cfgOption: TComboBox;
    OpenDialog: TOpenDialog;
    SelectFilesDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    UpDown: TUpDown;
    yOption: TComboBox;
    yOptionBtn: TBitBtn;
    cfgOptionBtn: TBitBtn;
    cdOption: TEdit;
    dOption: TComboBox;
    dOptionLabel: TLabel;
    eOption: TEdit;
    cfgOptionLabel: TLabel;
    eOptionLabel: TLabel;
    mOption: TComboBox;
    mOptionLabel: TLabel;
    ufOption: TComboBox;
    ufOptionLabel: TLabel;
    yOptionLabel: TLabel;
    Pages: TPageControl;
    PageGeneral: TTabSheet;
    PageAdvanced: TTabSheet;
    Options: TGroupBox;
    rOption: TCheckBox;
    sOption: TCheckBox;
    tOption: TCheckBox;
    kOption: TCheckBox;
    PageFiles: TTabSheet;
    FilesLabel: TLabel;
    Files: TTreeView;
    FilesImages: TImageList;
    RootLabel: TLabel;
    Root: TEdit;
    // ---
    PopupMenu: TPopupMenu;
    PopupMenu_AddFolder: TMenuItem;
    PopupMenu_N2: TMenuItem;
    PopupMenu_AddFiles: TMenuItem;
    PopupMenu_N1: TMenuItem;
    PopupMenu_View: TMenuItem;
    PopupMenu_PlusMinus: TMenuItem;
    PopupMenu_Modify: TMenuItem;
    PopupMenu_Delete: TMenuItem;
    BtnSave: TBitBtn;
    BtnFiles: TBitBtn;
    BtnView: TBitBtn;
    BtnModify: TBitBtn;
    BtnDelete: TBitBtn;
    BtnPlusMinus: TBitBtn;
    BtnFolder: TBitBtn;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FilesSelectionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);

    procedure PagesPageChanged(Sender: TObject);
    procedure aOptionCheckChange(Sender: TObject);
    procedure cdOptionCheckChange(Sender: TObject);


    procedure PopupMenu_AddFolderClick(Sender: TObject);
    procedure PopupMenu_AddFilesClick(Sender: TObject);
    procedure PopupMenu_ViewClick(Sender: TObject);
    procedure PopupMenu_PlusMinusClick(Sender: TObject);
    procedure PopupMenu_ModifyClick(Sender: TObject);
    procedure PopupMenu_DeleteClick(Sender: TObject);




    procedure yOptionBtnClick(Sender: TObject);
    procedure cfgOptionBtnClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure BtnSaveClick(Sender: TObject);
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  private
    { private declarations }
    FArchivePath: string;
    function GetArchiveName: string;
    procedure SetArchiveName(Value: string);
  public
    property ArchiveName: string read GetArchiveName write SetArchiveName;
  end;

var
  AddFrm: TAddFrm;
  
implementation

uses
  BeeGui_Consts,
  BeeGui_SysUtils,
  BeeGui_Messages,
  BeeGui_RenameFrm;

 { TAddFrm class }
 
 {$I beegui_addfrm_saveproperty.inc}
 {$I beegui_addfrm_loadproperty.inc}
 {$I beegui_addfrm_savelanguage.inc}
 {$I beegui_addfrm_loadlanguage.inc}
 
  procedure TAddFrm.FormCreate(Sender: TObject);
  begin
    LoadLanguage;
    LoadProperty;

    SetLength(FArchivePath, 0);
    Pages.ActivePage := PageGeneral;
  end;
  
  procedure TAddFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    {$IFDEF DEBUG}
      SaveLanguage;
    {$ENDIF}
    SaveProperty;
  end;

  procedure TAddFrm.aOptionCheckChange(Sender: TObject);
  begin
    aOption.Enabled := aOptionCheck.Checked;
    if aOption.Enabled then
      aOption.Color := clWindow
    else
      aOption.Color := clInactiveCaptionText;
  end;
  
  procedure TAddFrm.cdOptionCheckChange(Sender: TObject);
  begin
    cdOption.Enabled := cdOptionCheck.Checked;
    if cdOption.Enabled then
      cdOption.Color := clWindow
    else
      cdOption.Color := clInactiveCaptionText;
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

  procedure TAddFrm.yOptionBtnClick(Sender: TObject);
  var
    FolderName: string;
  begin
    SetLength(FolderName, 0);
    if SelectDirectory(rsSelectFolder, '', FolderName) then
    begin
      yOption.Text := FolderName;
    end;
  end;
  
  procedure TAddFrm.cfgOptionBtnClick(Sender: TObject);
  begin
    OpenDialog.FileName := cfgOption.Text;
    if OpenDialog.Execute then
    begin
      cfgOption.Text := OpenDialog.FileName;
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
    I: integer;
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

  procedure TAddFrm.PopupMenu_ViewClick(Sender: TObject);
  var
    I: integer;
  begin
    for I := 0 to FilesMgr.Count - 1 do
    begin
      if FilesMgr.MultiSelected[I] then
      begin
        ShellExec(ExcludeTrailingBackSlash(ExtractFilePath(FilesMgr.Items[I])), '');
      end;
    end;
  end;
  
  procedure TAddFrm.PopupMenu_PlusMinusClick(Sender: TObject);
  var
    I: integer;
  begin
    for I := Files.Items.Count - 1 downto 0 do
    begin
      if Files.Items[I].Selected then
      begin
        FilesMgr.PlusMinus(I);
      end;
    end;
  end;

  procedure TAddFrm.PopupMenu_ModifyClick(Sender: TObject);
  var
    I: integer;
    F: TRenameFrm;
    NewMask: string;
  begin
    for I := 0 to FilesMgr.Count - 1 do
      if FilesMgr.MultiSelected[I] then
      begin
        F := TRenameFrm.Create(Self);
        F.ToFN.Text := ExtractFileName(FilesMgr.Items[I]);
        F.FromFN.Caption := ExtractFileName(FilesMgr.Items[I]);
        
        if F.ShowModal = mrOk then
        begin
          NewMask := ExcludeTrailingBackSlash(F.ToFN.Text);
          while Pos(PathDelim, NewMask) = 1 do
          begin
            Delete(NewMask, 1, 1);
          end;

          if Length(NewMask) > 0 then
          begin
            FilesMgr.ChangeMask(I, NewMask);
            FilesMgr.Update;
          end;
        end;
        F.Free;
      end;
  end;

  procedure TAddFrm.PopupMenu_DeleteClick(Sender: TObject);
  var
    I: integer;
  begin
    for I := FilesMgr.Count - 1 downto 0 do
    begin
      if FilesMgr.MultiSelected[I] then
      begin
        FilesMgr.DeleteFile(I);
      end;
    end;
  end;

  procedure TAddFrm.FilesSelectionChanged (Sender: TObject);
  begin
    if Files.Selected = nil then
    begin
      PopupMenu_AddFolder.Enabled := True;
      PopupMenu_AddFiles .Enabled := True;
      // --
      PopupMenu_View     .Enabled := False;
      PopupMenu_PlusMinus.Enabled := False;
      PopupMenu_Modify   .Enabled := False;
      // --
      PopupMenu_Delete   .Enabled := False;
    end else
    begin
      PopupMenu_AddFolder.Enabled := True;
      PopupMenu_AddFiles .Enabled := True;
      // --
      PopupMenu_View     .Enabled := True;
      PopupMenu_PlusMinus.Enabled := True;
      PopupMenu_Modify   .Enabled := True;
      // --
      PopupMenu_Delete   .Enabled := True;
    end;

    BtnView     .Enabled := PopupMenu_View     .Enabled;
    BtnPlusMinus.Enabled := PopupMenu_PlusMinus.Enabled;
    BtnModify   .Enabled := PopupMenu_Modify   .Enabled;
    BtnDelete   .Enabled := PopupMenu_Delete   .Enabled;
  end;

  procedure TAddFrm.FormDropFiles(Sender: TObject; const FileNames: array of string);
  var
    I: integer;
  begin
    for I := Low(FileNames) to High(FileNames) do
    begin
      if DirectoryExists(FileNames[I]) then
        FilesMgr.AddFolder(FileNames[I])
      else
        FilesMgr.AddFile(FileNames[I]);
    end;
  end;

  procedure TAddFrm.PagesPageChanged(Sender: TObject);
  begin
    if Pages.ActivePage = PageFiles then
      AllowdropFiles := True
    else
      AllowDropFiles := False;
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

  {$i beegui_addfrm.lrs}

end.
