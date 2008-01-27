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

unit BeeCore_AddFrm;

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
  LResources,
  XMLPropStorage,
  BeeCore_AddTreeViewMgr;

type

  { TAddFrm class }

  TAddFrm = class(TForm)
    aOptionCheck: TCheckBox;
    aOption: TComboBox;
    ArchiveName: TComboBox;
    ArchiveNameLabel: TLabel;

    cfgOption: TComboBox;
    cfgOption1: TComboBox;
    cfgOptionBtn: TBitBtn;
    cfgOptionBtn1: TBitBtn;
    cdOption: TEdit;
    dOption: TComboBox;
    dOptionLabel: TLabel;
    eOption: TEdit;
    Label1: TLabel;
    cdOptionLabel: TLabel;
    eOptionLabel: TLabel;
    mOption: TComboBox;
    mOptionLabel: TLabel;
    priOption: TComboBox;
    priOptionLabel: TLabel;
    ufOption: TComboBox;
    ufOptionLabel: TLabel;
    yOptionLabel: TLabel;
    lOption: TCheckBox;
    Storage: TXMLPropStorage;
    OpenDialog: TOpenDialog;
    Pages: TPageControl;
    PageGeneral: TTabSheet;
    Advanced: TTabSheet;
    Options: TGroupBox;
    rOption: TCheckBox;
    sOption: TCheckBox;
    tOption: TCheckBox;
    kOption: TCheckBox;
    PageFiles: TTabSheet;
    FilesLabel: TLabel;
    Files: TTreeView;
    FilesImages: TImageList;
    FilesMgr: TAddTreeViewMgr;
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
    BtnOpen: TBitBtn;
    BtnDown: TBitBtn;
    BtnUp: TBitBtn;
    BtnFiles: TBitBtn;
    BtnView: TBitBtn;
    BtnModify: TBitBtn;
    BtnDelete: TBitBtn;
    BtnPlusMinus: TBitBtn;
    BtnFolder: TBitBtn;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    // ---
    procedure aOptionCheckChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FilesSelectionChanged(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure PagesPageChanged(Sender: TObject);
    // ---
    procedure PopupMenu_AddFolderClick(Sender: TObject);
    procedure PopupMenu_AddFilesClick(Sender: TObject);
    procedure PopupMenu_ViewClick(Sender: TObject);
    procedure PopupMenu_PlusMinusClick(Sender: TObject);
    procedure PopupMenu_ModifyClick(Sender: TObject);
    procedure PopupMenu_DeleteClick(Sender: TObject);
    // ---
    procedure BtnUpClick(Sender: TObject);
    procedure BtnDownClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
  public
    { public declarations }
  private
    { private declarations }
  end;
  

  { Confirm Add routines }

  function ConfirmAdd(const AppParams: TStringList): boolean;
  
implementation

uses
  BeeCore_RenameFrm,
  BeeCore_SysUtils,
  Bee_Common;

 { TAddFrm class }
 
  procedure TAddFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    Pages.ActivePage := PageGeneral;
    CfgFolder := IncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder + ('addfrm.xml');
    end;
    {$i beecore_addfrm.inc}
    Storage.Restore;
  end;

  procedure TAddFrm.aOptionCheckChange(Sender: TObject);
  begin
    aOption.Enabled := aOptionCheck.Checked;
    if aOption.Enabled then
      aOption.Color := clWindow
    else
      aOption.Color := clBtnFace;
  end;
  
  procedure TAddFrm.BtnOpenClick(Sender: TObject);
  begin
    if OpenDialog.Execute then
    begin
      ArchiveName.Text := OpenDialog.FileName;
    end;
  end;
  
  procedure TAddFrm.PopupMenu_AddFolderClick(Sender: TObject);
  var
    FolderName: string;
  begin
    FolderName := '';
    if SelectDirectory('AddFrm_MSG_SelectDir', '', FolderName) then
    begin
      FilesMgr.AddFolder(FolderName);
    end;
    Files.Selected := nil;
  end;
  
  procedure TAddFrm.PopupMenu_AddFilesClick(Sender: TObject);
  var
    I: integer;
  begin
    if OpenDialog.Execute then
    begin
      for I := 0 to OpenDialog.Files.Count - 1 do
      begin
        FilesMgr.AddFile(OpenDialog.Files[I]);
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
      if FilesMgr.Selected[I] then
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
      if FilesMgr.Selected[I] then
      begin
        F := TRenameFrm.Create(Self);
        F.RenameTo.Caption := ExtractFileName(FilesMgr.Items[I]);

        if F.ShowModal = mrOk then
        begin
          NewMask := ExcludeTrailingBackSlash(F.RenameTo.Text);
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
      if FilesMgr.Selected[I] then
      begin
        FilesMgr.DeleteFile(I);
      end;
    end;
  end;
  
  procedure TAddFrm.BtnUpClick (Sender: TObject);
  begin
    FilesMgr.Spin := FilesMgr.Spin + 1;
  end;

  procedure TAddFrm.BtnDownClick(Sender: TObject);
  begin
    FilesMgr.Spin := FilesMgr.Spin - 1;
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
  
  { Confirm Add routines }

  function ConfirmAdd(const AppParams: TStringList): boolean;
  var
    fOption: boolean;
    uOption: boolean;
    F: TAddFrm;
    i: integer;
    S: string;
  begin
    fOption := True;
    uOption := True;

    F := TAddFrm.Create(Application);
    // catch options, command, archive name and name of files
    for i := 0 to AppParams.Count - 1 do
    begin
      S := AppParams.Strings[I];
      if (Length(S) > 1) and (S[1] = '-') then
      begin
        // options...
        case UpCase(S[2]) of
          'S': begin
                 Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   F.sOption.Checked := True
                 else
                   if (S = '-') then F.sOption.Checked := False;
               end;
          'U': begin
                 Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   uOption := True
                 else
                   if (S = '-') then uOption := False;
               end;
        'F': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 fOption := True
               else
                 if (S = '-') then fOption := False;
             end;
        'T': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 tOption := True
               else
                 if (S = '-') then tOption := False;
             end;
        'L': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 lOption := True
               else
                 if (S = '-') then lOption := False;
             end;
        'K': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 kOption := True
               else
                 if (S = '-') then kOption := False;
             end;
        'R': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 rOption := True
               else
                 if (S = '-') then rOption := False;
             end;
        'Y': begin
               Delete(S, 1, 2);
               if Bee_Common.DirectoryExists(Bee_Common.ExcludeTrailingBackslash(S)) then
               begin
                 yOption := Bee_Common.ExcludeTrailingBackslash(S);
               end;
             end;
        'A': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 aOption := 'beesfx.bin'
               else
                 if (S = '-') then
                   aOption := 'beesfx.empty'
                 else
                   aOption := S;
             end;
        'M': begin
               Delete(S, 1, 2);
               if (Length(S)= 1) and (S[1] in ['0'..'3']) then
               begin
                 Cfg.Selector('\main');
                 Cfg.CurrentSection.Values['Method'] := S;
               end;
             end;
        'O': begin
               Delete(S, 1, 2);
               if (Length(S) = 1) and (UpCase(S[1]) in ['A', 'S', 'Q']) then
               begin
                 oOption := UpCase(S[1]);
               end;
             end;
        'D': begin
               Delete(S, 1, 2);
               if (Length(S)= 1) and (S[1] in ['0'..'9']) then
               begin
                 Cfg.Selector('\main');
                 Cfg.CurrentSection.Values['Dictionary'] := S;
               end;
             end;
        'E': begin
               Delete(S, 1, 2);
               if ExtractFileExt('.' + S) <> '.' then
               begin
                 eOption := ExtractFileExt('.' + S);
               end;
             end;
        'X': begin
               Delete(S, 1, 2);
               if Length(S) > 0 then
               begin
                 xOption.Add(S);
               end;
             end;
        else if FileNamePos('-pri', S) = 1 then
             begin
               Delete(S, 1, 4);
               if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
               begin
                 SetPriority(StrToInt(S[1]));
               end;
             end else
             begin
               if FileNamePos('-cd', S) = 1 then
               begin
                 Delete(S, 1, 3);
                 if Length(cdOption) > 0 then
                 begin
                   cdOption := Bee_Common.IncludeTrailingBackslash(Bee_Common.FixDirName(S));
                 end;
               end;
             end;
        end; // end case
    end else
    begin
      // command or filenames...
      if Command = ' ' then
      begin
        if Length(S) = 1 then
          Command := UpCase(S[1])
        else
          Command := '?';
      end else
        if ArcName = '' then
        begin
          ArcName := S;
          if ExtractFileExt(ArcName) = '' then
          begin
            ArcName := ChangeFileExt(ArcName, '.bee');
          end;
        end else
          FileMasks.Add(Bee_Common.DoDirSeparators(S));
    end;
  end; // end for loop



    if F.ShowModal = mrOk then
    begin
      Result := True;
      
    end else
      Result := False;
    F.Free;
  end;

initialization

  {$i beecore_addfrm.lrs}

end.
