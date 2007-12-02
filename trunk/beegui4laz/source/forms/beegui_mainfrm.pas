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

    BeeGui Main form.

    Modifyed:

    v1.0.1 build 9156 - 2005.07.16 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.1 build 9160 - 2005.08.03 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0130 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_MainFrm;

interface

uses
  Forms,
  Menus,
  Classes,
  Dialogs,
  Buttons,
  SysUtils,
  Graphics,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  LResources,
  XMLPropStorage,
  // --
  Bee_Interface,
  BeeGui_IconList,
  BeeGui_AppViewer,
  BeeGui_AddTreeView,
  BeeGui_ArchiveTreeView,
  BeeGui_StatusProgressBar;

type
  TTerminateWith = (twNone, twNewArc, twCheckOut,
    twViewInt, twViewExt, twDrag, twClose);

type
  { TMainFrm }

  TMainFrm = class(TForm)
    MainFrm_LargeIconList: TIconList;
    MainFrm_SmallIconList: TIconList;
    MainFrm_ButtonsPanel: TPanel;
    MainFrm_Storage: TXMLPropStorage;
    MainFrm_ArchiveTreeView: TArchiveTreeView;
    // ---
    BtnNew: TSpeedButton;
    BtnOpen: TSpeedButton;
    BevelFirst: TBevel;
    BtnAdd: TSpeedButton;
    BtnExtract: TSpeedButton;
    BtnView: TSpeedButton;
    BtnDelete: TSpeedButton;
    BevelSecond: TBevel;
    BtnTest: TSpeedButton;
    BtnConfig: TSpeedButton;
    BtnCheckOut: TSpeedButton;
    BevelThird: TBevel;
    BtnHelp: TSpeedButton;
    BtnExit: TSpeedButton;
    // ---
    MainFrm_OpenDialog: TOpenDialog;
    MainFrm_SaveDialog: TSaveDialog;
    MainFrm_FontDialog: TFontDialog;
    // ---
    MainFrm_PopupMenu: TPopupMenu;
    MainFrm_PopupMenu_N1: TMenuItem;
    MainFrm_PopupMenu_N2: TMenuItem;
    MainFrm_PopupMenu_N3: TMenuItem;
    MainFrm_PopupMenu_N4: TMenuItem;
    MainFrm_PopupMenu_Open: TMenuItem;
    MainFrm_PopupMenu_Open_IntViewer: TMenuItem;
    MainFrm_PopupMenu_Delete: TMenuItem;
    MainFrm_PopupMenu_Extract: TMenuItem;
    MainFrm_PopupMenu_ExtractAll: TMenuItem;
    MainFrm_PopupMenu_Test: TMenuItem;
    MainFrm_PopupMenu_Rename: TMenuItem;
    MainFrm_PopupMenu_ExpandCollapse: TMenuItem;
    MainFrm_PopupMenu_ExpandOne: TMenuItem;
    MainFrm_PopupMenu_CollapseAll: TMenuItem;
    MainFrm_PopupMenu_ExpandAll: TMenuItem;
    MainFrm_PopupMenu_CollapseOne: TMenuItem;
    MainFrm_PopupMenu_SelectDeselect: TMenuItem;
    MainFrm_PopupMenu_SelectAll: TMenuItem;
    MainFrm_PopupMenu_SelectMask: TMenuItem;
    MainFrm_PopupMenu_DeselectAll: TMenuItem;
    MainFrm_PopupMenu_DeselectMask: TMenuItem;
    MainFrm_PopupMenu_Invert: TMenuItem;
    MainFrm_PopupMenu_Property: TMenuItem;
    // ---
    MainFrm_PopupMenu_Btns: TPopupMenu;
    MainFrm_PopupMenu_Btns_N1: TMenuItem;
    MainFrm_PopupMenu_Btns_N2: TMenuItem;
    MainFrm_PopupMenu_Btns_N3: TMenuItem;
    MainFrm_PopupMenu_Btns_New: TMenuItem;
    MainFrm_PopupMenu_Btns_Open: TMenuItem;
    MainFrm_PopupMenu_Btns_Add: TMenuItem;
    MainFrm_PopupMenu_Btns_Extract: TMenuItem;
    MainFrm_PopupMenu_Btns_View: TMenuItem;
    MainFrm_PopupMenu_Btns_Delete: TMenuItem;
    MainFrm_PopupMenu_Btns_Test: TMenuItem;
    MainFrm_PopupMenu_Btns_CheckOut: TMenuItem;
    MainFrm_PopupMenu_Btns_Config: TMenuItem;
    MainFrm_PopupMenu_Btns_Help: TMenuItem;
    MainFrm_PopupMenu_Btns_Exit: TMenuItem;
    // ---
    MainFrm_MainMenu: TMainMenu;
    MainFrm_MainMenu_File: TMenuItem;
    MainFrm_MainMenu_File_N1: TMenuItem;
    MainFrm_MainMenu_File_N2: TMenuItem;
    MainFrm_MainMenu_File_N3: TMenuItem;
    MainFrm_MainMenu_File_New: TMenuItem;
    MainFrm_MainMenu_File_Open: TMenuItem;
    MainFrm_MainMenu_File_Close: TMenuItem;
    MainFrm_MainMenu_File_Info: TMenuItem;
    MainFrm_MainMenu_File_Move: TMenuItem;
    MainFrm_MainMenu_File_Copy: TMenuItem;
    MainFrm_MainMenu_File_Rename: TMenuItem;
    MainFrm_MainMenu_File_Delete: TMenuItem;
    MainFrm_MainMenu_File_Exit: TMenuItem;
    // ---
    MainFrm_MainMenu_Actions: TMenuItem;
    MainFrm_MainMenu_Actions_N1: TMenuItem;
    MainFrm_MainMenu_Actions_N2: TMenuItem;
    MainFrm_MainMenu_Actions_Add: TMenuItem;
    MainFrm_MainMenu_Actions_Delete: TMenuItem;
    MainFrm_MainMenu_Actions_Extract: TMenuItem;
    MainFrm_MainMenu_Actions_ExtractAll: TMenuItem;
    MainFrm_MainMenu_Actions_Test: TMenuItem;
    MainFrm_MainMenu_Actions_Rename: TMenuItem;
    MainFrm_MainMenu_Actions_View: TMenuItem;
    MainFrm_MainMenu_Actions_SelectAll: TMenuItem;
    MainFrm_MainMenu_Actions_SelectMask: TMenuItem;
    MainFrm_MainMenu_Actions_DeselectAll: TMenuItem;
    MainFrm_MainMenu_Actions_DeselectMask: TMenuItem;
    MainFrm_MainMenu_Actions_Invert: TMenuItem;
    MainFrm_MainMenu_Actions_CheckOut: TMenuItem;
    MainFrm_MainMenu_Actions_TestAll: TMenuItem;
    // ---
    MainFrm_MainMenu_Options: TMenuItem;
    MainFrm_MainMenu_Options_N1: TMenuItem;
    MainFrm_MainMenu_Options_N2: TMenuItem;
    MainFrm_MainMenu_Options_Config: TMenuItem;
    MainFrm_MainMenu_Options_Password: TMenuItem;
    MainFrm_MainMenu_Options_SaveOnExit: TMenuItem;
    MainFrm_MainMenu_Options_SaveNow: TMenuItem;
    MainFrm_MainMenu_Options_Default: TMenuItem;
    MainFrm_MainMenu_Options_LogReport: TMenuItem;
    // ---
    MainFrm_MainMenu_Help: TMenuItem;
    MainFrm_MainMenu_Help_N1: TMenuItem;
    MainFrm_MainMenu_Help_N2: TMenuItem;
    MainFrm_MainMenu_Help_F1: TMenuItem;
    MainFrm_MainMenu_Help_Internet: TMenuItem;
    MainFrm_MainMenu_Help_License: TMenuItem;
    MainFrm_MainMenu_Help_About: TMenuItem;
    MainFrm_ProgressBar: TProgressBar;
    MainFrm_StatusBar: TStatusProgressBar;
    // ---
    procedure MainFrm_MainMenu_File_NewClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_OpenClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_CloseClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_InfoClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_MoveClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_CopyClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_RenameClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_DeleteClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_ExitClick (Sender: TObject);
    // ----
    procedure MainFrm_MainMenu_Actions_AddClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_DeleteClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ExtractClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ExtractAllClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_TestClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_RenameClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ViewClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_SelectAllClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_SelectMaskClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_DeselectMasksClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_DeselectAllClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_InvertClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_TestAllClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_CheckOutClick (Sender: TObject);
    // ---
    procedure MainFrm_MainMenu_Options_ConfigClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_SaveNowClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_DefaultClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_PasswordClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_LogReportClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_SaveOnExitClick (Sender: TObject);
    // ---
    procedure MainFrm_MainMenu_Help_F1Click (Sender: TObject);
    procedure MainFrm_MainMenu_Help_InternetClick (Sender: TObject);
    procedure MainFrm_MainMenu_Help_LicenseClick (Sender: TObject);
    procedure MainFrm_MainMenu_Help_AboutClick (Sender: TObject);
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    // ---
    procedure MainFrm_ArchiveTreeViewSelectionChanged(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_AddClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_CheckOutClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_ConfigClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_DeleteClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_ExitClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_ExtractClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_HelpClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_N1Click(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_N2Click(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_N3Click(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_NewClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_OpenClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_TestClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Btns_ViewClick(Sender: TObject);
    // ---
    procedure MainFrm_PopupMenuPopup(Sender: TObject);
    procedure MainFrm_PopupMenu_Open_IntViewerClick(Sender: TObject);
    procedure MainFrm_PopupMenu_CollapseAllClick(Sender: TObject);
    procedure MainFrm_PopupMenu_CollapseOneClick(Sender: TObject);
    procedure MainFrm_PopupMenu_ExpandAllClick(Sender: TObject);
    procedure MainFrm_PopupMenu_ExpandOneClick(Sender: TObject);
    procedure MainFrm_PopupMenu_PropertyClick(Sender: TObject);
  public
    App: TApp;
    AppKey: string;
    AppArcName: string;
    AppParams: TStringList;
    AppTerminated: boolean;
    AppTerminateWith: TTerminateWith;
    AppTerminatedWithError: boolean;
    procedure AppCreate(CreateLogFile: boolean);
  private
    AppRenameFolder: boolean;
    AppRenameFolderTo: string;
    AppRenameFolderFrom: string;
    AppTempFolder: string;
    AppCheckOutFolder: string;
    AppLogFile: TStringList;
    AppInterface: TAppInterface;
    procedure OnAppKey;
    procedure OnAppTick;
    procedure OnAppList;
    procedure OnAppError;
    procedure OnAppClear;
    procedure OnAppRename;
    procedure OnAppWarning;
    procedure OnAppDisplay;
    procedure OnAppOverWrite;
    procedure OnAppFatalError;
    procedure OnAppRequest;
    procedure OnAppTerminate(Sender: TObject);
    // ---
    procedure AppTerminateWithLog;
    procedure AppTerminateWithNew;
    procedure AppTerminateWithCheckOut;
    procedure AppTerminateWithView(FileName: string; UseIntViewer: boolean);
 private
    AppViewer: TAppViewer;
    AppViewerTerminated: boolean;
    procedure OnAppViewerTerminate(Sender: TObject);
  private
    procedure MainFrm_AddFile(const FileName: string);
    procedure MainFrm_AddFiles(FilesList: TStrings; EnableFilesList: boolean);
    // ---
    function  MainFrm_ProcessFiles(var Params: TStringList): integer;
    procedure MainFrm_ProcessFolder(FolderNode: TTreeNode; var Params: TStringList);
    // ---
    procedure MainFrm_ConfigFrm_Show(PageIndex: integer);
    procedure MainFrm_DeleteFolders;
    // ---
    procedure MainFrm_UpdateButtons; overload;
    procedure MainFrm_UpdateCursor(Value: TCursor);
    procedure MainFrm_UpdateButtons(Value: boolean); overload;
  end;

var
  MainFrm: TMainFrm;
  
implementation

uses
  dynlibs,

  Bee_App,
  // ---
  BeeGui_SysUtils,
  // ---
  BeeGui_AddFrm,
  BeeGui_InfoFrm,
  BeeGui_AboutFrm,
  BeeGui_SeDeseFrm,
  BeeGui_RenameFrm,
  BeeGui_ConfigFrm,
  BeeGui_ExtractFrm,
  BeeGui_PasswordFrm,
  BeeGui_OverwriteFrm,
  BeeGui_IntViewerFrm;

  // - Section ------------------------------------------------------------ //
  //                                                                        //
  //  MainFrm                                                               //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.FormCreate

  procedure TMainFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    AppKey                   := '';
    AppArcName               := '';
    AppTempFolder            := GetApplicationTempDir;
    AppCheckOutFolder        := GetApplicationCheckOutDir;
    // ---
    AppParams                := TStringList.Create;
    AppLogFile               := nil;
    // ---
    AppInterface              := TAppInterface.Create;
    AppInterface.OnKey        := MainFrm.OnAppKey;
    AppInterface.OnList       := MainFrm.OnAppList;
    AppInterface.OnTick       := MainFrm.OnAppTick;
    AppInterface.OnError      := MainFrm.OnAppError;
    AppInterface.OnClear      := MainFrm.OnAppClear;
    AppInterface.OnRename     := MainFrm.OnAppRename;
    AppInterface.OnWarning    := MainFrm.OnAppWarning;
    AppInterface.OnDisplay    := MainFrm.OnAppDisplay;
    AppInterface.OnOverWrite  := MainFrm.OnAppOverWrite;
    AppInterface.OnFatalError := MainFrm.OnAppFatalError;
    AppInterface.OnRequest    := MainFrm.OnAppRequest;

    // ---
    MainFrm_SmallIconList.Initialize(ExtractFilePath(ParamStr(0)) + 'smallicons');
    MainFrm_LargeIconList.Initialize(ExtractFilePath(ParamStr(0)) + 'largeicons');
    // ---
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      MainFrm_Storage.FileName := CfgFolder + ('mainfrm.xml');
    end;
    {$I beegui_mainfrm.inc}
    MainFrm_Storage.Restore;
    // ---
    MainFrm_UpdateButtons(False);
    MainFrm_UpdateButtons;
  end;
  
  /// TMainFrm.FormDestroy
  
  procedure TMainFrm.FormDestroy(Sender: TObject);
  begin
    // ---
  end;

  /// TMainFrm.FormCloseQuery

  procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then
    begin
      CanClose := MessageDlg('Active proces. Terminate it?', mtWarning, [mbYes, mbNo], 0) = mrYes;
    end else
    begin
      CanClose := True;
    end;
  end;

  /// TMainFrm.FormClose

  procedure TMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    if not MainFrm_MainMenu_Options_SaveOnExit.Checked then
    begin
      MainFrm.SessionProperties := '';
      ConfigFrm.SessionProperties := '';
    end;
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then
    begin
      if Assigned(App) then App.Terminate;
      if Assigned(AppViewer) then AppViewer.Terminate;
    end else
    begin
      MainFrm_MainMenu_File_Close.Click;
    end;
  end;

  /// TMainFrm.FormResize

  procedure TMainFrm.FormResize(Sender: TObject);
  begin
    MainFrm_StatusBar.Panels[0].Width := MainFrm_StatusBar.Width * 2 div 3;
    MainFrm_StatusBar.Panels[1].Width := MainFrm_StatusBar.Width * 1 div 3;
    // ---
    MainFrm_StatusBar.DrawProgressBar(1);
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Update Graphic interface                                              //
  //                                                                        //
  //  --------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_UpdateButtons

  procedure TMainFrm.MainFrm_UpdateButtons;
  var
    Buttons: array [0..13] of TControl;
    CurrLeft:integer;
    CurrTop: integer;
    Check: boolean;
    I: integer;
  begin
    Buttons[ 0] := BtnNew;
    Buttons[ 1] := BtnOpen;
    Buttons[ 2] := BevelFirst;
    Buttons[ 3] := BtnAdd;
    Buttons[ 4] := BtnExtract;
    Buttons[ 5] := BtnView;
    Buttons[ 6] := BtnDelete;
    Buttons[ 7] := BevelSecond;
    Buttons[ 8] := BtnTest;
    Buttons[ 9] := BtnCheckOut;
    Buttons[10] := BevelThird;
    Buttons[11] := BtnConfig;
    Buttons[12] := BtnHelp;
    Buttons[13] := BtnExit;
    // ---
    CurrTop := 6;
    CurrLeft := 6;
    MainFrm_ButtonsPanel.Height := 68;
    for I:= Low(Buttons) to High(Buttons) do
    begin
      Buttons[I].Visible := MainFrm_PopupMenu_Btns.Items[I].Checked;
      if Buttons[I].Visible then
      begin
        Buttons[I].Top := CurrTop;
        Buttons[I].Left := CurrLeft;
        Inc(CurrLeft, Buttons[I].Width + 4);
      end;
    end;
    // ---
    Check := False;
    for I:= Low(Buttons) to High(Buttons) do
    begin
      if Buttons[I].Visible then
      begin
        Check := True;
        Break;
      end;;
    end;
    MainFrm_ButtonsPanel.Visible := Check;
    //---
    MainFrm.Refresh;
  end;

  /// TMainFrm.MainFrm_UpdateCursor

  procedure TMainFrm.MainFrm_UpdateCursor(Value: TCursor);
  begin
    MainFrm.Cursor := Value;
    MainFrm_ArchiveTreeView.Cursor := Value;
  end;

  /// TMainFrm.MainFrm_UpdateButtons

  procedure TMainFrm.MainFrm_UpdateButtons(Value: boolean);
  var
    I: integer;
  begin
    // Lazarus bug
    BtnAdd     .Enabled := Value;  BtnAdd     .Font := MainFrm.Font;
    BtnExtract .Enabled := Value;  BtnExtract .Font := MainFrm.Font;
    BtnView    .Enabled := Value;  BtnView    .Font := MainFrm.Font;
    BtnDelete  .Enabled := Value;  BtnDelete  .Font := MainFrm.Font;
    BtnTest    .Enabled := Value;  BtnTest    .Font := MainFrm.Font;
    BtnCheckOut.Enabled := Value;  BtnCheckOut.Font := MainFrm.Font;
    // ---
    MainFrm_MainMenu_File_Close .Enabled := Value;
    MainFrm_MainMenu_File_Info  .Enabled := Value;
    MainFrm_MainMenu_File_Move  .Enabled := Value;
    MainFrm_MainMenu_File_Copy  .Enabled := Value;
    MainFrm_MainMenu_File_Rename.Enabled := Value;
    MainFrm_MainMenu_File_Delete.Enabled := Value;
    // ---
    for I := 0 to MainFrm_MainMenu_Actions.Count -1 do
    begin
      MainFrm_MainMenu_Actions.Items[I].Enabled := Value;
    end;
    // ---
    MainFrm_ArchiveTreeView.Enabled := Value;
    if Value then
      MainFrm_ArchiveTreeView.BackgroundColor := clWindow
    else
      MainFrm_ArchiveTreeView.BackgroundColor := clBtnFace;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Delete Folders                                                        //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_DeleteFolders

  procedure TMainFrm.MainFrm_DeleteFolders;
  begin
    if BeeGui_SysUtils.DirectoryExists(AppTempFolder) then
    begin
      BeeGui_SysUtils.DeleteDirectory(AppTempFolder);
    end;
    // ---
    if BeeGui_SysUtils.DirectoryExists(AppCheckOutFolder) then
    begin
      if MessageDlg('Delete CheckOut folder?', mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        BeeGui_SysUtils.DeleteDirectory(AppCheckOutFolder);
      end;
    end;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Config Form Show                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //
  
  /// TMainFrm.MainFrm_ConfigFrm_Show

  procedure TMainFrm.MainFrm_ConfigFrm_Show(PageIndex: integer);
  begin
    ConfigFrm.ConfigFrm_Tree.Selected := ConfigFrm.ConfigFrm_Tree.Items[PageIndex];
    if ConfigFrm.ShowModal = mrOk then
    begin
      MainFrm_UpdateButtons;
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Add files routines                                                    //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.AddFiles

  procedure TMainFrm.MainFrm_AddFiles(FilesList: TStrings; EnableFilesList: boolean);
  var
    f: TAddFrm;
    I: integer;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;
    
    f := ConfigFrm.AddFrmCreate;
    if Assigned(FilesList) then
      for I := 0 to FilesList.Count -1 do
      begin
        if FileExists(FilesList.Strings[I]) then
          f.AddFrm_FilesTree.AddFileName(FilesList.Strings[I])
        else
          f.AddFrm_FilesTree.AddDirName(FilesList.Strings[I]);
      end;
    f.EnableFilesList(EnableFilesList);

    if f.ShowModal = mrOk then
    begin
      if f.AddFrm_FilesTree.Items.Count > 0 then
      begin
        AppParams.Clear;
        AppParams.Add('a');
        AppParams.Add('-l');
        AppParams.Add('-y' + AppTempFolder);

        if f.AddFrm_rOption.Checked then AppParams.Add('-r');
        if f.AddFrm_sOption.Checked then AppParams.Add('-s');
        if f.AddFrm_tOption.Checked then AppParams.Add('-t');
        if f.AddFrm_aOption.Checked then AppParams.Add('-a');
        if f.AddFrm_kOption.Checked then AppParams.Add('-k');

        AppParams.Add('-m'   + IntToStr(f.AddFrm_Method     .ItemIndex));
        AppParams.Add('-d'   + IntToStr(f.AddFrm_Dictionary .ItemIndex));
        AppParams.Add('-e'   +          f.AddFrm_eOption.Text          );
        AppParams.Add('-pri' + IntToStr(3 - f.ConfigFrm_Priority.ItemIndex));

        AppParams.Add('-cd' + MainFrm_ArchiveTreeView.ArchiveFolder);

        AppParams.Add(AppArcName);
        AppTerminateWith := twNone;

        for I := 0 to f.AddFrm_FilesTree.Items.Count -1 do
        begin
          if f.AddFrm_FilesTree.Items[I].ImageIndex = ATV_SELECTED then
            AppParams.Add(f.AddFrm_FilesTree.Items[I].Text)
          else
            AppParams.Add('-x' + f.AddFrm_FilesTree.Items[I].Text);
        end;

        if Length(f.AddFrm_Root.Text) > 0 then
        begin
          if SetCurrentDir(f.AddFrm_Root.Text) then
            AppCreate(MainFrm_MainMenu_Options_LogReport.Checked or f.AddFrm_tOption.Checked)
          else
            MessageDlg('Unable to set current dir', mtError, [mbOk], 0);
        end else
          AppCreate(MainFrm_MainMenu_Options_LogReport.Checked or f.AddFrm_tOption.Checked);
      end else
        MessageDlg('No files selected', mtWarning, [mbOk], 0);
    end;
    f.Free;
  end;

  procedure TMainFrm.MainFrm_AddFile(const FileName: string);
  var
    Files: TStringList;
  begin
    Files := TStringList.Create;
    Files.Add(FileName);
    MainFrm_AddFiles(Files, False);
    Files.Free;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  MainFrm_AtchiveTreeView process selected files                        //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_ProcessFiles


  function TMainFrm.MainFrm_ProcessFiles(var Params: TStringList): integer;
  var
    I: integer;
    Node: TTreeNode;
  begin
    Result := 0;
    for I := 0 to MainFrm_ArchiveTreeView.Items.Count -1 do
    begin
      if MainFrm_ArchiveTreeView.Items[I].MultiSelected then
      begin
        Node := MainFrm_ArchiveTreeView.Items[I];
        with TArchiveListItem(Node.Data) do
        begin
          if (FileAttr and faDirectory) = faDirectory then
          begin
            Params.Add(FilePath +  FileName + PathDelim + '*!');
            // MainFrm_ProcessFolder(Node, Params);
          end else
            Params.Add(FilePath + FileName);
        end;
        Inc (Result);
      end;
    end;
  end;
  
  procedure TMainFrm.MainFrm_ProcessFolder(FolderNode: TTreeNode; var Params: TStringList);
  var
    i: integer;
  begin
    for i := 0 to FolderNode.Count -1 do
    begin
      with TArchiveListItem(FolderNode.Items[i].Data) do
      begin
        if (FileAttr and faDirectory) = faDirectory then
        begin
          MainFrm_ProcessFolder(FolderNode.Items[i], Params);
        end else
        begin
          Params.Add(FilePath + FileName);
        end;
      end;
    end;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  MainMenu Click                                                        //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_MainMenu_File_NewClick

  procedure TMainFrm.MainFrm_MainMenu_File_NewClick(Sender: TObject);
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      MainFrm_SaveDialog.FileName := '';
      if MainFrm_SaveDialog.Execute then
      begin
        MainFrm_MainMenu_File_Close.Click;
        AppArcName := MainFrm_SaveDialog.FileName;
        case MainFrm_SaveDialog.FilterIndex of
          1: AppArcName := ChangeFileExt(AppArcName, '.bee');
          2: AppArcName := ChangeFileExt(AppArcName, '.exe');
        end;

        Caption := 'BeeGui' + ' - ' + ExtractFileName(AppArcName);

        AppParams.Clear;
        AppParams.Add('l');
        AppParams.Add('-y' + AppTempFolder);
        AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
        AppParams.Add(AppArcName);
        AppParams.Add('*!');
        AppTerminateWith := twNewArc;
        AppCreate(False);
      end;
    end else
      MessageDlg('An process active', mtInformation, [mbOk], 0);
  end;

  /// TMainFrm.MainFrm_MainMenu_File_OpenClick

  procedure TMainFrm.MainFrm_MainMenu_File_OpenClick(Sender: TObject);
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      MainFrm_OpenDialog.FileName := '';
      MainFrm_OpenDialog.Filter :=
        'bee file (*.bee)|*.bee|' +
        'exe file (*.bee)|*.exe|' +
        'all files  (*.*)|*.*|';

      if MainFrm_OpenDialog.Execute then
      begin
        MainFrm_MainMenu_File_Close.Click;
        AppArcName := MainFrm_OpenDialog.FileName;

        Caption := 'BeeGui' + ' - ' + ExtractFileName(AppArcName);

        AppParams.Clear;
        AppParams.Add('l');
        AppParams.Add('-y' + AppTempFolder);
        AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
        AppParams.Add(AppArcName);
        AppParams.Add('*!');

        AppTerminateWith := twNone;
        AppCreate(False);
      end;
    end else
      MessageDlg('An process active', mtInformation, [mbOk], 0);
  end;

  /// TMainFrm.MainFrm_MainMenu_File_CloseClick

  procedure TMainFrm.MainFrm_MainMenu_File_CloseClick(Sender: TObject);
  begin
    MainFrm_UpdateCursor(crDefault);
    MainFrm_UpdateButtons(False);
    MainFrm_DeleteFolders;

    MainFrm_StatusBar.Panels[0].Text := '';
    MainFrm_ArchiveTreeView.Clear;
    AppParams.Clear;
    AppArcName := '';

    Caption := 'BeeGui';
  end;

  /// TMainFrm.MainFrm_MainMenu_File_MoveClick

  procedure TMainFrm.MainFrm_MainMenu_File_MoveClick(Sender: TObject);
  var
    NewName: string;
  begin
    NewName := '';
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;
    // ---
    if SelectDirectory('Move archive to:', '', NewName) then
    begin
      NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(AppArcName);
      if RenameFile(AppArcName, NewName) then
        AppArcName := NewName
      else
        MessageDlg('Error moving archive', mtError, [mbOk], 0);
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_InfoClick

  procedure TMainFrm.MainFrm_MainMenu_File_InfoClick(Sender: TObject);
  var
    f: TInfoFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;
    // ---
    f := TInfoFrm.Create(Self);
    if f.UpdateAInfo(AppArcName, MainFrm.MainFrm_ArchiveTreeView.ArchiveFileDetails) then
    begin
      f.ShowModal;
    end else
      MessageDlg('Can''t load archive infomations.', mtInformation, [mbOk], 0);
    f.Free;
  end;
  
  /// TMainFrm.MainFrm_MainMenu_File_CopyClick

  procedure TMainFrm.MainFrm_MainMenu_File_CopyClick (Sender: TObject);
  var
    NewName: string;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;
    // ---
    NewName := '';
    if SelectDirectory('Copy archive to:', '', NewName) then
    begin
      NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(AppArcName);
      if CopyFile(AppArcName, NewName) then
        AppArcName := NewName
      else
        MessageDlg('Error copying archive', mtError, [mbOk], 0);
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_RenameClick

  procedure TMainFrm.MainFrm_MainMenu_File_RenameClick(Sender: TObject);
  var
    f: TRenameFrm;
    NewName: string;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;

    f := TRenameFrm.Create(Self);
    f.Caption := 'Rename archive';
    f.RenameFrm_To.Text := ExtractFileName(AppArcName);

    if f.ShowModal = mrOk then
    begin
      if AnsiCompareFileName(f.RenameFrm_From.Caption, f.RenameFrm_To.Text) <> 0 then
      begin
        NewName := ExtractFilePath(AppArcName) + f.RenameFrm_To.Text;
        if RenameFile (AppArcName, NewName) then
        begin
          MainFrm_MainMenu_File_Close.Click;
          AppArcName := NewName;

          Caption := 'BeeGui' + ' - ' + ExtractFileName(AppArcName);

          AppParams.Clear;
          AppParams.Add('l');
          AppParams.Add('-y' + AppTempFolder);
          AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
          AppParams.Add(AppArcName);
          AppParams.Add('*!');

          AppTerminateWith := twNone;
          AppCreate(False);
        end else
          MessageDlg('Error on renaming archive', mtError, [mbOk], 0);
      end;
    end;
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_DeleteClick

  procedure TMainFrm.MainFrm_MainMenu_File_DeleteClick(Sender: TObject);
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;
    // ---
    if MessageDlg('Delete archive?', mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      if DeleteFile(AppArcName) then
        MainFrm_MainMenu_File_Close.Click
      else
        MessageDlg('Error on deleting archive.', mtError, [mbOk], 0);
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_ExitClick

  procedure TMainFrm.MainFrm_MainMenu_File_ExitClick(Sender: TObject);
  begin
    Close;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_AddClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_AddClick(Sender: TObject);
  begin
    MainFrm_AddFiles(nil, True);
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_DeleteClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_DeleteClick(Sender: TObject);
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;
    // ---
    if MessageDlg('Delete selected files?' , mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      AppParams.Clear;
      AppParams.Add('d');
      AppParams.Add('-l');
      AppParams.Add('-y' + AppTempFolder);
      AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
      AppParams.Add(AppArcName);
      MainFrm_ProcessFiles(AppParams);

      AppTerminateWith := twNone;
      AppCreate(MainFrm_MainMenu_Options_LogReport.Checked);
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_ExtractClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_ExtractClick(Sender: TObject);
  var
    f: TExtractFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;

    f := ConfigFrm.ExtractFrmCreate;
    if f.ShowModal = mrOK then
    begin
      AppParams.Clear;

      case f.ExtractFrm_xOption.Checked of
        true : AppParams.Add('x');
        false: AppParams.Add('e');
      end;

      case f.ExtractFrm_ExtrOverWrite.ItemIndex of
        0: AppParams.Add('-oS');
        1: AppParams.Add('-oA');
      end;

      AppParams.Add ('-cd' + MainFrm_ArchiveTreeView.ArchiveFolder);

      AppParams.Add('-y' + AppTempFolder);
      AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
      AppParams.Add(AppArcName);

      if MainFrm_ProcessFiles(AppParams) = 0 then AppParams.Add('*!');

      AppTerminateWith := twNone;

      ForceDirectories(f.ExtractFrm_FolderEdit.Text);
      if SetCurrentDir(f.ExtractFrm_FolderEdit.Text) then
        AppCreate(MainFrm_MainMenu_Options_LogReport.Checked)
      else
        MessageDlg('Error on setting directory', mtError, [mbOk], 0);
    end;
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_ExtractAllClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_ExtractAllClick (Sender: TObject);
  var
    f: TSeDeseFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      f := TSeDeseFrm.Create (Self);
      f.DeSelectAll (MainFrm_ArchiveTreeView);
      f.Free;

      MainFrm_MainMenu_Actions_Extract.Click;
    end;
  end;

  procedure TMainFrm.MainFrm_MainMenu_Actions_TestClick(Sender: TObject);
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      AppParams.Clear;
      AppParams.Add('t');
      AppParams.Add('-y' + AppTempFolder);
      AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
      AppParams.Add(AppArcName);

      if MainFrm_ProcessFiles(AppParams) = 0 then AppParams.Add('*!');

      AppTerminateWith := twNone;
      AppCreate(True);
    end;
  end;
  
  /// TMainFrm.MainFrm_MainMenu_Actions_RenameClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_RenameClick(Sender: TObject);
  var
    I: integer;
    f: TRenameFrm;
    SelCount: integer;
    SelIndex: integer;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;

    SelCount := 0;
    for I := 0 to MainFrm_ArchiveTreeView.Items.Count -1 do
    begin
      if MainFrm_ArchiveTreeView.Items[I].MultiSelected then
      begin
        Inc(SelCount);
        SelIndex := I;
      end;
    end;

    if SelCount > 0 then
    begin
      AppParams.Clear;
      AppParams.Add('r');
      AppParams.Add('-l');
      AppParams.Add('-y' + AppTempFolder);
      AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
      AppParams.Add(AppArcName);
      MainFrm_ProcessFiles(AppParams);

      AppRenameFolder := False;
      if (TArchiveListItem(MainFrm_ArchiveTreeView.Items[SelIndex].Data).FileAttr and faDirectory) = faDirectory then
      begin
        if SelCount = 1 then
        begin
          f := TRenameFrm.Create(Self);
          f.Caption := 'Rename folder';
          f.RenameFrm_To.Text := MainFrm_ArchiveTreeView.Items[SelIndex].Text;
          if f.ShowModal = mrOk then
          begin
            AppRenameFolder     := True;
            AppRenameFolderFrom := IncludeTrailingBackslash(TArchiveListItem(MainFrm_ArchiveTreeView.Items[SelIndex].Data).FilePath + f.RenameFrm_From.Caption);
            AppRenameFolderTo   := IncludeTrailingBackslash(TArchiveListItem(MainFrm_ArchiveTreeView.Items[SelIndex].Data).FilePath + f.RenameFrm_To.Text);
          end;
          f.Free;

          if AppRenameFolder then
          begin
            AppTerminateWith := twNone;
            AppCreate(MainFrm_MainMenu_Options_LogReport.Checked);
          end;
        end;

      end else
      begin
        AppTerminateWith := twNone;
        AppCreate(MainFrm_MainMenu_Options_LogReport.Checked);
      end;
    end;
    
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_ViewClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_ViewClick(Sender: TObject);
  var
    I: integer;
    SelCount: integer;
    SelNode: TTreeNode;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;

    SelCount := 0;
    for I := MainFrm_ArchiveTreeView.Items.Count -1 downto 0  do
    begin
      if MainFrm_ArchiveTreeView.Items[I].MultiSelected then
      begin
        Inc(SelCount);
        SelNode :=  MainFrm_ArchiveTreeView.Items[I];
      end;
    end;

    if SelCount > 0 then
    begin
      if (TArchiveListItem(SelNode.Data).FileAttr and faDirectory) = faDirectory then
      begin
        SelNode.Expand(False);
      end else
      begin
        AppParams.Clear;
        AppParams.Add('x');
        AppParams.Add('-oA');
        AppParams.Add('-y' + AppTempFolder);
        AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
        AppParams.Add(AppArcName);
        MainFrm_ProcessFiles(AppParams);

        AppTerminateWith := twViewExt;
        ForceDirectories(AppTempFolder);
        if SetCurrentDir(AppTempFolder) then
          AppCreate(MainFrm_MainMenu_Options_LogReport.Checked)
        else
          MessageDlg('Error on setting current directory', mtError, [mbOk], 0);
      end;
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_CheckOutClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_CheckOutClick(Sender: TObject);
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      AppParams.Clear;
      AppParams.Add('x');
      AppParams.Add('-oA');
      AppParams.Add('-y' + AppTempFolder);
      AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
      AppParams.Add(AppArcName);

      if MainFrm_ProcessFiles(AppParams) = 0 then AppParams.Add('*!');

      AppTerminateWith := twCheckOut;
      ForceDirectories(AppCheckOutFolder);
      if SetCurrentDir(AppCheckOutFolder) then
        AppCreate(MainFrm_MainMenu_Options_LogReport.Checked)
      else
        MessageDlg('Error on setting checkout directory', mtError, [mbOk], 0);
    end;
  end;
 
  /// TMainFrm.MainFrm_MainMenu_Actions_TestAllClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_TestAllClick(Sender: TObject);
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      AppParams.Clear;
      AppParams.Add('t');
      AppParams.Add('-y' + AppTempFolder);
      AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
      AppParams.Add(AppArcName);
      AppParams.Add('*!');

      AppTerminateWith := twNone;
      AppCreate(True);
    end;
  end;
  
  /// TMainFrm.MainFrm_MainMenu_Actions_SelectAllClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_SelectAllClick(Sender: TObject);
  var
    f: TSeDeseFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      MainFrm_ArchiveTreeView.SetFocus;
      f := TSeDeseFrm.Create(Self);
      try
        f.SelectAll(MainFrm_ArchiveTreeView);
      finally
        f.Free;
      end;
      MainFrm_ArchiveTreeViewSelectionChanged(Self);
    end;
  end;
  
  /// TMainFrm.MainFrm_MainMenu_Actions_SelectMaskClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_SelectMaskClick(Sender: TObject);
  var
    f: TSeDeseFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      MainFrm_ArchiveTreeView.SetFocus;
      f := TSeDeseFrm.Create(Self);
      try
        f.Caption := 'Select mask';
        if f.ShowModal = mrOk then
        begin
          f.SelectItems(MainFrm_ArchiveTreeView);
        end;
      finally
        f.Free;
      end;
      MainFrm_ArchiveTreeViewSelectionChanged(Self);
    end;
  end;
  
  /// TMainFrm.MainFrm_MainMenu_Actions_DeselectAllClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_DeselectAllClick(Sender: TObject);
  var
    f: TSeDeseFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      MainFrm_ArchiveTreeView.SetFocus;
      f := TSeDeseFrm.Create(Self);
      try
        f.DeSelectAll(MainFrm_ArchiveTreeView);
      finally
        f.Free;
      end;
      MainFrm_ArchiveTreeViewSelectionChanged(Self);
    end;
  end;
  
  /// TMainFrm.MainFrm_MainMenu_Actions_DeselectMasksClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_DeselectMasksClick (Sender: TObject);
  var
    f: TSeDeseFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      MainFrm_ArchiveTreeView.SetFocus;
      f := TSeDeseFrm.Create(Self);
      try
        f.Caption := 'Deselect mask';
        if f.ShowModal = mrOk then
        begin
          f.DeselectItems(MainFrm_ArchiveTreeView);
        end;
      finally
        f.Free;
      end;
      MainFrm_ArchiveTreeViewSelectionChanged(Self);
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_InvertClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_InvertClick(Sender: TObject);
  var
    f: TSeDeseFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      MainFrm_ArchiveTreeView.SetFocus;
      f := TSeDeseFrm.Create(Self);
      try
        f.InvertSelection(MainFrm_ArchiveTreeView);
      finally
        f.Free;
      end;
      MainFrm_ArchiveTreeViewSelectionChanged(Self);
    end;
  end;
  
  /// TMainFrm.MainFrm_MainMenu_Options_ConfigClick

  procedure TMainFrm.MainFrm_MainMenu_Options_ConfigClick(Sender: TObject);
  begin
    MainFrm_ConfigFrm_Show(0);
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_PasswordClick

  procedure TMainFrm.MainFrm_MainMenu_Options_PasswordClick(Sender: TObject);
  var
    f: TPasswordFrm;
  begin
    f := ConfigFrm.PasswordFrmCreate;
    f.SetKey(AppKey);
    if f.ShowModal = mrOK then
    begin
      AppKey := f.PasswordFrm_Key.Text;
    end;
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_SaveOnExitClick

  procedure TMainFrm.MainFrm_MainMenu_Options_SaveOnExitClick(Sender: TObject);
  begin
    MainFrm_MainMenu_Options_SaveOnExit.Checked := not MainFrm_MainMenu_Options_SaveOnExit.Checked;
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_SaveNowClick

  procedure TMainFrm.MainFrm_MainMenu_Options_SaveNowClick(Sender: TObject);
  begin
    MainFrm_Storage.Save;
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_DefaultClick

  procedure TMainFrm.MainFrm_MainMenu_Options_DefaultClick(Sender: TObject);
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      if MessageDlg('Default setting?', mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        MainFrm_MainMenu_File_Exit.Click;
        ClearDirectory(GetApplicationConfigDir);
        // ---
        ShellExec(ParamStr(0), '');
      end;
    end else
      MessageDlg('An active process. Please wait.', mtInformation, [mbOk], 0);
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_LogReportClick

  procedure TMainFrm.MainFrm_MainMenu_Options_LogReportClick (Sender: TObject);
  begin
    MainFrm_MainMenu_Options_LogReport.Checked := not MainFrm_MainMenu_Options_LogReport.Checked;
  end;

  /// TMainFrm.MainFrm_MainMenu_Help_AboutClick

  procedure TMainFrm.MainFrm_MainMenu_Help_AboutClick(Sender: TObject);
  var
    f: TForm;
  begin
    f := TAboutFrm.Create(Self);
    f.ShowModal;
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Help_F1Click
  
  procedure TMainFrm.MainFrm_MainMenu_Help_F1Click(Sender: TObject);
  var
    help_filename: string;
    help_filepath: string;
  begin
    help_filepath := ExtractFilePath(ParamStr(0));
    help_filename := help_filepath + 'docs' + PathDelim + 'help.htm';
    // --
    ShellExec(help_filename, help_filepath);
  end;

  /// TMainFrm.MainFrm_MainMenu_Help_InternetClick

  procedure TMainFrm.MainFrm_MainMenu_Help_InternetClick(Sender: TObject);
  var
    f: TAboutFrm;
  begin
    f := TaboutFrm.Create(Self);
    f.AboutFrm_LinkClick(Self);
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Help_LicenseClick

  procedure TMainFrm.MainFrm_MainMenu_Help_LicenseClick(Sender: TObject);
  var
    f: TAboutFrm;
  begin
    f := TaboutFrm.Create(Self);
    f.BtnLicense.Click;
    f.Free;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  PopupnMenu Buttons Click                                              //
  //                                                                        //
  // ---------------------------------------------------------------------- //
  
  procedure TMainFrm.MainFrm_PopupMenu_Btns_NewClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_New.Checked := not MainFrm_PopupMenu_Btns_New.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_OpenClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_Open.Checked := not MainFrm_PopupMenu_Btns_Open.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_N1Click(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_N1.Checked := not MainFrm_PopupMenu_Btns_N1.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_AddClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_Add.Checked := not MainFrm_PopupMenu_Btns_Add.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_ExtractClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_Extract.Checked := not MainFrm_PopupMenu_Btns_Extract.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_ViewClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_View.Checked := not MainFrm_PopupMenu_Btns_View.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_DeleteClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_Delete.Checked := not MainFrm_PopupMenu_Btns_Delete.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_N2Click(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_N2.Checked := not MainFrm_PopupMenu_Btns_N2.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_TestClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_Test.Checked := not MainFrm_PopupMenu_Btns_Test.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_CheckOutClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_CheckOut.Checked := not MainFrm_PopupMenu_Btns_CheckOut.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_N3Click(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_N3.Checked := not MainFrm_PopupMenu_Btns_N3.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_ConfigClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_Config.Checked := not MainFrm_PopupMenu_Btns_Config.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_HelpClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_Help.Checked := not MainFrm_PopupMenu_Btns_Help.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_Btns_ExitClick(Sender: TObject);
  begin
    MainFrm_PopupMenu_Btns_Exit.Checked := not MainFrm_PopupMenu_Btns_Exit.Checked;
    MainFrm_UpdateButtons;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  PopupnMenu Click                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //
  
  /// TMainFrm.MainFrm_PopupMenu_Open_IntViewerClick

  procedure TMainFrm.MainFrm_PopupMenu_Open_IntViewerClick (Sender: TObject);
  var
    I: integer;
    SelCount: integer;
    SelIndex: integer;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;

    SelCount := 0;
    for I := MainFrm_ArchiveTreeView.Items.Count -1 downto 0  do
    begin
      if MainFrm_ArchiveTreeView.Items[I].MultiSelected then
      begin
        Inc(SelCount);
        SelIndex := I;
      end;
    end;

    if SelCount = 1 then
    begin
      if (TArchiveListItem(MainFrm_ArchiveTreeView.Items[SelIndex].Data).FileAttr and faDirectory) = faDirectory  then
      begin
        MainFrm_ArchiveTreeView.Items[SelIndex].Expand(False);
      end else
      begin
        AppParams.Clear;
        AppParams.Add('x');
        AppParams.Add('-oA');
        AppParams.Add('-y' + AppTempFolder);
        AppParams.Add('-pri' + IntToStr(ConfigFrm.ConfigFrm_Priority.ItemIndex));
        AppParams.Add(AppArcName);
        MainFrm_ProcessFiles(AppParams);

        AppTerminateWith := twViewInt;

        ForceDirectories(AppTempFolder);
        if SetCurrentDir(AppTempFolder) then
          AppCreate(MainFrm_MainMenu_Options_LogReport.Checked)
        else
          MessageDlg('Error on setting temp directory', mtError, [mbOk], 0);
      end;
    end;
  end;
  
  procedure TMainFrm.MainFrm_PopupMenu_CollapseAllClick(Sender: TObject);
  begin
    MainFrm_ArchiveTreeView.CollapseAll;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_ExpandAllClick(Sender: TObject);
  begin
    MainFrm_ArchiveTreeView.ExpandAll;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_ExpandOneClick(Sender: TObject);
  begin
    MainFrm_ArchiveTreeView.ExpandOne;
  end;
  
  procedure TMainFrm.MainFrm_PopupMenu_CollapseOneClick(Sender: TObject);
  begin
    MainFrm_ArchiveTreeView.CollapseOne;
  end;

  procedure TMainFrm.MainFrm_PopupMenu_PropertyClick(Sender: TObject);
  var
    I: integer;
    f: TInfoFrm;
    SelCount: integer;
    SelData: pointer;
  begin
    if MainFrm_ArchiveTreeView.Cursor = crHourGlass then Exit;
    // ---
    SelCount := 0;
    for I := MainFrm_ArchiveTreeView.Items.Count -1 downto 0  do
    begin
      if MainFrm_ArchiveTreeView.Items[I].MultiSelected then
      begin
        Inc(SelCount);
        SelData := MainFrm_ArchiveTreeView.Items[I].Data;
      end;
    end;
    // ---
    if (SelCount = 1) and ((TArchiveListItem(SelData).FileAttr and faDirectory) = 0) then
    begin
      f := TInfoFrm.Create(Self);
      try
        if f.UpdateFInfo(SelData) then
          f.ShowModal
        else
          MessageDlg('Can''t load file infomations.', mtInformation, [mbOk], 0);
      finally
        f.Free;
      end;
    end;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  PopupMenu Events                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_PopupMenuPopup
  
  procedure TMainFrm.MainFrm_PopupMenuPopup(Sender: TObject);
  var
    I: integer;
    SelCount: integer;
    SelData: pointer;
  begin
    SelCount := 0;
    for I := 0 to MainFrm_ArchiveTreeView.Items.Count -1 do
    begin
      if MainFrm_ArchiveTreeView.Items[I].MultiSelected then
      begin
        Inc(SelCount);
        SelData := MainFrm_ArchiveTreeView.Items[I].Data;
      end;
    end;
    if SelCount = 1 then
    begin
      for I := 0 to MainFrm_PopupMenu.Items.Count -1 do
      begin
        MainFrm_PopupMenu.Items[I].Visible := True;
      end;
      if (TArchiveListItem(SelData).FileAttr and fadirectory) = faDirectory then
      begin
        for I := MainFrm_PopupMenu.Items.Count -2 to MainFrm_PopupMenu.Items.Count -1 do
        begin
          MainFrm_PopupMenu.Items[I].Visible := False;
        end;
      end;
    end else
    begin
      if SelCount > 0 then
      begin
        for I := 0 to MainFrm_PopupMenu.Items.Count -1 do
        begin
          MainFrm_PopupMenu.Items[I].Visible := True;
        end;
        for I := MainFrm_PopupMenu.Items.Count -2 to MainFrm_PopupMenu.Items.Count -1 do
        begin
          MainFrm_PopupMenu.Items[I].Visible := False;
        end;
      end else
      begin
        if SelCount = 0 then
        begin
          for I := 0 to MainFrm_PopupMenu.Items.Count -1 do
          begin
            MainFrm_PopupMenu.Items[I].Visible := False;
          end;
          MainFrm_PopupMenu_ExpandCollapse.Visible := True;
          MainFrm_PopupMenu_SelectDeselect.Visible := True;
        end;
      end;
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  ArchiveTreeView events                                                //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MainFrm_ArchiveTreeViewSelectionChanged(Sender: TObject);
  var
    I: integer;
    SelCount: integer;
    SelIndex: integer;
    SelData: TArchiveListItem;
  begin
    SelCount := 0;
    for I := 0 to MainFrm_ArchiveTreeView.Items.Count -1 do
    begin
      if MainFrm_ArchiveTreeView.Items[I].MultiSelected then
      begin
        Inc(SelCount);
        SelIndex := I;
      end;
    end;
    // ---
    if SelCount = 0 then
    begin
      MainFrm_StatusBar.Panels[0].Text := Format('Items: %u', [MainFrm_ArchiveTreeView.Items.Count])
    end else
    begin
      if SelCount = 1 then
      begin
        SelData := TArchiveListItem(MainFrm_ArchiveTreeView.Items[SelIndex].Data);
        if (SelData.FileAttr and faDirectory) = 0 then
        begin
          MainFrm_StatusBar.Panels[0].Text := SelData.FileName +  ',' +
            '  Size ' + SizeToStr(SelData.FileSize) +
            '  Packed ' + SizeToStr(SelData.FilePacked);
        end else
        begin
          MainFrm_StatusBar.Panels[0].Text := Format('Items selected: %u', [SelCount]);
        end;
      end else
      begin
        MainFrm_StatusBar.Panels[0].Text := Format('Items selected: %u', [SelCount]);
      end;
    end;
  end;

  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Thread events                                                         //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.OnAppOverWrite

  procedure TMainFrm.OnAppOverWrite;
  var
    f: TOverWriteFrm;
  begin
    if App.Suspended = False then
    begin;
      App.Suspended := True;
      f := TOverWriteFrm.Create (Self);
      try
        f.OverWriteFrm_The.Caption     := f.OverWriteFrm_The    .Caption + ' "' + ExtractFileName(AppInterface.cFileName) + '".';
        f.OverWriteFrm_NewSize.Caption := f.OverWriteFrm_NewSize.Caption + '  ' + IntToStr(AppInterface.cFileSize);
        f.OverWriteFrm_NewDate.Caption := f.OverWriteFrm_NewDate.Caption + '  ' + DateTimeToStr(FileDateToDateTime(AppInterface.cFileTime));
        f.OverWriteFrm_OldSize.Caption := f.OverWriteFrm_OldSize.Caption + '  ' + SizeToStr(SizeOfFile(AppInterface.cFileName));
        f.OverWriteFrm_OldDate.Caption := f.OverWriteFrm_OldDate.Caption + '  ' + DateTimeToStr(FileDateToDateTime(FileAge(AppInterface.cFileName)));
        with TIconList(MainFrm_LargeIconList) do
        begin
          GetBitmap(GetIconIndex(ExtractFileExt(AppInterface.cFileName)), f.OverWriteFrm_OldImage.Picture.Bitmap);
          GetBitmap(GetIconIndex(ExtractFileExt(AppInterface.cFileName)), f.OverWriteFrm_NewImage.Picture.Bitmap);
        end;
        case f.ShowModal of
          mrAbort   : AppInterface.cMsg := 'Q';
          mrNoToAll : AppInterface.cMsg := 'S';
          mrYesToAll: AppInterface.cMsg := 'A';
          mrNo      : AppInterface.cMsg := 'N';
          mrYes     : AppInterface.cMsg := 'Y';
        end;
      finally
        f.Free;
      end;
      App.Suspended := False;
    end;
  end;
  
  /// TMainFrm.OnAppRename

  procedure TMainFrm.OnAppRename;
  var
    f: TRenameFrm;
  begin
    if App.Suspended = False then
    begin;
      App.Suspended := True;
      if AppRenameFolder = False then
      begin
        f := TRenameFrm.Create(Self);
        try
          f.Caption := 'Rename file';
          f.RenameFrm_To.Text := AppInterface.cFileName;
          if f.ShowModal = mrOk then
            AppInterface.cMsg := f.RenameFrm_To.Text
          else
            AppInterface.cMsg := '';
        finally
          f.Free;
        end;
      end else
      begin
        AppInterface.cMsg := AppInterface.cFileName;
        Delete(AppInterface.cMsg, 1, Length(AppRenameFolderFrom));
        AppInterface.cMsg := AppRenameFolderTo + AppInterface.cMsg;
      end;
      App.Suspended := False;
    end;
  end;
  
  /// TMainFrm.OnAppWarning

  procedure TMainFrm.OnAppWarning;
  begin
    if Assigned(AppLogFile) = False then
    begin
      AppLogFile := TStringList.Create;
    end;
    AppLogFile.Add(AppInterface.cMsg);
  end;

  /// TMainFrm.OnAppError

  procedure TMainFrm.OnAppError;
  begin
    AppTerminatedWithError := True;
    // ---
    if Assigned(AppLogFile) = False then
    begin
      AppLogFile := TStringList.Create;
    end;
    AppLogFile.Add(AppInterface.cMsg);
  end;
  
  procedure TMainFrm.OnAppFatalError;
  begin
  
  
  end;
  
  procedure TMainFrm.OnAppRequest;
  begin
  
  
  end;
  
  /// TMainFrm.OnAppDisplay

  procedure TMainFrm.OnAppDisplay;
  begin
    if Assigned(AppLogFile) then
    begin
      AppLogFile.Add(AppInterface.cMsg);
    end;
    MainFrm_StatusBar.Panels[0].Text := AppInterface.cMsg;
  end;

  /// TMainFrm.OnAppTick

  procedure TMainFrm.OnAppTick;
  begin
    MainFrm_ProgressBar.Position := AppInterface.cPercentage;
  end;

  /// TMainFrm.OnAppClear

  procedure TMainFrm.OnAppClear;
  begin
    // nothing to do!
  end;

  /// TMainFrm.OnAppList

  procedure TMainFrm.OnAppList;
  begin
    AppInterface.cList := MainFrm_ArchiveTreeView.ArchiveFiles;
  end;

  /// TMainFrm.OnAppKey

  procedure TMainFrm.OnAppKey;
  begin
    if App.Suspended = False then
    begin;
      App.Suspended := True;
      if Length(AppKey) = 0 then
      begin
        MainFrm_MainMenu_Options_PasswordClick(Self);
      end;
      AppInterface.cMsg := AppKey;
      App.Suspended := False;
    end;
  end;

  /// TMainFrm.AppCreate

  procedure TMainFrm.AppCreate(CreateLogFile: boolean);
  //type
    //TAppCreateProc = procedure(aAppInterface: TAppInterface; aAppParams: TStringList; aAppTerminate: TNotifyEvent);
  //var
    //LibHandle: TLibHandle;
    //AppCreateProc: TAppCreateProc;
  begin
    MainFrm_UpdateButtons(False);
    MainFrm_UpdateCursor(crHourGlass);
    // ---
    if CreateLogFile then
    begin
      AppLogFile := TStringList.Create;
    end;
    // ---

    //LibHandle := LoadLibrary('beecore.dll');
    //AppCreateProc  := GetProcedureAddress(LibHandle, 'AppCreate');
    //if Assigned(AppCreateProc) then
    //begin
    //  AppCreateProc(AppInterface, AppParams, MainFrm.OnAppTerminate);
    //end;
    
    App := TBeeApp.Create(AppInterface, AppParams, MainFrm.OnAppTerminate);
    AppTerminatedWithError := False;
    AppTerminated := False;
    App.Suspended := False;
  end;

  /// TMainFrm.OnAppTerminate

  procedure TMainFrm.OnAppTerminate(Sender: TObject);
  begin
    if AppTerminated = False then
    begin
      AppTerminated := True;
      // ---
      MainFrm_ProgressBar.Position := 0;
      MainFrm_ArchiveTreeView.Initialize;
      if (AppTerminateWith <> twViewExt) then
      begin
        MainFrm_UpdateButtons(True);
        MainFrm_UpdateCursor(crDefault);
      end;
      AppTerminateWithLog;
      case AppTerminateWith of
        twClose: Close;
        twNewArc:   AppTerminateWithNew;
        twCheckOut: AppTerminateWithCheckOut;
        twViewExt:  AppTerminateWithView(AnsiIncludeTrailingBackSlash(AppTempFolder) +
                      AppParams.Strings[AppParams.Count -1], False);
        twViewInt:  AppTerminateWithView(AnsiIncludeTrailingBackSlash(AppTempFolder) +
                      AppParams.Strings[AppParams.Count -1], True);
      end;
    end;
  end;

  /// TMainFrm.OnAppViewerTerminate

  procedure TMainFrm.OnAppViewerTerminate(Sender: TObject);
  begin
    if AppViewerTerminated = False then
    begin
      AppViewerTerminated := True;
      // ---
      MainFrm_UpdateButtons(True);
      MainFrm_UpdateCursor(crDefault);
      // ---
      if FileAge(AppViewer.FileName) > AppViewer.FileTime then
      begin
        if MessageDlg('Update file modified?', mtInformation, [mbYes, mbNo], 0) = mrYes then
        begin
          AppParams.Clear;
          AppParams.Add('a');
          AppParams.Add('-l');
          AppParams.Add('-y' + AppTempFolder);
          AppParams.Add('-f');
          AppParams.Add(AppArcName);
          AppParams.Add(AppViewer.FileName);
          // ---
          AppTerminateWith := twNone;
          AppCreate(MainFrm_MainMenu_Options_LogReport.Checked);
        end;
      end;
    end;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  OnTerminate Thread Mode                                               //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.AppTerminateWithNew

  procedure TMainFrm.AppTerminateWithNew;
  begin
    MainFrm_MainMenu_Actions_Add.Click;
  end;

  /// TMainFrm.App_TerminateWithView

  procedure TMainFrm.AppTerminateWithView(FileName: string; UseIntViewer: boolean);
  var
    f: TIntViewerFrm;
  begin
    if FileExists(FileName) = False then
    begin
      MainFrm_UpdateButtons(True);
      MainFrm_UpdateCursor(crDefault);
    end else
    begin
      if UseIntViewer = True then
      begin
        f := TIntViewerFrm.Create(Self);
        f.IntViewerFrm_ViewMemo.Lines.LoadFromFile(FileName);
        f.ShowModal;
        f.Free;
      end else
      begin
        AppViewer := TAppViewer.Create(FileName);
        AppViewer.OnTerminate := MainFrm.OnAppViewerTerminate;
        AppViewerTerminated := False;
        AppViewer.Resume;
      end;
    end;
  end;

  /// TMainFrm.AppTerminateWithLog

  procedure TMainFrm.AppTerminateWithLog;
  var
    f: TIntViewerFrm;
  begin
    if Assigned(AppLogFile) then
    begin
      if AppLogFile.Count > 0 then
      begin
        f := TIntViewerFrm.Create(Self);
        f.IntViewerFrm_ViewMemo.Lines.Text := AppLogFile.Text;
        try
          f.ShowModal;
        except
          MessageDlg('Error loading application results', mtError, [mbYes], 0);
        end;
        f.Free;
      end;
      FreeAndNil(AppLogFile);
    end;
  end;

  ///  TMainFrm.AppTerminateWithCheckOut

  procedure TMainFrm.AppTerminateWithCheckOut;
  begin
    ShellExec(GetCurrentDir, '');
  end;

initialization

  {$I beegui_mainfrm.lrs}
  
end.


