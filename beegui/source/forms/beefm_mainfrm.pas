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

{   Contains:

    BeeGui Main form.

    Modifyed:
}

unit BeeFM_MainFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Menus,
  ToolWin,
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
  // ---
  BeeGui_IconList,
  BeeGui_ArchiveProcess,
  BeeGui_ArchiveListViewMgr,
  BeeGui_ArchiveFolderBox;

type
  TTerminateWith = (twNone, twNewArc, twCheckOut,
    twViewInt, twViewExt, twDrag, twClose);

type
  { TMainFrm }

  TMainFrm = class(TForm)
    ArchiveListView: TArcListView;
    Storage: TXMLPropStorage;
    FolderBox: TArchiveFolderBox;
    FolderBoxToolBar: TToolBar;
    StatusBar: TStatusBar;
    LargeIcons: TIconList;
    SmallIcons: TIconList;
    ArchiveProcess: TArchiveProcess;
    ArchiveProcessTimer: TIdleTimer;
    // ---
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    FontDialog: TFontDialog;
    // ---
    ButtonsToolBar: TToolBar;
    BevelFirst: TBevel;
    BevelSecond: TBevel;
    BevelThird: TBevel;
    BtnAdd: TSpeedButton;
    BtnCheckOut: TSpeedButton;
    BtnConfig: TSpeedButton;
    BtnDelete: TSpeedButton;
    BtnExit: TSpeedButton;
    BtnExtract: TSpeedButton;
    BtnHelp: TSpeedButton;
    BtnNew: TSpeedButton;
    BtnOpen: TSpeedButton;
    BtnTest: TSpeedButton;
    BtnView: TSpeedButton;
    BtnUp: TSpeedButton;
    // ---
    MainMenu: TMainMenu;
    MainMenu_File: TMenuItem;
    MainMenu_File_New: TMenuItem;
    MainMenu_File_Open: TMenuItem;
    MainMenu_File_Close: TMenuItem;
    MainMenu_File_N1: TMenuItem;
    MainMenu_File_Property: TMenuItem;
    MainMenu_File_N2: TMenuItem;
    MainMenu_File_Move: TMenuItem;
    MainMenu_File_Copy: TMenuItem;
    MainMenu_File_Rename: TMenuItem;
    MainMenu_File_Delete: TMenuItem;
    MainMenu_File_N3: TMenuItem;
    MainMenu_File_Exit: TMenuItem;
    // ---
    MainMenu_View: TMenuItem;
    MainMenu_View_Toolbars: TMenuItem;
    MainMenu_View_Toolbars_Buttons: TMenuItem;
    MainMenu_View_Toolbars_AddressBar: TMenuItem;
    MainMenu_View_StatusBar: TMenuItem;
    MainMenu_View_N1: TMenuItem;
    MainMenu_View_LargeIcons: TMenuItem;
    MainMenu_View_SmallIcons: TMenuItem;
    MainMenu_View_List: TMenuItem;
    MainMenu_View_Report: TMenuItem;
    MainMenu_View_N2: TMenuItem;
    MainMenu_View_RowSelect: TMenuItem;
    MainMenu_View_GridLines: TMenuItem;
    MainMenu_View_ListMode: TMenuItem;
    MainMenu_View_N3: TMenuItem;
    MainMenu_View_OrderBy: TMenuItem;
    MainMenu_View_OrderBy_Name: TMenuItem;
    MainMenu_View_OrderBy_Crc: TMenuItem;
    MainMenu_View_OrderBy_Path: TMenuItem;
    MainMenu_View_OrderBy_Position: TMenuItem;
    MainMenu_View_OrderBy_Size: TMenuItem;
    MainMenu_View_OrderBy_Packed: TMenuItem;
    MainMenu_View_OrderBy_Ratio: TMenuItem;
    MainMenu_View_OrderBy_Type: TMenuItem;
    MainMenu_View_OrderBy_Modified: TMenuItem;
    MainMenu_View_OrderBy_Attributes: TMenuItem;
    MainMenu_View_OrderBy_Method: TMenuItem;
    MainMenu_View_OrderBy_Password: TMenuItem;
    // ---
    MainMenu_View_Details: TMenuItem;
    MainMenu_View_Details_Name: TMenuItem;
    MainMenu_View_Details_CRC: TMenuItem;
    MainMenu_View_Details_Path: TMenuItem;
    MainMenu_View_Details_Position: TMenuItem;
    MainMenu_View_Details_Size: TMenuItem;
    MainMenu_View_Details_Packed: TMenuItem;
    MainMenu_View_Details_Ratio: TMenuItem;
    MainMenu_View_Details_Type: TMenuItem;
    MainMenu_View_Details_Modified: TMenuItem;
    MainMenu_View_Details_Attributes: TMenuItem;
    MainMenu_View_Details_Method: TMenuItem;
    MainMenu_View_Details_Password: TMenuItem;
    // ---
    MainMenu_Actions: TMenuItem;
    MainMenu_Actions_Add: TMenuItem;
    MainMenu_Actions_Delete: TMenuItem;
    MainMenu_Actions_Extract: TMenuItem;
    MainMenu_Actions_ExtractAll: TMenuItem;
    MainMenu_Actions_Test: TMenuItem;
    MainMenu_Actions_Rename: TMenuItem;
    MainMenu_Actions_View: TMenuItem;
    MainMenu_Actions_N1: TMenuItem;
    MainMenu_Actions_SelectAll: TMenuItem;
    MainMenu_Actions_SelectMask: TMenuItem;
    MainMenu_Actions_DeselectAll: TMenuItem;
    MainMenu_Actions_DeselectMask: TMenuItem;
    MainMenu_Actions_Invert: TMenuItem;
    MainMenu_Actions_N2: TMenuItem;
    MainMenu_Actions_CheckOut: TMenuItem;
    MainMenu_Actions_TestAll: TMenuItem;
    // ---
    MainMenu_Options: TMenuItem;
    MainMenu_Options_N1: TMenuItem;
    MainMenu_Options_N2: TMenuItem;
    MainMenu_Options_Configuration: TMenuItem;
    MainMenu_Options_Password: TMenuItem;
    MainMenu_Options_SaveOnExit: TMenuItem;
    MainMenu_Options_SaveNow: TMenuItem;
    MainMenu_Options_DefaultSetting: TMenuItem;
    MainMenu_Options_LogReport: TMenuItem;
    // ---
    MainMenu_Help: TMenuItem;
    MainMenu_Help_N1: TMenuItem;
    MainMenu_Help_N2: TMenuItem;
    MainMenu_Help_F1: TMenuItem;
    MainMenu_Help_Internet: TMenuItem;
    MainMenu_Help_License: TMenuItem;
    MainMenu_Help_About: TMenuItem;
    // ---
    PopupMenu: TPopupMenu;
    PopupMenu_N1: TMenuItem;
    PopupMenu_N2: TMenuItem;
    PopupMenu_N3: TMenuItem;
    PopupMenu_N4: TMenuItem;
    PopupMenu_Open: TMenuItem;
    PopupMenu_Open_IntViewer: TMenuItem;
    PopupMenu_Delete: TMenuItem;
    PopupMenu_Extract: TMenuItem;
    PopupMenu_ExtractAll: TMenuItem;
    PopupMenu_Test: TMenuItem;
    PopupMenu_Rename: TMenuItem;
    PopupMenu_SelectDeselect: TMenuItem;
    PopupMenu_SelectAll: TMenuItem;
    PopupMenu_SelectMask: TMenuItem;
    PopupMenu_DeselectAll: TMenuItem;
    PopupMenu_DeselectMask: TMenuItem;
    PopupMenu_Invert: TMenuItem;
    PopupMenu_Property: TMenuItem;
    // ---
    PopupMenu_Buttons: TPopupMenu;
    PopupMenu_Buttons_N1: TMenuItem;
    PopupMenu_Buttons_N2: TMenuItem;
    PopupMenu_Buttons_N3: TMenuItem;
    PopupMenu_Buttons_New: TMenuItem;
    PopupMenu_Buttons_Open: TMenuItem;
    PopupMenu_Buttons_Add: TMenuItem;
    PopupMenu_Buttons_Extract: TMenuItem;
    PopupMenu_Buttons_View: TMenuItem;
    PopupMenu_Buttons_Delete: TMenuItem;
    PopupMenu_Buttons_Test: TMenuItem;
    PopupMenu_Buttons_CheckOut: TMenuItem;
    PopupMenu_Buttons_Configuration: TMenuItem;
    PopupMenu_Buttons_Help: TMenuItem;
    PopupMenu_Buttons_Exit: TMenuItem;
    // ---
    procedure ArchiveListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ArchiveProcessTimerTimer(Sender: TObject);
    procedure FolderBoxSelect(Sender: TObject);

    procedure MainMenu_Actions_AddClick(Sender: TObject);

    // ---
    procedure MainMenu_File_NewClick (Sender: TObject);
    procedure MainMenu_File_OpenClick (Sender: TObject);
    procedure MainMenu_File_CloseClick (Sender: TObject);
    procedure MainMenu_File_PropertyClick (Sender: TObject);
    procedure MainMenu_File_MoveClick (Sender: TObject);
    procedure MainMenu_File_CopyClick (Sender: TObject);
    procedure MainMenu_File_RenameClick (Sender: TObject);
    procedure MainMenu_File_DeleteClick (Sender: TObject);
    procedure MainMenu_File_ExitClick (Sender: TObject);
    procedure MainMenu_View_GridLinesClick(Sender: TObject);
    // ---

    procedure MainMenu_View_ListModeClick(Sender: TObject);
    procedure MainMenu_View_RowSelectClick(Sender: TObject);
    procedure MainMenu_View_ViewStyle_Click(Sender: TObject);
    procedure MainMenu_View_OrderBy_Click(Sender: TObject);
    // ----
    procedure MainMenu_Actions_DeleteClick (Sender: TObject);
    procedure MainMenu_Actions_ExtractClick (Sender: TObject);
    procedure MainMenu_Actions_ExtractAllClick (Sender: TObject);
    procedure MainMenu_Actions_TestClick (Sender: TObject);
    procedure MainMenu_Actions_RenameClick (Sender: TObject);
    procedure MainMenu_Actions_ViewClick (Sender: TObject);
    procedure MainMenu_Actions_SelectAllClick (Sender: TObject);
    procedure MainMenu_Actions_SelectMaskClick (Sender: TObject);
    procedure MainMenu_Actions_DeselectMasksClick (Sender: TObject);
    procedure MainMenu_Actions_DeselectAllClick(Sender: TObject);
    procedure MainMenu_Actions_InvertClick (Sender: TObject);
    procedure MainMenu_Actions_TestAllClick (Sender: TObject);
    procedure MainMenu_Actions_CheckOutClick (Sender: TObject);
    // ---
    procedure MainMenu_Options_ConfigurationClick (Sender: TObject);
    procedure MainMenu_Options_SaveNowClick (Sender: TObject);
    procedure MainMenu_Options_DefaultSettingClick (Sender: TObject);
    procedure MainMenu_Options_PasswordClick (Sender: TObject);
    procedure MainMenu_Options_LogReportClick (Sender: TObject);
    procedure MainMenu_Options_SaveOnExitClick (Sender: TObject);
    // ---
    procedure MainMenu_Help_F1Click (Sender: TObject);
    procedure MainMenu_Help_InternetClick (Sender: TObject);
    procedure MainMenu_Help_LicenseClick (Sender: TObject);
    procedure MainMenu_Help_AboutClick (Sender: TObject);

    // ---
    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupMenu_Open_IntViewerClick(Sender: TObject);
    procedure PopupMenu_PropertyClick(Sender: TObject);
    // ---
    procedure PopupMenu_Buttons_AddClick(Sender: TObject);
    procedure PopupMenu_Buttons_CheckOutClick(Sender: TObject);
    procedure PopupMenu_Buttons_ConfigurationClick(Sender: TObject);
    procedure PopupMenu_Buttons_DeleteClick(Sender: TObject);
    procedure PopupMenu_Buttons_ExitClick(Sender: TObject);
    procedure PopupMenu_Buttons_ExtractClick(Sender: TObject);
    procedure PopupMenu_Buttons_HelpClick(Sender: TObject);
    procedure PopupMenu_Buttons_N1Click(Sender: TObject);
    procedure PopupMenu_Buttons_N2Click(Sender: TObject);
    procedure PopupMenu_Buttons_N3Click(Sender: TObject);
    procedure PopupMenu_Buttons_NewClick(Sender: TObject);
    procedure PopupMenu_Buttons_OpenClick(Sender: TObject);
    procedure PopupMenu_Buttons_TestClick(Sender: TObject);
    procedure PopupMenu_Buttons_ViewClick(Sender: TObject);
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);


    procedure BtnUpClick(Sender: TObject);
  private
    procedure MainFrm_UpdateButtons; overload;
    procedure MainFrm_UpdateButtons(Value: boolean); overload;
    procedure MainFrm_UpdateCursor(Value: TCursor);
    procedure MainFrm_UpdateStyle;
  end;

var
  MainFrm: TMainFrm;
  
implementation

uses
  Bee_common,

  BeeGui_ViewFrm,
  BeeGui_AboutFrm,
  BeeGui_SysUtils,

  BeeFm_ConfigFrm,
  BeeFM_PropertyFrm;

  { TMainFrm }

  procedure TMainFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    ArchiveListView.InitColumns;
    // ---
    SmallIcons.IconFolder := ExtractFilePath(ParamStr(0)) + 'smallicons';
    LargeIcons.IconFolder := ExtractFilePath(ParamStr(0)) + 'largeicons';
    // ---
    CfgFolder := IncludeTrailingBackSlash(GetApplicationConfigDir('BeeGui'));
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder + ('mainfrm.xml');
    end;
    {$I beefm_mainfrm.inc}
    Storage.Restore;
    // ---
    MainFrm_UpdateButtons(False);
    MainFrm_UpdateButtons;
    MainFrm_UpdateStyle;
  end;
  
  procedure TMainFrm.FormDestroy(Sender: TObject);
  begin
  
  end;

  procedure TMainFrm.ArchiveProcessTimerTimer(Sender: TObject);
  var
    F: TViewFrm;
  begin
    with ArchiveProcess do
    begin
      if Running = False then
      begin
        ArchiveProcessTimer.Enabled := False;
        ArchiveListView.OpenArchive(ArchiveName, ArchiveLink);
        MainFrm_UpdateButtons(True);
      end;
    end;
  end;

  procedure TMainFrm.FolderBoxSelect(Sender: TObject);
  begin
    ArchiveListView.Folder := FolderBox.Text;
  end;

  procedure TMainFrm.ArchiveListViewColumnClick(Sender: TObject; Column: TListColumn);
  begin
    case Column.Index of
      0: MainMenu_View_OrderBy_Name.Click;
      1: MainMenu_View_OrderBy_Size.Click;
      2: MainMenu_View_OrderBy_Packed.Click;
      3: MainMenu_View_OrderBy_Ratio.Click;
      4: MainMenu_View_OrderBy_Type.Click;
      5: MainMenu_View_OrderBy_Modified.Click;
      6: MainMenu_View_OrderBy_Attributes.Click;
      7: MainMenu_View_OrderBy_Method.Click;
      8: MainMenu_View_OrderBy_Password.Click;
      9: MainMenu_View_OrderBy_Crc.Click;
     10: MainMenu_View_OrderBy_Path.Click;
     11: MainMenu_View_OrderBy_Position.Click;
    end;
  end;

  procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    if Cursor = crHourGlass then
    begin
      CanClose := MessageDlg('Active proces. Terminate it?', mtWarning, [mbYes, mbNo], 0) = mrYes;
    end else
    begin
      CanClose := True;
    end;
  end;

  procedure TMainFrm.BtnUpClick(Sender: TObject);
  begin
    ArchiveListView.Up;
  end;

  procedure TMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    if not MainMenu_Options_SaveOnExit.Checked then
    begin
      MainFrm.SessionProperties := '';
      // ConfigFrm.SessionProperties := '';
    end;
    if Cursor = crHourGlass then
    begin
    
    end else
    begin
      MainMenu_File_Close.Click;
    end;
  end;

  procedure TMainFrm.FormResize(Sender: TObject);
  begin
    (*
    MainFrm_StatusBar.Panels[0].Width := MainFrm_StatusBar.Width * 2 div 3;
    MainFrm_StatusBar.Panels[1].Width := MainFrm_StatusBar.Width * 1 div 3;
    // ---
    MainFrm_StatusBar.DrawProgressBar(1);
    *)
  end;

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
    for I:= Low(Buttons) to High(Buttons) do
    begin
      Buttons[I].Visible := PopupMenu_Buttons.Items[I].Checked;
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
  end;

  procedure TMainFrm.MainFrm_UpdateCursor(Value: TCursor);
  begin
    MainFrm.Cursor := Value;
  end;

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
    MainMenu_File_Close .Enabled := Value;
    MainMenu_File_Property  .Enabled := Value;
    MainMenu_File_Move  .Enabled := Value;
    MainMenu_File_Copy  .Enabled := Value;
    MainMenu_File_Rename.Enabled := Value;
    MainMenu_File_Delete.Enabled := Value;
    // ---
    for I := 0 to MainMenu_Actions.Count -1 do
    begin
      MainMenu_Actions.Items[I].Enabled := Value;
    end;
  end;
  
  procedure TMainFrm.MainFrm_UpdateStyle;
  begin
    if MainMenu_View_LargeIcons.Checked then
      ArchiveListView.ViewStyle := vsIcon
    else
    if MainMenu_View_SmallIcons.Checked then
      ArchiveListView.ViewStyle := vsSmallIcon
    else
    if MainMenu_View_List.Checked then
      ArchiveListView.ViewStyle := vsList
    else
    if MainMenu_View_Report.Checked then
      ArchiveListView.ViewStyle := vsReport;
      
    ArchiveListView.RowSelect  := MainMenu_View_RowSelect.Checked;
    ArchiveListView.GridLines  := MainMenu_View_GridLines.Checked;
    ArchiveListView.SimpleList := MainMenu_View_ListMode.Checked;
    
    FolderBoxToolBar.Visible := not ArchiveListView.SimpleList;
    if FolderBoxToolBar.Visible then
      ButtonsToolBar.EdgeBorders := [ebLeft, ebTop, ebRight]
    else
      ButtonsToolBar.EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
    FolderBoxToolBar.EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  end;

  procedure TMainFrm.MainMenu_File_NewClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if Cursor <> crHourGlass then
    begin
      SaveDialog.FileName := '';
      if SaveDialog.Execute then
      begin
        MainMenu_File_Close.Click;

        with ArchiveProcess do
        begin
          ArchiveName := SaveDialog.FileName;
          case SaveDialog.FilterIndex of
            1: ArchiveName := ChangeFileExt(ArchiveName, '.bee');
            2: ArchiveName := ChangeFileExt(ArchiveName, '.exe');
          end;
          Caption := 'Bee' + ' - ' + ExtractFileName(ArchiveName);
        end;
        // ---
        CmdLine := 'BeeGui ';
        // ---
        ArchiveProcess.CommandLine := CmdLine;
        ArchiveProcess.Execute;
        ArchiveProcessTimer.Enabled := True;
      end;
    end else
      MessageDlg('An process active', mtInformation, [mbOk], 0);
  end;

  procedure TMainFrm.MainMenu_Actions_AddClick(Sender: TObject);
  begin

  end;

  procedure TMainFrm.MainMenu_File_OpenClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if Cursor <> crHourGlass then
    begin
      OpenDialog.FileName := '';
      OpenDialog.Filter :=
        'bee file (*.bee)|*.bee|' +
        'exe file (*.bee)|*.exe|' +
        'all files  (*.*)|*.*|';

      if OpenDialog.Execute then
      begin
        MainMenu_File_Close.Click;

        ArchiveProcess.ArchiveName := OpenDialog.FileName;
        Caption := 'BeeFM' + ' - ' + ExtractFileName(ArchiveProcess.ArchiveName);
        // ---
        CmdLine := 'BeeGui';
        CmdLine := CmdLine + ' L ';
        if MainMenu_Options_LogReport.Checked then
          CmdLine := CmdLine + ' -1+ '
        else
          CmdLine := CmdLine + ' -1- ';
        CmdLine := CmdLine + '"' + ArchiveProcess.ArchiveName + '"';
        CmdLine := CmdLine + ' *!';
        // ---
        ArchiveProcess.CommandLine := CmdLine;
        ArchiveProcessTimer.Enabled := True;
        ArchiveProcess.Execute;
      end;
    end else
      MessageDlg('An process active', mtInformation, [mbOk], 0);
  end;

  procedure TMainFrm.MainMenu_File_CloseClick(Sender: TObject);
  begin
    Caption := 'BeeFM';
    
    MainFrm_UpdateCursor(crDefault);
    MainFrm_UpdateButtons(False);
    
    ArchiveListView.CloseArchive;
  end;

  procedure TMainFrm.MainMenu_File_MoveClick(Sender: TObject);
  var
    NewName: string;
  begin
    NewName := '';
    if Cursor = crHourGlass then Exit;
    // ---
    if SelectDirectory('Move archive to:', '', NewName) then
    begin
      NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(ArchiveProcess.ArchiveName);
      if RenameFile(ArchiveProcess.ArchiveName, NewName) then
        ArchiveProcess.ArchiveName := NewName
      else
        MessageDlg('Error moving archive', mtError, [mbOk], 0);
    end;
  end;

  procedure TMainFrm.MainMenu_File_PropertyClick(Sender: TObject);
  var
    F: TInfoFrm;
  begin
    if Cursor = crHourGlass then Exit;
    // ---
    F := TInfoFrm.Create(Self);
    if F.UpdateAInfo(ArchiveProcess.ArchiveName, ArchiveListView.Details) then
    begin
      F.ShowModal;
    end else
      MessageDlg('Can''t load archive infomations.', mtInformation, [mbOk], 0);
    F.Free;
  end;

  procedure TMainFrm.MainMenu_File_CopyClick(Sender: TObject);
  var
    NewName: string;
  begin
    if Cursor <> crHourGlass then
    begin
      NewName := '';
      if SelectDirectory('Copy archive to:', '', NewName) then
      begin
        NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(ArchiveProcess.ArchiveName);
        if CopyFile(ArchiveProcess.ArchiveName, NewName) then
          ArchiveProcess.ArchiveName := NewName
        else
          MessageDlg('Error copying archive', mtError, [mbOk], 0);
      end;
    end;
  end;

  procedure TMainFrm.MainMenu_File_RenameClick(Sender: TObject);
  begin
  (*
  var
    F: TRenameFrm;
    NewName: string;
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      F := TRenameFrm.Create(Self);
      F.Caption := 'Rename archive';
      F.RenameFrm_To.Text := ExtractFileName(AppArcName);

      if f.ShowModal = mrOk then
      begin
        if AnsiCompareFileName(f.RenameFrm_From.Caption, f.RenameFrm_To.Text) <> 0 then
        begin
          NewName := ExtractFilePath(AppArcName) + f.RenameFrm_To.Text;
          if RenameFile (AppArcName, NewName) then
          begin
            MainMenu_File_Close.Click;
            AppArcName := NewName;

            Caption := 'Bee' + ' - ' + ExtractFileName(AppArcName);
          end else
            MessageDlg('Error on renaming archive', mtError, [mbOk], 0);
        end;
      end;
      f.Free;
    end;
    *)
  end;

  procedure TMainFrm.MainMenu_File_DeleteClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
      if MessageDlg('Delete archive?', mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        if DeleteFile(ArchiveProcess.ArchiveName) then
          MainMenu_File_Close.Click
        else
          MessageDlg('Error on deleting archive.', mtError, [mbOk], 0);
      end;
    end;
  end;

  procedure TMainFrm.MainMenu_File_ExitClick(Sender: TObject);
  begin
    Close;
  end;



  procedure TMainFrm.MainMenu_View_ViewStyle_Click(Sender: TObject);
  begin
    if Sender <> nil then
    begin
      MainMenu_View_LargeIcons.Checked := Sender = MainMenu_View_LargeIcons;
      MainMenu_View_SmallIcons.Checked := Sender = MainMenu_View_SmallIcons;
      MainMenu_View_Report    .Checked := Sender = MainMenu_View_Report;
      MainMenu_View_List      .Checked := Sender = MainMenu_View_List;
    end;
    MainFrm_UpdateStyle;
  end;
  
  procedure TMainFrm.MainMenu_View_RowSelectClick(Sender: TObject);
  begin
    MainMenu_View_RowSelect.Checked := not MainMenu_View_RowSelect.Checked;
    MainFrm_UpdateStyle;
  end;
  
  procedure TMainFrm.MainMenu_View_GridLinesClick(Sender: TObject);
  begin
    MainMenu_View_GridLines.Checked := not MainMenu_View_GridLines.Checked;
    MainFrm_UpdateStyle;
  end;
  
  procedure TMainFrm.MainMenu_View_ListModeClick(Sender: TObject);
  begin
    MainMenu_View_ListMode.Checked := not MainMenu_View_ListMode.Checked;
    MainFrm_UpdateStyle;
  end;

  procedure TMainFrm.MainMenu_Actions_DeleteClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
      if MessageDlg('Delete selected files?' , mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin

      end;
    end;
  end;

  procedure TMainFrm.MainMenu_Actions_ExtractClick(Sender: TObject);
  begin
  end;

  procedure TMainFrm.MainMenu_Actions_ExtractAllClick(Sender: TObject);
  begin
  (*
  var
    F: TSeDeseFrm;
  begin
    if MainFrm_ArchiveTreeView.Cursor <> crHourGlass then
    begin
      F := TSeDeseFrm.Create (Self);
      F.DeSelectAll (MainFrm_ArchiveTreeView);
      F.Free;
    end;
  *)
  end;

  procedure TMainFrm.MainMenu_Actions_TestClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin

    end;
  end;

  procedure TMainFrm.MainMenu_Actions_RenameClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
    end;
  end;

  procedure TMainFrm.MainMenu_Actions_ViewClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
      if ArchiveListView.Selected <> nil then
      begin
        if Length(ArchiveListView.Selected.SubItems[ArchiveListView.Columns.Count - 2]) = 0 then
        begin
          ArchiveListView.Folder :=IncludeTrailingBackSlash(
            ArchiveListView.Folder) + ArchiveListView.Selected.Caption;
        end else
        begin
          // file Extraction
        end;
      end;
    end;
  end;

  procedure TMainFrm.MainMenu_Actions_CheckOutClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin

    end;
  end;

  procedure TMainFrm.MainMenu_Actions_TestAllClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin

    end;
  end;

  procedure TMainFrm.MainMenu_Actions_SelectAllClick(Sender: TObject);
  begin
  (*
  var
    F: TSeDeseFrm;
  begin
    if Cursor <> crHourGlass then
    begin
      MainFrm_ArchiveTreeView.SetFocus;
      F := TSeDeseFrm.Create(Self);
      try
        F.SelectAll(MainFrm_ArchiveTreeView);
      finally
        F.Free;
      end;
      MainFrm_ArchiveTreeViewSelectionChanged(Self);
    end;
    *)
  end;

  procedure TMainFrm.MainMenu_Actions_SelectMaskClick(Sender: TObject);
  begin
  (*
  var
    F: TSeDeseFrm;
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
    *)
  end;

  procedure TMainFrm.MainMenu_Actions_DeselectAllClick(Sender: TObject);
  begin
  (*
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
    *)
  end;

  procedure TMainFrm.MainMenu_Actions_DeselectMasksClick (Sender: TObject);
  begin
  (*
  var
    F: TSeDeseFrm;
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
    *)
  end;

  procedure TMainFrm.MainMenu_Actions_InvertClick(Sender: TObject);
  begin
  (*
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
    *)
  end;

  procedure TMainFrm.MainMenu_Options_ConfigurationClick(Sender: TObject);
  begin
    ConfigFrm.ShowModal;
  end;

  procedure TMainFrm.MainMenu_Options_PasswordClick(Sender: TObject);
  begin
  end;

  procedure TMainFrm.MainMenu_Options_SaveOnExitClick(Sender: TObject);
  begin
    MainMenu_Options_SaveOnExit.Checked := not MainMenu_Options_SaveOnExit.Checked;
  end;

  procedure TMainFrm.MainMenu_Options_SaveNowClick(Sender: TObject);
  begin
    Storage.Save;
  end;

  procedure TMainFrm.MainMenu_Options_DefaultSettingClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
      if MessageDlg('Default setting?', mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        MainMenu_File_Exit.Click;
        ClearDirectory(GetApplicationConfigDir('BeeGui'));
        // ---
        ShellExec(ParamStr(0), '');
      end;
    end else
      MessageDlg('An active process. Please wait.', mtInformation, [mbOk], 0);
  end;

  procedure TMainFrm.MainMenu_Options_LogReportClick (Sender: TObject);
  begin
    MainMenu_Options_LogReport.Checked := not MainMenu_Options_LogReport.Checked;
  end;

  procedure TMainFrm.MainMenu_Help_AboutClick(Sender: TObject);
  var
    F: TForm;
  begin
    F := TAboutFrm.Create(Self);
    F.ShowModal;
    F.Free;
  end;


  procedure TMainFrm.MainMenu_View_OrderBy_Click(Sender: TObject);
  begin
    if Sender = MainMenu_View_OrderBy_Name then
      ArchiveListView.SortCol := alvcName
    else
    if Sender = MainMenu_View_OrderBy_Size then
      ArchiveListView.SortCol := alvcSize
    else
    if Sender = MainMenu_View_OrderBy_Packed then
      ArchiveListView.SortCol := alvcPacked
    else
    if Sender = MainMenu_View_OrderBy_Ratio then
      ArchiveListView.SortCol := alvcRatio
    else
    if Sender = MainMenu_View_OrderBy_Type then
      ArchiveListView.SortCol := alvcType
    else
    if Sender = MainMenu_View_OrderBy_Modified then
      ArchiveListView.SortCol := alvcTime
    else
    if Sender = MainMenu_View_OrderBy_Attributes then
      ArchiveListView.SortCol := alvcAttr
    else
    if Sender = MainMenu_View_OrderBy_Method then
      ArchiveListView.SortCol := alvcMethod
    else
    if Sender = MainMenu_View_OrderBy_Password then
      ArchiveListView.SortCol := alvcPassword
    else
    if Sender = MainMenu_View_OrderBy_Crc then
      ArchiveListView.SortCol := alvcCRC
    else
    if Sender = MainMenu_View_OrderBy_Path then
      ArchiveListView.SortCol := alvcPath
    else
    if Sender = MainMenu_View_OrderBy_Position then
      ArchiveListView.SortCol := alvcPosition;
  end;

  procedure TMainFrm.MainMenu_Help_F1Click(Sender: TObject);
  var
    Help_FileName: string;
    Help_FilePath: string;
  begin
    Help_FilePath := ExtractFilePath(ParamStr(0));
    Help_FileName := Help_FilePath + 'docs' + PathDelim + 'help.htm';

    ShellExec(Help_FileName, Help_FilePath);
  end;

  procedure TMainFrm.MainMenu_Help_InternetClick(Sender: TObject);
  var
    F: TAboutFrm;
  begin
    F := TaboutFrm.Create(Self);
    F.LinkClick(Self);
    F.Free;
  end;

  procedure TMainFrm.MainMenu_Help_LicenseClick(Sender: TObject);
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
  
  procedure TMainFrm.PopupMenu_Buttons_NewClick(Sender: TObject);
  begin
    PopupMenu_Buttons_New.Checked := not PopupMenu_Buttons_New.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_OpenClick(Sender: TObject);
  begin
    PopupMenu_Buttons_Open.Checked := not PopupMenu_Buttons_Open.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_N1Click(Sender: TObject);
  begin
    PopupMenu_Buttons_N1.Checked := not PopupMenu_Buttons_N1.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_AddClick(Sender: TObject);
  begin
    PopupMenu_Buttons_Add.Checked := not PopupMenu_Buttons_Add.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_ExtractClick(Sender: TObject);
  begin
    PopupMenu_Buttons_Extract.Checked := not PopupMenu_Buttons_Extract.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_ViewClick(Sender: TObject);
  begin
    PopupMenu_Buttons_View.Checked := not PopupMenu_Buttons_View.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_DeleteClick(Sender: TObject);
  begin
    PopupMenu_Buttons_Delete.Checked := not PopupMenu_Buttons_Delete.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_N2Click(Sender: TObject);
  begin
    PopupMenu_Buttons_N2.Checked := not PopupMenu_Buttons_N2.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_TestClick(Sender: TObject);
  begin
    PopupMenu_Buttons_Test.Checked := not PopupMenu_Buttons_Test.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_CheckOutClick(Sender: TObject);
  begin
    PopupMenu_Buttons_CheckOut.Checked := not PopupMenu_Buttons_CheckOut.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_N3Click(Sender: TObject);
  begin
    PopupMenu_Buttons_N3.Checked := not PopupMenu_Buttons_N3.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_ConfigurationClick(Sender: TObject);
  begin
    PopupMenu_Buttons_Configuration.Checked := not PopupMenu_Buttons_Configuration.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_HelpClick(Sender: TObject);
  begin
    PopupMenu_Buttons_Help.Checked := not PopupMenu_Buttons_Help.Checked;
    MainFrm_UpdateButtons;
  end;

  procedure TMainFrm.PopupMenu_Buttons_ExitClick(Sender: TObject);
  begin
    PopupMenu_Buttons_Exit.Checked := not PopupMenu_Buttons_Exit.Checked;
    MainFrm_UpdateButtons;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  PopupnMenu Click                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.PopupMenu_Open_IntViewerClick (Sender: TObject);
  begin

  end;

  procedure TMainFrm.PopupMenu_PropertyClick(Sender: TObject);
  begin
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  PopupMenu Events                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //
  
  procedure TMainFrm.PopupMenuPopup(Sender: TObject);
  begin

  end;
  
initialization

  {$I beefm_mainfrm.lrs}
  
end.


