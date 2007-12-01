unit BeeGui_MainFrmRes;

interface

uses
  SysUtils;

const
  _CMainFrm_TerminateProcess = 13;
  _CMainFrm_NoFilesSelected = 15;
  _CMainFrm_DeleteCheckoutDir = 18;
  _CMainFrm_ErrOnSetCurrDir = 20;
  _CMainFrm_ActiveProces = 14;
  _CMainFrm_MoveTo = 0;
  _CMainFrm_ErrOnMoveTo = 1;
  _CMainFrm_CopyTo = 2;
  _CMainFrm_ErrOnCopyTo = 3;
  _CMainFrm_RenameArc = 4;
  _CMainFrm_ErrOnRenameArc = 5;
  _CMainFrm_DeleteArc = 6;
  _CMainFrm_ErrOnDeleteArc = 7;
  _CMainFrm_Delete_Files = 12;
  _CMainFrm_RenameFolder = 21;
  _CMainFrm_ErrOnCreateCheckOutDir = 19;
  _CMainFrm_IsNeedToRestartApp = 10;
  _CMainFrm_ErrOnDeleteCfgFile = 11;
  _CMainFrm_Items  = 17;
  _CMainFrm_ItemsSelected = 16;
  _CMainFrm_RenameFile = 8;
  _CMainFrm_FreshFileModified = 24;
  _CMainFrm_ErrOnLoadArcInfo = 25;

(*
0 Select a directory where move the archive
1 error on moving the archive
2 Select a directory where copy the archive
3 error on copying the archive
4 Rename archive
5 error on renaming the archive
6 Do you want really to delete the archive?
7 error on deleting the archive
8 Rename file
9 error on renaming file
10 Is necessary restart the application. Continue?
11 error deleting setting file
12 Do You want to delete files selected?
13 Actually there is an active process. Terminate this process?
14 Actually there is an active process.
15 No files selected.
16 Items selected: %u
17 Items: %u
18 CheckOut directory founded. Delete?
19 error on create CheckOut directory.
20 error on set current directory.
21 Rename directory
22 error renaming directory
23 Associate .Bee file type with BeeGui application?
24 File is modified. Do You want freshen it?
25 Can''t load archive infomations.
*)

resourcestring
  _SAppTitle = 'BeeGui';
  _SAppPausedTitle = 'Pause - BeeGui';

  _SMainFrm_HelpFile = 'docs' + PathDelim + 'help.htm';

  _SMainFrm_Property = 'MainFrm.WindowState;' +
    'MainFrm_PopupMenu_Buttons_New.Checked;' +
    'MainFrm_PopupMenu_Buttons_Open.Checked;' +
    'MainFrm_PopupMenu_Buttons_N1.Checked;' +
    'MainFrm_PopupMenu_Buttons_Add.Checked;' +
    'MainFrm_PopupMenu_Buttons_Extract.Checked;' +
    'MainFrm_PopupMenu_Buttons_View.Checked;' +
    'MainFrm_PopupMenu_Buttons_Delete.Checked;' +
    'MainFrm_PopupMenu_Buttons_N2.Checked;' +
    'MainFrm_PopupMenu_Buttons_Test.Checked;' +
    'MainFrm_PopupMenu_Buttons_Info.Checked;' +
    'MainFrm_PopupMenu_Buttons_CheckOut.Checked;' +
    'MainFrm_PopupMenu_Buttons_N3.Checked;' +
    'MainFrm_PopupMenu_Buttons_Help.Checked;' +
    'MainFrm_PopupMenu_Buttons_Exit.Checked;' +
    'MainFrm_MainMenu_View_ToolBar_AddressBar.Checked;' +
    'MainFrm_MainMenu_View_StatusBar.Checked;' +
    'MainFrm_MainMenu_View_RowSelect.Checked;' +
    'MainFrm_MainMenu_View_GridLines.Checked;' +
    'MainFrm_MainMenu_View_ListMode.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Name.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Type.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Packed.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Size.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Ratio.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Method.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Modified.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Attributes.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Password.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_CRC.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Path.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Position.Checked;' +
    'MainFrm_MainMenu_View_Details_Name.Checked;' +
    'MainFrm_MainMenu_View_Details_Type.Checked;' +
    'MainFrm_MainMenu_View_Details_Packed.Checked;' +
    'MainFrm_MainMenu_View_Details_Size.Checked;' +
    'MainFrm_MainMenu_View_Details_Ratio.Checked;' +
    'MainFrm_MainMenu_View_Details_Method.Checked;' +
    'MainFrm_MainMenu_View_Details_Modified.Checked;' +
    'MainFrm_MainMenu_View_Details_Attributes.Checked;' +
    'MainFrm_MainMenu_View_Details_Password.Checked;' +
    'MainFrm_MainMenu_View_Details_CRC.Checked;' +
    'MainFrm_MainMenu_View_Details_Path.Checked;' +
    'MainFrm_MainMenu_View_Details_Position.Checked;' +
    'MainFrm_MainMenu_Options_SaveOnExit.Checked;' +
    'MainFrm_ListView.ViewStyle;';

  _SMainFrm_PropertyFull = 'MainFrm.Left;' +
    'MainFrm.Height;' + 'MainFrm.Top;' +
    'MainFrm.Width;' +
    'MainFrm.WindowState;' +
    'MainFrm_PopupMenu_Buttons_New.Checked;' +
    'MainFrm_PopupMenu_Buttons_Open.Checked;' +
    'MainFrm_PopupMenu_Buttons_N1.Checked;' +
    'MainFrm_PopupMenu_Buttons_Add.Checked;' +
    'MainFrm_PopupMenu_Buttons_Extract.Checked;' +
    'MainFrm_PopupMenu_Buttons_View.Checked;' +
    'MainFrm_PopupMenu_Buttons_Delete.Checked;' +
    'MainFrm_PopupMenu_Buttons_N2.Checked;' +
    'MainFrm_PopupMenu_Buttons_Test.Checked;' +
    'MainFrm_PopupMenu_Buttons_Info.Checked;' +
    'MainFrm_PopupMenu_Buttons_CheckOut.Checked;' +
    'MainFrm_PopupMenu_Buttons_N3.Checked;' +
    'MainFrm_PopupMenu_Buttons_Help.Checked;' +
    'MainFrm_PopupMenu_Buttons_Exit.Checked;' +
    'MainFrm_MainMenu_View_ToolBar_AddressBar.Checked;' +
    'MainFrm_MainMenu_View_StatusBar.Checked;' +
    'MainFrm_MainMenu_View_RowSelect.Checked;' +
    'MainFrm_MainMenu_View_GridLines.Checked;' +
    'MainFrm_MainMenu_View_ListMode.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Name.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Type.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Packed.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Size.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Ratio.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Method.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Modified.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Attributes.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Password.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_CRC.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Path.Checked;' +
    'MainFrm_MainMenu_View_OrderBy_Position.Checked;' +
    'MainFrm_MainMenu_View_Details_Name.Checked;' +
    'MainFrm_MainMenu_View_Details_Type.Checked;' +
    'MainFrm_MainMenu_View_Details_Packed.Checked;' +
    'MainFrm_MainMenu_View_Details_Size.Checked;' +
    'MainFrm_MainMenu_View_Details_Ratio.Checked;' +
    'MainFrm_MainMenu_View_Details_Method.Checked;' +
    'MainFrm_MainMenu_View_Details_Modified.Checked;' +
    'MainFrm_MainMenu_View_Details_Attributes.Checked;' +
    'MainFrm_MainMenu_View_Details_Password.Checked;' +
    'MainFrm_MainMenu_View_Details_CRC.Checked;' +
    'MainFrm_MainMenu_View_Details_Path.Checked;' +
    'MainFrm_MainMenu_View_Details_Position.Checked;' +
    'MainFrm_MainMenu_Options_SaveOnExit.Checked;' +
    'MainFrm_ListView.ViewStyle;';

  _SMainFrm_LanguageS1 = 'MainFrm.Caption;' +
    'BtnUp.Caption;' +
    'BtnOpen.Caption;' +
    'BtnNew.Caption;' +
    'BtnAdd.Caption;' +
    'BtnExtract.Caption;' +
    'BtnView.Caption;' +
    'BtnDelete.Caption;' +
    'BtnTest.Caption;' +
    'BtnInfo.Caption;' +
    'BtnCheckOut.Caption;' +
    'BtnHelp.Caption;' +
    'BtnExit.Caption;' +
    'MainFrm_PopupMenu_Buttons_New.Caption;' +
    'MainFrm_PopupMenu_Buttons_Open.Caption;' +
    'MainFrm_PopupMenu_Buttons_N1.Caption;' +
    'MainFrm_PopupMenu_Buttons_Add.Caption;' +
    'MainFrm_PopupMenu_Buttons_Extract.Caption;' +
    'MainFrm_PopupMenu_Buttons_View.Caption;' +
    'MainFrm_PopupMenu_Buttons_Delete.Caption;' +
    'MainFrm_PopupMenu_Buttons_N2.Caption;' +
    'MainFrm_PopupMenu_Buttons_Test.Caption;' +
    'MainFrm_PopupMenu_Buttons_Info.Caption;' +
    'MainFrm_PopupMenu_Buttons_CheckOut.Caption;' +
    'MainFrm_PopupMenu_Buttons_N3.Caption;' +
    'MainFrm_PopupMenu_Buttons_Help.Caption;' +
    'MainFrm_PopupMenu_Buttons_Exit.Caption;' +
    'MainFrm_PopupMenu_Open.Caption;' +
    'MainFrm_PopupMenu_Open_IntViewer.Caption;' +
    'MainFrm_PopupMenu_Delete.Caption;' +
    'MainFrm_PopupMenu_Extract.Caption;' +
    'MainFrm_PopupMenu_ExtractAll.Caption;' +
    'MainFrm_PopupMenu_Test.Caption;' +
    'MainFrm_PopupMenu_Rename.Caption;' +
    'MainFrm_PopupMenu_OrderBy.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Name.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Type.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Packed.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Size.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Ratio.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Method.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Modified.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Attributes.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Password.Caption;' +
    'MainFrm_PopupMenu_OrderBy_CRC.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Path.Caption;' +
    'MainFrm_PopupMenu_OrderBy_Position.Caption;' +
    'MainFrm_PopupMenu_Details.Caption;' +
    'MainFrm_PopupMenu_Details_Name.Caption;' +
    'MainFrm_PopupMenu_Details_Type.Caption;' +
    'MainFrm_PopupMenu_Details_Packed.Caption;' +
    'MainFrm_PopupMenu_Details_Size.Caption;' +
    'MainFrm_PopupMenu_Details_Ratio.Caption;' +
    'MainFrm_PopupMenu_Details_Method.Caption;' +
    'MainFrm_PopupMenu_Details_Modified.Caption;' +
    'MainFrm_PopupMenu_Details_Attributes.Caption;' +
    'MainFrm_PopupMenu_Details_Password.Caption;' +
    'MainFrm_PopupMenu_Details_CRC.Caption;' +
    'MainFrm_PopupMenu_Details_Path.Caption;' +
    'MainFrm_PopupMenu_Details_Position.Caption;';

  _SMainFrm_LanguageS2 = 'MainFrm_MainMenu_File.Caption;' +
    'MainFrm_MainMenu_File_New.Caption;' +
    'MainFrm_MainMenu_File_Open.Caption;' +
    'MainFrm_MainMenu_File_Close.Caption;' +
    'MainFrm_MainMenu_File_Info.Caption;' +
    'MainFrm_MainMenu_File_Move.Caption;' +
    'MainFrm_MainMenu_File_Copy.Caption;' +
    'MainFrm_MainMenu_File_Rename.Caption;' +
    'MainFrm_MainMenu_File_Delete.Caption;' +
    'MainFrm_MainMenu_File_Exit.Caption;' +
    'MainFrm_MainMenu_View.Caption;' +
    'MainFrm_MainMenu_View_ToolBar.Caption;' +
    'MainFrm_MainMenu_View_ToolBar_Btns.Caption;' +
    'MainFrm_MainMenu_View_ToolBar_AddressBar.Caption;' +
    'MainFrm_MainMenu_View_StatusBar.Caption;' +
    'MainFrm_MainMenu_View_Icons.Caption;' +
    'MainFrm_MainMenu_View_SmallIcons.Caption;' +
    'MainFrm_MainMenu_View_List.Caption;' +
    'MainFrm_MainMenu_View_Report.Caption;' +
    'MainFrm_MainMenu_View_RowSelect.Caption;' +
    'MainFrm_MainMenu_View_GridLines.Caption;' +
    'MainFrm_MainMenu_View_ListMode.Caption;' +
    'MainFrm_MainMenu_View_OrderBy.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Name.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Type.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Packed.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Size.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Ratio.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Method.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Modified.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Attributes.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Password.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_CRC.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Path.Caption;' +
    'MainFrm_MainMenu_View_OrderBy_Position.Caption;' +
    'MainFrm_MainMenu_View_Details.Caption;' +
    'MainFrm_MainMenu_View_Details_Name.Caption;' +
    'MainFrm_MainMenu_View_Details_Type.Caption;' +
    'MainFrm_MainMenu_View_Details_Packed.Caption;' +
    'MainFrm_MainMenu_View_Details_Size.Caption;' +
    'MainFrm_MainMenu_View_Details_Ratio.Caption;' +
    'MainFrm_MainMenu_View_Details_Method.Caption;' +
    'MainFrm_MainMenu_View_Details_Modified.Caption;' +
    'MainFrm_MainMenu_View_Details_Attributes.Caption;' +
    'MainFrm_MainMenu_View_Details_Password.Caption;' +
    'MainFrm_MainMenu_View_Details_CRC.Caption;' +
    'MainFrm_MainMenu_View_Details_Path.Caption;' +
    'MainFrm_MainMenu_View_Details_Position.Caption;' +
    'MainFrm_MainMenu_Actions.Caption;' +
    'MainFrm_MainMenu_Actions_Add.Caption;' +
    'MainFrm_MainMenu_Actions_Delete.Caption;' +
    'MainFrm_MainMenu_Actions_Extract.Caption;' +
    'MainFrm_MainMenu_Actions_ExtractAll.Caption;' +
    'MainFrm_MainMenu_Actions_Test.Caption;' +
    'MainFrm_MainMenu_Actions_Rename.Caption;' +
    'MainFrm_MainMenu_Actions_View.Caption;' +
    'MainFrm_MainMenu_Actions_SelectAll.Caption;' +
    'MainFrm_MainMenu_Actions_SelectMask.Caption;' +
    'MainFrm_MainMenu_Actions_UnselectMask.Caption;' +
    'MainFrm_MainMenu_Actions_Invert.Caption;' +
    'MainFrm_MainMenu_Actions_CheckOut.Caption;' +
    'MainFrm_MainMenu_Actions_TestAll.Caption;' +
    'MainFrm_MainMenu_Options.Caption;' +
    'MainFrm_MainMenu_Options_Config.Caption;' +
    'MainFrm_MainMenu_Options_Password.Caption;' +
    'MainFrm_MainMenu_Options_SaveOnExit.Caption;' +
    'MainFrm_MainMenu_Options_SaveNow.Caption;' +
    'MainFrm_MainMenu_Options_Default.Caption;' +
    'MainFrm_MainMenu_Options_LogReport.Caption;' +
    'MainFrm_MainMenu_Help.Caption;' +
    'MainFrm_MainMenu_Help_F1.Caption;' +
    'MainFrm_MainMenu_Help_Internet.Caption;' +
    'MainFrm_MainMenu_Help_License.Caption;' +
    'MainFrm_MainMenu_Help_About.Caption;';

implementation

end.
