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

unit BeeGui_MainFrm;

{$I compiler.inc}

interface

uses
  Math,
  Forms,
  Menus,
  Classes,
  Dialogs,
  Buttons,
  ImgList,
  Windows,
  ActiveX,
  Messages,
  SysUtils,
  Graphics,
  Controls,
  ComCtrls,
  StdCtrls,
  FileCtrl,
  ExtCtrls,
  AppEvnts,
  // ---
  Bee_Interface,
  // ---
  BeeGui_ConfigFrm,
  // ---
  BeeGui_AppViewer,
  BeeGui_FormStorage,
  BeeGui_DragFilesTrg,
  BeeGui_DragFilesSrc,
  BeeGui_ArchiveListManager;

type
  TMainAppTerminateFlag =
    (tfNew, tfOpen, tfAdd, tfExtract, tfCheckOut,
     tfViewInt, tfViewExt, tfClose, tfFatalError, tfLog);

type
  TMainAppTerminateFlags = set of TMainAppTerminateFlag;

type
  TMainAppRunRes =
    (rfNone, rfContinue, rfAbort, rfPause, rfAbortAndClose);

type
  TMainApp = class
  private
    FApp: TApp;
    FAppArcName: string;
    FAppKey: string;
    FAppLog: TStringList;
    FAppRenameDir: boolean;
    FAppParams: TStringList;
    FAppRenameDirTo: string;
    FAppRenameDirFrom: string;
    FAppInterface: TAppInterface;
    FAppTerminateFlags: TMainAppTerminateFlags;
    procedure FAppOnKey;
    procedure FAppOnTick;
    procedure FAppOnList;
    procedure FAppOnError;
    procedure FAppOnClear;
    procedure FAppOnRename;
    procedure FAppOnWarning;
    procedure FAppOnDisplay;
    procedure FAppOnDisplayWithLog;
    procedure FAppFatalError;
    procedure FAppOnOverWrite;
    // ---
    procedure FAppInterfaceCreate;
    procedure FAppInterfaceUpdate;
    procedure FAppInterfaceDestroy;
    // ---
    procedure FAppOnTerminate(Sender: TObject);
    // ---
    procedure FAppTerminateWithNew;
    procedure FAppTerminateWithAdd;
    procedure FAppTerminateWithLog;
    procedure FAppTerminateWithOpen;
    procedure FAppTerminateWithExtract;
    procedure FAppTerminateWithCheckOut;
    procedure FAppTerminateWithView(FileName: string; UseIntViewer: boolean);
  private
    FViewer: TAppViewer;
    procedure FViewerOnTerminate(Sender: TObject);
  public
    property AppKey: string Read FAppKey Write FAppKey;
    property AppArcName: string Read FAppArcName Write FAppArcName;
    property AppParams: TStringList Read FAppParams Write FAppParams;
    // ---
    property AppTerminateFlags: TMainAppTerminateFlags
      Read FAppTerminateFlags Write FAppTerminateFlags;
    // ---
    procedure AppInterfaceClear;
    function CmdIsSupported(const cmd: char): boolean;
    constructor Create;
    destructor Destroy;
    procedure Execute;
  end;

type
  TMainFrm = class(TForm)
    // Panel
    MainFrm_ButtonsPanel: TPanel;
    // ---
    MainFrm_Spacer: TBevel;
    MainFrm_Bevel:  TBevel;
    MainFrm_BevelFirst: TBevel;
    MainFrm_BevelSecond: TBevel;
    MainFrm_BevelThird: TBevel;
    MainFrm_PathBox: TComboBox;
    MainFrm_StatusBar: TStatusBar;
    // Speed Buttons
    MainFrm_BtnUp:  TSpeedButton;
    MainFrm_BtnOpen: TSpeedButton;
    MainFrm_BtnNew: TSpeedButton;
    MainFrm_BtnAdd: TSpeedButton;
    MainFrm_BtnExtract: TSpeedButton;
    MainFrm_BtnView: TSpeedButton;
    MainFrm_BtnDelete: TSpeedButton;
    MainFrm_BtnTest: TSpeedButton;
    MainFrm_BtnInfo: TSpeedButton;
    MainFrm_BtnCheckOut: TSpeedButton;
    MainFrm_BtnHelp: TSpeedButton;
    MainFrm_BtnExit: TSpeedButton;
    // Dialogs
    MainFrm_OpenDialog: TOpenDialog;
    MainFrm_SaveDialog: TSaveDialog;
    MainFrm_FontDialog: TFontDialog;
    // Popup Menu
    MainFrm_PopupMenu: TPopupMenu;
    MainFrm_PopupMenu_N1: TMenuItem;
    MainFrm_PopupMenu_N2: TMenuItem;
    MainFrm_PopupMenu_N3: TMenuItem;
    MainFrm_PopupMenu_Open: TMenuItem;
    MainFrm_PopupMenu_Open_IntViewer: TMenuItem;
    MainFrm_PopupMenu_Delete: TMenuItem;
    MainFrm_PopupMenu_Extract: TMenuItem;
    MainFrm_PopupMenu_ExtractAll: TMenuItem;
    MainFrm_PopupMenu_Test: TMenuItem;
    MainFrm_PopupMenu_Rename: TMenuItem;
    // ---
    MainFrm_PopupMenu_OrderBy: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Name: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Type: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Packed: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Size: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Ratio: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Method: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Modified: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Attributes: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Password: TMenuItem;
    MainFrm_PopupMenu_OrderBy_CRC: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Path: TMenuItem;
    MainFrm_PopupMenu_OrderBy_Position: TMenuItem;
    // ---
    MainFrm_PopupMenu_Details: TMenuItem;
    MainFrm_PopupMenu_Details_Name: TMenuItem;
    MainFrm_PopupMenu_Details_Type: TMenuItem;
    MainFrm_PopupMenu_Details_Packed: TMenuItem;
    MainFrm_PopupMenu_Details_Size: TMenuItem;
    MainFrm_PopupMenu_Details_Ratio: TMenuItem;
    MainFrm_PopupMenu_Details_Method: TMenuItem;
    MainFrm_PopupMenu_Details_Modified: TMenuItem;
    MainFrm_PopupMenu_Details_Attributes: TMenuItem;
    MainFrm_PopupMenu_Details_Password: TMenuItem;
    MainFrm_PopupMenu_Details_CRC: TMenuItem;
    MainFrm_PopupMenu_Details_Path: TMenuItem;
    MainFrm_PopupMenu_Details_Position: TMenuItem;
    // Main Menu
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
    MainFrm_MainMenu_View: TMenuItem;
    MainFrm_MainMenu_View_N1: TMenuItem;
    MainFrm_MainMenu_View_N2: TMenuItem;
    MainFrm_MainMenu_View_N3: TMenuItem;

    MainFrm_MainMenu_View_ToolBar: TMenuItem;
    MainFrm_MainMenu_View_ToolBar_Btns: TMenuItem;
    MainFrm_MainMenu_View_ToolBar_AddressBar: TMenuItem;
    // ---
    MainFrm_MainMenu_View_StatusBar: TMenuItem;
    MainFrm_MainMenu_View_Icons: TMenuItem;
    MainFrm_MainMenu_View_SmallIcons: TMenuItem;
    MainFrm_MainMenu_View_List: TMenuItem;
    MainFrm_MainMenu_View_Report: TMenuItem;
    // ---
    MainFrm_MainMenu_View_RowSelect: TMenuItem;
    MainFrm_MainMenu_View_GridLines: TMenuItem;
    MainFrm_MainMenu_View_ListMode: TMenuItem;
    // ---
    MainFrm_MainMenu_View_OrderBy: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Name: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Type: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Packed: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Size: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Ratio: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Method: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Modified: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Attributes: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Password: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_CRC: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Path: TMenuItem;
    MainFrm_MainMenu_View_OrderBy_Position: TMenuItem;
    // ---
    MainFrm_MainMenu_View_Details: TMenuItem;
    MainFrm_MainMenu_View_Details_Name: TMenuItem;
    MainFrm_MainMenu_View_Details_Type: TMenuItem;
    MainFrm_MainMenu_View_Details_Packed: TMenuItem;
    MainFrm_MainMenu_View_Details_Size: TMenuItem;
    MainFrm_MainMenu_View_Details_Ratio: TMenuItem;
    MainFrm_MainMenu_View_Details_Method: TMenuItem;
    MainFrm_MainMenu_View_Details_Modified: TMenuItem;
    MainFrm_MainMenu_View_Details_Attributes: TMenuItem;
    MainFrm_MainMenu_View_Details_Password: TMenuItem;
    MainFrm_MainMenu_View_Details_CRC: TMenuItem;
    MainFrm_MainMenu_View_Details_Path: TMenuItem;
    MainFrm_MainMenu_View_Details_Position: TMenuItem;
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
    MainFrm_MainMenu_Actions_UnselectMask: TMenuItem;
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
    // ---
    MainFrm_PopupMenu_Buttons: TPopupMenu;
    MainFrm_PopupMenu_Buttons_New: TMenuItem;
    MainFrm_PopupMenu_Buttons_Open: TMenuItem;
    MainFrm_PopupMenu_Buttons_N1: TMenuItem;
    MainFrm_PopupMenu_Buttons_Add: TMenuItem;
    MainFrm_PopupMenu_Buttons_Extract: TMenuItem;
    MainFrm_PopupMenu_Buttons_View: TMenuItem;
    MainFrm_PopupMenu_Buttons_Delete: TMenuItem;
    MainFrm_PopupMenu_Buttons_N2: TMenuItem;
    MainFrm_PopupMenu_Buttons_Test: TMenuItem;
    MainFrm_PopupMenu_Buttons_Info: TMenuItem;
    MainFrm_PopupMenu_Buttons_CheckOut: TMenuItem;
    MainFrm_PopupMenu_Buttons_N3: TMenuItem;
    MainFrm_PopupMenu_Buttons_Help: TMenuItem;
    MainFrm_PopupMenu_Buttons_Exit: TMenuItem;
    MainFrm_ListView: TListView;
    MainFrm_Messages: TComboBox;
    MainFrm_ProgressBar: TProgressBar;
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    // ---
    procedure MainFrm_MainMenu_File_NewClick(Sender: TObject);
    procedure MainFrm_MainMenu_File_OpenClick(Sender: TObject);
    procedure MainFrm_MainMenu_File_CloseClick(Sender: TObject);
    procedure MainFrm_MainMenu_File_InfoClick(Sender: TObject);
    procedure MainFrm_MainMenu_File_MoveClick(Sender: TObject);
    procedure MainFrm_MainMenu_File_CopyClick(Sender: TObject);
    procedure MainFrm_MainMenu_File_RenameClick(Sender: TObject);
    procedure MainFrm_MainMenu_File_DeleteClick(Sender: TObject);
    procedure MainFrm_MainMenu_File_ExitClick(Sender: TObject);
    // ---
    procedure MainFrm_MainMenu_View_ToolBar_BtnsClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_ToolBar_AddressBarClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_StatusBarClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_IconsClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_SmallIconsClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_ListClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_ReportClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_RowSelectClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_GridLinesClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_ListModeClick(Sender: TObject);
    // ---
    procedure MainFrm_MainMenu_View_OrderBy_NameClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_TypeClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_PackedClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_SizeClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_RatioClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_MethodClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_ModifiedClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_AttributesClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_PasswordClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_CRCClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_PathClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_PositionClick(Sender: TObject);
    // ---
    procedure MainFrm_MainMenu_View_Details_NameClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_TypeClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_PackedClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_SizeClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_RatioClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_MethodClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_ModifiedClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_AttributesClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_PasswordClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_CRCClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_PathClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_PositionClick(Sender: TObject);
    // ---
    procedure MainFrm_MainMenu_Actions_AddClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_DeleteClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ExtractClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ExtractAllClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_TestClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_RenameClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ViewClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_InvertClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_SelectAllClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_SelectMaskClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_UnselectMaskClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_TestAllClick(Sender: TObject);
    procedure MainFrm_MainMenu_Actions_CheckOutClick(Sender: TObject);
    // ---
    procedure MainFrm_MainMenu_Options_ConfigClick(Sender: TObject);
    procedure MainFrm_MainMenu_Options_SaveNowClick(Sender: TObject);
    procedure MainFrm_MainMenu_Options_DefaultClick(Sender: TObject);
    procedure MainFrm_MainMenu_Options_PasswordClick(Sender: TObject);
    procedure MainFrm_MainMenu_Options_LogReportClick(Sender: TObject);
    procedure MainFrm_MainMenu_Options_SaveOnExitClick(Sender: TObject);
    // ---
    procedure MainFrm_MainMenu_Help_F1Click(Sender: TObject);
    procedure MainFrm_MainMenu_Help_InternetClick(Sender: TObject);
    procedure MainFrm_MainMenu_Help_LicenseClick(Sender: TObject);
    procedure MainFrm_MainMenu_Help_AboutClick(Sender: TObject);
    // ---
    procedure MainFrm_StatusBarResize(Sender: TObject);
    procedure MainFrm_StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);

    procedure MainFrm_PopupMenuPopup(Sender: TObject);
    procedure MainFrm_PopupMenu_Open_IntViewerClick(Sender: TObject);
    // ---
    procedure MainFrm_PathBoxClick(Sender: TObject);
    procedure MainFrm_ListViewClick(Sender: TObject);
    procedure MainFrm_ListViewColumnClick(Sender: TObject; Column: TListColumn);
    // ---
    procedure MainFrm_DragFilesTrgDrop(Sender: TObject);
    procedure MainFrm_DragFilesSrcDropping(Sender: TObject);
    // ---
    procedure MainFrm_ListViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure MainFrm_ListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    // ---
    procedure MainFrm_PopupMenu_Buttons_NewClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_OpenClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_N1Click(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_AddClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_ExtractClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_ViewClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_DeleteClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_N2Click(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_TestClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_InfoClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_CheckOutClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_N3Click(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_HelpClick(Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_ExitClick(Sender: TObject);
    // ---
    procedure MainFrm_BtnUpClick(Sender: TObject);
  private
    FDragStartPos: TPoint;
    FConfigFrm: TConfigFrm;
    FFormStorage: TFormStorage;
    FDragFilesTrg: TDragFilesTrg;
    FDragFilesSrc: TDragFilesSrc;
    FArchiveListManager: TArchiveListManager;
  private
    procedure MainFrm_DeleteDirectories;
    function MainFrm_AddFile(const FileName: string): boolean;
    function MainFrm_AddFiles(EnableFilesList: boolean): boolean;
    // ---
    function MainFrm_ProcessFiles(var Params: TStringList): integer;
    procedure MainFrm_ConfigFrmShow(PageIndex: integer);
    // ---
    procedure MainFrm_UpdateDetails
      (Column: TArchiveListColumns; ChangeStatus: boolean);
    // ---
    procedure MainFrm_UpdateOrder
      (Column: TArchiveListColumns; ChangeStatus: boolean);
    // ---
    procedure MainFrm_UpdateButtons(Value: boolean); overload;
    procedure MainFrm_UpdateStyle(ViewStyle: TViewStyle);
    procedure MainFrm_UpdateCursor(Value: TCursor);
    procedure MainFrm_UpdateButtons; overload;
    // ---
    function MainFrm_MainAppRun: TMainAppRunRes;
  public
    MainApp: TMainApp;
    MainApp_FileMasks: TStringList;
    procedure MainApp_ArcName(const ArcName: string);
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

uses
  Bee_App,
  // ---
  BeeGui_ZipApp,
  // ---
  BeeGui_SysUtils,
  // ---
  BeeGui_MainFrmRes,
  // ---
  BeeGui_AddFrm,
  BeeGui_TaskFrm,
  BeeGui_InfoFrm,
  BeeGui_AboutFrm,
  BeeGui_SelectFrm,
  BeeGui_SelectFrmRes,
  BeeGui_RenameFrm,
  BeeGui_ExtractFrm,
  BeeGui_PasswordFrm,
  BeeGui_OverWriteFrm,
  BeeGui_IntViewerFrm;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  MainFrm                                                               //
 //                                                                        //
 // ---------------------------------------------------------------------- //

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  MainApp := TMainApp.Create;
  MainApp_FileMasks := nil;
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SMainFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SMainFrm_LanguageS1 + _SMainFrm_LanguageS2;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SMainFrm_LanguageS1 + _SMainFrm_LanguageS2;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
  MainFrm_ProgressBar.Parent := MainFrm_StatusBar;
  // ---
  FConfigFrm := TConfigFrm.Create(Self);
  // ---
  FArchiveListManager :=
    TArchiveListManager.Create(MainFrm_ListView, MainFrm_PathBox);
  // ---
  FDragFilesTrg := TDragFilesTrg.Create(Self);
  FDragFilesTrg.OnDrop := MainFrm_DragFilesTrgDrop;
  FDragFilesTrg.Target := MainFrm_ListView;
  FDragFilesTrg.Enabled := True;
  // ---
  FDragFilesSrc := TDragFilesSrc.Create(Self);
  FDragFilesSrc.OnDropping := MainFrm_DragFilesSrcDropping;
  FDragFilesSrc.DropEffect := deMove;
  // ---
  MainFrm_UpdateStyle(MainFrm_ListView.ViewStyle);
  MainFrm_UpdateDetails(csName, False);
  MainFrm_UpdateOrder(csName, False);
  MainFrm_UpdateCursor(crDefault);
  MainFrm_UpdateButtons(False);
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  MainApp.Destroy;
  // ---
  MainFrm_DeleteDirectories;
  // ---
  if MainFrm_MainMenu_Options_SaveOnExit.Checked then
  begin
    if WindowState = wsNormal then
      FFormStorage.Properties := _SMainFrm_PropertyFull
    else
      FFormStorage.Properties := _SMainFrm_Property;
    FFormStorage.Save(GetApplicationSettingFileName);
  end;
  FreeAndNil(FConfigFrm);
  FreeAndNil(FFormStorage);
  FreeAndNil(FArchiveListManager);
  // ---
  FDragFilesTrg.Enabled := False;
  FreeAndNil(FDragFilesTrg);
  FreeAndNil(FDragFilesSrc);
end;

procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := (MainFrm_MainAppRun = rfNone);
end;

procedure TMainFrm.MainFrm_UpdateOrder
  (Column: TArchiveListColumns; ChangeStatus: boolean);
var
  I: integer;
begin
  if ChangeStatus then
  begin
    MainFrm_MainMenu_View_OrderBy_Name.Checked := (Column = csName);
    MainFrm_MainMenu_View_OrderBy_Size.Checked := (Column = csSize);
    MainFrm_MainMenu_View_OrderBy_Packed.Checked := (Column = csPacked);
    MainFrm_MainMenu_View_OrderBy_Ratio.Checked := (Column = csRatio);
    MainFrm_MainMenu_View_OrderBy_Type.Checked := (Column = csType);
    MainFrm_MainMenu_View_OrderBy_Modified.Checked := (Column = csTime);
    MainFrm_MainMenu_View_OrderBy_Attributes.Checked := (Column = csAttr);
    MainFrm_MainMenu_View_OrderBy_Method.Checked := (Column = csMethod);
    MainFrm_MainMenu_View_OrderBy_Password.Checked := (Column = csPassword);
    MainFrm_MainMenu_View_OrderBy_CRC.Checked  := (Column = csCRC);
    MainFrm_MainMenu_View_OrderBy_Path.Checked := (Column = csPath);
    MainFrm_MainMenu_View_OrderBy_Position.Checked := (Column = csPosition);
  end else
  begin
    for I := 0 to MainFrm_MainMenu_View_OrderBy.Count - 1 do
    begin
      if MainFrm_MainMenu_View_OrderBy.Items[I].Checked then
      begin
        Column := TArchiveListColumns(I);
        Break;
      end;
    end;
  end;

  for I := 0 to MainFrm_PopupMenu_OrderBy.Count - 1 do
  begin
    MainFrm_PopupMenu_OrderBy.Items[I].Checked :=
      MainFrm_MainMenu_View_OrderBy.Items[I].Checked;
  end;
  FArchiveListManager.ColumnSort := Column;
end;

procedure TMainFrm.MainFrm_UpdateDetails
  (Column: TArchiveListColumns; ChangeStatus: boolean);
var
  I: integer;
begin
  if ChangeStatus then
  begin
    with MainFrm_MainMenu_View_Details.Items[Ord(Column)] do
    begin
      Checked := not Checked;
    end;
  end;
  for I := 0 to MainFrm_MainMenu_View_Details.Count - 1 do
  begin
    MainFrm_PopupMenu_Details.Items[I].Checked :=
      MainFrm_MainMenu_View_Details.Items[I].Checked;
    if MainFrm_MainMenu_View_Details.Items[I].Checked then
    begin
      if MainFrm_ListView.Columns[I].Width < 20 then
      begin
        MainFrm_ListView.Columns[I].MaxWidth := 0;
        MainFrm_ListView.Columns[I].MinWidth := 0;
        MainFrm_ListView.Columns[I].Width := 60;
      end;
    end else
    begin
      MainFrm_ListView.Columns[I].MaxWidth := 1;
      MainFrm_ListView.Columns[I].MinWidth := 0;
      MainFrm_ListView.Columns[I].Width := 0;
    end;
  end;
end;

procedure TMainFrm.MainFrm_UpdateStyle(ViewStyle: TViewStyle);
begin
  MainFrm_MainMenu_View_Icons.Checked := (ViewStyle = vsIcon);
  MainFrm_MainMenu_View_SmallIcons.Checked := (ViewStyle = vsSmallIcon);
  MainFrm_MainMenu_View_Report.Checked := (ViewStyle = vsReport);
  MainFrm_MainMenu_View_List.Checked := (ViewStyle = vsList);
  // ---
  MainFrm_ListView.ViewStyle := ViewStyle;
end;

procedure TMainFrm.MainFrm_UpdateButtons;
const
  TopMargin  = 4;
  LeftMargin = 4;
var
  I: integer;
  TopPos: integer;
  LeftPos: integer;
  Btns: array [0..10] of TControl;
begin
  TopPos  := TopMargin;
  LeftPos := LeftMargin;

  MainFrm_BtnNew      .Visible := MainFrm_PopupMenu_Buttons_New     .Checked;
  MainFrm_BtnOpen     .Visible := MainFrm_PopupMenu_Buttons_Open    .Checked;
  MainFrm_BevelFirst  .Visible := MainFrm_PopupMenu_Buttons_N1      .Checked;
  MainFrm_BtnAdd      .Visible := MainFrm_PopupMenu_Buttons_Add     .Checked;
  MainFrm_BtnExtract  .Visible := MainFrm_PopupMenu_Buttons_Extract .Checked;
  MainFrm_BtnView     .Visible := MainFrm_PopupMenu_Buttons_View    .Checked;
  MainFrm_BtnDelete   .Visible := MainFrm_PopupMenu_Buttons_Delete  .Checked;
  MainFrm_BevelSecond .Visible := MainFrm_PopupMenu_Buttons_N2      .Checked;
  MainFrm_BtnTest     .Visible := MainFrm_PopupMenu_Buttons_Test    .Checked;
  MainFrm_BtnInfo     .Visible := MainFrm_PopupMenu_Buttons_Info    .Checked;
  MainFrm_BtnCheckOut .Visible := MainFrm_PopupMenu_Buttons_CheckOut.Checked;
  MainFrm_BevelThird  .Visible := MainFrm_PopupMenu_Buttons_N3      .Checked;
  MainFrm_BtnHelp     .Visible := MainFrm_PopupMenu_Buttons_Help    .Checked;
  MainFrm_BtnExit     .Visible := MainFrm_PopupMenu_Buttons_Exit    .Checked;

  MainFrm_BtnNew.Top  := TopPos;
  MainFrm_BtnNew.Left := LeftPos;
  if MainFrm_BtnNew.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnNew.Width);

  MainFrm_BtnOpen.Top  := TopPos;
  MainFrm_BtnOpen.Left := LeftPos;
  if MainFrm_BtnOpen.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnOpen.Width);

  MainFrm_BevelFirst.Top  := TopPos;
  MainFrm_BevelFirst.Left := LeftPos;
  if MainFrm_BevelFirst.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BevelFirst.Width);

  MainFrm_BtnAdd.Top  := TopPos;
  MainFrm_BtnAdd.Left := LeftPos;
  if MainFrm_BtnAdd.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnAdd.Width);

  MainFrm_BtnExtract.Top  := TopPos;
  MainFrm_BtnExtract.Left := LeftPos;
  if MainFrm_BtnExtract.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnExtract.Width);

  MainFrm_BtnView.Top  := TopPos;
  MainFrm_BtnView.Left := LeftPos;
  if MainFrm_BtnView.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnView.Width);

  MainFrm_BtnDelete.Top  := TopPos;
  MainFrm_BtnDelete.Left := LeftPos;
  if MainFrm_BtnDelete.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnDelete.Width);

  MainFrm_BevelSecond.Top  := TopPos;
  MainFrm_BevelSecond.Left := LeftPos;
  if MainFrm_BevelSecond.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BevelSecond.Width);

  MainFrm_BtnTest.Top  := TopPos;
  MainFrm_BtnTest.Left := LeftPos;
  if MainFrm_BtnTest.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnTest.Width);

  MainFrm_BtnInfo.Top  := TopPos;
  MainFrm_BtnInfo.Left := LeftPos;
  if MainFrm_BtnInfo.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnInfo.Width);

  MainFrm_BtnCheckOut.Top  := TopPos;
  MainFrm_BtnCheckOut.Left := LeftPos;
  if MainFrm_BtnCheckOut.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnCheckOut.Width);

  MainFrm_BevelThird.Top  := TopPos;
  MainFrm_BevelThird.Left := LeftPos;
  if MainFrm_BevelThird.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BevelThird.Width);

  MainFrm_BtnHelp.Top  := TopPos;
  MainFrm_BtnHelp.Left := LeftPos;
  if MainFrm_BtnHelp.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnHelp.Width);

  MainFrm_BtnExit.Top  := TopPos;
  MainFrm_BtnExit.Left := LeftPos;
  if MainFrm_BtnExit.Visible then
    Inc(LeftPos, LeftMargin + MainFrm_BtnExit.Width);

  if not (LeftPos = LeftMargin) then
  begin
    Inc(TopPos, TopMargin + MainFrm_BtnExit.Height);
  end;

  MainFrm_StatusBar.Visible := MainFrm_MainMenu_View_StatusBar.Checked;

  if MainFrm_MainMenu_View_ListMode.Checked then
    MainFrm_MainMenu_View_ToolBar_AddressBar.Enabled := False
  else
    MainFrm_MainMenu_View_ToolBar_AddressBar.Enabled := True;

  with MainFrm_MainMenu_View_ToolBar_AddressBar do
  begin
    MainFrm_BtnUp.Visible := Checked and Enabled;
  end;

  MainFrm_PathBox.Visible := MainFrm_BtnUp.Visible;

  MainFrm_ListView.RowSelect := MainFrm_MainMenu_View_RowSelect.Checked;
  MainFrm_ListView.GridLines := MainFrm_MainMenu_View_GridLines.Checked;
  FArchiveListManager.ListMode := MainFrm_MainMenu_View_ListMode.Checked;

  MainFrm_Bevel.Top := TopPos;
  MainFrm_Bevel.Visible :=
    (not (LeftPos = LeftMargin)) and (MainFrm_BtnUp.Visible);

  if MainFrm_Bevel.Visible then
  begin
    Inc(TopPos, TopMargin + MainFrm_Bevel.Height);
  end;

  MainFrm_BtnUp.Top := TopPos;
  MainFrm_PathBox.Top := TopPos;
  if MainFrm_PathBox.Visible then
  begin
    Inc(TopPos, TopMargin + MainFrm_PathBox.Height);
  end;

  MainFrm_ButtonsPanel.Height := TopPos;

  if not (MainFrm_ButtonsPanel.Height = TopMargin) then
    MainFrm_ButtonsPanel.Visible := True
  else
    MainFrm_ButtonsPanel.Visible := False;
  // ---

  Btns[ 0] := MainFrm_BtnNew;
  Btns[ 1] := MainFrm_BtnOpen;
  Btns[ 2] := MainFrm_BtnAdd;
  Btns[ 3] := MainFrm_BtnExtract;
  Btns[ 4] := MainFrm_BtnView;
  Btns[ 5] := MainFrm_BtnDelete;
  Btns[ 6] := MainFrm_BtnTest;
  Btns[ 7] := MainFrm_BtnInfo;
  Btns[ 8] := MainFrm_BtnCheckOut;
  Btns[ 9] := MainFrm_BtnHelp;
  Btns[10] := MainFrm_BtnExit;
  
  FConfigFrm.ToolBar_Skin_Buttons_Load(Btns);
end;

procedure TMainFrm.MainApp_ArcName(const ArcName: string);
begin
  if Length(ArcName) > 0 then
  begin
    MainFrm.Caption :=
      ApplicationName + ' - ' + ExtractFileName(ArcName);

    MainFrm.FArchiveListManager.FolderBoxSign :=
      AnsiIncludeTrailingBackslash(ExtractFileName(ArcName));

    MainApp.AppArcName := ArcName;
  end;
end;

procedure TMainFrm.MainFrm_UpdateButtons(Value: boolean);
var
  I: integer;
begin
  MainFrm_MainMenu_File_New   .Enabled := True;
  MainFrm_MainMenu_File_Open  .Enabled := True;
  MainFrm_MainMenu_File_Close .Enabled := Value;
  MainFrm_MainMenu_File_Info  .Enabled := Value;
  MainFrm_MainMenu_File_Move  .Enabled := Value;
  MainFrm_MainMenu_File_Copy  .Enabled := Value;
  MainFrm_MainMenu_File_Rename.Enabled := Value;
  MainFrm_MainMenu_File_Delete.Enabled := Value;
  MainFrm_MainMenu_File_Exit  .Enabled := True;

  MainFrm_MainMenu_Actions_Add       .Enabled := (MainApp.CmdIsSupported('a') and Value);
  MainFrm_MainMenu_Actions_Delete    .Enabled := (MainApp.CmdIsSupported('d') and Value);
  MainFrm_MainMenu_Actions_Extract   .Enabled := (MainApp.CmdIsSupported('e') and Value);
  MainFrm_MainMenu_Actions_ExtractAll.Enabled := (MainApp.CmdIsSupported('e') and Value);
  MainFrm_MainMenu_Actions_Test      .Enabled := (MainApp.CmdIsSupported('t') and Value);
  MainFrm_MainMenu_Actions_Rename    .Enabled := (MainApp.CmdIsSupported('r') and Value);
  MainFrm_MainMenu_Actions_View      .Enabled := (MainApp.CmdIsSupported('e') and Value);
  MainFrm_MainMenu_Actions_CheckOut  .Enabled := (MainApp.CmdIsSupported('e') and Value);
  MainFrm_MainMenu_Actions_TestAll   .Enabled := (MainApp.CmdIsSupported('t') and Value);

  MainFrm_BtnNew     .Enabled := MainFrm_MainMenu_File_New        .Enabled;
  MainFrm_BtnOpen    .Enabled := MainFrm_MainMenu_File_Open       .Enabled;
  MainFrm_BtnInfo    .Enabled := MainFrm_MainMenu_File_Info       .Enabled;
  MainFrm_BtnAdd     .Enabled := MainFrm_MainMenu_Actions_Add     .Enabled;
  MainFrm_BtnExtract .Enabled := MainFrm_MainMenu_Actions_Extract .Enabled;
  MainFrm_BtnView    .Enabled := MainFrm_MainMenu_Actions_View    .Enabled;
  MainFrm_BtnDelete  .Enabled := MainFrm_MainMenu_Actions_Delete  .Enabled;
  MainFrm_BtnTest    .Enabled := MainFrm_MainMenu_Actions_Test    .Enabled;
  MainFrm_BtnCheckOut.Enabled := MainFrm_MainMenu_Actions_CheckOut.Enabled;
  MainFrm_BtnHelp    .Enabled := True;
  MainFrm_BtnExit    .Enabled := True;

  MainFrm_MainMenu_Actions_SelectAll    .Enabled := Value;
  MainFrm_MainMenu_Actions_SelectMask   .Enabled := Value;
  MainFrm_MainMenu_Actions_UnselectMask .Enabled := Value;
  MainFrm_MainMenu_Actions_Invert       .Enabled := Value;

  MainFrm_BtnUp.Enabled := Value;
  MainFrm_PathBox.Enabled := Value;
  MainFrm_ListView.Enabled := Value;

  if Value then
    MainFrm_PathBox.Color := clWindow
  else
    MainFrm_PathBox.Color := clbtnFace;

  MainFrm_ListView.Color := MainFrm_PathBox.Color;
end;

procedure TMainFrm.MainFrm_UpdateCursor(Value: TCursor);
begin
  MainFrm.Cursor := Value;
  MainFrm_PathBox.Cursor := Value;
  MainFrm_ListView.Cursor := Value;
end;

procedure TMainFrm.MainFrm_DeleteDirectories;
begin
  if BeeGui_SysUtils.DirectoryExists(GetApplicationTempDir) then
  begin
    BeeGui_SysUtils.DeleteDirectory(GetApplicationTempDir);
  end;

  if BeeGui_SysUtils.DirectoryExists(GetApplicationCheckOutDir) then
  begin
    if MessageDlg(MainFrm_Messages.Items[_CMainFrm_DeleteCheckoutDir],
      mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      BeeGui_SysUtils.DeleteDirectory(GetApplicationCheckOutDir);
    end;
  end;
end;

function TMainFrm.MainFrm_MainAppRun: TMainAppRunRes;
var
  f: TTaskFrm;
  Suspended: boolean;
begin
  Result := rfNone;
  if (MainFrm.Cursor = crHourGlass) then
  begin
    f := TTaskFrm.Create(Self);
    if (MainApp.FApp.Suspended) then
      f.SetAction(0)
    else
      f.SetAction(1);

    f.SetGeneralPriority(MainApp.FApp.Priority);
    if (f.ShowModal = mrOk) then
    begin
      if (MainFrm.Cursor = crHourGlass) then
      begin
        case f.TaskFrm_Actions.ItemIndex of
          0: begin
               MainApp.FApp.Priority := f.GetGeneralPriority;
               if MainApp.FApp.Suspended = True then
                 MainApp.FApp.Resume;
               Result := rfContinue;
             end;
          1: begin
               Application.Title := _SAppPausedTitle;
               if MainApp.FApp.Suspended = False then
                 MainApp.FApp.Suspend;
               Result := rfPause;
             end;
          2: begin
               if MainApp.FApp.Suspended = True then
                 MainApp.FApp.Resume;
               MainApp.FApp.Terminate;
               Result := rfAbort;
             end;
          3: begin
               MainApp.FAppTerminateFlags := [tfClose];
               if MainApp.FApp.Suspended = True then
               begin
                 MainApp.FApp.Resume;
               end;
               MainApp.FApp.Terminate;
               Result := rfAbortAndClose;
             end;
        end;
      end else
        Result := rfContinue;
    end else
    begin
      if (MainFrm.Cursor = crHourGlass) then
      begin
        Result := rfContinue;
      end;
    end;
    f.Free;
  end;
end;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  Status Bar                                                            //
 //                                                                        //
 // ---------------------------------------------------------------------- //  

procedure TMainFrm.MainFrm_StatusBarDrawPanel
  (StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
begin
  if Panel.Index = 1 then
    with MainFrm_ProgressBar do
    begin
      Visible := True;
      Left := Rect.Left - 1;
      Top  := Rect.Top - 1;
      Height := (Rect.Bottom - Rect.Top) + 2;
      Width := (Rect.Right - Rect.Left) - 14;
    end;
end;

procedure TMainFrm.MainFrm_StatusBarResize(Sender: TObject);
begin
  MainFrm_StatusBar.Panels[0].Width := MainFrm.Width * 2 div 3;
end;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  MainFrm Routines                                                      //
 //                                                                        //
 // ---------------------------------------------------------------------- //

function TMainFrm.MainFrm_AddFiles(EnableFilesList: boolean): boolean;
var
  I: integer;
  F: TAddFrm;
  MainFrmState: TWindowState;
begin
  F := TAddFrm.Create(Self, MainApp.FAppArcName, FArchiveListManager.CurrFolder);

  F.LoadOptions(FConfigFrm);
  if Assigned(MainApp_FileMasks) then
  begin
    for I := 0 to MainApp_FileMasks.Count - 1 do
    begin
      if BeeGui_SysUtils.DirectoryExists(MainApp_FileMasks.Strings[I]) then
        F.AddToFileList(MainApp_FileMasks.Strings[I], True)
      else
        F.AddToFileList(MainApp_FileMasks.Strings[I], False);
    end;
    
    if MainApp_FileMasks.Count = 1 then
    begin
      if FileExists(MainApp_FileMasks.Strings[0]) then
      begin
        F.AddFrm_rOption.Checked := False;
      end;
    end;
    FreeAndNil(MainApp_FileMasks);
  end;

  F.EnableFileList(EnableFilesList);
  F.AddFrm_cOption.Enabled := not MainFrm_MainMenu_View_ListMode.Checked;

  MainFrmState := MainFrm.WindowState;
  if fConfigFrm.General_Options_HideMainFrm.Checked then
  begin
    MainFrm.WindowState := wsMinimized;
  end;

  MainApp.AppInterfaceClear;
  Result := (F.ShowModal = mrOk);
  if Result then
  begin
    F.SetAppParams(MainApp.FAppParams);

    if Assigned(MainApp.FAppParams) then
    begin
      if (Length(F.AddFrm_Root.Text) = 0) or
        SetCurrentDir(F.AddFrm_Root.Text) then
      begin
        MainApp.Execute;
      end else
        MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnSetCurrDir], mtError, [mbOK], 0);
    end else
      MessageDlg(MainFrm_Messages.Items[_CMainFrm_NoFilesSelected], mtWarning, [mbOK], 0);
  end;
  MainFrm.WindowState := MainFrmState;

  F.Free;
end;

function TMainFrm.MainFrm_AddFile(const FileName: string): boolean;
begin
  MainApp_FileMasks := TStringList.Create;
  MainApp_FileMasks.Add(FileName);
  Result := MainFrm_AddFiles(False);

  if (Result = False) and (tfClose in MainApp.FAppTerminateFlags) then
    Close;
end;

procedure TMainFrm.MainFrm_ConfigFrmShow(PageIndex: integer);
begin
  FConfigFrm.Buttons := MainFrm_PopupMenu_Buttons;
  FConfigFrm.ConfigFrm_Notebook.PageIndex := PageIndex;
  FConfigFrm.ConfigFrm_Tree.Items[PageIndex].Selected := True;

  case FConfigFrm.ShowModal of
    mrCancel:
    begin
    end;
    mrOk:
    begin
      MainFrm_PopupMenu_Buttons := FConfigFrm.Buttons;
      MainFrm_UpdateButtons;
    end;
  end;
end;

function TMainFrm.MainFrm_ProcessFiles;
var
  I: integer;
  Node: TListItem;
begin
  Result := 0;
  for I := 0 to MainFrm_ListView.Items.Count - 1 do
  begin
    if MainFrm_ListView.Items[I].Selected then
    begin
      Node := MainFrm_ListView.Items[I];
      if Length(Node.SubItems[MainFrm_ListView.Columns.Count - 2]) = 0 then
        Params.Add(Node.SubItems[MainFrm_ListView.Columns.Count - 3] +
          Node.Caption + '*!')
      else
        Params.Add(Node.SubItems[MainFrm_ListView.Columns.Count - 3] +
          Node.Caption);

      Inc(Result);
    end;
  end;
end;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  MainFrm_PopupMenu_Buttons Click                                       //
 //                                                                        //
 // ---------------------------------------------------------------------- //

procedure TMainFrm.MainFrm_PopupMenu_Buttons_NewClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_New.Checked :=
    not MainFrm_PopupMenu_Buttons_New.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_OpenClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_Open.Checked :=
    not MainFrm_PopupMenu_Buttons_Open.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_N1Click(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_N1.Checked :=
    not MainFrm_PopupMenu_Buttons_N1.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_AddClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_Add.Checked :=
    not MainFrm_PopupMenu_Buttons_Add.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_ExtractClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_Extract.Checked :=
    not MainFrm_PopupMenu_Buttons_Extract.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_ViewClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_View.Checked :=
    not MainFrm_PopupMenu_Buttons_View.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_DeleteClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_Delete.Checked :=
    not MainFrm_PopupMenu_Buttons_Delete.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_N2Click(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_N2.Checked :=
    not MainFrm_PopupMenu_Buttons_N2.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_TestClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_Test.Checked :=
    not MainFrm_PopupMenu_Buttons_Test.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_InfoClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_Info.Checked :=
    not MainFrm_PopupMenu_Buttons_Info.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_CheckOutClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_CheckOut.Checked :=
    not MainFrm_PopupMenu_Buttons_CheckOut.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_N3Click(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_N3.Checked :=
    not MainFrm_PopupMenu_Buttons_N3.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_HelpClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_Help.Checked :=
    not MainFrm_PopupMenu_Buttons_Help.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_PopupMenu_Buttons_ExitClick(Sender: TObject);
begin
  MainFrm_PopupMenu_Buttons_Exit.Checked :=
    not MainFrm_PopupMenu_Buttons_Exit.Checked;
  MainFrm_UpdateButtons;
end;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  MainFrm_MainMenu Click                                                //
 //                                                                        //
 // ---------------------------------------------------------------------- //

/// TMainFrm.MainFrm_MainMenu_File

procedure TMainFrm.MainFrm_MainMenu_File_NewClick(Sender: TObject);
begin
  case MainFrm_MainAppRun of
    rfNone:
    begin
      MainFrm_SaveDialog.FileName := '';
      if MainFrm_SaveDialog.Execute then
      begin
        MainFrm_MainMenu_File_Close.Click;
        MainApp.FAppArcName := MainFrm_SaveDialog.FileName;

        case MainFrm_SaveDialog.FilterIndex of
          1: MainApp.FAppArcName :=
              ChangeFileExt(MainApp.FAppArcName, '.bee');
          2: MainApp.FAppArcName :=
              ChangeFileExt(MainApp.FAppArcName, '.zip');
          3: MainApp.FAppArcName :=
              ChangeFileExt(MainApp.FAppArcName, '.tar');
          4: MainApp.FAppArcName :=
              ChangeFileExt(MainApp.FAppArcName, '.exe');
        end;

        Caption :=
          Application.Title + ' - ' + ExtractFileName(MainApp.FAppArcName);

        FArchiveListManager.FolderBoxSign :=
          IncludeTrailingBackslash(ExtractFileName(MainApp.FAppArcName));

        MainApp.AppInterfaceClear;
        MainApp.FAppParams.Add('l');
        MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
        MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
        MainApp.FAppParams.Add(MainApp.FAppArcName);
        MainApp.FAppParams.Add('*!');

        MainApp.FAppTerminateFlags := [tfAdd];
        MainApp.Execute;
      end;
    end;
    rfAbort: Include(MainApp.FAppTerminateFlags, tfNew);
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_File_OpenClick(Sender: TObject);
begin
  case MainFrm_MainAppRun of
    rfNone:
    begin
      MainFrm_OpenDialog.FileName := '';
      if MainFrm_OpenDialog.Execute then
      begin
        MainFrm_MainMenu_File_Close.Click;
        MainApp.FAppArcName := MainFrm_OpenDialog.FileName;

        Caption :=
          Application.Title + ' - ' + ExtractFileName(MainApp.FAppArcName);

        FArchiveListManager.FolderBoxSign :=
          IncludeTrailingBackslash(ExtractFileName(MainApp.FAppArcName));

        MainApp.AppInterfaceClear;
        MainApp.FAppParams.Add('l');
        MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
        MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
        MainApp.FAppParams.Add(MainApp.FAppArcName);
        MainApp.FAppParams.Add('*!');

        MainApp.FAppTerminateFlags := [];
        MainApp.Execute;
      end;
    end;
    rfAbort: Include(MainApp.FAppTerminateFlags, tfOpen);
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_File_CloseClick(Sender: TObject);
begin
  Application.Title := _SAppTitle;
  Caption := _SAppTitle;
  // ---
  MainApp.FAppLog.Clear;
  FArchiveListManager.Clear;
  SetLength(MainApp.FAppArcName, 0);
  // ---
  MainFrm_DeleteDirectories;
  MainFrm_UpdateButtons(False);
  MainFrm_StatusBar.Panels[0].Text := ('');
end;

procedure TMainFrm.MainFrm_MainMenu_File_InfoClick(Sender: TObject);
var
  f: TInfoFrm;
begin
  f := TInfoFrm.Create(Self);
  if f.UpdateInfo(MainApp.FAppArcName, FArchiveListManager.Details) then
    f.ShowModal
  else
    MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnLoadArcInfo],
      mtInformation, [mbOK], 0);

  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_File_MoveClick(Sender: TObject);
var
  MoveTo:  string;
  NewName: string;
begin
  SetLength(MoveTo, 0);
  SelectDirectory(MainFrm_Messages.Items[_CMainFrm_MoveTo], '', MoveTo);
  if Length(MoveTo) > 0 then
  begin
    NewName :=
      IncludeTrailingBackslash(MoveTo) + ExtractFileName(MainApp.FAppArcName);

    if RenameFile(MainApp.FAppArcName, NewName) then
      MainApp.FAppArcName := NewName
    else
      MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnMoveTo],
        mtError, [mbOK], 0);
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_File_CopyClick(Sender: TObject);
var
  CopyTo:  string;
  NewName: string;
begin
  SetLEngth(CopyTo, 0);
  SelectDirectory(MainFrm_Messages.Items[_CMainFrm_CopyTo], '', CopyTo);
  if Length(CopyTo) > 0 then
  begin
    NewName :=
      IncludeTrailingBackslash(CopyTo) + ExtractFileName(MainApp.FAppArcName);

    if CopyFile(PChar(MainApp.FAppArcName), PChar(NewName)) then
      MainApp.FAppArcName := NewName
    else
      MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnCopyTo],
        mtError, [mbOK], 0);
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_File_RenameClick(Sender: TObject);
var
  f: TRenameFrm;
  NewName: string;
begin
  f := TRenameFrm.Create(Self);
  f.Caption := MainFrm_Messages.Items[_CMainFrm_RenameArc];
  f.RenameFrm_To.Text := ExtractFileName(MainApp.FAppArcName);
  if f.ShowModal = mrOk then
  begin
    if (CompareText(f.RenameFrm_From.Caption, f.RenameFrm_To.Text) <> 0) and
      (Length(f.RenameFrm_To.Text) > 0) then
    begin
      NewName := ExtractFilePath(MainApp.FAppArcName) + f.RenameFrm_To.Text;
      if RenameFile(MainApp.FAppArcName, NewName) then
      begin
        MainFrm_MainMenu_File_Close.Click;
        MainApp.FAppArcName := NewName;

        Caption :=
          _SAppTitle + ' - ' + ExtractFileName(MainApp.FAppArcName);

        FArchiveListManager.FolderBoxSign :=
          IncludeTrailingBackslash(ExtractFileName(MainApp.FAppArcName));

        MainApp.AppInterfaceClear;
        MainApp.FAppParams.Add('l');
        MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
        MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
        MainApp.FAppParams.Add(MainApp.FAppArcName);
        MainApp.FAppParams.Add('*!');

        MainApp.FAppTerminateFlags := [];
        MainApp.Execute;
      end else
        MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnRenameArc],
          mtError, [mbOK], 0);
    end;
  end;
  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_File_DeleteClick(Sender: TObject);
begin

  if MessageDlg(MainFrm_Messages.Items[_CMainFrm_DeleteArc],
    mtInformation, [mbYes, mbNo], 0) = mrYes then
  begin
    if DeleteFile(MainApp.FAppArcName) then
      MainFrm_MainMenu_File_Close.Click
    else
      MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnDeleteArc],
        mtError, [mbOK], 0);
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_File_ExitClick(Sender: TObject);
begin
  Close;
end;

/// TMainFrm.MainFrm_MainMenu_View

procedure TMainFrm.MainFrm_MainMenu_View_ToolBar_BtnsClick(Sender: TObject);
begin
  MainFrm_ConfigFrmShow(2);
end;

procedure TMainFrm.MainFrm_MainMenu_View_ToolBar_AddressBarClick(Sender: TObject);
begin
  MainFrm_MainMenu_View_ToolBar_AddressBar.Checked :=
    not MainFrm_MainMenu_View_ToolBar_AddressBar.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_MainMenu_View_StatusBarClick(Sender: TObject);
begin
  MainFrm_MainMenu_View_StatusBar.Checked :=
    not MainFrm_MainMenu_View_StatusBar.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_MainMenu_View_RowSelectClick(Sender: TObject);
begin
  MainFrm_MainMenu_View_RowSelect.Checked :=
    not MainFrm_MainMenu_View_RowSelect.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_MainMenu_View_GridLinesClick(Sender: TObject);
begin
  MainFrm_MainMenu_View_GridLines.Checked :=
    not MainFrm_MainMenu_View_GridLines.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_MainMenu_View_ListModeClick(Sender: TObject);
begin
  MainFrm_MainMenu_View_ListMode.Checked :=
    not MainFrm_MainMenu_View_ListMode.Checked;
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_MainMenu_View_IconsClick(Sender: TObject);
begin
  MainFrm_UpdateStyle(vsIcon);
end;

procedure TMainFrm.MainFrm_MainMenu_View_SmallIconsClick(Sender: TObject);
begin
  MainFrm_UpdateStyle(vsSmallIcon);
end;

procedure TMainFrm.MainFrm_MainMenu_View_ListClick(Sender: TObject);
begin
  MainFrm_UpdateStyle(vsList);
end;

procedure TMainFrm.MainFrm_MainMenu_View_ReportClick(Sender: TObject);
begin
  MainFrm_UpdateStyle(vsReport);
end;

/// TMainFrm.MainFrm_MainMenu_View_OrderBy

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_NameClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csName, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_SizeClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csSize, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_PackedClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csPacked, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_RatioClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csRatio, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_TypeClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csType, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_ModifiedClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csTime, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_AttributesClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csAttr, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_MethodClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csMethod, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_PasswordClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csPassword, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_CRCClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csCRC, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_PathClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csPath, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_PositionClick(Sender: TObject);
begin
  MainFrm_UpdateOrder(csPosition, True);
end;

/// TMainFrm.MainFrm_MainMenu_View_Details

procedure TMainFrm.MainFrm_MainMenu_View_Details_NameClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csName, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_SizeClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csSize, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_PackedClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csPacked, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_RatioClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csRatio, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_TypeClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csType, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_ModifiedClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csTime, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_AttributesClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csAttr, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_MethodClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csMethod, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_PasswordClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csPassword, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_CRCClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csCRC, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_PathClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csPath, True);
end;

procedure TMainFrm.MainFrm_MainMenu_View_Details_PositionClick(Sender: TObject);
begin
  MainFrm_UpdateDetails(csPosition, True);
end;

/// TMainFrm.MainFrm_MainMenu_Actions

procedure TMainFrm.MainFrm_MainMenu_Actions_AddClick(Sender: TObject);
begin
  if MainFrm_AddFiles(True) = False then
  begin
    if tfClose in MainApp.FAppTerminateFlags then
      Close;
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_DeleteClick(Sender: TObject);
begin
  if MainFrm_ListView.Selected <> nil then
  begin
    if MessageDlg(MainFrm_Messages.Items[_CMainFrm_Delete_Files],
      mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      MainApp.AppInterfaceClear;
      MainApp.FAppParams.Add('d');
      MainApp.FAppParams.Add('-l');
      MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
      MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
      MainApp.FAppParams.Add(MainApp.FAppArcName);

      MainFrm_ProcessFiles(MainApp.FAppParams);

      MainApp.FAppTerminateFlags := [];
      MainApp.Execute;
    end;
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_ExtractClick(Sender: TObject);
var
  f: TExtractFrm;
  fResult: integer;
begin
  f := TExtractFrm.Create(Self);
  f.GetOptions(FConfigFrm);

  f.ExtractFrm_cOption.Enabled := not MainFrm_MainMenu_View_ListMode.Checked;

  fResult := f.ShowModal;
  if fResult = mrOk then
  begin
    MainApp.AppInterfaceClear;
    MainApp.FAppParams.Add(f.GetCommandStr);
    MainApp.FAppParams.Add(f.GetPriorityStr);
    MainApp.FAppParams.Add(f.GetOverWriteModestr);
    MainApp.FAppParams.Add('-y' + GetApplicationTempDir);

    if f.ExtractFrm_cOption.Enabled and f.ExtractFrm_cOption.Checked then
    begin
      MainApp.FAppParams.Add('-c' + FArchiveListManager.CurrFolder);
    end;

    MainApp.FAppParams.Add(MainApp.FAppArcName);
    if MainFrm_ProcessFiles(MainApp.FAppParams) = 0 then
    begin
      MainApp.FAppParams.Add('*!');
    end;

    MainApp.FAppTerminateFlags := [];
    ForceDirectories(f.ExtractFrm_Folder.Text);

    if SetCurrentDir(f.ExtractFrm_Folder.Text) then
      MainApp.Execute
    else
      MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnSetCurrDir], mtError, [mbOK], 0);
  end;
  f.Free;

  if (fResult <> mrOk) then
    if (tfClose in MainApp.FAppTerminateFlags) then
      Close;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_ExtractAllClick(Sender: TObject);
var
  f: TSelectFrm;
begin
  f := TSelectFrm.Create(Self);
  f.DeSelectAll(MainFrm_ListView);
  f.Free;

  MainFrm_MainMenu_Actions_Extract.Click;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_TestClick(Sender: TObject);
begin
  MainApp.AppInterfaceClear;
  MainApp.FAppParams.Add('t');
  MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
  MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
  MainApp.FAppParams.Add(MainApp.FAppArcName);
  if MainFrm_ProcessFiles(MainApp.FAppParams) = 0 then
  begin
    MainApp.FAppParams.Add('*!');
  end;

  MainApp.FAppTerminateFlags := [tfLog];
  MainApp.Execute;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_RenameClick(Sender: TObject);
var
  f: TRenameFrm;
begin
  if MainFrm_ListView.Selected <> nil then
  begin
    MainApp.AppInterfaceClear;
    MainApp.FAppParams.Add('r');
    MainApp.FAppParams.Add('-l');
    MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
    MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
    MainApp.FAppParams.Add(MainApp.FAppArcName);

    MainFrm_ProcessFiles(MainApp.FAppParams);

    MainApp.FAppRenameDir := False;
    if Length(MainFrm_ListView.Selected.SubItems
      [MainFrm_ListView.Columns.Count - 2]) = 0 then
    begin
      if MainFrm_ListView.SelCount = 1 then
      begin
        f := TRenameFrm.Create(Self);
        f.Caption := MainFrm_Messages.Items[_CMainFrm_RenameFolder];
        f.RenameFrm_To.Text := MainFrm_ListView.Selected.Caption;

        if f.ShowModal = mrOk then
        begin
          MainApp.FAppRenameDir := True;
          MainApp.FAppRenameDirFrom :=
            IncludeTrailingBackslash(MainFrm_ListView.Selected.SubItems
            [MainFrm_ListView.Columns.Count - 3] +
            f.RenameFrm_From.Caption);

          MainApp.FAppRenameDirTo :=
            IncludeTrailingBackslash(MainFrm_ListView.Selected.SubItems
            [MainFrm_ListView.Columns.Count - 3] + f.RenameFrm_To.Text);
        end;
        f.Free;

        if MainApp.FAppRenameDir then
        begin
          MainApp.FAppTerminateFlags := [];
          ;
          MainApp.Execute;
        end;
      end;

    end else
    begin
      MainApp.FAppTerminateFlags := [];
      MainApp.Execute;
    end;
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_ViewClick(Sender: TObject);
begin
  if MainFrm_ListView.Selected <> nil then
  begin
    if Length(MainFrm_ListView.Selected.SubItems[MainFrm_ListView.Columns.Count -
      2]) = 0 then
    begin
      FArchiveListManager.CurrFolder :=
        IncludeTrailingBackslash(MainFrm_PathBox.Items
        [MainFrm_PathBox.ItemIndex] + MainFrm_ListView.Selected.Caption);

      MainFrm_ListViewClick(Self);
      MainFrm_ListView.SetFocus;
    end else
    begin
      MainApp.AppInterfaceClear;
      MainApp.FAppParams.Add('x');
      MainApp.FAppParams.Add('-oA');
      MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
      MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
      MainApp.FAppParams.Add(MainApp.FAppArcName);
      MainFrm_ProcessFiles(MainApp.FAppParams);

      CreateDir(GetApplicationTempDir);
      if SetCurrentDir(GetApplicationTempDir) then
      begin
        MainApp.FAppTerminateFlags := [tfViewExt];
        MainApp.Execute;
      end else
        MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnSetCurrDir],
          mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_SelectAllClick(Sender: TObject);
var
  f: TSelectFrm;
begin
  MainFrm_ListView.SetFocus;
  f := TSelectFrm.Create(Self);
  f.SelectAll(MainFrm_ListView);
  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_SelectMaskClick(Sender: TObject);
var
  f: TSelectFrm;
begin
  MainFrm_ListView.SetFocus;
  f := TSelectFrm.Create(Self);
  f.Caption := f.SelectFrm_Messages.Items[_CSelectFrm_SelectFrmCaption];
  if f.ShowModal = mrOk then
  begin
    f.SelectItems(MainFrm_ListView);
  end;
  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_UnselectMaskClick(Sender: TObject);
var
  f: TSelectFrm;
begin
  MainFrm_ListView.SetFocus;
  f := TSelectFrm.Create(Self);
  f.Caption := f.SelectFrm_Messages.Items[_CSelectFrm_UnselectFrmCaption];
  if f.ShowModal = mrOk then
  begin
    f.DeselectItems(MainFrm_ListView);
  end;
  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_InvertClick(Sender: TObject);
var
  f: TSelectFrm;
begin
  MainFrm_ListView.SetFocus;
  f := TSelectFrm.Create(Self);
  f.InvertSelection(MainFrm_ListView);
  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_CheckOutClick(Sender: TObject);
begin
  MainApp.AppInterfaceClear;
  MainApp.FAppParams.Add('x');
  MainApp.FAppParams.Add('-oA');
  MainApp.FAppParams.Add('-y' + GetApplicationCheckOutDir);
  MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
  MainApp.FAppParams.Add(MainApp.FAppArcName);
  if MainFrm_ProcessFiles(MainApp.FAppParams) = 0 then
  begin
    MainApp.FAppParams.Add('*!');
  end;

  CreateDir(GetApplicationCheckOutDir);
  if SetCurrentDir(GetApplicationCheckOutDir) then
  begin
    MainApp.FAppTerminateFlags := [tfCheckOut];
    MainApp.Execute;
  end else
    MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnCreateCheckOutDir],
      mtError, [mbOK], 0);
end;

procedure TMainFrm.MainFrm_MainMenu_Actions_TestAllClick(Sender: TObject);
begin
  MainApp.AppInterfaceClear;
  MainApp.FAppParams.Add('t');
  MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
  MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
  MainApp.FAppParams.Add(MainApp.FAppArcName);
  MainApp.FAppParams.Add('*!');

  MainApp.FAppTerminateFlags := [tfLog];
  MainApp.Execute;
end;

/// TMainFrm.MainFrm_MainMenu_Options

procedure TMainFrm.MainFrm_MainMenu_Options_ConfigClick(Sender: TObject);
begin
  MainFrm_ConfigFrmShow(0);
end;

procedure TMainFrm.MainFrm_MainMenu_Options_PasswordClick(Sender: TObject);
var
  f: TPasswordFrm;
begin
  f := TPasswordFrm.Create(Self);
  f.SetKey(MainApp.FAppKey);
  if f.ShowModal = mrOk then
  begin
    MainApp.FAppKey := f.PasswordFrm_Key.Text;
  end;
  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_Options_SaveOnExitClick(Sender: TObject);
begin
  MainFrm_MainMenu_Options_SaveOnExit.Checked :=
    not MainFrm_MainMenu_Options_SaveOnExit.Checked;
end;

procedure TMainFrm.MainFrm_MainMenu_Options_SaveNowClick(Sender: TObject);
begin
  if WindowState = wsNormal then
    FFormStorage.Properties := _SMainFrm_PropertyFull
  else
    FFormStorage.Properties := _SMainFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
end;

procedure TMainFrm.MainFrm_MainMenu_Options_DefaultClick(Sender: TObject);
begin
  FConfigFrm.Free;
  // ---
  begin
    DeleteFile(GetApplicationSettingFileName);
    CopyFile(GetApplicationDefSettingFileName,
      GetApplicationSettingFileName);
  end;
  FConfigFrm := TConfigFrm.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SMainFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // ---
  MainFrm_UpdateStyle(MainFrm_ListView.ViewStyle);
  MainFrm_UpdateDetails(csName, False);
  MainFrm_UpdateOrder(csName, False);
  MainFrm_UpdateCursor(crDefault);
  MainFrm_UpdateButtons(False);
  MainFrm_UpdateButtons;
end;

procedure TMainFrm.MainFrm_MainMenu_Options_LogReportClick(Sender: TObject);
begin
  MainFrm_MainMenu_Options_LogReport.Checked :=
    not MainFrm_MainMenu_Options_LogReport.Checked;
end;

/// TMainFrm.MainFrm_MainMenu_Help

procedure TMainFrm.MainFrm_MainMenu_Help_F1Click(Sender: TObject);
begin
  ShellExec(_SMainFrm_HelpFile, ExtractFilePath(ParamStr(0)));
end;

procedure TMainFrm.MainFrm_MainMenu_Help_InternetClick(Sender: TObject);
var
  f: TAboutFrm;
begin
  f := TAboutFrm.Create(Self);
  f.AboutFrm_LinkClick(Self);
  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_Help_LicenseClick(Sender: TObject);
var
  f: TAboutFrm;
begin
  f := TAboutFrm.Create(Self);
  f.BtnLicense.Click;
  f.Free;
end;

procedure TMainFrm.MainFrm_MainMenu_Help_AboutClick(Sender: TObject);
var
  f: TForm;
begin
  f := TAboutFrm.Create(Self);
  f.ShowModal;
  f.Free;
end;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  Others Click                                                          //
 //                                                                        //
 // ---------------------------------------------------------------------- //

procedure TMainFrm.MainFrm_BtnUpClick(Sender: TObject);
begin
  if (FArchiveListManager.Up = False) and
    (FConfigFrm.General_Options_UpAsExit.Checked) then
  begin
    MainFrm_MainMenu_File_Exit.Click;
  end else
  begin
    MainFrm_ListViewClick(Self);
    MainFrm_ListView.SetFocus;
  end;
end;

procedure TMainFrm.MainFrm_PathBoxClick(Sender: TObject);
var
  I: integer;
begin
  I := MainFrm_PathBox.ItemIndex;
  if not (I = -1) then
  begin
    FArchiveListManager.CurrFolder := MainFrm_PathBox.Items[I];
  end;
  MainFrm_ListViewClick(Self);
  MainFrm_ListView.SetFocus;
end;

procedure TMainFrm.MainFrm_ListViewClick(Sender: TObject);
begin
  if MainFrm_ListView.SelCount = 0 then
    MainFrm_StatusBar.Panels[0].Text :=
      Format(MainFrm_Messages.Items[_CMainFrm_Items], [MainFrm_ListView.Items.Count])
  else
  if MainFrm_ListView.SelCount = 1 then
  begin
    if (TArchiveListItem(FArchiveListManager.CurrFiles
      [MainFrm_ListView.Selected.Index]).FileAttr and faDirectory) = 0 then

      MainFrm_StatusBar.Panels[0].Text :=
        MainFrm_ListView.Columns[1].Caption + ': ' +
        MainFrm_ListView.Selected.SubItems[0] + '  ' +
        MainFrm_ListView.Columns[2].Caption + ': ' +
        MainFrm_ListView.Selected.SubItems[1] + '  ' +
        MainFrm_ListView.Columns[3].Caption + ': ' +
        MainFrm_ListView.Selected.SubItems[2]
    else
      MainFrm_StatusBar.Panels[0].Text :=
        Format(MainFrm_Messages.Items[_CMainFrm_ItemsSelected],
        [MainFrm_ListView.SelCount]);
  end else
    MainFrm_StatusBar.Panels[0].Text :=
      Format(MainFrm_Messages.Items[_CMainFrm_ItemsSelected],
      [MainFrm_ListView.SelCount]);
end;

procedure TMainFrm.MainFrm_ListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  MainFrm_UpdateOrder(TArchiveListColumns(Column.Index), True);
end;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  MainFrm_PopupMenu                                                     //
 //                                                                        //
 // ---------------------------------------------------------------------- //

procedure TMainFrm.MainFrm_PopupMenuPopup(Sender: TObject);
var
  b: bool;
  I: integer;
begin
  if MainFrm_ListView.SelCount = 0 then
    b := False
  else
    b := True;

  for I := 0 to 9 do
  begin
    MainFrm_PopupMenu.Items[I].Visible := b;
  end;
end;

procedure TMainFrm.MainFrm_PopupMenu_Open_IntViewerClick(Sender: TObject);
begin
  if (MainFrm.Cursor <> crHourGlass) then
  begin
    if MainFrm_ListView.Selected <> nil then
    begin
      if Length(MainFrm_ListView.Selected.SubItems
        [MainFrm_ListView.Columns.Count - 2]) = 0 then
      begin
        FArchiveListManager.CurrFolder :=
          IncludeTrailingBackslash(MainFrm_PathBox.Items
          [MainFrm_PathBox.ItemIndex] + MainFrm_ListView.Selected.Caption);

        MainFrm_ListViewClick(Self);
        MainFrm_ListView.SetFocus;
      end else
      begin
        MainApp.AppInterfaceClear;
        MainApp.FAppParams.Add('x');
        MainApp.FAppParams.Add('-oA');
        MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
        MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
        MainApp.FAppParams.Add(MainApp.FAppArcName);
        MainFrm_ProcessFiles(MainApp.FAppParams);

        CreateDir(GetApplicationTempDir);
        if SetCurrentDir(GetApplicationTempDir) then
        begin
          MainApp.FAppTerminateFlags := [tfViewInt];
          MainApp.Execute;
        end else
          MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnSetCurrDir],
            mtError, [mbOK], 0);
      end;
    end;
  end;
end;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  Drag & Drop features                                                  //
 //                                                                        //
 // ---------------------------------------------------------------------- //

procedure TMainFrm.MainFrm_DragFilesTrgDrop(Sender: TObject);
begin
  if MainFrm_ListView.Cursor <> crHourGlass then
  begin
    MainApp_FileMasks := TStringList.Create;
    MainApp_FileMasks.AddStrings(FDragFilesTrg.FileList);
    MainFrm_AddFiles(True);
  end;
end;

procedure TMainFrm.MainFrm_DragFilesSrcDropping(Sender: TObject);
begin
  if (MainFrm.Cursor <> crHourGlass) then
  begin
    MainApp.AppInterfaceClear;
    MainApp.FAppParams.Add('x');
    MainApp.FAppParams.Add('-oA');
    MainApp.FAppParams.Add('-y' + GetApplicationTempDir);
    MainApp.FAppParams.Add(fConfigFrm.GetPriorityStr);
    MainApp.FAppParams.Add(MainApp.FAppArcName);
    MainFrm_ProcessFiles(MainApp.FAppParams);

    CreateDir(GetApplicationTempDir);
    if SetCurrentDir(GetApplicationTempDir) then
    begin
      MainApp.FAppTerminateFlags := [];
      MainApp.Execute;
      repeat
        Sleep(250);
        Application.ProcessMessages;
      until MainFrm.Cursor <> crHourGlass;
    end else
      MessageDlg(MainFrm_Messages.Items[_CMainFrm_ErrOnCreateCheckOutDir], mtError, [mbOK], 0);
  end else
    FDragFilesSrc.ClearFiles;
end;

procedure TMainFrm.MainFrm_ListViewMouseDown
  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    FDragStartPos.X := X;
    FDragStartPos.Y := Y;
  end;
end;

procedure TMainFrm.MainFrm_ListViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  I: integer;
begin
  if (MainFrm.Cursor <> crHourGlass) then
  begin
    if (not (ssLeft in Shift)) or
       ((Abs(fDragStartPos.X - X) < 20) and (Abs(fDragStartPos.Y - Y) < 20)) or
       (MainFrm_ListView.Selected = nil) then Exit;

    FDragFilesSrc.ClearFiles;
    for I := 0 to MainFrm_ListView.Items.Count - 1 do
    begin
      if MainFrm_ListView.items[I].Selected then
      begin
        FDragFilesSrc.AddFile(
          AnsiIncludeTrailingBackslash(GetApplicationTempDir) +
          MainFrm_ListView.Items[I].SubItems[MainFrm_ListView.Columns.Count - 3] +
          MainFrm_ListView.Items[I].Caption);
      end;
    end;

    FDragFilesSrc.VerifyFiles := False;
    FDragFilesTrg.Enabled := False;
    try
      FDragFilesSrc.Execute;
    finally
      FDragFilesTrg.Enabled := True;
    end;
  end;
end;

 // ----------------------------------------------------------------------- //
 //                                                                         //
 //  TMainApp                                                               //
 //                                                                         //
 // ----------------------------------------------------------------------- //

procedure TMainApp.FAppInterfaceCreate;
begin
  FAppParams := TStringList.Create;
  FAppLog := TStringList.Create;
  // ---
  FAppInterface := TAppInterface.Create;
  FAppInterface.OnKey := FAppOnKey;
  FAppInterface.OnList := FAppOnList;
  FAppInterface.OnTick := FAppOnTick;
  FAppInterface.OnError := FAppOnError;
  FAppInterface.OnClear := FAppOnClear;
  FAppInterface.OnRename := FAppOnRename;
  FAppInterface.OnWarning := FAppOnWarning;
  FAppInterface.OnOverWrite := FAppOnOverWrite;
  FAppInterface.OnFatalError := FAppFatalError;
  // ---
  FAppTerminateFlags := [];
  FAppInterfaceUpdate;
end;

procedure TMainApp.AppInterfaceClear;
begin
  FAppParams.Clear;
  FAppLog.Clear;
end;

procedure TMainApp.FAppInterfaceUpdate;
begin
  if (tfLog in FAppTerminateFlags) or
    (MainFrm.MainFrm_MainMenu_Options_LogReport.Checked) then
    FAppInterface.OnDisplay := FAppOnDisplayWithLog
  else
    FAppInterface.OnDisplay := FAppOnDisplay;
end;

procedure TMainApp.FAppInterfaceDestroy;
begin
  FAppInterface.Free;
  // ---
  FAppParams.Free;
  FAppLog.Free;
end;

 // ----------------------------------------------------------------------- //
 //                                                                         //
 //  TMainApp                                                               //
 //                                                                         //
 // ----------------------------------------------------------------------- //

constructor TMainApp.Create;
begin
  FAppInterfaceCreate;
  FAppArcName := '';
  FAppKey := '';
end;

destructor TMainApp.Destroy;
begin
  FAppInterfaceDestroy;
end;

procedure TMainApp.Execute;
var
  ArcExt: string;
begin
  MainFrm.MainFrm_UpdateButtons(False);
  MainFrm.MainFrm_UpdateCursor(crHourGlass);

  FAppInterfaceUpdate;
  ArcExt := ExtractFileExt(AppArcName);
  if (AnsiCompareText(ArcExt, '.zip') = 0) or
     (AnsiCompareText(ArcExt, '.tar') = 0) or
     (AnsiCompareText(ArcExt, '.gz' ) = 0) or
     (AnsiCompareText(ArcExt, '.tgz') = 0) then
  begin
    FApp := TZipApp.Create(FAppInterface, FAppParams);
  end else
    FApp := TBeeApp.Create(FAppInterface, FAppParams);

  FApp.OnTerminate := FAppOnTerminate;
  FApp.Resume;
end;

procedure TMainApp.FAppOnKey;
begin
  if Length(FAppKey) = 0 then
  begin
    MainFrm.MainFrm_MainMenu_Options_Password.Click;
  end;
  FAppInterface.cMsg := FAppKey;
end;

procedure TMainApp.FAppOnTick;
begin
  MainFrm.MainFrm_ProgressBar.Position := FAppInterface.cPercentage;
  Application.Title := IntToStr(FAppInterface.cPercentage) + '% BeeGui';
end;

procedure TMainApp.FAppOnList;
begin
  FAppInterface.cList := MainFrm.FArchiveListManager.Files;
end;

procedure TMainApp.FAppOnDisplay;
begin
  MainFrm.MainFrm_StatusBar.Panels[0].Text := (FAppInterface.cMsg);
end;

procedure TMainApp.FAppOnDisplayWithLog;
begin
  FAppLog.Add(FAppInterface.cMsg);
  MainFrm.MainFrm_StatusBar.Panels[0].Text := (FAppInterface.cMsg);
end;

procedure TMainApp.FAppOnWarning;
begin
  FAppLog.Add(FAppInterface.cMsg);
  MainFrm.MainFrm_StatusBar.Panels[0].Text := (FAppInterface.cMsg);
end;

procedure TMainApp.FAppOnError;
begin
  FAppLog.Add(FAppInterface.cMsg);
  MainFrm.MainFrm_StatusBar.Panels[0].Text := (FAppInterface.cMsg);
end;

procedure TMainApp.FAppOnClear;
begin
  // nothing to do!
end;

procedure TMainApp.FAppOnRename;
var
  f: TRenameFrm;
begin
  if FAppRenameDir = False then
  begin
    f := TRenameFrm.Create(MainFrm);
    f.Caption := MainFrm.MainFrm_Messages.Items[_CMainFrm_RenameFile];
    f.RenameFrm_To.Text := FAppInterface.cFileName;
    begin
      if f.ShowModal = mrOk then
        FAppInterface.cMsg := f.RenameFrm_To.Text
      else
        FAppInterface.cMsg := '';
    end;
    f.Free;
  end else
  begin
    FAppInterface.cMsg := FAppInterface.cFileName;
    Delete(FAppInterface.cMsg, 1, Length(FAppRenameDirFrom));
    FAppInterface.cMsg := FAppRenameDirTo + FAppInterface.cMsg;
  end;
end;

procedure TMainApp.FAppOnOverWrite;
var
  f: TOverWriteFrm;
begin
  f := TOverWriteFrm.Create(MainFrm);
  with FAppInterface do
  begin
    f.Update(cFileName, cFileSize, cFileTime);
  end;
  case f.ShowModal of
    mrAbort: FAppInterface.cMsg := 'Q';
    mrNoToAll: FAppInterface.cMsg := 'S';
    mrYesToAll: FAppInterface.cMsg := 'A';
    mrNo: FAppInterface.cMsg  := 'N';
    mrYes: FAppInterface.cMsg := 'Y';
  end;
  f.Free;
end;

procedure TMainApp.FAppFatalError;
begin
  MessageDlg(FAppInterface.cMsg, mtError, [mbOk], 0);
  FAppTerminateFlags := [tfFatalError];
end;

procedure TMainApp.FAppOnTerminate;
begin
  MainFrm.MainFrm_ProgressBar.Position := 0;
  Application.Title := _SAppTitle;
  // ---
  if not (tfFatalError in FAppTerminateFlags) then
    MainFrm.FArchiveListManager.Init;
  // ---
  if not (tfViewExt in FAppTerminateFlags) then
  begin
    MainFrm.MainFrm_UpdateButtons(True);
    MainFrm.MainFrm_UpdateCursor(crDefault);
  end;

  if not (tfFatalError in FAppTerminateFlags) then
    FAppTerminateWithLog;

  if tfFatalError in FAppTerminateFlags then
  begin
    Exclude(FAppTerminateFlags, tfFatalError);
    MainFrm.MainFrm_MainMenu_File_Close.Click;
  end else
  if tfNew in FAppTerminateFlags then
    FAppTerminateWithNew
  else
  if tfOpen in FAppTerminateFlags then
    FAppTerminateWithOpen
  else
  if tfAdd in FAppTerminateFlags then
    FAppTerminateWithAdd
  else
  if tfExtract in FAppTerminateFlags then
    FAppTerminateWithExtract
  else
  if tfCheckOut in FAppTerminateFlags then
    FAppTerminateWithCheckOut
  else
  if tfViewExt in FAppTerminateFlags then
    FAppTerminateWithView(
      AnsiIncludeTrailingBackSlash(GetApplicationTempDir) +
      FAppParams.Strings[FAppParams.Count - 1], False)
  else
  if tfViewInt in FAppTerminateFlags then
    FAppTerminateWithView(
      AnsiIncludeTrailingBackSlash(GetApplicationTempDir) +
      FAppParams.Strings[FAppParams.Count - 1], True)
  else
  if tfClose in FAppTerminateFlags then
  begin
    Exclude(FAppTerminateFlags, tfClose);
    MainFrm.Close;
  end;

end;

 // ------------------------------------------------------------------------ //
 //                                                                          //
 //  OnTerminate App Thread Mode                                             //
 //                                                                          //
 // ------------------------------------------------------------------------ //  

procedure TMainApp.FAppTerminateWithNew;
begin
  Exclude(FAppTerminateFlags, tfNew);
  MainFrm.MainFrm_MainMenu_File_New.Click;
end;

procedure TMainApp.FAppTerminateWithOpen;
begin
  Exclude(FAppTerminateFlags, tfOpen);
  MainFrm.MainFrm_MainMenu_File_Open.Click;
end;

procedure TMainApp.FAppTerminateWithAdd;
begin
  Exclude(FAppTerminateFlags, tfAdd);
  MainFrm.MainFrm_MainMenu_Actions_Add.Click;
end;

procedure TMainApp.FAppTerminateWithExtract;
begin
  Exclude(FAppTerminateFlags, tfExtract);
  MainFrm.MainFrm_MainMenu_Actions_Extract.Click;
end;

procedure TMainApp.FAppTerminateWithCheckOut;
begin
  Exclude(FAppTerminateFlags, tfCheckOut);
  ShellExec(GetCurrentDir, '');
end;

procedure TMainApp.FAppTerminateWithView
  (FileName: string; UseIntViewer: boolean);
var
  f: TIntViewerFrm;
begin
  Exclude(FAppTerminateFlags, tfViewInt);
  Exclude(FAppTerminateFlags, tfViewExt);
  if FileExists(FileName) then
  begin
    if UseIntViewer = True then
    begin
      f := TIntViewerFrm.Create(MainFrm);
      f.IntViewerFrm_Viewer.Lines.LoadFromFile(FileName);
      f.ShowModal;
      f.Free;
    end else
    begin
      FViewer := TAppViewer.Create(FileName);
      FViewer.OnTerminate := FViewerOnTerminate;
      FViewer.Resume;
    end;
  end else
  begin
    MainFrm.MainFrm_UpdateButtons(True);
    MainFrm.MainFrm_UpdateCursor(crDefault);
  end;
end;

procedure TMainApp.FAppTerminateWithLog;
var
  f: TIntViewerFrm;
begin
  Exclude(FAppTerminateFlags, tfLog);
  if Assigned(FAppLog) then
  begin
    if (FAppLog.Count > 0) then
    begin
      f := TIntViewerFrm.Create(MainFrm);
      f.IntViewerFrm_Viewer.Lines.AddStrings(FAppLog);
      f.ShowModal;
      f.Free;
    end;
    FAppLog.Clear;
  end;
end;

function TMainApp.CmdIsSupported(const Cmd: char): boolean;
var
  FAppArcExt: string;
begin
  FAppArcExt := '|' + ExtractFileExt(FAppArcName) + '|';
  case Cmd of
    'a': Result := (Pos(FAppArcExt, '|.bee|.zip|.tar|              ') > 0);
    'd': Result := (Pos(FAppArcExt, '|.bee|.zip|.tar|.tgz|.gz|     ') > 0);
    'e': Result := (Pos(FAppArcExt, '|.bee|.zip|.tar|.tgz|.gz|.cab|') > 0);
    'x': Result := (Pos(FAppArcExt, '|.bee|.zip|.tar|.tgz|.gz|.cab|') > 0);
    'l': Result := (Pos(FAppArcExt, '|.bee|.zip|.tar|.tgz|.gz|.cab|') > 0);
    't': Result := (Pos(FAppArcExt, '|.bee|.zip|                   ') > 0);
    'r': Result := (Pos(FAppArcExt, '|.bee|.zip|.tar|.tgz|.gz|.cab|') > 0);
    else Result := False;
  end;
end;

 // ---------------------------------------------------------------------- //
 //                                                                        //
 //  FViewer                                                               //
 //                                                                        //
 // ---------------------------------------------------------------------- //

procedure TMainApp.FViewerOnTerminate;
begin
  MainFrm.MainFrm_UpdateButtons(True);
  MainFrm.MainFrm_UpdateCursor(crDefault);

  if FileAge(FViewer.FileName) > FViewer.FileTime then
  begin
    if MessageDlg(MainFrm.MainFrm_Messages.Items[_CMainFrm_FreshFileModified],
      mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      AppInterfaceClear;
      FAppParams.Add('a');
      FAppParams.Add('-l');
      FAppParams.Add('-y' + GetApplicationTempDir);
      FAppParams.Add('-f');
      FAppParams.Add(MainFrm.fConfigFrm.GetPriorityStr);
      FAppParams.Add(FAppArcName);
      FAppParams.Add(ExtractFileName(FViewer.FileName));

      FAppTerminateFlags := [];
      Execute;
    end;
  end;
end;

end.
