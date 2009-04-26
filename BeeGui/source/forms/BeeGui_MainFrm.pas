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

{   Contains:

    BeeGui Main form.

    Modifyed:

    v1.0.1 build 9156 - 2005.07.16 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.1 build 9160 - 2005.08.03 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0312 - 2007.01.27 by Melchiorre Caruso.
}

unit BeeGui_MainFrm;

{$R-,Q-,S-}

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

  uDragFilesTrg,
  uDragFilesSrc,

  Bee_Interface,
  BeeGui_Components;

type
  TTerminateWith = (twNone, twNewArc, twCheckOut, twViewInt, twViewExt, twDrag, twClose);

type
  TMainCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    procedure   Save (HideForm: boolean);
    procedure   LoadLanguage;
  end;

type
  TMainFrm = class (TMainCustomFrm)
    MainFrm_ButtonsPanel: TPanel;
    MainFrm_Bevel: TBevel;
    MainFrm_BevelFirst: TBevel;
    MainFrm_BevelSecond: TBevel;
    MainFrm_BevelThird: TBevel;
    MainFrm_Spacer: TBevel;
    MainFrm_PathBox: TBeeGui_PathBox;
    MainFrm_StatusBar: TStatusBar;
    MainFrm_MSGs: TComboBox;

    BtnUp: TSpeedButton;
    BtnOpen: TSpeedButton;
    BtnNew: TSpeedButton;
    BtnAdd: TSpeedButton;
    BtnExtract: TSpeedButton;
    BtnView: TSpeedButton;
    BtnDelete: TSpeedButton;
    BtnTest: TSpeedButton;
    BtnInfo: TSpeedButton;
    BtnCheckOut: TSpeedButton;
    BtnHelp: TSpeedButton;
    BtnExit: TSpeedButton;

    MainFrm_OpenDialog: TOpenDialog;
    MainFrm_SaveDialog: TSaveDialog;
    MainFrm_FontDialog: TFontDialog;

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

    MainFrm_MainMenu_View: TMenuItem;
    MainFrm_MainMenu_View_N1: TMenuItem;
    MainFrm_MainMenu_View_N2: TMenuItem;
    MainFrm_MainMenu_View_N3: TMenuItem;

    MainFrm_MainMenu_View_ToolBar: TMenuItem;
    MainFrm_MainMenu_View_ToolBar_Btns: TMenuItem;

    MainFrm_MainMenu_View_ToolBar_AddressBar: TMenuItem;

    MainFrm_MainMenu_View_StatusBar: TMenuItem;
    MainFrm_MainMenu_View_Icons: TMenuItem;
    MainFrm_MainMenu_View_SmallIcons: TMenuItem;
    MainFrm_MainMenu_View_List: TMenuItem;
    MainFrm_MainMenu_View_Report: TMenuItem;

    MainFrm_MainMenu_View_RowSelect: TMenuItem;
    MainFrm_MainMenu_View_GridLines: TMenuItem;
    MainFrm_MainMenu_View_ListMode: TMenuItem;

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

    MainFrm_MainMenu_Options: TMenuItem;
    MainFrm_MainMenu_Options_N1: TMenuItem;
    MainFrm_MainMenu_Options_N2: TMenuItem;
    MainFrm_MainMenu_Options_Config: TMenuItem;
    MainFrm_MainMenu_Options_Password: TMenuItem;
    MainFrm_MainMenu_Options_SaveOnExit: TMenuItem;
    MainFrm_MainMenu_Options_SaveNow: TMenuItem;
    MainFrm_MainMenu_Options_Default: TMenuItem;
    MainFrm_MainMenu_Options_LogReport: TMenuItem;

    MainFrm_MainMenu_Help: TMenuItem;
    MainFrm_MainMenu_Help_N1: TMenuItem;
    MainFrm_MainMenu_Help_N2: TMenuItem;
    MainFrm_MainMenu_Help_F1: TMenuItem;
    MainFrm_MainMenu_Help_Internet: TMenuItem;
    MainFrm_MainMenu_Help_License: TMenuItem;
    MainFrm_MainMenu_Help_About: TMenuItem;

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

    MainFrm_ListView: TBeeGui_ListView;
    MainFrm_ProgressBar: TProgressBar;

    MainFrm_DragFilesSrc: TDragFilesSrc;
    MainFrm_DragFilesTrg: TDragFilesTrg;

    procedure MainFrm_MainMenu_File_NewClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_OpenClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_CloseClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_InfoClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_MoveClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_CopyClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_RenameClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_DeleteClick (Sender: TObject);
    procedure MainFrm_MainMenu_File_ExitClick (Sender: TObject);

    procedure MainFrm_MainMenu_View_ToolBar_BtnsClick(Sender: TObject);
    procedure MainFrm_MainMenu_View_ToolBar_AddressBarClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_StatusBarClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_IconsClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_SmallIconsClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_ListClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_ReportClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_RowSelectClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_GridLinesClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_ListModeClick (Sender: TObject);

    procedure MainFrm_MainMenu_View_OrderBy_NameClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_TypeClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_PackedClick ( Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_SizeClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_RatioClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_MethodClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_ModifiedClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_AttributesClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_PasswordClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_CRCClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_PathClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_OrderBy_PositionClick (Sender: TObject);

    procedure MainFrm_MainMenu_View_Details_NameClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_TypeClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_PackedClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_SizeClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_RatioClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_MethodClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_ModifiedClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_AttributesClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_PasswordClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_CRCClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_PathClick (Sender: TObject);
    procedure MainFrm_MainMenu_View_Details_PositionClick (Sender: TObject);

    procedure MainFrm_MainMenu_Actions_AddClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_DeleteClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ExtractClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ExtractAllClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_TestClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_RenameClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_ViewClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_InvertClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_SelectAllClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_SelectMaskClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_UnselectMaskClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_TestAllClick (Sender: TObject);
    procedure MainFrm_MainMenu_Actions_CheckOutClick (Sender: TObject);

    procedure MainFrm_MainMenu_Options_ConfigClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_SaveNowClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_DefaultClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_PasswordClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_LogReportClick (Sender: TObject);
    procedure MainFrm_MainMenu_Options_SaveOnExitClick (Sender: TObject);

    procedure MainFrm_MainMenu_Help_F1Click (Sender: TObject);
    procedure MainFrm_MainMenu_Help_InternetClick (Sender: TObject);
    procedure MainFrm_MainMenu_Help_LicenseClick (Sender: TObject);
    procedure MainFrm_MainMenu_Help_AboutClick (Sender: TObject);

    procedure FormResize (Sender: TObject);
    procedure FormCreate (Sender: TObject);
    procedure FormClose (Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery (Sender: TObject; var CanClose: Boolean);

    procedure MainFrm_StatusBarResize (Sender: TObject);
    procedure MainFrm_StatusBarDrawPanel (StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);

    procedure MainFrm_PopupMenuPopup (Sender: TObject);
    procedure MainFrm_PopupMenu_Open_IntViewerClick(Sender: TObject);

    procedure BtnUpClick (Sender: TObject);
    procedure MainFrm_PathBoxClick (Sender: TObject);
    procedure MainFrm_ListViewClick (Sender: TObject);
    procedure MainFrm_ListViewColumnClick (Sender: TObject; Column: TListColumn);

    procedure MainFrm_DragFilesTrgDrop (Sender: TObject);
    procedure MainFrm_DragFilesSrcDropping (Sender: TObject);
    
    procedure MainFrm_ListViewMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MainFrm_ListViewMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure MainFrm_PopupMenu_Buttons_NewClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_OpenClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_N1Click (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_AddClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_ExtractClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_ViewClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_DeleteClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_N2Click (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_TestClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_InfoClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_CheckOutClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_N3Click (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_HelpClick (Sender: TObject);
    procedure MainFrm_PopupMenu_Buttons_ExitClick (Sender: TObject);
  public
    App: TApp;
    App_Key: string;
    App_ArcName: string;
    App_Params: TStringList;
    App_TerminateWith: TTerminateWith;
    procedure App_Create (CreateLogFile: boolean);
  private
    App_RenameDir: boolean;
    App_RenameDirTo: string;
    App_RenameDirFrom: string;

    App_TempDir: string;
    App_CheckOutDir: string;
    
    App_LogFile: TStringList;
    App_Interface: TAppInterface;

    procedure App_OnKey;
    procedure App_OnTick;
    procedure App_OnList;
    procedure App_OnError;
    procedure App_OnClear;
    procedure App_OnRename;
    procedure App_OnWarning;
    procedure App_OnDisplay;
    procedure App_OnOverWrite;
    procedure App_OnTerminate (Sender: TObject);

    procedure App_TerminateWith_Log;
    procedure App_TerminateWith_New;
    procedure App_TerminateWith_CheckOut;
    procedure App_TerminateWith_View (FileName: string; UseIntViewer: boolean);
 private
    AppView: TBeeGui_AppView;
    procedure AppView_OnTerminate (Sender: TObject);
  private
    MainFrm_DragStartPos: TPoint;
    MainFrm_Buttons: array [0.. 13] of TControl;

    procedure MainFrm_AddFile (const FileName: string);
    procedure MainFrm_AddFiles (FilesList: TStrings; EnableFilesList: boolean);

    function  MainFrm_ProcessFiles (var Params: TStringList): integer;
    procedure MainFrm_ConfigFrm_Show(PageIndex: integer);
    procedure MainFrm_CheckFileAssociation;
    procedure MainFrm_DeleteDirectories;

    procedure MainFrm_UpdateButtons; overload;
    procedure MainFrm_UpdateOrder (index: integer);
    procedure MainFrm_UpdateStyle (index: integer);
    procedure MainFrm_UpdateCursor (Value: TCursor);
    procedure MainFrm_UpdateDetails (index: integer);
    procedure MainFrm_UpdateButtons (Value: boolean); overload;
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.DFM}

uses
  Bee_App,

  BeeGui_WinOS,
  BeeGui_AddFrm,
  BeeGui_Common,
  BeeGui_InfoFrm,
  BeeGui_AboutFrm,
  BeeGui_SelectFrm,
  BeeGui_RenameFrm,
  BeeGui_ConfigFrm,
  BeeGui_ExtractFrm,
  BeeGui_PasswordFrm,
  BeeGui_OverWriteFrm,
  BeeGui_IntViewerFrm;

const

  // MainFrm Messages Index

  MainFrm_MSGs_MoveTo            =  0;
  MainFrm_MSGs_MoveTo_E          =  1;
  MainFrm_MSGs_CopyTo            =  2;
  MainFrm_MSGs_CopyTo_E          =  3;
  MainFrm_MSGs_Rename_Arc        =  4;
  MainFrm_MSGs_Rename_Arc_E      =  5;
  MainFrm_MSGs_Delete            =  6;
  MainFrm_MSGs_Delete_E          =  7;
  MainFrm_MSGs_Rename_File       =  8;
  MainFrm_MSGs_Renane_File_E     =  9;
  MainFrm_MSGs_Default_Cfg       = 10;
  MainFrm_MSGs_Default_Cfg_E     = 11;
  MainFrm_MSGs_Delete_Files      = 12;
  MainFrm_MSGs_Terminate_Process = 13;
  MainFrm_MSGs_Process           = 14;
  MainFrm_MSGs_No_Files_Selected = 15;
  MainFrm_MSGs_Selected_Items    = 16;
  MainFrm_MSGs_N_Items           = 17;
  MainFrm_MSGs_Delete_CheckOut   = 18;
  MainFrm_MSGs_Checkout_E        = 19;
  MainFrm_MSGs_Set_Cur_Dir_E     = 20;
  MainFrm_MSGs_Rename_Dir        = 21;
  MainFrm_MSGs_Rename_Dir_E      = 22;
  MainFrm_MSGs_Register_FileType = 23;
  MainFrm_MSGs_Freshen_File      = 24;

  // - Section ------------------------------------------------------------ //
  //                                                                        //
  //  Custom functions and procedures                                       //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  function GetPriority: integer;
  begin
    Result := 3 - BeeGui_Configuration.GetIntegerValue (PPriority_CLASS, PPriority_Ident, 2);
  end;

  // - Section ------------------------------------------------------------ //
  //                                                                        //
  //  MainCustomFrm                                                         //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  // TMainCustomFrm.Create

  constructor TMainCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);
    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  // TMainCustomFrm.Save

  procedure TMainCustomFrm.Save;
  var
    I: integer;
  begin
    {$IFNDEF DEBUG}
    Visible := not HideForm;
    {$ENDIF}
    BeeGui_Configuration.Save_TForm (Self);
    for I := 0 to ComponentCount - 1 do
      BeeGui_Configuration.Save_TControl (TControl (Components [I]));

    BeeGui_Configuration.Save;
    {$IFDEF DEBUG}
    BeeGui_Language.Save;
    {$ENDIF}
  end;

  // TMainCustomFrm.LoadLanguage

  procedure TMainCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (TForm (Self));
    for I := 0 to ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Components [I]));
  end;

  // - Section ------------------------------------------------------------ //
  //                                                                        //
  //  MainFrm                                                               //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.FormCreate

  procedure TMainFrm.FormCreate (Sender: TObject);
  begin
    MainFrm_Buttons [ 0] := BtnNew;
    MainFrm_Buttons [ 1] := BtnOpen;
    MainFrm_Buttons [ 2] := MainFrm_BevelFirst;
    MainFrm_Buttons [ 3] := BtnAdd;
    MainFrm_Buttons [ 4] := BtnExtract;
    MainFrm_Buttons [ 5] := BtnView;
    MainFrm_Buttons [ 6] := BtnDelete;
    MainFrm_Buttons [ 7] := MainFrm_BevelSecond;
    MainFrm_Buttons [ 8] := BtnTest;
    MainFrm_Buttons [ 9] := BtnInfo;
    MainFrm_Buttons [10] := BtnCheckOut;
    MainFrm_Buttons [11] := MainFrm_BevelThird;
    MainFrm_Buttons [12] := BtnHelp;
    MainFrm_Buttons [13] := BtnExit;

    App_Key         := '';
    App_ArcName     := '';
    App_TempDir     := ReturnTempDir;
    App_CheckOutDir := ReturnCheckOutDir;
    App_Params      := TStringList.Create;

    App_Interface             := TAppInterface.Create;
    App_Interface.OnKey       := MainFrm.App_OnKey;
    App_Interface.OnList      := MainFrm.App_OnList;
    App_Interface.OnTick      := MainFrm.App_OnTick;
    App_Interface.OnError     := MainFrm.App_OnError;
    App_Interface.OnClear     := MainFrm.App_OnClear;
    App_Interface.OnRename    := MainFrm.App_OnRename;
    App_Interface.OnWarning   := MainFrm.App_OnWarning;
    App_Interface.OnDisplay   := MainFrm.App_OnDisplay;
    App_Interface.OnOverWrite := MainFrm.App_OnOverWrite;

    LoadBtnsSkin (MainFrm_Buttons);
    MainFrm_UpdateButtons (False);
    MainFrm_CheckFileAssociation;
    MainFrm_UpdateDetails (-1);
    MainFrm_UpdateStyle (-1);
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.FormCloseQuery

  procedure TMainFrm.FormCloseQuery (Sender: TObject; var CanClose: Boolean);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then
      CanClose := MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Terminate_Process], mtWarning, [mbYes, mbNo], 0) = mrYes
    else
      CanClose := True;
  end;

  /// TMainFrm.FormClose

  procedure TMainFrm.FormClose (Sender: TObject; var Action: TCloseAction);
  begin
    if MainFrm_MainMenu_Options_SaveOnExit.Checked then Save (True);

    if MainFrm_ListView.Cursor = crHourGlass then
    begin
      if Assigned (App) then App.Terminate;
      if Assigned (AppView) then AppView.Terminate;
    end else
      MainFrm_MainMenu_File_Close.Click;
  end;

  /// TMainFrm.FormResize

  procedure TMainFrm.FormResize (Sender: TObject);
  begin
    MainFrm_ProgressBar.Parent := MainFrm_StatusBar;
    MainFrm_StatusBar.Panels [0].Width := MainFrm.Width * 2 div 3;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Update Graphic interface                                              //
  //                                                                        //
  //  --------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_UpdateOrder

  procedure TMainFrm.MainFrm_UpdateOrder;
  var
    I: integer;
  begin
    if (index > -1) and (index < MainFrm_MainMenu_View_OrderBy.Count) then
    begin
      for I := 0 to MainFrm_MainMenu_View_OrderBy.Count -1 do
      begin
        MainFrm_MainMenu_View_OrderBy.Items [I].Checked := False;
        MainFrm_PopupMenu_OrderBy.Items [I].Checked := False;
      end;
      MainFrm_MainMenu_View_OrderBy.Items [index].Checked :=True;
      MainFrm_PopupMenu_OrderBy.Items [index].Checked := True;

      case index of
         0: MainFrm_ListView.ColumnSort := byName;
         1: MainFrm_ListView.ColumnSort := bySize;
         2: MainFrm_ListView.ColumnSort := byPacked;
         3: MainFrm_ListView.ColumnSort := byRatio;
         4: MainFrm_ListView.ColumnSort := byType;
         5: MainFrm_ListView.ColumnSort := byTime;
         6: MainFrm_ListView.ColumnSort := byAttr;
         7: MainFrm_ListView.ColumnSort := byMethod;
         8: MainFrm_ListView.ColumnSort := byPassword;
         9: MainFrm_ListView.ColumnSort := byCRC;
        10: MainFrm_ListView.ColumnSort := byPath;
        11: MainFrm_ListView.ColumnSort := byPosition;
      end;
    end;
  end;

  /// TMainFrm.MainFrm_UpdateDetails

  procedure TMainFrm.MainFrm_UpdateDetails;
  var
    I: integer;
  begin
    if (index > -1) and (index < MainFrm_MainMenu_View_Details.Count) then
    begin
      MainFrm_MainMenu_View_Details.Items [index].Checked := not MainFrm_MainMenu_View_Details.Items [index].Checked;
    end;
    for I := 0 to MainFrm_MainMenu_View_Details.Count -1 do
    begin
      MainFrm_PopupMenu_Details.Items [I].Checked := MainFrm_MainMenu_View_Details.Items [I].Checked;
      if MainFrm_MainMenu_View_Details.Items [I].Checked then
      begin
        if MainFrm_ListView.Columns [I].Width < 20 then
        begin
          MainFrm_ListView.Columns [I].MaxWidth :=  0;
          MainFrm_ListView.Columns [I].MinWidth :=  0;
          MainFrm_ListView.Columns [I].Width    := 60;
        end;
      end else
      begin
        MainFrm_ListView.Columns [I].MaxWidth := 1;
        MainFrm_ListView.Columns [I].MinWidth := 0;
        MainFrm_ListView.Columns [I].Width    := 0;
      end;
    end;
  end;

  /// TMainFrm.MainFrm_UpdateStyle

  procedure TMainFrm.MainFrm_UpdateStyle;
  var
    I: integer;
  begin
    if (index > 2) and (index < 7) then
    begin
      for I := 3 to 6 do
        if I = index then
          MainFrm_MainMenu_View.Items [I].Checked := True
        else
          MainFrm_MainMenu_View.Items [I].Checked := False;
    end;

    for I := 3 to 6 do if MainFrm_MainMenu_View.Items [I].Checked then index := I;

    if index = 3 then MainFrm_ListView.ViewStyle := vsIcon;
    if index = 4 then MainFrm_ListView.ViewStyle := vsSmallIcon;
    if index = 5 then MainFrm_ListView.ViewStyle := vsList;
    if index = 6 then MainFrm_ListView.ViewStyle := vsReport;
  end;

  /// TMainFrm.MainFrm_UpdateButtons

  procedure TMainFrm.MainFrm_UpdateButtons;
  const
    TopMargin = 4;
    LeftMargin = 4;
  var
    I: integer;
    TopPos: integer;
    LeftPos: integer;
  begin
    TopPos := TopMargin;
    LeftPos := LeftMargin;

    for I := 0 to High (MainFrm_Buttons) do
      MainFrm_Buttons [I].Visible := MainFrm_PopupMenu_Buttons.Items [I].Checked;

    for I := 0 to High (MainFrm_Buttons) do
    begin
      MainFrm_Buttons [I].Top  := TopPos;
      MainFrm_Buttons [I].Left := LeftPos;
      if MainFrm_Buttons [I].Visible then
        Inc (LeftPos, LeftMargin + MainFrm_Buttons [I].Width);
    end;

    if not (LeftPos = LeftMargin) then
      Inc (TopPos, TopMargin + MainFrm_Buttons [0].Height);

    MainFrm_StatusBar.Visible := MainFrm_MainMenu_View_StatusBar.Checked;

    if MainFrm_MainMenu_View_ListMode.Checked then
      MainFrm_MainMenu_View_ToolBar_AddressBar.Enabled := False
    else
      MainFrm_MainMenu_View_ToolBar_AddressBar.Enabled := True;

    with MainFrm_MainMenu_View_ToolBar_AddressBar do
      BtnUp.Visible := Checked and Enabled;

    MainFrm_PathBox.Visible := BtnUp.Visible;

    MainFrm_ListView.RowSelect := MainFrm_MainMenu_View_RowSelect.Checked;
    MainFrm_ListView.GridLines := MainFrm_MainMenu_View_GridLines.Checked;
    MainFrm_ListView.ListMode  := MainFrm_MainMenu_View_ListMode.Checked;

    MainFrm_Bevel.Top := TopPos;
    MainFrm_Bevel.Visible := (not (LeftPos = LeftMargin)) and (BtnUp.Visible);
    if MainFrm_Bevel.Visible then Inc (TopPos, TopMargin + MainFrm_Bevel.Height);

    BtnUp.Top := TopPos;
    MainFrm_PathBox.Top := TopPos;
    if MainFrm_PathBox.Visible then
      Inc (TopPos, TopMargin + MainFrm_PathBox.Height);

    MainFrm_ButtonsPanel.Height := TopPos;

    if not (MainFrm_ButtonsPanel.Height = TopMargin) then
      MainFrm_ButtonsPanel.Visible := True
    else
      MainFrm_ButtonsPanel.Visible := False;
  end;

  /// TMainFrm.MainFrm_UpdateCursor

  procedure TMainFrm.MainFrm_UpdateCursor;
  begin
    MainFrm.Cursor          := Value;
    MainFrm_PathBox.Cursor  := Value;
    MainFrm_ListView.Cursor := Value;
  end;

  /// TMainFrm.MainFrm_UpdateButtons (Value: boolean)

  procedure TMainFrm.MainFrm_UpdateButtons (Value: boolean);
  var
    I: integer;
  begin
    with MainFrm_MainMenu_File    do for I := 2 to 9        do Items [I].Enabled := Value;
    with MainFrm_MainMenu_Actions do for I := 0 to Count -1 do Items [I].Enabled := Value;

    for I := 3 to High (MainFrm_Buttons) -3 do
      if MainFrm_Buttons [I].ClassType = TSpeedButton then
        TSpeedButton (MainFrm_Buttons [I]).Enabled := Value;
      
      BtnUp.Enabled            := Value;
      MainFrm_PathBox.Enabled  := Value;
      MainFrm_ListView.Enabled := Value;

      if Value then
        MainFrm_PathBox.Color := clWindow
      else
        MainFrm_PathBox.Color := clbtnFace;

    MainFrm_ListView.Color :=  MainFrm_PathBox.Color;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Register .bee file type                                               //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_CheckFileType

  procedure TMainFrm.MainFrm_CheckFileAssociation;
  begin
    if (BeeGui_Configuration.GetBoolValue (CHECKFILETYPE_CLASS, CHECKFILETYPE_IDENT, False)) and (not BeeGui_CheckRegisterFileType ('bee', ParamStr (0))) then
      if MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Register_FileType], mtinformation, [mbYes, mbNo], 0) = mrYes then
      begin
        BeeGui_RegisterFileType ('bee', ParamStr (0));
      end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Delete temp directories                                               //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_DeleteDirectories

  procedure TMainFrm.MainFrm_DeleteDirectories;
  begin
    if BeeGui_Common.DirectoryExists (App_TempDir) then
      BeeGui_Common.DeleteDir (App_TempDir);

    if BeeGui_Common.DirectoryExists (App_CheckOutDir) then
      if MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_delete_checkout], mtInformation, [mbYes, mbNo], 0) = mrYes then
        BeeGui_Common.DeleteDir (App_CheckOutDir);
  end;

  /// TMainFrm.MainFrm_ConfigFrm_Show

  procedure TMainFrm.MainFrm_ConfigFrm_Show (PageIndex: integer);
  var
    f: TConfigFrm;
  begin
    f := TConfigFrm.Create (Self);
    f.ConfigFrm_Notebook.PageIndex := PageIndex;
    f.Set_Btns_PopupMenu (MainFrm_PopupMenu_Buttons);
    f.ConfigFrm_Tree.Items [PageIndex].Selected :=True;

    case f.ShowModal of
      mrCancel: f.Destroy;
      mrOk    : begin
                  f.Get_Btns_PopupMenu;
                  f.SaveAndDestroy;
                  // update skin
                  LoadBtnsSkin (MainFrm_Buttons);
                  MainFrm_UpdateButtons;
                  // update language
                  BeeGui_UpdateLanguage;
                  MainFrm.LoadLanguage;
                end;
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Add files                                                             //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.AddFiles (FilesList: TStrings);

  procedure TMainFrm.MainFrm_AddFiles (FilesList: TStrings; EnableFilesList: boolean);
  var
    f: TAddFrm;
    I: integer;
    MainFrmState: TWindowState;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;
    
    f := TAddFrm.Create (Self);
    
    if Assigned (FilesList) then
      for I := 0 to FilesList.Count -1 do
      begin
        if FileExists (FilesList.Strings [I]) then
          f.AddFrm_FilesList.AddFile (FilesList.Strings [I])
        else
          f.AddFrm_FilesList.AddDirectory (FilesList.Strings [I]);
      end;

    f.EnableFilesList (EnableFilesList);
    f.AddFrm_cOption.Enabled := not MainFrm_MainMenu_View_ListMode.Checked;

    MainFrmState := MainFrm.WindowState;
    if BeeGui_Configuration.GetBoolValue (HIDEMAINFRM_CLASS, HIDEMAINFRM_IDENT, False) then
      MainFrm.WindowState := wsMinimized;

    if f.ShowModal = mrOk then
    begin
      if (f.AddFrm_FilesList.Items.Count > 0) then
      begin
        App_Params.Clear;
        App_Params.Add ('a');
        App_Params.Add ('-y' + App_TempDir);

        if f.AddFrm_rOption.Checked then App_Params.Add ('-r');
        if f.AddFrm_sOption.Checked then App_Params.Add ('-s');
        if f.AddFrm_tOption.Checked then App_Params.Add ('-t');
        if f.AddFrm_aOption.Checked then App_Params.Add ('-a');
        if f.AddFrm_kOption.Checked then App_Params.Add ('-k');

        App_Params.Add ('-m'   + IntToStr (f.AddFrm_Method     .ItemIndex));
        App_Params.Add ('-d'   + IntToStr (f.AddFrm_Dictionary .ItemIndex));
        App_Params.Add ('-e'   +           f.AddFrm_eOption.Text          );
        App_Params.Add ('-pri' + IntToStr (3 - f.ConfigFrm_Priority.ItemIndex));

        if f.AddFrm_cOption.Enabled and f.AddFrm_cOption.Checked then App_Params.Add ('-c' + MainFrm_ListView.CurrDir);

        App_Params.Add (App_ArcName);
        App_TerminateWith := twNone;

        for I := 0 to f.AddFrm_FilesList.Items.Count -1 do
        begin
          if f.AddFrm_FilesList.Items [I].ImageIndex = S_Index then
            App_Params.Add (f.AddFrm_FilesList.Items [I].Caption)
          else
            App_Params.Add ('-x' + f.AddFrm_FilesList.Items [I].Caption);
        end;

        if (Length (f.AddFrm_Root.Text) = 0) or SetCurrentDir (f.AddFrm_Root.Text) then
          App_Create (MainFrm_MainMenu_Options_LogReport.Checked or f.AddFrm_tOption.Checked)
        else
          MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Set_Cur_Dir_E], mtError, [mbOk], 0);

      end else
        MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_No_Files_Selected], mtWarning, [mbOk], 0);
    end;

    MainFrm.WindowState := MainFrmState;
    f.Free;
  end;

  procedure TMainFrm.MainFrm_AddFile (const FileName: string);
  var
    Files: TStringList;
  begin
    Files := TStringList.Create;
    Files.Add (FileName);
    MainFrm_AddFiles (Files, False);
    Files.Free;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Status Bar                                                            //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_StatusBarDrawPanel

  procedure TMainFrm.MainFrm_StatusBarDrawPanel (StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
  begin
    if Panel.Index = 1 then
      with MainFrm_ProgressBar do
      begin
        Visible := True;
        Left    := Rect.Left - 1;
        Top     := Rect.Top  - 1;
        Height  := (Rect.Bottom - Rect.Top ) +  2;
        Width   := (Rect.Right  - Rect.Left) - 14;
      end;
  end;

  /// TMainFrm.MainFrm_StatusBarResize

  procedure TMainFrm.MainFrm_StatusBarResize (Sender: TObject);
  begin
    MainFrm_StatusBar.Panels [0].Width := (MainFrm.Width * 2 div 3);
  end;  

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  MainFrm_ListView process selected files                               //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_ProcessFiles

  function TMainFrm.MainFrm_ProcessFiles;
  var
    I: integer;
    Node: TListItem;
  begin
    Result := 0;
    for I := 0 to MainFrm_ListView.Items.Count -1 do
      if MainFrm_ListView.Items [I].Selected then
      begin
        Node := MainFrm_ListView.Items [I];
        if Length (Node.SubItems [MainFrm_ListView.Columns.Count - 2]) = 0 then 
          Params.Add (Node.SubItems [MainFrm_ListView.Columns.Count - 3] + Node.Caption + '\*\*.*')
        else
          Params.Add (Node.SubItems [MainFrm_ListView.Columns.Count - 3] + Node.Caption);
        Inc (Result);
      end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  MainMenu Click                                                        //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_MainMenu_File_NewClick

  procedure TMainFrm.MainFrm_MainMenu_File_NewClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor <> crHourGlass then
    begin
      MainFrm_SaveDialog.FileName := '';
      if MainFrm_SaveDialog.Execute then
      begin
        MainFrm_MainMenu_File_Close.Click;
        App_ArcName := MainFrm_SaveDialog.FileName;

        case MainFrm_SaveDialog.FilterIndex of
          1: App_ArcName := ChangeFileExt (App_ArcName, '.bee');
          2: App_ArcName := ChangeFileExt (App_ArcName, '.exe');
        end;

        Caption := Application.Title + ' - ' + ExtractFileName (App_ArcName);
        MainFrm_ListView.PathBoxSign := IncludeTrailingBackslash (ExtractFileName (App_ArcName));

        App_Params.Clear;
        App_Params.Add ('l');
        App_Params.Add ('-y' + App_TempDir);
        App_Params.Add ('-pri' + IntToStr (GetPriority));
        App_Params.Add (App_ArcName);
        App_Params.Add ('*\*.*');

        App_TerminateWith := twNewArc;

        App_Create (False);
      end;
    end else
      MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_process], mtInformation, [mbOk], 0);
  end;

  /// TMainFrm.MainFrm_MainMenu_File_OpenClick

  procedure TMainFrm.MainFrm_MainMenu_File_OpenClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor <> crHourGlass then
    begin
      MainFrm_OpenDialog.FileName := '';
      if MainFrm_OpenDialog.Execute then
      begin
        MainFrm_MainMenu_File_Close.Click;
        App_ArcName := MainFrm_OpenDialog.FileName;

        Caption := Application.Title + ' - ' + ExtractFileName (App_ArcName);
        MainFrm_ListView.PathBoxSign := IncludeTrailingBackslash (ExtractFileName (App_ArcName));

        App_Params.Clear;
        App_Params.Add ('l');
        App_Params.Add ('-y' + App_TempDir);
        App_Params.Add ('-pri' + IntToStr (GetPriority));
        App_Params.Add (App_ArcName);
        App_Params.Add ('*\*.*');

        App_TerminateWith := twNone;

        App_Create (False);
      end;
    end else
      MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_process], mtInformation, [mbOk], 0);
  end;

  /// TMainFrm.MainFrm_MainMenu_File_CloseClick

  procedure TMainFrm.MainFrm_MainMenu_File_CloseClick (Sender: TObject);
  begin
    MainFrm_UpdateCursor (crDefault);
    MainFrm_UpdateButtons (False);
    MainFrm_DeleteDirectories;

    Caption := Application.Title;
    MainFrm_StatusBar.Panels [0].Text := '';
    MainFrm_ListView.Clear;
    MainFrm_PathBox.Clear;
    App_ArcName := '';
    App_Params.Clear;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_MoveClick

  procedure TMainFrm.MainFrm_MainMenu_File_MoveClick (Sender: TObject);
  var
    MoveTo: string;
    NewName: string;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    MoveTo := '';
    SelectDirectory (MainFrm_MSGs.Items [MainFrm_MSGs_MoveTo], '', MoveTo);

    if Length (MoveTo) > 0 then
    begin
      NewName := IncludeTrailingBackslash (MoveTo) + ExtractFileName (App_ArcName);
      if RenameFile (App_ArcName, NewName) then
        App_ArcName := NewName
      else
        MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_MoveTo_E], mtError, [mbOk], 0);
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_InfoClick

  procedure TMainFrm.MainFrm_MainMenu_File_InfoClick (Sender: TObject);
  var
    f: TInfoFrm;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;
    
    f := TInfoFrm.Create (Self);
    if f.UpdateInfo (App_ArcName, MainFrm.MainFrm_ListView.Info) then
      f.ShowModal
    else
      MessageDlg ('Can''t load archive infomations.', mtInformation, [mbOk], 0);
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_CopyClick

  procedure TMainFrm.MainFrm_MainMenu_File_CopyClick (Sender: TObject);
  var
    CopyTo: string;
    NewName: string;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    CopyTo := '';
    SelectDirectory (MainFrm_MSGs.Items [MainFrm_MSGs_CopyTo], '', CopyTo);

    if Length (CopyTo) > 0 then
    begin
      NewName := IncludeTrailingBackslash (CopyTo) + ExtractFileName (App_ArcName);
      if CopyFile (PChar (App_ArcName), PChar (NewName), True) then
        App_ArcName := NewName
      else
        MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_CopyTo_E], mtError, [mbOk], 0);
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_RenameClick

  procedure TMainFrm.MainFrm_MainMenu_File_RenameClick (Sender: TObject);
  var
    f: TRenameFrm;
    NewName: string;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    f := TRenameFrm.Create (Self);
    f.Caption := MainFrm_MSGs.Items [MainFrm_MSGs_Rename_Arc];
    f.RenameFrm_To.Text := ExtractFileName (App_ArcName);

    if f.ShowModal = mrOk then
    begin
      if  (CompareText (f.RenameFrm_From.Caption, f.RenameFrm_To.Text) <> 0) and (Length (f.RenameFrm_To.Text) > 0) then
      begin
        NewName := ExtractFilePath (App_ArcName) + f.RenameFrm_To.Text;
        if RenameFile (App_ArcName, NewName) then
        begin
          MainFrm_MainMenu_File_Close.Click;
          App_ArcName := NewName;

          Caption := Application.Title + ' - ' + ExtractFileName (App_ArcName);
          MainFrm_ListView.PathBoxSign := IncludeTrailingBackslash (ExtractFileName (App_ArcName));

          App_Params.Clear;
          App_Params.Add ('l');
          App_Params.Add ('-y' + App_TempDir);
          App_Params.Add ('-pri' + IntToStr (GetPriority));
          App_Params.Add (App_ArcName);
          App_Params.Add ('*\*.*');

          App_TerminateWith := twNone;

          App_Create (False);
        end else
          MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Rename_Arc_E], mtError, [mbOk], 0);
      end;
    end;
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_DeleteClick

  procedure TMainFrm.MainFrm_MainMenu_File_DeleteClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;
   
    if MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Delete], mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      if DeleteFile (App_ArcName) then
        MainFrm_MainMenu_File_Close.Click
      else
        MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Delete_E], mtError, [mbOk], 0);
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_File_ExitClick

  procedure TMainFrm.MainFrm_MainMenu_File_ExitClick (Sender: TObject);
  begin
    Close;
  end;

  /// TMainFrm.MainFrm_MainMenu_View_ToolBar_BtnsClick

  procedure TMainFrm.MainFrm_MainMenu_View_ToolBar_BtnsClick (Sender: TObject);
  begin
    MainFrm_ConfigFrm_Show (2);
  end;

  /// TMainFrm.MainFrm_MainMenu_View_ToolBar_AddressBarClick

  procedure TMainFrm.MainFrm_MainMenu_View_ToolBar_AddressBarClick ( Sender: TObject);
  begin
    MainFrm_MainMenu_View_ToolBar_AddressBar.Checked := not MainFrm_MainMenu_View_ToolBar_AddressBar.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_MainMenu_View_StatusBarClick

  procedure TMainFrm.MainFrm_MainMenu_View_StatusBarClick (Sender: TObject);
  begin
    MainFrm_MainMenu_View_StatusBar.Checked := not MainFrm_MainMenu_View_StatusBar.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_MainMenu_View_RowSelectClick

  procedure TMainFrm.MainFrm_MainMenu_View_RowSelectClick (Sender: TObject);
  begin
    MainFrm_MainMenu_View_RowSelect.Checked := not MainFrm_MainMenu_View_RowSelect.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_MainMenu_View_GridLinesClick

  procedure TMainFrm.MainFrm_MainMenu_View_GridLinesClick (Sender: TObject);
  begin
    MainFrm_MainMenu_View_GridLines.Checked := not MainFrm_MainMenu_View_GridLines.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_MainMenu_View_ListModeClick

  procedure TMainFrm.MainFrm_MainMenu_View_ListModeClick (Sender: TObject);
  begin
    MainFrm_MainMenu_View_ListMode.Checked := not MainFrm_MainMenu_View_ListMode.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_MainMenu_View_ ...

  procedure TMainFrm.MainFrm_MainMenu_View_IconsClick              (Sender: TObject); begin MainFrm_UpdateStyle (3); end;
  procedure TMainFrm.MainFrm_MainMenu_View_SmallIconsClick         (Sender: TObject); begin MainFrm_UpdateStyle (4); end;
  procedure TMainFrm.MainFrm_MainMenu_View_ListClick               (Sender: TObject); begin MainFrm_UpdateStyle (5); end;
  procedure TMainFrm.MainFrm_MainMenu_View_ReportClick             (Sender: TObject); begin MainFrm_UpdateStyle (6); end;

  /// TMainFrm.MainFrm_MainMenu_View_OrderBy_ ...

  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_NameClick       (Sender: TObject); begin MainFrm_UpdateOrder ( 0); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_SizeClick       (Sender: TObject); begin MainFrm_UpdateOrder ( 1); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_PackedClick     (Sender: TObject); begin MainFrm_UpdateOrder ( 2); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_RatioClick      (Sender: TObject); begin MainFrm_UpdateOrder ( 3); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_TypeClick       (Sender: TObject); begin MainFrm_UpdateOrder ( 4); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_ModifiedClick   (Sender: TObject); begin MainFrm_UpdateOrder ( 5); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_AttributesClick (Sender: TObject); begin MainFrm_UpdateOrder ( 6); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_MethodClick     (Sender: TObject); begin MainFrm_UpdateOrder ( 7); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_PasswordClick   (Sender: TObject); begin MainFrm_UpdateOrder ( 8); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_CRCClick        (Sender: TObject); begin MainFrm_UpdateOrder ( 9); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_PathClick       (Sender: TObject); begin MainFrm_UpdateOrder (10); end;
  procedure TMainFrm.MainFrm_MainMenu_View_OrderBy_PositionClick   (Sender: TObject); begin MainFrm_UpdateOrder (11); end;

  /// TMainFrm.MainFrm_ListViewColumnClick

  procedure TMainFrm.MainFrm_ListViewColumnClick (Sender: TObject; Column: TListColumn);
  begin
    MainFrm_UpdateOrder (Column.Index);
  end;

  /// TMainFrm.MainFrm_MainMenu_View_Details_ ...

  procedure TMainFrm.MainFrm_MainMenu_View_Details_NameClick       (Sender: TObject); begin MainFrm_UpdateDetails ( 0); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_SizeClick       (Sender: TObject); begin MainFrm_UpdateDetails ( 1); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_PackedClick     (Sender: TObject); begin MainFrm_UpdateDetails ( 2); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_RatioClick      (Sender: TObject); begin MainFrm_UpdateDetails ( 3); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_TypeClick       (Sender: TObject); begin MainFrm_UpdateDetails ( 4); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_ModifiedClick   (Sender: TObject); begin MainFrm_UpdateDetails ( 5); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_AttributesClick (Sender: TObject); begin MainFrm_UpdateDetails ( 6); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_MethodClick     (Sender: TObject); begin MainFrm_UpdateDetails ( 7); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_PasswordClick   (Sender: TObject); begin MainFrm_UpdateDetails ( 8); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_CRCClick        (Sender: TObject); begin MainFrm_UpdateDetails ( 9); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_PathClick       (Sender: TObject); begin MainFrm_UpdateDetails (10); end;
  procedure TMainFrm.MainFrm_MainMenu_View_Details_PositionClick   (Sender: TObject); begin MainFrm_UpdateDetails (11); end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_NewClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_NewClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_New.Checked := not MainFrm_PopupMenu_Buttons_New.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_OpenClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_OpenClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_Open.Checked := not MainFrm_PopupMenu_Buttons_Open.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_N1Click

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_N1Click (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_N1.Checked := not MainFrm_PopupMenu_Buttons_N1.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_AddClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_AddClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_Add.Checked := not MainFrm_PopupMenu_Buttons_Add.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_ExtractClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_ExtractClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_Extract.Checked := not MainFrm_PopupMenu_Buttons_Extract.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_ViewClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_ViewClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_View.Checked := not MainFrm_PopupMenu_Buttons_View.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_DeleteClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_DeleteClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_Delete.Checked := not MainFrm_PopupMenu_Buttons_Delete.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_N2Click

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_N2Click (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_N2.Checked := not MainFrm_PopupMenu_Buttons_N2.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_TestClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_TestClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_Test.Checked := not MainFrm_PopupMenu_Buttons_Test.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_InfoClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_InfoClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_Info.Checked := not MainFrm_PopupMenu_Buttons_Info.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_CheckOutClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_CheckOutClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_CheckOut.Checked := not MainFrm_PopupMenu_Buttons_CheckOut.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_N3Click

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_N3Click (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_N3.Checked := not MainFrm_PopupMenu_Buttons_N3.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_HelpClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_HelpClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_Help.Checked := not MainFrm_PopupMenu_Buttons_Help.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Buttons_ExitClick

  procedure TMainFrm.MainFrm_PopupMenu_Buttons_ExitClick (Sender: TObject);
  begin
    MainFrm_PopupMenu_Buttons_Exit.Checked := not MainFrm_PopupMenu_Buttons_Exit.Checked;
    MainFrm_UpdateButtons;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_AddClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_AddClick (Sender: TObject);
  begin
    MainFrm_AddFiles (nil, True);
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_DeleteClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_DeleteClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    if MainFrm_ListView.Selected <> nil then
    begin
      if MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_delete_files] , mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        App_Params.Clear;
        App_Params.Add ('d');
        App_Params.Add ('-y' + App_TempDir);
        App_Params.Add ('-pri' + IntToStr (GetPriority));
        App_Params.Add (App_ArcName);
        MainFrm_ProcessFiles (App_Params);

        App_TerminateWith := twNone;

        App_Create (MainFrm_MainMenu_Options_LogReport.Checked);
      end;
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_ExtractClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_ExtractClick (Sender: TObject);
  var
    f: TExtractFrm;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    f := TExtractFrm.Create (Self);
    f.ExtractFrm_Tree.Images := MainFrm_ListView.SmallImages;
    f.ExtractFrm_cOption.Enabled := not MainFrm_MainMenu_View_ListMode.Checked;

    if f.ShowModal = mrOK then
    begin
      App_Params.Clear;

      case f.ExtractFrm_ExtrDirName.Checked of
        true : App_Params.Add ('x');
        false: App_Params.Add ('e');
      end;

      case f.ExtractFrm_ExtrOverWrite.ItemIndex of
        0: App_Params.Add ('-oS');
        1: App_Params.Add ('-oA');
      end;

      if f.ExtractFrm_cOption.Enabled and f.ExtractFrm_cOption.Checked then App_Params.Add ('-c' + MainFrm_ListView.CurrDir);

      App_Params.Add ('-y' + App_TempDir);
      App_Params.Add ('-pri' + IntToStr (3 - f.ConfigFrm_Priority.ItemIndex));
      App_Params.Add (App_ArcName);

      if MAinFrm_ProcessFiles (App_Params) = 0 then App_Params.Add ('*\*.*');

      App_TerminateWith := twNone;

      ForceDirectories (f.ExtractFrm_ExtrDir.Text);
      if SetCurrentDir (f.ExtractFrm_ExtrDir.Text) then
        App_Create (MainFrm_MainMenu_Options_LogReport.Checked)
      else
        MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Set_Cur_Dir_E], mtError, [mbOk], 0);

    end;
    
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_ExtractAllClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_ExtractAllClick (Sender: TObject);
  var
    f: TSelectFrm;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    f := TSelectFrm.Create (Self);
    f.DeSelectAll (MainFrm_ListView);
    f.Free;

    MainFrm_MainMenu_Actions_Extract.Click;
  end;

  procedure TMainFrm.MainFrm_MainMenu_Actions_TestClick(Sender: TObject);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    App_Params.Clear;
    App_Params.Add ('t');
    App_Params.Add ('-y' + App_TempDir);
    App_Params.Add ('-pri' + IntToStr (GetPriority));
    App_Params.Add (App_ArcName);

    if MainFrm_ProcessFiles (App_Params) = 0 then App_Params.Add ('*\*.*');

    App_TerminateWith := twNone;

    App_Create (True);
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_RenameClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_RenameClick (Sender: TObject);
  var
    f: TRenameFrm;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    if MainFrm_ListView.Selected <> nil then
    begin
      App_Params.Clear;
      App_Params.Add ('r');
      App_Params.Add ('-y' + App_TempDir);
      App_Params.Add ('-pri' + IntToStr (GetPriority));
      App_Params.Add (App_ArcName);
      MainFrm_ProcessFiles (App_Params);

      App_RenameDir := False;
      if Length (MainFrm_ListView.Selected.SubItems [MainFrm_ListView.Columns.Count - 2]) = 0 then
      begin
        if MainFrm_ListView.SelCount = 1 then
        begin
          f := TRenameFrm.Create (Self);
          f.Caption := MainFrm_MSGs.Items [MainFrm_MSGs_rename_dir];
          f.RenameFrm_To.Text := MainFrm_ListView.Selected.Caption;
          if f.ShowModal = mrOk then
          begin
            App_RenameDir     := True;
            App_RenameDirFrom := IncludeTrailingBackslash (MainFrm_ListView.Selected.SubItems [MainFrm_ListView.Columns.Count - 3] + f.RenameFrm_From.Caption);
            App_RenameDirTo   := IncludeTrailingBackslash (MainFrm_ListView.Selected.SubItems [MainFrm_ListView.Columns.Count - 3] + f.RenameFrm_To.Text);
          end;
          f.Free;

          if App_RenameDir then
          begin
            App_TerminateWith := twNone;
            
            App_Create (MainFrm_MainMenu_Options_LogReport.Checked);
          end;
        end;

      end else
      begin
        App_TerminateWith := twNone;

        App_Create (MainFrm_MainMenu_Options_LogReport.Checked);
      end;
    end;
    
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_ViewClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_ViewClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    if MainFrm_ListView.Selected <> nil then
    begin
      if Length (MainFrm_ListView.Selected.SubItems [MainFrm_ListView.Columns.Count - 2]) = 0 then
      begin
        MainFrm_ListView.CurrDir := IncludeTrailingBackslash (MainFrm_PathBox.Items [MainFrm_PathBox.ItemIndex] + MainFrm_ListView.Selected.Caption);
        MainFrm_ListViewClick (Self);
        MainFrm_ListView.SetFocus;
      end else
      begin
        App_Params.Clear;
        App_Params.Add ('x');
        App_Params.Add ('-oA');
        App_Params.Add ('-y' + App_TempDir);
        App_Params.Add ('-pri' + IntToStr (GetPriority));
        App_Params.Add (App_ArcName);
        MainFrm_ProcessFiles (App_Params);

        App_TerminateWith := twViewExt;

        CreateDir (App_TempDir);
        if SetCurrentDir (App_TempDir) then
          App_Create (MainFrm_MainMenu_Options_LogReport.Checked)
        else
          MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Set_Cur_Dir_E], mtError, [mbOk], 0);
      end;

    end;
  end;

  /// TMainFrm.MainFrm_PopupMenu_Open_IntViewerClick

  procedure TMainFrm.MainFrm_PopupMenu_Open_IntViewerClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    if MainFrm_ListView.Selected <> nil then
    begin
      if Length (MainFrm_ListView.Selected.SubItems [MainFrm_ListView.Columns.Count - 2]) = 0 then
      begin
        MainFrm_ListView.CurrDir := IncludeTrailingBackslash (MainFrm_PathBox.Items [MainFrm_PathBox.ItemIndex] + MainFrm_ListView.Selected.Caption);
        MainFrm_ListViewClick (Self);
        MainFrm_ListView.SetFocus;
      end else
      begin
        App_Params.Clear;
        App_Params.Add ('x');
        App_Params.Add ('-oA');
        App_Params.Add ('-y' + App_TempDir);
        App_Params.Add ('-pri' + IntToStr (GetPriority));
        App_Params.Add (App_ArcName);
        MainFrm_ProcessFiles (App_Params);

        App_TerminateWith := twViewInt;

        CreateDir (App_TempDir);
        if SetCurrentDir (App_TempDir) then
          App_Create (MainFrm_MainMenu_Options_LogReport.Checked)
        else
          MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Set_Cur_Dir_E], mtError, [mbOk], 0);

      end;
      
    end;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_CheckOutClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_CheckOutClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    App_Params.Clear;
    App_Params.Add ('x');
    App_Params.Add ('-oA');
    App_Params.Add ('-y' + App_TempDir);
    App_Params.Add ('-pri' + IntToStr (GetPriority));
    App_Params.Add (App_ArcName);

    if MainFrm_ProcessFiles (App_Params) = 0 then App_Params.Add ('*\*.*');

    App_TerminateWith := twCheckOut;

    CreateDir (App_CheckOutDir);
    if SetCurrentDir (App_CheckOutDir) then
      App_Create (MainFrm_MainMenu_Options_LogReport.Checked)
    else
      MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_CheckOut_E], mtError, [mbOk], 0);

  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_TestAllClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_TestAllClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    App_Params.Clear;
    App_Params.Add ('t');
    App_Params.Add ('-y' + App_TempDir);
    App_Params.Add ('-pri' + IntToStr (GetPriority));
    App_Params.Add (App_ArcName);
    App_Params.Add ('*\*.*');

    App_TerminateWith := twNone;

    App_Create (True);
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_SelectAllClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_SelectAllClick (Sender: TObject);
  var
    f: TSelectFrm;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    MainFrm_ListView.SetFocus;
    f := TSelectFrm.Create (Self);
    f.SelectAll (MainFrm_ListView);
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_SelectMaskClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_SelectMaskClick (Sender: TObject);
  var
    f: TSelectFrm;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    MainFrm_ListView.SetFocus;
    f := TSelectFrm.Create (Self);
    f.Caption := f.SelectFrm_FrmCaptionCB.Items [0];

    if f.ShowModal = mrOk then
      f.SelectItems (MainFrm_ListView);

    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_DeselectMasksClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_UnselectMaskClick (Sender: TObject);
  var
    f: TSelectFrm;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    MainFrm_ListView.SetFocus;
    f := TSelectFrm.Create (Self);
    f.Caption := f.SelectFrm_FrmCaptionCB.Items [1];

    if f.ShowModal = mrOk then
      f.DeselectItems (MainFrm_ListView);

    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Actions_InvertClick

  procedure TMainFrm.MainFrm_MainMenu_Actions_InvertClick (Sender: TObject);
  var
    f: TSelectFrm;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    MainFrm_ListView.SetFocus;
    f := TSelectFrm.Create (Self);
    f.InvertSelection (MainFrm_ListView);
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_ConfigClick

  procedure TMainFrm.MainFrm_MainMenu_Options_ConfigClick (Sender: TObject);
  begin
    MainFrm_ConfigFrm_Show (0);
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_PasswordClick

  procedure TMainFrm.MainFrm_MainMenu_Options_PasswordClick (Sender: TObject);
  var
    f: TPasswordFrm;
  begin
    f := TPasswordFrm.Create (Self);
    f.SetKey (App_Key);

    if f.ShowModal = mrOK then
      App_Key := f.PasswordFrm_Key.Text;

    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_SaveOnExitClick

  procedure TMainFrm.MainFrm_MainMenu_Options_SaveOnExitClick (Sender: TObject);
  begin
    MainFrm_MainMenu_Options_SaveOnExit.Checked := not MainFrm_MainMenu_Options_SaveOnExit.Checked;
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_SaveNowClick

  procedure TMainFrm.MainFrm_MainMenu_Options_SaveNowClick (Sender: TObject);
  begin
    Save (False);
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_DefaultClick

  procedure TMainFrm.MainFrm_MainMenu_Options_DefaultClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor <> crHourGlass then
    begin
      if MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Default_Cfg], mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        MainFrm_MainMenu_File_Exit.Click;
        if not DeleteFile(BeeGui_Configuration.FileName) then
          MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Default_Cfg_E], mtError, [mbOk], 0)
        else
          BeeGui_ShellExecute (ParamStr (0), '');
      end;
    end else
      MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_process], mtInformation, [mbOk], 0);
  end;

  /// TMainFrm.MainFrm_MainMenu_Options_LogReportClick

  procedure TMainFrm.MainFrm_MainMenu_Options_LogReportClick (Sender: TObject);
  begin
    MainFrm_MainMenu_Options_LogReport.Checked := not MainFrm_MainMenu_Options_LogReport.Checked;
  end;

  /// TMainFrm.MainFrm_MainMenu_Help_AboutClick

  procedure TMainFrm.MainFrm_MainMenu_Help_AboutClick (Sender: TObject);
  var
    f: TForm;
  begin
    f := TAboutFrm.Create (Self);
    f.ShowModal;
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Help_F1Click
  
  procedure TMainFrm.MainFrm_MainMenu_Help_F1Click (Sender: TObject);
  begin
    BeeGui_ShellExecute (HLP_FILENAME, ExtractFilePath (ParamStr (0)));
  end;

  /// TMainFrm.MainFrm_MainMenu_Help_InternetClick

  procedure TMainFrm.MainFrm_MainMenu_Help_InternetClick (Sender: TObject);
  var
    f: TAboutFrm;
  begin
    f := TaboutFrm.Create (Self);
    f.AboutFrm_LinkClick (Self);
    f.Free;
  end;

  /// TMainFrm.MainFrm_MainMenu_Help_LicenseClick

  procedure TMainFrm.MainFrm_MainMenu_Help_LicenseClick (Sender: TObject);
  var
    f: TAboutFrm;
  begin
    f := TaboutFrm.Create (Self);
    f.BtnLicense.Click;
    f.Free;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Others Click                                                          //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.BtnUpClick

  procedure TMainFrm.BtnUpClick (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    if (MainFrm_ListView.Up = False) and (BeeGui_Configuration.GetBoolValue (UPBTN2EXIT_Class, UPBTN2EXIT_Ident, False)) then
    begin
      MainFrm_MainMenu_File_Exit.Click;
    end else
    begin
      MainFrm_ListViewClick (Self);
      MainFrm_ListView.SetFocus;
    end;
  end;

  /// TMainFrm.MainFrm_PathBoxClick

  procedure TMainFrm.MainFrm_PathBoxClick (Sender: TObject);
  var
    I: integer;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    I := MainFrm_PathBox.ItemIndex;

    if not (I = -1 ) then
      MainFrm_ListView.CurrDir := MainFrm_PathBox.Items [I];

    MainFrm_ListViewClick (Self);
    MainFrm_ListView.SetFocus;
  end;

  /// TMainFrm.MainFrm_ListViewClick

  procedure TMainFrm.MainFrm_ListViewClick (Sender: TObject);
  begin

    if MainFrm_ListView.Cursor <> crHourGlass then
    begin
      if MainFrm_ListView.SelCount = 0 then
        MainFrm_StatusBar.Panels [0].Text := Format (MainFrm_MSGs.Items [MainFrm_MSGs_n_items], [MainFrm_ListView.Items.Count])
      else
        if MainFrm_ListView.SelCount = 1 then
        begin
          if (TBeeGui_List_Node (MainFrm_ListView.CurrFiles [MainFrm_ListView.Selected.Index]).FileAttr and faDirectory) = 0 then
            MainFrm_StatusBar.Panels [0].Text :=
              MainFrm_ListView.Columns [1].Caption +  ': ' + MainFrm_ListView.Selected.SubItems [0] + '  ' +
              MainFrm_ListView.Columns [2].Caption +  ': ' + MainFrm_ListView.Selected.SubItems [1] + '  ' +
              MainFrm_ListView.Columns [3].Caption +  ': ' + MainFrm_ListView.Selected.SubItems [2]
          else
            MainFrm_StatusBar.Panels [0].Text := Format (MainFrm_MSGs.Items [MainFrm_MSGs_selected_items], [MainFrm_ListView.SelCount])
        end else
          MainFrm_StatusBar.Panels [0].Text := Format (MainFrm_MSGs.Items [MainFrm_MSGs_selected_items], [MainFrm_ListView.SelCount]);
    end;

  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  OnPopup                                                               //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_PopupMenuPopup
  
  procedure TMainFrm.MainFrm_PopupMenuPopup (Sender: TObject);
  var
    b: bool;
    I: integer;
  begin
    if MainFrm_ListView.SelCount = 0 then
      b := False
    else
      b := True;

    for I := 0 to 9 do
      MainFrm_PopupMenu.Items [I].Visible:= b;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Drag & Drop features                                                  //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.MainFrm_DragFilesTrgDrop

  procedure TMainFrm.MainFrm_DragFilesTrgDrop (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor <> crHourGlass then
      MainFrm_AddFiles (MainFrm_DragFilesTrg.FileList, True);
  end;

  /// TMainFrm.MainFrm_DragFilesSrcDropping

  procedure TMainFrm.MainFrm_DragFilesSrcDropping (Sender: TObject);
  begin
    if MainFrm_ListView.Cursor <> crHourGlass then
    begin
      App_Params.Clear;
      App_Params.Add ('x');
      App_Params.Add ('-oA');
      App_Params.Add ('-y' + App_TempDir);
      App_Params.Add ('-pri' + IntToStr (GetPriority));
      App_Params.Add (App_ArcName);
      MainFrm_ProcessFiles (App_Params);

      App_TerminateWith := twNone;

      CreateDir (App_TempDir);
      if SetCurrentDir (App_TempDir) then
      begin
        App_Create (MainFrm_MainMenu_Options_LogReport.Checked);
        repeat
          Application.ProcessMessages;
        until MainFrm.Cursor <> crHourGlass;
      end else
        MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_CheckOut_E], mtError, [mbOk], 0);

    end else
      MainFrm_DragFilesSrc.ClearFiles;
  end;

  /// TMainFrm.MainFrm_ListViewMouseDown

  procedure TMainFrm.MainFrm_ListViewMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    if Button = mbLeft then
    begin
      MainFrm_DragStartPos.x := X;
      MainFrm_DragStartPos.y := Y;
    end;
  end;

  /// TMainFrm.MainFrm_ListViewMouseMove

  procedure TMainFrm.MainFrm_ListViewMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    I: integer;
  begin
    if MainFrm_ListView.Cursor = crHourGlass then Exit;

    if (not (ssLeft in Shift)) or ((abs (MainFrm_DragStartPos.x - x) < 20) and
       (abs (MainFrm_DragStartPos.y - y) < 20)) or (MainFrm_ListView.Selected = nil) then Exit;

    with MainFrm_DragFilesSrc do
    begin
      ClearFiles;

      for I := 0 to MainFrm_ListView.Items.Count -1 do
      begin
        if MainFrm_ListView.items[I].Selected then
          AddFile (IncludeTrailingBackslash (App_TempDir) +
                   MainFrm_ListView.Items[I].SubItems [MainFrm_ListView.Columns.Count - 3] +
                   MainFrm_ListView.Items[I].Caption);
      end;

      MainFrm_DragFilesSrc.VerifyFiles := False;
      MainFrm_DragFilesTrg.Enabled := False;
      Execute;
      MainFrm_DragFilesTrg.Enabled  := True;
    end;

  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Thread events                                                         //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.App_OnOverWrite

  procedure TMainFrm.App_OnOverWrite;
  var
    f: TOverWriteFrm;
  begin
    f                              := TOverWriteFrm.Create (Self);
    f.OverWriteFrm_The.Caption     := f.OverWriteFrm_The    .Caption + ' "' + ExtractFileName (App_Interface.cFileName) + '".';
    f.OverWriteFrm_NewSize.Caption := f.OverWriteFrm_NewSize.Caption + '  ' + IntToStr (App_Interface.cFileSize);
    f.OverWriteFrm_NewDate.Caption := f.OverWriteFrm_NewDate.Caption + '  ' + DateTimeToString (FileDateToDateTime (App_Interface.cFileTime));
    f.OverWriteFrm_OldSize.Caption := f.OverWriteFrm_OldSize.Caption + '  ' + SizeToStr (SizeOfFile (App_Interface.cFileName));
    f.OverWriteFrm_OldDate.Caption := f.OverWriteFrm_OldDate.Caption + '  ' + DateTimeToString (FileDateToDateTime (FileAge (App_Interface.cFileName)));

    MainFrm_ListView.LargeImages.GetBitmap (BeeGui_GetFileImageIndex (App_Interface.cFileName, faArchive), f.OverWriteFrm_OldImage.Picture.Bitmap);
    MainFrm_ListView.LargeImages.GetBitmap (BeeGui_GetFileImageIndex (App_Interface.cFileName, faArchive), f.OverWriteFrm_NewImage.Picture.Bitmap);

    case f.ShowModal of
      mrAbort   : App_Interface.cMsg := 'Q';
      mrNoToAll : App_Interface.cMsg := 'S';
      mrYesToAll: App_Interface.cMsg := 'A';
      mrNo      : App_Interface.cMsg := 'N';
      mrYes     : App_Interface.cMsg := 'Y';
    end;

    f.Free;
  end;

  /// TMainFrm.App_OnRename

  procedure TMainFrm.App_OnRename;
  var
    f: TRenameFrm;
  begin
    if App_RenameDir = False then
    begin
      f := TRenameFrm.Create (Self);
      f.Caption := MainFrm_MSGs.Items [MainFrm_MSGs_rename_file];
      f.RenameFrm_To.Text := App_Interface.cFileName;
      begin
        if f.ShowModal = mrOk then
          App_Interface.cMsg := f.RenameFrm_To.Text
        else
         App_Interface.cMsg := '';
      end;
      f.Free;
    end else
    begin
      App_Interface.cMsg := App_Interface.cFileName;
      Delete (App_Interface.cMsg, 1, Length (App_RenameDirFrom));
      App_Interface.cMsg := App_RenameDirTo + App_Interface.cMsg;
     end;
  end;
  
  /// TMainFrm.App_OnWarning

  procedure TMainFrm.App_OnWarning;
  begin
    if Assigned (App_LogFile) = False then
      App_LogFile := TStringList.Create;

    App_LogFile.Add (App_Interface.cMsg);
  end;

  /// TMainFrm.App_OnError

  procedure TMainFrm.App_OnError;
  begin
    if Assigned (App_LogFile) = False then
      App_LogFile := TStringList.Create;

    App_LogFile.Add (App_Interface.cMsg);
  end;

  /// TMainFrm.App_OnDisplay

  procedure TMainFrm.App_OnDisplay;
  begin
    if Assigned (App_LogFile) = True then
      App_LogFile.Add (App_Interface.cMsg);

    MainFrm_StatusBar.Panels [0].Text := App_Interface.cMsg;
  end;

  /// TMainFrm.App_OnTick

  procedure TMainFrm.App_OnTick;
  begin
    MainFrm_ProgressBar.Position := App_Interface.cPercentage;
  end;

  /// TMainFrm.App_OnClear

  procedure TMainFrm.App_OnClear;
  begin
    // nothing to do!
  end;

  /// TMainFrm.App_OnList

  procedure TMainFrm.App_OnList;
  begin
    App_Interface.cList := MainFrm_ListView.Files;
  end;

  /// TMainFrm.App_OnKey

  procedure TMainFrm.App_OnKey;
  begin
    if Length (App_Key) = 0 then
      MainFrm_MainMenu_Options_PasswordClick (Self);

    App_Interface.cMsg := App_Key;
  end;

  /// TMainFrm.App_Create

  procedure TMainFrm.App_Create;
  begin
    MainFrm_UpdateButtons (False);
    MainFrm_UpdateCursor (crHourGlass);

    if CreateLogFile then
      App_LogFile := TStringList.Create;

    App := TBeeApp.Create (App_Interface, App_Params);
    App.OnTerminate := MainFrm.App_OnTerminate;
    App.Resume;
  end;

  /// TMainFrm.App_OnTerminate

  procedure TMainFrm.App_OnTerminate;
  begin
    MainFrm_ProgressBar.Position := 0;
    MainFrm_ListView.Initialize;

    if App_TerminateWith <> twViewExt then
    begin
      MainFrm_UpdateButtons (True);
      MainFrm_UpdateCursor (crDefault);
    end;
    
    App_TerminateWith_Log;
    case App_TerminateWith of
      twClose:    Close;
      twNewArc:   App_TerminateWith_New;
      twCheckOut: App_TerminateWith_CheckOut;
      twViewExt:  App_TerminateWith_View (App_Params.Strings [App_Params.Count -1], False);
      twViewInt:  App_TerminateWith_View (App_Params.Strings [App_Params.Count -1], True);
    end;
  end;

  /// TMainFrm.AppView_OnTerminate

  procedure TMainFrm.AppView_OnTerminate;
  begin
    MainFrm_UpdateButtons (True);
    MainFrm_UpdateCursor (crDefault);

    if FileAge (AppView.cFileName) > AppView.cFileTime then
    begin
      if MessageDlg (MainFrm_MSGs.Items [MainFrm_MSGs_Freshen_File], mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        App_Params.Clear;
        App_Params.Add ('a');
        App_Params.Add ('-y' + App_TempDir);
        App_Params.Add ('-f');
        App_Params.Add (App_ArcName);
        App_Params.Add (AppView.cFileName);

        App_TerminateWith := twNone;

        App_Create (MainFrm_MainMenu_Options_LogReport.Checked);
      end;
    end;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  OnTerminate Thread Mode                                               //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  /// TMainFrm.App_TerminateWith_New

  procedure TMainFrm.App_TerminateWith_New;
  begin
    MainFrm_MainMenu_Actions_AddClick (Self);
  end;

  /// TMainFrm.App_TerminateWith_View

  procedure TMainFrm.App_TerminateWith_View;
  var
    f: TIntViewerFrm;
  begin
    if FileExists (FileName) then
    begin
      if UseIntViewer = True then
      begin
        f := TIntViewerFrm.Create (Self);
        f.IntViewerFrm_Viewer.Lines.LoadFromFile (FileName);
        f.ShowModal;
        f.Free;
      end else
      begin
        AppView := TBeeGui_AppView.Create (FileName, '');
        AppView.OnTerminate := MainFrm.AppView_OnTerminate;
        AppView.Resume;
      end;
    end else
    begin
      MainFrm_UpdateButtons (True);
      MainFrm_UpdateCursor (crDefault);
    end;
  end;

  /// TMainFrm.App_TerminateWith_Log

  procedure TMainFrm.App_TerminateWith_Log;
  var
    f: TIntViewerFrm;
  begin
    if Assigned (App_LogFile) then
    begin
      if not (App_LogFile.Count = 0) then
      begin
        f := TIntViewerFrm.Create (Self);
        f.IntViewerFrm_Viewer.Lines.AddStrings (App_LogFile);
        f.ShowModal;
        f.Free;
      end;
      FreeAndNil (App_LogFile);
    end;
  end;

  ///  TMainFrm.App_TerminateWith_CheckOut

  procedure TMainFrm.App_TerminateWith_CheckOut;
  begin
    BeeGui_ShellExecute (GetCurrentDir, '');
  end;
  




end.


