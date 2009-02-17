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
  Classes,
  Dialogs,
  Buttons,
  IniFiles,
  SysUtils,
  Graphics,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  LResources,
  // ---
  BeeGui_Process,
  BeeGui_IconList,
  BeeGui_CommandLine,
  BeeGui_ArchiveFolderBox,
  BeeGui_ArchiveListViewMgr;

type
  { TMainFrm }

  TMainFrm = class(TForm)
    ToolBar: TToolBar;
    AddressToolBar: TToolBar;
    FolderBox: TArchiveFolderBox;
    FolderBoxLabel: TLabel;
    ListView: TArchiveListView;
    LargeImages: TIconList;
    SmallImages: TIconList;
    StatusBar: TStatusBar;
    // ---
    FileProcess: TFileProcess;
    // ---
    BevelFirst: TBevel;
    BevelSecond: TBevel;
    BevelThird: TBevel;
    BevelFour: TBevel;
    BtnNew: TSpeedButton;
    BtnOpen: TSpeedButton;
    BtnAdd: TSpeedButton;
    BtnExtract: TSpeedButton;
    BtnView: TSpeedButton;
    BtnDelete: TSpeedButton;
    BtnTest: TSpeedButton;
    BtnCheckOut: TSpeedButton;
    BtnConfig: TSpeedButton;
    BtnHelp: TSpeedButton;
    BtnExit: TSpeedButton;
    BtnUp: TSpeedButton;
    // ---
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    FontDialog: TFontDialog;
    // ---
    MMenu: TMainMenu;
    MMenuFile: TMenuItem;
    MMenuFileNew: TMenuItem;
    MMenuFileOpen: TMenuItem;
    MMenuFileClose: TMenuItem;
    MMenuFileN1: TMenuItem;
    MMenuFileProperty: TMenuItem;
    MMenuFileN2: TMenuItem;
    MMenuFileMove: TMenuItem;
    MMenuFileCopy: TMenuItem;
    MMenuFileRename: TMenuItem;
    MMenuFileDelete: TMenuItem;
    MMenuFileN3: TMenuItem;
    MMenuFileExit: TMenuItem;
    // ---
    MMenuView: TMenuItem;
    MMenuViewToolbars: TMenuItem;
    MMenuViewButtons: TMenuItem;
    MMenuViewAddressBar: TMenuItem;
    MMenuViewStatusBar: TMenuItem;
    MMenuViewN1: TMenuItem;
    MMenuViewLargeIcons: TMenuItem;
    MMenuViewSmallIcons: TMenuItem;
    MMenuViewList: TMenuItem;
    MMenuViewReport: TMenuItem;
    MMenuViewN2: TMenuItem;
    MMenuViewRowSelect: TMenuItem;
    MMenuViewGridLines: TMenuItem;
    MMenuViewListMode: TMenuItem;
    MMenuViewN3: TMenuItem;
    MMenuViewOrderBy: TMenuItem;
    MMenuViewOrderByName: TMenuItem;
    MMenuViewOrderByCrc: TMenuItem;
    MMenuViewOrderByPath: TMenuItem;
    MMenuViewOrderByPosition: TMenuItem;
    MMenuViewOrderBySize: TMenuItem;
    MMenuViewOrderByPacked: TMenuItem;
    MMenuViewOrderByRatio: TMenuItem;
    MMenuViewOrderByType: TMenuItem;
    MMenuViewOrderByModified: TMenuItem;
    MMenuViewOrderByAttributes: TMenuItem;
    MMenuViewOrderByMethod: TMenuItem;
    MMenuViewOrderByPassword: TMenuItem;
    MMenuViewN4: TMenuItem;
    MMenuViewUp: TMenuItem;
    MMenuViewUpdate: TMenuItem;
    // ---
    MMenuViewDetails: TMenuItem;
    MMenuViewDetailsName: TMenuItem;
    MMenuViewDetailsCRC: TMenuItem;
    MMenuViewDetailsPath: TMenuItem;
    MMenuViewDetailsPosition: TMenuItem;
    MMenuViewDetailsSize: TMenuItem;
    MMenuViewDetailsPacked: TMenuItem;
    MMenuViewDetailsRatio: TMenuItem;
    MMenuViewDetailsType: TMenuItem;
    MMenuViewDetailsModified: TMenuItem;
    MMenuViewDetailsAttributes: TMenuItem;
    MMenuViewDetailsMethod: TMenuItem;
    MMenuViewDetailsPassword: TMenuItem;
    // ---
    MMenuActions: TMenuItem;
    MMenuActionsAdd: TMenuItem;
    MMenuActionsDelete: TMenuItem;
    MMenuActionsExtract: TMenuItem;
    MMenuActionsExtractAll: TMenuItem;
    MMenuActionsTest: TMenuItem;
    MMenuActionsRename: TMenuItem;
    MMenuActionsView: TMenuItem;
    MMenuActionsN1: TMenuItem;
    MMenuActionsSelectAll: TMenuItem;
    MMenuActionsSelectMask: TMenuItem;
    MMenuActionsDeselectAll: TMenuItem;
    MMenuActionsDeselectMask: TMenuItem;
    MMenuActionsInvert: TMenuItem;
    MMenuActionsN2: TMenuItem;
    MMenuActionsCheckOut: TMenuItem;
    MMenuActionsTestAll: TMenuItem;
    // ---
    MMenuOptions: TMenuItem;
    MMenuOptionsN1: TMenuItem;
    MMenuOptionsN2: TMenuItem;
    MMenuOptionsConfiguration: TMenuItem;
    MMenuOptionsSaveOnExit: TMenuItem;
    MMenuOptionsSaveNow: TMenuItem;
    MMenuOptionsDefault: TMenuItem;
    MMenuOptionsLogReport: TMenuItem;
    // ---
    MMenuHelp: TMenuItem;
    MMenuHelpN1: TMenuItem;
    MMenuHelpN2: TMenuItem;
    MMenuHelpF1: TMenuItem;
    MMenuHelpInternet: TMenuItem;
    MMenuHelpLicense: TMenuItem;
    MMenuHelpAbout: TMenuItem;
    // ---
    PMenu: TPopupMenu;
    PMenuN1: TMenuItem;
    PMenuN2: TMenuItem;
    PMenuN3: TMenuItem;
    PMenuN4: TMenuItem;
    PMenuOpen: TMenuItem;
    PMenuOpenIntViewer: TMenuItem;
    PMenuDelete: TMenuItem;
    PMenuExtract: TMenuItem;
    PMenuExtractAll: TMenuItem;
    PMenuTest: TMenuItem;
    PMenuRename: TMenuItem;
    PMenuSelectDeselect: TMenuItem;
    PMenuSelectAll: TMenuItem;
    PMenuSelectMask: TMenuItem;
    PMenuDeselectAll: TMenuItem;
    PMenuDeselectMask: TMenuItem;
    PMenuInvert: TMenuItem;
    PMenuProperty: TMenuItem;
    // ---
    BMenu: TPopupMenu;
    BMenuN1: TMenuItem;
    BMenuN2: TMenuItem;
    BMenuN3: TMenuItem;
    BMenuNew: TMenuItem;
    BMenuOpen: TMenuItem;
    BMenuAdd: TMenuItem;
    BMenuExtract: TMenuItem;
    BMenuView: TMenuItem;
    BMenuDelete: TMenuItem;
    BMenuTest: TMenuItem;
    BMenuCheckOut: TMenuItem;
    BMenuConfiguration: TMenuItem;
    BMenuHelp: TMenuItem;
    BMenuExit: TMenuItem;
    // ---
    procedure FileProcessStartTimer(Sender: TObject);
    procedure FileProcessStopTimer(Sender: TObject);
    procedure FileProcessTimer(Sender: TObject);
    // ---
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    // ---
    procedure FolderBoxSelect(Sender: TObject);
    // ---
    procedure ListViewChangeFolder(Sender: TObject);
    procedure ListViewClick(Sender: TObject);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    // ---
    procedure MMenuFileNewClick(Sender: TObject);
    procedure MMenuFileOpenClick(Sender: TObject);
    procedure MMenuFileCloseClick(Sender: TObject);
    procedure MMenuFilePropertyClick(Sender: TObject);
    procedure MMenuFileMoveClick(Sender: TObject);
    procedure MMenuFileCopyClick(Sender: TObject);
    procedure MMenuFileRenameClick(Sender: TObject);
    procedure MMenuFileDeleteClick(Sender: TObject);
    procedure MMenuFileExitClick(Sender: TObject);
    // ----
    procedure OptionsClick(Sender: TObject);
    procedure OrderByClick(Sender: TObject);
    procedure DetailsClick(Sender: TObject);
    procedure ViewStyleClick(Sender: TObject);
    procedure ListViewStyleClick(Sender: TObject);
    // ---
    procedure MMenuViewUpClick(Sender: TObject);
    procedure MMenuViewUpdateClick(Sender: TObject);
    // ----
    procedure MMenuActionsAddClick(Sender: TObject);
    procedure MMenuActionsDeleteClick (Sender: TObject);
    procedure MMenuActionsExtractClick (Sender: TObject);
    procedure MMenuActionsExtractAllClick (Sender: TObject);
    procedure MMenuActionsTestClick (Sender: TObject);
    procedure MMenuActionsRenameClick (Sender: TObject);
    procedure MMenuActionsViewClick(Sender: TObject);
    procedure MMenuActionsSelectAllClick (Sender: TObject);
    procedure MMenuActionsDeselectMaskClick(Sender: TObject);
    procedure MMenuActionsSelectMaskClick (Sender: TObject);
    procedure MMenuActionsDeselectAllClick(Sender: TObject);
    procedure MMenuActionsInvertClick (Sender: TObject);
    procedure MMenuActionsTestAllClick (Sender: TObject);
    procedure MMenuActionsCheckOutClick (Sender: TObject);
    // ---
    procedure MMenuOptionsConfigurationClick (Sender: TObject);
    procedure MMenuOptionsSaveNowClick (Sender: TObject);
    procedure MMenuOptionsDefaultClick (Sender: TObject);
    // ---
    procedure MMenuHelpF1Click (Sender: TObject);
    procedure MMenuHelpInternetClick (Sender: TObject);
    procedure MMenuHelpLicenseClick (Sender: TObject);
    procedure MMenuHelpAboutClick (Sender: TObject);
    // ---
    procedure PMenuPopup(Sender: TObject);
    procedure PMenuPropertyClick(Sender: TObject);
    // ---
    procedure BMenuClick(Sender: TObject);
  private
    FWorkStatus: integer;
    FArchiveName: string;
    FCommandLine: TCustomCommandLine;
    procedure Execute(const aArchiveName: string);
    procedure OpenArchive(const aArchiveName: string);
    procedure SetArchiveName(const aArchiveName: string);
    function  CheckWorkStatus(HideMsg: boolean): boolean;
    procedure IncWorkStatus;
    procedure DecWorkStatus;
  private
    { private declarations }
    procedure UpdateStyle;
    procedure UpdateButtons; overload;
    procedure UpdateButtons(Value: boolean); overload;
    procedure UpdateCursor(Value: TCursor);
  public
    procedure ShowAndOpenArchive(const aArchiveName: string);
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  end;

var
  MainFrm: TMainFrm;
  
implementation

uses
  Math,
  Bee_Common,

  BeeFm_ViewFrm,
  BeeGui_TickFrm,
  BeeGui_AboutFrm,

  BeeGui_Consts,
  BeeGui_Messages,
  BeeGui_SysUtils,
  BeeGui_RenameFrm,


  BeeFm_ConfigFrm,
  BeeFm_SelectFrm,
  BeeFm_PropertyFrm;

  { TMainFrm }

  {$I beefm_mainfrm_saveproperty.inc}
  {$I beefm_mainfrm_loadproperty.inc}
  {$I beefm_mainfrm_savelanguage.inc}
  {$I beefm_mainfrm_loadlanguage.inc}

  procedure TMainFrm.FormCreate(Sender: TObject);
  begin
    SmallImages.IconFolder := GetApplicationSmallIconsDir;
    LargeImages.IconFolder := GetApplicationLargeIconsDir;
    {$IFDEF UNIX}
    AddressToolBar.ButtonHeight := 30;
    FolderBox.Style := csDropDown;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    AddressToolBar.ButtonHeight := FolderBox.Height + 4;
    FolderBox.Style := csOwnerDrawFixed;
    {$ENDIF}
    UpdateButtons(False);
    // --- //
    LoadLanguage;
    LoadProperty;
    // --- //
    FWorkStatus  := 0;
    FCommandLine := TCustomCommandLine.Create(False);
    // --- //
    Caption := GetApplicationCaption(cApplicationCaption ,rsWelcome);
  end;

  procedure TMainFrm.FormDestroy(Sender: TObject);
  begin
    FCommandLine.Destroy;
    FWorkStatus := 0;
  end;

  procedure TMainFrm.FormShow(Sender: TObject);
  begin
    UpdateButtons;
    UpdateStyle;
  end;

  procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
  begin
    CanClose := CheckWorkStatus(False);
  end;

  procedure TMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    MMenuFileCloseClick(Sender);
    if MMenuOptionsSaveOnExit.Checked then
    begin
      ConfigFrm.SaveProperty;
      SaveProperty;
    end;
    {$IFDEF SAVELANGUAGE}
    SaveLanguage;
    {$ENDIF}
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  ListView routines                                                     //
  //                                                                        //
  // ---------------------------------------------------------------------- //
  
  procedure TMainFrm.ListViewColumnClick(Sender: TObject; Column: TListColumn);
  begin
    case Column.Index of
       0: OrderByClick(MMenuViewOrderByName);
       1: OrderByClick(MMenuViewOrderBySize);
       2: OrderByClick(MMenuViewOrderByPacked);
       3: OrderByClick(MMenuViewOrderByRatio);
       4: OrderByClick(MMenuViewOrderByType);
       5: OrderByClick(MMenuViewOrderByModified);
       6: OrderByClick(MMenuViewOrderByAttributes);
       7: OrderByClick(MMenuViewOrderByMethod);
       8: OrderByClick(MMenuViewOrderByPassword);
       9: OrderByClick(MMenuViewOrderByCrc);
      10: OrderByClick(MMenuViewOrderByPath);
      11: OrderByClick(MMenuViewOrderByPosition);
    end;
  end;

  procedure TMainFrm.ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
  begin
    Compare := ListView.CompareFn(Item1, Item2);
  end;

  procedure TMainFrm.ListViewChangeFolder(Sender: TObject);
  begin
    if ListView.Enabled then
    begin
      StatusBar.BeginUpdate;
      StatusBar.Panels[0].Text := rsSelectedItems + IntToStr(ListView.SelCount);
      StatusBar.Panels[1].Text := SizeToStr(ListView.SelFileSize);
      StatusBar.Panels[2].Text := SizeToStr(ListView.SelFilePackedSize);
      if ListView.SelCount > 0 then
        StatusBar.Panels[3].Text := FileTimeToString(ListView.SelFileTime)
      else
        StatusBar.Panels[3].Text := '';
      StatusBar.EndUpdate;
    end else
    begin
      StatusBar.BeginUpdate;
      StatusBar.Panels[0].Text := '';
      StatusBar.Panels[1].Text := '';
      StatusBar.Panels[2].Text := '';
      StatusBar.Panels[3].Text := '';
      StatusBar.EndUpdate;
    end;
  end;

  procedure TMainFrm.ListViewClick(Sender: TObject);
  begin
    ListViewChangeFolder(Sender);
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Folder ox routines                                                    //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.FolderBoxSelect(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      ListView.Folder := FolderBox.Text;
    end else
      FolderBox.Text := ListView.Folder;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Style routines                                                        //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.UpdateCursor(Value: TCursor);
  begin
    Cursor := Value;
  end;

  procedure TMainFrm.UpdateButtons(Value: boolean);
  var
    I: integer;
  begin
    BtnAdd     .Enabled := Value;
    BtnExtract .Enabled := Value;
    BtnView    .Enabled := Value;
    BtnDelete  .Enabled := Value;
    BtnTest    .Enabled := Value;
    BtnCheckOut.Enabled := Value and (GetOSFileManager <> '');
    // ---
    BtnUp      .Enabled := Value;
    // ---
    MMenuFileClose   .Enabled := Value;
    MMenuFileProperty.Enabled := Value;
    MMenuFileMove    .Enabled := Value;
    MMenuFileCopy    .Enabled := Value;
    MMenuFileRename  .Enabled := Value;
    MMenuFileDelete  .Enabled := Value;
    // ---
    MMenuViewUp      .Enabled := Value;
    MMenuViewUpdate  .Enabled := Value;
    // ---
    for I := 0 to MMenuActions.Count -1 do
    begin
      MMenuActions.Items[I].Enabled := Value;
    end;
    MMenuActionsCheckOut.Enabled := Value and (GetOSFileManager <> '');
    // ---
    FolderBox.Enabled := Value;
    if Value = False then
      FolderBox.Color := clInactiveBorder
    else
      FolderBox.Color := clWindow;
    // ---
    ListView.Enabled := Value;
    if Value = False then
      ListView.Color := clInactiveBorder
    else
      try
        if ListView.CanFocus then
          ListView.SetFocus;
      finally
        ListView.Color := clWindow;
      end;
  end;

  procedure TMainFrm.UpdateButtons;
  var
    Buttons: array [0..13] of TControl;
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

    for I := Low(Buttons) to High(Buttons) do
    begin
      Buttons[I].Align := alRight;
      Buttons[I].Visible := False;
      if BMenu.Items[I].Checked then
      begin
        Buttons[I].Align := alLeft;
        Buttons[I].Visible := True;
      end;
    end;
  end;

  procedure TMainFrm.UpdateStyle;
  begin
    DetailsClick(nil);

    if MMenuViewLargeIcons.Checked then
      ListView.ViewStyle := vsIcon
    else
      if MMenuViewSmallIcons.Checked then
        ListView.ViewStyle := vsSmallIcon
      else
        if MMenuViewList.Checked then
          ListView.ViewStyle := vsList
        else
          if MMenuViewReport.Checked then
            ListView.ViewStyle := vsReport;

    ListView.RowSelect  := MMenuViewRowSelect.Checked;
    ListView.GridLines  := MMenuViewGridLines.Checked;
    ListView.SimpleList := MMenuViewListMode.Checked;

    if MMenuViewAddressBar.Checked = False then
      AddressToolBar.Visible := False
    else
      AddressToolBar.Visible := not ListView.SimpleList;
    MMenuViewAddressBar.Enabled := not ListView.SimpleList;

    StatusBar.Visible := MMenuViewStatusBar.Checked;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Command line execure routines                                         //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.SetArchiveName(const aArchiveName: string);
  begin
    Caption := GetApplicationCaption(cApplicationCaption, aArchiveName);
    FArchiveName := aArchiveName;
  end;

  procedure TMainFrm.IncWorkStatus;
  begin
    if FWorkStatus = 0 then
    begin
      UpdateButtons(False);
      UpdateCursor(crHourGlass);
    end;
    Inc(FWorkStatus);
  end;

  procedure TMainFrm.DecWorkStatus;
  begin
    Dec(FWorkStatus);
    if FWorkStatus = 0 then
    begin
      UpdateButtons(True);
      UpdateCursor(crDefault);
    end;
  end;

  function TMainFrm.CheckWorkStatus(HideMsg: boolean): boolean;
  begin
    Result := FWorkStatus = 0;
    if (Result = False) and (HideMsg = False) then
    begin
      MessageDlg(rsProcessExists, mtInformation, [mbOk], 0);
    end;
  end;

  // File process timer //

  procedure TMainFrm.FileProcessStartTimer(Sender: TObject);
  begin
    IncWorkStatus;
  end;

  procedure TMainFrm.FileProcessTimer(Sender: TObject);
  begin
    // monitoring
  end;

  procedure TMainFrm.FileProcessStopTimer(Sender: TObject);
  begin
    if FileProcess.FileIsModified then
    begin
      if MessageDlg(rsFreshFile, mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        FCommandLine.Clear;
        FCommandLine.Command := 'A';
        FCommandLine.fOption := True;
        FCommandLine.uOption := False;
        FCommandLine.Confirm := False;
        FCommandLine.cdOption := ListView.Folder;
        FCommandLine.ArchiveName := FArchiveName;
        FCommandLine.FileMasks.Add(FileProcess.FileName);
        begin
          Execute(FArchiveName);
        end;
      end;
    end;
    DeleteFile(FileProcess.FileName);
    DecWorkStatus;
  end;

  // Open archive and execute commands

  procedure TMainFrm.ShowAndOpenArchive(const aArchiveName: string);
  begin
    if Visible = False then
    begin
      Visible := True;
    end;
    OpenArchive(aArchiveName);
  end;

  procedure TMainFrm.OpenArchive(const aArchiveName: string);
  var
    FList: TList;
    FFolder: string;
  begin
    IncWorkStatus;
    Caption := GetApplicationCaption(cApplicationCaption, rsOpening);

    FCommandLine.Clear;
    FCommandLine.Command := 'L';
    FCommandLine.Log := False;
    FCommandLine.ArchiveName := aArchiveName;

    FCommandLine.FileMasks.Add('*');
    FCommandLine.rOption := True;

    if FCommandLine.Run then
    begin
      FList := TList.Create;

      TickFrm := TTickFrm.Create(Application);
      TickFrm.Execute(FCommandLine, FList);
      repeat
        Application.ProcessMessages;
        if TickFrm.FrmCanClose then Break;
        if TickFrm.FrmCanShow  then Break;
      until FCommandLine.Log;
      if FCommandLine.Log then
        TickFrm.ShowModal
      else
        if TickFrm.FrmCanClose = False then
          TickFrm.ShowModal;
      FreeAndNil(TickFrm);

      if ExitCode < 2 then
      begin
        FFolder := ListView.Folder;
        if ListView.Open(aArchiveName, FList) then
          UpdateButtons(True)
        else
          UpdateButtons(False);
        ListView.Folder := FFolder;
        SetArchiveName(aArchiveName);
      end else
        SetArchiveName('');

      FList.Free;
    end;
    DecWorkStatus;
  end;

  procedure TMainFrm.Execute(const aArchiveName: string);
  var
    FArchiveAge: integer;
  begin
    if FCommandLine.Confirm then
    begin
      Visible := not ConfigFrm.HideWithAddFrmOrExtractFrmOption.Checked;
    end;

    if FCommandLine.Run then
    begin
      IncWorkStatus;
      FArchiveAge := FileAge(aArchiveName);
      TickFrm := TTickFrm.Create(Application);
      TickFrm.Execute(FCommandLine, nil);
      repeat
        if TickFrm.FrmCanClose then Break;
        if TickFrm.FrmCanShow  then Break;
        Application.ProcessMessages;
      until FCommandLine.Log;

      if FCommandLine.Log then
      begin
        Visible := not ConfigFrm.HideWithTickFrmOption.Checked;
        TickFrm.ShowModal;
      end else
        if TickFrm.FrmCanClose = False then
        begin
          Visible := not ConfigFrm.HideWithTickFrmOption.Checked;
          TickFrm.ShowModal;
        end;

      FreeAndNil(TickFrm);
      if Visible = False then
      begin
        Visible := True;
      end;
      DecWorkStatus;

      if FileAge(aArchiveName) > FArchiveAge then
      begin
        MMenuFileCloseClick(MMenuViewUpdate);
        OpenArchive(aArchiveName);
      end;
    end;

    if Visible = False then
    begin
      Visible := True;
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu - File                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuFileNewClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      SaveDialog.FileName := '';
      if SaveDialog.Execute then
      begin
        MMenuFileCloseClick(Sender);
        with SaveDialog do
        begin
          case FilterIndex of
            1: FileName := ChangeFileExt(FileName, '.bee');
            2: FileName := ChangeFileExt(FileName, '.exe');
          end;
        end;
        FCommandLine.Clear;
        FCommandLine.Command := 'A';
        FCommandLine.Confirm := True;
        FCommandLine.Log := MMenuOptionsLogReport.Checked;
        FCommandLine.ArchiveName := SaveDialog.FileName;
        ConfigFrm.AddOptions('', FCommandLine);
        begin
          Execute(SaveDialog.FileName);
        end;
      end;
    end;
  end;

  procedure TMainFrm.MMenuFileOpenClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      OpenDialog.FileName := '';
      if OpenDialog.Execute then
      begin
        MMenuFileCloseClick(Sender);
        OpenArchive(OpenDialog.FileName);
      end;
    end;
  end;

  procedure TMainFrm.MMenuFileCloseClick(Sender: TObject);
  var
    FCheckOutDir: string;
  begin
    if CheckWorkStatus(False) then
    begin
      FCheckOutDir := GetApplicationCheckoutDir(cApplicationName);
      if DirectoryIsEmpty(FCheckOutDir) = False then
      begin
        if MessageDlg(rsConfirmDeleteCheckoutDir, mtInformation, [mbYes, mbNo], 0) = mrYes then
          DeleteDirectory(IncludeTrailingBackSlash(FCheckoutDir));
      end;
      UpdateButtons(False);
      UpdateCursor(crDefault);
      if Sender = MMenuViewUpdate then
        ListView.CloseArchive(False)
      else
        ListView.CloseArchive(True);
      SetArchiveName('');
    end;
  end;

  procedure TMainFrm.MMenuFilePropertyClick(Sender: TObject);
  var
    Ratio: cardinal;
  begin
    PropertyFrm := TInfoFrm.Create(Application);
    try
      PropertyFrm.Caption := rsArcProperty;

      PropertyFrm.ANameValue.Caption    := ExtractFileName(FArchiveName);
      PropertyFrm.AVersionValue.Caption := FloatToStr(ListView.Details.Version);
      PropertyFrm.AVersionValue.Caption := FloatToStr(ListView.Details.Version);
      PropertyFrm.AFilesValue.Caption   := IntToStr  (ListView.Details.FilesCount);
      PropertyFrm.ASizeValue.Caption    := SizeToStr (ListView.Details.FilesSize);
      PropertyFrm.APackedValue.Caption  := SizeToStr (ListView.Details.FilesPacked);

      with ListView.Details do
      begin
        if FilesSize > 0 then
          Ratio := Round(100 * FilesPacked / FilesSize)
        else
          Ratio := 0;
      end;

      PropertyFrm.AR.Caption                 := IntToStr(Ratio) + '%';
      PropertyFrm.ARatioValue.Caption        := IntToStr(Ratio) + '%';
      PropertyFrm.AFilesCryptedValue.Caption := IntToStr(ListView.Details.FilesCrypted);
      PropertyFrm.AArcSizeValue.Caption      := SizeToStr(SizeOfFile(FArchiveName));
      PropertyFrm.AModifiedValue.Caption     := DateTimeToStr(FileDateToDateTime(FileAge(FArchiveName)));

      PropertyFrm.Pages.ActivePage := PropertyFrm.APage;
      PropertyFrm.FPage.TabVisible := False;
      PropertyFrm.APage.TabVisible := True;

      PropertyFrm.ShowModal;
    finally
      FreeAndNil(PropertyFrm);
    end;
  end;

  procedure TMainFrm.MMenuFileMoveClick(Sender: TObject);
  var
    NewName: string;
  begin
    if CheckWorkStatus(False) then
    begin
      NewName := '';
      if SelectDirectory(rsMoveArcTo, '', NewName) then
      begin
        NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(FArchiveName);
        if RenameFile(FArchiveName, NewName) then
          SetArchiveName(NewName)
        else
          MessageDlg(rseMoveArcTo, mtError, [mbOk], 0);
      end;
    end;
  end;

  procedure TMainFrm.MMenuFileCopyClick(Sender: TObject);
  var
    NewName: string;
  begin
    if CheckWorkStatus(False) then
    begin
      NewName := '';
      if SelectDirectory(rsCopyArcTo, '', NewName) then
      begin
        NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(FArchiveName);
        if CopyFile(FArchiveName, NewName) then
          SetArchiveName(NewName)
        else
          MessageDlg(rseCopyArcTo, mtError, [mbOk], 0);
      end;
    end;
  end;

  procedure TMainFrm.MMenuFileRenameClick(Sender: TObject);
  var
    NewName: string;
  begin
    if CheckWorkStatus(False) then
    begin
      RenameFrm := TRenameFrm.Create(Application);
      try
        RenameFrm.Caption := rsRenameArc;
        RenameFrm.ToFN.Text := ExtractFileName(FArchiveName);
        RenameFrm.FromFN.Caption := ExtractFileName(FArchiveName);
        if RenameFrm.ShowModal = mrOk then
        begin
          if CompareFileName(RenameFrm.ToFN.Text, RenameFrm.FromFN.Caption) <> 0 then
          begin
            NewName := ExtractFilePath(FArchiveName) + RenameFrm.ToFN.Text;
            if RenameFile(FArchiveName, NewName) then
              SetArchiveName(NewName)
            else
              MessageDlg(rseRenameArc, mtError, [mbOk], 0);
          end;
        end;
      finally
        FreeAndNil(RenameFrm);
      end;
    end;
  end;

  procedure TMainFrm.MMenuFileDeleteClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      if MessageDlg(rsConfirmDeleteArc, mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        if DeleteFile(FArchiveName) then
          MMenuFileCloseClick(Sender)
        else
          MessageDlg(rseDeleteArc, mtError, [mbOk], 0);
      end;
    end;
  end;

  procedure TMainFrm.MMenuFileExitClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      MMenuFileCloseClick(Sender); { --> } Close;
    end;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu - View                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.ViewStyleClick(Sender: TObject);
  begin
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
    UpdateStyle;
  end;

  procedure TMainFrm.ListViewStyleClick(Sender: TObject);
  begin
    MMenuViewLargeIcons.Checked := Sender = MMenuViewLargeIcons;
    MMenuViewSmallIcons.Checked := Sender = MMenuViewSmallIcons;
    MMenuViewReport    .Checked := Sender = MMenuViewReport;
    MMenuViewList      .Checked := Sender = MMenuViewList;
    UpdateStyle;
  end;
  
  procedure TMainFrm.OrderByClick(Sender: TObject);
  var
    I: integer;
  begin
    for I := 0 to MMenuViewOrderBy.Count -1 do
    begin
      MMenuViewOrderBy.Items[I].Checked := False;
    end;
    TMenuItem(Sender).Checked := True;

    ListView.SortBy(TMenuItem(Sender).MenuIndex);
  end;
  
  procedure TMainFrm.DetailsClick(Sender: TObject);
  var
    I: integer;
  begin
    if Sender <> nil then
    begin
      TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
    end;

    for I := 0 to Min(MMenuViewDetails.Count, ListView.Columns.Count) -1 do
      with ListView.Columns[I] do
      begin
        Visible := MMenuViewDetails.Items[I].Checked;
        if Visible then
        begin
          if Width < 10 then
            Width := 80;
        end else
          Width := 0;
      end;
  end;

  procedure TMainFrm.MMenuViewUpClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      if (ListView.Up = False) and ConfigFrm.UpBtnCloseOption.Checked then
      begin
        MMenuFileExitClick(Sender);
      end;
    end;
  end;

  procedure TMainFrm.MMenuViewUpdateClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      MMenuFileCloseClick(Sender);
      OpenArchive(ListView.FileName);
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Frm - DropFiles                                                  //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.FormDropFiles(Sender: TObject; const FileNames: array of string);
  var
    I: integer;
  begin
    if CheckWorkStatus(False) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'A';
      FCommandLine.Confirm := True;
      FCommandLine.Log := MMenuOptionsLogReport.Checked;
      ConfigFrm.AddOptions(ListView.Folder, FCommandLine);
      FCommandLine.ArchiveName := FArchiveName;
      for I := Low(FileNames) to High(FileNames) do
      begin
        FCommandLine.FileMasks.Add(FileNames[I]);
      end;
      Execute(FArchiveName);
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu - Actions                                                   //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuActionsAddClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'A';
      FCommandLine.Confirm := True;
      FCommandLine.Log := MMenuOptionsLogReport.Checked;
      ConfigFrm.AddOptions(ListView.Folder, FCommandLine);
      FCommandLine.ArchiveName := FArchiveName;
      begin
        Execute(FArchiveName);
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsDeleteClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) and (ListView.SelCount > 0) then
    begin
      if MessageDlg(rsConfirmDeleteFiles, mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        FCommandLine.Clear;
        FCommandLine.Command := 'D';
        FCommandLine.Log := MMenuOptionsLogReport.Checked;
        FCommandLine.ArchiveName := FArchiveName;
        ListView.GetMasks(FCommandLine.FileMasks);
        begin
          Execute(FArchiveName);
        end;
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsExtractClick(Sender: TObject);
  begin
    if (ListView.SelCount = 0) then
    begin
      MMenuActionsExtractAllClick(Sender);
    end else
      if CheckWorkStatus(False) then
      begin
        FCommandLine.Clear;
        FCommandLine.Command := 'X';
        FCommandLine.Confirm := True;
        FCommandLine.Log := MMenuOptionsLogReport.Checked;
        ConfigFrm.ExtractOptions(ListView.Folder, FCommandLine);
        FCommandLine.ArchiveName := FArchiveName;
        ListView.GetMasks(FCommandLine.FileMasks);
        begin
          Execute(FArchiveName);
        end;
      end;
  end;

  procedure TMainFrm.MMenuActionsExtractAllClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'X';
      FCommandLine.Confirm := True;
      FCommandLine.Log := MMenuOptionsLogReport.Checked;
      ConfigFrm.ExtractOptions(ListView.Folder, FCommandLine);
      FCommandLine.ArchiveName := FArchiveName;

      FCommandLine.FileMasks.Add('*');
      FCommandLine.rOption := True;
      begin
        Execute(FArchiveName);
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsTestClick(Sender: TObject);
  begin
    if (ListView.SelCount = 0) then
    begin
      MMenuActionsTestAllClick(Sender);
    end else
      if CheckWorkStatus(False) then
      begin
        FCommandLine.Clear;
        FCommandLine.Command := 'T';
        FCommandLine.Log := True;
        FCommandLine.ArchiveName := FArchiveName;
        ListView.GetMasks(FCommandLine.FileMasks);
        begin
          Execute(FArchiveName);
        end;
      end;
  end;

  procedure TMainFrm.MMenuActionsRenameClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) and (ListView.SelCount > 0) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'R';
      FCommandLine.Log := MMenuOptionsLogReport.Checked;
      FCommandLine.ArchiveName := FArchiveName;
      ListView.GetMasks(FCommandLine.FileMasks);
      begin
        Execute(FArchiveName);
      end;
    end;
  end;
  
  procedure TMainFrm.MMenuActionsViewClick(Sender: TObject);
  var
    FFileName: string;
  begin
    if CheckWorkStatus(False) and (ListView.SelCount = 1) then
    begin
      FFileName := ListView.Selected.Caption;
      if Pos('D', ListView.Selected.SubItems[5]) > 0 then
      begin
        ListView.Folder := IncludeTrailingBackSlash(ListView.Folder) + FFileName;
      end else
      begin
        FCommandLine.Clear;
        FCommandLine.Command := 'E';
        FCommandLine.oOption := 'A';
        FCommandLine.Log := MMenuOptionsLogReport.Checked;
        FCommandLine.ArchiveName := FArchiveName;
        ListView.GetMasks(FCommandLine.FileMasks);
        if SetCurrentDir(GetApplicationTempDir(cApplicationName)) then
        begin
          Execute(FArchiveName);
          if (ExitCode < 2) then
          begin
            if Sender = PMenuOpenIntViewer then
              with FileProcess do Execute(ParamStr(0) + ' V', FFileName)
            else
              with FileProcess do Execute('', FFileName);
          end;
        end;
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsCheckOutClick(Sender: TObject);
  var
    FCheckOutDir: string;
  begin
    if CheckWorkStatus(False) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'X';
      FCommandLine.oOption := 'A';
      FCommandLine.Log := MMenuOptionsLogReport.Checked;
      FCommandLine.ArchiveName := FArchiveName;

      FCommandLine.FileMasks.Add('*');
      FCommandLine.rOption := True;

      FCheckOutDir := GetApplicationCheckoutDir(cApplicationName);
      ForceDirectories(FCheckOutDir);
      if SetCurrentDir(FCheckOutDir) then
      begin
        ClearDirectory(IncludeTrailingBackSlash(FCheckoutDir));
        if GetOSFileManager <> '' then
        begin
          Execute(FArchiveName);
          if (ExitCode < 2) then
          begin
            with FileProcess do Execute(GetOSFileManager, FCheckOutDir);
          end;
        end else
          MessageDlg(rseCannotFoundFM, mtError, [mbOk], 0);
      end else
        MessageDlg(rseSetCheckoutDir, mtError, [mbOk], 0);
    end;
  end;

  procedure TMainFrm.MMenuActionsTestAllClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'T';
      FCommandLine.Log := True;
      FCommandLine.ArchiveName := FArchiveName;

      FCommandLine.FileMasks.Add('*');
      FCommandLine.rOption := True;
      begin
        Execute(FArchiveName);
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsSelectAllClick(Sender: TObject);
  begin
    ListView.SetMask('*', True);
    if ListView.CanFocus then
    begin
      ListView.SetFocus;
    end;
    ListViewChangeFolder(Sender);
  end;
  
  procedure TMainFrm.MMenuActionsDeselectAllClick(Sender: TObject);
  begin
    ListView.SetMask('*', False);
    if ListView.CanFocus then
    begin
      ListView.SetFocus;
    end;
    ListViewChangeFolder(Sender);
  end;
  
  procedure TMainFrm.MMenuActionsInvertClick(Sender: TObject);
  begin
    ListView.InvertMasks;
    if ListView.CanFocus then
    begin
      ListView.SetFocus;
    end;
    ListViewChangeFolder(Sender);
  end;

  procedure TMainFrm.MMenuActionsSelectMaskClick(Sender: TObject);
  begin
    SelectFrm := TSelectFrm.Create(Application);
    try
      SelectFrm.Caption := rsSelectFrmCaption;
      if SelectFrm.ShowModal = mrOk then
      begin
        ListView.SetMask(SelectFrm.Mask.Text, True);
        if ListView.CanFocus then
        begin
          ListView.SetFocus;
        end;
        ListViewChangeFolder(Sender);
      end;
    finally
      FreeAndNil(SelectFrm);
    end;
  end;

  procedure TMainFrm.MMenuActionsDeselectMaskClick(Sender: TObject);
  begin
    SelectFrm := TSelectFrm.Create(Application);
    try
      SelectFrm.Caption := rsDeselectFrmCaption;
      if SelectFrm.ShowModal = mrOk then
      begin
        ListView.SetMask(SelectFrm.Mask.Text, False);
        if ListView.CanFocus then
        begin
          ListView.SetFocus;
        end;
        ListViewChangeFolder(Sender);
      end;
    finally
      FreeAndNil(SelectFrm);
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu - Options                                                   //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuOptionsConfigurationClick(Sender: TObject);
  begin
    ConfigFrm.LoadButtons(BMenu);
    if Sender = MMenuViewButtons then
      ConfigFrm.SetPage(2)
    else
      ConfigFrm.SetPage(0);

    if ConfigFrm.ShowModal = mrOk then
    begin
      if ConfigFrm.SaveButtons(BMenu) then
      begin
        UpdateButtons;
      end;
    end;
  end;

  procedure TMainFrm.OptionsClick(Sender: TObject);
  begin
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  end;

  procedure TMainFrm.MMenuOptionsSaveNowClick(Sender: TObject);
  begin
    ConfigFrm.SaveProperty;
    SaveProperty;
  end;

  procedure TMainFrm.MMenuOptionsDefaultClick(Sender: TObject);
  begin
    if CheckWorkStatus(False) then
    begin
      if MessageDlg(rsConfirmDefault, mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        ClearDirectory(GetApplicationConfigDir(cApplicationName));
        if ShellExec(ParamStr(0), '') then
        begin
          MMenuOptionsSaveOnExit.Checked := False;
          MMenuFileExitClick(Sender);
        end;
      end;
    end else
      MessageDlg(rsProcessExists, mtInformation, [mbOk], 0);
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu - Help                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuHelpAboutClick(Sender: TObject);
  begin
    AboutFrm := TAboutFrm.Create(Application);
    try
      AboutFrm.ShowModal;
    finally
      FreeAndNil(AboutFrm);
    end;
  end;

  procedure TMainFrm.MMenuHelpF1Click(Sender: TObject);
  var
    FHelpFile: string;
  begin
    FHelpFile := ExtractFilePath(ParamStr(0)) +
      IncludeTrailingBackSlash(cApplicationDocsFolder) + cApplicationHelpFile;

    if GetOSWebBrowser <> '' then
    begin
      ShellExec(FHelpFile, GetOSWebBrowser);
    end;
  end;

  procedure TMainFrm.MMenuHelpInternetClick(Sender: TObject);
  begin
    ShowMessage(GetOSWebBrowser);

    if GetOSWebBrowser <> '' then
    begin
      ShellExec(cApplicationHomePage, GetOSWebBrowser);
    end;
  end;

  procedure TMainFrm.MMenuHelpLicenseClick(Sender: TObject);
  begin
    AboutFrm := TAboutFrm.Create(Application);
    try
      AboutFrm.BtnLicense.Click;
    finally
      FreeAndNil(AboutFrm);
    end;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Popup menu - Events                                                   //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.PMenuPropertyClick(Sender: TObject);
  begin
    with ListView do
      if SelCount = 1 then
      begin
        if Pos('D', Selected.SubItems[5]) = 0 then
        begin
          PropertyFrm := TInfoFrm.Create(Application);
          try
            PropertyFrm.Caption := rsFileProperty;

            PropertyFrm.FNameValue.Caption      := Selected.Caption;
            PropertyFrm.FVersionValue.Caption   := FloatToStr(Details.Version);

            PropertyFrm.FSizeValue.Caption      := Selected.SubItems[0];
            PropertyFrm.FPackedValue.Caption    := Selected.SubItems[1];
            PropertyFrm.FRatioValue.Caption     := Selected.SubItems[2];
            PropertyFrm.FR.Caption              := Selected.SubItems[2];
            PropertyFrm.FAttributeValue.Caption := Selected.SubItems[5];
            PropertyFrm.FPasswordValue.Caption  := Selected.SubItems[7];
            PropertyFrm.FMethodValue.Caption    := Selected.SubItems[6];
            PropertyFrm.FModifiedValue.Caption  := Selected.SubItems[4];

            PropertyFrm.Pages.ActivePage := PropertyFrm.FPage;
            PropertyFrm.APage.TabVisible := False;
            PropertyFrm.FPage.TabVisible := True;

            PropertyFrm.ShowModal;
          finally
            FreeAndNil(PropertyFrm);
          end;
        end;
      end;
  end;
  
  procedure TMainFrm.PMenuPopup(Sender: TObject);
  var
    I: integer;
  begin
    for I := 0 to PMenu.Items.Count -1  do
    begin
      PMenu.Items[I].Visible := True;
    end;

    case ListView.SelCount of
      0: begin
           // Nil in selected
           PMenuOpen         .Visible := False;
           PMenuOpenIntViewer.Visible := False;
           PMenuN1           .Visible := False;

           PMenuExtract      .Visible := False;
           PMenuTest         .Visible := False;
           PMenuN2           .Visible := False;
           PMenuDelete       .Visible := False;
           PMenuRename       .Visible := False;

           PMenuN4           .Visible := False;
           PMenuProperty     .Visible := False;
         end;
      1: if Pos('D', ListView.Selected.SubItems[5]) > 0 then
         begin
           // A directory is selected
           PMenuOpenIntViewer.Visible := False;

           PMenuN4           .Visible := False;
           PMenuProperty     .Visible := False;
         end else
         begin
           // A file is selected
         end
    else begin
           // Some files and directories are selected
           PMenuOpen         .Visible := False;
           PMenuOpenIntViewer.Visible := False;
           PMenuN1           .Visible := False;

           PMenuN4           .Visible := False;
           PMenuProperty     .Visible := False;
         end;
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Buttons popup menu - Events                                           //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.BMenuClick(Sender: TObject);
  begin
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
    UpdateButtons;
  end;

initialization

  {$I beefm_mainfrm.lrs}
  
end.
