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
    FolderBox: TArchiveFolderBox;
    DownToolBarBevel: TBevel;
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
    DownToolBar: TPanel;
    FileProcess: TFileProcess;
    BtnUp: TSpeedButton;
    MMenuViewUpdate: TMenuItem;
    MMenuViewUp: TMenuItem;
    MMenuViewN4: TMenuItem;
    StatusBar: TStatusBar;
    ListView: TArchiveListView;
    LargeImages: TIconList;
    SmallImages: TIconList;
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
    ToolBar: TPanel;
    ToolBarBevel: TBevel;
    UpToolBar: TPanel;
    // ---

    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

    procedure MMenuActionsViewClick(Sender: TObject);
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
    procedure MMenuActionsAddClick(Sender: TObject);
    procedure MMenuActionsDeleteClick (Sender: TObject);
    procedure MMenuActionsExtractClick (Sender: TObject);
    procedure MMenuActionsExtractAllClick (Sender: TObject);
    procedure MMenuActionsTestClick (Sender: TObject);
    procedure MMenuActionsRenameClick (Sender: TObject);
    procedure MMenuActionsSelectAllClick (Sender: TObject);
    procedure MMenuActionsSelectMaskClick (Sender: TObject);
    procedure MainMenu_Actions_DeselectMasksClick (Sender: TObject);
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
    procedure MMenuViewUpClick(Sender: TObject);
    procedure MMenuViewUpdateClick(Sender: TObject);
    // ---
    procedure PMenuPopup(Sender: TObject);
    procedure PMenuOpenIntViewerClick(Sender: TObject);
    procedure PMenuPropertyClick(Sender: TObject);
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    // ---





    procedure OnFileViewTimer(Sender: TObject);

    procedure OnFileUpdateTimer(Sender: TObject);
    
    procedure FolderBoxSelect(Sender: TObject);


    // ---
    procedure ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewStyleClick(Sender: TObject);
    procedure ViewStyleClick(Sender: TObject);
    procedure OrderByClick(Sender: TObject);
    procedure DetailsClick(Sender: TObject);
    procedure OptionsClick(Sender: TObject);

    procedure BMenuClick(Sender: TObject);
  private
    FWorking: boolean;
    FArchiveName: string;
    FCommandLine: TCustomCommandLine;
    procedure SetArchiveName(const aArchiveName: string);
    procedure OpenArchive(const aArchiveName: string);
    procedure Execute(const aArchiveName: string);
    procedure OpenFile(const aFileName: string);
  private
    { private declarations }
    procedure UpdateStyle;
    procedure UpdateButtons; overload;
    procedure UpdateButtons(Value: boolean); overload;
    procedure UpdateCursor(Value: TCursor);
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
    SmallImages.IconFolder := ExtractFilePath(ParamStr(0)) + 'smallicons';
    LargeImages.IconFolder := ExtractFilePath(ParamStr(0)) + 'largeicons';

    // --- //
    UpdateButtons(False);
    // --- //
    LoadLanguage;
    LoadProperty;
    // --- //
    {$IFDEF MSWINDOWS}
    ToolBar.BevelOuter := bvLowered;
    ToolBar.BevelInner := bvRaised;
    ToolBarBevel.Visible := True;
    UpToolBar.BevelOuter := bvNone;
    DownToolBar.BevelOuter := bvNone;
    DownToolBar.BorderSpacing.Bottom := 2;
    ListView.BorderSpacing.Top := 4;
    {$ENDIF}
    FWorking := False;
    FCommandLine := TCustomCommandLine.Create(False);
    // --- //
    Caption := GetApplicationCaption(rsWelcome);
  end;

  procedure TMainFrm.FormDestroy(Sender: TObject);
  begin
    FCommandLine.Destroy;
    FWorking := False;
  end;

  procedure TMainFrm.FormShow(Sender: TObject);
  begin
    UpdateButtons;
    UpdateStyle;
  end;

  procedure TMainFrm.ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  var
    I: integer;
  begin
     StatusBar.Panels[0].Text := rsSelectedItems + IntToStr(ListView.SelCount);
     StatusBar.Panels[1].Text := SizeToStr(ListView.SelFileSize);
     StatusBar.Panels[2].Text := SizeToStr(ListView.SelFilePackedSize);
     try
       StatusBar.Panels[3].Text := DateTimeToString(
         FileDateToDateTime(ListView.SelFileTime));
     except
       StatusBar.Panels[3].Text := '';
     end
  end;

  procedure TMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    MMenuFileCloseClick(Sender);
    if MMenuOptionsSaveOnExit.Checked then
    begin
      ConfigFrm.SaveProperty;
      SaveProperty;
    end;
    {$IFDEF DEBUG}
      SaveLanguage;
    {$ENDIF}
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
    // ---
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
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  ListView                                                              //
  //                                                                        //
  // ---------------------------------------------------------------------- //
  
  procedure TMainFrm.ColumnClick(Sender: TObject; Column: TListColumn);
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

  procedure TMainFrm.FolderBoxSelect(Sender: TObject);
  begin
    ListView.Folder := FolderBox.Text;
  end;
  

  

  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  procedure TMainFrm.OnFileViewTimer(Sender: TObject);
  begin
    (*
    with ArcProcess do
      if Running = False then
      begin
        Idle.Enabled := False;
        Idle.OnTimer := nil;
        if ArcProcess.ExitStatus = 0 then
        begin
          FileProcess.Execute;
          Idle.OnTimer := OnFileUpdateTimer;
          Idle.Enabled := True;
        end;
      end;
    *)
  end;
  
  procedure TMainFrm.OnFileUpdateTimer(Sender: TObject);
  var
    F: TViewFrm;
  begin
    (*
    with FileProcess do
      if Running = False then
      begin
        Idle.Enabled := False;
        Idle.OnTimer := nil;
        if FileProcess.FileIsUpdated then
        begin
          ShowMessage('2# Aggiornare file');
        end;
      end;
    *)
  end;




  procedure TMainFrm.UpdateCursor(Value: TCursor);
  begin
    Cursor := Value;
  end;

  procedure TMainFrm.UpdateButtons(Value: boolean);
  var
    I: integer;
  begin
    BtnAdd.Enabled := Value;
    BtnAdd.Font := Font;

    BtnExtract.Enabled := Value;
    BtnExtract.Font := Font;

    BtnView.Enabled := Value;
    BtnView.Font := Font;

    BtnDelete.Enabled := Value;
    BtnDelete.Font := Font;

    BtnTest.Enabled := Value;
    BtnTest.Font := Font;

    BtnCheckOut.Enabled := Value;
    BtnCheckOut.Font := Font;
    // ---
    MMenuFileClose   .Enabled := Value;
    MMenuFileProperty.Enabled := Value;
    MMenuFileMove    .Enabled := Value;
    MMenuFileCopy    .Enabled := Value;
    MMenuFileRename  .Enabled := Value;
    MMenuFileDelete  .Enabled := Value;
    // ---
    for I := 0 to MMenuActions.Count -1 do
    begin
      MMenuActions.Items[I].Enabled := Value;
    end;
    BtnUp.Enabled := Value;
    BtnUp.Font := Font;
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
      DownToolBar.Visible := False
    else
      DownToolBar.Visible := not ListView.SimpleList;
    MMenuViewAddressBar.Enabled := not ListView.SimpleList;

    StatusBar.Visible := MMenuViewStatusBar.Checked;
  end;


  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu - File                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.Execute(const aArchiveName: string);
  var
    FTime: integer;
  begin
    if FCommandLine.Confirm then
      Visible := not ConfigFrm.HideAddFrmOption.Checked;

    if FCommandLine.Run then
    begin
      FWorking := True;
      UpdateCursor(crHourGlass);
      FTime := FileAge(aArchiveName);

      TickFrm := TTickFrm.Create(Application);
      TickFrm.Execute(FCommandLine, nil);
      repeat
        if TickFrm.CanClose then Break;
        if TickFrm.CanShow  then Break;
        Application.ProcessMessages;
      until FCommandLine.Log;

      if FCommandLine.Log then
      begin
        Visible := not ConfigFrm.HideMainFrmOption.Checked;
        TickFrm.ShowModal
      end else
        if TickFrm.CanClose = False then
        begin
          Visible := not ConfigFrm.HideMainFrmOption.Checked;
          TickFrm.ShowModal;
        end;
      if not Visible then Visible := True;

      FreeAndNil(TickFrm);
      UpdateCursor(crDefault);
      FWorking := False;

      if FileAge(aArchiveName) > FTime then
      begin
        OpenArchive(aArchiveName);
      end;
    end;
    Visible := True;
  end;

  procedure TMainFrm.OpenArchive(const aArchiveName: string);
  var
    FList: TList;
    FFolder: string;
  begin
    FWorking := True;
    UpdateCursor(crHourGlass);

    Caption := GetApplicationCaption(rsOpening);

    FCommandLine.Clear;
    FCommandLine.Command := 'L';
    FCommandLine.rOption := True;
    FCommandLine.Log := False;
    FCommandLine.ArchiveName := aArchiveName;
    FCommandLine.FileMasks.Add('*');

    if FCommandLine.Run then
    begin
      FList := TList.Create;

      TickFrm := TTickFrm.Create(Application);
      TickFrm.Execute(FCommandLine, FList);
      repeat
        Application.ProcessMessages;
        if TickFrm.CanClose then Break;
        if TickFrm.CanShow  then Break;
      until FCommandLine.Log;
      if FCommandLine.Log then
        TickFrm.ShowModal
      else
        if TickFrm.CanClose = False then
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
    UpdateCursor(crDefault);
    FWorking := False;
  end;

  procedure TMainFrm.OpenFile(const aFileName: string);
  begin
    ShowMessage('ShowFileName');
  end;

  procedure TMainFrm.SetArchiveName(const aArchiveName: string);
  begin
    FArchiveName := aArchiveName;
    Caption := GetApplicationCaption(FArchiveName);
  end;

  procedure TMainFrm.MMenuFileNewClick(Sender: TObject);
  begin
    if FWorking = False then
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
        // if FCommandLine.Run then
        begin
          Execute(SaveDialog.FileName);
        end;
      end;
    end;
  end;

  procedure TMainFrm.MMenuFileOpenClick(Sender: TObject);
  begin
    if FWorking = False then
    begin
      OpenDialog.FileName := '';
      if OpenDialog.Execute then
      begin
        ListView.AutoLoadFolderBox := True;
        MMenuFileCloseClick(Sender);
        OpenArchive(OpenDialog.FileName);
      end;
    end;
  end;

  procedure TMainFrm.MMenuFileCloseClick(Sender: TObject);
  begin
    UpdateCursor(crDefault);
    UpdateButtons(False);

    if Sender = MMenuViewUpdate then
      ListView.CloseArchive(False)
    else
      ListView.CloseArchive(True);

    SetArchiveName('');
  end;

  procedure TMainFrm.MMenuFilePropertyClick(Sender: TObject);
  var
    Ratio: cardinal;
  begin
    PropertyFrm := TInfoFrm.Create(Application);
    PropertyFrm.Caption := rsArcProperty;
    begin
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
    end;
    PropertyFrm.Pages.ActivePage := PropertyFrm.APage;
    PropertyFrm.FPage.TabVisible := False;
    PropertyFrm.APage.TabVisible := True;
    PropertyFrm.ShowModal;
    FreeAndNil(PropertyFrm);
  end;

  procedure TMainFrm.MMenuFileMoveClick(Sender: TObject);
  var
    NewName: string;
  begin
    if FWorking = False then
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
    if FWorking = False then
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
    if FWorking = False then
    begin
      RenameFrm := TRenameFrm.Create(Application);
      RenameFrm.Caption := rsRenameArc;
      RenameFrm.ToFN.Text := ExtractFileName(FArchiveName);
      RenameFrm.FromFN.Caption := ExtractFileName(FArchiveName);
      if RenameFrm.ShowModal = mrOk then
      begin
        if CompareFileName(RenameFrm.ToFN.Text, RenameFrm.FromFN.Caption) <> 0 then
        begin
          NewName := ExtractFilePath(FArchiveName) + RenameFrm.ToFN.Text;
          if RenameFile(FArchiveName, NewName) then
          begin
            MMenuFileCloseClick(Sender);
            OpenArchive(NewName);
          end else
            MessageDlg(rseRenameArc, mtError, [mbOk], 0);
        end;
      end;
      FreeAndNil(RenameFrm);
    end;
  end;

  procedure TMainFrm.MMenuFileDeleteClick(Sender: TObject);
  begin
    if FWorking = False then
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
    if FWorking = False then
    begin
      MMenuFileCloseClick(Sender);
      Close;
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
    MMenuViewReport.Checked := Sender = MMenuViewReport;
    MMenuViewList.Checked := Sender = MMenuViewList;
    UpdateStyle;
  end;
  
  procedure TMainFrm.OrderByClick(Sender: TObject);
  var
    I: integer;
  begin
    for I := 0 to MMenuViewOrderBy.Count - 1 do
    begin
      MMenuViewOrderBy.Items[I].Checked := False;
    end;
    TMenuItem(Sender).Checked := True;
    
    if Sender = MMenuViewOrderByName then
      ListView.SortCol := alvcName
    else
    if Sender = MMenuViewOrderBySize then
      ListView.SortCol := alvcSize
    else
    if Sender = MMenuViewOrderByPacked then
      ListView.SortCol := alvcPacked
    else
    if Sender = MMenuViewOrderByRatio then
      ListView.SortCol := alvcRatio
    else
    if Sender = MMenuViewOrderByType then
      ListView.SortCol := alvcType
    else
    if Sender = MMenuViewOrderByModified then
      ListView.SortCol := alvcTime
    else
    if Sender = MMenuViewOrderByAttributes then
      ListView.SortCol := alvcAttr
    else
    if Sender = MMenuViewOrderByMethod then
      ListView.SortCol := alvcMethod
    else
    if Sender = MMenuViewOrderByPassword then
      ListView.SortCol := alvcPassword
    else
    if Sender = MMenuViewOrderByCrc then
      ListView.SortCol := alvcCRC
    else
    if Sender = MMenuViewOrderByPath then
      ListView.SortCol := alvcPath
    else
    if Sender = MMenuViewOrderByPosition then
      ListView.SortCol := alvcPosition;
  end;
  
   procedure TMainFrm.DetailsClick(Sender: TObject);
   var
     I: integer;
   begin
     if Sender <> nil then
     begin
       TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
     end;
     for I := 0 to MMenuViewDetails.Count -1 do
     begin
       ListView.Columns[I].Visible := MMenuViewDetails.Items[I].Checked;
       if ListView.Columns[I].Width = 0 then
         ListView.Columns[I].Width := 50;
     end;
   end;

  procedure TMainFrm.MMenuViewUpClick(Sender: TObject);
  begin
    if FWorking = False then
    begin
      if (ListView.Up = False) and ConfigFrm.UpBtnCloseOption.Checked then
      begin
        MMenuFileExitClick(Self);
      end;
    end;
  end;

  procedure TMainFrm.MMenuViewUpdateClick(Sender: TObject);
  begin
    if FWorking = False then
    begin
      MMenuFileCloseClick(Sender);
      OpenArchive(ListView.FileName);
    end;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu - Actions                                                   //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuActionsAddClick(Sender: TObject);
  begin
    if FWorking = False then
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
    if (FWorking = False) and (ListView.SelCount <> 0) then
    begin
      if MessageDlg(rsConfirmDeleteFiles, mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        FCommandLine.Clear;
        FCommandLine.Command := 'D';
        FCommandLine.rOption := True;
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
    if (FWorking = False) and (ListView.SelCount <> 0) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'X';
      FCommandLine.Confirm := True;
      FCommandLine.rOption := True;
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
    if (FWorking = False) and (ListView.SelCount <> 0) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'X';
      FCommandLine.Confirm := True;
      FCommandLine.rOption := True;
      FCommandLine.Log := MMenuOptionsLogReport.Checked;
      ConfigFrm.ExtractOptions(ListView.Folder, FCommandLine);
      FCommandLine.FileMasks.Add('*');
      begin
        Execute(FArchiveName);
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsTestClick(Sender: TObject);
  begin
    if (FWorking = False) and (ListView.SelCount <> 0) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'T';
      FCommandLine.rOption := True;
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
    if (FWorking = False) and (ListView.SelCount <> 0) then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'R';
      FCommandLine.rOption := True;
      FCommandLine.Log := MMenuOptionsLogReport.Checked;
      FCommandLine.ArchiveName := FArchiveName;
      ListView.GetMasks(FCommandLine.FileMasks);
      begin
        Execute(FArchiveName);
      end;
    end;
  end;
  
  procedure TMainFrm.MMenuActionsViewClick(Sender: TObject);
  begin
    if ListView.Selected <> nil then
    begin
      if Pos('D', ListView.Selected.SubItems[5]) > 0 then
      begin
        ListView.Folder := IncludeTrailingBackSlash(ListView.Folder) + ListView.Selected.Caption;
      end else
        if (FWorking = False) and (ListView.SelCount = 1) then
        begin
          FCommandLine.Clear;
          FCommandLine.Command := 'X';
          FCommandLine.oOption := 'A';
          FCommandLine.rOption := False;
          FCommandLine.Log := MMenuOptionsLogReport.Checked;
          FCommandLine.ArchiveName := FArchiveName;
          ListView.GetMasks(FCommandLine.FileMasks);
          begin
            Execute(FArchiveName);
          end;
          OpenFile(FArchiveName);
        end;
    end;
  end;

  procedure TMainFrm.MMenuActionsCheckOutClick(Sender: TObject);
  begin
    if FWorking = False then
    begin

    end;
  end;

  procedure TMainFrm.MMenuActionsTestAllClick(Sender: TObject);
  begin
    if FWorking = False then
    begin
      FCommandLine.Clear;
      FCommandLine.Command := 'T';
      FCommandLine.rOption := True;
      FCommandLine.Log := True;
      FCommandLine.ArchiveName := FArchiveName;
      FCommandLine.FileMasks.Add('*');
      begin
        Execute(FArchiveName);
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsSelectAllClick(Sender: TObject);
  begin
    ListView.SetMask('*', True);
    ListView.SetFocus;
  end;
  
  procedure TMainFrm.MMenuActionsDeselectAllClick(Sender: TObject);
  begin
    ListView.SetMask('*', False);
    ListView.SetFocus;
  end;
  
  procedure TMainFrm.MMenuActionsInvertClick(Sender: TObject);
  begin
    ListView.InvertMasks;
    ListView.SetFocus;
  end;

  procedure TMainFrm.MMenuActionsSelectMaskClick(Sender: TObject);
  begin
    SelectFrm := TSelectFrm.Create(Application);
    SelectFrm.Caption := rsSelectFrmCaption;
    if SelectFrm.ShowModal = mrOk then
    begin
      ListView.SetMask(SelectFrm.Mask.Text, True);
      ListView.SetFocus;
    end;
    FreeAndNil(SelectFrm);
  end;

  procedure TMainFrm.MainMenu_Actions_DeselectMasksClick(Sender: TObject);
  begin
    SelectFrm := TSelectFrm.Create(Application);
    SelectFrm.Caption := rsDeselectFrmCaption;
    if SelectFrm.ShowModal = mrOk then
    begin
      ListView.SetMask(SelectFrm.Mask.Text, False);
      ListView.SetFocus;
    end;
    FreeAndNil(SelectFrm);
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
      if ConfigFrm.SaveButtons(BMenu) then
      begin
        UpdateButtons;
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
    if Cursor <> crHourGlass then
    begin
      if MessageDlg(rsConfirmDefault, mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        ClearDirectory(GetApplicationConfigDir(cApplicationName));
        if ShellExec(ParamStr(0), '') then
        begin
          MMenuOptionsSaveOnExit.Checked := False;
          MMenuFileExit.Click;
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
    AboutFrm.ShowModal;
    FreeAndNil(AboutFrm);
  end;

  procedure TMainFrm.MMenuHelpF1Click(Sender: TObject);
  begin
    ShellExec(ExtractFilePath(ParamStr(0)) +
      IncludeTrailingBackSlash(cApplicationDocsFolder) + cApplicationHelpFile, '');
  end;

  procedure TMainFrm.MMenuHelpInternetClick(Sender: TObject);
  begin
    ShellExec(cApplicationHomePage, '');
  end;

  procedure TMainFrm.MMenuHelpLicenseClick(Sender: TObject);
  begin
    AboutFrm := TAboutFrm.Create(Application);
    AboutFrm.BtnLicense.Click;
    FreeAndNil(AboutFrm);
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Popup menu - Events                                                   //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.PMenuOpenIntViewerClick(Sender: TObject);
  begin
    { TODO : Open with internal viewer (da terminate) }
  end;

  procedure TMainFrm.PMenuPropertyClick(Sender: TObject);
  begin
    if ListView.Selected <> nil then
    begin
      with ListView do
      begin
        if Pos('D', Selected.SubItems[5]) = 0 then
        begin
          PropertyFrm := TInfoFrm.Create(Application);
          PropertyFrm.Caption := rsFileProperty;
          begin
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
          end;
          PropertyFrm.Pages.ActivePage := PropertyFrm.FPage;
          PropertyFrm.APage.TabVisible := False;
          PropertyFrm.FPage.TabVisible := True;
          PropertyFrm.ShowModal;
          FreeAndNil(PropertyFrm);
        end;
      end;
    end;
  end;
  
  procedure TMainFrm.PMenuPopup(Sender: TObject);
  var
    Selection: integer;
  begin
    with ListView do
    begin
      if SelCount = 1 then
      begin
        if Pos('D',Selected.SubItems[5]) > 0 then
          Selection := 2
        else
          Selection := 1;
      end else
        Selection := 3;
    end;

    case Selection of
      1: begin
           PMenuOpen         .Visible := True;
           PMenuOpenIntViewer.Visible := True;
           PMenuN1           .Visible := True;
           PMenuN4           .Visible := True;
           PMenuProperty     .Visible := True;
         end;
      2: begin
           PMenuOpen         .Visible := True;
           PMenuOpenIntViewer.Visible := False;
           PMenuN1           .Visible := True;
           PMenuN4           .Visible := False;
           PMenuProperty     .Visible := False;
         end;
      3: begin
           PMenuOpen         .Visible := False;
           PMenuOpenIntViewer.Visible := False;
           PMenuN1           .Visible := False;
           PMenuN4           .Visible := False;
           PMenuProperty     .Visible := False;
         end;
    end;
  end;
  
initialization

  {$I beefm_mainfrm.lrs}

finalization
  
end.


