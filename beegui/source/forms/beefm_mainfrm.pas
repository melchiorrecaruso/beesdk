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
  IniFiles,
  SysUtils,
  Graphics,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  LResources,
  // ---
  BeeGui_IconList,
  BeeGui_ArchiveFolderBox,
  BeeGui_ArchiveListViewMgr,
  BeeGui_Process;

type
  { TMainFrm }

  TMainFrm = class(TForm)
    ArcProcess: TArcProcess;
    FileProcess: TFileProcess;
    UpToolBar: TToolBar;
    DownToolBar: TToolBar;
    FolderBox: TArchiveFolderBox;
    ListView: TArcListView;
    StatusBar: TStatusBar;
    LargeImages: TIconList;
    SmallImages: TIconList;
    Idle: TIdleTimer;
    // ---
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    FontDialog: TFontDialog;
    // ---
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
    // ---

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
    // ---
    procedure PMenuPopup(Sender: TObject);
    procedure PMenuOpenIntViewerClick(Sender: TObject);
    procedure PMenuPropertyClick(Sender: TObject);
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    // ---


    procedure OnArcTimer(Sender: TObject);
    procedure OnFileTimer(Sender: TObject);

    procedure FolderBoxSelect(Sender: TObject);


    // ---
    procedure ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewStyleClick(Sender: TObject);
    procedure ViewStyleClick(Sender: TObject);
    procedure OrderByClick(Sender: TObject);
    procedure DetailsClick(Sender: TObject);
    procedure OptionsClick(Sender: TObject);

    procedure BMenuClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    // ---
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
    UpdateButtons(False);
    LoadLanguage;
    LoadProperty;
  end;
  
  procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    if Cursor = crHourGlass then
    begin
      CanClose := MessageDlg(rsConfirmAbortProcess , mtWarning, [mbYes, mbNo], 0) = mrYes;
    end else
    begin
      CanClose := True;
    end;
  end;
  
  procedure TMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    if Cursor = crHourGlass then
    begin

    end else
    begin
      MMenuFileClose.Click;
    end;
    {$IFDEF DEBUG}
      SaveLanguage;
    {$ENDIF}
    if MMenuOptionsSaveOnExit.Checked then
    begin
      SaveProperty;
      ConfigFrm.SaveProperty;
    end;
  end;
  
  procedure TMainFrm.FormShow(Sender: TObject);
  begin
    UpdateButtons;
    UpdateStyle;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Buttons Popup Menu                                                    //
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
      Buttons[I].Visible := BMenu.Items[I].Checked;
      if Buttons[I].Visible then
      begin
        Buttons[I].Top := CurrTop;
        Buttons[I].Left := CurrLeft;
        Inc(CurrLeft, Buttons[I].Width + 4);
      end;
    end;
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







  // ---
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  procedure TMainFrm.FolderBoxSelect(Sender: TObject);
  begin
    ListView.Folder := FolderBox.Text;
  end;
  
  procedure TMainFrm.BtnUpClick(Sender: TObject);
  begin
    ListView.Up;
  end;
  

  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  procedure TMainFrm.OnArcTimer(Sender: TObject);
  var
    LastFolder: string;
  begin
    with ArcProcess do
      if Running = False then
      begin
        Idle.Enabled := False;
        Idle.OnTimer := nil;
        
        LastFolder := ListView.Folder;
        if ListView.Open(ArcName, ArcLink) then
          UpdateButtons(True)
        else
          UpdateButtons(False);
        ListView.Folder := LastFolder;
      end;
  end;
  
  procedure TMainFrm.OnFileTimer(Sender: TObject);
  var
    F: TViewFrm;
  begin
    with ArcProcess do
      if Running = False then
      begin
        Idle.Enabled := False;
        Idle.OnTimer := nil;
        FileProcess.Execute;
      end;
  end;




  procedure TMainFrm.UpdateCursor(Value: TCursor);
  begin
    MainFrm.Cursor := Value;
  end;

  procedure TMainFrm.UpdateButtons(Value: boolean);
  var
    I: integer;
  begin
    BtnAdd.Enabled := Value;
    BtnAdd.Font := MainFrm.Font;

    BtnExtract.Enabled := Value;
    BtnExtract.Font := MainFrm.Font;

    BtnView.Enabled := Value;
    BtnView.Font := MainFrm.Font;

    BtnDelete.Enabled := Value;
    BtnDelete.Font := MainFrm.Font;

    BtnTest.Enabled := Value;
    BtnTest.Font := MainFrm.Font;

    BtnCheckOut.Enabled := Value;
    BtnCheckOut.Font := MainFrm.Font;
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
    BtnUp.Font := MainFrm.Font;
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
    
    DownToolBar.Visible := not ListView.SimpleList;
    if DownToolBar.Visible then
      UpToolBar.EdgeBorders := [ebLeft, ebTop, ebRight]
    else
      UpToolBar.EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
    DownToolBar.EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  end;


  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu File                                                        //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuFileNewClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if Cursor <> crHourGlass then
    begin
      SaveDialog.FileName := '';
      if SaveDialog.Execute then
      begin
        MMenuFileClose.Click;
        with ArcProcess do
        begin
          ArcName := SaveDialog.FileName;
          case SaveDialog.FilterIndex of
            1: ArcName := ChangeFileExt(ArcName, '.bee');
            2: ArcName := ChangeFileExt(ArcName, '.exe');
          end;
          Caption := 'BeeFM' + ' - ' + ExtractFileName(ArcName);
        end;
        CmdLine := 'beegui a' + ' -2+';
        if MMenuOptionsLogReport.Checked then
          CmdLine := CmdLine + ' -1+'
        else
          CmdLine := CmdLine + ' -1-';
        CmdLine := CmdLine + ConfigFrm.AddOptions('') + ' "' + ArcProcess.ArcName + '"';
        ArcProcess.CommandLine := CmdLine;
        ArcProcess.CurrentDirectory := '';
        ArcProcess.Execute;
        Idle.OnTimer := OnArcTimer;
        Idle.Enabled := True;
      end;
    end else
      MessageDlg(rsProcessExists, mtInformation, [mbOk], 0);
  end;

  procedure TMainFrm.MMenuFileOpenClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if Cursor <> crHourGlass then
    begin
      OpenDialog.FileName := '';
      if OpenDialog.Execute then
      begin
        MMenuFileClose.Click;
        with ArcProcess do
        begin
          ArcName := OpenDialog.FileName;
          Caption := 'BeeFM' + ' - ' + ExtractFileName(ArcName);
        end;
        CmdLine := 'beegui l';
        if MMenuOptionsLogReport.Checked then
          CmdLine := CmdLine + ' -1+'
        else
          CmdLine := CmdLine + ' -1-';
        CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '"' + ' *!';
        ArcProcess.CommandLine := CmdLine;
        ArcProcess.CurrentDirectory := '';
        ArcProcess.Execute;
        Idle.OnTimer := OnArcTimer;
        Idle.Enabled := True;
      end;
    end else
      MessageDlg(rsProcessExists, mtInformation, [mbOk], 0);
  end;

  procedure TMainFrm.MMenuFileCloseClick(Sender: TObject);
  begin
    Caption := cApplicationTitle;
    ListView.CloseArchive;
    UpdateCursor(crDefault);
    UpdateButtons(False);
  end;

  procedure TMainFrm.MMenuFilePropertyClick(Sender: TObject);
  var
    F: TInfoFrm;
    Ratio: integer;
  begin
    F := TInfoFrm.Create(Self);
    F.Caption := rsArcProperty;
    begin
      F.ANameValue.Caption         := ExtractFileName(ArcProcess.ArcName);
      F.AVersionValue.Caption      := FloatToStr(ListView.Details.Version);
      F.AVersionValue.Caption      := FloatToStr(ListView.Details.Version);
      F.AFilesValue.Caption        := IntToStr(ListView.Details.FilesCount);
      F.ASizeValue.Caption         := SizeToStr(ListView.Details.FilesSize);
      F.APackedValue.Caption       := SizeToStr(ListView.Details.FilesPacked);

      with ListView.Details do
      begin
        if FilesSize <> 0 then
          Ratio := Round(100 * FilesPacked / FilesSize)
        else
          Ratio := 0;
      end;

      F.AR.Caption                 := IntToStr(Ratio) + '%';
      F.ARatioValue.Caption        := IntToStr(Ratio) + '%';
      F.AFilesCryptedValue.Caption := IntToStr(ListView.Details.FilesCrypted);
      F.AArcSizeValue.Caption      := SizeToStr(SizeOfFile(ArcProcess.ArcName));
      F.AModifiedValue.Caption     := DateTimeToStr(FileDateToDateTime(FileAge(ArcProcess.ArcName)));
    end;
    F.Pages.ActivePage := F.APage;
    F.FPage.TabVisible := False;
    F.APage.TabVisible := True;
    F.ShowModal;
    F.Free;
  end;

  procedure TMainFrm.MMenuFileMoveClick(Sender: TObject);
  var
    NewName: string;
  begin
    NewName := '';
    if SelectDirectory(rsMoveArcTo, '', NewName) then
    begin
      NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(ArcProcess.ArcName);
      if RenameFile(ArcProcess.ArcName, NewName) then
        ArcProcess.ArcName := NewName
      else
        MessageDlg(rseMoveArcTo, mtError, [mbOk], 0);
    end;
  end;

  procedure TMainFrm.MMenuFileCopyClick(Sender: TObject);
  var
    NewName: string;
  begin
    NewName := '';
    if SelectDirectory(rsCopyArcTo, '', NewName) then
    begin
      NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(ArcProcess.ArcName);
      if CopyFile(ArcProcess.ArcName, NewName) then
        ArcProcess.ArcName := NewName
      else
        MessageDlg(rseCopyArcTo, mtError, [mbOk], 0);
    end;
  end;

  procedure TMainFrm.MMenuFileRenameClick(Sender: TObject);
  var
    F: TRenameFrm;
    NewName: string;
  begin
    F := TRenameFrm.Create(Self);
    F.Caption := rsRenameArc;
    F.ToFN.Text := ExtractFileName(ArcProcess.ArcName);
    F.FromFN.Caption := ExtractFileName(ArcProcess.ArcName);
    if F.ShowModal = mrOk then
    begin
      if CompareFileName(F.ToFN.Text, F.FromFN.Caption) <> 0 then
      begin
        NewName := ExtractFilePath(ArcProcess.ArcName) + F.ToFN.Text;
        if RenameFile(ArcProcess.ArcName, NewName) then
        begin
          ArcProcess.ArcName := NewName;
          Caption := 'BeeFM' + ' - ' + ExtractFileName(ArcProcess.ArcName);
        end else
          MessageDlg(rseRenameArc, mtError, [mbOk], 0);
      end;
    end;
    F.Free;
  end;

  procedure TMainFrm.MMenuFileDeleteClick(Sender: TObject);
  begin
    if MessageDlg(rsConfirmDeleteArc, mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      if DeleteFile(ArcProcess.ArcName) then
        MMenuFileClose.Click
      else
        MessageDlg(rseDeleteArc, mtError, [mbOk], 0);
    end;
  end;

  procedure TMainFrm.MMenuFileExitClick(Sender: TObject);
  begin
    MMenuFileClose.Click;
    Close;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu View                                                        //
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

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu Action                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuActionsAddClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if Cursor <> crHourGlass then
    begin
      CmdLine := 'beegui a -2+' + ConfigFrm.AddOptions(ListView.Folder);
      if MMenuOptionsLogReport.Checked then
        CmdLine := CmdLine + ' -1+'
      else
        CmdLine := CmdLine + ' -1-';
      CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '"';
      ArcProcess.CommandLine := CmdLine;
      ArcProcess.CurrentDirectory := '';
      ArcProcess.Execute;
      Idle.OnTimer := OnArcTimer;
      Idle.Enabled := True;
    end;
  end;

  procedure TMainFrm.MMenuActionsDeleteClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if ListView.SelCount = 0 then Exit;
    if Cursor <> crHourGlass then
    begin
      if MessageDlg(rsConfirmDeleteFiles, mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        CmdLine := 'beegui d' + ConfigFrm.DeleteOptions;
        if MMenuOptionsLogReport.Checked then
          CmdLine := CmdLine + ' -1+'
        else
          CmdLine := CmdLine + ' -1-';
        CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '" ' + ListView.GetMasks;
        ArcProcess.CommandLine := CmdLine;
        ArcProcess.CurrentDirectory := '';
        ArcProcess.Execute;
        Idle.OnTimer := OnArcTimer;
        Idle.Enabled := True;
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsExtractClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if ListView.SelCount = 0 then Exit;
    if Cursor <> crHourGlass then
    begin
      CmdLine := 'beegui -2+' + ConfigFrm.ExtractOptions(ListView.Folder);
      if MMenuOptionsLogReport.Checked then
        CmdLine := CmdLine + ' -1+'
      else
        CmdLine := CmdLine + ' -1-';
      CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '" ' + ListView.GetMasks;
      ArcProcess.CommandLine := CmdLine;
      ArcProcess.CurrentDirectory := '';
      ArcProcess.Execute;
      Idle.OnTimer := nil;
      Idle.Enabled := False;
    end;
  end;

  procedure TMainFrm.MMenuActionsExtractAllClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if Cursor <> crHourGlass then
    begin
      CmdLine := 'beegui -2+' + ConfigFrm.ExtractOptions(ListView.Folder);
      if MMenuOptionsLogReport.Checked then
        CmdLine := CmdLine + ' -1+'
      else
        CmdLine := CmdLine + ' -1-';
      CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '" ' + '*!';
      ArcProcess.CommandLine := CmdLine;
      ArcProcess.CurrentDirectory := '';
      ArcProcess.Execute;
      Idle.OnTimer := nil;
      Idle.Enabled := False;
    end;
  end;

  procedure TMainFrm.MMenuActionsTestClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if ListView.SelCount = 0 then Exit;
    if Cursor <> crHourGlass then
    begin
      CmdLine := 'beegui t -1+';
      CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '" ' + ListView.GetMasks;
      ArcProcess.CommandLine := CmdLine;
      ArcProcess.CurrentDirectory := '';
      ArcProcess.Execute;
      Idle.OnTimer := nil;
      Idle.Enabled := False;
    end;
  end;

  procedure TMainFrm.MMenuActionsRenameClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if ListView.SelCount = 0 then Exit;
    if Cursor <> crHourGlass then
    begin
      CmdLine := 'beegui r -l+';
      if MMenuOptionsLogReport.Checked then
        CmdLine := CmdLine + ' -1+'
      else
        CmdLine := CmdLine + ' -1-';
      CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '" ' + ListView.GetMasks;
      ArcProcess.CommandLine := CmdLine;
      ArcProcess.CurrentDirectory := '';
      ArcProcess.Execute;
      Idle.OnTimer := OnArcTimer;
      Idle.Enabled := True;
    end;
  end;
  
  procedure TMainFrm.MMenuActionsViewClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if ListView.Selected <> nil then
    begin
      with ListView do
      begin
        if Pos('D', Selected.SubItems[5]) > 0 then
        begin
          Folder :=IncludeTrailingBackSlash(Folder) + Selected.Caption;
        end else
          if Cursor <> crHourGlass then
          begin
            CmdLine := 'beegui x -oA';
            CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '" ' + ListView.GetMasks;

             FileProcess.CurrentDirectory := GetApplicationTempDir(Application.Name);
             FileProcess.FileName := ListView.GetMasks;

             ArcProcess.CurrentDirectory := GetApplicationTempDir(Application.Name);
             ArcProcess.CommandLine := CmdLine;
             ArcProcess.Execute;

             Idle.OnTimer := OnFileTimer;
             Idle.Enabled := True;
            end;
      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsCheckOutClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin

    end;
  end;

  procedure TMainFrm.MMenuActionsTestAllClick(Sender: TObject);
  var
    CmdLine: string;
  begin
    if Cursor <> crHourGlass then
    begin
      CmdLine := 'beegui t -1+';
      CmdLine := CmdLine + ' "' + ArcProcess.ArcName + '" ' + '*!';
      ArcProcess.CommandLine := CmdLine;
      ArcProcess.CurrentDirectory := '';
      ArcProcess.Execute;
      Idle.OnTimer := nil;
      Idle.Enabled := False;
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
  var
    F: TSelectFrm;
  begin
    F := TSelectFrm.Create(Self);
    F.Caption := rsSelectFrmCaption;
    if F.ShowModal = mrOk then
    begin
      ListView.SetMask(F.Mask.Text, True);
      ListView.SetFocus;
    end;
    F.Free;
  end;

  procedure TMainFrm.MainMenu_Actions_DeselectMasksClick (Sender: TObject);
  var
    F: TSelectFrm;
  begin
    F := TSelectFrm.Create(Self);
    F.Caption := rsDeselectFrmCaption;
    if F.ShowModal = mrOk then
    begin
      ListView.SetMask(F.Mask.Text, False);
      ListView.SetFocus;
    end;
    F.Free;
  end;

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Menu Options                                                     //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuOptionsConfigurationClick(Sender: TObject);
  begin
    ConfigFrm.ShowModal;
  end;

  procedure TMainFrm.OptionsClick(Sender: TObject);
  begin
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  end;

  procedure TMainFrm.MMenuOptionsSaveNowClick(Sender: TObject);
  begin
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
  //  Main Menu Options                                                     //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.MMenuHelpAboutClick(Sender: TObject);
  var
    F: TAboutFrm;
  begin
    F := TAboutFrm.Create(Self);
    F.ShowModal;
    F.Free;
  end;

  procedure TMainFrm.MMenuHelpF1Click(Sender: TObject);
  begin
    ShellExec(ExtractFilePath(ParamStr(0)) +
      IncludeTrailingBackSlash('docs') + 'help.htm', '');
  end;

  procedure TMainFrm.MMenuHelpInternetClick(Sender: TObject);
  begin
    ShellExec(cApplicationHomePage, '');
  end;

  procedure TMainFrm.MMenuHelpLicenseClick(Sender: TObject);
  var
    F: TAboutFrm;
  begin
    F := TAboutFrm.Create(Self);
    F.BtnLicense.Click;
    F.Free;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  PopupnMenu Click                                                      //
  //                                                                        //
  // ---------------------------------------------------------------------- //

  procedure TMainFrm.PMenuOpenIntViewerClick (Sender: TObject);
  var
    F: TInfoFrm;
  begin
    F := TInfoFrm.Create(Self);
    //if F.UpdateFInfo() then
    begin
      F.ShowModal;
    end; // else
      // MessageDlg(rseReadArcProperty, mtInformation, [mbOk], 0);
    F.Free;
  end;

  procedure TMainFrm.PMenuPropertyClick(Sender: TObject);
  var
    F: TInfoFrm;
  begin
    if ListView.Selected <> nil then
      with ListView do
      begin
        if Pos('D', Selected.SubItems[5]) = 0 then
        begin
          F := TInfoFrm.Create(Self);
          F.Caption := rsFileProperty;
          begin
            F.FNameValue.Caption      := Selected.Caption;
            F.FVersionValue.Caption   := FloatToStr(Details.Version);

            F.FSizeValue.Caption      := Selected.SubItems[0];
            F.FPackedValue.Caption    := Selected.SubItems[1];
            F.FRatioValue.Caption     := Selected.SubItems[2];
            F.FR.Caption              := Selected.SubItems[2];
            F.FAttributeValue.Caption := Selected.SubItems[5];
            F.FPasswordValue.Caption  := Selected.SubItems[7];
            F.FMethodValue.Caption    := Selected.SubItems[6];
            F.FModifiedValue.Caption  := Selected.SubItems[4];
          end;
          F.Pages.ActivePage := F.FPage;
          F.APage.TabVisible := False;
          F.FPage.TabVisible := True;
          F.ShowModal;
          F.Free;
        end else
        begin
          { TODO : Property per le directory }
        end;
      end;
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  PMenu Events                                                          //
  //                                                                        //
  // ---------------------------------------------------------------------- //
  
  procedure TMainFrm.PMenuPopup(Sender: TObject);
  begin

  end;
  
initialization

  {$I beefm_mainfrm.lrs}
  
end.


