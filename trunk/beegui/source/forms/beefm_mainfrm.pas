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
  BeeGui_ArchiveProcess,
  BeeGui_ArchiveFolderBox,
  BeeGui_ArchiveListViewMgr;

type
  { TMainFrm }

  TMainFrm = class(TForm)
    UpToolBar: TToolBar;
    DownToolBar: TToolBar;
    FolderBox: TArchiveFolderBox;
    ListView: TArcListView;
    StatusBar: TStatusBar;
    LargeImages: TIconList;
    SmallImages: TIconList;
    Process: TArchiveProcess;
    ProcessTimer: TIdleTimer;
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
    procedure FormShow(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
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
    // ---



    procedure ProcessTimerTimer(Sender: TObject);
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
    procedure UpdateStyle;
    procedure UpdateButtons; overload;
    procedure UpdateButtons(Value: boolean); overload;
    procedure UpdateCursor(Value: TCursor);
  end;
  
var
  MainFrm: TMainFrm;
  
implementation

uses
  Bee_Common,

  BeeFm_ViewFrm,
  BeeGui_AboutFrm,

  BeeGui_Messages,
  BeeGui_SysUtils,
  BeeGui_RenameFrm,

  BeeFm_ConfigFrm,
  BeeFm_PropertyFrm;
  
  { TMainFrm }

  procedure TMainFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    SmallImages.IconFolder := ExtractFilePath(ParamStr(0)) + 'smallicons';
    LargeImages.IconFolder := ExtractFilePath(ParamStr(0)) + 'largeicons';
    {$I beefm_mainfrm_loadlanguage.inc}
    {$I beefm_mainfrm_loadproperty.inc}
    UpdateButtons(False);
  end;
  
  procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    if Cursor = crHourGlass then
    begin
      CanClose := MessageDlg(rsAbortProcess , mtWarning, [mbYes, mbNo], 0) = mrYes;
    end else
    begin
      CanClose := True;
    end;
  end;
  
  procedure TMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    if Cursor = crHourGlass then
    begin

    end else
    begin
      MMenuFileClose.Click;
    end;
    {$IFDEF DEBUG}
      {$I beefm_mainfrm_savelanguage.inc}
    {$ENDIF}
    if MMenuOptionsSaveOnExit.Checked then
    begin
      {$I beefm_mainfrm_saveproperty.inc}
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
  
  procedure TMainFrm.ListViewDblClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
      if ListView.Selected <> nil then
      begin
        if Length(ListView.Selected.SubItems[ListView.Columns.Count - 2]) = 0 then
        begin
          ListView.Folder :=IncludeTrailingBackSlash(ListView.Folder) + ListView.Selected.Caption;
        end else
        begin
          { TODO 2 : Estrazione e visualizzazione file }
        end;
      end;
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
  

  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  procedure TMainFrm.ProcessTimerTimer(Sender: TObject);
  var
    F: TViewFrm;
  begin
    with Process do
    begin
      if Running = False then
      begin
        ProcessTimer.Enabled := False;
        if ListView.OpenArchive(ArchiveName, ArchiveLink) then
        begin
          UpdateButtons(True);
        end;
      end;
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
    BtnAdd     .Enabled := Value;
    BtnAdd     .Font := MainFrm.Font;

    BtnExtract .Enabled := Value;
    BtnExtract .Font := MainFrm.Font;

    BtnView    .Enabled := Value;
    BtnView    .Font := MainFrm.Font;

    BtnDelete  .Enabled := Value;
    BtnDelete  .Font := MainFrm.Font;

    BtnTest    .Enabled := Value;
    BtnTest    .Font := MainFrm.Font;

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
        with Process do
        begin
          ArchiveName := SaveDialog.FileName;
          case SaveDialog.FilterIndex of
            1: ArchiveName := ChangeFileExt(ArchiveName, '.bee');
            2: ArchiveName := ChangeFileExt(ArchiveName, '.exe');
          end;
          Caption := 'BeeFM' + ' - ' + ExtractFileName(ArchiveName);
        end;
        CmdLine := 'beegui a' + ' -2+';
        if MMenuOptionsLogReport.Checked then
          CmdLine := CmdLine + ' -1+'
        else
          CmdLine := CmdLine + ' -1-';
        CmdLine := CmdLine + ConfigFrm.AddOptions + ' "' + Process.ArchiveName + '"';
        Process.CommandLine :=  CmdLine;
        Process.Execute;
        ProcessTimer.Enabled := True;
      end;
    end else
      MessageDlg('An process active', mtInformation, [mbOk], 0);
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
        with Process do
        begin
          ArchiveName := OpenDialog.FileName;
          Caption := 'BeeFM' + ' - ' + ExtractFileName(ArchiveName);
        end;
        CmdLine := 'beegui l';
        if MMenuOptionsLogReport.Checked then
          CmdLine := CmdLine + ' -1+'
        else
          CmdLine := CmdLine + ' -1-';
        CmdLine := CmdLine + ' "' + Process.ArchiveName + '"' + ' *!';
        Process.CommandLine := CmdLine;
        ProcessTimer.Enabled := True;
        Process.Execute;
      end;
    end else
      MessageDlg('An process active', mtInformation, [mbOk], 0);
  end;

  procedure TMainFrm.MMenuFileCloseClick(Sender: TObject);
  begin
    ListView.CloseArchive;
    UpdateCursor(crDefault);
    UpdateButtons(False);
    Caption := 'BeeFM';
  end;
  
  procedure TMainFrm.MMenuFilePropertyClick(Sender: TObject);
  var
    F: TInfoFrm;
  begin
    F := TInfoFrm.Create(Self);
    if F.UpdateAInfo(Process.ArchiveName, ListView.Details) then
    begin
      F.ShowModal;
    end else
      MessageDlg('Can''t load archive infomations.', mtInformation, [mbOk], 0);
    F.Free;
  end;
  
  procedure TMainFrm.MMenuFileMoveClick(Sender: TObject);
  var
    NewName: string;
  begin
    NewName := '';
    if SelectDirectory('Move archive to:', '', NewName) then
    begin
      NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(Process.ArchiveName);
      if RenameFile(Process.ArchiveName, NewName) then
        Process.ArchiveName := NewName
      else
        MessageDlg('Error moving archive', mtError, [mbOk], 0);
    end;
  end;

  procedure TMainFrm.MMenuFileCopyClick(Sender: TObject);
  var
    NewName: string;
  begin
    NewName := '';
    if SelectDirectory('Copy archive to:', '', NewName) then
    begin
      NewName := IncludeTrailingBackslash(NewName) + ExtractFileName(Process.ArchiveName);
      if CopyFile(Process.ArchiveName, NewName) then
        Process.ArchiveName := NewName
      else
        MessageDlg('Error copying archive', mtError, [mbOk], 0);
    end;
  end;

  procedure TMainFrm.MMenuFileRenameClick(Sender: TObject);
  var
    F: TRenameFrm;
    NewName: string;
  begin
    F := TRenameFrm.Create(Self);
    F.Caption := rsRenameArchive;
    F.ToFN.Text := ExtractFileName(Process.ArchiveName);
    F.FromFN.Caption := ExtractFileName(Process.ArchiveName);
    if F.ShowModal = mrOk then
    begin
      if AnsiCompareFileName(F.ToFN.Text, F.FromFN.Caption) <> 0 then
      begin
        NewName := ExtractFilePath(Process.ArchiveName) + F.ToFN.Text;
        if RenameFile(Process.ArchiveName, NewName) then
        begin
          Process.ArchiveName := NewName;
          Caption := 'BeeFM' + ' - ' + ExtractFileName(Process.ArchiveName);
        end else
          MessageDlg('Error on renaming archive', mtError, [mbOk], 0);
      end;
    end;
    F.Free;
  end;

  procedure TMainFrm.MMenuFileDeleteClick(Sender: TObject);
  begin
    if MessageDlg('Delete archive?', mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      if DeleteFile(Process.ArchiveName) then
        MMenuFileClose.Click
      else
        MessageDlg('Error on deleting archive.', mtError, [mbOk], 0);
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
      CmdLine := 'beegui a -2' + ConfigFrm.AddOptions;
      if MMenuOptionsLogReport.Checked then
        CmdLine := CmdLine + ' -1+'
      else
        CmdLine := CmdLine + ' -1-';
      CmdLine := CmdLine + ' "' + Process.ArchiveName + '"';
      Process.CommandLine := CmdLine;
      ProcessTimer.Enabled := True;
      Process.Execute;
    end;
  end;

  procedure TMainFrm.MMenuActionsDeleteClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
      if MessageDlg('Delete selected files?' , mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin

      end;
    end;
  end;

  procedure TMainFrm.MMenuActionsExtractClick(Sender: TObject);
  begin
  end;

  procedure TMainFrm.MMenuActionsExtractAllClick(Sender: TObject);
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

  procedure TMainFrm.MMenuActionsTestClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin

    end;
  end;

  procedure TMainFrm.MMenuActionsRenameClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
    end;
  end;

  procedure TMainFrm.MMenuActionsCheckOutClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin

    end;
  end;

  procedure TMainFrm.MMenuActionsTestAllClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin

    end;
  end;

  procedure TMainFrm.MMenuActionsSelectAllClick(Sender: TObject);
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

  procedure TMainFrm.MMenuActionsSelectMaskClick(Sender: TObject);
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

  procedure TMainFrm.MMenuActionsDeselectAllClick(Sender: TObject);
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

  procedure TMainFrm.MMenuActionsInvertClick(Sender: TObject);
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
  var
    Folder: string;
    Storage: TIniFile;
  begin
    {$I beefm_mainfrm_saveproperty.inc}
  end;

  procedure TMainFrm.MMenuOptionsDefaultClick(Sender: TObject);
  begin
    if Cursor <> crHourGlass then
    begin
      if MessageDlg('Default setting?', mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        ClearDirectory(GetApplicationConfigDir('BeeGui'));
        // ShellExec(ParamStr(0), '');
      end;
    end else
      MessageDlg('An active process. Please wait.', mtInformation, [mbOk], 0);
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
  var
    Help_FileName: string;
    Help_FilePath: string;
  begin
    Help_FilePath := ExtractFilePath(ParamStr(0));
    Help_FileName := Help_FilePath + 'docs' + PathDelim + 'help.htm';

    ShellExec(Help_FileName, Help_FilePath);
  end;

  procedure TMainFrm.MMenuHelpInternetClick(Sender: TObject);
  var
    F: TAboutFrm;
  begin
    F := TAboutFrm.Create(Self);
    // F.LinkClick(Self);
    F.Free;
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
  begin

  end;

  procedure TMainFrm.PMenuPropertyClick(Sender: TObject);
  begin
  
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


