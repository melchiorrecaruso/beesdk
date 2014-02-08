{
  Copyright (c) 2013 Melchiorre Caruso

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

    Main form class.

  Modifyed:

    v0.1.0 build 0642 - 2011.11.25 by Melchiorre Caruso.
}


unit bxm_MainFrm;

{$I bxm_compiler.inc}

interface

uses
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  FileUtil, TreeFilterEdit, ShortPathEdit, ListFilterEdit,
  Forms,
  Graphics,
  Menus,
  StdCtrls, FileCtrl, Spin, EditBtn, Grids,
  SysUtils,
  // ---
  bxm_Plugins,
  bxm_ArchiveListViewMgr,
  bxm_ArchiveFolderBox,
  bxm_IconList, bxm_archivetreeviewmgr, VirtualTrees;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    BackGround: TImage;
    FromFilter: TDateEdit;
    HC: THeaderControl;
    LV: TListView;
    ToFilter: TDateEdit;
    TypeFilter: TEdit;
    TypeFilterLabel: TLabel;
    FromFilterLabel: TLabel;
    Label3: TLabel;
    SearchBtn: TBitBtn;
    ClearBtn: TBitBtn;
    MinSize: TEdit;
    MaxSize: TEdit;
    NameFilter: TComboBox;
    PathFilterLabel: TLabel;
    MinSizeLabel: TLabel;
    NameFilterLabel: TLabel;
    PathFilter: TComboBox;
    IdleTimer: TIdleTimer;
    ImageList: TImageList;
    IconList: TImageList;
    MenuItem1: TMenuItem;
    AboutMenuItem: TMenuItem;
    NulMenuItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SearchPanel: TPanel;
    PreferencesMenuItem: TMenuItem;
    MainMenu: TPopupMenu;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    ShapeBotton: TShape;
    ShapeTop: TShape;
    ShareMenu: TPopupMenu;
    MaxSizeLabel: TLabel;
    ToolBar: TToolBar;
    ToolBarMenu: TToolBar;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    AddButton: TToolButton;
    ShareButton: TToolButton;
    MenuButton: TToolButton;
    ExtractButton: TToolButton;
    FindButton: TToolButton;
    MaxSizeUpDown: TUpDown;
    MinSizeUpDown: TUpDown;
    procedure ClearBtnClick(Sender: TObject);
    procedure LVData(Sender: TObject; Item: TListItem);
    procedure SearchBtnClick(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HeaderControlSectionClick(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure HeaderControlSectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure HeaderControlSectionSeparatorDblClick(
      HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure IdleTimerStartTimer(Sender: TObject);
    procedure IdleTimerStopTimer(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure ListViewClick(Sender: TObject);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);

    procedure MainMenuClose(Sender: TObject);
    procedure MenuButtonClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure ShareButtonClick(Sender: TObject);
    procedure ShareMenuClose(Sender: TObject);







  private
    { private declarations }
    ParserCommandLine: TParserCommandLine;
    ParserList: TParserList;
    Parser: TParser;
    Paths: TStringList;
  public
    { public declarations }
    procedure Adjust;

    procedure DefaultButtons;
    procedure DisableButtons;
    procedure EnableButtons;
  end;

var
  MainFrm: TMainFrm;
  FirstPoint, ThisPoint: TPoint;

implementation

{$R *.lfm}

uses
  bx_dirscanner,

  bxm_AddFrm,
  bxm_AboutFrm,
  bxm_TickFrm;

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
var
  I: longint;
begin
  ParserCommandLine := TParserCommandLine.Create;
  ParserList := TParserList.Create;
  Paths := TStringList.Create;
  Paths.CaseSensitive := FileNameCaseSensitive;
  Paths.Sorted := TRUE;

  for I := 0 to HC.Sections.Count - 1 do
  begin
    LV.Columns.Add;
  end;

  {$IFDEF MSWINDOWS}
  {$ENDIF}

  Adjust;
  DefaultButtons;
  FindButtonClick(nil);
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  Paths.Destroy;
  ParserList.Destroy;
  ParserCommandLine.Destroy;
end;

procedure TMainFrm.FormResize(Sender: TObject);
begin
  Adjust;
end;

procedure TMainFrm.FindButtonClick(Sender: TObject);
var
  I: longint;
begin
  if SearchPanel.Enabled = FALSE then
  begin
    NameFilter.Clear;
    PathFilter.Clear;
    for I := 0 to Paths.Count - 1 do
      PathFilter.AddItem(Paths[I], nil);

    ShapeBotton.Visible := TRUE;
    SearchPanel.Height  := 130;
    SearchPanel.Enabled := TRUE;
  end else
  begin
    ShapeBotton.Visible := FALSE;
    SearchPanel.Height  := 1;
    SearchPanel.Enabled := FALSE;
  end;
end;

procedure TMainFrm.SearchBtnClick(Sender: TObject);
begin
 (*
  while XNode <> nil do
  begin
    Data:=VST.GetNodeData(XNode);

    VST.VisiblePath[XNode] := TRUE;




    if PathFilter.Text <> '' then
    begin
      if AnsiCompareFileName(Data^.Column4, PathFilter.Text) <> 0 then
      begin
        VST.VisiblePath[XNode] := FALSE;
      end;
    end;

    XNode:= VST.GetNextSibling(XNode);
  end;
  VST.Refresh;
  *)
end;

procedure TMainFrm.ClearBtnClick(Sender: TObject);
begin
  NameFilter.Text := '';
  PathFilter.Text := '';

  MinSize.Text    := '';
  MaxSize.Text    := '';

  TypeFilter.Text := '';
  FromFilter.Text := '';
  ToFilter.Text   := '';
end;

procedure TMainFrm.LVData(Sender: TObject; Item: TListItem);
var
  I: longint;
  ItemType: string;
begin
  Item.Caption   := ParserList.Items[Item.Index].ItemName;
  Item.SubItems.Add(ParserList.Items[Item.Index].ItemSize);
  Item.SubItems.Add(ParserList.Items[Item.Index].ItemType);
  Item.SubItems.Add(ParserList.Items[Item.Index].ItemTime);
  Item.SubItems.Add(ParserList.Items[Item.Index].ItemPath);

  if Paths.Find(ParserList.Items[Item.Index].ItemPath, I) = FALSE then
    Paths.Add(ParserList.Items[Item.Index].ItemPath);

  ItemType := LowerCase(ParserList.Items[Item.Index].ItemType);

  if ItemType = '.avi'         then Item.ImageIndex := 0  else
  if ItemType = '.bat'         then Item.ImageIndex := 1  else
  if ItemType = '.bmp'         then Item.ImageIndex := 2  else
  if ItemType = '.png'         then Item.ImageIndex := 2  else
  if ItemType = '.cddrive'     then Item.ImageIndex := 3  else
  if ItemType = '.doc'         then Item.ImageIndex := 4  else
  if ItemType = '.exe'         then Item.ImageIndex := 5  else
  if ItemType = '.folderclose' then Item.ImageIndex := 6  else
  if ItemType = '.folderopen'  then Item.ImageIndex := 7  else
  if ItemType = '.harddrive'   then Item.ImageIndex := 8  else
  if ItemType = '.html'        then Item.ImageIndex := 9  else
  if ItemType = '.mp3'         then Item.ImageIndex := 10 else
  if ItemType = '.deb'         then Item.ImageIndex := 11 else
  if ItemType = '.pkg'         then Item.ImageIndex := 11 else
  if ItemType = '.ppd'         then Item.ImageIndex := 12 else
  if ItemType = '.ttf'         then Item.ImageIndex := 13 else
  if ItemType = '.txt'         then Item.ImageIndex := 14 else
  if ItemType = '.unknow'      then Item.ImageIndex := 15 else
  if ItemType = '.wab'         then Item.ImageIndex := 16 else
  if ItemType = '.xls'         then Item.ImageIndex := 17 else
    Item.ImageIndex := 15;
end;

procedure TMainFrm.HeaderControlSectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin

end;

procedure TMainFrm.HeaderControlSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  Adjust;
end;

procedure TMainFrm.HeaderControlSectionSeparatorDblClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin

  Adjust;
end;

procedure TMainFrm.Adjust;
var
  I, J, ColumnWidth: longint;
begin
  LV.BeginUpdate;
  LV.ScrollBars := ssNone;
  LV.AutoWidthLastColumn := FALSE;

  HC.Sections[4].Width :=
    - HC.Sections[0].Width
    - HC.Sections[1].Width
    - HC.Sections[2].Width
    - HC.Sections[3].Width + HC.Width - 10;

  for I := 0 to HC.Sections.Count - 1 do
     LV.Columns[I].Width := HC.Sections[I].Width;
  (*
  for I := LV.Columns.Count - 1 downto 0 do
  begin
    ColumnWidth := LV.Width - 20;
    for J := 0 to I - 1 do
      Dec(ColumnWidth, LV.Columns[J].Width);

    if ColumnWidth > 0 then
    begin
      LV.Columns[I].Width := ColumnWidth;
      Break;
    end else
      LV.Columns[I].Width := 0;
  end;
  *)
  LV.ScrollBars := ssAutoVertical;
  LV.EndUpdate;
end;

procedure TMainFrm.IdleTimerStartTimer(Sender: TObject);
begin
  DisableButtons;
  LV.Clear;

  TickFrm := TTickFrm. Create(Self);
  with TickFrm.ActionLabel do
    case ParserCommandLine.Command of
      cNone:     Caption := '???';
      cAdd:      Caption := 'Adding files';
      cDelete:   Caption := 'Deleting files';
      cExtract:  Caption := 'Extracting files';
      cList:     Caption := 'Listing archive';
      cTest:     Caption := 'Testing files';
      cxExtract: Caption := 'Extracting files';
    end;
  TickFrm.Show;


  Parser := TParser.Create(ParserCommandLine);
  Parser.Resume;
end;

procedure TMainFrm.IdleTimerTimer(Sender: TObject);
begin

  if TickFrm.Visible = FALSE then
  begin
    ShowMessage('ABORTED');
    Parser.Terminate;
  end;

  if Parser.Terminated then
    IdleTimer.Enabled := FALSE;
end;

procedure TMainFrm.ListViewClick(Sender: TObject);
begin

end;

procedure TMainFrm.ListViewCompare(Sender: TObject;
  Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  P1, P2: TParserItem;
begin
  P1 := TParserItem(Item1.Data);
  P2 := TParserItem(Item2.Data);

  (*
  case ListView.SortColumn of
    0:   Compare := AnsiCompareFileName(P1.ItemName, P2.ItemName);
    1:   if P1.ItemSize > P2.ItemSize then
           Compare := 1
         else
           if P1.ItemSize < P2.ItemSize then
             Compare := -1
           else
             Compare := 0;

    2:   Compare := AnsiCompareFileName(P1.ItemType, P2.ItemType);
    3:   if P1.ItemTime > P2.ItemTime then
           Compare := 1
         else
           if P1.ItemTime < P2.ItemTime then
             Compare := -1
           else
             Compare := 0;

    4:   Compare := AnsiCompareFileName(P1.ItemPath, P2.ItemPath);
    else Compare := 0;
  end;
  *)

  (*
  if ListView.SortDirection = sdDescending then
  begin
    Compare := - Compare;
  end;
  *)
end;










procedure TMainFrm.IdleTimerStopTimer(Sender: TObject);
var
  I, J: longint;
begin

  if ParserCommandLine.Command in [cList] then
  begin
    ParserList.Clear;
    ParserList.Execute(Parser);

    ClearBtnClick(Sender);

    LV.Items.Count := ParserList.Count;
  end else

    if ParserCommandLine.Command in [cAdd, cDelete] then
    begin

    end else
      if ParserCommandLine.Command in [cExtract, cTest, cxExtract] then
      begin

      end;

  FreeAndNil(Parser);
  FreeAndNil(TickFrm);

  EnableButtons;
end;

procedure TMainFrm.DefaultButtons;
begin
  ToolBar.Buttons[0].Enabled := TRUE;
  ToolBar.Buttons[1].Enabled := TRUE;
  ToolBar.Buttons[2].Enabled := FALSE;
  ToolBar.Buttons[3].Enabled := FALSE;
  ToolBar.Buttons[4].Enabled := FALSE;

  HC.Enabled := FALSE;
  HC.Visible := FALSE;
  LV.Enabled := FALSE;
  LV.Visible := FALSE;
end;

procedure TMainFrm.DisableButtons;
begin
  ToolBar.Buttons[0].Enabled := FALSE;
  ToolBar.Buttons[1].Enabled := FALSE;
  ToolBar.Buttons[2].Enabled := FALSE;
  ToolBar.Buttons[3].Enabled := FALSE;
  ToolBar.Buttons[4].Enabled := FALSE;

  HC.Enabled := FALSE;
  HC.Visible := FALSE;
  LV.Enabled := FALSE;
  LV.Visible := FALSE;
end;

procedure TMainFrm.EnableButtons;
begin
  ToolBar.Buttons[0].Enabled := TRUE;
  ToolBar.Buttons[1].Enabled := TRUE;
  ToolBar.Buttons[2].Enabled := TRUE;
  ToolBar.Buttons[3].Enabled := TRUE;
  ToolBar.Buttons[4].Enabled := TRUE;

  HC.Enabled := TRUE;
  HC.Visible := TRUE;
  LV.Enabled := TRUE;
  LV.Visible := TRUE;
end;

procedure TMainFrm.MenuButtonClick(Sender: TObject);
const
  {$IFDEF MSWINDOWS}
  T = 59; L = 3;
  {$ELSE}
  T = 59; L = 1;
  {$ENDIF}
begin
  MenuButton.Down := TRUE;
  MainMenu.PopUp(Left + ToolBarMenu.Left + MenuButton.Left + L, Top + T);
end;

procedure TMainFrm.MainMenuClose(Sender: TObject);
begin
  MenuButton.Down := FALSE;
end;

procedure TMainFrm.ShareButtonClick(Sender: TObject);
const
  {$IFDEF MSWINDOWS}
  T = 59; L = 3;
  {$ELSE}
  T = 59; L = 1;
  {$ENDIF}
begin
  ShareButton.Down := TRUE;
  ShareMenu.PopUp(Left + ToolBarMenu.Left + ShareButton.Left + L, Top  + T);
end;

procedure TMainFrm.ShareMenuClose(Sender: TObject);
begin
  ShareButton.Down := FALSE;
end;








procedure TMainFrm.AboutMenuItemClick(Sender: TObject);
begin
  AboutFrm := TAboutFrm.Create(Self);
  AboutFrm.ShowModal;
  AboutFrm.Destroy;
end;

procedure TMainFrm.OpenButtonClick(Sender: TObject);
var
  I: longint;
begin
  if OpenDialog.Execute then
  begin
    ParserCommandLine.Clear;

    ParserCommandLine.Exec := '7zip';
    ParserCommandLine.Command := cList;
    ParserCommandLine.ArchiveName := OpenDialog.FileName;
    // START
    IdleTimer.Enabled := TRUE;
  end;
end;

procedure TMainFrm.NewButtonClick(Sender: TObject);
var
  I: longint;
begin
  AddFrm := TAddFrm.Create(Self);
  if AddFrm.ShowModal = mrOk then
  begin
    ParserCommandLine.Clear;
    ParserCommandLine.Command := cAdd;
    ParserCommandLine.CompressionMode := TCompressionMode(
      Ord(AddFrm.CompressionMethod.ItemIndex));

    ParserCommandLine.ArchiveName := AddFrm.ArchiveName;

    SetCurrentDir(AddFrm.Root.Text);
    for I := 0 to AddFrm.Files.Items.Count - 1 do
    begin
      case AddFrm.Files.Items[I].ImageIndex of
        0: ParserCommandLine.FileMasks   .Add(AddFrm.Files.Items[I].Text);
        1: ParserCommandLine.ExcludeMasks.Add(AddFrm.Files.Items[I].Text);
      end;
    end;

    ParserCommandLine.Recursive := AddFrm.RecurseSubdirectories.Checked;

    if AddFrm.ArchiveWithPasswordCheck.Checked then
      ParserCommandLine.Password := AddFrm.ArchiveWithPassword.Text;

    FreeAndNil(AddFrm);

    // START
    IdleTimer.Enabled := TRUE;
  end;

  if Assigned(AddFrm) then
    FreeAndNil(AddFrm);
end;

end.
