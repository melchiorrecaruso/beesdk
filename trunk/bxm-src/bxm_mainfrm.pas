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
    VST: TVirtualStringTree;
    procedure ClearBtnClick(Sender: TObject);
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
    procedure VSTChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
  private
    { private declarations }
    ParserCommandLine: TParserCommandLine;
    ParserList: TParserList;
    Parser: TParser;
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

type
  PTreeData = ^TTreeData;
  TTreeData = record
    Column0: ansistring;
    Column1: ansistring;
    Column2: ansistring;
    Column3: ansistring;
    Column4: ansistring;
  end;

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
var
  I: longint;
begin
  ParserCommandLine := TParserCommandLine.Create;
  ParserList := TParserList.Create;

  {$IFDEF MSWINDOWS}
  {$ENDIF}

  Adjust;
  DefaultButtons;
  FindButtonClick(nil);
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  ParserList.Destroy;
  ParserCommandLine.Destroy;
end;

procedure TMainFrm.FormResize(Sender: TObject);
begin
  Adjust;
end;

procedure TMainFrm.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh;
end;

procedure TMainFrm.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh;
end;

procedure TMainFrm.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
begin
  Data := VST.GetNodeData(Node);
  if Assigned(Data) then
  begin
    Data^.Column0 := '';
    Data^.Column1 := '';
    Data^.Column2 := '';
    Data^.Column3 := '';
    Data^.Column4 := '';
  end;
end;

procedure TMainFrm.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TMainFrm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PTreeData;
begin
  Data := VST.GetNodeData(Node);
  case Column of
    0: CellText := Data^.Column0;
    1: CellText := Data^.Column1;
    2: CellText := Data^.Column2;
    3: CellText := Data^.Column3;
    4: CellText := Data^.Column4;
  end;
end;

procedure TMainFrm.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PTreeData;
  ItemExt: ansistring;
begin
  if Column = 0 then
  begin
    Data := VST.GetNodeData(Node);

    ItemExt := lowercase(ExtractFileExt(Data^.Column0));

    if ItemExt = '.avi'         then ImageIndex := 0  else
    if ItemExt = '.bat'         then ImageIndex := 1  else
    if ItemExt = '.bmp'         then ImageIndex := 2  else
    if ItemExt = '.png'         then ImageIndex := 2  else
    if ItemExt = '.cddrive'     then ImageIndex := 3  else
    if ItemExt = '.doc'         then ImageIndex := 4  else
    if ItemExt = '.exe'         then ImageIndex := 5  else
    if ItemExt = '.folderclose' then ImageIndex := 6  else
    if ItemExt = '.folderopen'  then ImageIndex := 7  else
    if ItemExt = '.harddrive'   then ImageIndex := 8  else
    if ItemExt = '.html'        then ImageIndex := 9  else
    if ItemExt = '.mp3'         then ImageIndex := 10 else
    if ItemExt = '.deb'         then ImageIndex := 11 else
    if ItemExt = '.pkg'         then ImageIndex := 11 else
    if ItemExt = '.ppd'         then ImageIndex := 12 else
    if ItemExt = '.ttf'         then ImageIndex := 13 else
    if ItemExt = '.txt'         then ImageIndex := 14 else
    if ItemExt = '.unknow'      then ImageIndex := 15 else
    if ItemExt = '.wab'         then ImageIndex := 16 else
    if ItemExt = '.xls'         then ImageIndex := 17 else ImageIndex := 15;
  end;
end;





























procedure TMainFrm.FindButtonClick(Sender: TObject);
begin
  if SearchPanel.Enabled = FALSE then
  begin
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
var
  Data: PTreeData;
  XNode: PVirtualNode;
begin

  XNode:= VST.GetFirst;
  while XNode <> nil do
  begin
    Data:=VST.GetNodeData(XNode);

    VST.VisiblePath[XNode] := TRUE;
    if PathFilter.Text <> '' then
    begin
      if AnsiCompareFileName(Data^.Column4, PathFilter.Text) <> 0 then
        VST.VisiblePath[XNode] := FALSE;
    end;

    XNode:= VST.GetNextSibling(XNode);
  end;
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





procedure TMainFrm.HeaderControlSectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  (*
  if ListView.SortColumn = Section.Index then
  begin
    if ListView.SortDirection = sdAscending then
      ListView.SortDirection := sdDescending
    else
      ListView.SortDirection type
  PTreeData = ^TTreeData;
  TTreeData = record
    Column0: String;
    Column1: String;
    Column2: String;
  end;:= sdAscending;
  end;
  ListView.SortColumn := Section.Index;
  *)
end;

procedure TMainFrm.HeaderControlSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  Adjust;
end;

procedure TMainFrm.HeaderControlSectionSeparatorDblClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  //ListView.Columns[Section.Index].AutoSize := TRUE;
  //begin
  //  Section.Width := ListView.Columns[Section.Index].Width;
  //end;
  //ListView.Columns[Section.Index].AutoSize := FALSE;
  // Adjust;
end;

procedure TMainFrm.Adjust;
var
  I, J, ColumnWidth: longint;
begin
  //ListView.BeginUpdate;
  //Listview.ScrollBars := ssNone;
  //HeaderControl.Sections[4].Width :=
  //  - HeaderControl.Sections[0].Width
  //  - HeaderControl.Sections[1].Width
  //  - HeaderControl.Sections[2].Width
  //  - HeaderControl.Sections[3].Width + HeaderControl.Width + 1000;

  //for I := 0 to HeaderControl.Sections.Count - 1 do
  //   ListView.Columns[I].Width := HeaderControl.Sections[I].Width;

  //for I := ListView.Columns.Count - 1 downto 0 do
  //begin
  //  ColumnWidth := ListView.Width - 20;
  //  for J := 0 to I - 1 do
  //    Dec(ColumnWidth, ListView.Columns[J].Width);

  //  if ColumnWidth > 0 then
  //  begin
  //    ListView.Columns[I].Width := ColumnWidth;
  //    Break;
  //  end else
  //    ListView.Columns[I].Width := 0;
  //end;
  //Listview.ScrollBars := ssAutoVertical;
  //ListView.EndUpdate;
end;

procedure TMainFrm.IdleTimerStartTimer(Sender: TObject);
begin
  DisableButtons;
  VST.Clear;

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
  I: longint;
  Data: PTreeData;
  XNode: PVirtualNode;
begin

  if ParserCommandLine.Command in [cList] then
  begin
    ParserList.Clear;
    ParserList.Execute(Parser);

    PathFilter.Clear;
    NameFilter.Clear;
    ClearBtnClick(Sender);




    for I := 0 to ParserList.Count - 1 do
    begin
      XNode:=VST.AddChild(nil);
      if VST.AbsoluteIndex(XNode) > -1 then
      begin
        Data := VST.GetNodeData(XNode);
        Data^.Column0 := ParserList.Items[I].ItemName;
        Data^.Column1 := ParserList.Items[I].ItemSize;
        Data^.Column2 := ParserList.Items[I].ItemType;
        Data^.Column3 := ParserList.Items[I].ItemTime;
        Data^.Column4 := ParserList.Items[I].ItemPath;
      end;

      //if PathFilter.Items.IndexOf(ParserList.Items[I].ItemPath) = -1 then
      //begin
      //  PathFilter.Items.Add(ParserList.Items[I].ItemPath);
      //end;


    end;



    //VST.RootNodeCount := ParserList.Count;

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

  VST.Enabled := FALSE;
  VST.Visible := FALSE;
end;

procedure TMainFrm.DisableButtons;
begin
  ToolBar.Buttons[0].Enabled := FALSE;
  ToolBar.Buttons[1].Enabled := FALSE;
  ToolBar.Buttons[2].Enabled := FALSE;
  ToolBar.Buttons[3].Enabled := FALSE;
  ToolBar.Buttons[4].Enabled := FALSE;

  VST.Enabled := FALSE;
  VST.Visible := FALSE;
end;

procedure TMainFrm.EnableButtons;
begin
  ToolBar.Buttons[0].Enabled := TRUE;
  ToolBar.Buttons[1].Enabled := TRUE;
  ToolBar.Buttons[2].Enabled := TRUE;
  ToolBar.Buttons[3].Enabled := TRUE;
  ToolBar.Buttons[4].Enabled := TRUE;

  VST.Enabled := TRUE;
  VST.Visible := TRUE;
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

