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


unit bxm_mainfrm;

{$I bxm_compiler.inc}

interface

uses
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  FileUtil,
  Forms,
  Graphics,
  Masks,
  Menus,
  StdCtrls, FileCtrl, Spin, EditBtn, Grids,
  SysUtils,
  // ---
  bxm_Plugins,
  bxm_IconList, VirtualTrees;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    BackGround: TImage;
    TypeFilter: TComboBox;
    FromFilter: TDateEdit;
    ToFilter: TDateEdit;
    TypeFilterLabel: TLabel;
    FromFilterLabel: TLabel;
    Label3: TLabel;
    ApplyBtn: TBitBtn;
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


    procedure ApplyBtnClick(Sender: TObject);
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





    procedure MainMenuClose(Sender: TObject);
    procedure MenuButtonClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure ShareButtonClick(Sender: TObject);
    procedure ShareMenuClose(Sender: TObject);
    procedure VSTDblClick(Sender: TObject);

    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);

    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);

  private
    { private declarations }
    ParserCommandLine: TParserCommandLine;
    ParserList: TParserList;
    Parser: TParser;

    Names: TStringList;
    Paths: TStringList;
    Types: TStringList;
    List: TList;
  public
    { public declarations }
    procedure Adjust;

    procedure DefaultButtons;
    procedure DisableButtons;
    procedure EnableButtons;
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

uses
  bx_dirscanner,

  bxm_AddFrm,
  bxm_AboutFrm,
  bxm_TickFrm;

var
  ListSortAscending: boolean;
  ListSortColumn: longint;

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
var
  I: longint;
begin
  ParserCommandLine := TParserCommandLine.Create;
  ParserList := TParserList.Create;

  Names := TStringList.Create;
  Names.CaseSensitive := FileNameCaseSensitive;
  Names.Sorted := TRUE;

  Paths := TStringList.Create;
  Paths.CaseSensitive := FileNameCaseSensitive;
  Paths.Sorted := TRUE;

  Types := TStringList.Create;
  Types.CaseSensitive := FileNameCaseSensitive;
  Types.Sorted := TRUE;

  List := TList.Create;
  ListSortColumn    := 0;
  ListSortAscending := TRUE;

  {$IFDEF MSWINDOWS}
  VST.BorderStyle := bsNone;
  {$ENDIF}
  {$IFDEF UNIX}
  VST.BorderStyle := bsNone;
  {$ENDIF}

  Adjust;
  DefaultButtons;
  FindButtonClick(nil);
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  List.Destroy;
  Names.Destroy;
  Paths.Destroy;
  Types.Destroy;
  ParserList.Destroy;
  ParserCommandLine.Destroy;
end;

procedure TMainFrm.FormResize(Sender: TObject);
begin
  Adjust;
end;

procedure TMainFrm.FindButtonClick(Sender: TObject);
begin
  if SearchPanel.Enabled = FALSE then
  begin
    ShapeBotton.Visible := TRUE;
    SearchPanel.Height  := 130;
    SearchPanel.Enabled := TRUE;
    FindButton.Down     := TRUE;
  end else
  begin
    ShapeBotton.Visible := FALSE;
    SearchPanel.Height  := 1;
    SearchPanel.Enabled := FALSE;
    FindButton.Down     := FALSE;
  end;
end;

procedure TMainFrm.VSTDblClick(Sender: TObject);
var
  Data: TParserItem;
begin
  if Assigned(VST.FocusedNode) then
  begin
    Data := TParserItem(List.Items[VST.FocusedNode^.Index]);
    if Data.ItemType = '.folderclose' then
    begin
      PathFilter.Text :=
        Data.ItemPath +
        Data.ItemName +
        PathDelim;
      ApplyBtnClick(Self);
    end;
  end;
end;

procedure TMainFrm.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: TParserItem;
  S: string;
begin
  if Column = 0 then
  begin
    Data := TParserItem(List.Items[Node.Index]);
    S := LowerCase(Data.ItemType);

    if S = '.avi'         then ImageIndex := 0  else
    if S = '.bat'         then ImageIndex := 1  else
    if S = '.bmp'         then ImageIndex := 2  else
    if S = '.png'         then ImageIndex := 2  else
    if S = '.cddrive'     then ImageIndex := 3  else
    if S = '.doc'         then ImageIndex := 4  else
    if S = '.exe'         then ImageIndex := 5  else
    if S = '.folderclose' then ImageIndex := 6  else
    if S = '.folderopen'  then ImageIndex := 7  else
    if S = '.harddrive'   then ImageIndex := 8  else
    if S = '.html'        then ImageIndex := 9  else
    if S = '.mp3'         then ImageIndex := 10 else
    if S = '.deb'         then ImageIndex := 11 else
    if S = '.pkg'         then ImageIndex := 11 else
    if S = '.ppd'         then ImageIndex := 12 else
    if S = '.ttf'         then ImageIndex := 13 else
    if S = '.txt'         then ImageIndex := 14 else
    if S = '.unknow'      then ImageIndex := 15 else
    if S = '.wab'         then ImageIndex := 16 else
    if S = '.xls'         then ImageIndex := 17 else ImageIndex := 15;
  end;
end;

procedure TMainFrm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: TParserItem;
begin
  Data := TParserItem(List.Items[Node.Index]);
  if Assigned(Data) then
    case Column of
      0: CellText := Data.ItemName;
      1: CellText := Data.ItemSize;
      2: CellText := Data.ItemType;
      3: CellText := Data.ItemTime;
      4: CellText := Data.ItemPath;
    end;
end;

function CompareItem(Item1, Item2: pointer): longint; inline;
var
  P1, P2: TParserItem;
  P1IsDir, P2IsDir: boolean;
begin
  Result := 0;

  P1 := TParserItem(Item1);
  P2 := TParserItem(Item2);

  P1IsDir := Pos('D', P1.ItemAttr) > 0;
  P2IsDir := Pos('D', P2.ItemAttr) > 0;

  if P1IsDir = P2IsDir then
  begin
    case ListSortColumn of
      0:   Result := AnsiCompareFileName(P1.ItemName, P2.ItemName);
      1:   if P1.ItemSize > P2.ItemSize then
             Result := 1
           else
             if P1.ItemSize < P2.ItemSize then
               Result := -1
             else
               Result := 0;

      2:   Result := AnsiCompareFileName(P1.ItemType, P2.ItemType);
      3:   if P1.ItemTime > P2.ItemTime then
             Result := 1
           else
             if P1.ItemTime < P2.ItemTime then
               Result := -1
             else
               Result := 0;

      4:   Result := AnsiCompareFileName(P1.ItemPath, P2.ItemPath);
    end;

  end else
    if P1IsDir then
      Result := -1
    else
      if P2IsDir then
        Result := 1;

  if ListSortAscending = FALSE then
  begin
    Result := - Result;
  end;
end;

procedure TMainFrm.ApplyBtnClick(Sender: TObject);
var
  I: longint;
  PI: TParserItem;
  RES: boolean;
begin
  List.Clear;
  for I := 0 to ParserList.Count - 1 do
  begin
    PI := ParserList.Items[I];

    RES := TRUE;
    if MatchesMask(PI.ItemName, NameFilter.Text, FileNameCaseSensitive) = FALSE then RES := FALSE;
    if MatchesMask(PI.ItemType, TypeFilter.Text, FileNameCaseSensitive) = FALSE then RES := FALSE;
    if MatchesMask(PI.ItemPath, PathFilter.Text, FileNameCaseSensitive) = FALSE then RES := FALSE;

    if RES then List.Add(PI);
  end;
  List.Sort(CompareItem);

  if Names.Find(NameFilter.Text, I) = FALSE then
  begin
    NameFilter.AddItem(NameFilter.Text, nil);
    Names.Add(NameFilter.Text);
  end;

  // VST.BeginUpdate;
  VST.Clear;
  VST.RootNodeCount := List.Count;
  //for I := 0 to List.Count - 1 do
  //  VST.GetNodeData(VST.AddChild(nil));
  // VST.EndUpdate;
end;

procedure TMainFrm.ClearBtnClick(Sender: TObject);
var
  I: longint;
begin
  NameFilter.Text := '*';
  PathFilter.Text := '*';
  TypeFilter.Text := '*';

  MinSize.Text    := '';
  MaxSize.Text    := '';

  FromFilter.Text := '';
  ToFilter.Text   := '';

  List.Clear;
  for I := 0 to ParserList.Count - 1 do
    List.Add(ParserList.Items[I]);
  List.Sort(CompareItem);

  /// VST.BeginUpdate;
  VST.Clear;
  VST.RootNodeCount := List.Count;
  //for I := 0 to List.Count - 1 do
  //  VST.GetNodeData(VST.AddChild(nil));
  // VST.EndUpdate;
end;

procedure TMainFrm.HeaderControlSectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  if ListSortColumn <> Section.Index then
    ListSortColumn := Section.Index
  else
    ListSortAscending := not ListSortAscending;
  ApplyBtnClick(Self);
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
  VST.BeginUpdate;



  //HC.Sections[4].Width :=
  //  - HC.Sections[0].Width
  //  - HC.Sections[1].Width
  //  - HC.Sections[2].Width
  //  - HC.Sections[3].Width + HC.Width - 25;

  //for I := 0 to HC.Sections.Count - 1 do
  //begin
  //  LV.Columns[I].Width := HC.Sections[I].Width;
  //  VST.Header.Columns[I].Width := HC.Sections[I].Width;
  //end;



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

  VSt.EndUpdate;
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

procedure TMainFrm.IdleTimerStopTimer(Sender: TObject);
var
  I, J: longint;
begin

  if ParserCommandLine.Command in [cList] then
  begin
    ParserList.Clear;
    ParserList.Execute(Parser);

    Names.Clear;
    Paths.Clear;
    Types.Clear;

    for I := 0 to ParserList.Count - 1 do
    begin
      if Paths.Find(ParserList.Items[I].ItemPath, J) = FALSE then
        Paths.Add(ParserList.Items[I].ItemPath);

      if Types.Find(ParserList.Items[I].ItemType, J) = FALSE then
        Types.Add(ParserList.Items[I].ItemType);
    end;

    NameFilter.Clear;
    NameFilter.AddItem('*', nil);
    for I := 0 to Names.Count - 1 do
      NameFilter.AddItem(Name[I], nil);

    PathFilter.Clear;
    PathFilter.AddItem('*', nil);
    for I := 0 to Paths.Count - 1 do
      PathFilter.AddItem(Paths[I], nil);

    TypeFilter.Clear;
    TypeFilter.AddItem('*', nil);
    for I := 0 to Types.Count - 1 do
      TypeFilter.AddItem(Types[I], nil);

    ClearBtnClick(Sender);

    for I := 0 to Parser.Count - 1 do
    begin
      TickFrm.DetailsReport.Lines.Add(Parser.Messages[I]);
    end;

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

