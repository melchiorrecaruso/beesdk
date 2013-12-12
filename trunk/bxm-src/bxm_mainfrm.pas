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
  FileUtil,
  Forms,
  Graphics,
  Menus,
  StdCtrls,
  SysUtils,
  // ---
  bxm_Plugins,
  bxm_ArchiveListViewMgr,
  bxm_ArchiveFolderBox,
  bxm_IconList;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    BackGround: TImage;
    HeaderControl: THeaderControl;
    IdleTimer: TIdleTimer;
    ImageList: TImageList;
    IconList: TImageList;
    ListView: TListView;
    MenuItem1: TMenuItem;
    AboutMenuItem: TMenuItem;
    NulMenuItem: TMenuItem;
    OpenDialog: TOpenDialog;
    PreferencesMenuItem: TMenuItem;
    MainMenu: TPopupMenu;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    ShareMenu: TPopupMenu;
    Shape: TShape;
    ToolBar: TToolBar;
    ToolBarMenu: TToolBar;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    AddButton: TToolButton;
    ShareButton: TToolButton;
    MenuButton: TToolButton;
    ExtractButton: TToolButton;
    FindButton: TToolButton;
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
    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
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
  bx_FileScanner,

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

  ListView.Columns.Clear;
  for I := 0 to HeaderControl.Sections.Count - 1 do
  begin
    ListView.Columns.Add;
  end;

  {$IFDEF MSWINDOWS}
  {$ENDIF}

  Adjust;
  DefaultButtons;
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

procedure TMainFrm.HeaderControlSectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  if ListView.SortColumn = Section.Index then
  begin
    if ListView.SortDirection = sdAscending then
      ListView.SortDirection := sdDescending
    else
      ListView.SortDirection := sdAscending;
  end;
  ListView.SortColumn := Section.Index;
end;

procedure TMainFrm.HeaderControlSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  Adjust;
end;

procedure TMainFrm.HeaderControlSectionSeparatorDblClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  ListView.Columns[Section.Index].AutoSize := TRUE;
  begin
    Section.Width := ListView.Columns[Section.Index].Width;
  end;
  ListView.Columns[Section.Index].AutoSize := FALSE;
  Adjust;
end;

procedure TMainFrm.Adjust;
var
  I, J, ColumnWidth: longint;
begin
  ListView.BeginUpdate;
  Listview.ScrollBars := ssNone;
  HeaderControl.Sections[4].Width :=
    - HeaderControl.Sections[0].Width
    - HeaderControl.Sections[1].Width
    - HeaderControl.Sections[2].Width
    - HeaderControl.Sections[3].Width + HeaderControl.Width + 1000;

  for I := 0 to HeaderControl.Sections.Count - 1 do
     ListView.Columns[I].Width := HeaderControl.Sections[I].Width;

  for I := ListView.Columns.Count - 1 downto 0 do
  begin
    ColumnWidth := ListView.Width - 20;
    for J := 0 to I - 1 do
      Dec(ColumnWidth, ListView.Columns[J].Width);

    if ColumnWidth > 0 then
    begin
      ListView.Columns[I].Width := ColumnWidth;
      Break;
    end else
      ListView.Columns[I].Width := 0;
  end;
  Listview.ScrollBars := ssAutoVertical;
  ListView.EndUpdate;
end;

procedure TMainFrm.IdleTimerStartTimer(Sender: TObject);
begin
  DisableButtons;
  ListView.Items.Clear;

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

  if ListView.SortDirection = sdDescending then
  begin
    Compare := - Compare;
  end;
end;

procedure TMainFrm.ListViewData(Sender: TObject; Item: TListItem);
var
  ItemExt: string;
begin
  Item.Data := ParserList.Items[Item.Index];

  Item.Caption   := ' ' + ParserList.Items[Item.Index].ItemName;
  Item.SubItems.Add(' ' + ParserList.Items[Item.Index].ItemSize);
  Item.SubItems.Add(' ' + ParserList.Items[Item.Index].ItemType);
  Item.SubItems.Add(' ' + ParserList.Items[Item.Index].ItemTime);
  Item.SubItems.Add(' ' + ParserList.Items[Item.Index].ItemPath);

  ItemExt := LowerCase(ExtractFileExt(
    ParserList.Items[Item.Index].ItemName));

  if ItemExt = '.avi'         then Item.ImageIndex := 0  else
  if ItemExt = '.bat'         then Item.ImageIndex := 1  else
  if ItemExt = '.bmp'         then Item.ImageIndex := 2  else
  if ItemExt = '.png'         then Item.ImageIndex := 2  else
  if ItemExt = '.cddrive'     then Item.ImageIndex := 3  else
  if ItemExt = '.doc'         then Item.ImageIndex := 4  else
  if ItemExt = '.exe'         then Item.ImageIndex := 5  else
  if ItemExt = '.folderclose' then Item.ImageIndex := 6  else
  if ItemExt = '.folderopen'  then Item.ImageIndex := 7  else
  if ItemExt = '.harddrive'   then Item.ImageIndex := 8  else
  if ItemExt = '.html'        then Item.ImageIndex := 9  else
  if ItemExt = '.mp3'         then Item.ImageIndex := 10 else
  if ItemExt = '.deb'         then Item.ImageIndex := 11 else
  if ItemExt = '.pkg'         then Item.ImageIndex := 11 else
  if ItemExt = '.ppd'         then Item.ImageIndex := 12 else
  if ItemExt = '.ttf'         then Item.ImageIndex := 13 else
  if ItemExt = '.txt'         then Item.ImageIndex := 14 else
  if ItemExt = '.unknow'      then Item.ImageIndex := 15 else
  if ItemExt = '.wab'         then Item.ImageIndex := 16 else
  if ItemExt = '.xls'         then Item.ImageIndex := 17 else Item.ImageIndex := 15;
end;

procedure TMainFrm.ListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin

end;

procedure TMainFrm.IdleTimerStopTimer(Sender: TObject);
var
  I: longint;
begin

  if ParserCommandLine.Command in [cList] then
  begin
    ParserList.Clear;
    ParserList.Execute(Parser);

    ListView.BeginUpdate;
    for I := 0 to ParserList.Count - 1 do
    begin
      ListViewData(ListView, ListView.Items.Add);
    end;
    ListView.EndUpdate;
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

  HeaderControl.Enabled := FALSE;
  HeaderControl.Visible := FALSE;
  ListView.Enabled := FALSE;
  ListView.Visible := FALSE;
end;

procedure TMainFrm.DisableButtons;
begin
  ToolBar.Buttons[0].Enabled := FALSE;
  ToolBar.Buttons[1].Enabled := FALSE;
  ToolBar.Buttons[2].Enabled := FALSE;
  ToolBar.Buttons[3].Enabled := FALSE;
  ToolBar.Buttons[4].Enabled := FALSE;

  HeaderControl.Enabled := FALSE;
  HeaderControl.Visible := FALSE;
  ListView.Enabled := FALSE;
  ListView.Visible := FALSE;
end;

procedure TMainFrm.EnableButtons;
begin
  ToolBar.Buttons[0].Enabled := TRUE;
  ToolBar.Buttons[1].Enabled := TRUE;
  ToolBar.Buttons[2].Enabled := TRUE;
  ToolBar.Buttons[3].Enabled := TRUE;
  ToolBar.Buttons[4].Enabled := TRUE;

  HeaderControl.Enabled := TRUE;
  HeaderControl.Visible := TRUE;
  ListView.Enabled := TRUE;
  ListView.Visible := TRUE;
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
    ParserCommandLine.Command := cList;
    ParserCommandLine.ArchiveName := OpenDialog.FileName;
    // START
    IdleTimer.Enabled := TRUE;
  end;
end;

procedure TMainFrm.NewButtonClick(Sender: TObject);
var
  I: longint;
  Scanner: TFileScanner;
begin

  AddFrm := TAddFrm.Create(Self);

  if AddFrm.ShowModal = mrOk then
  begin
    ParserCommandLine.Clear;
    ParserCommandLine.Command := cAdd;
    ParserCommandLine.CompressionMode := TCompressionMode(
      Ord(AddFrm.CompressionMethod.ItemIndex));

    if AddFrm.ArchiveWithPasswordCheck.Checked then
      ParserCommandLine.Password := AddFrm.ArchiveWithPassword.Text;

    ParserCommandLine.ArchiveName := AddFrm.ArchiveName;

    SetCurrentDir(AddFrm.Root.Text);
    Scanner := TFileScanner.Create;
    for i := 0 to AddFrm.Files.Items.Count - 1 do
      if AddFrm.Files.Items[i].ImageIndex = 0 then
        Scanner.Add(AddFrm.Files.Items[i].Text,
          AddFrm.RecurseSubdirectories.Checked);

    for i := 0 to AddFrm.Files.Items.Count - 1 do
      if AddFrm.Files.Items[i].ImageIndex = 1 then
        Scanner.Delete(AddFrm.Files.Items[i].Text,
          AddFrm.RecurseSubdirectories.Checked);

    for i := 0 to Scanner.Count - 1 do
      ParserCommandLine.FileMasks.Add(Scanner.Items[i].ItemName);
    Scanner.Destroy;

    // START
    IdleTimer.Enabled := TRUE;
  end;
  AddFrm.Destroy;
end;

end.

