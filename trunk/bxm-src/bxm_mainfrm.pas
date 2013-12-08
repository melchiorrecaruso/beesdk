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
  Menus, Grids, FileCtrl, DBGrids,
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
    StringGrid: TStringGrid;
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
    procedure HeaderControlSectionTrack(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    procedure IdleTimerStartTimer(Sender: TObject);
    procedure IdleTimerStopTimer(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure MainMenuClose(Sender: TObject);
    procedure MenuButtonClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure Shape2ChangeBounds(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure ShareButtonClick(Sender: TObject);
    procedure ShareMenuClose(Sender: TObject);
    procedure StringGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    { private declarations }
    ParserCommandLine: TParserCommandLine;
    ParserList: TParserList;
    Parser: TParser;

    Selection: array of boolean;
  public
    { public declarations }
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

  StringGrid.FocusRectVisible := FALSE;
  StringGrid.Columns.Clear;
  for I := 0 to HeaderControl.Sections.Count - 1 do
  begin
    StringGrid.Columns.Add;
  end;
  StringGrid.FixedCols:= 0;
  StringGrid.RowCount := 0;
  StringGrid.FixedRows:= 0;

  DefaultButtons;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  ParserList.Destroy;
  ParserCommandLine.Destroy;
end;

procedure TMainFrm.FormResize(Sender: TObject);
begin
  HeaderControl.Sections[4].Width :=
    - HeaderControl.Sections[0].Width
    - HeaderControl.Sections[1].Width
    - HeaderControl.Sections[2].Width
    - HeaderControl.Sections[3].Width + HeaderControl.Width + 5;
end;

procedure TMainFrm.HeaderControlSectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  StringGrid.SortColRow(TRUE, Section.Index);
end;

procedure TMainFrm.HeaderControlSectionTrack(HeaderControl: TCustomHeaderControl;
  Section: THeaderSection; Width: Integer;State: TSectionTrackState);
begin
  if Section.Index < StringGrid.ColCount then
  begin
    StringGrid.ColWidths[Section.Index] := Width;
    FormResize(Self);
  end;
end;

procedure TMainFrm.IdleTimerStartTimer(Sender: TObject);
begin
  DisableButtons;

  StringGrid.Clear;

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
  I: longint;
  Item: TListItem;
begin

  if ParserCommandLine.Command in [cList] then
  begin
    ParserList.Clear;
    ParserList.Execute(Parser);

    StringGrid.BeginUpdate;
    StringGrid.RowCount := ParserList.Count;
    SetLength(Selection, ParserList.Count);
    for I := 0 to StringGrid.RowCount - 1 do
    begin
      StringGrid.Cells[0, I] := ParserList.Items[I].ItemName;
      StringGrid.Cells[1, I] := ParserList.Items[I].ItemSize;
      StringGrid.Cells[2, I] := ParserList.Items[I].ItemType;
      StringGrid.Cells[3, I] := ParserList.Items[I].ItemTime;
      StringGrid.Cells[4, I] := ParserList.Items[I].ItemPath;

      Selection[I] := FALSE;
    end;
    StringGrid.EndUpdate;

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
  StringGrid.Enabled := FALSE;
  StringGrid.Visible := TRUE;
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
  StringGrid.Enabled := FALSE;
  StringGrid.Visible := TRUE;
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
  StringGrid.Enabled := TRUE;
  StringGrid.Visible := TRUE;
end;

procedure TMainFrm.MenuButtonClick(Sender: TObject);
begin
  MenuButton.Down := TRUE;
  MainMenu.PopUp(Left + ToolBarMenu.Left + MenuButton.Left, Top + 59);
end;

procedure TMainFrm.MainMenuClose(Sender: TObject);
begin
  MenuButton.Down := FALSE;
end;

procedure TMainFrm.ShareButtonClick(Sender: TObject);
begin
  ShareButton.Down := TRUE;
  ShareMenu.PopUp(Left + ToolBarMenu.Left + ShareButton.Left, Top  + 59);
end;

procedure TMainFrm.ShareMenuClose(Sender: TObject);
begin
  ShareButton.Down := FALSE;
end;

procedure TMainFrm.StringGridCompareCells(Sender: TObject;
  ACol, ARow, BCol, BRow: Integer; var Result: integer);
begin
  if ACOL = BCOL then
    Result := AnsiCompareFileName(StringGrid.Cells[ACol, ARow], StringGrid.Cells[BCol, BRow]);
end;

function GetIconIndex(const FileExt: string): longint;
var
  S: string;
begin
  Result := 15;

  if FileExt = '.avi'         then Result := 0;
  if FileExt = '.bat'         then Result := 1;
  if FileExt = '.bmp'         then Result := 2;
  if FileExt = '.cddrive'     then Result := 3;
  if FileExt = '.doc'         then Result := 4;
  if FileExt = '.exe'         then Result := 5;
  if FileExt = '.folderclose' then Result := 6;
  if FileExt = '.folderopen'  then Result := 7;
  if FileExt = '.harddrive'   then Result := 8;
  if FileExt = '.html'        then Result := 9;
  if FileExt = '.mp3'         then Result := 10;
  if FileExt = '.pkg'         then Result := 11;
  if FileExt = '.ppd'         then Result := 12;
  if FileExt = '.ttf'         then Result := 13;
  if FileExt = '.txt'         then Result := 14;
  if FileExt = '.unknow'      then Result := 15;
  if FileExt = '.wab'         then Result := 16;
  if FileExt = '.xls'         then Result := 17;
end;

procedure TMainFrm.StringGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  B:  TBitmap;
  I: longint;
  R: TRect;
begin
  for I := 0 to StringGrid.ColCount - 1 do
    StringGrid.ColWidths[I] := HeaderControl.Sections[I].Width;

  if aCol = 0 then
  begin
    B := TBitmap.Create;
    try
      IconList.GetBitmap(GetIconIndex(LowerCase(
        ExtractFileExt(StringGrid.Cells[aCol, aRow]))), B);

      R.Top := aRect.Top + (StringGrid.DefaultRowHeight - B.Height) div 2;
      R.Left := aRect.Left + 2;
      R.Right := R.Left + B.Width;
      R.Bottom := R.Top + B.Height;

      if Selection[aRow] = TRUE then
      begin
        StringGrid.Canvas.Brush.Color := clHighlight;
        StringGrid.Canvas.Font.Color  := clHighlightText;
      end else
      begin
        StringGrid.Canvas.Brush.Color := clDefault;
        StringGrid.Canvas.Font.Color  := clDefault;
      end;


      StringGrid.Canvas.Clear;
      StringGrid.Canvas.StretchDraw(R, B);
      StringGrid.Canvas.TextOut(R.Right + 4, R.Top, StringGrid.Cells[aCol, aRow]);
    finally
      B.Free;
    end;
  end;
end;

procedure TMainFrm.StringGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

end;

procedure TMainFrm.StringGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin



  end;


end;

procedure TMainFrm.StringGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin

end;

procedure TMainFrm.AboutMenuItemClick(Sender: TObject);
begin
  AboutFrm := TAboutFrm.Create(Self);
  AboutFrm.ShowModal;
  AboutFrm.Destroy;
end;

procedure TMainFrm.MenuItem6Click(Sender: TObject);
begin

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

    IdleTimer.Enabled := TRUE;
  end;

end;

procedure TMainFrm.Shape2ChangeBounds(Sender: TObject);
begin

end;

procedure TMainFrm.NewButtonClick(Sender: TObject);
var
  I: longint;
  Scanner: TFileScanner;
begin


  AddFrm := TAddFrm.Create(Self);

  (*
  if Add.ShowModal = mrOk then
  begin
    Application.ProcessMessages;

    ParserCommandLine.Command := cAdd;
    ParserCommandLine.CompressionMode := TCompressionMode(
      Ord(Add.CompressionMethod.ItemIndex));

    if Add.ArchiveWithPasswordCheck.Checked then
      ParserCommandLine.Password := Add.ArchiveWithPassword.Text;

    ParserCommandLine.ArchiveName := Add.ArchiveName;

    SetCurrentDir(Add.Root.Text);
    Scanner := TFileScanner.Create;
    for i := 0 to Add.Files.Items.Count - 1 do
      if Add.Files.Items[i].ImageIndex = 0 then
        Scanner.Add(Add.Files.Items[i].Text,
          Add.RecurseSubdirectories.Checked);

    for i := 0 to Add.Files.Items.Count - 1 do
      if Add.Files.Items[i].ImageIndex = 1 then
        Scanner.Delete(Add.Files.Items[i].Text,
          Add.RecurseSubdirectories.Checked);

    for i := 0 to Scanner.Count - 1 do
      ParserCommandLine.FileMasks.Add(Scanner.Items[i].Name);
    Scanner.Destroy;

    Tick := TTickFrm.Create(Self);
    if Tick.ShowModal(Parser) = mrOk then
    begin
      Application.ProcessMessages;





    end;
    Tick.Destroy;
  end;


  *)
  AddFrm.Destroy;
end;

end.

