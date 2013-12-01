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
  SysUtils,
  // ---
  bxm_Plugins;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    HeaderControl: THeaderControl;
    BackGround: TImage;
    ImageList: TImageList;
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
    procedure MainMenuClose(Sender: TObject);
    procedure MenuButtonClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure Shape2ChangeBounds(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure ShareButtonClick(Sender: TObject);
    procedure ShareMenuClose(Sender: TObject);
  private
    { private declarations }
    ParserCommandLine: TParserCommandLine;
    Parser: TParser;
    ParserList: TParserList;
  public
    { public declarations }
    procedure DisableButtons;
    procedure EnableButtons;
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

uses
  bx_FileStream,

  bxm_AddFrm,
  bxm_AboutFrm,
  bxm_TickFrm;

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  ParserCommandLine := TParserCommandLine.Create;
  Parser := TParser.Create(ParserCommandLine);
  ParserList := TParserList.Create(Parser);

  DisableButtons;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin


  ParserList.Destroy;
  Parser.Destroy;
  ParserCommandLine.Destroy;
end;

procedure TMainFrm.DisableButtons;
begin
  ToolBar.Buttons[0].Enabled := TRUE;
  ToolBar.Buttons[1].Enabled := TRUE;
  ToolBar.Buttons[2].Enabled := FALSE;
  ToolBar.Buttons[3].Enabled := FALSE;
  ToolBar.Buttons[4].Enabled := FALSE;

  HeaderControl.Enabled := FALSE;
  ListView.Enabled := FALSE;
end;

procedure TMainFrm.EnableButtons;
begin
  ToolBar.Buttons[0].Enabled := TRUE;
  ToolBar.Buttons[1].Enabled := TRUE;
  ToolBar.Buttons[2].Enabled := TRUE;
  ToolBar.Buttons[3].Enabled := TRUE;
  ToolBar.Buttons[4].Enabled := TRUE;

  HeaderControl.Enabled := TRUE;
  ListView.Enabled := FALSE;
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
begin
  if OpenDialog.Execute then
  begin
    DisableButtons;

    ParserCommandLine.Clear;
    ParserCommandLine.Command := cList;
    ParserCommandLine.ArchiveName := OpenDialog.FileName;

    TickFrm := TTickFrm.Create(Self);
    if TickFrm.ShowModal(Parser) = mrOk then
    begin


    end;
    TickFrm.Destroy;

    ParserList.Clear;
    ParserList.Execute;









    EnableButtons;
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
  ParserCommandLine := TParserCommandLine.Create;
  Parser := TParser.Create(ParserCommandLine);
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

  Parser.Destroy;
  ParserCommandLine.Destroy;
end;

end.

