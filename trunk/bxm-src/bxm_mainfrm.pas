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
    HeaderControl1: THeaderControl;
    BackGround: TImage;
    ImageList: TImageList;
    ListView: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
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
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure Shape2ChangeBounds(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure ShareButtonClick(Sender: TObject);
    procedure ShareMenuPopup(Sender: TObject);
  private
    { private declarations }
    ParserCommandLine: TParserCommandLine;
  public
    { public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

uses
  bxm_AddFrm,
  bxm_AboutFrm,
  bxm_TickFrm;

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  ParserCommandLine := TParserCommandLine.Create;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  ParserCommandLine.Destroy;
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

procedure TMainFrm.ShareMenuPopup(Sender: TObject);
begin
  ShareButton.Down := FALSE;
end;

procedure TMainFrm.MenuItem2Click(Sender: TObject);
begin
  AboutShowModal;
end;

procedure TMainFrm.MenuItem6Click(Sender: TObject);
begin

end;

procedure TMainFrm.Shape2ChangeBounds(Sender: TObject);
begin

end;

procedure TMainFrm.NewButtonClick(Sender: TObject);
begin
  ParserCommandLine.Clear;
  if AddShowModal(ParserCommandLine) = mrOk then
  begin
    TickShowModal;




  end;
end;

end.

