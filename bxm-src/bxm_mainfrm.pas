unit bxm_MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, FileUtil, Forms, Graphics, ComCtrls, ExtCtrls,
  Buttons, Menus, SysUtils;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    HeaderControl1: THeaderControl;
    Image1: TImage;
    ImageList: TImageList;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MainMenu: TPopupMenu;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    ShareMenu: TPopupMenu;
    Shape1: TShape;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    AddButton: TToolButton;
    ShareButton: TToolButton;
    MenuButton: TToolButton;
    ExtractButton: TToolButton;
    FindButton: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
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
  public
    { public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

uses
  bxm_AboutFrm,
  bxm_AddFrm;

{$R *.lfm}

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
begin

end;

procedure TMainFrm.Image1Click(Sender: TObject);
begin

end;

procedure TMainFrm.MenuButtonClick(Sender: TObject);
begin
  MenuButton.Down := TRUE;
  MainMenu.PopUp(Left + ToolBar2.Left + MenuButton.Left, Top + 59);
end;

procedure TMainFrm.MainMenuClose(Sender: TObject);
begin
  MenuButton.Down := FALSE;
end;

procedure TMainFrm.ShareButtonClick(Sender: TObject);
begin
  ShareButton.Down := TRUE;
  ShareMenu.PopUp(Left + ToolBar2.Left + ShareButton.Left, Top  + 59);
end;

procedure TMainFrm.ShareMenuPopup(Sender: TObject);
begin
  ShareButton.Down := FALSE;
end;

procedure TMainFrm.MenuItem2Click(Sender: TObject);
begin
  AboutFrm.ShowModal;
end;

procedure TMainFrm.MenuItem6Click(Sender: TObject);
begin

end;

procedure TMainFrm.Shape2ChangeBounds(Sender: TObject);
begin

end;

procedure TMainFrm.NewButtonClick(Sender: TObject);
begin
  AddFrm.ShowModal;
end;

end.

