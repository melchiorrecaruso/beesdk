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
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MainMenu: TPopupMenu;
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
    procedure MenuButtonClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure Shape2ChangeBounds(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
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




  MainMenu.PopUp;




end;

procedure TMainFrm.MenuItem2Click(Sender: TObject);
begin
  AboutFrm.ShowModal;
end;

procedure TMainFrm.Shape2ChangeBounds(Sender: TObject);
begin

end;

procedure TMainFrm.NewButtonClick(Sender: TObject);
begin
  AddFrm.ShowModal;
end;


end.

