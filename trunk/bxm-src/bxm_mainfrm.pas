unit bxm_mainfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, FileUtil, Forms, Graphics, ComCtrls, ExtCtrls,
  Buttons, Menus, SysUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

uses
  bxm_AboutFrm;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Image1Click(Sender: TObject);
begin

end;

procedure TForm1.MenuButtonClick(Sender: TObject);
begin




  MainMenu.PopUp;




end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  About: TAboutFrm;
begin
  About := TAboutFrm.Create(Self);
  About.ShowModal;
  About.Destroy;
end;

procedure TForm1.Shape2ChangeBounds(Sender: TObject);
begin

end;

procedure TForm1.NewButtonClick(Sender: TObject);
begin


end;


end.

