{Implements Genetic optimization for make optimal parameters
  for Bee 0.7.7.
}

unit BeeOpt_Form;

{$I compiler.inc}

interface

uses
  Math,
  Forms,
  Classes,
  dialogs,
  Messages,
  SysUtils,
  ComCtrls,
  StdCtrls,
  Controls,
  ExtCtrls,
  LResources,

  BeeOpt_Optimizer,
  BeeLib_Configuration;

type
  // TMainForm class

  TMainForm = class (TForm)
    Label_Level: TLabel;
    Label_BestPackedSize: TLabel;
    Label_BestPackedSizeValue: TLabel;
    PageControl: TPageControl;
    TabSheet_Progress: TTabSheet;
    TabSheet_About: TTabSheet;
    Label_Extension: TLabel;
    Label_ExtensionValue: TLabel;
    Label_variantsEstimated: TLabel;
    Label_variantsEstimatedValue: TLabel;
    Label_PercentOfImprovements: TLabel;
    Label_PercentOfImprovementsValue: TLabel;
    Label_LevelValue: TLabel;
    StatusBar: TStatusBar;
    Label_About: TLabel;
    Label_SampleSize: TLabel;
    Label_SampleSizeValue: TLabel;
    Label_PackedSize: TLabel;
    Label_PackedSizeValue: TLabel;
    Label_DictionaryLevel: TLabel;
    Label_DictionaryLevelValue: TLabel;
    TrayIcon: TTrayIcon;
    procedure FormCreate (Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose  (Sender: TObject; var Action: TCloseAction);
    procedure FormWindowStateChange(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  public
    procedure DoDrawMessage(const Message: string);
    procedure DoDrawCurrAge(CurrentAge: longint);
    procedure DoDrawCurrPopulation(CurrentPopulation: longint);
    procedure DoDrawImprovment(Improvment: double);
    procedure DoDrawCost(Cost: longint);
    procedure DoDrawSampleSize(SampleSize: longint);
    procedure DoDrawBestPackedSize(BestPackedSize: longint);
    procedure DoDrawDictionaryLevel(DictionaryLevel: longint);
    procedure DoDrawExtension(const Extension: string);
  end;

var
  MainForm: TMainForm;

implementation

// TMainForm

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DrawMessage         := DoDrawMessage;
  DrawCurrAge         := DoDrawCurrAge;
  DrawCurrPopulation  := DoDrawCurrPopulation;
  DrawImprovment      := DoDrawImprovment;
  DrawCost            := DoDrawCost;
  DrawSampleSize      := DoDrawSampleSize;
  DrawBestPackedSize  := DoDrawBestPackedSize;
  DrawDictionaryLevel := DoDrawDictionaryLevel;
  DrawExtension       := DoDrawExtension;

  Optimizer := TOptimizer.Create;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Optimizer.NeedToClose := True;
  DrawMessage('Saving ...');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Optimizer <> nil then Optimizer.Free;
end;

procedure TMainForm.DoDrawMessage(const Message: string);
begin
  MainForm.StatusBar.Panels[0].Text := Message;
  if Optimizer.NeedToClose then
  begin
    while PageControl.PageCount > 1 do
    begin
      PageControl.Pages[0].Destroy;
    end;
  end;
end;

procedure TMainForm.DoDrawCurrAge(CurrentAge: longint);
begin
  Label_VariantsEstimatedValue.Caption := Format('%d', [CurrentAge]);
end;

procedure TMainForm.DoDrawCurrPopulation(CurrentPopulation: longint);
begin
  Label_LevelValue.Caption := Format('%d', [CurrentPopulation]);
end;

procedure TMainForm.DoDrawImprovment(Improvment: double);
begin
  Label_PercentOfImprovementsValue.Caption := Format('%1.3f%%', [Improvment]);
end;

procedure TMainForm.DoDrawCost(Cost: longint);
begin
  if Cost <> 0 then
    Label_PackedSizeValue.Caption := Format('%d', [Cost])
  else
    Label_PackedSizeValue.Caption := '?';
end;

procedure TMainForm.DoDrawSampleSize(SampleSize: longint);
begin
  Label_SampleSizeValue.Caption := Format('%d', [SampleSize]);
end;

procedure TMainForm.DoDrawBestPackedSize(BestPackedSize: longint);
begin
  Label_BestPackedSizeValue.Caption := Format('%d', [BestPackedSize]);
  TrayIcon.Hint := Format('Best packed size %d', [BestPackedSize]);
end;

procedure TMainForm.DoDrawDictionaryLevel(DictionaryLevel: longint);
begin
  Label_DictionaryLevelValue.Caption := Format ('%d (~%d Mb)', [DictionaryLevel,
    (1 shl (17 + Min (Max (0, DictionaryLevel), 9))) * 20 shr 20]);
end;

procedure TMainForm.DoDrawExtension(const Extension: string);
begin
  Label_ExtensionValue.Caption := Format('<%s>', [Extension]);
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  case WindowState of
    wsMinimized: Visible := False;
  end;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Visible     := True;
end;

initialization

  {$I beeopt_form.lrs}

end.

