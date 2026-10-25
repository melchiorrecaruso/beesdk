unit BeeOptMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, BeeOptCore, BeeModel;

type
  TMainForm = class;

  TBeeOptRunner = class(TThread)
  private
    FForm: TMainForm;
    FProgress: TBeeOptProgress;
    FMessage, FError: string;
    procedure CoreProgress(const Progress: TBeeOptProgress);
    procedure CoreMessage(const Text: string);
    function CoreShouldStop: Boolean;
    procedure SyncProgress;
    procedure SyncMessage;
    procedure SyncFinished;
  protected
    procedure Execute; override;
  public
    constructor Create(AForm: TMainForm);
  end;

  TMainForm = class(TForm)
    LabelAge: TLabel;
    LabelAgeValue: TLabel;
    LabelCompressed: TLabel;
    LabelCompressedValue: TLabel;
    LabelImprovementCount: TLabel;
    LabelImprovementCountValue: TLabel;
    LabelCorpus: TLabel;
    LabelCorpusValue: TLabel;
    LabelDictionary: TLabel;
    LabelDictionaryValue: TLabel;
    LabelImprovementRate: TLabel;
    LabelImprovementRateValue: TLabel;
    LabelPopulation: TLabel;
    LabelPopulationValue: TLabel;
    LabelSample: TLabel;
    LabelSampleValue: TLabel;
    LabelThreads: TLabel;
    LabelThreadsValue: TLabel;
    LabelAbout: TLabel;
    PageControl: TPageControl;
    StatusBar: TStatusBar;
    TabAbout: TTabSheet;
    TabProgress: TTabSheet;
    TimerClose: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerCloseTimer(Sender: TObject);
  private
    FRunner: TBeeOptRunner;
    FRunDone, FClosing: Boolean;
    procedure ShowProgress(const Progress: TBeeOptProgress);
    procedure ShowStatus(const AText: string);
    procedure RunFinished(const ErrorText: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

constructor TBeeOptRunner.Create(AForm: TMainForm);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FForm := AForm;
end;

procedure TBeeOptRunner.CoreProgress(const Progress: TBeeOptProgress);
begin
  FProgress := Progress;
  Synchronize(@SyncProgress);
end;

procedure TBeeOptRunner.CoreMessage(const Text: string);
begin
  FMessage := Text;
  Synchronize(@SyncMessage);
end;

function TBeeOptRunner.CoreShouldStop: Boolean;
begin
  Result := Terminated;
end;

procedure TBeeOptRunner.SyncProgress;
begin
  FForm.ShowProgress(FProgress);
end;

procedure TBeeOptRunner.SyncMessage;
begin
  FForm.ShowStatus(FMessage);
end;

procedure TBeeOptRunner.SyncFinished;
begin
  FForm.RunFinished(FError);
end;

procedure TBeeOptRunner.Execute;
begin
  FError := '';
  try
    RunBeeOpt(@CoreProgress, @CoreMessage, @CoreShouldStop);
  except
    on E: EBeeOptHelp do FError := E.Message;
    on E: Exception do FError := 'Error: ' + E.Message;
  end;
  Synchronize(@SyncFinished);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FRunDone := False;
  FClosing := False;
  { The executable resource supplies the icon on Windows.  On other widget sets
    retain the copy embedded in the form resource. }
  if Assigned(Application.Icon) and (not Application.Icon.Empty) then
    Icon.Assign(Application.Icon);
  StatusBar.SimpleText := 'Initialization...';
  FRunner := TBeeOptRunner.Create(Self);
  FRunner.Start;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FRunner) then
  begin
    FRunner.Terminate;
    FRunner.WaitFor;
    FRunner.Free;
  end;
end;

procedure TMainForm.ShowStatus(const AText: string);
begin
  StatusBar.SimpleText := AText;
end;

procedure TMainForm.ShowProgress(const Progress: TBeeOptProgress);
var ImprovementRate: Double;
begin
  LabelCorpusValue.Caption := ExtractFileName(
    ExcludeTrailingPathDelimiter(Progress.Corpus));
  LabelCorpusValue.Hint := Progress.Corpus;
  LabelAgeValue.Caption := IntToStr(Progress.Age);
  LabelImprovementCountValue.Caption :=
    FormatFloat('#,##0', Progress.Improvements);
  LabelPopulationValue.Caption := IntToStr(Progress.Population);
  if Progress.Age > 0 then
    ImprovementRate := Progress.Improvements / Progress.Age * 100
  else
    ImprovementRate := 0;
  LabelImprovementRateValue.Caption := Format('%0.3f%%', [ImprovementRate]);
  LabelSampleValue.Caption := FormatFloat('#,##0', Progress.TotalBytes) + ' byte';
  if Progress.CompressedBytes >= 0 then
    LabelCompressedValue.Caption := Format('%s byte (%0.3f%%)',
      [FormatFloat('#,##0', Progress.CompressedBytes), Progress.Percentage])
  else
    LabelCompressedValue.Caption := '?';
  LabelDictionaryValue.Caption := Format('d%d (~%d MiB/thread)',
    [Progress.Dictionary,
     BeeDictionaryMemoryBytes(Progress.Dictionary) div (1024 * 1024)]);
  LabelThreadsValue.Caption := IntToStr(Progress.Threads);
  StatusBar.SimpleText := 'Optimization in progress...';
end;

procedure TMainForm.RunFinished(const ErrorText: string);
begin
  FRunDone := True;
  if ErrorText <> '' then
  begin
    StatusBar.SimpleText := 'Operation failed';
    MessageDlg('BeeOpt', ErrorText, mtError, [mbOK], 0);
  end
  else
    StatusBar.SimpleText := 'Operation completed';
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FRunDone;
  if not CanClose then
  begin
    FClosing := True;
    FRunner.Terminate;
    StatusBar.SimpleText := 'Stopping after the current batch...';
  end;
end;

procedure TMainForm.TimerCloseTimer(Sender: TObject);
begin
  if FClosing and FRunDone then Close;
end;

end.
