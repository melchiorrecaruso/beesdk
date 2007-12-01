unit BeeGui_ProgressFrm;

interface

uses
  Forms,
  Classes,
  Dialogs,
  Buttons,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Graphics,
  Controls,
  StdCtrls,
  ComCtrls,
  // --
  Bee_Interface,
  // --
  BeeGui_FormStorage,
  BeeGui_ArchiveListManager, ExtCtrls;

type
  TProgressFrm = class(TForm)
    BtnForeGround: TBitBtn;
    BtnBackground: TBitBtn;
    BtnContinue: TBitBtn;
    BtnCancel: TBitBtn;
    BtnPause: TBitBtn;
    ProgressFrm_Messages: TComboBox;
    Bevel1: TBevel;
    ProgressFrm_ProgressBar: TProgressBar;
    ProgressFrm_Msg: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnBackgroundClick(Sender: TObject);
    procedure BtnForeGroundClick(Sender: TObject);
    procedure BtnPauseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    {Private declarations }
    FFormStorage: TFormStorage;
  private
    FApp: TApp;
    FAppList: TList;
    FAppKey: string;
    FAppArcName: string;
    FAppCreateLog: boolean;
    FAppRenameDir: boolean;
    FAppParams: TStringList;
    FAppRenameDirTo: string;
    FAppRenameDirFrom: string;
    FAppInterface: TAppInterface;
    FAppTerminated: boolean;
    FAppStarted: TDateTime;
    procedure FAppCreate;
    procedure FAppOnKey;
    procedure FAppOnTick;
    procedure FAppOnList;
    procedure FAppOnError;
    procedure FAppOnClear;
    procedure FAppOnRename;
    procedure FAppOnWarning;
    procedure FAppOnDisplay;
    procedure FAppOnOverWrite;
    procedure FAppInterfaceCreate;
    procedure FAppInterfaceDestroy;
    procedure FAppOnTerminate(Sender: TObject);
  public
    { Public declarations }
    function ShowModal: integer; override;
  public
    property AppList: TList Read FAppList Write FAppList;
    property AppKey: string Read FAppKey Write FAppKey;
    property AppArcName: string Read FAppArcName Write FAppArcName;
    property AppCreateLog: boolean Read FAppCreateLog Write FAppCreateLog;
    property AppRenameDir: boolean Read FAppRenameDir Write FAppRenameDir;
    property AppRenameDirTo: string Read FAppRenameDirTo Write FAppRenameDirTo;
    property AppRenameDirFrom: string Read FAppRenameDirFrom Write FAppRenameDirFrom;
    property AppParams: TStringList Read FAppParams Write FAppParams;
  end;

var
  ProgressFrm: TProgressFrm;

implementation

{$R *.dfm}

uses
  BeeGui_ProgressFrmRes,
  BeeGui_OverWriteFrm,
  BeeGui_RenameFrm,
  BeeGui_SysUtils,
  // ---
  Bee_App;

procedure TProgressFrm.FormCreate(Sender: TObject);
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SProgressFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SProgressFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SProgressFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
  BtnForeGround.Left := BtnBackGround.Left;
  BtnForeGround.Top := BtnBackGround.Top;
  BtnContinue.Left := BtnPause.Left;
  BtnContinue.Top  := BtnPause.Top;
  // ---
  FAppInterfaceCreate;
end;

procedure TProgressFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SProgressFrm_PropertyFull
  else
    FFormStorage.Properties := _SProgressFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
  // ---
  FAppInterfaceDestroy;
end;

procedure TProgressFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FAppTerminated;
  if not CanClose then
  begin
    BtnCancel.Click;
  end;
end;

function TProgressFrm.ShowModal: integer;
begin
  if Assigned(FAppParams) then
  begin
    FAppCreate;
    Result := inherited ShowModal;
  end else
    Result := mrCancel;
end;

/// ---

procedure TProgressFrm.BtnBackgroundClick(Sender: TObject);
begin
  BtnBackGround.Visible := False;
  BtnBackGround.Enabled := False;
  BtnForeGround.Visible := True;
  BtnForeGround.Enabled := True;
  if Assigned(FApp) then
  begin
    FApp.Priority := tpIdle;
  end;
end;

procedure TProgressFrm.BtnForeGroundClick(Sender: TObject);
begin
  BtnBackGround.Visible := True;
  BtnBackGround.Enabled := True;
  BtnForeGround.Visible := False;
  BtnForeGround.Enabled := False;
  if Assigned(FApp) then
  begin
    FApp.Priority := tpNormal;
  end;
end;

procedure TProgressFrm.BtnContinueClick(Sender: TObject);
begin
  if Assigned(FApp) then
  begin
    BtnPause.Visible := True;
    BtnPause.Enabled := True;
    BtnContinue.Visible := False;
    BtnContinue.Enabled := False;
    // ---
    FApp.Resume;
  end;
end;

procedure TProgressFrm.BtnPauseClick(Sender: TObject);
begin
  if Assigned(FApp) then
  begin
    BtnPause.Visible := False;
    BtnPause.Enabled := False;
    BtnContinue.Visible := True;
    BtnContinue.Enabled := True;
    // ---
    FApp.Suspend;
  end;
end;

procedure TProgressFrm.BtnCancelClick(Sender: TObject);
begin
  if Assigned(FApp) then
  begin
    FApp.Terminate;
    if FApp.Suspended then
    begin
      BtnContinue.Click;
    end;
  end;
end;



procedure TProgressFrm.FAppInterfaceCreate;
begin
  FAppCreateLog  := False;
  FAppTerminated := False;
  // ---
  FAppInterface  := TAppInterface.Create;
  FAppInterface.OnKey := FAppOnKey;
  FAppInterface.OnList := FAppOnList;
  FAppInterface.OnTick := FAppOnTick;
  FAppInterface.OnError := FAppOnError;
  FAppInterface.OnClear := FAppOnClear;
  FAppInterface.OnRename := FAppOnRename;
  FAppInterface.OnWarning := FAppOnWarning;
  FAppInterface.OnDisplay := FAppOnDisplay;
  FAppInterface.OnOverWrite := FAppOnOverWrite;
end;

procedure TProgressFrm.FAppInterfaceDestroy;
begin
  FAppInterface.Free;
end;

procedure TProgressFrm.FAppCreate;
begin
  FApp := TBeeApp.Create(FAppInterface, FAppParams);
  FApp.OnTerminate := FAppOnTerminate;
  FAppTerminated := False;
  FAppStarted := Now;
  FApp.Resume;
end;

procedure TProgressFrm.FAppOnKey;
begin
  FAppInterface.cMsg := FAppKey;
end;

procedure TProgressFrm.FAppOnTick;
begin
  ProgressFrm_ProgressBar.Position := FAppInterface.cPercentage;
end;

procedure TProgressFrm.FAppOnList;
begin
  FAppInterface.cList := FAppList;
end;

procedure TProgressFrm.FAppOnError;
begin
  ProgressFrm_Msg.Caption := (FAppInterface.cMsg);
end;

procedure TProgressFrm.FAppOnClear;
begin
  // nothing to do!
end;

procedure TProgressFrm.FAppOnRename;
var
  f: TRenameFrm;
begin
  if FAppRenameDir = False then
  begin
    f := TRenameFrm.Create(Self);
    f.Caption := ProgressFrm_Messages.Items[_CProgressFrm_RenameFile];
    f.RenameFrm_To.Text := FAppInterface.cFileName;
    begin
      if f.ShowModal = mrOk then
        FAppInterface.cMsg := f.RenameFrm_To.Text
      else
        FAppInterface.cMsg := '';
    end;
    f.Free;
  end else
  begin
    FAppInterface.cMsg := FAppInterface.cFileName;
    Delete(FAppInterface.cMsg, 1, Length(FAppRenameDirFrom));
    FAppInterface.cMsg := FAppRenameDirTo + FAppInterface.cMsg;
  end;
end;

procedure TProgressFrm.FAppOnWarning;
begin
  ProgressFrm_Msg.Caption := (FAppInterface.cMsg);
end;

procedure TProgressFrm.FAppOnDisplay;
begin
  ProgressFrm_Msg.Caption := (FAppInterface.cMsg);
end;

procedure TProgressFrm.FAppOnOverWrite;
var
  f: TOverWriteFrm;
begin
  f := TOverWriteFrm.Create(Self);
  with FAppInterface do
  begin
    f.Update(cFileName, cFileSize, cFileTime);
  end;
  case f.ShowModal of
    mrAbort: FAppInterface.cMsg := 'Q';
    mrNoToAll: FAppInterface.cMsg := 'S';
    mrYesToAll: FAppInterface.cMsg := 'A';
    mrNo: FAppInterface.cMsg  := 'N';
    mrYes: FAppInterface.cMsg := 'Y';
  end;
  f.Free;
end;

procedure TProgressFrm.FAppOnTerminate;
const
  TimeToSleep = 500;
begin
  if FAppTerminated = False then
  begin
    FAppTerminated := True;
    if ((Now - FAppStarted) * (24 * 60 * 60 * 100)) < TimeToSleep then
    begin
      Sleep(TimeToSleep);
    end;
    BtnCancel.Click;
  end;
end;

end.
