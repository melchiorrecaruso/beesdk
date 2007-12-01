unit BeeGui_TaskFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Buttons,
  Classes,
  Controls,
  StdCtrls,
  Graphics,
  ExtCtrls,
  SysUtils,
  // ---
  BeeGui_FormStorage;

type
  TTaskFrm = class(TForm)
    General_Priority: TComboBox;
    TaskFrm_ExistsActiveProcess: TLabel;
    TaskFrm_Actions: TComboBox;
    TaskFrm_PriorityL: TLabel;
    TaskFrm_ActionsL: TLabel;
    TaskFrm_Image: TImage;
    BtnOk: TBitBtn;
    Bevel: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TaskFrm_ActionsChange(Sender: TObject);
  private
    { Private declarations }
    FFormStorage: TFormStorage;
  public
    { Public declarations }
    procedure SetAction(Action: integer);
    procedure SetGeneralPriority(Priority: TThreadPriority);
    function  GetGeneralPriority: TThreadPriority;
  end;

var
  TaskFrm: TTaskFrm;

implementation

{$R *.dfm}

uses
  BeeGui_TaskFrmRes,
  BeeGui_SysUtils;

procedure TTaskFrm.FormCreate(Sender: TObject);
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _STaskFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _STaskFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _STaskFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
  TaskFrm_ActionsChange(Self);
end;

procedure TTaskFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _STaskFrm_PropertyFull
  else
    FFormStorage.Properties := _STaskFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
end;

procedure TTaskFrm.TaskFrm_ActionsChange(Sender: TObject);
begin
  if TaskFrm_Actions.ItemIndex = 0 then
    General_Priority.Enabled := True
  else
    General_Priority.Enabled := False;
end;

procedure TTaskFrm.SetAction(Action: integer);
begin
  TaskFrm_Actions.ItemIndex := Action;
  TaskFrm_ActionsChange(Self);
end;

procedure TTaskFrm.SetGeneralPriority(Priority: TThreadPriority);
begin
  case Priority of
    tpIdle:         General_Priority.ItemIndex := 0;
    tpNormal:       General_Priority.ItemIndex := 1;
    tpHigher:       General_Priority.ItemIndex := 2;
    tpTimeCritical: General_Priority.ItemIndex := 3;
  end;
end;

function TTaskFrm.GetGeneralPriority;
begin
  case General_Priority.ItemIndex of
    0: Result := tpIdle;
    1: Result := tpNormal;
    2: Result := tpHigher;
    3: Result := tpTimeCritical;
  end;
end;

end.
