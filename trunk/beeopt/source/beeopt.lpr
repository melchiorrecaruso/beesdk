program BeeOpt;

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Interfaces,
  Forms,
  BeeOpt_Form,
  BeeOpt_Optimizer;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  if Assigned(Optimizer) then Optimizer.Evolution;
  Application.Run;
end.

