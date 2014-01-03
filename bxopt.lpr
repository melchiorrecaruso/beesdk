program BeeOpt;

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Interfaces,
  Forms,
  bxOpt_MainFrm,
  bxOpt_Optimizer;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  if Assigned(Optimizer) then Optimizer.Evolution;
  Application.Run;
end.
