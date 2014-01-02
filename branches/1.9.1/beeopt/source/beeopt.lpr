program BxOpt;

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Interfaces,
  Forms,
  BxOpt_MainFrm,
  BxOpt_Optimizer;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  if Assigned(Optimizer) then Optimizer.Evolution;
  Application.Run;
end.
