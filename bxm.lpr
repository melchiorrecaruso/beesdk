program bxm;

{$I bxm_compiler.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms, virtualtreeview_package,
  lazcontrols,

  bxm_mainfrm;

{$R *.res}

begin
  RequireDerivedFormResource := TRUE;
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.



