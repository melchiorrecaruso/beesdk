program BeeOpt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces, Forms, BeeOptMainForm;

{$IFDEF WINDOWS}
{$R beeopt.res}
{$ENDIF}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.Title := 'Bee optimizer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
