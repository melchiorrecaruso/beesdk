program bxm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  lazcontrols,

  bxm_addfrm,
  bxm_aboutFrm,
  bxm_mainfrm
  { you can add units after this };

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainFrm,  MainFrm);
  Application.CreateForm(TAboutFrm, AboutFrm);
  Application.Run;
end.

