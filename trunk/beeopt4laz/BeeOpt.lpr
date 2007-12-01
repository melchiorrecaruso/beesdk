program BeeOpt;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here },
  BeeOpt_Form in 'BeeOpt_Form.pas' {MainForm},
  Bee_Configuration in '..\bee_configuration.pas',
  Bee_Common in '..\bee_common.pas',
  Bee_Files in '..\bee_files.pas',
  Bee_Headers in '..\bee_headers.pas',
  Bee_MainPacker in '..\bee_mainpacker.pas',
  Bee_Crc in '..\bee_crc.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  If Assigned(MainForm.App) then MainForm.App.Evolution;
  Application.Run;
end.

