program BeeOpt;

uses
  Forms,
  BeeOpt_Form in 'BeeOpt_Form.pas' {MainForm},
  BeeConfiguration in '..\beeconfiguration.pas',
  BeeCommon in '..\beecommon.pas',
  BeeFiles in '..\beefiles.pas',
  BeeHeaders in '..\beeheaders.pas',
  BeeMainPacker in '..\beemainpacker.pas',
  BeeCrc in '..\beecrc.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  if MainForm.App <> nil then
    MainForm.App.Evolution;
  Application.Run;
end.
