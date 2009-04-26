program BeeGui;

{ Contains:

  BeeGui Main Source.

  (C) 2005 Andrew Filinsky, Melchiorre Caruso

  Modifyed:

  v1.0.1 build 9143 - 2005/07/09 Melchiorre Caruso
  v1.0.1 build 9158 - 2005/07/24 Melchiorre Caruso
  v1.0.1 build 9160 - 2005/08/03 Melchiorre Caruso

  v1.0.2 build 0216 - 2005/10/31 Melchiorre Caruso
}

{$R-,Q-,S-}

uses
  Forms,
  SysUtils,
  dialogs,
  BeeGui_Config,
  BeeGui_Common,
  BeeGui_AddFrm in 'forms\BeeGui_AddFrm.pas' {AddFrm},
  BeeGui_MainFrm in 'forms\BeeGui_MainFrm.pas' {MainFrm},
  BeeGui_InfoFrm in 'forms\BeeGui_InfoFrm.pas' {InfoFrm},
  BeeGui_AboutFrm in 'forms\BeeGui_AboutFrm.pas' {AboutFrm},
  BeeGui_RenameFrm in 'forms\BeeGui_RenameFrm.pas' {RenameFrm},
  BeeGui_ConfigFrm in 'forms\BeeGui_ConfigFrm.pas' {ConfigFrm},
  BeeGui_ExtractFrm in 'forms\BeeGui_ExtractFrm.pas' {ExtractFrm},
  BeeGui_PasswordFrm in 'forms\BeeGui_PasswordFrm.pas' {PasswordFrm},
  BeeGui_OverWriteFrm in 'forms\BeeGui_OverWriteFrm.pas' {OverwriteFrm},
  BeeGui_IntViewerFrm in 'forms\BeeGui_IntViewerFrm.pas' {IntViewerFrm},
  BeeGui_SelectFrm in 'forms\BeeGui_SelectFrm.pas' {SelectFrm};

{$R *.RES}

var
  I: integer;

begin
  Application.Initialize;
  Application.Title := 'BeeGui';
  Application.HelpFile := '';
  Application.CreateForm(TMainFrm, MainFrm);
  if (ParamCount > 2) then
  begin
    MainFrm.MainFrm_MainMenu_File_Close.Click;

    for I := 2 to ParamCount do
      if ParamStr (I)[1] <> '-' then
      begin
         MainFrm.App_ArcName := ParamStr (I);
         Break;
      end;

    if LowerCase (ParamStr (1) [1]) = 'l' then
      MainFrm.App_TerminateWith := twNone
    else
      MainFrm.App_TerminateWith := twClose;

    MainFrm.Caption := 'BeeGui' + ' - ' + ExtractFileName (MainFrm.App_ArcName);
    MainFrm.MainFrm_ListView.PathBoxSign := IncludeTrailingBackslash (ExtractFileName (MainFrm.App_ArcName));
    
    MainFrm.App_Params.Clear;
    for I := 1 to ParamCount do MainFrm.App_Params.Add (ParamStr (I));

    MainFrm.MainFrm_ListView.Initialize;
    MainFrm.App_Create (False);
  end;
  Application.Run;
end.
