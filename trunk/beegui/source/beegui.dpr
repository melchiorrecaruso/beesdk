{
    Copyright (c) 2005-2007 Andrew Filinsky and Melchiorre Caruso

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

program BeeGui;

{$I compiler.inc}

uses
  Forms,
  Classes,
  Dialogs,
  SysUtils,
  // ---
  BeeGui_AddFrm in 'forms\BeeGui_AddFrm.pas' {AddFrm},
  BeeGui_TaskFrm in 'forms\BeeGui_TaskFrm.pas' {TaskFrm},
  BeeGui_MainFrm in 'forms\BeeGui_MainFrm.pas' {MainFrm},
  BeeGui_InfoFrm in 'forms\BeeGui_InfoFrm.pas' {InfoFrm},
  BeeGui_AboutFrm in 'forms\BeeGui_AboutFrm.pas' {AboutFrm},
  BeeGui_SelectFrm in 'forms\BeeGui_SelectFrm.pas' {SelectFrm},
  BeeGui_RenameFrm in 'forms\BeeGui_RenameFrm.pas' {RenameFrm},
  BeeGui_ConfigFrm in 'forms\BeeGui_ConfigFrm.pas' {ConfigFrm},
  BeeGui_ExtractFrm in 'forms\BeeGui_ExtractFrm.pas' {ExtractFrm},
  BeeGui_PasswordFrm in 'forms\BeeGui_PasswordFrm.pas' {PasswordFrm},
  BeeGui_OverWriteFrm in 'forms\BeeGui_OverWriteFrm.pas' {OverwriteFrm},
  BeeGui_IntViewerFrm in 'forms\BeeGui_IntViewerFrm.pas' {IntViewerFrm};
  

{$R *.res}

var
  I: integer;
  Command: string;
begin
  Application.Initialize;
  Application.Title := 'BeeGui';
  Application.HelpFile := '';
  Application.CreateForm(TMainFrm, MainFrm);

  if ParamCount > 2 then
  begin
    MainFrm.MainApp.AppInterfaceClear;
    MainFrm.MainApp.AppTerminateFlags := [tfClose];
    // ---
    Command := ParamStr(1);
    if (Length(Command) = 2) and (Command[2]= '?') then
    begin
      case Command[1] of
        'a': begin
               Command := 'l';
               MainFrm.MainApp.AppTerminateFlags := [tfClose, tfAdd];
             end;
        'e': begin
               Command := 'l';
               MainFrm.MainApp.AppTerminateFlags := [tfClose, tfExtract];
             end;
        'x': begin
               Command := 'l';
               MainFrm.MainApp.AppTerminateFlags := [tfClose, tfExtract];
             end;
        'l': begin
		           Command := 'l';
               MainFrm.MainApp.AppTerminateFlags := [];
             end;
        else Command := Command[1];
      end;
      MainFrm.MainApp.AppParams.Add(Command);

      I := 2;
      while I <= ParamCount do
      begin
        if ParamStr(I)[1] <> '-' then
        begin
          MainFrm.MainApp_ArcName(ExpandFileName(ParamStr(I)));
          MainFrm.MainApp.AppParams.Add(ExpandFileName(ParamStr(I)));
          Inc(I);
          Break;
        end;
        Inc(I);
      end;

      MainFrm.MainApp_FileMasks := TStringList.Create;
      while I <= ParamCount do
      begin
        MainFrm.MainApp_FileMasks.Add(ParamStr(I));
        Inc(I);
      end;
    end else
    begin
      MainFrm.MainApp.AppParams.Add(Command);
      for I := 2 to ParamCount do
      begin
        MainFrm.MainApp.AppParams.Add(ParamStr(I));
      end;
      for I := 2 to ParamCount do
      begin
        if ParamStr(I)[1] <> '-' then
        begin
          MainFrm.MainApp_ArcName(ParamStr(I));
          Break;
        end;
      end;
    end;
    MainFrm.MainApp.Execute;
  end;
  Application.Run;
end.
