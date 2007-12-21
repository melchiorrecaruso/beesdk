{
    Copyright (c) 2003-2007 Andrew Filinsky and Melchiorre Caruso

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
  {$IFDEF FPC} {$IFDEF UNIX}
  cthreads,
  {$ENDIF} {$ENDIF}
  // this includes the LCL widgetset
  Interfaces,
  SysUtils,
  Dialogs,
  Classes,
  Forms,
  // --
  BeeGui_SysUtils,
  // --
  BeeGui_ConfigFrm,
  BeeGui_MainFrm;

var
  I: integer;
  ParamList: TStringList;
  
  {$R beegui-ico.res}

begin
  Application.Title := 'BeeGui';

  {$I regcmd.inc}

  Application.Initialize;
  Application.HelpFile := '';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TConfigFrm, ConfigFrm);
  ParamList := TStringList.Create;
  try
    for I := 1 to ParamCount do
    begin
      ParamList.Add(ParamStr(I));
    end;
    if (ParamList.Count = 1) and (FileExists(ParamList.Strings[0])) then
    begin
      ParamList.Insert(0, 'l');
      ParamList.Add('*!');
    end;
    if (ParamList.Count > 2) then
    begin
      MainFrm.MainFrm_MainMenu_File_Close.Click;
      for I := 0 to ParamList.Count -1 do
      begin
        MainFrm.AppParams.Add(ParamList.Strings[I]);
        if (MainFrm.AppArcName = '') and (I > 0) then
        begin
          if (ParamList.Strings[I][1] <> '-') then
            MainFrm.AppArcName := ParamList.Strings[I];
        end;
      end;
      if (LowerCase(ParamList.Strings[0][1]) = 'l') then
      begin
        MainFrm.AppTerminateWith := twNone
      end else
      begin
        MainFrm.AppTerminateWith := twClose;
      end;
      MainFrm.Caption := 'BeeGui' + ' - ' + ExtractFileName(MainFrm.AppArcName);
      MainFrm.AppCreate(False);
    end;
  finally
    ParamList.Free;
  end;
  Application.Run;
end.

