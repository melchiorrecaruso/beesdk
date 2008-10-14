{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

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

{ Contains:

    BeeGui application.

  Modifyed:
}

program BeeGui;

{$I compiler.inc}

{$IFDEF MSWINDOWS}
  {$R beegui.res}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cThreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Interfaces,
  SysUtils,
  Forms,
  // --- //
  BeeGui_Consts,
  BeeGui_Package,
  // --- //
  BeeGui_CmdLine,
  BeeGui_TickFrm,
  BeeGui_AboutFrm,
  BeeGui_RenameFrm,
  // --- //
  BeeFM_MainFrm,
  BeeFM_ViewFrm,
  BeeFM_ConfigFrm,
  BeeFM_SelectFrm,
  BeeFM_PropertyFrm;

var
  CmdLine: TCmdLine;

begin
  MaxKeptOSChunks := 8;
  if (ParamCount = 1) and (Lowercase(ParamStr(1)) = '-filemanager') then
  begin
    Application.Initialize;
    Application.HelpFile := '';
    Application.Name := cApplicationName;
    Application.Title := cApplicationTitle;
    Application.CreateForm(TMainFrm, MainFrm);
    Application.CreateForm(TConfigFrm, ConfigFrm);
    Application.Run;
  end else
  begin
    Application.Initialize;
    Application.Title:= cApplicationName;
    CmdLine := TCmdLine.Create;
    if CmdLine.Run then
    begin
      if CmdLine.Command in [' ', '?'] then
      begin
        Application.CreateForm(TAboutFrm, AboutFrm);
        Application.Run;
        AboutFrm.Free;
      end else
      begin
        Application.CreateForm(TTickFrm, TickFrm);
        with TickFrm do
        begin
          Start(CmdLine);
          repeat
            if CmdLine.Log then Break;
            if RemaingTime > 0 then Break;
            Application.ProcessMessages;
          until CanClose;
          if CmdLine.Log or (CanClose = False) then
          begin
            Application.Run;
          end;
        end;
        TickFrm.Free;
      end;
    end;
    CmdLine.Free;
  end;
end.

