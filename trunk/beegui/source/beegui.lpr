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

{$IFDEF WINDOWS}
  {$R beegui.rc}
{$ENDIF}

uses
  {$IFDEF UNIX}
    cThreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  Forms,
  SysUtils,
  Interfaces,
  LResources,
  // --- //
  BeeGui_Consts,
  BeeGui_Package,
  // --- //
  BeeGui_TickFrm,
  BeeGui_AboutFrm,
  BeeGui_CommandLine,
  // --- //
  BeeFM_MainFrm,
  BeeFM_ConfigFrm;

var
  CommandLine: TCustomCommandLine;

begin
  {$I beegui.lrs}
  MaxKeptOSChunks := 8;

  Application.Initialize;
  Application.HelpFile := '';
  Application.Name := cApplicationName;
  Application.Title := cApplicationName;

  CommandLine := TCustomCommandLine.Create(True);
  if CommandLine.Command = ' ' then
  begin
    Application.CreateForm(TMainFrm, MainFrm);
    Application.CreateForm(TConfigFrm, ConfigFrm);
    Application.Run;
  end else
  begin
    if CommandLine.Run then
    begin
      if CommandLine.Command in ['?'] then
      begin
        Application.CreateForm(TAboutFrm, AboutFrm);
        Application.Run;
      end else
      begin
        Application.CreateForm(TTickFrm, TickFrm);
        TickFrm.Execute(CommandLine, nil);
        TickFrm.OnlyAForm := True;
        repeat
          Application.ProcessMessages;
          if TickFrm.FrmCanClose then Break;
          if TickFrm.FrmCanShow  then Break;
        until CommandLine.Log;
        if CommandLine.Log then
          Application.Run
        else
          if TickFrm.FrmCanClose = False then
            Application.Run;
      end;
    end;
  end;
  FreeAndNil(CommandLine);;
end.

