{
  Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

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

    v1.0.5 build 0860 - 2009.05.05 by Melchiorre Caruso.
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
  Forms,
  Dialogs,
  SysUtils,
  Interfaces,
  LResources,
  // --- //
  BeeGui_Consts,
  BeeGui_Package,
  BeeGui_SysUtils,
  // --- //
  BeeGui_TickFrm,
  BeeGui_AboutFrm,
  BeeGui_CommandLine,
  // --- //
  BeeFM_MainFrm,
  BeeFm_ViewFrm,
  BeeFM_ConfigFrm;

var
  TickFrm:     TTickFrm;
  CommandLine: TCustomCommandLine;

begin
  {$I beegui.lrs}
  Application.Initialize;
  Application.HelpFile := '';

  CommandLine := TCustomCommandLine.Create(True);
  if (ParamCount = 1) and FileExists(ParamStr(1)) then
  begin
    Application.Name  := cApplicationName;
    Application.Title := cApplicationName;
    Application.CreateForm(TMainFrm, MainFrm);
    Application.CreateForm(TConfigFrm, ConfigFrm);
    begin
      MainFrm.ShowAndOpenArchive(ParamStr(1));
    end;
    Application.Run;
  end else
  if CommandLine.Command = ' ' then
  begin
    Application.Name  := cApplicationName;
    Application.Title := cApplicationName;
    Application.CreateForm(TMainFrm, MainFrm);
    Application.CreateForm(TConfigFrm, ConfigFrm);
    Application.Run;
  end else
  if CommandLine.Command = 'L' then
  begin
    Application.Name  := cApplicationName;
    Application.Title := cApplicationName;
    Application.CreateForm(TMainFrm, MainFrm);
    Application.CreateForm(TConfigFrm, ConfigFrm);
    if FileExists(CommandLine.ArchiveName) then
    begin
      MainFrm.ShowAndOpenArchive(ParamStr(1));
    end;
    Application.Run;
  end else
  if CommandLine.Command = '?' then
  begin
    Application.CreateForm(TAboutFrm, AboutFrm);
    Application.Run;
  end else
  if CommandLine.Command = 'V' then
  begin
    if (ParamCount = 2) and FileExists(ParamStr(2)) then
    begin
      Application.Name  := cApplicationViewerName;
      Application.Title := cApplicationViewerName;
      Application.CreateForm(TViewFrm, ViewFrm);
      begin
        ViewFrm.LoadFile(ParamStr(2));
      end;
      Application.Run;
    end;
  end else
  begin
    Application.Name  := cApplicationName;
    Application.Title := cApplicationName;
    if CommandLine.Run then
    begin
      Application.CreateForm(TTickFrm, TickFrm);
      TickFrm.Execute(CommandLine, nil);
      TickFrm.ProgressOnTitle := True;
      repeat
        Application.ProcessMessages;
        if TickFrm.FrmCanClose then Break;
        if TickFrm.FrmCanShow then Break;
      until CommandLine.Log;
      if CommandLine.Log then
        Application.Run
      else
        if TickFrm.FrmCanClose = False then
          Application.Run;
    end;
  end;
  FreeAndNil(CommandLine);
end.
