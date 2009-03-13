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
  TickFrm: TTickFrm;
  CommandLine: TCustomCommandLine;

begin
  {$I beegui.lrs}
  Application.Initialize;
  Application.HelpFile := '';

  CommandLine := TCustomCommandLine.Create(True);
  case CommandLine.Command of
  ' ': begin
         Application.Name := cApplicationName;
         Application.Title:= cApplicationName;
         Application.CreateForm(TMainFrm, MainFrm);
         Application.CreateForm(TConfigFrm, ConfigFrm);
         Application.Run;
       end;
  'O': begin
         Application.Name := cApplicationName;
         Application.Title:= cApplicationName;
         Application.CreateForm(TMainFrm, MainFrm);
         Application.CreateForm(TConfigFrm, ConfigFrm);
         if FileExists(CommandLine.ArchiveName) then
         begin
           MainFrm.ShowAndOpenArchive(CommandLine.ArchiveName);
         end;
         Application.Run;
       end;
  'V': if (ParamCount = 2) and FileExists(ParamStr(2)) then
       begin
         Application.Name := cApplicationViewerName;
         Application.Title := cApplicationViewerName;
         Application.CreateForm(TViewFrm, ViewFrm);
         begin
           ViewFrm.LoadFile(ParamStr(2));
         end;
         Application.Run;
       end;
  else begin
         Application.Name := cApplicationName;
         Application.Title := cApplicationName;
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
             TickFrm.ProgressOnTitle := True;
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
  end;
  FreeAndNil(CommandLine);;
end.
