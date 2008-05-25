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
  Dialogs,
  Forms,
  // --- //
  BeeGui_CmdLine,
  BeeGui_TickFrm,
  BeeGui_AboutFrm;

var
  CmdLine:  TCmdLine  = nil;
  TickFrm:  TTickFrm  = nil;
  AboutFrm: TAboutFrm = nil;

begin
  Application.Initialize;
  Application.Title := 'BeeGui';
  CmdLine := TCmdLine.Create;
  if CmdLine.Run then
  begin
    if CmdLine.Command in [' ', '?'] then
    begin
       Application.CreateForm(TAboutFrm, AboutFrm);
       Application.Run;
       AboutFrm.Free;
       AboutFrm:=nil;
    end else
    begin
      Application.CreateForm(TTickFrm, TickFrm);
      TickFrm.Start(CmdLine);
      with TickFrm.Interfaces do
      begin
        while (OnTick.Data.TotalSize < $3FFFC) and (CmdLine.Log = False) do
        begin
          if Properties.Terminated = False then
            Application.ProcessMessages
          else
            Break;
        end;
        if (OnTick.Data.TotalSize >= $3FFFC) or (CmdLine.Log = True) then
        begin
          Application.Run;
        end;
      end;
      TickFrm.Free;
      TickFrm:=nil;
    end;
  end;
  CmdLine.Free;
  CmdLine:=nil;
end.

