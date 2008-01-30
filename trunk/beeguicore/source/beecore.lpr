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

    BeeCore application.

  Modifyed:

}

program BeeCore;

{$I compiler.inc}
{$R beegui.ico.res}

uses
  {$IFDEF UNIX} cThreads, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Interfaces,
  Forms,
  // ---
  BeeCore_Gui,
  BeeCore_CmdLine,
  BeeCore_TickFrm,
  BeeCore_AboutFrm;

var
  Gui: TGui;
  CmdLine: TCmdLine;

begin
  Application.Initialize;
  Application.Title := 'BeeCore';
  CmdLine := TCmdLine.Create;
  if CmdLine.Run then
  begin
    if CmdLine.Command in [' ', '?'] then
    begin
       Application.CreateForm(TAboutFrm, AboutFrm);
       Application.Run;
    end else
    begin
      Application.CreateForm(TTickFrm, TickFrm);
      Gui := TGui.Create(TickFrm, CmdLine);
      repeat
        if Gui.Terminated then
          Break
        else
          Application.ProcessMessages;
      until Gui.Switch;

      if Gui.Switch then
      begin
        Application.Run;
      end;
      Gui.Free;
    end;
    CmdLine.Free;
  end;
end.

