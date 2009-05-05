{
  Copyright (c) 1999-2009 Andrew Filinsky and Melchiorre Caruso

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

    Implements Genetic optimization for make optimal parameters for Bee 0.7.7.

  Modifyed:

}

program BeeOpt;

{$I compiler.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  Bee_Common,
  BeeOpt_App;

  {$IFDEF MSWINDOWS}
  function CtrlHandler(CtrlType: longword): longbool;
  begin
    case CtrlType of
      CTRL_C_EVENT:        App.NeedToClose := True;
      CTRL_BREAK_EVENT:    App.NeedToClose := True;
      CTRL_CLOSE_EVENT:    App.NeedToClose := True;
      CTRL_LOGOFF_EVENT:   App.NeedToClose := True;
      CTRL_SHUTDOWN_EVENT: App.NeedToClose := True;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  procedure CtrlHandler(sig: cint);
  begin
    case sig of
      SIGINT:  App.NeedToClose := True;
      SIGQUIT: App.NeedToClose := True;
      SIGKILL: App.NeedToClose := True;
      SIGSTOP: App.NeedToClose := True;
    end;
  end;
  {$ENDIF}

begin
  SetCtrlCHandler(@CtrlHandler);
  App := TOptApp.Create;
  App.Evolution;
  App.Free;
end.

