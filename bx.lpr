{
  Copyright (c) 2012-2014 Melchiorre Caruso.

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

{
  Contains:

    The data compression utility.

  Modifyed:

    v1.0.0 build 2225 - 2014.01.26 by Melchiorre Caruso.

}

program bx;

{$I bx_compiler.inc}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$IFDEF MAC} TODO... , {$ENDIF}
  bx_application,
  bx_common,
  Classes;

var
  App: TBxApplication;

  { control+c event }

  {$IFDEF MSWINDOWS}
  function CtrlHandler(CtrlType: longword): longbool;
  begin
    case CtrlType of
      CTRL_C_EVENT:        App.Terminate;
      CTRL_BREAK_EVENT:    App.Terminate;
      CTRL_CLOSE_EVENT:    App.Terminate;
      CTRL_LOGOFF_EVENT:   App.Terminate;
      CTRL_SHUTDOWN_EVENT: App.Terminate;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  procedure CtrlHandler(sig: cint);
  begin
    case sig of
      SIGINT:  App.Terminate;
      SIGQUIT: App.Terminate;
      SIGKILL: App.Terminate;
      SIGSTOP: App.Terminate;
    end;
  end;
  {$ENDIF}

  {$IFDEF MAC}
  TODO...
  {$ENDIF}

begin
  SetCtrlCHandler(@CtrlHandler);
  App := TBxApplication.Create;
  App.Execute;
  App.Destroy;
end.