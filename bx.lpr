{
  Copyright (c) 2013 Melchiorre Caruso.

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

  Fist release:

    v1.0 build 0020 - 2013.10.06 by Melchiorre Caruso.

  Modifyed:

}

program bx;

{$I bx_compiler.inc}

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Classes,
  // ---
  bx_Application,
  bx_Common, bx_filescanner;

var
  Application: TBxApplication;

  { control+c event }

  {$IFDEF MSWINDOWS}
  function CtrlHandler(CtrlType: longword): longbool;
  begin
    case CtrlType of
      CTRL_C_EVENT:        Application.Terminate;
      CTRL_BREAK_EVENT:    Application.Terminate;
      CTRL_CLOSE_EVENT:    Application.Terminate;
      CTRL_LOGOFF_EVENT:   Application.Terminate;
      CTRL_SHUTDOWN_EVENT: Application.Terminate;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  procedure CtrlHandler(sig: cint);
  begin
    case sig of
      SIGINT:  Application.Terminate;
      SIGQUIT: Application.Terminate;
      SIGKILL: Application.Terminate;
      SIGSTOP: Application.Terminate;
    end;
  end;
  {$ENDIF}

begin
  SetCtrlCHandler(@CtrlHandler);
  Application := TBxApplication.Create;
  Application.Execute;
  Application.Destroy;
end.
