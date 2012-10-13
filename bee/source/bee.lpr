{
  Copyright (c) 2003-2010 Andrew Filinsky and Melchiorre Caruso

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

    The data compression utility.

    Features:

    1. Uses context modelling and some variant of arithmetic encoding.
    2. Uses integer arithmetic only.
    3. Uses half-byte alphabet.

  Modifyed:

    v0.7.8 build 0150 - 2005.06.27 by Melchiorre Caruso;
    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.8 build 0154 - 2005.07.23 by Melchiorre Caruso;
    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;
    v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;

    v0.8.0 build 1180 - 2011.02.15 by Melchiorre Caruso.
}

program Bee;

{$I compiler.inc}

uses
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  {$ENDIF}
  Classes,
  SysUtils,
  Bee_App,
  Bee_Types,
  Bee_Consts,
  Bee_Common;

type
  { TCustomBeeApp class }

  TCustomBeeApp = class(TBeeApp)
  public
    procedure DoMessage(const aMessage: string); override;
    procedure DoRequest(const aMessage: string); override;
    function  DoRename(const aItem: THeader; const aValue: string): string; override;
    procedure DoList(const aItem: THeader); override;
    function  DoTick(Value: longint): boolean; override;
    procedure DoClear; override;
  end;

  { ------------------------------------------------------------------------ }
  { Implementation                                                           }
  { ------------------------------------------------------------------------ }

  { TCustomBeeApp class }

  procedure TCustomBeeApp.DoMessage(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  function TCustomBeeApp.DoRename(const aItem: THeader; const aValue: string): string;
  begin
    Write('Rename file "', ParamToOem(aItem.Name), '" as (empty to skip):');
    Readln(Result);
    // convert oem to param
    Result := OemToParam(Result);
  end;

  procedure TCustomBeeApp.DoList(const aItem: THeader);
  begin
    Writeln(Format('%16s %7s %12s %12s %3s %s', [
      FileTimeToString(aItem.Time), AttrToStr(aItem.Attr),
      SizeToStr(aItem.Size), SizeToStr(aItem.PackedSize),
      MethodToStr(aItem), aItem.Name]));
  end;

  procedure TCustomBeeApp.DoRequest(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  function TCustomBeeApp.DoTick(Value: longint): boolean;
  begin
    Result := inherited DoTick(Value);
    // not convert oem to param
    Write(#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8,
      Format('%5d KB/s %3d%%', [Speed shr 10, Progress]));
  end;

  procedure TCustomBeeApp.DoClear;
  begin
    Write(#13, #13: 80);
  end;

  { ------------------------------------------------------------------------ }
  { main block                                                               }
  { ------------------------------------------------------------------------ }

var
  I:      longint;
  Params: TStringList;
  App:    TCustomBeeApp;

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

begin
  SetCtrlCHandler(@CtrlHandler);
  Params := TStringList.Create;
  for I := 1 to ParamCount do
  begin
    Params.Add(ParamStr(I));
  end;
  App := TCustomBeeApp.Create(Params.Text);
  App.Execute;
  begin
    ExitCode := App.ExitCode;
  end;
  App.Destroy;
  Params.Destroy;
end.
