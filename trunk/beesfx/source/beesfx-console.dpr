{
  Copyright (c) 2005-2009 Andrew Filinsky and Melchiorre Caruso

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

  The SFX module for generic platform.

  Features:

  1. Uses context modelling and some variant of arithmetic encoding.
  2. Uses integer arithmetic only.
  3. Uses half-byte alphabet.

  (C) 2005-2009 Melchiorre Caruso.

  Modifyed:

  v0.1.0 build 0062 - 2007.01.27 by Melchiorre Caruso;

  v0.1.0 build 0070 - 2009.05.26 by Melchiorre Caruso.
}


program BeeSfx;

{$I compiler.inc}

{$R beesfx-ico.res}

uses
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$ENDIF}
  Classes,
  SysUtils,
  // ---
  Bee_App,
  Bee_Types,
  Bee_Consts,
  Bee_Common,
  Bee_Headers,
  Bee_Interface;

type

  TCustomBeeApp = class(TBeeApp)
  public
    procedure OnMessage(const aMessage: string); override;
    procedure OnRequest(const aMessage: string); override;
    function  OnRename(const aItem: THeader; const aValue: string): string; override;
    procedure OnList(const aItem: THeader); override;
    procedure OnProgress; override;
    procedure OnClear; override;
  end;

  procedure TCustomBeeApp.OnMessage(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TCustomBeeApp.OnRequest(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  function TCustomBeeApp.OnRename(const aItem: THeader; const aValue: string): string;
  begin
    // nothing to do
  end;

  procedure TCustomBeeApp.OnList(const aItem: THeader);
  begin
    // nothing to do
  end;

  procedure TCustomBeeApp.OnProgress;
  begin
    // not convert oem to param
    Write(#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8,
      Format('%5d KB/s %3d%%', [Speed shr 10, Percentage]));
  end;

  procedure TCustomBeeApp.OnClear;
  begin
    Write(#13, #13: 80);
  end;

  /// Main block

var
  I: longint;
  Answer: char;
  Params: TStringList;
  App: TCustomBeeApp = nil;
  ExtractionPath: string = '';
  Password: string;

  /// control+c event ///

  {$IFDEF MSWINDOWS}
  function CtrlHandler(CtrlType: longword): longbool;
  begin
    case CtrlType of
      CTRL_C_EVENT:        App.Code := ccUserAbort;
      CTRL_BREAK_EVENT:    App.Code := ccUserAbort;
      CTRL_CLOSE_EVENT:    App.Code := ccUserAbort;
      CTRL_LOGOFF_EVENT:   App.Code := ccUserAbort;
      CTRL_SHUTDOWN_EVENT: App.Code := ccUserAbort;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  procedure CtrlHandler(sig: cint);
  begin
    case sig of
      SIGINT:  App.Code := ccUserAbort;
      SIGQUIT: App.Code := ccUserAbort;
      SIGKILL: App.Code := ccUserAbort;
      SIGSTOP: App.Code := ccUserAbort;
    end;
  end;
  {$ENDIF}

  // main-block //

  begin
    SetCtrlCHandler(@CtrlHandler);
    Writeln('The Bee self-extractor 0.2.0 build 0100 archiver utility, Feb 2011.' +
      Cr + '(C) 2005-2009 Andrew Filinsky and Melchiorre Caruso.');

    Write(Cr + 'Set extraction path (empty for current path): ');
    Readln(ExtractionPath);

    Write(Cr + 'If archive has a password, you can now insert: ');
    Readln(Password);

    ForceDirectories(ExtractionPath);
    if SetCurrentDir(ExtractionPath) then
    begin
      repeat
        Write(Cr + 'Do you continue with files extraction? [Y, N] ');
        Readln(Answer);
      until UpCase(Answer) in ['Y', 'N'];

      if UpCase(Answer) in ['Y', 'y'] then
      begin
        Params := TStringList.Create;
        Params.Add('t');
        Params.Add('-p' + Password);
        Params.Add(ParamStr(0));

        Write(Cr + 'Start extractor module ...' + Cr);
        App := TCustomBeeApp.Create(Params.Text);
        App.Execute;
        ExitCode := App.Code;
        App.Destroy;
        Params.Destroy;
      end else
        Write(Cr + 'Error: can''t set extraction path');
    end;
  end.
