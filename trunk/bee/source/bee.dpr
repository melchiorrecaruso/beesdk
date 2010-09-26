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

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
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
  Bee_Common,
  Bee_Headers;

type
  { TCustomBeeApp class }

  TCustomBeeApp = class(TBeeApp)
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure OnMessage(const aMessage: string); override;
    procedure OnRequest(const aMessage: string); override;
    function  OnRename(const aItem: THeaderRec; const aValue: string): string; override;
    procedure OnList(const aItem: TFileInfoExtra; aVerbose: boolean); override;
    procedure OnProgress; override;
    procedure OnClear; override;
  end;

  { ------------------------------------------------------------------------ }
  { Implementation                                                           }
  { ------------------------------------------------------------------------ }

  { TCustomBeeApp class }

  constructor TCustomBeeApp.Create(aParams: TStringList);
  begin
    inherited Create(aParams);
  end;

  destructor TCustomBeeApp.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TCustomBeeApp.OnMessage(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  function TCustomBeeApp.OnRename(const aItem: THeaderRec; const aValue: string): string;
  begin
    Write('Rename file "', ParamToOem(aItem.Name), '" as (empty to skip):');
    Readln(Result);
    // convert oem to param
    Result := OemToParam(Result);
  end;

  procedure TCustomBeeApp.OnList(const aItem: TFileInfoExtra; aVerbose: boolean);
  begin
    with aItem do
    begin
      if aVerbose then
      begin
        if Length({Path +} Name) <= 15 then
        begin
          Writeln(ParamToOem(Format('%-15s', [{Path +} Name]) +
            Format(' %10s %10s %4u%% %14s %6s %8.8x %3s',
            [SizeToStr(Size), SizeToStr(PackedSize), Ratio,
            FileTimeToString(Time), AttrToStr(Attr), Crc, Method])));
        end else
        begin
          Writeln(ParamToOem({Path +} Name));
          Writeln(ParamToOem(StringOfChar(' ', 15) +
            Format(' %10s %10s %4u%% %14s %6s %8.8x %3s',
            [SizeToStr(Size), SizeToStr(PackedSize), Ratio,
            FileTimeToString(Time), AttrToStr(Attr), Crc, Method])));
        end;
      end else
      begin
        if Length({Path +} Name) <= 39 then
        begin
          Writeln(ParamToOem(Format('%-39s', [{Path +} Name]) +
            Format(' %10s %4u%% %14s %6s', [SizeToStr(Size),
            Ratio, FileTimeToString(Time), AttrToStr(Attr)])));
        end else
        begin
          Writeln(ParamToOem({Path +} Name));
          Writeln(ParamToOem(StringOfChar(' ', 39) +
            Format(' %10s %4u%% %14s %6s', [SizeToStr(Size),
            Ratio, FileTimeToString(Time), AttrToStr(Attr)])));
        end;
      end;
    end;
  end;

  procedure TCustomBeeApp.OnRequest(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
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

{$R *.res}

begin
  SetCtrlCHandler(@CtrlHandler);
  Params := TStringList.Create;
  for I := 1 to ParamCount do
  begin
    Params.Add(ParamStr(I));
  end;
  App := TCustomBeeApp.Create(Params);
  App.Execute;
  with App do
  begin
    ExitCode := Code;
  end;
  App.Destroy;
  Params.Destroy;
end.
