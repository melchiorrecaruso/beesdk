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

    v0.8.0 build 1034 - 2009.04.27 by Melchiorre Caruso.
}

program Bee;

{$I compiler.inc}

uses
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$ENDIF}
  SysUtils,
  Classes,
  // ---
  Bee_App,
  Bee_Types,
  Bee_Common,
  Bee_Interface, bee_consts;

type
  // TConsole class

  TConsole = class
  private
    FApp: TApp;
    FKey: string;
    FParams: TStringList;
  private
    procedure ProcessFatalError(const aMessage: string);
    procedure ProcessError(const aMessage: string);
    procedure ProcessWarning(const aMessage: string);
    procedure ProcessMessage(const aMessage: string);
    procedure ProcessOverwrite(const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessRename(const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessList(const aFileInfo: TFileInfoExtra);
    procedure ProcessPassword(const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessRequest(const aMessage: string);
    procedure ProcessProgress;
    procedure ProcessClear;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

  /// implementation ///

  // TConsole

  constructor TConsole.Create;
  var
    I: longint;
  begin
    inherited Create;
    SetLength(FKey, 0);
    FParams := TStringList.Create;
    for I := 1 to ParamCount do
    begin
      FParams.Add(ParamStr(I));
    end;
    FApp := TBeeApp.Create(FParams);
    FApp.OnFatalError := ProcessFatalError;
    FApp.OnError      := ProcessError;
    FApp.OnWarning    := ProcessWarning;
    FApp.OnMessage    := ProcessMessage;
    FApp.OnOverwrite  := ProcessOverwrite;
    FApp.OnRename     := ProcessRename;
    FApp.OnList       := ProcessList;
    FApp.OnPassword   := ProcessPassword;
    FApp.OnRequest    := ProcessRequest;
    FApp.OnProgress   := ProcessProgress;
    FApp.OnClear      := ProcessClear;
  end;

  destructor TConsole.Destroy;
  begin
    FApp.Destroy;
    FParams.Destroy;
    SetLength(FKey, 0);
    inherited Destroy;
  end;

  procedure TConsole.Execute;
  begin
    FApp.Execute;
    ExitCode := FApp.Code;
  end;

  procedure TConsole.ProcessFatalError(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessError(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessWarning(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessMessage(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessOverwrite(const aFileInfo: TFileInfo; var Result: string);
  begin
    with aFileInfo do
    begin
      Writeln('Warning: file "',
        ParamToOem(PCharToString(FilePath)),
        ParamToOem(PCharToString(FileName)), '" already exists.');

      Write('Overwrite it?  [Yes/No/Rename/All/Skip/Quit]: ');
    end;
    // not convert oem to param
    Readln(Result);
  end;

  procedure TConsole.ProcessRename(const aFileInfo: TFileInfo; var Result: string);
  begin
    with aFileInfo do
    begin
      Write('Rename file "',
        ParamToOem(PCharToString(FilePath)),
        ParamToOem(PCharToString(FileName)), '" as (empty to skip):');
    end;
    Readln(Result);
    // convert oem to param
    Result := OemToParam(Result);
  end;

  procedure TConsole.ProcessList(const aFileInfo: TFileInfoExtra);
  begin
    with aFileInfo do
    begin
      if Length({FilePath +} FileName) <= 15 then
      begin
        Writeln(ParamToOem(Format('%-15s', [{FilePath +} FileName]) +
          Format(' %10s %10s %4u%% %14s %6s %8.8x %4s',
          [SizeToStr(FileSize), SizeToStr(FilePacked), FileRatio,
          FileTimeToString(FileTime), AttrToStr(FileAttr), FileCrc, FileMethod])));
      end else
      begin
        Writeln(ParamToOem({FilePath +} FileName));
        Writeln(ParamToOem(StringOfChar(' ', 15) +
          Format(' %10s %10s %4u%% %14s %6s %8.8x %4s',
          [SizeToStr(FileSize), SizeToStr(FilePacked), FileRatio,
          FileTimeToString(FileTime), AttrToStr(FileAttr), FileCrc, FileMethod])));
      end;
    end;
  end;

  procedure TConsole.ProcessPassword(const aFileInfo: TFileInfo; var Result: string);
  var
    S: string;
  begin
    if Length(FKey) = 0 then
    begin
      Write('Insert a key (min length 4 char): ');
      Readln(Result);
      // convert oem to param
      Result := OemToParam(Result);
      // store password
      Write('Do you want to use password for this session? [Yes, No]: ');
      Readln(S);
      if (Length(S)= 1) and (Upcase(S) = 'Y') then FKey := Result;
    end else
      Result := FKey;
  end;

  procedure Tconsole.ProcessRequest(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessProgress;
  begin
    // not convert oem to param
    Write(#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8 +
      Format('%5d KB/s %3d%%', [FApp.Speed shr 10, FApp.Percentes]));
  end;

  procedure TConsole.ProcessClear;
  begin
    Write(#13, #13: 80);
  end;

  /// main block ///

var
  Console: TConsole;

  /// control+c event ///

  {$IFDEF MSWINDOWS}
  function CtrlHandler(CtrlType: longword): longbool;
  begin
    case CtrlType of
      CTRL_C_EVENT:        Console.FApp.Terminated := True;
      CTRL_BREAK_EVENT:    Console.FApp.Terminated := True;
      CTRL_CLOSE_EVENT:    Console.FApp.Terminated := True;
      CTRL_LOGOFF_EVENT:   Console.FApp.Terminated := True;
      CTRL_SHUTDOWN_EVENT: Console.FApp.Terminated := True;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  procedure CtrlHandler(sig: cint);
  begin
    case sig of
      SIGINT:  Console.FApp.Terminated := True;
      SIGQUIT: Console.FApp.Terminated := True;
      SIGKILL: Console.FApp.Terminated := True;
      SIGSTOP: Console.FApp.Terminated := True;
    end;
  end;
  {$ENDIF}

  begin
    SetCtrlCHandler(@CtrlHandler);
    Console := TConsole.Create;
    Console.Execute;
    Console.Destroy;
  end.
