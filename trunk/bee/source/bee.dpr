{
  Copyright (c) 2003-2007 Andrew Filinsky and Melchiorre Caruso

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

  v0.7.9 build 0693 - 2008.03.31 by Melchiorre Caruso.
}

program Bee;

{$I compiler.inc}

uses
  {$IFDEF FPC} {$IFDEF UNIX}
  cThreads,
  {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  // ---
  Bee_App,
  Bee_Common,
  Bee_Interface;

type

  // TConsole class

  TConsole = class
  private
    App: TBeeApp;
    AppKey: string;
    AppInterface: TAppInterface;
    AppParams: TStringList;
    procedure OnFatalError;
    procedure OnOverWrite;
    procedure OnWarning;
    procedure OnDisplay;
    procedure OnRequest;
    procedure OnRename;
    procedure OnError;
    procedure OnClear;
    procedure OnList;
    procedure OnTick;
    procedure OnKey;
  public
    constructor Create; 
    destructor Destroy; override;
    procedure Execute;
  end;

  /// implementation ///

  // TConsole

  constructor TConsole.Create;
  var
    I: integer;
  begin
    inherited Create;

    AppInterface := TAppInterface.Create;
    AppInterface.OnFatalError.Method := OnFatalError;
    AppInterface.OnOverWrite.Method := OnOverWrite;
    AppInterface.OnWarning.Method := OnWarning;
    AppInterface.OnDisplay.Method := OnDisplay;
    AppInterface.OnRequest.Method := OnRequest;
    AppInterface.OnRename.Method := OnRename;
    AppInterface.OnClear.Method := OnClear;
    AppInterface.OnError.Method := OnError;
    AppInterface.OnList.Method := OnList;
    AppInterface.OnTick.Method := OnTick;
    AppInterface.OnKey.Method := OnKey;

    SetLength(AppKey, 0);
    AppParams := TStringList.Create;
    for I := 1 to ParamCount do
    begin
      AppParams.Add(ParamStr(I));
    end;
    App := TBeeApp.Create(AppInterface, AppParams);
  end;

  destructor TConsole.Destroy;
  begin
    SetLength(AppKey, 0);
    AppInterface.Destroy;
    AppParams.Destroy;
    inherited Destroy;
  end;

  procedure TConsole.Execute;
  begin
    App.Execute;
  end;

  procedure TConsole.OnFatalError;
  begin
    Writeln(ParamToOem(AppInterface.OnFatalError.Data.Msg));
  end;

  procedure TConsole.OnOverWrite;
  begin
    Writeln;
    with AppInterface.OnOverWrite.Data do
    begin
      Writeln('"' + ParamToOem(FilePath + FileName) + '" already exists.');
    end;
    Write('Overwrite it?  [Yes/No/Rename/All/Skip/Quit]: ');
    // not convert oem to param
    Readln(AppInterface.OnOverWrite.Answer);
    Writeln;
  end;

  procedure TConsole.OnKey;
  begin
    if Length(AppKey) = 0 then
    begin
      Writeln;
      Write('Insert a key (min length 4 char): ');
      Readln(AppKey);
      Writeln;
      // convert oem to param
      AppKey := OemToParam(AppKey);
    end;
    AppInterface.OnKey.Answer := AppKey;
  end;

  procedure TConsole.OnRename;
  begin
    Writeln;
    with AppInterface.OnRename.Data do
    begin
      Write('Rename file "' + ParamToOem(FilePath + FileName) + '" as (empty to skip):');
    end;
    Readln(AppInterface.OnRename.Answer);
    Writeln;
    // convert oem to param
    AppInterface.OnRename.Answer := OemToParam(AppInterface.OnRename.Answer);
  end;

  procedure TConsole.OnWarning;
  begin
    Writeln(ParamToOem(AppInterface.OnWarning.Data.Msg));
  end;

  procedure TConsole.OnDisplay;
  begin
    Writeln(ParamToOem(AppInterface.OnDisplay.Data.Msg));
  end;

  procedure Tconsole.OnRequest;
  begin
    Writeln(ParamToOem(AppInterface.OnRequest.Data.Msg));
  end;

  procedure TConsole.OnError;
  begin
    Writeln(ParamToOem(AppInterface.OnError.Data.Msg));
  end;

  procedure TConsole.OnList;
  begin
    with AppInterface.OnList.Data do
    begin
      Writeln(ParamToOem(FilePath + FileName));
      Writeln(ParamToOem(StringOfChar(' ', 15) +
        Format(' %10s %10s %4u%% %14s %6s %8.8x %4s',
        [SizeToStr(FileSize),
         SizeToStr(FilePacked),
         FileRatio,
         FileTimeToString(FileTime),
         AttrToStr(FileAttr),
         FileCrc,
         FileMethod
        ])));
    end;
  end;

  procedure TConsole.OnTick;
  begin
    // not convert oem to param
    Write(#8#8#8#8#8#8#8 + Format('  (%d%%)', [AppInterface.OnTick.Data.Percentage]));
  end;

  procedure TConsole.OnClear;
  begin
    Write(#13, #13: 80);
  end;

  /// main block ///

var
  Console: TConsole;

begin
  Console := TConsole.Create;
  Console.Execute;
  Console.Destroy;
end.
