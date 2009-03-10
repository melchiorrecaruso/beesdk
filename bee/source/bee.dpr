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

    v0.7.9 build 0962 - 2009.02.27 by Melchiorre Caruso.
}

program Bee;

{$I compiler.inc}

uses
  {$IFDEF FPC}
  {$IFDEF UNIX}
  cThreads,
  cMem,
  {$ENDIF}
  {$ENDIF}
  SysUtils,
  Classes,
  // ---
  Bee_App,
  Bee_Common,
  Bee_Interface;

type
  // TConsole class

  TConsole = class
  private
    App: TApp;
    AppKey: string;
    AppParams: TParams;
    AppInterfaces: TInterfacesRec;
    procedure FatalError;
    procedure Error;
    procedure Warning;
    procedure Display;
    procedure FileOverWrite;
    procedure FileRename;
    procedure FileList;
    procedure FileKey;
    procedure Request;
    procedure Clear;
    procedure Tick;
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
  with AppInterfaces do
  begin
    OnFatalError   .Method := FatalError;
    OnError        .Method := Error;
    OnWarning      .Method := Warning;
    OnDisplay      .Method := Display;
    OnFileOverWrite.Method := FileOverWrite;
    OnFileRename   .Method := FileRename;
    OnFileList     .Method := FileList;
    OnFileKey      .Method := FileKey;
    OnRequest      .Method := Request;
    OnClear        .Method := Clear;
    OnTick         .Method := Tick;
  end;

  AppKey := '';
  AppParams := TStringList.Create;
  for I := 1 to ParamCount do
  begin
    AppParams.Add(ParamStr(I));
  end;
  App := TBeeApp.Create(@AppInterfaces, AppParams);
end;

destructor TConsole.Destroy;
begin
  AppKey := '';
  AppParams.Destroy;
  inherited Destroy;
end;

procedure TConsole.Execute;
begin
  App.Execute;
end;

procedure TConsole.FatalError;
begin
  Writeln(ParamToOem(AppInterfaces.OnFatalError.Data.Msg));
end;

procedure TConsole.FileOverWrite;
begin
  with AppInterfaces.OnFileOverWrite.Data do
  begin
    Writeln('Warning: file "' + ParamToOem(FilePath + FileName) + '" already exists.');
    Write('Overwrite it?  [Yes/No/Rename/All/Skip/Quit]: ');
  end;
  // not convert oem to param
  Readln(AppInterfaces.OnFileOverWrite.Answer);
end;

procedure TConsole.FileKey;
begin
  if Length(AppKey) = 0 then
  begin
    Write('Insert a key (min length 4 char): ');
    Readln(AppKey);
    // convert oem to param
    AppKey := OemToParam(AppKey);
  end;
  AppInterfaces.OnFileKey.Answer := AppKey;
end;

procedure TConsole.FileRename;
begin
  with AppInterfaces.OnFileRename.Data do
  begin
    Write('Rename file "' + ParamToOem(FilePath + FileName) + '" as (empty to skip):');
  end;
  Readln(AppInterfaces.OnFileRename.Answer);
  // convert oem to param
  AppInterfaces.OnFileRename.Answer := OemToParam(AppInterfaces.OnFileRename.Answer);
end;

procedure TConsole.Warning;
begin
  Writeln(ParamToOem(AppInterfaces.OnWarning.Data.Msg));
end;

procedure TConsole.Display;
begin
  Writeln(ParamToOem(AppInterfaces.OnDisplay.Data.Msg));
end;

procedure Tconsole.Request;
begin
  Writeln(ParamToOem(AppInterfaces.OnRequest.Data.Msg));
end;

procedure TConsole.Error;
begin
  Writeln(ParamToOem(AppInterfaces.OnError.Data.Msg));
end;

procedure TConsole.FileList;
begin
  with AppInterfaces.OnFileList.Data do
  begin
    if Length({FilePath +} FileName) <= 15 then
    begin
      Writeln(ParamToOem(
        Format('%-15s', [{FilePath +} FileName]) +
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

procedure TConsole.Tick;
begin
  // not convert oem to param
  with AppInterfaces.OnTick.Data do
  begin
    Write(#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8 +
      Format('%5d KB/s %3d%%', [Speed, Percentage]));
  end;
end;

procedure TConsole.Clear;
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
