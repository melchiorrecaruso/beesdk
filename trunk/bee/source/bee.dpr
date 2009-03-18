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
  Bee_Types,
  Bee_Common,
  Bee_Interface;

type
  // TConsole class

  TConsole = class
  private
    FApp: TApp;
    FKey: string;
    FParams: TStringList;
  private
    procedure ProcessFatalError(const aMessage: string);
    procedure ProcessError     (const aMessage: string);
    procedure ProcessWarning   (const aMessage: string);
    procedure ProcessMessage   (const aMessage: string);
    procedure ProcessOverwrite (const aFileInfo: TFileInfoRec; var Result: char);
    procedure ProcessRename    (const aFileInfo: TFileInfoRec; var Result: string);
    procedure ProcessList      (const aFileInfo: TFileFullInfoRec);
    procedure ProcessKey       (const aFileInfo: TFileInfoRec; var Result: string);
    procedure ProcessRequest   (const aMessage: string);
    procedure ProcessTick;
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
    I: integer;
begin
  inherited Create;
  FKey := '';
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
  FApp.OnKey        := ProcessKey;
  FApp.OnRequest    := ProcessRequest;
  FApp.OnTick       := ProcessTick;
  FApp.OnClear      := ProcessClear;
end;

destructor TConsole.Destroy;
begin
  FKey := '';
  FApp.Destroy;
  FParams.Destroy;
  inherited Destroy;
end;

procedure TConsole.Execute;
begin
  FApp.Execute;
  ExitCode := FApp.ExitCode;
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

procedure TConsole.ProcessOverwrite(const aFileInfo: TFileInfoRec; var Result: char);
begin
  with aFileInfo do
  begin
    Writeln('Warning: file "' + ParamToOem(FilePath + FileName) + '" already exists.');
    Write('Overwrite it?  [Yes/No/Rename/All/Skip/Quit]: ');
  end;
  // not convert oem to param
  Readln(Result);
end;

procedure TConsole.ProcessRename(const aFileInfo: TFileInfoRec; var Result: string);
begin
  with aFileInfo do
  begin
    Write('Rename file "' + ParamToOem(FilePath + FileName) + '" as (empty to skip):');
  end;
  Readln(Result);
  // convert oem to param
  Result := OemToParam(Result);
end;

procedure TConsole.ProcessList(const aFileInfo: TFileFullInfoRec);
begin
  with aFileInfo do
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

procedure TConsole.ProcessKey(const aFileInfo: TFileInfoRec; var Result: string);
begin
  if Length(FKey) = 0 then
  begin
    Write('Insert a key (min length 4 char): ');
    Readln(FKey);
    // convert oem to param
    FKey := OemToParam(FKey);
  end;
  Result := FKey;
end;

procedure Tconsole.ProcessRequest(const aMessage: string);
begin
  Writeln(ParamToOem(aMessage));
end;

procedure TConsole.ProcessTick;
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

begin
  Console := TConsole.Create;
  Console.Execute;
  Console.Destroy;
end.
