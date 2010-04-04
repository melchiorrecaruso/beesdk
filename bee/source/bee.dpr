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

    v0.8.0 build 1100 - 2010.01.23 by Melchiorre Caruso.
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
  private
    FPassword: string;
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure OnMessage(const aMessage: string); override;
    procedure OnRequest(const aMessage: string); override;
    function  OnOverwrite(const aFileInfo: TFileInfo; const aValue: TOverwriteMode): TOverwriteMode; override;
    function  OnRename(const aFileInfo: TFileInfo; const aValue: string): string; override;
    function  OnPassword(const aFileInfo: TFileInfo; const aValue: string): string; override;
    procedure OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean); override;
    procedure OnProgress; override;
    procedure OnClearLine; override;
  end;

  { ------------------------------------------------------------------------ }
  { Implementation                                                           }
  { ------------------------------------------------------------------------ }

  { TCustomBeeApp class }

  constructor TCustomBeeApp.Create(aParams: TStringList);
  begin
    inherited Create(aParams);
    SetLength(FPassword, 0);
  end;

  destructor TCustomBeeApp.Destroy;
  begin
    SetLength(FPassword, 0);
    inherited Destroy;
  end;

  procedure TCustomBeeApp.OnMessage(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  function TCustomBeeApp.OnOverwrite(const aFileInfo: TFileInfo; const aValue: TOverwriteMode): TOverwriteMode;
  var
    I: longint;
  begin
    Writeln('Warning: file "',
      ParamToOem(PCharToString(aFileInfo.FilePath)),
      ParamToOem(PCharToString(aFileInfo.FileName)),
      '" already exists,' + Cr + 'Do you want:');

    Writeln(' 0 - Add');
    Writeln(' 1 - Update');
    Writeln(' 2 - Replace');
    Writeln(' 3 - AddUpdate');
    Writeln(' 4 - AddReplace');
    Writeln(' 5 - AddAutoRename');
    Writeln(' 6 - UpdateOne');
    Writeln(' 7 - ReplaceOne');
    Writeln(' 8 - RenameOne');
    Writeln(' 9 - omSkip');
    Writeln('10 - Quit');
    repeat
      Write('Insert code [0.. 10]: ');
      Read(I);
    until I in [0.. 10];

    case I of
       0: Result := omAdd;
       1: Result := omUpdate;
       2: Result := omReplace;
       3: Result := omAddUpdate;
       4: Result := omAddReplace;
       5: Result := omAddAutoRename;
       6: Result := omUpdateOne;
       7: Result := omReplaceOne;
       8: Result := omRenameOne;
       9: Result := omSkip;
      10: Result := omQuit;
    end;
  end;

  function TCustomBeeApp.OnRename(const aFileInfo: TFileInfo; const aValue: string): string;
  begin
    with aFileInfo do
    begin
      Write('Rename file "',
        ParamToOem(PCharToString(FilePath)),
        ParamToOem(PCharToString(FileName)), '" as (empty to skip):');
    end;
    Readln;
    Readln(Result);
    // convert oem to param
    Result := OemToParam(Result);
  end;

  procedure TCustomBeeApp.OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
  begin
    with aFileInfo do
    begin
      if aVerbose then
      begin
        if Length({FilePath +} FileName) <= 15 then
        begin
          Writeln(ParamToOem(Format('%-15s', [{FilePath +} FileName]) +
            Format(' %10s %10s %4u%% %14s %6s %8.8x %3s',
            [SizeToStr(FileSize), SizeToStr(FilePacked), FileRatio,
            FileTimeToString(FileTime), AttrToStr(FileAttr), FileCrc, FileMethod])));
        end
        else
        begin
          Writeln(ParamToOem({FilePath +} FileName));
          Writeln(ParamToOem(StringOfChar(' ', 15) +
            Format(' %10s %10s %4u%% %14s %6s %8.8x %3s',
            [SizeToStr(FileSize), SizeToStr(FilePacked), FileRatio,
            FileTimeToString(FileTime), AttrToStr(FileAttr), FileCrc, FileMethod])));
        end;
      end
      else
      begin
        if Length({FilePath +} FileName) <= 39 then
        begin
          Writeln(ParamToOem(Format('%-39s', [{FilePath +} FileName]) +
            Format(' %10s %4u%% %14s %6s', [SizeToStr(FileSize),
            FileRatio, FileTimeToString(FileTime), AttrToStr(FileAttr)])));
        end
        else
        begin
          Writeln(ParamToOem({FilePath +} FileName));
          Writeln(ParamToOem(StringOfChar(' ', 39) +
            Format(' %10s %4u%% %14s %6s', [SizeToStr(FileSize),
            FileRatio, FileTimeToString(FileTime), AttrToStr(FileAttr)])));
        end;
      end;
    end;
  end;

  function TCustomBeeApp.OnPassword(const aFileInfo: TFileInfo; const aValue: string): string;
  var
    S: string;
  begin
    if Length(FPassword) = 0 then
    begin
      Write('Insert a key (min length 4 char): ');
      Readln(Result);
      // convert oem to param
      Result := OemToParam(Result);
      // store password
      Write('Do you want to use password for this session? [Yes, No]: ');
      Readln(S);
      if (Length(S) = 1) and (UpCase(S[1]) = 'Y') then
        FPassword := Result;
    end
    else
      Result := FPassword;
  end;

  procedure TCustomBeeApp.OnRequest(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TCustomBeeApp.OnProgress;
  begin
    // not convert oem to param
    Write(#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8 +
      Format('%5d KB/s %3d%%', [Speed shr 10, Percentes]));
  end;

  procedure TCustomBeeApp.OnClearLine;
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
      SIGINT: App.Terminated  := True;
      SIGQUIT: App.Terminated := True;
      SIGKILL: App.Terminated := True;
      SIGSTOP: App.Terminated := True;
    end;
  end;
  {$ENDIF}

begin
  SetCtrlCHandler(@CtrlHandler);

  Params := TStringList.Create;
  for I := 1 to ParamCount do
    Params.Add(ParamStr(I));
  App := TCustomBeeApp.Create(Params);
  App.Execute;

  ExitCode := App.Code;

  App.Destroy;
  Params.Destroy;
end.
