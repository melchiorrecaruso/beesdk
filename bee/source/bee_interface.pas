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

    TApp class

  Modifyed:

    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

    v0.8.0 build 1075 - 2009.11.15 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Types;

type
  // TAppIO class ...

  TAppIO = class
  public
    procedure OnFatalError(const aMessage: string); virtual; abstract;
    procedure OnError(const aMessage: string); virtual; abstract;
    procedure OnWarning(const aMessage: string); virtual; abstract;
    procedure OnRequest(const aMessage: string); virtual; abstract;
    procedure OnMessage(const aMessage: string); virtual; abstract;
    function OnOverwrite(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    function OnRename(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    function OnPassword(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    procedure OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean); virtual; abstract;
    procedure OnProgress; virtual; abstract;
    procedure OnClearLine; virtual; abstract;
  end;

  // TApp class ...

  TApp = class(TAppIO)
  protected
    FParams: TStringList;
    FSuspendedTime: double;
    FStartTime: double;
    FTotalSize: int64;
    FSize: int64;
    FSuspended: boolean;
    FTerminated: boolean;
    FCode: byte;
  protected
    function GetSpeed: longint;
    function GetPercentes: longint;
    function GetTotalTime: longint;
    function GetTime: longint;
    procedure SetSuspended(aValue: boolean);
    procedure SetPriority(aPriority: byte);
    procedure SetCode(aCode: byte);
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; virtual;
  public
    procedure DoFatalError(const aMessage: string; aCode: byte);
    procedure DoError(const aMessage: string; aCode: byte);
    procedure DoWarning(const aMessage: string; aCode: byte);
    procedure DoRequest(const aMessage: string);
    procedure DoMessage(const aMessage: string);
    function  DoOverwrite(const aFileInfo: TFileInfo; const aValue: string): string;
    function  DoRename(const aFileInfo: TFileInfo; const aValue: string): string;
    function  DoPassword(const aFileInfo: TFileInfo; const aValue: string): string;
    procedure DoList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
    procedure DoProgress;
    procedure DoClearLine;
  public
    procedure IncSize(const aValue: int64); overload;
    procedure IncSize; overload;
    procedure DecSize(const aValue: int64); overload;
    procedure DecSize; overload;
  public
    property TotalTime: longint read GetTotalTime;
    property TotalSize: int64 read FTotalSize;
    property Time: longint read GetTime;
    property Size: int64 read FSize;
    property Percentes: longint read GetPercentes;
    property Speed: longint read GetSpeed;
    property Terminated: boolean read FTerminated write FTerminated;
    property Suspended: boolean read FSuspended  write SetSuspended;
    property Code: byte read FCode;
  end;

implementation

uses
  {$IFDEF CONSOLEAPPLICATION} {$IFDEF MSWINDOWS}
  Bee_Common,
  {$ENDIF} {$ENDIF}
  DateUtils,
  SysUtils;

  // TApp class ...

  constructor TApp.Create(aParams: TStringList);
  begin
    inherited Create;
    FParams := aParams;
    FSuspendedTime := 0;
    FStartTime := 0;
    FTotalSize := 0;
    FSize := 0;
    FSuspended := False;
    FTerminated := False;
    FCode := 0;
  end;

  destructor TApp.Destroy;
  begin
    FParams := nil;
    inherited Destroy;
  end;

  procedure TApp.Execute;
  begin
    FStartTime := Now;
  end;

  function TApp.GetSpeed: longint;
  var
    I: int64;
  begin
    if not FSuspended then
      I := MilliSecondsBetween(Now, FStartTime)
    else
      I := MilliSecondsBetween(FSuspendedTime - FStartTime, 0);

    if I > 0 then
      Result := Round((FSize / I) * 1000)
    else
      Result := 0;
  end;

  function TApp.GetPercentes: longint;
  begin
    if FTotalSize > 0 then
      Result := Round((FSize / FTotalSize) * 100)
    else
      Result := 0;
  end;

  function TApp.GetTotalTime: longint;
  begin
    if not FSuspended then
      Result := SecondsBetween(Now, FStartTime)
    else
      Result := SecondsBetween(FSuspendedTime - FStartTime, 0);
  end;

  function TApp.GetTime: longint;
  var
    I: longint;
  begin
    I := GetSpeed;
    if I > 0 then
      Result := (FTotalSize - FSize) div I
    else
      Result := 0;
  end;

  procedure TApp.SetSuspended(aValue: boolean);
  begin
    if FSuspended <> aValue then
    begin
      if aValue then
        FSuspendedTime := Now
      else
        FStartTime := FStartTime + (Now - FSuspendedTime);

      FSuspended := aValue;
    end;
  end;

  procedure TApp.SetCode(aCode: byte);
  begin
    if FCode < aCode then
    begin
      FCode := aCode;
    end;
  end;

  procedure TApp.SetPriority(aPriority: byte);
  begin
    {$IFDEF CONSOLEAPPLICATION} {$IFDEF MSWINDOWS}
    Bee_Common.SetPriority(aPriority);
    {$ENDIF} {$ENDIF}
  end;

  procedure TApp.IncSize(const aValue: int64);
  begin
    Inc(FSize, aValue);
  end;

  procedure TApp.IncSize;
  begin
    Inc(FSize);
  end;

  procedure TApp.DecSize(const aValue: int64);
  begin
    Dec(FSize, aValue);
  end;

  procedure TApp.DecSize;
  begin
    Dec(FSize);
  end;

  procedure TApp.DoFatalError(const aMessage: string; aCode: byte);
  var
    X: double;
  begin
    SetCode(aCode);
    X := Now;
    OnFatalError(aMessage);
    FStartTime := FStartTime + (Now - X);
  end;

  procedure TApp.DoError(const aMessage: string; aCode: byte);
  var
    X: double;
  begin
    SetCode(aCode);
    X := Now;
    OnError(aMessage);
    FStartTime := FStartTime + (Now - X);
  end;

  procedure TApp.DoWarning(const aMessage: string; aCode: byte);
  var
    X: double;
  begin
    SetCode(aCode);
    X := Now;
    OnWarning(aMessage);
    FStartTime := FStartTime + (Now - X);
  end;

  procedure TApp.DoMessage(const aMessage: string);
  var
    X: double;
  begin
    X := Now;
    OnMessage(aMessage);
    FStartTime := FStartTime + (Now - X);
  end;

  function TApp.DoOverWrite(const aFileInfo: TFileInfo; const aValue: string): string;
  var
    X: double;
  begin
    X := Now;
    Result := OnOverWrite(aFileInfo, aValue);
    FStartTime := FStartTime + (Now - X);
  end;

  function TApp.DoRename(const aFileInfo: TFileInfo; const aValue: string): string;
  var
    X: double;
  begin
    X := Now;
    Result := OnRename(aFileInfo, aValue);
    FStartTime := FStartTime + (Now - X);
  end;

  function TApp.DoPassword(const aFileInfo: TFileInfo; const aValue: string): string;
  var
    X: double;
  begin
    X := Now;
    Result := OnPassword(aFileInfo, aValue);
    FStartTime := FStartTime + (Now - X);
  end;

  procedure TApp.DORequest(const aMessage: string);
  var
    X: double;
  begin
    X := Now;
    OnRequest(aMessage);
    FStartTime := FStartTime + (Now - X);
  end;

  procedure TApp.DoList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
  var
    X: double;
  begin
    X := Now;
    OnList(aFileInfo, aVerbose);
    FStartTime := FStartTime + (Now - X);
  end;

  procedure TApp.DoProgress;
  var
    X: double;
  begin
    X := Now;
    OnProgress;
    FStartTime := FStartTime + (Now - X);
  end;

  procedure TApp.DoClearLine;
  var
    X: double;
  begin
    X := Now;
    OnClearLine;
    FStartTime := FStartTime + (Now - X);
  end;

end.
