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

    v0.8.0 build 1112 - 2010.03.10 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Types,
  Bee_Consts,
  Bee_Common;

type
  { TApp class }

  TApp = class
  private
    FCode: byte;
    FTerminated: boolean;
  protected
    FParams: TStringList;
    FStartTime: double;
    FSuspendedTime: double;
    FTotalSize: int64;
    FSize: int64;
    FSuspended: boolean;
    function GetSpeed: longint;
    function GetPercentes: longint;
    function GetTotalTime: longint;
    function GetTime: longint;
    procedure SetPriority(aPriority: byte);
    procedure SetSuspended(aValue: boolean); overload;
    procedure SetTerminated(aValue: boolean);
    procedure SetCode(aCode: byte);
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure Terminate;

    procedure IncSize; overload;
    procedure DecSize; overload;
    procedure IncSize(const aValue: int64); overload;
    procedure DecSize(const aValue: int64); overload;

    procedure DoMessage(const aMessage: string; aCode: byte); overload;
    procedure OnMessage(const aMessage: string; aCode: byte); virtual; overload;
    procedure DoRequest(const aMessage: string);
    procedure OnRequest(const aMessage: string); virtual; abstract;
    procedure DoMessage(const aMessage: string); overload;
    procedure OnMessage(const aMessage: string); virtual; abstract; overload;
    function DoOverwrite(const aFileInfo: TFileInfo; const aValue: TOverwriteMode): TOverwriteMode;
    function OnOverwrite(const aFileInfo: TFileInfo; const aValue: TOverwriteMode): TOverwriteMode; virtual; abstract;
    function DoRename(const aFileInfo: TFileInfo; const aValue: string): string;
    function OnRename(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    function DoPassword(const aFileInfo: TFileInfo; const aValue: string): string;
    function OnPassword(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    procedure DoList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
    procedure OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean); virtual; abstract;
    procedure DoProgress;
    procedure OnProgress; virtual; abstract;
    procedure DoClearLine;
    procedure OnClearLine; virtual; abstract;

    property Time: longint Read GetTime;
    property TotalTime: longint Read GetTotalTime;
    property Size: int64 Read FSize;
    property TotalSize: int64 Read FTotalSize;
    property Percentes: longint Read GetPercentes;
    property Speed: longint Read GetSpeed;
    property Suspended: boolean read FSuspended write SetSuspended;
    property Terminated: boolean read FTerminated;
    property Code: byte read FCode write SetCode;
  end;


implementation

uses
  SysUtils,
  DateUtils;

{ TApp class }

constructor TApp.Create(aParams: TStringList);
begin
  inherited Create;
  FParams := aParams;
  FCode := ccSuccesful;
  FTerminated := False;
  FSuspended := False;
  FSuspendedTime := 0;
  FStartTime := 0;
  FTotalSize := 0;
  FSize := 0;
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

procedure TApp.Terminate;
begin
  SetCode(ccUserAbort);
end;

procedure TApp.IncSize(const aValue: int64); {$IFDEF FPC} inline; {$ENDIF}
begin
  Inc(FSize, aValue);
end;

procedure TApp.IncSize; {$IFDEF FPC} inline; {$ENDIF}
begin
  Inc(FSize);
end;

procedure TApp.DecSize(const aValue: int64); {$IFDEF FPC} inline; {$ENDIF}
begin
  Dec(FSize, aValue);
end;

procedure TApp.DecSize; {$IFDEF FPC} inline; {$ENDIF}
begin
  Dec(FSize);
end;

function TApp.GetSpeed: longint;
var
  I: int64;
begin
  if not FSuspended then
    I := MilliSecondsBetween(Now, FStartTime)
  else
    I := MilliSecondsBetween(FSuspendedTime - FStartTime, 0);

  if I <> 0 then
    Result := Round((FSize / I) * 1000)
  else
    Result := 0;
end;

function TApp.GetPercentes: longint;
begin
  if FTotalSize <> 0 then
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
  if I <> 0 then
    Result := (FTotalSize - FSize) div I
  else
    Result := 0;
end;

procedure TApp.SetTerminated(aValue: boolean);
begin
  if not FTerminated then
  begin
    FTerminated := aValue;
  end;
end;

procedure TApp.SetCode(aCode: byte);
begin
  if FCode < aCode then
  begin
    FCode := aCode;
    if FCode >= ccError then
      SetTerminated(True);
  end;
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

procedure TApp.SetPriority(aPriority: byte);
begin
  {$IFDEF CONSOLEAPPLICATION}
    {$IFDEF MSWINDOWS}
    Bee_Common.SetPriority(aPriority);
    {$ENDIF}
  {$ELSE}
    { TODO :  DA IMPLEMENTARE}
  {$ENDIF}
end;

procedure TApp.DoMessage(const aMessage: string; aCode: byte); {$IFDEF FPC} inline; {$ENDIF}
var
  X: double;
begin
  X := Now;
  OnMessage(aMessage, aCode);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.OnMessage(const aMessage: string; aCode: byte); {$IFDEF FPC} inline; {$ENDIF}
begin
  SetCode(aCode);
end;

procedure TApp.DoMessage(const aMessage: string);
var
  X: double;
begin
  X := Now;
  OnMessage(aMessage);
  FStartTime := FStartTime + (Now - X);
end;

function TApp.DoOverWrite(const aFileInfo: TFileInfo; const aValue: TOverwriteMode): TOverwriteMode;
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
  OnRename(aFileInfo, aValue);
  FStartTime := FStartTime + (Now - X);
end;

function TApp.DoPassword(const aFileInfo: TFileInfo; const aValue: string): string;
var
  X: double;
begin
  X := Now;
  OnPassword(aFileInfo, aValue);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoRequest(const aMessage: string);
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

procedure TApp.DoProgress; {$IFDEF FPC} inline; {$ENDIF}
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
