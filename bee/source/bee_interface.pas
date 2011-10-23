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

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Types,
  Bee_Consts,
  Bee_Common,
  Bee_Headers,
  BeeLib_Types;

type
  { TApp class }

  TApp = class
  protected
    FStartTime: double;
    FSuspendedTime: double;
    FSuspended: boolean;
    FTerminated: boolean;
    FCode: byte;
    FSize: int64;
    FProcessedSize: int64;
    function GetElapsedTime: longint;
    function GetRemainingTime: longint;
    procedure SetSuspended(Value: boolean);
    function GetPercentage: longint;
    function GetSpeed: longint;
    procedure SetPriority(Priority: byte);
    procedure SetCode(Code: byte);
    procedure SetTerminated(Value: boolean);

    procedure DoMessage(const aMessage: string); overload;
    procedure DoMessage(const aMessage: string; aCode: byte); overload;
    procedure DoRequest(const aMessage: string);
    function DoRename(const aItem: THeader; const aValue: string): string;
    function DoOverWrite(const aItem: THeader; const aValue: string): string;
    procedure DoList(const aItem: THeader);
    function DoUserAbort: boolean;
    {$IFDEF CONSOLEAPPLICATION}
    procedure DoClear;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure Terminate;

    procedure OnMessage(const aMessage: string); virtual; abstract;
    procedure OnRequest(const aMessage: string); virtual; abstract;
    function OnRename(const aItem: THeader; const aValue: string): string; virtual; abstract;
    function OnOverWrite(const aItem: THeader; const aValue: string): string; virtual; abstract;
    procedure OnList(const aItem: THeader); virtual; abstract;
    procedure OnProgress; virtual; abstract;
    {$IFDEF CONSOLEAPPLICATION}
    procedure OnClear; virtual; abstract;
    {$ENDIF}
    property Speed: longint read GetSpeed;
    property Percentage: longint read GetPercentage;
    property ElapsedTime: longint read GetElapsedTime;
    property RemainingTime: longint read GetRemainingTime;
    property ProcessedSize: int64 read FProcessedSize;
    property Size: int64 read FSize;
    property Suspended: boolean read FSuspended write SetSuspended;
    property Terminated: boolean read FTerminated;
    property Code: byte read FCode write SetCode;
  end;

  function DoTick(Handle: pointer): boolean; {$IFDEF USECDLL} cdecl; {$ENDIF}

implementation

uses
  DateUtils,
  SysUtils;

function DoTick(Handle: pointer): boolean;
begin
  Result := TApp(Handle).DoUserAbort;
end;



{ TApp class }

constructor TApp.Create;
begin
  inherited Create;
  FStartTime     := 0;
  FSuspendedTime := 0;
  FSuspended     := False;
  FTerminated    := False;
  FCode          := ccSuccesful;
  FSize          := 0;
  FProcessedSize := 0;
end;

destructor TApp.Destroy;
begin
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

  if I <> 0 then
    Result := Round((FProcessedSize / I) * 1000)
  else
    Result := 0;
end;

function TApp.GetPercentage: longint;
begin
  if FSize <> 0 then
    Result := Round((FProcessedSize / FSize) * 100)
  else
    Result := 0;
end;

function TApp.GetElapsedTime: longint;
begin
  if not FSuspended then
    Result := SecondsBetween(Now, FStartTime)
  else
    Result := SecondsBetween(FSuspendedTime - FStartTime, 0);
end;

function TApp.GetRemainingTime: longint;
var
  I: longint;
begin
  I := GetSpeed;
  if I <> 0 then
    Result := (FSize - FProcessedSize) div I
  else
    Result := 0;
end;

procedure TApp.SetSuspended(Value: boolean);
begin
  if FSuspended <> Value then
  begin
    if Value then
      FSuspendedTime := Now
    else
      FStartTime := FStartTime + (Now - FSuspendedTime);

    FSuspended := Value;
  end;
end;

procedure TApp.Terminate;
begin
  SetCode(ccUserAbort);
end;

procedure TApp.SetTerminated(Value: boolean);
begin
  if not FTerminated then
  begin
    FTerminated := Value;
  end;
end;

procedure TApp.SetCode(Code: byte);
begin
  if FCode < Code then
  begin
    FCode := Code;
    if FCode >= ccError then
      SetTerminated(True);
  end;
end;

procedure TApp.SetPriority(Priority: byte);
begin
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS}
    Bee_Common.SetPriority(Priority);
  {$ENDIF}
  {$ELSE}
    { TODO : }
  {$ENDIF}
end;

procedure TApp.DoMessage(const aMessage: string; aCode: byte);
begin
  SetCode(aCode);
  OnMessage(aMessage);
end;

procedure TApp.DoMessage(const aMessage: string);
begin
  OnMessage(aMessage);
end;

function TApp.DoRename(const aItem: THeader; const aValue: string): string;
var
  X: double;
begin
  X := Now;
  Result := OnRename(aItem, aValue);
  FStartTime := FStartTime + (Now - X);
end;

function TApp.DoOverWrite(const aItem: THeader; const aValue: string): string;
var
  X: double;
begin
  X := Now;
  Result := OnOverWrite(aItem, aValue);
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

procedure TApp.DoList(const aItem: THeader);
begin
  OnList(aItem);
end;

function TApp.DoUserAbort: boolean;
var
  X: double;
begin
  Inc(FProcessedSize, DefaultTickStepSize);
  begin
    X := Now;
    OnProgress;
    while FSuspended do Sleep(250);
    FStartTime := FStartTime + (Now - X);
  end;
  Result := Code > ccWarning;
end;

{$IFDEF CONSOLEAPPLICATION}
procedure TApp.DoClear;
begin
  OnClear;
end;
{$ENDIF}

end.
