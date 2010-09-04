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

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Types,
  Bee_Common,
  Bee_Consts,
  Bee_Headers;

type

  { TBenchmark class }

  TBenchmark = class
  protected
    FStartTime: double;
    FSuspendedTime: double;
    FSuspended: boolean;
    FSize: int64;
    FProcessedSize: int64;
    function GetElapsedTime: longint;
    function GetRemainingTime: longint;
    procedure SetSize(const Value: int64);
    procedure SetProcessedSize(const Value: int64);
    procedure SetSuspended(Value: boolean);
    function GetPercentage: longint;
    function GetSpeed: longint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Step;
    procedure Execute;
    property Speed: longint read GetSpeed;
    property Percentage: longint read GetPercentage;
    property ElapsedTime: longint read GetElapsedTime;
    property RemainingTime: longint read GetRemainingTime;
    property ProcessedSize: int64 read FProcessedSize write SetProcessedSize;
    property Size: int64 read FSize write SetSize;
    property Suspended: boolean read FSuspended write SetSuspended;
  end;

  { TApp class }

  TApp = class(TBenchmark)
  private
    FCode: byte;
    FTerminated: boolean;
  protected
    FParams: TStringList;
    procedure SetPriority(Priority: byte);
    procedure SetTerminated(Value: boolean);
    procedure SetCode(Code: byte);
  public
    procedure DoMessage(const aMessage: string); overload;
    procedure DoMessage(const aMessage: string; aCode: byte); overload;
    procedure DoRequest(const aMessage: string);
    function  DoRename(const aItem: THeaderRec; const aValue: string): string;
    function  DoPassword(const aItem: THeaderRec; const aValue: string): string;
    procedure DoList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
    procedure DoStep;
    {$IFDEF CONSOLEAPPLICATION}
    procedure DoClearLine;
    {$ENDIF}
  public
    constructor Create(Params: TStringList);
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure Terminate;
    property Code: byte read FCode write SetCode;
    property Terminated: boolean read FTerminated;
  end;

//procedure OnMessage(const aMessage: string); virtual; abstract;
//procedure OnRequest(const aMessage: string); virtual; abstract;
//function  OnRename(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
//function  OnPassword(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
//procedure OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean); virtual; abstract;
//procedure OnProgress; virtual; abstract;
//{$IFDEF CONSOLEAPPLICATION}
//procedure OnClearLine; virtual; abstract;
//{$ENDIF}

implementation

uses
  DateUtils,
  SysUtils;

{ TBenchmark class }

constructor TBenchmark.Create;
begin
  inherited Create;
  FSuspended := False;
  FStartTime     := 0;
  FSuspendedTime := 0;
  FSize          := 0;
  FProcessedSize := 0;
end;

destructor TBenchmark.Destroy;
begin
  // nothing to do
end;

procedure TBenchmark.SetSize(const Value: int64);
begin
  FSize := Value;
end;

procedure TBenchmark.SetProcessedSize(const Value: int64);
begin
  FProcessedSize := Value;
end;

procedure TBenchmark.Execute;
begin
  FStartTime := Now;
end;

procedure TBenchmark.Step;
begin
  Inc(FProcessedSize);
end;

function TBenchmark.GetSpeed: longint;
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

function TBenchmark.GetPercentage: longint;
begin
  if FSize <> 0 then
    Result := Round((FProcessedSize / FSize) * 100)
  else
    Result := 0;
end;

function TBenchmark.GetElapsedTime: longint;
begin
  if not FSuspended then
    Result := SecondsBetween(Now, FStartTime)
  else
    Result := SecondsBetween(FSuspendedTime - FStartTime, 0);
end;

function TBenchmark.GetRemainingTime: longint;
var
  I: longint;
begin
  I := GetSpeed;
  if I <> 0 then
    Result := (FSize - FProcessedSize) div I
  else
    Result := 0;
end;

procedure TBenchmark.SetSuspended(Value: boolean);
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

{ TApp class }

constructor TApp.Create(Params: TStringList);
begin
  inherited Create;
  FParams := Params;
  FCode := ccSuccesful;
  FTerminated := False;
end;

destructor TApp.Destroy;
begin
  FParams := nil;
  inherited Destroy;
end;

procedure TApp.Execute;
begin
  inherited Execute;
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
    { TODO :  DA IMPLEMENTARE}
  {$ENDIF}
end;

procedure TApp.DoMessage(const aMessage: string; aCode: byte);
var
  X: double;
begin
  SetCode(aCode);
  X := Now;
  // OnMessage(aMessage);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoMessage(const aMessage: string);
var
  X: double;
begin
  X := Now;
  // OnMessage(aMessage);
  FStartTime := FStartTime + (Now - X);
end;

function TApp.DoRename(const aItem: THeaderRec; const aValue: string): string;
var
  X: double;
begin
  X := Now;
  // Result := OnRename(aFileInfo, aValue);
  FStartTime := FStartTime + (Now - X);
end;

function TApp.DoPassword(const aItem: THeaderRec; const aValue: string): string;
var
  X: double;
begin
  X := Now;
  // Result := OnPassword(aFileInfo, aValue);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoRequest(const aMessage: string);
var
  X: double;
begin
  X := Now;
  // OnRequest(aMessage);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
var
  X: double;
begin
  X := Now;
  // OnList(aFileInfo, aVerbose);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoStep;
var
  X: double;
begin
  X := Now;
  // OnProgress;
  FStartTime := FStartTime + (Now - X);
end;

{$IFDEF CONSOLEAPPLICATION}
procedure TApp.DoClearLine;
var
  X: double;
begin
  X := Now;
  // OnClearLine;
  FStartTime := FStartTime + (Now - X);
end;
{$ENDIF}

end.
