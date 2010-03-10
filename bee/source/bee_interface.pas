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
  { TAppIO class }

  TAppIO = class
  private
    procedure SetTerminated(aValue: boolean);
    function SetCode(aCode: byte): byte;
  protected
    FTerminated: boolean;
    FCode: byte;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnError(const aMessage: string; aCode: byte); virtual;
    procedure OnRequest(const aMessage: string); virtual; abstract;
    procedure OnMessage(const aMessage: string); virtual; abstract;
    function  OnOverwrite(const aFileInfo: TFileInfo; const aValue: TOverwriteMode): TOverwriteMode; virtual; abstract;
    function  OnRename(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    function  OnPassword(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    procedure OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean); virtual; abstract;
    procedure OnProgress; virtual; abstract;
    procedure OnClearLine; virtual; abstract;
  published
    property Terminated: boolean read FTerminated write SetTerminated;
    property Code: byte read FCode;
  end;

  { TApp class }

  TApp = class(TAppIO)
  protected
    FSuspended: boolean;
    FParams: TStringList;
    FSuspendedTime: double;
    FStartTime: double;
    FTotalSize: int64;
    FSize: int64;
    function GetSpeed: longint;
    function GetBit4Byte: byte;
    function GetPercentes: longint;
    function GetTotalTime: longint;
    function GetTime: longint;
    procedure SetSuspended(aValue: boolean); overload;
    procedure SetPriority(aPriority: byte);
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; virtual;

    procedure DoError(const aMessage: string; aCode: byte);
    procedure DoRequest(const aMessage: string);
    procedure DoMessage(const aMessage: string);
    function  DoOverwrite(const aFileInfo: TFileInfo; const aValue: TOverwriteMode): TOverwriteMode;
    function  DoRename(const aFileInfo: TFileInfo; const aValue: string): string;
    function  DoPassword(const aFileInfo: TFileInfo; const aValue: string): string;
    procedure DoList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
    procedure DoProgress;
    procedure DoClearLine;

    procedure IncSize(const aValue: int64); overload;
    procedure IncSize; overload;
    procedure DecSize(const aValue: int64); overload;
    procedure DecSize; overload;

    property TotalTime: longint Read GetTotalTime;
    property TotalSize: int64 Read FTotalSize;
    property Time: longint Read GetTime;
    property Size: int64 Read FSize;
    property Percentes: longint Read GetPercentes;
    property Bit4Byte: byte Read GetBit4Byte;
    property Speed: longint Read GetSpeed;
    property Suspended: boolean read FSuspended write SetSuspended;
  end;

implementation

uses
  DateUtils,
  SysUtils;

{ TAppIO class }

constructor TAppIO.Create;
begin
  inherited Create;
  FTerminated := False;
  FCode := ccSuccesful;
end;

destructor TAppIO.Destroy;
begin
  inherited Destroy;
end;

procedure TAppIO.OnError(const aMessage: string; aCode: byte);
begin
  SetCode(aCode);
end;

procedure TAppIO.SetTerminated(aValue: boolean);
begin
  if not FTerminated then
  begin
    FTerminated := aValue;
  end;
end;

function TAppIO.SetCode(aCode: byte): byte;
begin
  if FCode < aCode then
  begin
    FCode := aCode;
    if FCode >= ccError then
      SetTerminated(True);
  end;
  Result := FCode;
end;

{ TApp class }

constructor TApp.Create(aParams: TStringList);
begin
  inherited Create;
  FSuspended := False;
  FParams := aParams;
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

function TApp.GetBit4Byte: byte;
begin
  Result := 0; { TODO :  DA IMPLEMENTARE}
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

procedure TApp.IncSize(const aValue: int64); inline;
begin
  Inc(FSize, aValue);
end;

procedure TApp.IncSize; inline;
begin
  Inc(FSize);
end;

procedure TApp.DecSize(const aValue: int64); inline;
begin
  Dec(FSize, aValue);
end;

procedure TApp.DecSize; inline;
begin
  Dec(FSize);
end;

procedure TApp.DoError(const aMessage: string; aCode: byte);
var
  X: double;
begin
  X := Now;
  OnError(aMessage, aCode);
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
