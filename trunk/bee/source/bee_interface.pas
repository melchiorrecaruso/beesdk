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

    TApp, abstract class of Thread.

  Modifyed:

    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

    v0.7.9 build 0970 - 2009.03.10 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Types;

type

  // TApp class ...

  TApp = class
  private
    FOnFatalError: TOnMessageEvent;
    FOnError:   TOnMessageEvent;
    FOnWarning: TOnMessageEvent;
    FOnMessage: TOnMessageEvent;

    FOnOverWrite: TOnOverWriteEvent;
    FOnRename:  TOnRenameEvent;
    FOnList:    TOnListEvent;
    FOnKey:     TOnKeyEvent;
    FOnRequest: TOnMessageEvent;
    FOnTick:    TOnCustomEvent;
    FOnClear:   TOnCustomEvent;
  protected
    FParams:     TStringList;
    FSuspendedTime: double;
    FStartTime:  double;
    FTotalSize:  int64;
    FProcessedSize: int64;
    FTerminated: boolean;
    FSuspended:  boolean;
    FExitCode:   byte;
  protected
    function GetSpeed: cardinal;
    function GetPercentes: cardinal;
    function GetElapsedTime: cardinal;
    function GetRemainingTime: cardinal;
    procedure SetSuspended(Value: boolean);
    procedure SetExitCode(aExitCode: integer);
    procedure SetPriority(aPriority: integer); overload;
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; virtual;

    procedure ProcessFatalError(const aMessage: string; aExitCode: byte);
    procedure ProcessError(const aMessage: string; aExitCode: byte);
    procedure ProcessWarning(const aMessage: string; aExitCode: byte);
    procedure ProcessMessage(const aMessage: string);
    function  ProcessOverwrite(const aFileInfo: TFileInfoA; const Value: char): char;
    function  ProcessRename(const aFileInfo: TFileInfoA; const Value: string): string;
    procedure ProcessList(const aFileInfo: TFileInfoB);
    function  ProcessKey(const aFileInfo: TFileInfoA; const Value: string): string;
    procedure ProcessRequest(const aMessage: string);
    procedure ProcessTick;
    procedure ProcessClear;

    procedure IncProcessedSize(Value: cardinal); overload;
    procedure IncProcessedSize; overload;
    procedure DecProcessedSize(Value: cardinal); overload;
    procedure DecProcessedSize; overload;
  public
    property OnFatalError: TOnMessageEvent Read FOnFatalError Write FOnFatalError;
    property OnError: TOnMessageEvent Read FOnError Write FOnError;
    property OnWarning: TOnMessageEvent Read FOnWarning Write FOnWarning;
    property OnMessage: TOnMessageEvent Read FOnMessage Write FOnMessage;

    property OnOverWrite: TOnOverWriteEvent Read FOnOverWrite Write FOnOverWrite;
    property OnRename: TOnRenameEvent Read FOnRename Write FOnRename;
    property OnList: TOnListEvent Read FOnList Write FOnList;
    property OnKey: TOnKeyEvent Read FOnKey Write FOnKey;
    property OnRequest: TOnMessageEvent Read FOnRequest Write FOnRequest;
    property OnTick: TOnCustomEvent Read FOnTick Write FOnTick;
    property OnClear: TOnCustomEvent Read FOnClear Write FOnClear;
  public
    property TotalSize: int64 Read FTotalSize;
    property ProcessedSize: int64 Read FProcessedSize;
    property Percentes: cardinal Read GetPercentes;
    property Speed: cardinal Read GetSpeed;
    property ElapsedTime: cardinal Read GetElapsedTime;
    property RemainingTime: cardinal Read GetRemainingTime;

    property Terminated: boolean Read FTerminated Write FTerminated;
    property Suspended: boolean Read FSuspended Write SetSuspended;
    property ExitCode: byte Read FExitCode;
  end;

implementation

uses
  SysUtils,
  DateUtils,
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS}
  Bee_Common,
  {$ENDIF}
  {$ENDIF}
  Bee_Assembler;

// TApp class ...

constructor TApp.Create(aParams: TStringList);
begin
  inherited Create;
  FParams     := aParams;
  FExitCode   := 0;
  FTotalSize  := 0;
  FProcessedSize := 0;
  FSuspended  := False;
  FTerminated := False;
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

function TApp.GetSpeed: cardinal;
var
  I: int64;
begin
  if not FSuspended then
    I := MilliSecondsBetween(Now, FStartTime)
  else
    I := MilliSecondsBetween(FSuspendedTime - FStartTime, 0);

  if I <> 0 then
    Result := MulDiv(FProcessedSize, 1000, I)
  else
    Result := 0;
end;

function TApp.GetPercentes: cardinal;
begin
  Result := MulDiv(FProcessedSize, 100, FTotalSize);
end;

function TApp.GetElapsedTime: cardinal;
begin
  if not FSuspended then
    Result := SecondsBetween(Now, FStartTime)
  else
    Result := SecondsBetween(FSuspendedTime - FStartTime, 0);
end;

function TApp.GetRemainingTime: cardinal;
var
  I: cardinal;
begin
  I := GetSpeed;
  if I <> 0 then
    Result := (FTotalSize - FProcessedSize) div I
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
      FStartTime     := FStartTime + (Now - FSuspendedTime);

    FSuspended := Value;
  end;
end;

procedure TApp.SetExitCode(aExitCode: integer);
begin
  if FExitCode < aExitCode then
  begin
    FExitCode := aExitCode;
  end;
end;

procedure TApp.SetPriority(aPriority: integer);
begin
  {$IFDEF CONSOLEAPPLICATION} {$IFDEF MSWINDOWS}
  Bee_Common.SetPriority(aPriority);
  {$ENDIF} {$ENDIF}
end;

procedure TApp.IncProcessedSize(Value: cardinal);
begin
  Inc(FProcessedSize, Value);
end;

procedure TApp.IncProcessedSize;
begin
  Inc(FProcessedSize);
end;

procedure TApp.DecProcessedSize(Value: cardinal);
begin
  Dec(FProcessedSize, Value);
end;

procedure TApp.DecProcessedSize;
begin
  Dec(FProcessedSize);
end;

procedure TApp.ProcessFatalError(const aMessage: string; aExitCode: byte);
begin
  SetExitCode(aExitCode);
  if Assigned(FOnFatalError) then
  begin
    FOnFatalError(aMessage);
  end;
end;

procedure TApp.ProcessError(const aMessage: string; aExitCode: byte);
begin
  SetExitCode(aExitCode);
  if Assigned(FOnError) then
  begin
    FOnError(aMessage);
  end;
end;

procedure TApp.ProcessWarning(const aMessage: string; aExitCode: byte);
begin
  SetExitCode(aExitCode);
  if Assigned(FOnWarning) then
  begin
    FOnWarning(aMessage);
  end;
end;

procedure TApp.ProcessMessage(const aMessage: string);
begin
  if Assigned(FOnMessage) then
  begin
    FOnMessage(aMessage);
  end;
end;

function TApp.ProcessOverwrite(const aFileInfo: TFileInfoA; const Value: char): char;
begin
  Result := Value;
  if Assigned(FOnOverWrite) then
  begin
    FOnOverWrite(aFileInfo, Result);
  end;
end;

function TApp.ProcessRename(const aFileInfo: TFileInfoA; const Value: string): string;
begin
  Result := Value;
  if Assigned(FOnRename) then
  begin
    FOnRename(aFileInfo, Result);
  end;
end;

procedure TApp.ProcessList(const aFileInfo: TFileInfoB);
begin
  if Assigned(FOnList) then
  begin
    FOnList(aFileInfo);
  end;
end;

function TApp.ProcessKey(const aFileInfo: TFileInfoA; const Value: string): string;
begin
  Result := Value;
  if Assigned(FOnKey) then
  begin
    FOnKey(aFileInfo, Result);
  end;
end;

procedure TApp.ProcessRequest(const aMessage: string);
begin
  if Assigned(FOnRequest) then
  begin
    FOnRequest(aMessage);
  end;
end;

procedure TApp.ProcessTick;
begin
  if Assigned(FOnTick) then
  begin
    FOnTick;
  end;
end;

procedure TApp.ProcessClear;
begin
  if Assigned(FOnClear) then
  begin
    FOnClear;
  end;
end;

end.
