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
    FOnFatalError:  TMessageEvent;
    FOnError:       TMessageEvent;
    FOnWarning:     TMessageEvent;
    FOnMessage:     TMessageEvent;
    FOnOverWrite:   TRequestEvent;
    FOnPassword:    TRequestEvent;
    FOnRename:      TRequestEvent;
    FOnRequest:     TMessageEvent;
    FOnProgress:    TCustomEvent;
    FOnClear:       TCustomEvent;
    FOnList:        TListEvent;
  protected
    FParams:        TStringList;
    FSuspendedTime: double;
    FTotalTime:     double;
    FTotalSize:     int64;
    FSize:          int64;
    FSuspended:     boolean;
    FTerminated:    boolean;
    FCode:          byte;
  protected
    function GetSpeed: integer;
    function GetPercentes: integer;
    function GetTotalTime: integer;
    function GetTime: integer;

    procedure SetCode(aCode: integer);
    procedure SetSuspended(aValue: boolean);
    procedure SetPriority(aPriority: integer); overload;
  public
    constructor Create(aParams: TStringList);
    destructor  Destroy; override;
    procedure   Execute; virtual;
  public
    procedure ProcessFatalError(const aMessage: string; aCode: byte);
    procedure ProcessError     (const aMessage: string; aCode: byte);
    procedure ProcessWarning   (const aMessage: string; aCode: byte);
    procedure ProcessMessage   (const aMessage: string);
    procedure ProcessRequest   (const aMessage: string);

    procedure ProcessList      (const aFileInfo: TFileInfoExtra);
    function  ProcessOverwrite (const aFileInfo: TFileInfo; const Value: char): char;
    function  ProcessRename    (const aFileInfo: TFileInfo; const Value: string): string;
    function  ProcessKey       (const aFileInfo: TFileInfo; const Value: string): string;

    procedure ProcessProgress;
    procedure ProcessClear;
  public
    procedure IncSize(aValue: integer); overload;
    procedure IncSize;                  overload;
    procedure DecSize(aValue: integer); overload;
    procedure DecSize;                  overload;
  public
    property OnFatalError: TMessageEvent read FOnFatalError write FOnFatalError;
    property OnError:      TMessageEvent read FOnError      write FOnError;
    property OnWarning:    TMessageEvent read FOnWarning    write FOnWarning;
    property OnMessage:    TMessageEvent read FOnMessage    write FOnMessage;

    property OnOverWrite: TRequestEvent  read FOnOverWrite  write FOnOverWrite;
    property OnRename:    TRequestEvent  read FOnRename     write FOnRename;

    property OnPassword:  TRequestEvent  read FOnPassword   write FOnPassword;
    property OnRequest:   TMessageEvent  read FOnRequest    write FOnRequest;

    property OnProgress:  TCustomEvent   read FOnProgress   write FOnProgress;
    property OnClear:     TCustomEvent   read FOnClear      write FOnClear;
    property OnList:      TListEvent     read FOnList       write FOnList;
  public
    property TotalTime:  integer read GetTotalTime;
    property TotalSize:  int64   read FTotalSize;
    property Time:       integer read GetTime;
    property Size:       int64   read FSize;
    property Percentes:  integer read GetPercentes;
    property Speed:      integer read GetSpeed;
    property Terminated: boolean read FTerminated write FTerminated;
    property Suspended:  boolean read FSuspended  write SetSuspended;
    property Code:       byte    read FCode;
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
  FCode       := 0;
  FSize       := 0;
  FTotalSize  := 0;
  FTotalTime  := 0;
  FSuspended  := False;
  FTerminated := False;
  FParams     := aParams;
end;

destructor TApp.Destroy;
begin
  FParams := nil;
  inherited Destroy;
end;

procedure TApp.Execute;
begin
  FTotalTime := Now;
end;

function TApp.GetSpeed: integer;
var
  I: int64;
begin
  if not FSuspended then
    I := MilliSecondsBetween(Now, FTotalTime)
  else
    I := MilliSecondsBetween(FSuspendedTime - FTotalTime, 0);

  if I <> 0 then
    Result := MulDiv(FSize, 1000, I)
  else
    Result := 0;
end;

function TApp.GetPercentes: integer;
begin
  if FtotalSize <> 0 then
    Result := MulDiv(FSize, 100, FTotalSize)
  else
    Result := 0;
end;

function TApp.GetTotalTime: integer;
begin
  if not FSuspended then
    Result := SecondsBetween(Now, FTotalTime)
  else
    Result := SecondsBetween(FSuspendedTime - FTotalTime, 0);
end;

function TApp.GetTime: integer;
var
  I: cardinal;
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
      FTotalTime := FTotalTime + (Now - FSuspendedTime);

    FSuspended := aValue;
  end;
end;

procedure TApp.SetCode(aCode: integer);
begin
  if FCode < aCode then
  begin
    FCode := aCode;
  end;
end;

procedure TApp.SetPriority(aPriority: integer);
begin
  {$IFDEF CONSOLEAPPLICATION} {$IFDEF MSWINDOWS}
  Bee_Common.SetPriority(aPriority);
  {$ENDIF} {$ENDIF}
end;

procedure TApp.IncSize(aValue: integer);
begin
  Inc(FSize, aValue);
end;

procedure TApp.IncSize;
begin
  Inc(FSize);
end;

procedure TApp.DecSize(aValue: integer);
begin
  Dec(FSize, aValue);
end;

procedure TApp.DecSize;
begin
  Dec(FSize);
end;

procedure TApp.ProcessFatalError(const aMessage: string; aCode: byte);
begin
  SetCode(aCode);
  if Assigned(FOnFatalError) then
  begin
    FOnFatalError(aMessage);
  end;
end;

procedure TApp.ProcessError(const aMessage: string; aCode: byte);
begin
  SetCode(aCode);
  if Assigned(FOnError) then
  begin
    FOnError(aMessage);
  end;
end;

procedure TApp.ProcessWarning(const aMessage: string; aCode: byte);
begin
  SetCode(aCode);
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

function TApp.ProcessOverWrite(const aFileInfo: TFileInfo; const Value: char): char;
var
  S: string;
begin
  S := Value;
  if Assigned(FOnOverWrite) then
  begin
    FOnOverWrite(aFileInfo, S);
  end;

  if Length(S) = 1 then
    Result := S[1]
  else
    Result := Value;
end;

function TApp.ProcessRename(const aFileInfo: TFileInfo; const Value: string): string;
begin
  Result := Value;
  if Assigned(FOnRename) then
  begin
    FOnRename(aFileInfo, Result);
  end;
end;

procedure TApp.ProcessList(const aFileInfo: TFileInfoExtra);
begin
  if Assigned(FOnList) then
  begin
    FOnList(aFileInfo);
  end;
end;

function TApp.ProcessKey(const aFileInfo: TFileInfo; const Value: string): string;
begin
  Result := Value;
  if Assigned(FOnPassword) then
  begin
    FOnPassword(aFileInfo, Result);
  end;
end;

procedure TApp.ProcessRequest(const aMessage: string);
begin
  if Assigned(FOnRequest) then
  begin
    FOnRequest(aMessage);
  end;
end;

procedure TApp.ProcessProgress;
begin
  if Assigned(FOnProgress) then
  begin
    FOnProgress;
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
