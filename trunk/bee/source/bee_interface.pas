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
  Classes;

type
  // TFileInfoRec record ...

  TFileInfoRec = record
    FileName: string;
    FilePath: string;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
  end;

  // TFileFullInfoRec record ...

  TFileFullInfoRec = record
    FileName: string;
    FilePath: string;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
    FilePacked: cardinal;
    FileRatio: cardinal;
    FileComm: string;
    FileCrc: cardinal;
    FileMethod: string;
    FileVersion: string;
    FilePassword: string;
    FilePosition: cardinal;
  end;

  // TEvents procedure ...

  TOnCustomEvent    = procedure of object;
  TOnMessageEvent   = procedure(const aMessage: string) of object;
  TOnListEvent      = procedure(const aFileInfo: TFileFullInfoRec) of object;
  TOnOverWriteEvent = procedure(const aFileInfo: TFileInfoRec; var Result: char) of object;
  TOnRenameEvent    = procedure(const aFileInfo: TFileInfoRec; var Result: string) of object;
  TOnKeyEvent       = procedure(const aFileInfo: TFileInfoRec; var Result: string) of object;

  // TApp class ...

  TApp = class
  private
    FOnFatalError: TOnMessageEvent;
    FOnError:      TOnMessageEvent;
    FOnWarning:    TOnMessageEvent;
    FOnMessage:    TOnMessageEvent;
    FOnOverWrite:  TOnOverWriteEvent;
    FOnRename:     TOnRenameEvent;
    FOnList:       TOnListEvent;
    FOnKey:        TOnKeyEvent;
    FOnRequest:    TOnMessageEvent;
    FOnTick:       TOnCustomEvent;
    FOnClear:      TOnCustomEvent;
  protected
    FParams: TStringList;
    FxSuspendedTime: TDateTime;
    FxTime: TDateTime;
    FTotalSize: int64;
    FProcessedSize: int64;
    FTerminated: boolean;
    FSuspended: boolean;
    FExitCode: byte;
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
    procedure ProcessError     (const aMessage: string; aExitCode: byte);
    procedure ProcessWarning   (const aMessage: string; aExitCode: byte);
    procedure ProcessMessage   (const aMessage: string);
    function  ProcessOverwrite (const aFileInfo: TFileInfoRec; const Value: char): char;
    function  ProcessRename    (const aFileInfo: TFileInfoRec; const Value: string): string;
    procedure ProcessList      (const aFileInfo: TFileFullInfoRec);
    function  ProcessKey       (const aFileInfo: TFileInfoRec; const Value: string): string;
    procedure ProcessRequest   (const aMessage: string);
    procedure ProcessTick;
    procedure ProcessClear;

    procedure IncProcessedSize(Value: cardinal); overload;
    procedure IncProcessedSize; overload;
    procedure DecProcessedSize(Value: cardinal); overload;
    procedure DecProcessedSize; overload;
  public
    property OnFatalError: TOnMessageEvent   read FOnFatalError write FOnFatalError;
    property OnError:      TOnMessageEvent   read FOnError      write FOnError;
    property OnWarning:    TOnMessageEvent   read FOnWarning    write FOnWarning;
    property OnMessage:    TOnMessageEvent   read FOnMessage    write FOnMessage;
    property OnOverWrite:  TOnOverWriteEvent read FOnOverWrite  write FOnOverWrite;
    property OnRename:     TOnRenameEvent    read FOnRename     write FOnRename;
    property OnList:       TOnListEvent      read FOnList       write FOnList;
    property OnKey:        TOnKeyEvent       read FOnKey        write FOnKey;
    property OnRequest:    TOnMessageEvent   read FOnRequest    write FOnRequest;
    property OnTick:       TOnCustomEvent    read FOnTick       write FOnTick;
    property OnClear:      TOnCustomEvent    read FOnClear      write FOnClear;
  public
    property TotalSize: int64 read FTotalSize;
    property ProcessedSize: int64 read FProcessedSize;
    property Percentes: cardinal read GetPercentes;
    property Speed: cardinal read GetSpeed;
    property ElapsedTime: cardinal read GetElapsedTime;
    property RemainingTime: cardinal read GetRemainingTime;

    property Terminated: boolean read FTerminated write FTerminated;
    property Suspended: boolean read FSuspended write SetSuspended;
    property ExitCode: byte read FExitCode;
  end;

  // TCoreStatus ...

  TCoreStatus = (csReady, csExecuting, csWaitingOverwrite,
    csWaitingRename, csWaitingKey, csWaitingRequest, csTerminated);

implementation

uses
  SysUtils,
  DateUtils,
  {$IFDEF CONSOLEAPPLICATION} {$IFDEF MSWINDOWS}
  Bee_Common,
  {$ENDIF} {$ENDIF}
  Bee_Assembler;

// TApp class ...

constructor TApp.Create;
begin
  inherited Create;
  FParams := aParams;
  FExitCode := 0;
  FTotalSize := 0;
  FProcessedSize := 0;
  FSuspended := False;
  FTerminated := False;
end;

destructor TApp.Destroy;
begin
  FParams := nil;
  inherited Destroy;
end;

procedure TApp.Execute;
begin
  FxTime := Now;
end;

function TApp.GetSpeed: cardinal;
begin
  if not FSuspended then
    Result := MulDiv(FProcessedSize, 1000, MilliSecondsBetween(Now, FxTime))
  else
    Result := MulDiv(FProcessedSize, 1000, MilliSecondsBetween(0,   FxTime - FxSuspendedTime));
end;

function TApp.GetPercentes: cardinal;
begin
  Result := MulDiv(FProcessedSize, 100, FTotalSize);
end;

function TApp.GetElapsedTime: cardinal;
begin
  Result := SecondsBetween(Now, FxTime);
end;

function TApp.GetRemainingTime: cardinal;
begin
  Result := (FTotalSize - FProcessedSize) div GetSpeed;
end;

procedure TApp.SetSuspended(Value: boolean);
begin
  if FSuspended <> Value then
  begin
    if Value then
      FxSuspendedTime := Now
    else
      FxTime := FxTime + (Now - FxSuspendedTime);

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

function TApp.ProcessOverwrite(const aFileInfo: TFileInfoRec; const Value: char): char;
begin
  Result := Value;
  if Assigned(FOnOverWrite) then
  begin
    FOnOverWrite(aFileInfo, Result);
  end;
end;

function TApp.ProcessRename(const aFileInfo: TFileInfoRec; const Value: string): string;
begin
  Result := Value;
  if Assigned(FOnRename) then
  begin
    FOnRename(aFileInfo, Result);
  end;
end;

procedure TApp.ProcessList(const aFileInfo: TFileFullInfoRec);
begin
  if Assigned(FOnList) then
  begin
    FOnList(aFileInfo);
  end;
end;

function TApp.ProcessKey(const aFileInfo: TFileInfoRec; const Value: string): string;
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
