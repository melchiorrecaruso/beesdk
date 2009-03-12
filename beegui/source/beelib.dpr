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

    Bee library.

  Modifyed:
}

library BeeLib;

{$I compiler.inc}

uses
  Classes,
  SysUtils,
  // ---
  Bee_App,
  Bee_Interface;

type
  TCore = class(TThread)
  private
    FApp: TApp;
    FKey: string;
    FStatus: TCoreStatus;
    FParams: TStringList;
    FMessages: TStringList;
    FMessage: string;
    FDataRes: pointer;
    FData: pointer;
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
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Suspended(Value: boolean);
    procedure Terminate;
  public
    property Status: TCoreStatus read FStatus write FStatus;
    property DataRes: pointer read FDataRes;
    property Data: pointer read FData;
  end;

// -------------------------------------------------------------------------- //
//                                                                            //
//  Library TCore class implementation                                        //
//                                                                            //
// -------------------------------------------------------------------------- //

  constructor TCore.Create(const aCommandLine: string);
  begin
    inherited Create(True);

    FKey := '';
    FMessage := '';
    FStatus := csReady;
    FMessages := TStringList.Create;
    FParams := TStringList.Create;
    FParams.Text := aCommandLine;
    FDataRes := nil;
    FData := nil;

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

  destructor TCore.Destroy;
  begin
    FKey := '';
    FMessage := '';
    FParams.Destroy;
    FMessages.Destroy;
    inherited Destroy;
  end;

  procedure TCore.Execute;
  begin
    FStatus := csExecuting;
    begin
      FApp.Execute;
    end;
    FStatus := csTerminated;
  end;

  procedure TCore.Suspended(Value: boolean);
  begin
    FApp.Suspended := Value;
  end;

  procedure TCore.Terminate;
  begin
    FApp.Terminated:= True;
  end;

  procedure TCore.ProcessFatalError(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCore.ProcessError(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCore.ProcessWarning(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCore.ProcessMessage(const aMessage: string);
  begin
    FMessages.Add(aMessage);
    FMessage := aMessage;
  end;

  procedure TCore.ProcessOverwrite(const aFileInfo: TFileInfoRec; var Result: char);
  begin
    FData := @aFileInfo;
    FDataRes := @Result;
    FStatus := csWaitingOverwrite;
    while FStatus = csWaitingOverwrite do
    begin
      Sleep(250);
    end;
    FDataRes := nil;
    FData := nil;
  end;

  procedure TCore.ProcessRename(const aFileInfo: TFileInfoRec; var Result: string);
  begin
    FData := @aFileInfo;
    FDataRes := @Result;
    FStatus := csWaitingRename;
    while FStatus = csWaitingRename do
    begin
      Sleep(250);
    end;
    FDataRes := nil;
    FData := nil;
  end;

  procedure TCore.ProcessList(const aFileInfo: TFileFullInfoRec);
  begin

  end;

  procedure TCore.ProcessKey(const aFileInfo: TFileInfoRec; var Result: string);
  begin
    FData := @aFileInfo;
    FDataRes := @Result;
    FStatus := csWaitingKey;
    while FStatus = csWaitingKey do
    begin
      Sleep(250);
    end;
    FDataRes := nil;
    FData := nil;
  end;

  procedure TCore.ProcessRequest(const aMessage: string);
  begin
    FMessage := aMessage;
    FStatus := csWaitingRequest;
    while FStatus = csWaitingRequest do
    begin
      Sleep(250);
    end;
  end;

  procedure TCore.ProcessTick;
  begin

  end;

  procedure TCore.ProcessClear;
  begin

  end;

// -------------------------------------------------------------------------- //
//                                                                            //
//  Library core routines                                                     //
//                                                                            //
// -------------------------------------------------------------------------- //

function CoreCreate(const aCommandLine: string): pointer;
begin
  Result := TCore.Create(aCommandLine);
end;

function CoreExecute(ID: pointer): pointer;
begin
  Result := ID;
  if Assigned(TCore(ID)) then
  begin
        TCore(ID).Resume;
  end;
end;

function CoreDestroy(ID: pointer): pointer;
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Destroy;
    Result := nil;
  end else
    Result := ID;
end;

procedure CoreSuspended(ID: pointer; Value: boolean);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).FApp.Suspended := Value;
  end;
end;

procedure CoreTerminate(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Terminate;
    TCore(ID).Suspended(False);
  end;
end;

function GetCoreExitCode(ID: pointer): integer;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).FApp.ExitCode
  else
    Result := 0;
end;

function GetCoreStatus(ID: pointer): TCoreStatus;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).Status
  else
    Result := csTerminated;
end;

function GetCoreMessage(ID: pointer): string;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).FMessage
  else
    Result := '';
end;

function GetCoreElapsedTime(ID: pointer): cardinal;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).FApp.ElapsedTime
  else
    Result := 0;
end;

function GetCoreRemainingTime(ID: pointer): cardinal;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).FApp.RemainingTime
  else
    Result := 0;
end;

function GetCorePercentes(ID: pointer): cardinal;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).FApp.Percentes
  else
    Result := 0;
end;

function GetCoreSpeed(ID: pointer): cardinal;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).FApp.Speed
  else
    Result := 0;
end;

function GetCoreTotalSize(ID: pointer): int64;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).FApp.TotalSize
  else
    Result := 0;
end;

function GetCoreProcessedSize(ID: pointer): int64;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).FApp.ProcessedSize
  else
    Result := 0;
end;

procedure SetCorePriority(ID: pointer; aPriority: TThreadPriority);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Priority := aPriority;
  end;
end;

function GetCorePriority(ID: pointer): TThreadPriority;
begin
  if Assigned(TCore(ID)) then
    Result := TCore(ID).Priority
  else
    Result := tpNormal;
end;

function GetOverwriteFileInfo(ID: pointer): TFileInfoRec;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TFileInfoRec(TCore(ID).Data^)
  end;
end;

procedure SetOverwriteFileInfoRes(ID: pointer; Result: char);
begin
  if Assigned(TCore(ID)) then
  begin
    char(TCore(ID).DataRes^) := Result;
  end;
  TCore(ID).Status := csExecuting;
end;

function GetRenameFileInfo(ID: pointer): TFileInfoRec;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TFileInfoRec(TCore(ID).Data^)
  end;
end;

procedure SetRenameFileInfoRes(ID: pointer; Result: string);
begin
  if Assigned(TCore(ID)) then
  begin
    string(TCore(ID).DataRes^) := Result;
  end;
  TCore(ID).Status := csExecuting;
end;

function GetPasswordFileInfo(ID: pointer): TFileInfoRec;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TFileInfoRec(TCore(ID).Data^);
  end;
end;

procedure SetPasswordFileInfoRes(ID: pointer; Result: string);
begin
  if Assigned(TCore(ID)) then
  begin
    string(TCore(ID).DataRes^) := Result;
  end;
  TCore(ID).Status := csExecuting;
end;

function GetRequestMessage(ID: pointer): string;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FMessage;
  end;
end;

procedure SetRequestMessage(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
  end;
  TCore(ID).Status := csExecuting;
end;

// -------------------------------------------------------------------------- //
//                                                                            //
//  Library core routines exported                                            //
//                                                                            //
// -------------------------------------------------------------------------- //

exports
  CoreCreate,
  CoreExecute,
  CoreDestroy,
  CoreSuspended,
  CoreTerminate,

  GetCoreExitCode,
  GetCoreStatus,
  GetCoreMessage,
  GetCorePercentes,
  GetCoreSpeed,
  GetCoreTotalSize,
  GetCoreProcessedSize,
  GetCoreElapsedTime,
  GetCoreRemainingTime,

  GetCorePriority,
  SetCorePriority,

  GetOverwriteFileInfo,
  SetOverwriteFileInfoRes,

  GetRenameFileInfo,
  SetRenameFileInfoRes,

  GetPasswordFileInfo,
  SetPasswordFileInfoRes,

  GetRequestMessage,
  SetRequestMessage;

begin

end.

