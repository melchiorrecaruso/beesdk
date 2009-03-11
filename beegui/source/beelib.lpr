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
  TCoreStatus = (csReady, csExecuting, csWaitingOverwrite,
    csWaitingRename, csWaitingKey, csWaitingRequest, csTerminated);

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
    procedure Suspend;
    procedure Resume;
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

  procedure TCore.Suspend;
  begin
    FApp.Suspended := True;
  end;

  procedure TCore.Resume;
  begin
    FApp.Suspended := False;
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

function  CoreCreate(const ACommandLine: string): pointer;
begin
  Result := TCore.Create(ACommandLine);
end;

procedure CoreExecute(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Execute;
  end;
end;

procedure CoreDestroy(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Destroy;
  end;
end;

procedure CoreSuspend(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Suspend;
  end;
end;

procedure CoreResume(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Resume;
  end;
end;

procedure CoreTerminate(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Terminate;
  end;
end;

function  GetCoreStatus(ID: pointer): TCoreStatus;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).Status;
  end;
end;

function  GetCoreMessage(ID: pointer): string;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FMessage;
  end;
end;

function  GetCorePercentes(ID: pointer): integer;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FApp.Percentes;
  end;
end;

function  GetCoreSpeed(ID: pointer): integer;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FApp.Speed;
  end;
end;

function  GetCoreTotalSize(ID: pointer): int64;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FApp.TotalSize;
  end;
end;

function  GetCoreProcessedSize(ID: pointer): int64;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FApp.ProcessedSize;
  end;
end;

function  GetOverwriteFileInfo(ID: pointer): TFileInfoRec;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TFileInfoRec(TCore(ID).Data^);
  end;
end;

procedure SetOverwriteFileInfoRes(ID: pointer; Result: char);
begin
  if Assigned(TCore(ID)) then
  begin
    Char(TCore(ID).DataRes^) := Result;
    TCore(ID).Status := csExecuting;
  end;
end;

function  GetRenameFileInfo(ID: pointer): TFileInfoRec;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TFileInfoRec(TCore(ID).Data^);
  end;
end;

procedure SetRenameFileInfoRes(ID: pointer; Result: string);
begin
  if Assigned(TCore(ID)) then
  begin
    string(TCore(ID).DataRes^) := Result;
    TCore(ID).Status := csExecuting;
  end;
end;

function  GetPasswordFileInfo(ID: pointer): TFileInfoRec;
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
    TCore(ID).Status := csExecuting;
  end;
end;

function  GetRequestMessage(ID: pointer): string;
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
    TCore(ID).Status := csExecuting;
  end;
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
  CoreSuspend,
  CoreResume,
  CoreTerminate,

  GetCoreStatus,
  GetCoreMessage,
  GetCorePercentes,
  GetCoreSpeed,
  GetCoreTotalSize,
  GetCoreProcessedSize,

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

