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
  SysUtils,
  Classes,
  // ---
  Bee_App,
  Bee_Types,
  Bee_Interface;

type
  TCore = class(TThread)
  private
    FApp:      TApp;
    FKey:      string;
    FStatus:   integer;
    FParams:   TStringList;
    FMessages: TStringList;
    FMessage:  string;
    FDataRes:  pointer;
    FData:     pointer;
    FContents: TList;
  private
    procedure ProcessFatalError(const aMessage: string);
    procedure ProcessError(const aMessage: string);
    procedure ProcessWarning(const aMessage: string);
    procedure ProcessMessage(const aMessage: string);
    procedure ProcessOverwrite(const aFileInfo: TFileInfoRec; var Result: char);
    procedure ProcessRename(const aFileInfo: TFileInfoRec; var Result: string);
    procedure ProcessList(const aFileInfo: TFileFullInfoRec);
    procedure ProcessKey(const aFileInfo: TFileInfoRec; var Result: string);
    procedure ProcessRequest(const aMessage: string);
    procedure ProcessTick;
    procedure ProcessClear;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property Status: integer Read FStatus Write FStatus;
    property DataRes: pointer Read FDataRes;
    property Data: pointer Read FData;
  end;

  // -------------------------------------------------------------------------- //
  //                                                                            //
  //  Library TCore class implementation                                        //
  //                                                                            //
  // -------------------------------------------------------------------------- //

  constructor TCore.Create(const aCommandLine: string);
  begin
    inherited Create(True);
    FreeOnTerminate := False;

    FKey      := '';
    FMessage  := '';
    FStatus   := csReady;
    FMessages := TStringList.Create;
    FParams   := TStringList.Create;
    FParams.Text := aCommandLine;
    FDataRes  := nil;
    FData     := nil;
    FContents := TList.Create;

    FApp := TBeeApp.Create(FParams);
    FApp.OnFatalError := ProcessFatalError;
    FApp.OnError := ProcessError;
    FApp.OnWarning := ProcessWarning;
    FApp.OnMessage := ProcessMessage;
    FApp.OnOverwrite := ProcessOverwrite;
    FApp.OnRename := ProcessRename;
    FApp.OnList := ProcessList;
    FApp.OnKey := ProcessKey;
    FApp.OnRequest := ProcessRequest;
    FApp.OnTick := ProcessTick;
    FApp.OnClear := ProcessClear;
  end;

  destructor TCore.Destroy;
  var
    I: integer;
  begin
    FKey     := '';
    FMessage := '';
    FMessages.Free;
    FParams.Free;
    with FContents do
    begin
      for I := 0 to Count -1 do
      begin
        FreeMem(Items[I]);
      end;
      Clear;
    end;
    FContents.Free;
    inherited Destroy;
  end;

  procedure TCore.Execute;
  begin
    inherited Execute;
    FStatus := csExecuting;
    begin
      FApp.Execute;
    end;
    FStatus := csTerminated;
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
    FMessage := aMessage;
    FMessages.Add(FMessage);
  end;

  procedure TCore.ProcessOverwrite(const aFileInfo: TFileInfoRec; var Result: char);
  begin
    FData    := @aFileInfo;
    FDataRes := @Result;
    FStatus  := csWaitingOverwrite;
    while FStatus = csWaitingOverwrite do
    begin
      Sleep(250);
    end;
    FDataRes := nil;
    FData    := nil;
  end;

  procedure TCore.ProcessRename(const aFileInfo: TFileInfoRec; var Result: string);
  begin
    FData    := @aFileInfo;
    FDataRes := @Result;
    FStatus  := csWaitingRename;
    while FStatus = csWaitingRename do
    begin
      Sleep(250);
    end;
    FDataRes := nil;
    FData    := nil;
  end;

  procedure TCore.ProcessList(const aFileInfo: TFileFullInfoRec);
  var
   P: ^TFileFullInfoRec;
  begin
    GetMem(P, SizeOf(TFileFullInfoRec));
    P^ := aFileInfo;
    FContents.Add(P);
    with aFileInfo do
    begin
      FMessages.Add(FilePath + FileName);
    end;
  end;

  procedure TCore.ProcessKey(const aFileInfo: TFileInfoRec; var Result: string);
  begin
    FData    := @aFileInfo;
    FDataRes := @Result;
    FStatus  := csWaitingKey;
    while FStatus = csWaitingKey do
    begin
      Sleep(250);
    end;
    FDataRes := nil;
    FData    := nil;
  end;

  procedure TCore.ProcessRequest(const aMessage: string);
  begin
    FMessage := aMessage;
    FStatus  := csWaitingRequest;
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
    if ID <> nil then
    begin
      TCore(ID).Resume;
    end;
  end;

  function CoreDestroy(ID: pointer): pointer;
  var
    I: integer;
  begin
    Result := nil;
    if ID <> nil then
    begin
      TCore(ID).Free;
    end;
  end;

  procedure CoreSuspended(ID: pointer; Value: boolean);
  begin
    if ID <> nil then
    begin
      TCore(ID).FApp.Suspended := Value;
    end;
  end;

  procedure CoreTerminate(ID: pointer);
  begin
    if ID <> nil then
    begin
      TCore(ID).FApp.Terminated := True;
    end;
  end;

  function CoreGetExitCode(ID: pointer): integer;
  begin
    if ID <> nil then
      Result := TCore(ID).FApp.ExitCode
    else
      Result := 0;
  end;

  function CoreGetStatus(ID: pointer): integer;
  begin
    if ID <> nil then
      Result := TCore(ID).Status
    else
      Result := csTerminated;
  end;

  function CoreGetMessage(ID: pointer): string;
  begin
    if ID <> nil then
      Result := TCore(ID).FMessage
    else
      Result := '';
  end;

  function CoreGetMessages(ID: pointer): string;
  begin
    if ID <> nil then
      Result := TCore(ID).FMessages.Text
    else
      Result := '';
  end;

  function CoreGetElapsedTime(ID: pointer): cardinal;
  begin
    if ID <> nil then
      Result := TCore(ID).FApp.ElapsedTime
    else
      Result := 0;
  end;

  function CoreGetRemainingTime(ID: pointer): cardinal;
  begin
    if ID <> nil then
      Result := TCore(ID).FApp.RemainingTime
    else
      Result := 0;
  end;

  function CoreGetPercentes(ID: pointer): cardinal;
  begin
    if ID <> nil then
      Result := TCore(ID).FApp.Percentes
    else
      Result := 0;
  end;

  function CoreGetSpeed(ID: pointer): cardinal;
  begin
    if ID <> nil then
      Result := TCore(ID).FApp.Speed
    else
      Result := 0;
  end;

  function CoreGetTotalSize(ID: pointer): int64;
  begin
    if ID <> nil then
      Result := TCore(ID).FApp.TotalSize
    else
      Result := 0;
  end;

  function CoreGetProcessedSize(ID: pointer): int64;
  begin
    if ID <> nil then
      Result := TCore(ID).FApp.ProcessedSize
    else
      Result := 0;
  end;

  procedure CoreSetPriority(ID: pointer; aPriority: TThreadPriority);
  begin
    if ID <> nil then
    begin
      TCore(ID).Priority := aPriority;
    end;
  end;

  function CoreGetPriority(ID: pointer): TThreadPriority;
  begin
    if ID <> nil then
      Result := TCore(ID).Priority
    else
      Result := tpNormal;
  end;

  function CoreGetOverwriteFileInfo(ID: pointer): TFileInfoRec;
  begin
    if ID <> nil then
    begin
      Result := TFileInfoRec(TCore(ID).Data^);
    end;
  end;

  procedure CoreSetOverwriteFileInfoRes(ID: pointer; Result: char);
  begin
    if ID <> nil then
    begin
      Char(TCore(ID).DataRes^) := Result;
      TCore(ID).Status := csExecuting;
    end;
  end;

  function CoreGetRenameFileInfo(ID: pointer): TFileInfoRec;
  begin
    if ID <> nil then
    begin
      Result := TFileInfoRec(TCore(ID).Data^);
    end;
  end;

  procedure CoreSetRenameFileInfoRes(ID: pointer; Result: string);
  begin
    if ID <> nil then
    begin
      String(TCore(ID).DataRes^) := Result;
      TCore(ID).Status := csExecuting;
    end;
  end;

  function CoreGetPasswordFileInfo(ID: pointer): TFileInfoRec;
  begin
    if ID <> nil then
    begin
      Result := TFileInfoRec(TCore(ID).Data^);
    end;
  end;

  procedure CoreSetPasswordFileInfoRes(ID: pointer; Result: string);
  begin
    if ID <> nil then
    begin
      String(TCore(ID).DataRes^) := Result;
      TCore(ID).Status := csExecuting;
    end;

  end;

  function CoreGetRequestMessage(ID: pointer): string;
  begin
    if ID <> nil then
    begin
      Result := TCore(ID).FMessage;
    end;
  end;

  procedure CoreSetRequestMessage(ID: pointer);
  begin
    if ID <> nil then
    begin
      TCore(ID).Status := csExecuting;
    end;
  end;

  function CoreGetItemsCount(ID: pointer): cardinal;
  begin
    if ID <> nil then
      Result := TCore(ID).FContents.Count
    else
      Result := 0;
  end;

  function CoreGetItems(ID: pointer; Index: cardinal): TFileFullInfoRec;
  begin
    if ID <> nil then
    begin
      with TCore(ID).FContents do
      begin
        Result := TFileFullInfoRec(Items[Index]^);
      end;
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
  CoreSuspended,
  CoreTerminate,

  CoreGetPriority,
  CoreSetPriority,
  CoreGetExitCode,
  CoreGetStatus,
  CoreGetMessage,
  CoreGetMessages,
  CoreGetPercentes,
  CoreGetSpeed,
  CoreGetTotalSize,
  CoreGetProcessedSize,
  CoreGetElapsedTime,
  CoreGetRemainingTime,

  CoreGetOverwriteFileInfo,
  CoreSetOverwriteFileInfoRes,
  CoreGetRenameFileInfo,
  CoreSetRenameFileInfoRes,
  CoreGetPasswordFileInfo,
  CoreSetPasswordFileInfoRes,
  CoreGetRequestMessage,
  CoreSetRequestMessage,

  CoreGetItemsCount,
  CoreGetItems;

begin

end.
