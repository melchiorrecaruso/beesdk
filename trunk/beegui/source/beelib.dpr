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
  Dialogs,
  SysUtils,
  {$IFDEF UNIX}
  cThreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
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

  function CoreCreate(const aCommandLine: string): integer;
  begin
    Result := integer(TCore.Create(aCommandLine));

    // ShowMessage(IntToStr(Result);
  end;

  function CoreExecute(ID: integer): integer;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).Resume;
    end;
    Result := ID;

    // ShowMessage(IntToStr(Result);
  end;

  function CoreDestroy(ID: integer): integer;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).Destroy;
    end;
    Result := integer(nil);
  end;

  procedure CoreSuspended(ID: integer; Value: boolean);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).FApp.Suspended := Value;
    end;
  end;

  procedure CoreTerminate(ID: integer);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).FApp.Terminated := True;
      if TCore(P).Status = csReady then
      begin
        TCore(P).Status := csTerminated;
      end;
    end;
  end;

  function CoreGetExitCode(ID: integer): integer;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FApp.ExitCode
    else
      Result := 0;
  end;

  function CoreGetStatus(ID: integer): integer;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).Status
    else
      Result := csTerminated;
  end;

  function CoreGetMessage(ID: integer): string;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FMessage
    else
      Result := '';
  end;

  function CoreGetMessages(ID: integer): string;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FMessages.Text
    else
      Result := '';
  end;

  function CoreGetElapsedTime(ID: integer): cardinal;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FApp.ElapsedTime
    else
      Result := 0;
  end;

  function CoreGetRemainingTime(ID: integer): cardinal;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FApp.RemainingTime
    else
      Result := 0;
  end;

  function CoreGetPercentes(ID: integer): cardinal;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FApp.Percentes
    else
      Result := 0;
  end;

  function CoreGetSpeed(ID: integer): cardinal;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FApp.Speed
    else
      Result := 0;
  end;

  function CoreGetTotalSize(ID: integer): int64;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FApp.TotalSize
    else
      Result := 0;
  end;

  function CoreGetProcessedSize(ID: integer): int64;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FApp.ProcessedSize
    else
      Result := 0;
  end;

  procedure CoreSetPriority(ID: integer; aPriority: TThreadPriority);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).Priority := aPriority;
    end;
  end;

  function CoreGetPriority(ID: integer): TThreadPriority;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).Priority
    else
      Result := tpNormal;
  end;

  function CoreGetOverwriteFileInfo(ID: integer): TFileInfoRec;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      Result := TFileInfoRec(TCore(P).Data^);
    end;
  end;

  procedure CoreSetOverwriteFileInfoRes(ID: integer; Result: char);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      char(TCore(P).DataRes^) := Result;
      TCore(P).Status := csExecuting;
    end;
  end;

  function CoreGetRenameFileInfo(ID: integer): TFileInfoRec;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      Result := TFileInfoRec(TCore(P).Data^);
    end;
  end;

  procedure CoreSetRenameFileInfoRes(ID: integer; Result: string);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      string(TCore(P).DataRes^) := Result;
      TCore(P).Status := csExecuting;
    end;
  end;

  function CoreGetPasswordFileInfo(ID: integer): TFileInfoRec;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      Result := TFileInfoRec(TCore(P).Data^);
    end;
  end;

  procedure CoreSetPasswordFileInfoRes(ID: integer; Result: string);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      string(TCore(P).DataRes^) := Result;
      TCore(P).Status := csExecuting;
    end;
  end;

  function CoreGetRequestMessage(ID: integer): string;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      Result := TCore(P).FMessage;
    end;
  end;

  procedure CoreSetRequestMessage(ID: integer);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).Status := csExecuting;
    end;
  end;

  function CoreGetItemsCount(ID: integer): cardinal;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
      Result := TCore(P).FContents.Count
    else
      Result := 0;
  end;

  function CoreGetItems(ID: integer; Index: cardinal): TFileFullInfoRec;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      with TCore(P).FContents do
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
