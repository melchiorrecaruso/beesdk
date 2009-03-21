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
  {$IFDEF UNIX}
  cThreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Bee_App,
  Bee_Types,
  Bee_Common,
  Bee_Interface;

type
  TCore = class(TThread)
  private
    FApp:       TApp;
    FKey:       string;
    FStatus:    integer;
    FParams:    TStringList;
    FMessages:  TStringList;
    FMessage:   string;
    FFileInfo:  TFileInfoA;
    FResult:    string;
    FContents:  TList;
  private
    procedure ProcessFatalError(const aMessage: string);
    procedure ProcessError(const aMessage: string);
    procedure ProcessWarning(const aMessage: string);
    procedure ProcessMessage(const aMessage: string);
    procedure ProcessOverwrite(const aFileInfo: TFileInfoA; var Result: char);
    procedure ProcessRename(const aFileInfo: TFileInfoA; var Result: string);
    procedure ProcessList(const aFileInfo: TFileInfoB);
    procedure ProcessKey(const aFileInfo: TFileInfoA; var Result: string);
    procedure ProcessRequest(const aMessage: string);
    procedure ProcessTick;
    procedure ProcessClear;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
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
    FContents := TList.Create;

    FParams.Text := aCommandLine;

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
  var
    I: integer;
  begin
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
    FApp.Free;
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

  procedure TCore.ProcessOverwrite(const aFileInfo: TFileInfoA; var Result: char);
  begin
    FResult   := Result;
    FFileInfo := aFileInfo;
    FStatus   := csWaitingOverwrite;
    while FStatus = csWaitingOverwrite do
    begin
      Sleep(250);
    end;
    Result := FResult[1];
  end;

  procedure TCore.ProcessRename(const aFileInfo: TFileInfoA; var Result: string);
  begin
    FResult   := Result;
    FFileInfo := aFileInfo;
    FStatus   := csWaitingRename;
    while FStatus = csWaitingRename do
    begin
      Sleep(250);
    end;
    Result := FResult;
  end;

  procedure TCore.ProcessList(const aFileInfo: TFileInfoB);
  var
   P: ^TFileInfoB;
  begin
    GetMem(P, SizeOf(TFileInfoB));
    P^ := aFileInfo;
    FContents.Add(P);
  end;

  procedure TCore.ProcessKey(const aFileInfo: TFileInfoA; var Result: string);
  begin
    FResult   := Result;
    FFileInfo := aFileInfo;
    FStatus   := csWaitingKey;
    while FStatus = csWaitingKey do
    begin
      Sleep(250);
    end;
    Result := FResult;
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

  function CoreCreate(const ACommandLine: PChar): integer;
  begin
    Result := integer(TCore.Create(PChar(ACommandLine)));
  end;

  function CoreExecute(ID: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).Resume;
    end;
  end;

  function CoreDestroy(ID: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).Destroy;
    end;
  end;

  function CoreSuspended(ID: integer; Value: boolean): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).FApp.Suspended := Value;
    end;
  end;

  function CoreTerminate(ID: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).FApp.Terminated := True;
      if TCore(pointer(ID)).FStatus = csReady then
      begin
        TCore(pointer(ID)).FStatus := csTerminated;
      end;
    end;
  end;

  function CoreGetExitCode(ID: integer; var AExitCode: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AExitCode := TCore(pointer(ID)).FApp.ExitCode;
    end;
  end;

  function CoreGetStatus(ID: integer; var AStatus: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AStatus := TCore(pointer(ID)).FStatus;
    end;
  end;

  function CoreGetMessage(ID: integer; var AMessage: PChar): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      strcopy(aMessage, PChar(TCore(pointer(ID)).FMessage));
    end;
  end;

  function CoreGetMessages(ID: integer; var AMessages: PChar): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      strcopy(AMessages, PChar(TCore(pointer(ID)).FMessages.Text));
    end;
  end;

  function CoreGetElapsedTime(ID: integer; var AElapsedTime: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AElapsedTime := TCore(pointer(ID)).FApp.ElapsedTime;
    end;
  end;

  function CoreGetRemainingTime(ID: integer; var ARemainingTime: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      ARemainingTime := TCore(pointer(ID)).FApp.RemainingTime;
    end;;
  end;

  function CoreGetPercentes(ID: integer; var APercentes: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      APercentes := TCore(pointer(ID)).FApp.Percentes;
    end;
  end;

  function CoreGetSpeed(ID: integer; var ASpeed: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      ASpeed := TCore(pointer(ID)).FApp.Speed;
    end;
  end;

  function CoreGetTotalSize(ID: integer; var ATotalSize: int64): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      ATotalSize := TCore(pointer(ID)).FApp.TotalSize;
    end;
  end;

  function CoreGetProcessedSize(ID: integer; var AProcessedSize: int64): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AProcessedSize := TCore(pointer(ID)).FApp.ProcessedSize;
    end;
  end;

  function CoreSetPriority(ID: integer; aPriority: TThreadPriority): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).Priority := aPriority;
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

  function CoreGetOverwriteFileInfo(ID: integer): PFileInfoA;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      // Result := PFileInfoRec(TCore(P).Data^);
    end;
  end;

  procedure CoreSetOverwriteFileInfoRes(ID: integer; Result: char);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).FResult := Result;
      TCore(P).FStatus := csExecuting;
    end;
  end;

  function CoreGetRenameFileInfo(ID: integer): PFileInfoA;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      // Result := PFileInfoRec(TCore(P).Data^);
    end;
  end;

  procedure CoreSetRenameFileInfoRes(ID: integer; Result: Pchar);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).FResult := Result;
      TCore(P).FStatus := csExecuting;
    end;
  end;

  function CoreGetPasswordFileInfo(ID: integer): PFileInfoA;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      // Result := PFileInfoRec(TCore(P).Data^);
    end;
  end;

  procedure CoreSetPasswordFileInfoRes(ID: integer; Result: PChar);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).FResult := Result;
      TCore(P).FStatus := csExecuting;
    end;
  end;

  function CoreGetRequestMessage(ID: integer): PChar;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      Result := PChar(TCore(P).FMessage);
    end;
  end;

  procedure CoreSetRequestMessage(ID: integer);
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      TCore(P).FStatus := csExecuting;
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

  function CoreGetItems(ID: integer; Index: cardinal): PFileInfoB;
  var
    P: pointer;
  begin
    P := pointer(ID);
    if P <> nil then
    begin
      with TCore(P).FContents do
      begin
        Result := PFileInfoB(Items[Index]^);
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
