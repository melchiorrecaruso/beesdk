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

  function CoreSuspended(ID: integer; AValue: boolean): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).FApp.Suspended := AValue;
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

  function CoreGetExitCode(ID: integer; var AValue: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FApp.ExitCode;
    end;
  end;

  function CoreGetStatus(ID: integer; var AValue: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FStatus;
    end;
  end;

  function CoreGetMessage(ID: integer; var AValue: PChar): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      strcopy(AValue, PChar(TCore(pointer(ID)).FMessage));
    end;
  end;

  function CoreGetMessages(ID: integer; var AValue: PChar): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      strcopy(AValue, PChar(TCore(pointer(ID)).FMessages.Text));
    end;
  end;

  function CoreGetElapsedTime(ID: integer; var AValue: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FApp.ElapsedTime;
    end;
  end;

  function CoreGetRemainingTime(ID: integer; var AValue: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FApp.RemainingTime;
    end;
  end;

  function CoreGetPercentes(ID: integer; var AValue: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FApp.Percentes;
    end;
  end;

  function CoreGetSpeed(ID: integer; var AValue: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FApp.Speed;
    end;
  end;

  function CoreGetTotalSize(ID: integer; var AValue: int64): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FApp.TotalSize;
    end;
  end;

  function CoreGetProcessedSize(ID: integer; var AValue: int64): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FApp.ProcessedSize;
    end;
  end;

  function CoreSetPriority(ID: integer; const AValue: TThreadPriority): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).Priority := AValue;
    end;
  end;

  function CoreGetPriority(ID: integer; var AValue: TThreadPriority): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).Priority
    end;
  end;

  function CoreGetOverwriteFileInfo(ID: integer; var AValue: PFileInfoA): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      // ---
    end;
  end;

  function CoreSetOverwriteFileInfo(ID: integer; const AValue: char): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).FResult := AValue;
      TCore(pointer(ID)).FStatus := csExecuting;
    end;
  end;

  function CoreGetRenameFileInfo(ID: integer; var AValue: PFileInfoA): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      // ---
    end;
  end;

  function CoreSetRenameFileInfo(ID: integer; const AValue: PChar): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      strcopy(PChar(TCore(pointer(ID)).FResult), AValue);
      TCore(pointer(ID)).FStatus := csExecuting;
    end;
  end;

  function CoreGetPasswordFileInfo(ID: integer; var AValue: PFileInfoA): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      // ---
    end;
  end;

  function CoreSetPasswordFileInfo(ID: integer; const AValue: PChar): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      strcopy(PChar(TCore(pointer(ID)).FResult), AValue);
      TCore(pointer(ID)).FStatus := csExecuting;
    end;
  end;

  function CoreGetRequestMessage(ID: integer; var AValue: PChar): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      strpcopy(AValue, PChar(TCore(pointer(ID)).FMessage));
    end;
  end;

  function CoreSetRequestMessage(ID: integer; const AValue: PChar): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      TCore(pointer(ID)).FStatus := csExecuting;
    end;
  end;

  function CoreGetItemsCount(ID: integer; var AValue: integer): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      AValue := TCore(pointer(ID)).FContents.Count;
    end;
  end;

  function CoreGetItems(ID: integer; const AIndex: integer; var AValue: PFileInfoB): boolean;
  begin
    Result := pointer(ID) <> nil;
    if Result then
    begin
      with TCore(pointer(ID)).FContents do
      begin

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
  CoreSetOverwriteFileInfo,
  CoreGetRenameFileInfo,
  CoreSetRenameFileInfo,
  CoreGetPasswordFileInfo,
  CoreSetPasswordFileInfo,
  CoreGetRequestMessage,
  CoreSetRequestMessage,

  CoreGetItemsCount,
  CoreGetItems;

begin

end.
