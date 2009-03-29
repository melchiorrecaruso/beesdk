{
  Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

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
  {$IFDEF UNIX} cThreads, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Bee_App,
  Bee_Types,
  Bee_Common,
  Bee_Interface;

// -------------------------------------------------------------------------- //
//                                                                            //
//  Library routines ...                                                      //
//                                                                            //
// -------------------------------------------------------------------------- //

  function LibVersion: integer;
  begin
    Result := 100;
  end;

  procedure FreePChar(P: PChar);
  begin
    if P <> nil then
    begin
      StrDispose(P);
    end;
  end;

  // not exported routines ...

  procedure FreePFileInfo(P: PFileInfo);
  begin
    if P <> nil then
    begin
      StrDispose(P^.FileName);
      StrDispose(P^.FilePath);
    end;
  end;

  procedure FreePFileInfoExtra(P: PFileInfoExtra);
  begin
    if P <> nil then
    begin
      StrDispose(P^.FileName);
      StrDispose(P^.FilePath);
      StrDispose(P^.FileComm);
      StrDispose(P^.FileMethod);
      StrDispose(P^.FileVersion);
      StrDispose(P^.FilePassword);
    end;
  end;

// -------------------------------------------------------------------------- //
//                                                                            //
//  TCoreItems class ...                                                      //
//                                                                            //
// -------------------------------------------------------------------------- //

type

  TCoreItems = class
  private
    FCount: integer;
    FItems: array of TFileInfoExtra;
  private
    function GetItem(Index: integer): PFileInfoExtra;
  public
    constructor Create;
    destructor Destroy;
    procedure Clear;
    function Add(P: TFileInfoExtra): integer;
  public
    property Count: integer read FCount;
    property Items[Index: integer]: PFileInfoExtra read GetItem;
  end;

  constructor TCoreItems.Create;
  begin
    inherited Create;
    FItems := nil;
    FCount := 0;
  end;

  destructor TCoreItems.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;

  procedure TCoreItems.Clear;
  begin
    while FCount > 0 do
    begin
      FreePFileInfoExtra(@FItems[FCount -1]);
      Dec(Count);
    end;
    FItems := nil;
  end;

  function TCoreItems.GetItem(Index: integer): PFileInfoExtra;
  begin
    if (Index > -1) and (Index < FCount) then
      Result := @FItems[Index]
    else
      Result := nil;
  end;

  function TCoreItems.Add(P: TFileInfoExtra): integer;
  begin
    Inc(FCount);
    SetLength(FItems, FCount);
    with FItems[FCount -1] do
    begin
      FileName     := StrNew(P.FileName);
      FilePath     := StrNew(P.FilePath);
      FileSize     := P.FileSize;
      FileTime     := P.FileTime;
      FileAttr     := P.FileAttr;
      FilePacked   := P.FilePacked;
      FileRatio    := P.FileRatio;
      FileComm     := StrNew(P.FileComm);
      FileCrc      := P.FileCrc;
      FileMethod   := StrNew(P.FileMethod);
      FileVersion  := StrNew(P.FileVersion);
      FilePassword := StrNew(P.FilePassword);
      FilePosition := P.FileCrc;
    end;
    Result := FCount -1;
  end;

// -------------------------------------------------------------------------- //
//                                                                            //
//  TCore class ...                                                           //
//                                                                            //
// -------------------------------------------------------------------------- //

type

  TCore = class(TThread)
  private
    FApp:      TApp;
    FParams:   TStringList;
    FMessages: TStringList;
    FMessage:  string;
    FPassword: string;
    FStatus:   integer;
    FResult:   string;
    FItems:    TCoreItems;
    FItem:     TFileInfo;
  private
    procedure ProcessFatalError(const aMessage: string);
    procedure ProcessError     (const aMessage: string);
    procedure ProcessWarning   (const aMessage: string);
    procedure ProcessMessage   (const aMessage: string);
    procedure ProcessRequest   (const aMessage: string);

    procedure ProcessList     (const aFileInfo: TFileInfoExtra);
    procedure ProcessOverwrite(const aFileInfo: TFileInfo; var Result: char);
    procedure ProcessRename   (const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessKey      (const aFileInfo: TFileInfo; var Result: string);

    procedure ProcessTick;
    procedure ProcessClear;
  public
    constructor Create(const aCommandLine: string);
    destructor  Destroy; override;
    procedure   Execute; override;
  end;

  constructor TCore.Create(const aCommandLine: string);
  begin
    inherited Create(True);
    FreeOnTerminate := False;

    FItems    := TCoreItems.Create;
    FMessages := TStringList.Create;
    FParams   := TStringList.Create;
    with FParams do
    begin
      Text := aCommandLine;
    end;
    FStatus   := csReady;
    FMessage  := ' Ready ...';
    FPassword := '';

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
    FApp.Destroy;
    FParams.Destroy;
    FMessages.Destroy;
    FItems.Destroy;
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

  procedure TCore.ProcessOverwrite(const aFileInfo: TFileInfo; var Result: char);
  begin
    FResult := Result;
    FItem   := aFileInfo;
    FStatus := csWaitingOverwrite;
    while FStatus = csWaitingOverwrite do
    begin
      Sleep(250);
    end;

    if Length(FResult) = 1 then
    begin
      Result := FResult[1];
    end;
  end;

  procedure TCore.ProcessRename(const aFileInfo: TFileInfo; var Result: string);
  begin
    FResult := Result;
    FItem   := aFileInfo;
    FStatus := csWaitingRename;
    while FStatus = csWaitingRename do
    begin
      Sleep(250);
    end;
    Result := FResult;
  end;

  procedure TCore.ProcessKey(const aFileInfo: TFileInfo; var Result: string);
  begin
    FResult := Result;
    FItem   := aFileInfo;
    FStatus := csWaitingKey;
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

  procedure TCore.ProcessList(const aFileInfo: TFileInfoExtra);
  begin
    FItems.Add(aFileInfo);
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

var
  Core: TCore = nil;

  function CoreCreate(const aCommandLine: PChar): boolean;
  begin
    Result := (Core = nil);
    if Result then
    begin
      Core := TCore.Create(PCharToString(aCommandLine));
    end;
  end;

  function CoreDestroy: boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      FreeAndNil(Core);
    end;
  end;

  function CoreExecute: boolean;
  begin
    Result := (Core <> nil) and (Core.FStatus = csReady);
    if Result then
    begin
      Core.Resume;
    end;
  end;

  function CoreSuspended(aValue: boolean): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      Core.FApp.Suspended := aValue;
    end;
  end;

  function CoreTerminate: boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      Core.FApp.Terminated := True;
      if Core.FStatus <> csTerminated then
      begin
        Core.FStatus := csExecuting;
      end;
    end;
  end;

  // ---

  function CoreGetPriority(var aValue: TThreadPriority): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      aValue := Core.Priority
    end;
  end;

  function CoreSetPriority(const aValue: TThreadPriority): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      Core.Priority := aValue;
    end;
  end;

  // ---

  function CoreGetSpeed: integer;
  begin
    if (Core <> nil) then
      Result := Core.FApp.Speed
    else
      Result := -1;
  end;

  function CoreGetRequest: PChar;
  begin
    if (Core <> nil) then
      Result := StringToPChar(Core.FMessage)
    else
      Result := nil;
  end;

  function CoreGetMessage: PChar;
  begin
    if (Core <> nil) then
      Result := StringToPChar(Core.FMessage)
    else
      Result := nil;
  end;

  function CoreGetMessages: PChar;
  begin
    if (Core <> nil) then
      Result := StringToPChar(Core.FMessages.Text)
    else
      Result := nil;
  end;

  function CoreGetPercentes: integer;
  begin
    if (Core <> nil) then
      Result := Core.FApp.Percentes
    else
      Result := -1;
  end;

  function CoreGetTotalTime: integer;
  begin
    if (Core <> nil) then
      Result := Core.FApp.ElapsedTime
    else
      Result := -1;
  end;

  function CoreGetTotalSize: int64;
  begin
    if (Core <> nil) then
      Result := Core.FApp.TotalSize
    else
      Result := -1;
  end;

  function CoreGetTime: integer;
  begin
    if (Core <> nil) then
      Result := Core.FApp.RemainingTime
    else
      Result := -1;
  end;

  function CoreGetSize: int64;
  begin
    if (Core <> nil) then
      Result := Core.FApp.ProcessedSize
    else
      Result := -1;
  end;

  function CoreGetCode: integer;
  begin
    if (Core <> nil) then
      Result := Core.FApp.ExitCode
    else
      Result := esUnknow;
  end;

  function CoreGetStatus: integer;
  begin
    if (Core <> nil) then
      Result := Core.FStatus
    else
      Result := csUnknow;
  end;

  // ---

  function CoreSetRequest(const aValue: PChar): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      Core.FResult := PCharToString(aValue);
      Core.FStatus := csExecuting;
    end;
  end;

  function CoreSetRename(const aValue: PChar): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      Core.FResult := PCharToString(aValue);
      Core.FStatus := csExecuting;
    end;
  end;

  function CoreSetPassword(const aValue: PChar): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      Core.FResult := PCharToString(aValue);
      Core.FStatus := csExecuting;
    end;
  end;

  function CoreSetOverwrite(const aValue: char): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      Core.FResult := aValue;
      Core.FStatus := csExecuting;
    end;
  end;

  // ---

  function CoreGetItemsCount: integer;
  begin
    if (Core <> nil) then
      Result := Core.FItems.Count
    else
      Result := 0;
  end;

  function CoreGetItems(const aIndex: integer): PFileInfoExtra;
  begin
    if (Core <> nil) then
      Result := Core.FItems.Items[aIndex]
    else
      Result := nil;
  end;

  function CoreGetItem: PFileInfo;
  begin
    if (Core <> nil) then
      Result := @Core.FItem
    else
      Result := nil;
  end;

// -------------------------------------------------------------------------- //
//                                                                            //
//  Library core routines exported                                            //
//                                                                            //
// -------------------------------------------------------------------------- //

exports
  LibVersion;

exports
  FreePChar;

exports
  CoreCreate,
  CoreDestroy,
  CoreExecute,
  CoreSuspended,
  CoreTerminate;

exports
  CoreGetPriority,
  CoreSetPriority;

exports
  CoreGetSpeed,
  CoreGetRequest,
  CoreGetMessage,
  CoreGetMessages,
  CoreGetPercentes,
  CoreGetTotalTime,
  CoreGetTotalSize,
  CoreGetTime,
  CoreGetSize,
  CoreGetCode,
  CoreGetStatus;

exports
  CoreSetRequest,
  CoreSetRename,
  CoreSetPassword,
  CoreSetOverwrite;

exports
  CoreGetItemsCount,
  CoreGetItems,
  CoreGetItem;

// -------------------------------------------------------------------------- //
//                                                                            //
//  Library main block                                                        //
//                                                                            //
// -------------------------------------------------------------------------- //

begin
  // initilization code
end.
