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
      if P^.FileName <> nil then StrDispose(P^.FileName);
      if P^.FilePath <> nil then StrDispose(P^.FilePath);
    end;
  end;

  procedure FreePFileInfoExtra(P: PFileInfoExtra);
  begin
    if P <> nil then
    begin
      if P^.FileName     <> nil then StrDispose(P^.FileName);
      if P^.FilePath     <> nil then StrDispose(P^.FilePath);
      if P^.FileComm     <> nil then StrDispose(P^.FileComm);
      if P^.FileMethod   <> nil then StrDispose(P^.FileMethod);
      if P^.FileVersion  <> nil then StrDispose(P^.FileVersion);
      if P^.FilePassword <> nil then StrDispose(P^.FilePassword);
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
    destructor Destroy; override;
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
      Dec(FCount);
    end;
    FCount := 0;
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
    procedure ProcessRename   (const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessPassword (const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessOverWrite(const aFileInfo: TFileInfo; var Result: string);
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
    FApp.OnPassword   := ProcessPassword;
    FApp.OnRequest    := ProcessRequest;
    FApp.OnProgress   := nil;
    FApp.OnClear      := nil;
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

  procedure TCore.ProcessOverWrite(const aFileInfo: TFileInfo; var Result: string);
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

  procedure TCore.ProcessPassword(const aFileInfo: TFileInfo; var Result: string);
  begin
    FResult := Result;
    FItem   := aFileInfo;
    FStatus := csWaitingPassword;
    while FStatus = csWaitingPassword do
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

// -------------------------------------------------------------------------- //
//                                                                            //
//  Library core routines                                                     //
//                                                                            //
// -------------------------------------------------------------------------- //

var
  Core: TCore = nil;

  function CoreCreate(aCommandLine: PChar): boolean;
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

  function CoreGetPriority: integer;
  begin
    if (Core <> nil) then
    begin
      case Core.Priority of
        tpIdle:         Result :=  cpIdle;
        tpLowest:       Result :=  cpLowest;
        tpLower:        Result :=  cpLower;
        tpNormal:       Result :=  cpNormal;
        tpHigher:       Result :=  cpHigher;
        tpHighest:      Result :=  cpHighest;
        tpTimeCritical: Result :=  cpTimeCritical;
        else            Result :=  cpUnknow;
      end
    end else
      Result := -1;
  end;

  function CoreSetPriority(aValue: integer): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      case aValue of
        cpIdle:         Core.Priority := tpIdle;
        cpLowest:       Core.Priority := tpLowest;
        cpLower:        Core.Priority := tpLower;
        cpNormal:       Core.Priority := tpNormal;
        cpHigher:       Core.Priority := tpHigher;
        cpHighest:      Core.Priority := tpHighest;
        cpTimeCritical: Core.Priority := tpTimeCritical;
        else            Core.Priority := tpNormal;
      end
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
      Result := Core.FApp.TotalTime
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
      Result := Core.FApp.Time
    else
      Result := -1;
  end;

  function CoreGetSize: int64;
  begin
    if (Core <> nil) then
      Result := Core.FApp.Size
    else
      Result := -1;
  end;

  function CoreGetCode: integer;
  begin
    if (Core <> nil) then
      Result := Core.FApp.Code
    else
      Result := ccUnknow;
  end;

  function CoreGetStatus: integer;
  begin
    if (Core <> nil) then
      Result := Core.FStatus
    else
      Result := csUnknow;
  end;

  // ---

  function CoreGetRequestItem: PFileInfo;
  begin
    if (Core <> nil) then
      Result := @Core.FItem
    else
      Result := nil;
  end;

  function CoreGetRequest: PChar;
  begin
    if (Core <> nil) then
      Result := StringToPChar(Core.FMessage)
    else
      Result := nil;
  end;

  function CoreSetRequest(aValue: PChar): boolean;
  begin
    Result := (Core <> nil);
    if Result then
    begin
      Core.FResult := PCharToString(aValue);
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

  function CoreGetItems(aIndex: integer): PFileInfoExtra;
  begin
    if (Core <> nil) then
      Result := Core.FItems.Items[aIndex]
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
  FreePChar,
  FreePFileInfo,
  FreePFileInfoExtra;

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
  CoreGetRequestItem,
  CoreGetRequest,
  CoreSetRequest;

exports
  CoreGetItemsCount,
  CoreGetItems;

// -------------------------------------------------------------------------- //
//                                                                            //
//  Library main block                                                        //
//                                                                            //
// -------------------------------------------------------------------------- //

begin
  // initilization code
end.
