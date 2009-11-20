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

    Bee archiver library interface.

  Modifyed:

    v0.8.0 build 1065 - 2009.11.11 by Melchiorre Caruso.
}

unit Bee_Library;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF FPC} {$IFDEF PLUGINS}
  BeeLib_Plugins,
  {$ENDIF} {$ENDIF}
  Bee_App,
  Bee_Types,
  Bee_Consts,
  Bee_Common,
  Bee_Interface,
  Bee_CommandLine;

//  Library routines ...
function  CoreLibVersion: integer;
// ---
procedure CoreFreePChar(P: PChar);
procedure CoreFreePFileInfo(P: PFileInfo);
procedure CoreFreePFileInfoExtra(P: PFileInfoExtra);

//  Library core routines ...
function CoreCreate(aCommandLine: PChar): boolean;
function CoreDestroy: boolean;
function CoreExecute: boolean;
function CoreTerminate: boolean;
function CorePriority(aValue: integer): integer;
function CoreSuspend(aValue: boolean): boolean;
// ---
function CoreRequest(aValue: PChar): PChar;
function CoreGetMessage(all: boolean): PChar;
function CoreGetTime(all: boolean): integer;
function CoreGetSize(all: boolean): int64;

function CoreGetSpeed: integer;
function CoreGetPercentes: integer;
function CoreGetStatus: integer;
function CoreGetCode: integer;
// ---
function CoreGetItem: PFileInfo;
function CoreGetItems(aIndex: integer): PFileInfoExtra;

implementation

type
  // TCoreApp class ...

  TCoreApp = class(TBeeApp)
  private
    FItems: TList;
    FItem: TFileInfo;
    FMessages: TStringList;
    FMessage: string;
    FPassword: string;
    FStatus: integer;
  protected
    procedure ClearItems;
    procedure AddItem(aFileInfo: TFileInfoExtra);
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
  public
    procedure OnFatalError(const aMessage: string); override;
    procedure OnError(const aMessage: string); override;
    procedure OnWarning(const aMessage: string); override;
    procedure OnRequest(const aMessage: string); override;
    procedure OnMessage(const aMessage: string); override;
    function OnOverwrite(const aFileInfo: TFileInfo; const aValue: string): string; override;
    function OnRename(const aFileInfo: TFileInfo; const aValue: string): string; override;
    function OnPassword(const aFileInfo: TFileInfo; const aValue: string): string; override;
    procedure OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean); override;
    procedure OnProgress; override;
    procedure OnClearLine; override;
  end;

  // TCore class ...

  TCore = class(TThread)
  private
    FApp: TCoreApp;
    FParams: TStringList;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  end;

var
  Core: TCore = nil;

  // ------------------------------------------------------------------------ //
  //                                                                          //
  //  TCoreApp class ...                                                      //
  //                                                                          //
  // ------------------------------------------------------------------------ //

  constructor TCoreApp.Create(aParams: TStringList);
  begin
    inherited Create(aParams);
    FMessages := TStringList.Create;
    FItems := TList.Create;

    SetLength(FPassword, 0);
    SetLength(FMessage, 0);
    FStatus := csReady;
  end;

  destructor TCoreApp.Destroy;
  begin
    SetLength(FPassword, 0);
    SetLength(FMessage, 0);

    ClearItems;
    FItems.Destroy;
    FMessages.Destroy;
    inherited Destroy;
  end;

  procedure TCoreApp.ClearItems;
  var
    I: integer;
  begin
    for I := 0 to FItems.Count -1 do
    begin
      CoreFreePFileInfoExtra(FItems[I]);
    end;
  end;

  procedure TCoreApp.AddItem(aFileInfo: TFileInfoExtra);
  var
    P: PFileInfoExtra;
  begin
    P := GetMem(SizeOf(TFileInfoExtra));
    if P <> nil then
    begin
      FItems.Add(P);

      P^.FileName     := StrNew(aFileInfo.FileName);
      P^.FilePath     := StrNew(aFileInfo.FilePath);
      P^.FileSize     :=        aFileInfo.FileSize;
      P^.FileTime     :=        aFileInfo.FileTime;
      P^.FileAttr     :=        aFileInfo.FileAttr;
      P^.FilePacked   :=        aFileInfo.FilePacked;
      P^.FileRatio    :=        aFileInfo.FileRatio;
      P^.FileComm     := StrNew(aFileInfo.FileComm);
      P^.FileCrc      :=        aFileInfo.FileCrc;
      P^.FileMethod   := StrNew(aFileInfo.FileMethod);
      P^.FileVersion  := StrNew(aFileInfo.FileVersion);
      P^.FilePassword := StrNew(aFileInfo.FilePassword);
      P^.FilePosition :=        aFileInfo.FilePosition;
    end;
  end;

  procedure TCoreApp.OnFatalError(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCoreApp.OnError(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCoreApp.OnWarning(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCoreApp.OnMessage(const aMessage: string);
  begin
    FMessages.Add(aMessage);
    FMessage := aMessage;
  end;

  function TCoreApp.OnOverWrite(const aFileInfo: TFileInfo; const aValue: string): string;
  begin
    FMessage := aValue;
    FItem    := aFileInfo;
    FStatus  := csWaitingOverwrite;
    while FStatus = csWaitingOverwrite do Sleep(250);

    if Length(FMessage) = 1 then
      Result := FMessage[1];
  end;

  function TCoreApp.OnRename(const aFileInfo: TFileInfo; const aValue: string): string;
  begin
    FMessage := aValue;
    FItem    := aFileInfo;
    FStatus  := csWaitingRename;
    while FStatus = csWaitingRename do Sleep(250);

    Result := FMessage;
  end;

  function TCoreApp.OnPassword(const aFileInfo: TFileInfo; const aValue: string): string;
  begin
    FMessage := aValue;
    FItem    := aFileInfo;
    FStatus  := csWaitingPassword;
    while FStatus = csWaitingPassword do Sleep(250);

    Result := FMessage;
  end;

  procedure TCoreApp.OnRequest(const aMessage: string);
  begin
    FMessage := aMessage;
    FStatus  := csWaitingRequest;
    while FStatus = csWaitingRequest do Sleep(250);
  end;

  procedure TCoreApp.OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
  begin
    AddItem(aFileInfo);
  end;

  procedure TCoreApp.OnProgress;
  begin
    // nothing to do
  end;

  procedure TCoreApp.OnClearLine;
  begin
    // nothing to do
  end;

  // ------------------------------------------------------------------------ //
  //                                                                          //
  //  TCore class ...                                                         //
  //                                                                          //
  // ------------------------------------------------------------------------ //

  constructor TCore.Create(const aCommandLine: string);
  var
    FCommandLine: TCommandLine;
  begin
    inherited Create(True);
    FreeOnTerminate := False;

    FParams := TStringList.Create;
    with FParams do
    begin
      Text := aCommandLine;
    end;
    FCommandLine := TCommandLine.Create;
    FCommandLine.Process(FParams);
    {$IFDEF FPC} {$IFDEF PLUGINS}
    if SevenZipPlugin(FCommandLine.ArchiveName) then
      FApp := TSevenZipApp.Create(FParams)
    else
    {$ENDIF} {$ENDIF}
      FApp := TCoreApp.Create(FParams);
    FreeAndNil(FCommandLine);
  end;

  destructor TCore.Destroy;
  begin
    FApp.Destroy;
    FParams.Destroy;
    inherited Destroy;
  end;

  procedure TCore.Execute;
  begin
    FApp.FStatus := csExecuting;
    begin
      FApp.Execute;
    end;
    FApp.FStatus := csTerminated;
  end;

  // ------------------------------------------------------------------------ //
  //                                                                          //
  //  Library routines ...                                                    //
  //                                                                          //
  // ------------------------------------------------------------------------ //

  function CoreLibVersion: integer;
  begin
    Result := 103;
  end;

  procedure CoreFreePChar(P: PChar);
  begin
    if P <> nil then
    begin
      StrDispose(P);
      // P := nil;
    end;
  end;

  procedure CoreFreePFileInfo(P: PFileInfo);
  begin
    if P <> nil then
    begin
      if P^.FileName <> nil then StrDispose(P^.FileName);
      if P^.FilePath <> nil then StrDispose(P^.FilePath);
      // FreeMem(P);
      // P := nil;
    end;
  end;

  procedure CoreFreePFileInfoExtra(P: PFileInfoExtra);
  begin
    if P <> nil then
    begin
      if P^.FileName <> nil then StrDispose(P^.FileName);
      if P^.FilePath <> nil then StrDispose(P^.FilePath);
      if P^.FileComm <> nil then StrDispose(P^.FileComm);
      if P^.FileMethod <> nil then StrDispose(P^.FileMethod);
      if P^.FileVersion  <> nil then StrDispose(P^.FileVersion);
      if P^.FilePassword <> nil then StrDispose(P^.FilePassword);
      // FreeMem(P);
      // P := nil;
    end;
  end;

  // ------------------------------------------------------------------------ //
  //                                                                          //
  //  Library core routines ...                                               //
  //                                                                          //
  // ------------------------------------------------------------------------ //

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
    Result := (Core <> nil) and (Core.FApp.FStatus = csReady);
    if Result then
    begin
      Core.Resume;
    end;
  end;

  function CoreSuspend(aValue: boolean): boolean;
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
      if Core.FApp.FStatus <> csTerminated then
      begin
        Core.FApp.FStatus := csExecuting;
      end;
    end;
  end;

  // ---

  function CorePriority(aValue: integer): integer;
  begin
    if Core <> nil then
    begin
      case aValue of
        cpIdle:         Core.Priority := tpIdle;
        cpLowest:       Core.Priority := tpLowest;
        cpLower:        Core.Priority := tpLower;
        cpNormal:       Core.Priority := tpNormal;
        cpHigher:       Core.Priority := tpHigher;
        cpHighest:      Core.Priority := tpHighest;
        cpTimeCritical: Core.Priority := tpTimeCritical;
      end;

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
      Result :=  cpUnknow;
  end;

  // ---

  function CoreGetSpeed: integer;
  begin
    if (Core <> nil) then
      Result := Core.FApp.Speed
    else
      Result := -1;
  end;

  function CoreGetMessage(All: boolean): PChar;
  begin
    if (Core <> nil) then
    begin
      if All = False then
        Result := StringToPChar(Core.FApp.FMessage)
      else
        Result := StringToPChar(Core.FApp.FMessages.Text)
    end else
      Result := nil;
  end;

  function CoreGetPercentes: integer;
  begin
    if (Core <> nil) then
      Result := Core.FApp.Percentes
    else
      Result := -1;
  end;

  function CoreGetTime(All: boolean): integer;
  begin
    if (Core <> nil) then
    begin
      if All = False then
        Result := Core.FApp.Time
      else
        Result := Core.FApp.TotalTime;
    end else
      Result := -1;
  end;

  function CoreGetSize(All: boolean): int64;
  begin
    if (Core <> nil) then
    begin
      if All = False then
        Result := Core.FApp.Size
      else
        Result := Core.FApp.TotalSize
    end else
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
      Result := Core.FApp.FStatus
    else
      Result := csUnknow;
  end;

  // ---

  function CoreRequest(aValue: PChar): PChar;
  begin
    if (Core <> nil) then
    begin
      if aValue <> nil then
      begin
        Core.FApp.FMessage := PCharToString(aValue);
        Core.FApp.FStatus  := csExecuting;
      end else
        Result := StringToPChar(Core.FApp.FMessage);
    end else
      Result := nil;
  end;

  // ---

  function CoreGetItem: PFileInfo;
  begin
    if (Core <> nil) and (Core.FApp.FItems.Count > 0) then
    begin
      with Core.FApp do
      begin
        Result := FItems[FItems.Count -1];
      end;
    end else
      Result := nil;
  end;

  function CoreGetItems(aIndex: integer): PFileInfoExtra;
  begin
    if (Core <> nil) then
    begin
      if aIndex in [0.. Core.FApp.FItems.Count -1] then
        Result := Core.FApp.FItems[aIndex]
      else
        Result := nil;
    end else
      Result := nil;
  end;

end.