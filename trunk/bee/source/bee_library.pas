{
  Copyright (c) 2003-2010 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1100 - 2010.01.23 by Melchiorre Caruso.
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

{ Library routines }
function CoreLibVersion: integer;

procedure CoreFreePChar(P: PChar);
procedure CoreFreePFileInfo(P: Pointer);
procedure CoreFreePFileInfoExtra(P: Pointer);

{ Library core routines }
function CoreCreate(aCommandLine: PChar): boolean;
function CoreDestroy: boolean;
function CoreExecute: boolean;
function CoreTerminate: boolean;
function CorePriority(aValue: integer): integer;
function CoreSuspend(aValue: boolean): boolean;

function CoreRequest(aValue: PChar): PChar;
function CoreMessages(aIndex: integer): PChar;
function CoreTime(aValue: integer): integer;
function CoreSize(aValue: integer): int64;

function CoreSpeed: integer;
function CorePercentes: integer;
function CoreStatus: integer;
function CoreCode: integer;

function CoreItems(aIndex: integer): Pointer;

implementation

type
  { TCoreApp class }

  TCoreApp = class(TBeeApp)
  private
    FItems:    TList;
    FItem:     TFileInfo;
    FMessages: TStringList;
    FMessage:  string;
    FPassword: string;
    FStatus:   integer;
  protected
    procedure ClearItems;
    procedure AddItem(aFileInfo: TFileInfoExtra);
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;

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

  { TCore class }

  TCore = class(TThread)
  private
    FApp:    TCoreApp;
    FParams: TStringList;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  end;

var
  Core: TCore = nil;

{ TCoreApp class }

constructor TCoreApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  FMessages := TStringList.Create;
  FItems    := TList.Create;

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
  for I := 0 to FItems.Count - 1 do
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
    P^.FileSize     := aFileInfo.FileSize;
    P^.FileTime     := aFileInfo.FileTime;
    P^.FileAttr     := aFileInfo.FileAttr;
    P^.FilePacked   := aFileInfo.FilePacked;
    P^.FileRatio    := aFileInfo.FileRatio;
    P^.FileComm     := StrNew(aFileInfo.FileComm);
    P^.FileCrc      := aFileInfo.FileCrc;
    P^.FileMethod   := StrNew(aFileInfo.FileMethod);
    P^.FileVersion  := StrNew(aFileInfo.FileVersion);
    P^.FilePassword := StrNew(aFileInfo.FilePassword);
    P^.FilePosition := aFileInfo.FilePosition;
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

function TCoreApp.OnOverWrite(const aFileInfo: TFileInfo;
  const aValue: string): string;
begin
  FMessage := aValue;
  FItem    := aFileInfo;
  FStatus  := csWaitingOverwrite;
  while FStatus = csWaitingOverwrite do
    Sleep(250);

  if Length(FMessage) = 1 then
    Result := FMessage[1];
end;

function TCoreApp.OnRename(const aFileInfo: TFileInfo; const aValue: string): string;
begin
  FMessage := aValue;
  FItem    := aFileInfo;
  FStatus  := csWaitingRename;
  while FStatus = csWaitingRename do
    Sleep(250);

  Result := FMessage;
end;

function TCoreApp.OnPassword(const aFileInfo: TFileInfo; const aValue: string): string;
begin
  FMessage := aValue;
  FItem    := aFileInfo;
  FStatus  := csWaitingPassword;
  while FStatus = csWaitingPassword do
    Sleep(250);

  Result := FMessage;
end;

procedure TCoreApp.OnRequest(const aMessage: string);
begin
  FMessage := aMessage;
  FStatus  := csWaitingRequest;
  while FStatus = csWaitingRequest do
    Sleep(250);
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

{ TCore class }

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

{ Library routines }

function CoreLibVersion: integer;
begin
  Result := 103;
end;

procedure CoreFreePChar(P: PChar);
begin
  if P <> nil then
  begin
    StrDispose(P);
  end;
end;

procedure CoreFreePFileInfo(P: Pointer);
begin
  if P <> nil then
    with TFileInfo(P^) do
    begin
      if FileName <> nil then
        StrDispose(FileName);
      if FilePath <> nil then
        StrDispose(FilePath);
    end;
end;

procedure CoreFreePFileInfoExtra(P: Pointer);
begin
  if P <> nil then
    with TFileInfoExtra(P^) do
    begin
      if FileName <> nil then
        StrDispose(FileName);
      if FilePath <> nil then
        StrDispose(FilePath);
      if FileComm <> nil then
        StrDispose(FileComm);
      if FileMethod <> nil then
        StrDispose(FileMethod);
      if FileVersion <> nil then
        StrDispose(FileVersion);
      if FilePassword <> nil then
        StrDispose(FilePassword);
    end;
end;

{ Library core routines }

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

function CorePriority(aValue: integer): integer;
begin
  if Core <> nil then
  begin
    case aValue of
      cpIdle: Core.Priority    := tpIdle;
      cpLowest: Core.Priority  := tpLowest;
      cpLower: Core.Priority   := tpLower;
      cpNormal: Core.Priority  := tpNormal;
      cpHigher: Core.Priority  := tpHigher;
      cpHighest: Core.Priority := tpHighest;
      cpTimeCritical: Core.Priority := tpTimeCritical;
    end;

    case Core.Priority of
      tpIdle: Result    := cpIdle;
      tpLowest: Result  := cpLowest;
      tpLower: Result   := cpLower;
      tpNormal: Result  := cpNormal;
      tpHigher: Result  := cpHigher;
      tpHighest: Result := cpHighest;
      tpTimeCritical: Result := cpTimeCritical;
      else
        Result := cpUnknow;
    end;
  end
  else
    Result := cpUnknow;
end;

function CoreSpeed: integer;
begin
  if (Core <> nil) then
    Result := Core.FApp.Speed
  else
    Result := -1;
end;

function CoreMessages(aIndex: integer): PChar;
begin
  if (Core <> nil) then
  begin
    if aIndex < 0 then
      Result := StringToPChar(Core.FApp.FMessage)
    else
    begin
      if aIndex < Core.FApp.FMessages.Count then
        Result := StringToPChar(Core.FApp.FMessages[aIndex])
      else
        Result := nil;
    end;
  end;
end;

function CorePercentes: integer;
begin
  if (Core <> nil) then
    Result := Core.FApp.Percentes
  else
    Result := -1;
end;

function CoreTime(aValue: integer): integer;
begin
  if (Core <> nil) then
  begin
    case aValue of
      -1: Result := Core.FApp.Time;
      +1: Result := Core.FApp.TotalTime;
      else
        Result := -1;
    end;
  end
  else
    Result := -1;
end;

function CoreSize(aValue: integer): int64;
begin
  if (Core <> nil) then
  begin
    case aValue of
      -1: Result := Core.FApp.Size;
      +1: Result := Core.FApp.TotalSize;
      else
        Result := -1;
    end;
  end
  else
    Result := -1;
end;

function CoreCode: integer;
begin
  if (Core <> nil) then
    Result := Core.FApp.Code
  else
    Result := ccUnknow;
end;

function CoreStatus: integer;
begin
  if (Core <> nil) then
    Result := Core.FApp.FStatus
  else
    Result := csUnknow;
end;

function CoreRequest(aValue: PChar): PChar;
begin
  if (Core <> nil) then
  begin
    if aValue <> nil then
    begin
      Core.FApp.FMessage := PCharToString(aValue);
      Core.FApp.FStatus  := csExecuting;
    end
    else
      Result := StringToPChar(Core.FApp.FMessage);
  end
  else
    Result := nil;
end;

function CoreItems(aIndex: integer): Pointer;
begin
  if (Core <> nil) then
  begin
    if aIndex < 0 then
      Result := @Core.FApp.FItem
    else
    begin
      if aIndex < Core.FApp.FItems.Count then
        Result := Core.FApp.FItems[aIndex]
      else
        Result := nil;
    end;
  end
  else
    Result := nil;
end;

end.
