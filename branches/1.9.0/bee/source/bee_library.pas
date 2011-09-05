{
  Copyright (c) 2003-2011 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1280 - 2011.04.12 by Melchiorre Caruso.
}

unit Bee_Library;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
  {$IFDEF PLUGINS}
  BeeLib_Plugins,
  {$ENDIF}
  {$ENDIF}
  Bee_App,
  Bee_Types,
  Bee_Consts,
  Bee_Common,
  Bee_Headers,
  Bee_Interface,
  Bee_CommandLine;

{ Library routines }

function LibVersion: longint;

function CoreCreate(P: pchar): pointer;
function CoreExecute(HANDLE: pointer): boolean;
function CoreSuspend(HANDLE: pointer; VALUE: boolean): boolean;
function CoreTerminate(HANDLE: pointer): boolean;
function CoreDestroy(HANDLE: pointer): boolean;

function CoreGetStatus(HANDLE: pointer): longint;
function CoreGetCode(HANDLE: pointer): longint;
function CoreGetSpeed(HANDLE: pointer): longint;
function CoreGetPercentage(HANDLE: pointer): longint;
function CoreGetElapsedTime(HANDLE: pointer): longint;
function CoreGetRemainingTime(HANDLE: pointer): longint;

procedure CoreSetPriority(HANDLE: pointer; VALUE: longint);
function CoreGetPriority(HANDLE: pointer): longint;

function CoreGetSize(HANDLE: pointer): int64;
function CoreGetProcessedSize(HANDLE: pointer): int64;

function CoreGetMessage(HANDLE: pointer): pchar;
function CoreGetMessages(HANDLE: pointer; INDEX: longint): pchar;
function CoreGetMessageCount(HANDLE: pointer): longint;

function CoreGetItem(HANDLE: pointer): pointer;
function CoreGetItems(HANDLE: pointer; INDEX: longint): pointer;
function CoreGetItemCount(HANDLE: pointer): longint;

function CoreGetItemPending(HANDLE: pointer; STATUS: longint): pointer;
procedure CoreSetItemPending(HANDLE: pointer; STATUS: longint; VALUE: pointer);

implementation

type
  { TCustomBeeApp class }

  TCustomBeeApp = class(TBeeApp)
  private
    FStatus: longint;
    FMessages: TList;
    FItems:    TList;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure OnMessage(const aMessage: string); override;
    procedure OnRequest(const aMessage: string); override;
    function OnRename(const aItem: THeader; const aValue: string): string; override;
    procedure OnList(const aItem: THeader); override;
    procedure OnProgress; override;
    // procedure OnClear; override;
  end;

  { TCore class }

  TCore = class(TThread)
  private
    FApp: TCustomBeeApp;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  end;

{ Library routines }

function LibVersion: longint;
begin
  Result := 105;
end;

function THeaderToPFileInfo(aItem: THeader): PFileInfo;
begin
  GetMem(Result, SizeOf(TFileInfo));
  if Result <> nil then
  begin
    Result^.Name       := StringToPChar(aItem.Name);
    Result^.Size       := aItem.Size;
    Result^.Time       := aItem.Time;
    Result^.Attr       := aItem.Attr;
    Result^.PackedSize := aItem.PackedSize;

    Result^.Comm       := StringToPChar(aItem.Comment);
    Result^.Crc        := aItem.Crc;
    Result^.Method     := StringToPChar(MethodToStr(aItem));
    Result^.Version    := StringToPChar(VersionToStr(aItem.Version));

    if foPassword in aItem.Flags then
      Result^.Password := StringToPChar('yes')
    else
      Result^.Password := StringToPChar('no');
    Result^.Position   := aItem.Position;
  end;
end;

{ TCoreApp class }

constructor TCustomBeeApp.Create(const aCommandLine: string);
begin
  inherited Create(aCommandLine);
  FItems    := TList.Create;
  FMessages := TList.Create;
  FStatus   := csReady;
end;

destructor TCustomBeeApp.Destroy;
var
  I: longint;
begin
  for I := 0 to FMessages.Count - 1 do
    FreePChar(FMessages.Items[I]);
  FMessages.Destroy;

  for I := 0 to FItems.Count - 1 do
    FreePFileInfo(FItems.Items[I]);
  FItems.Destroy;
  inherited Destroy;
end;

procedure TCustomBeeApp.OnMessage(const aMessage: string);
begin
  FMessages.Add(StringToPChar(aMessage));
end;

function TCustomBeeApp.OnRename(const aItem: THeader; const aValue: string): string;
var
  I: longint;
begin
  FItems.Add(THeaderToPFileInfo(aItem));

  FStatus := csWaitRename;
  while FStatus = csWaitRename do
  begin
    Sleep(250);
  end;
  Result := PCharToString(PFileInfo(FItems.Last)^.Name);
end;

procedure TCustomBeeApp.OnRequest(const aMessage: string);
begin
  FMessages.Add(StringToPChar(aMessage));

  FStatus := csWaitRequest;
  while FStatus = csWaitRequest do
  begin
    Sleep(250);
  end;
end;

procedure TCustomBeeApp.OnList(const aItem: THeader);
begin
  FItems.Add(THeaderToPFileInfo(aItem));
  FMessages.Add(StringToPChar(Format(cmListing, [aItem.Name])));
end;

procedure TCustomBeeApp.OnProgress;
begin
  // nothing to do
end;

{ TCore class }

constructor TCore.Create(const aCommandLine: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  {$IFDEF FPC} {$IFDEF PLUGINS}
  if SevenZipPlugin(FCommandLine.ArchiveName) then
    FApp := TSevenZipApp.Create(aCommandLine)
  else
  {$ENDIF} {$ENDIF}
    FApp := TCustomBeeApp.Create(aCommandLine);
end;

destructor TCore.Destroy;
begin
  FApp.Destroy;
  inherited Destroy;
end;

procedure TCore.Execute;
begin
  FApp.FStatus := csExecuting;
  FApp.Execute;
  FApp.FStatus := csTerminated;
end;

{ Library core routines }

function CoreCreate(P: pchar): pointer;
begin
  try
    Result := TCore.Create(PCharToString(P));
  except
    Result := nil;
  end;
end;

function CoreExecute(HANDLE: pointer): boolean;
begin
  Result := Assigned(TCore(HANDLE));
  if Result then
  begin
    Result := TCore(HANDLE).FApp.FStatus = csReady;
    if Result then
    begin
      TCore(HANDLE).Resume;
    end;
  end;
end;

function CoreSuspend(HANDLE: pointer; VALUE: boolean): boolean;
begin
  Result := Assigned(TCore(HANDLE));
  if Result then
  begin
    TCore(HANDLE).FApp.Suspended := VALUE;
  end;
end;

function CoreTerminate(HANDLE: pointer): boolean;
begin
  Result := Assigned(TCore(HANDLE));
  if Result then
  begin
    TCore(HANDLE).FApp.Terminate;
    if TCore(HANDLE).FApp.FStatus <> csTerminated then
    begin
      TCore(HANDLE).FApp.FStatus := csExecuting;
    end;
  end;
end;

function CoreDestroy(HANDLE: pointer): boolean;
begin
  Result := Assigned(TCore(HANDLE));
  if Result then
  begin
    TCore(HANDLE).Destroy;
  end;
end;

function CoreGetStatus(HANDLE: pointer): longint;
begin
  Result := csUnknow;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FStatus;
  end;
end;

function CoreGetCode(HANDLE: pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.Code;
  end;
end;

function CoreGetSpeed(HANDLE: pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.Speed;
  end;
end;

function CoreGetPercentage(HANDLE: pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.Percentage;
  end;
end;

function CoreGetElapsedTime(HANDLE: pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.ElapsedTime;
  end;
end;

function CoreGetRemainingTime(HANDLE: pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.RemainingTime;
  end;
end;

procedure CoreSetPriority(HANDLE: pointer; VALUE: longint);
begin
  if Assigned(TCore(HANDLE)) then
  begin
    TCore(HANDLE).Priority := TThreadPriority(VALUE);
  end;
end;

function CoreGetPriority(HANDLE: pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := Ord(TCore(HANDLE).Priority);
  end;
end;

function CoreGetSize(HANDLE: pointer): int64;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FSize;
  end;
end;

function CoreGetProcessedSize(HANDLE: pointer): int64;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.ProcessedSize;
  end;
end;

function CoreGetMessage(HANDLE: pointer): pchar;
begin
  Result := nil;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FMessages.Last;
  end;
end;

function CoreGetMessages(HANDLE: pointer; INDEX: longint): pchar;
begin
  Result := nil;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FMessages[INDEX];
  end;
end;

function CoreGetMessageCount(HANDLE: pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FMessages.Count;
  end;
end;

function CoreGetItem(HANDLE: pointer): pointer;
begin
  Result := nil;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FItems.Last;
  end;
end;

function CoreGetItems(HANDLE: pointer; INDEX: longint): pointer;
begin
  Result := nil;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FItems[INDEX];
  end;
end;

function CoreGetItemCount(HANDLE: pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FItems.Count;
  end;
end;

function CoreGetItemPending(HANDLE: pointer; STATUS: longint): pointer;
begin
  Result := nil;
  if Assigned(TCore(HANDLE)) then
    case TCore(HANDLE).FApp.FStatus of
      csWaitOverwrite: Result := TCore(HANDLE).FApp.FItems.Last;
      csWaitRename:    Result := TCore(HANDLE).FApp.FItems.Last;
      csWaitPassword:  Result := TCore(HANDLE).FApp.FItems.Last;
      csWaitRequest:   Result := TCore(HANDLE).FApp.FItems.Last;
      csWaitList:      Result := TCore(HANDLE).FApp.FItems.Last;
    end;
end;

procedure CoreSetItemPending(HANDLE: pointer; STATUS: longint; VALUE: pointer);
begin
  if Assigned(TCore(HANDLE)) then
    case TCore(HANDLE).FApp.FStatus of
      // csWaitOverwrite:
      // csWaitPassword:
      // csWaitRequest:
      // csWaitList:
      csWaitRename: begin
        with PFileInfo(TCore(HANDLE).FApp.FItems.Last)^ do
        begin
          FreePChar(Name);
          Name := StrNew(VALUE);
        end;
        TCore(HANDLE).FApp.FStatus := csExecuting;
      end;
    end;
end;

end.
