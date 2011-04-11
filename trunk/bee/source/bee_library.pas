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

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
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
function CoreCreate(P: PChar): Pointer;
function CorePriority(ID:Pointer; VALUE: longint): longint;
function CoreQueryB8(ID: Pointer; VALUE: longint): boolean;
function CoreQueryI32(ID: Pointer; VALUE: longint): longint;
function CoreQueryI64(ID: Pointer; VALUE: longint): Int64;
function CoreQueryPointer(ID: Pointer; VALUE, INDEX: longint): Pointer;
function CoreReplyPointer(ID: Pointer; VALUE: longint; P: Pointer): boolean;

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

  FStatus := csWaitingRename;
  while FStatus = csWaitingRename do
  begin
    Sleep(250);
  end;
  Result := PCharToString(PFileInfo(FItems.Last)^.Name);
end;

procedure TCustomBeeApp.OnRequest(const aMessage: string);
begin
  FMessages.Add(StringToPChar(aMessage));

  FStatus := csWaitingRequest;
  while FStatus = csWaitingRequest do
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

function LibVersion: longint;
begin
  Result := 105;
end;

function CoreCreate(P: PChar): Pointer;
begin
  try
    Result := TCore.Create(PCharToString(P));
  except
    Result := nil;
  end;
end;

function CoreDestroy(HANDLE: Pointer): boolean;
begin
  Result := Assigned(TCore(HANDLE));
  if Result then
  begin
    TCore(HANDLE).Destroy;
  end;
end;

function CoreSetPriority(HANDLE: Pointer; VALUE: longint): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    case VALUE of
      csmPriorityIdle:         TCore(HANDLE).Priority := tpIdle;
      csmPriorityLowest:       TCore(HANDLE).Priority := tpLowest;
      csmPriorityLower:        TCore(HANDLE).Priority := tpLower;
      csmPriorityNormal:       TCore(HANDLE).Priority := tpNormal;
      csmPriorityHigher:       TCore(HANDLE).Priority := tpHigher;
      csmPriorityHighest:      TCore(HANDLE).Priority := tpHighest;
      csmPriorityTimeCritical: TCore(HANDLE).Priority := tpTimeCritical;
    end;
    Result := Ord(TCore(HANDLE).Priority);
  end;
end;

function CoreGetPriority(HANDLE: Pointer; VALUE: longint): longint;
begin
  Result := CoreSetPriority(HANDLE, VALUE);
end;

function CoreExecute(HANDLE: Pointer): boolean;
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

function CoreResume(HANDLE: Pointer): boolean;
begin
  Result := Assigned(TCore(HANDLE));
  if Result then
  begin
    TCore(HANDLE).FApp.Suspended := False;
  end;
end;

function CoreSuspend(HANDLE: Pointer): boolean;
begin
  Result := Assigned(TCore(HANDLE));
  if Result then
  begin
    TCore(HANDLE).FApp.Suspended := True;
  end;
end;

function CoreTerminate(HANDLE: Pointer): boolean;
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

function CoreGetStatus(HANDLE: Pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FStatus;
  end;
end;

function CoreGetCode(HANDLE: Pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FCode;
  end;
end;

function CoreGetSpeed(HANDLE: Pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.Speed;
  end;
end;

function CoreGetPercentage(HANDLE: Pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.Percentage;
  end;
end;

function CoreGetElapsedTime(HANDLE: Pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.ElapsedTime;
  end;
end;

function CoreGetRemainingTime(HANDLE: Pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.RemainingTime;
  end;
end;

function CoreGetDataSize(HANDLE: Pointer): Int64;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FSize;
  end;
end;

function CoreGetDataProcessedSize(HANDLE: Pointer): Int64;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.ProcessedSize;
  end;
end;

function CoreGetMessageCount(HANDLE: Pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FMessages.Count;
  end;
end;

function CoreGetLastMessage(HANDLE: Pointer): PChar;
var
  I: longint;
begin
  Result := nil;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FMessages.Last;
  end;
end;

function CoreGetMessage(HANDLE: Pointer; INDEX: longint): PChar;
var
  I: longint;
begin
  Result := nil;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FMessages[INDEX];
  end;
end;

function CoreGetItemCount(HANDLE: Pointer): longint;
begin
  Result := -1;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FItems.Count;
  end;
end;

function CoreGetItem(HANDLE: Pointer; INDEX: longint): PChar;
var
  I: longint;
begin
  Result := nil;
  if Assigned(TCore(HANDLE)) then
  begin
    Result := TCore(HANDLE).FApp.FItems[INDEX];
  end;
end;













function CoreReplyPointer(ID: Pointer; VALUE: longint; P: Pointer): boolean;
begin
  Result := Assigned(TCore(ID));
  if Result then
  begin
    case VALUE of
      csmWaitingRename: begin
        FreePChar(PFileInfo(TCore(ID).FApp.FItems.Last)^.Name);
        PFileInfo(TCore(ID).FApp.FItems.Last)^.Name := StrNew(P);
        TCore(ID).FApp.FStatus := csExecuting;
      end;
      else Result := False;
    end;
  end;
end;

end.
