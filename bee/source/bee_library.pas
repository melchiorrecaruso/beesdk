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

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
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
  Bee_Headers,
  Bee_Interface,
  Bee_CommandLine;

{ Library routines }

function CoreVersion: longint;
function CoreCreate(P: PChar): Pointer;
function CoreGetBool8(ID: Pointer; MESSAGE: longint): boolean;
function CoreGetInt32(ID: Pointer; MESSAGE: longint): longint;
function CoreGetInt64(ID: Pointer; MESSAGE: longint): Int64;
function CoreGetPointer(ID: Pointer; MESSAGE, INDEX: longint): Pointer;
function CoreSetPointer(ID: Pointer; MESSAGE: longint; P: Pointer): boolean;

implementation

type
  { TCustomBeeApp class }

  TCustomBeeApp = class(TBeeApp)
  private
    FItems:    TList;
    FMessages: TList;
    FStatus: longint;
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure OnMessage(const aMessage: string); override;
    procedure OnRequest(const aMessage: string); override;
    function  OnRename(const aItem: THeader; const aValue: string): string; override;
    procedure OnList(const aItem: THeader); override;
    procedure OnProgress; override;
    // procedure OnClear; override;
  end;

  { TCore class }

  TCore = class(TThread)
  private
    FApp:  TCustomBeeApp;
    FParams: TStringList;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  end;

{ Library routines }

function THeaderToPFileInfo(aItem: THeader): PFileInfo;
begin
  Result := GetMem(SizeOf(TFileInfo));
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
      Result^.Password   := StringToPChar('yes')
    else
      Result^.Password   := StringToPChar('no');
    Result^.Position   := aItem.Position;
  end;
end;

{ TCoreApp class }

constructor TCustomBeeApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  FMessages := TList.Create;
  FItems    := TList.Create;
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

  FStatus  := csWaitingRename;
  while FStatus = csWaitingRename do
  begin
    Sleep(250);
  end;
  Result := PCharToString(PFileInfo(FItems.Last).Name);
end;

procedure TCustomBeeApp.OnRequest(const aMessage: string);
begin
  FMessages.Add(StringToPChar(aMessage));

  FStatus  := csWaitingRequest;
  while FStatus = csWaitingRequest do
  begin
    Sleep(250);
  end;
end;

procedure TCustomBeeApp.OnProgress;
begin
  // nothing to do
end;

procedure TCustomBeeApp.OnList(const aItem: THeader);
begin
  FItems.Add(THeaderToPFileInfo(aItem));
  FMessages.Add(StringToPChar(Format(cmListing, [aItem.Name])));
end;

{ TCore class }

constructor TCore.Create(const aCommandLine: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;

  FParams := TStringList.Create;
  FParams.Text := aCommandLine;
  {$IFDEF FPC} {$IFDEF PLUGINS}
  if SevenZipPlugin(FCommandLine.ArchiveName) then
    FApp := TSevenZipApp.Create(FParams)
  else
  {$ENDIF} {$ENDIF}
    FApp := TCustomBeeApp.Create(FParams);
  FParams.Destroy;
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

function CoreVersion: longint;
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

function CoreGetBool8(ID: Pointer; MESSAGE: longint): boolean;
begin
  Result := Assigned(TCore(ID));
  if Result then
  begin
    case MESSAGE of
      csmExecute: begin
        Result := TCore(ID).FApp.FStatus = csReady;
        if Result then
        begin
          TCore(ID).Resume;
        end;
      end;
      csmSuspend: TCore(ID).FApp.Suspended := True;
      csmResume:  TCore(ID).FApp.Suspended := False;
      csmTerminate: begin
        TCore(ID).FApp.Terminate;
        if TCore(ID).FApp.FStatus <> csTerminated then
        begin
          TCore(ID).FApp.FStatus := csExecuting;
        end;
      end;
      csmDestroy: TCore(ID).Destroy;
      else Result := False;
    end;
  end;
end;

function CoreGetInt32(ID: Pointer; MESSAGE: longint): longint;
begin
  Result := -1;
  if Assigned(TCore(ID)) then
  begin
    case MESSAGE of
      csmStatus:        Result := TCore(ID).FApp.FStatus;
      csmCode:          Result := TCore(ID).FApp.Code;
      csmSpeed:         Result := TCore(ID).FApp.Speed;
      csmPercentage:    Result := TCore(ID).FApp.Percentage;
      csmElapsedTime:   Result := TCore(ID).FApp.ElapsedTime;
      csmRemainingTime: Result := TCore(ID).FApp.RemainingTime;

      csmPriority:      Result := Ord(TCore(ID).Priority);
      csmPriorityIdle:  begin
        TCore(ID).Priority := tpIdle;
        Result := Ord(TCore(ID).Priority);
      end;
      csmPriorityLowest: begin
        TCore(ID).Priority := tpLowest;
        Result := Ord(TCore(ID).Priority);
      end;
      csmPriorityLower: begin
        TCore(ID).Priority := tpLower;
        Result := Ord(TCore(ID).Priority);
      end;
      csmPriorityNormal: begin
        TCore(ID).Priority := tpNormal;
        Result := Ord(TCore(ID).Priority);
      end;
      csmPriorityHigher: begin
        TCore(ID).Priority := tpHigher;
        Result := Ord(TCore(ID).Priority);
      end;
      csmPriorityHighest: begin
        TCore(ID).Priority := tpHighest;
        Result := Ord(TCore(ID).Priority);
      end;
      csmPriorityTimeCritical: begin
        TCore(ID).Priority := tpTimeCritical;
        Result := Ord(TCore(ID).Priority);
      end;
    end;
  end;
end;

function CoreGetInt64(ID: Pointer; MESSAGE: longint): Int64;
begin
  Result := -1;
  if Assigned(TCore(ID)) then
  begin
    case MESSAGE of
      csmSize:          Result := TCore(ID).FApp.FSize;
      csmProcessedSize: Result := TCore(ID).FApp.ProcessedSize;
    end;
  end;
end;

function CoreGetPointer(ID: Pointer; MESSAGE, INDEX: longint): Pointer;
begin
  Result := nil;
  if Assigned(TCore(ID)) then
  begin
    case MESSAGE of
      csmMessage: begin
        if (INDEX = -1) then INDEX := TCore(ID).FApp.FMessages.Count - 1;
        if (INDEX > -1) and (INDEX <  TCore(ID).FApp.FMessages.Count) then
        begin
          Result := TCore(ID).FApp.FMessages[INDEX];
        end;
      end;
      csmItem: begin
        if (INDEX = -1) then INDEX := TCore(ID).FApp.FItems.Count - 1;
        if (INDEX > -1) and (INDEX <  TCore(ID).FApp.FItems.Count) then
        begin
          Result := TCore(ID).FApp.FItems[INDEX];
        end;
      end;
    end;
  end;
end;

function CoreSetPointer(ID: Pointer; MESSAGE: longint; P: Pointer): boolean;
begin
  Result := Assigned(TCore(ID));
  if Result then
  begin
    case MESSAGE of
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
