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
function CoreLibVersion: integer;

procedure CoreFreePChar(P: PChar);
procedure CoreFreePFileInfo(P: Pointer);

{ Library core routines }

function CoreCreate(aCommandLine: PChar): boolean;
function CoreDestroy: boolean;
function CoreExecute: boolean;
function CoreTerminate: boolean;
function CorePriority(aValue: longint): longint;
function CoreSuspended(aValue: boolean): boolean;

function CoreGetItem(aIndex: integer): Pointer;
function CoreGetMessage(aIndex: integer): PChar;
function CorePutMessage(aValue: PChar): PChar;

{ Core GetInfo param value:                             }
{    0..MaxInt: items                                   }
{   -1: speed                                           }
{   -2: percentage                                      }
{   -3: status                                          }
{   -4: exit code                                       }
{   -5: elapsed time                                    }
{   -6: remaining time                                  }
{   -7: processed size                                  }
{   -8: total size                                      }

function CoreGetInfo(aValue: longint): Pointer;

implementation

type
  { TCustomBeeApp class }

  TCustomBeeApp = class(TBeeApp)
  private
    FMessages: TStringList;
    FItems:    TList;
    FStatus:   integer;
    function GetItem(Index: longint): PFileInfo;
    function GetMessage(Index: longint): PChar;
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure OnMessage(const aMessage: string); override;
    procedure OnRequest(const aMessage: string); override;
    function  OnRename(const aItem: THeader; const aValue: string): string; override;
    procedure OnList(const aItem: THeader); override;
    // procedure OnProgress; override;
    // procedure OnClear; override;
    property Items[Index: longint]: PFileInfo read GetItem;
    property Messages[Index: longint]: PChar read GetMessage;
  end;

  { TCore class }

  TCore = class(TThread)
  private
    FApp:    TCustomBeeApp;
    FParams: TStringList;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  end;

var
  Core: TCore = nil;

function THeaderToPFileInfo(aItem: THeader): PFileInfo;
begin
  Result := GetMem(SizeOf(TFileInfo));
  if Result <> nil then
  begin
    Result^.Name       := StringToPChar(ExtractFileName(aItem.Name));
    Result^.Path       := StringToPChar(ExtractFilePath(aItem.Name));
    Result^.Size       := aItem.Size;
    Result^.Time       := aItem.Time;
    Result^.Attr       := aItem.Attr;
    Result^.PackedSize := aItem.PackedSize;

    if aItem.Size > 0 then
      Result^.Ratio    := Trunc(100 * (aItem.Size / aItem.PackedSize))
    else
      Result^.Ratio    := 0;

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
  FMessages := TStringList.Create;
  FItems    := TList.Create;
  FStatus   := csReady;
end;

destructor TCustomBeeApp.Destroy;
var
  I: longint;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    CoreFreePFileInfo(FItems[I]);
  end;
  FItems.Destroy;
  FMessages.Destroy;
  inherited Destroy;
end;

procedure TCustomBeeApp.OnMessage(const aMessage: string);
begin
  FMessages.Add(aMessage);
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
  Result := FMessage;
end;

procedure TCustomBeeApp.OnRequest(const aMessage: string);
begin
  FMessage := aMessage;
  FStatus  := csWaitingRequest;
  while FStatus = csWaitingRequest do
  begin
    Sleep(250);
  end;
end;

procedure TCustomBeeApp.OnList(const aItem: THeader);
begin
  FItems.Add(THeaderToPFileInfo(aItem));
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
    FApp := TCoreApp.Create(FParams);
  FParams.Destroy;
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
  Result := 104;
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
      if Name     <> nil then StrDispose(Name);
      if Path     <> nil then StrDispose(Path);
      if Comm     <> nil then StrDispose(Comm);
      if Method   <> nil then StrDispose(Method);
      if Version  <> nil then StrDispose(Version);
      if Password <> nil then StrDispose(Password);
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
    Core.FApp.Terminate;
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
      cpIdle:         Core.Priority := tpIdle;
      cpLowest:       Core.Priority := tpLowest;
      cpLower:        Core.Priority := tpLower;
      cpNormal:       Core.Priority := tpNormal;
      cpHigher:       Core.Priority := tpHigher;
      cpHighest:      Core.Priority := tpHighest;
      cpTimeCritical: Core.Priority := tpTimeCritical;
    end;

    case Core.Priority of
      tpIdle:         Result := cpIdle;
      tpLowest:       Result := cpLowest;
      tpLower:        Result := cpLower;
      tpNormal:       Result := cpNormal;
      tpHigher:       Result := cpHigher;
      tpHighest:      Result := cpHighest;
      tpTimeCritical: Result := cpTimeCritical;
      else            Result := cpUnknow;
    end;
  end else
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
    if aIndex = -1 then aIndex := Core.FApp.FMessages.Count;




    if aIndex > -1 then
    begin
      if aIndex < Core.FApp.FMessages.Count then
        Result := StringToPChar(Core.FApp.FMessages[aIndex])
      else
        Result := nil;
    end else
      Result := StringToPChar(Core.FApp.FMessage)
  end;
end;

function CorePercentes: integer;
begin
  Result := -1;
  if (Core <> nil) then
  begin
    Result := Core.FApp.Percentage
  end;
end;

function CoreTime(aValue: integer): integer;
begin
  Result := -1;
  if (Core <> nil) then
  begin
    case aValue of
      -1: Result := Core.FApp.ElapsedTime;
      +1: Result := Core.FApp.RemainingTime;
    end;
  end;
end;

function CoreSize(aValue: integer): int64;
begin
  Result := -1;
  if (Core <> nil) then
  begin
    case aValue of
      -1: Result := Core.FApp.ProcessedSize;
      +1: Result := Core.FApp.Size;
    end;
  end;
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
    end else
      Result := StringToPChar(Core.FApp.FMessage);
  end else
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
