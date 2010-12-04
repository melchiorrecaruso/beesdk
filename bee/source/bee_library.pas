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

function CoreSend(ID, CODE, DATA: Pointer): Pointer;

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
    // procedure OnProgress; override;
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

procedure FreePChar(P: PChar);
begin
  if P <> nil then
  begin
    StrDispose(P);
  end;
end;

procedure FreePFileInfo(P: Pointer);
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
  FMessages := TList.Create;
  FItems    := TList.Create;
  FStatus   := csReady;
end;

destructor TCustomBeeApp.Destroy;
var
  I: longint;
begin
  for I := 0 to FMessages.Count - 1 do
    FreePChar(FMessages[I]);
  FMessages.Destroy;

  for I := 0 to FItems.Count - 1 do
    FreePFileInfo(FItems[I]);
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

{ Core GetInfo params value:      }

{   1: exit code      - longint   }
{   2: speed          - longint   }
{   3: percentage     - longint   }
{   4: elapsed time   - longint   }
{   5: remaining time - longint   }
{   6: processed size - int64     }
{   7: total size     - int64     }
{   8: message        - PChar     }
{   9: messages count - longint   }
{  10: item           - PFileInfo }
{  11: items count    - longint   }

function CoreSend(ID, CODE, DATA: Pointer): Pointer;
begin
  case longint(CODE) of
    0: Result := TCore.Create(PCharToString(DATA));    // CODE= 0 -> CREATE
    1: TCore(ID).Destroy;                              // CODE= 1 -> DESTROY
    2: TCore(ID).Resume;                               // CODE= 2 -> RESUME
    3: begin                                           // CODE= 3 -> TERMINATE
         TCore(ID).FApp.Terminate;
         if TCore(ID).FApp.FStatus <> csTerminated then
         begin
           TCore(ID).FApp.FStatus := csExecuting;
         end;
       end;
    4: begin                                           // CODE= 4 -> PRIORITY
         case longint(DATA) of
           cpIdle:         TCore(ID).Priority := tpIdle;
           cpLowest:       TCore(ID).Priority := tpLowest;
           cpLower:        TCore(ID).Priority := tpLower;
           cpNormal:       TCore(ID).Priority := tpNormal;
           cpHigher:       TCore(ID).Priority := tpHigher;
           cpHighest:      TCore(ID).Priority := tpHighest;
           cpTimeCritical: TCore(ID).Priority := tpTimeCritical;
         end;
         case TCore(ID).Priority of
           tpIdle:         Result := cpIdle;
           tpLowest:       Result := cpLowest;
           tpLower:        Result := cpLower;
           tpNormal:       Result := cpNormal;
           tpHigher:       Result := cpHigher;
           tpHighest:      Result := cpHighest;
           tpTimeCritical: Result := cpTimeCritical;
           else            Result := cpUnknow;
         end;
       end;

   10: Result := Pointer(TCore(ID).FApp.FStatus);      // CODE=10 -> STATUS
   11: Result := Pointer(TCore(ID).FApp.Code);         // CODE=11 -> EXIT CODE
   12: Result := Pointer(TCore(ID).FApp.Speed);        // CODE=12 -> SPEED
   13: Result := Pointer(TCore(ID).FApp.Percentage);   // CODE=13 -> PERCENTAGE
   14: Result := Pointer(TCore(ID).FApp.ElapsedTime);  // CODE=14 -> ETIME
   15: Result := Pointer(TCore(ID).FApp.RemainingTime);// CODE=15 -> RTIME
   16: Result := Pointer(TCore(ID).FApp.ProcessedSize);// CODE=16 -> PSIZE
   17: Result := Pointer(TCore(ID).FApp.Size);         // CODE=17 -> TSIZE
   18: Result := TCore(ID).FApp.FMessages[longint(DATA)];       // CODE=18 -> MESSAGE
   19: Result := Pointer(TCore(ID).FApp.FMessages.Count);// CODE=19 -> MCOUNT
   20: Result := TCore(ID).FApp.FItems[longint(DATA)];          // CODE=20 -> ITEM
   21: Result := Pointer(TCore(ID).FApp.FItems.Count);   // CODE=21 -> ICOUNT
  end;
end;

end.
