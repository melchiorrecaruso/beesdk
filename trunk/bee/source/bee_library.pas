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

function CoreSend(var ID: PtrInt;  MESSAGE, DATA: PtrInt ): PtrInt;

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

function CoreSend(var ID: PtrInt;  MESSAGE, DATA: PtrInt ): PtrInt;
var
  P: PChar;
begin
  case longint(MESSAGE) of
    csmVersion:       Result := PtrInt(105);
    csmCreate:        begin
      Result := TCore.Create(PCharToString(DATA));
    end;
    csmDestroy:       begin
      if Assigned(TCore(ID)) then
      begin
        TCore(ID).Destroy;
      end;
      Result := nil;
    end;
    csmResume:        begin
      Result := Pointer(Assigned(TCore(ID)) and (TCore(ID).FApp.FStatus = csReady));
      if boolean(Result) then
      begin
        TCore(ID).Resume;
      end;
    end;
    csmSuspend:       begin
      Result := Pointer(Assigned(TCore(ID)));
      if boolean(Result) then
      begin
        TCore(ID).FApp.Suspended := boolean(DATA);
      end;
    end;
    csmTerminate:     begin
      Result := Pointer(Assigned(TCore(ID)));
      if boolean(Result) then
      begin
        TCore(ID).FApp.Terminate;
        if TCore(ID).FApp.FStatus <> csTerminated then
        begin
          TCore(ID).FApp.FStatus := csExecuting;
        end;
      end;
    end;
    csmPriority:      begin
      Result := Pointer(Assigned(TCore(ID)));
      if boolean(Result) then
      begin
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
          else            Result := -1;
        end;
      end;
    end;
    csmStatus:        begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.FStatus);
        False: Result := Pointer(-1);
      end;
    end;
    csmCode:          begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.Code);
        False: Result := Pointer(-1);
      end;
    end;
    csmSpeed:         begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.Speed);
        False: Result := Pointer(-1);
      end;
    end;
    csmPercentage:    begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.Percentage);
        False: Result := Pointer(-1);
      end;
    end;
    csmElapsedTime:   begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.ElapsedTime);
        False: Result := Pointer(-1);
      end;
    end;
    csmRemainingTime: begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.RemainingTime);
        False: Result := Pointer(-1);
      end;
    end;
    csmProcessedSize: begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.ProcessedSize);
        False: Result := Pointer(-1);
      end;
    end;
    csmSize:          begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.Size);
        False: Result := Pointer(-1);
      End;
    end;
    csmMessage:       begin
      case Assigned(TCore(ID)) of
        True:  Result := TCore(ID).FApp.FMessages[longint(DATA) - 1];
        False: Result := nil;
      end;
    end;
    csmMessageCount:  begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.FMessages.Count);
        False: Result := Pointer(-1);
      end;
    end;
    csmItem:          begin
      case Assigned(TCore(ID)) of
        True:  Result := TCore(ID).FApp.FItems[longint(DATA) - 1];
        False: Result := nil;
      end;
    end;
    csmItemCount:     begin
      case Assigned(TCore(ID)) of
        True:  Result := Pointer(TCore(ID).FApp.FItems.Count);
        False: Result := nil;
      end;
    end;

    csmItemName:      begin
      Result := Pointer(Assigned(TCore(ID)));
      if boolean(Result) then
      begin
        P := PFileInfo(TCore(ID).FApp.FItems[longint(DATA)])^.Name;
        FreePChar(P);

        P := StrNew(PChar(DATA));
        TCore(ID).FApp.FStatus := csExecuting;
      end;
    end;
  end;
end;

end.
