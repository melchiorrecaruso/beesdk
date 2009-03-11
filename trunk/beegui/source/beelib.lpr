{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

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
  // ---
  Bee_App,
  Bee_Interface;

type
  TCore = class(TThread)
  private
    FApp: TApp;
    FKey: string;
    FParams: TParams;
  private
    procedure ProcessFatalError(const aMessage: string);
    procedure ProcessError     (const aMessage: string);
    procedure ProcessWarning   (const aMessage: string);
    procedure ProcessMessage   (const aMessage: string);
    procedure ProcessOverwrite (const aFileInfo: TFileInfoRec; var Result: char);
    procedure ProcessRename    (const aFileInfo: TFileInfoRec; var Result: string);
    procedure ProcessList      (const aFileInfo: TFileFullInfoRec);
    procedure ProcessKey       (const aFileInfo: TFileInfoRec; var Result: string);
    procedure ProcessRequest   (const aMessage: string);
    procedure ProcessTick;
    procedure ProcessClear;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Suspend;
    procedure Resume;
    procedure Terminate;
  end;

  /// --- ///

  constructor TCore.Create(const aCommandLine: string);
  begin
    inherited Create(True);
    FKey := '';
    FParams := TParams.Create;
    FParams.Text := aCommandLine;

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
    FKey := '';
    FParams.Destroy;
    inherited Destroy;
  end;

  procedure TCore.Execute;
  begin
    FApp.Execute;
  end;

  procedure TCore.Suspend;
  begin
    FApp.Suspended := True;
  end;

  procedure TCore.Resume;
  begin
    FApp.Suspended := False;
  end;

  procedure TCore.Terminate;
  begin
    FApp.Terminated:= True;
  end;

  procedure TCore.ProcessFatalError(const aMessage: string);
  begin

  end;

  procedure TCore.ProcessError(const aMessage: string);
  begin

  end;

  procedure TCore.ProcessWarning(const aMessage: string);
  begin

  end;

  procedure TCore.ProcessMessage(const aMessage: string);
  begin

  end;

  procedure TCore.ProcessOverwrite(const aFileInfo: TFileInfoRec; var Result: char);
  begin

  end;

  procedure TCore.ProcessRename(const aFileInfo: TFileInfoRec; var Result: string);
  begin

  end;

  procedure TCore.ProcessList(const aFileInfo: TFileFullInfoRec);
  begin

  end;

  procedure TCore.ProcessKey(const aFileInfo: TFileInfoRec; var Result: string);
  begin

  end;

  procedure TCore.ProcessRequest(const aMessage: string);
  begin

  end;

  procedure TCore.ProcessTick;
  begin

  end;

  procedure TCore.ProcessClear;
  begin

  end;



//
//
//
//
//

function CreateCore(const ACommandLine: string): pointer;
begin
  Result := TCore.Create(ACommandLine);
end;

procedure ExecuteCore(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Execute;
  end;
end;

procedure DestroyCore(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Destroy;
  end;
end;

procedure SuspendCore(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Suspend;
  end;
end;

procedure ResumeCore(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Resume;
  end;
end;

procedure TerminateCore(ID: pointer);
begin
  if Assigned(TCore(ID)) then
  begin
    TCore(ID).Terminate;
  end;
end;

function PercentesCore(ID: pointer): integer;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FApp.Percentes;
  end;
end;

function SpeedCore(ID: pointer): integer;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FApp.Speed;
  end;
end;

function TotalSizeCore(ID: pointer): int64;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FApp.TotalSize;
  end;
end;

function ProcessedSizeCore(ID: pointer): int64;
begin
  if Assigned(TCore(ID)) then
  begin
    Result := TCore(ID).FApp.ProcessedSize;
  end;
end;


exports CreateCore;
exports ExecuteCore;
exports DestroyCore;
exports SuspendCore;
exports ResumeCore;
exports TerminateCore;

exports PercentesCore;
exports SpeedCore;
exports TotalSizeCore;
exports ProcessedSizeCore;

begin

end.

