{
  Copyright (c) 2005-2009 Andrew Filinsky and Melchiorre Caruso

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

    TApp, abstract class of Thread.

  Modifyed:

    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

    v0.7.9 build 0970 - 2009.03.10 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes;

type
  // TMessageRec packed record

  TMessageRec = record
    Method: TThreadMethod;
    Data: record
      Msg: string;
    end;
    Answer: string;
  end;

  // TFileQueryRec packed record

  TFileQueryRec = record
    Method: TThreadMethod;
    Data: record
      FileName: string;
      FilePath: string;
      FileTime: integer;
      FileSize: cardinal;
      FileAttr: integer;
    end;
    Answer: string;
  end;

  // TFileListRec packed record

  TFileListRec = record
    Method: TThreadMethod;
    Data: record
      FileName: string;
      FilePath: string;
      FileSize: cardinal;
      FilePacked: cardinal;
      FileRatio: cardinal;
      FileAttr: integer;
      FileTime: integer;
      FileComm: string;
      FileCrc: cardinal;
      FileMethod: string;
      FileVersion: string;
      FilePassword: string;
      FilePosition: cardinal;
    end;
    Answer: string;
  end;

  // TTickRec packed record

  TTickRec = record
    Method: TThreadMethod;
    Data:  record
      TotalSize: int64;
      ProcessedSize: int64;
      Percentage: integer;
      Speed: integer;
    end;
    Answer: string;
  end;

  TClearRec = record
    Method: TThreadMethod;
  end;

  // TStatusRec packed record

  TStatusRec = record
    Terminated: boolean;
    Suspended: boolean;
    Stop: boolean;
  end;

  // TInterfacesRec packed record

  TInterfacesRec = record
    OnFatalError: TMessageRec;
    OnError: TMessageRec;
    OnWarning: TMessageRec;
    OnDisplay: TMessageRec;
    OnRequest: TMessageRec;
    OnFileOverWrite: TFileQueryRec;
    OnFileRename: TFileQueryRec;
    OnFileList: TFileListRec;
    OnFileKey: TFileQueryRec;
    OnTick: TTickRec;
    OnClear: TClearRec;
    Status: TStatusRec;
  end;

  PInterfaces = ^TInterfacesRec;

  // TParams ...

  TParams = TStringList;

  // TSynchronizer ...

  TSynchronizer = procedure(aMethod: TThreadMethod) of object;

  // TApp class ...

  TApp = class(TThread)
  protected
    Interfaces: PInterfaces;
  protected
    procedure DoTerminate; override;
    procedure SetExitCode(Code: integer);
    procedure SetPriority(APriority: integer); overload;
  public
    constructor Create(aInterfaces: PInterfaces);
    procedure Synchronize(aMethod: TThreadMethod);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

{$IFDEF CONSOLEAPPLICATION}
{$IFDEF MSWINDOWS}
uses
  Bee_Common;
{$ENDIF}
{$ENDIF}

// TApp class ...

constructor TApp.Create(aInterfaces: PInterfaces);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpNormal;
  // Store interfaces
  Interfaces := aInterfaces;
  // Initialize interfaces status
  with Interfaces.Status do
  begin
    Terminated := False;
    Suspended  := False;
    Stop       := False;
  end;
  // Initialize interfaces ontick data
  with Interfaces.OnTick.Data do
  begin
    TotalSize     := 0;
    ProcessedSize := 0;
    Percentage    := 0;
    Speed         := 0;
  end;
  // Initialize application exitcode
  ExitCode := 0;
end;

destructor TApp.Destroy;
begin
  Interfaces := nil;
  inherited Destroy;
end;

procedure TApp.Execute;
begin
  inherited Execute;
end;

procedure TApp.DoTerminate;
begin
  with Interfaces.Status do
  begin
    Terminated := True;
  end;
  inherited DoTerminate;
end;

procedure TApp.Synchronize(aMethod: TThreadMethod);
begin
  //if Assigned(aMethod) then
  //begin
    inherited Synchronize(aMethod);
  //end;
end;

procedure TApp.SetExitCode(Code: integer);
begin
  if ExitCode < Code then
  begin
    ExitCode := Code;
  end;
end;

procedure TApp.SetPriority(APriority: integer);
begin
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS}
  Bee_Common.SetPriority(aPriority);
  {$ELSE}
  case aPriority of
    0: Priority := tpIdle;
    1: Priority := tpNormal;
    2: Priority := tpHigher;
    3: Priority := tpTimeCritical;
    else Priority := tpNormal;
  end;
  {$ENDIF}
  {$ELSE}
  case aPriority of
    0: Priority := tpIdle;
    1: Priority := tpNormal;
    2: Priority := tpHigher;
    3: Priority := tpTimeCritical;
    else Priority := tpNormal;
  end;
  {$ENDIF}
end;

end.
