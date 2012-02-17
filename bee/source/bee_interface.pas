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

    TApp class

  Modifyed:

    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Consts,
  Bee_Headers;

type
  { TApp class }

  TApp = class
  protected
    FExitCode: byte;
    FSuspended:  boolean;
    FTerminated: boolean;
    FProcessedSize: int64;
    FTotalSize: int64;
    function GetProgress: longint;
    procedure SetPriority(Value: byte);
    procedure SetExitCode(Value: byte);
    procedure SetTerminated(Value: boolean);
    procedure SetSuspended(Value: boolean);
    procedure DoMessage(const aMessage: string); overload; virtual; abstract;
    procedure DoMessage(const aMessage: string; aExitCode: byte); overload; virtual;
    procedure DoRequest(const aMessage: string); virtual abstract;
    function DoRename(const aItem: THeader; const aValue: string): string; virtual abstract;
    function DoOverWrite(const aItem: THeader; const aValue: string): string; virtual; abstract;
    procedure DoList(const aItem: THeader); virtual; abstract;
    procedure DoClear; virtual abstract;
    procedure DoTick(Value: longint); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
    procedure Terminate; virtual;
  public
    property Progress: longint read GetProgress;
    property ProcessedSize: int64 read FProcessedSize write FProcessedSize;
    property TotalSize: int64 read FTotalSize write FTotalSize;

    property Suspended: boolean read FSuspended write SetSuspended;
    property Terminated: boolean read FTerminated;
    property ExitCode: byte read FExitCode;
  end;

implementation

uses
  Bee_Common;

{ TApp class }

constructor TApp.Create;
begin
  inherited Create;
  FTotalSize     := 0;
  FProcessedSize := 0;
  FTerminated    := False;
  FSuspended     := False;
  FExitCode      := ccSuccesful;
end;

destructor TApp.Destroy;
begin
  inherited Destroy;
end;

procedure TApp.Terminate;
begin
  SetExitCode(ccUserAbort);
end;

function TApp.GetProgress: longint;
begin
  if FTotalSize <> 0 then
     Result := Round((FProcessedSize / FTotalSize) * 100)
   else
     Result := 0;
end;

procedure TApp.SetTerminated(Value: boolean);
begin
  if FTerminated = False then
  begin
    FTerminated := Value;
    if FTerminated = True then
    begin
      FSuspended  := False;
    end;
  end;
end;

procedure TApp.SetSuspended(Value: boolean);
begin
  if FTerminated = False then
  begin
    FSuspended := Value;
  end;
end;

procedure TApp.SetExitCode(Value: byte);
begin
  if FTerminated = False then
  begin
    if FExitCode < Value then
    begin
      FExitCode := Value;
      if FExitCode >= ccError then
        SetTerminated(True);
    end;
  end;
end;

procedure TApp.SetPriority(Value: byte);
begin
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS}
    Bee_Common.SetPriority(Value);
  {$ENDIF}
  {$ELSE}
    { TODO : }
  {$ENDIF}
end;

procedure TApp.DoMessage(const aMessage: string; aExitCode: byte);
begin
  SetExitCode(aExitCode);
end;

procedure TApp.DoTick(Value: longint);
begin
  Inc(FProcessedSize, Value);
end;

end.
