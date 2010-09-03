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

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Types,
  Bee_Common,
  Bee_Consts,
  Bee_Headers;

type

  TAppCounter class
  private
    FSize:

  public
    procedure Tick;

    property TotalSize: int64 read FTotalSize write SetTotalSize(const Value: int64);

  end;


  { TApp class }

  TApp = class
  private
    FCode: byte;
    FTerminated: boolean;
  protected
    FParams: TStringList;
    FStartTime: double;
    FSuspendedTime: double;
    FTotalSize: int64;
    FSize: int64;
    FSuspended: boolean;
    function GetSpeed: longint;
    function GetPercentes: longint;
    function GetTotalTime: longint;
    function GetTime: longint;
    procedure SetPriority(aPriority: byte);
    procedure SetSuspended(aValue: boolean); overload;
    procedure SetTerminated(aValue: boolean);
    procedure SetCode(aCode: byte);
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure Terminate;

    procedure IncSize; overload;
    procedure DecSize; overload;
    procedure IncSize(const aValue: int64); overload;
    procedure DecSize(const aValue: int64); overload;

    procedure DoMessage(const aMessage: string); overload;
    procedure DoMessage(const aMessage: string; aCode: byte); overload;
    procedure DoRequest(const aMessage: string);
    function  DoRename(const aItem: THeaderRec; const aValue: string): string;
    function  DoPassword(const aFileInfo: TFileInfo; const aValue: string): string;
    procedure DoList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
    procedure DoProgress;
    {$IFDEF CONSOLEAPPLICATION}
    procedure DoClearLine;
    {$ENDIF}
    procedure OnMessage(const aMessage: string); virtual; abstract; 
    procedure OnRequest(const aMessage: string); virtual; abstract;
    function  OnRename(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    function  OnPassword(const aFileInfo: TFileInfo; const aValue: string): string; virtual; abstract;
    procedure OnList(const aFileInfo: TFileInfoExtra; aVerbose: boolean); virtual; abstract;
    procedure OnProgress; virtual; abstract;
    {$IFDEF CONSOLEAPPLICATION}
    procedure OnClearLine; virtual; abstract;
    {$ENDIF}
    property Time: longint read GetTime;
    property TotalTime: longint read GetTotalTime;
    property Size: int64 read FSize;
    property TotalSize: int64 read FTotalSize;
    property Percentes: longint read GetPercentes;
    property Speed: longint read GetSpeed;
    property Suspended: boolean read FSuspended write SetSuspended;
    property Terminated: boolean read FTerminated;
    property Code: byte read FCode write SetCode;
  end;



  procedure ITick;
  function  IGetPassword: string;
  procedure IPutMessage(const aMessage: string); overload;
  procedure IPutMessage(const aMessage: string; aCode: byte); overload;
  procedure NewFileInfo(const THeader; var TFileInfo: TFileInfo);


var
  ITotalSize: int64 = 0;
  ISize     : int64 = 0;


implementation

uses
  DateUtils,
  SysUtils;

procedure ITick;

function IGetPassword: string;
//var
  //FI: TFileInfo;
begin
  //FI := NewFileInfo(P);
  //Result := App.DoPassword(FI, '');
  //if Length(Result) < MinBlowFishKeyLength then
  //begin
  //  Exclude(P.Flags, foPassword);
  //end;
  //FreeFileInfo(P);
end;

procedure IPutMessage(const aMessage: string);
begin
end;

procedure IPutMessage(const aMessage: string; aCode: byte);
begin
end;

procedure NewFileInfo(const THeader; var TFileInfo: TFileInfo);
begin

end;


{ TApp class }

constructor TApp.Create(aParams: TStringList);
begin
  inherited Create;
  FParams := aParams;
  FCode := ccSuccesful;
  FTerminated := False;
  FSuspended := False;
  FSuspendedTime := 0;
  FStartTime := 0;
  FTotalSize := 0;
  FSize := 0;
end;

destructor TApp.Destroy;
begin
  FParams := nil;
  inherited Destroy;
end;

procedure TApp.Execute;
begin
  FStartTime := Now;
end;

procedure TApp.Terminate;
begin
  SetCode(ccUserAbort);
end;

procedure TApp.IncSize(const aValue: int64); {$IFDEF FPC} inline; {$ENDIF}
begin
  Inc(FSize, aValue);
end;

procedure TApp.IncSize; {$IFDEF FPC} inline; {$ENDIF}
begin
  Inc(FSize);
end;

procedure TApp.DecSize(const aValue: int64); {$IFDEF FPC} inline; {$ENDIF}
begin
  Dec(FSize, aValue);
end;

procedure TApp.DecSize; {$IFDEF FPC} inline; {$ENDIF}
begin
  Dec(FSize);
end;

function TApp.GetSpeed: longint;
var
  I: int64;
begin
  if not FSuspended then
    I := MilliSecondsBetween(Now, FStartTime)
  else
    I := MilliSecondsBetween(FSuspendedTime - FStartTime, 0);

  if I <> 0 then
    Result := Round((FSize / I) * 1000)
  else
    Result := 0;
end;

function TApp.GetPercentes: longint;
begin
  if FTotalSize <> 0 then
    Result := Round((FSize / FTotalSize) * 100)
  else
    Result := 0;
end;

function TApp.GetTotalTime: longint;
begin
  if not FSuspended then
    Result := SecondsBetween(Now, FStartTime)
  else
    Result := SecondsBetween(FSuspendedTime - FStartTime, 0);
end;

function TApp.GetTime: longint;
var
  I: longint;
begin
  I := GetSpeed;
  if I <> 0 then
    Result := (FTotalSize - FSize) div I
  else
    Result := 0;
end;

procedure TApp.SetTerminated(aValue: boolean);
begin
  if not FTerminated then
  begin
    FTerminated := aValue;
  end;
end;

procedure TApp.SetCode(aCode: byte);
begin
  if FCode < aCode then
  begin
    FCode := aCode;
    if FCode >= ccError then
      SetTerminated(True);
  end;
end;

procedure TApp.SetSuspended(aValue: boolean);
begin
  if FSuspended <> aValue then
  begin
    if aValue then
      FSuspendedTime := Now
    else
      FStartTime := FStartTime + (Now - FSuspendedTime);

    FSuspended := aValue;
  end;
end;

procedure TApp.SetPriority(aPriority: byte);
begin
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS}
  Bee_Common.SetPriority(aPriority);
  {$ENDIF}
  {$ELSE}
    { TODO :  DA IMPLEMENTARE}
  {$ENDIF}
end;

procedure TApp.DoMessage(const aMessage: string; aCode: byte); {$IFDEF FPC} inline; {$ENDIF}
var
  X: double;
begin
  SetCode(aCode);
  X := Now;
  OnMessage(aMessage);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoMessage(const aMessage: string); {$IFDEF FPC} inline; {$ENDIF}
var
  X: double;
begin
  X := Now;
  OnMessage(aMessage);
  FStartTime := FStartTime + (Now - X);
end;

function TApp.DoRename(const aItem: THeaderRec; const aValue: string): string;
var
  X: double;
begin
  X := Now;
  // Result := OnRename(aFileInfo, aValue);
  FStartTime := FStartTime + (Now - X);
end;

function TApp.DoPassword(const aFileInfo: TFileInfo; const aValue: string): string;
var
  X: double;
begin
  X := Now;
  Result := OnPassword(aFileInfo, aValue);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoRequest(const aMessage: string);
var
  X: double;
begin
  X := Now;
  OnRequest(aMessage);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
var
  X: double; 
begin
  X := Now;
  OnList(aFileInfo, aVerbose);
  FStartTime := FStartTime + (Now - X);
end;

procedure TApp.DoProgress; {$IFDEF FPC} inline; {$ENDIF}
var
  X: double;
begin
  X := Now;
  OnProgress;
  FStartTime := FStartTime + (Now - X);
end;

{$IFDEF CONSOLEAPPLICATION}
procedure TApp.DoClearLine; {$IFDEF FPC} inline; {$ENDIF}
var
  X: double;
begin
  X := Now;
  OnClearLine;
  FStartTime := FStartTime + (Now - X); 
end;
{$ENDIF}

end.
