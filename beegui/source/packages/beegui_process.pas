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

    TArcProcess class;
    TFileProcess class.

  Modifyed:

    v1.0.5 build 0559 - 2008.12.22 by Melchiorre Caruso.
}

unit BeeGui_Process;

{$I compiler.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Process,
  Dialogs,
  Classes,
  SysUtils,
  ExtCtrls,
  LResources;

type

  { TFileProcess class }

  TFileProcess = class(TIdleTimer)
  private
    FFileName: string;
    FFileTime: integer;
    FProcess:  TProcess;
    FFileIsModified: boolean;
  private
    function GetFileExec: string;
    procedure DoOnTimer; override;
  public
    procedure Execute(const AFileExec, AFileName: string);
    constructor Create(Sender: TComponent); override;
    destructor Destroy; override;
  public
    property FileName: string Read FFileName;
    property FileIsModified: boolean Read FFileIsModified;
  end;

{ Register }

procedure Register;

implementation

uses
  Bee_Common;

{ TFileProcess class }

constructor TFileProcess.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  FFileName := '';
  FProcess  := nil;
  FFileIsModified := False;
end;

procedure TFileProcess.Execute(const AFileExec, AFileName: string);
var
  FFileExec: string;
begin
  FFileName := AFileName;
  FFileTime := FileAge(FFileName);
  FFileIsModified := False;

  if AFileExec = '' then
    FFileExec := GetFileExec
  else
    FFileExec := AFileExec;

  if FFileExec <> '' then
  begin
    FProcess := TProcess.Create(nil);
    FProcess.CommandLine := FFileExec + ' "' + FFileName + '"';
    FProcess.CurrentDirectory := '';
    FProcess.StartupOptions := [];
    FProcess.Options := [];
    FProcess.Execute;
  end;
  Enabled := True;
end;

procedure TFileProcess.DoOnTimer;
begin
  if Assigned(FProcess) then
  begin
    if FProcess.Running = False then
    begin
      if FProcess.ExitStatus < 2 then
      begin
        FFileIsModified := FileAge(FFileName) > FFileTime;
      end;
      FreeAndNil(FProcess);
      Enabled := False;
    end;
  end else
    Enabled := False;
  inherited DoOnTimer;
end;

destructor TFileProcess.Destroy;
begin
  FFileName := '';
  inherited Destroy;
end;

function TFileProcess.GetFileExec: string;
var
  {$IFDEF MSWINDOWS}
  P:      PChar;
  Buffer: array[0..MAX_PATH] of char;
  {$ENDIF}
  OpenDialog: TOpenDialog;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  FillChar(Buffer, SizeOf(Buffer), #0);
  if FindExecutable(PChar(FFileName), nil, Buffer) > 32 then
  begin
    P := Buffer;
    while PWord(P)^ <> 0 do
    begin
      if P^ = #0 then P^ := #32;
      Inc(P);
    end;
    Result := Buffer;
  end;
  {$ENDIF}
  if FileExists(Result) = False then
  begin
    OpenDialog := TOpenDialog.Create(nil);
    try
      OpenDialog.Title   := 'Open file with';
      OpenDialog.Options := [ofPathMustExist, ofFileMustExist];
      if OpenDialog.Execute then
      begin
        Result := OpenDialog.FileName;
        if DirectoryExists(Result) then
          Result := '';
      end
      else
        Result := '';
    finally
      OpenDialog.Free;
    end;
  end;
end;

{ Register }

procedure Register;
begin
  RegisterComponents('BeePackage', [TFileProcess]);
end;

initialization

  {$I beegui_process.lrs }

end.
