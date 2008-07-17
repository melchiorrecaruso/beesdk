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

{   Contains:

    TArcProcess class;
    TFileProcess class.

    Modifyed:
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
  LResources;
  
type

  { TArcProcess class }

  TArcProcess = class(TProcess)
  private
    FArcName: string;
    FArcLink: string;
  public
    property ArcName: string read FArcName write FArcName;
    property ArcLink: string read FArcLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TFileThread = class(TThread)
  private
    FCmdLine: string;
    FProcess: TProcess;
  public
    constructor Create(const CmdLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  end;
  
  TFileProcess = class(TComponent)
  private
    FFileName: string;
    FFileTime: integer;
    FFileExec: string;
    FRunning: boolean;
    FFileIsUpdated: boolean;
    FFileThread: TFileThread;
    function GetFileExec: string;
    procedure SetFileName(Value: string);
    procedure OnTerminate(Sender: TObject);
  public
    property FileName: string read FFileName write SetFileName;
    property FileIsUpdated: boolean read FFileIsUpdated;
    property Running: boolean read FRunning;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  end;
  

  { Register }

  procedure Register;

implementation

uses
  Bee_Common,
  BeeGui_SysUtils;

  { TArcProcess class }

  constructor TArcProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FArcName := '';
    FArcLink := '';
  end;
  
  destructor TArcProcess.Destroy;
  begin
    FArcLink := '';
    FArcName := '';
    inherited Destroy;
  end;
  
  procedure TArcProcess.Execute;
  begin
    FArcLink :=
    // IncludeTrailingBackSlash(GetApplicationTempDir('BeeGui')) +
    'C:\Documents and Settings\quacquero\Desktop\' +
      ExtractFileName(ChangeFileExt(FArcName, '.ini'));

    CommandLine := CommandLine + ' -0"' + FArcLink +'"';
    inherited Execute;
  end;
  
  { TFileThread class }
  
  constructor TFileThread.Create(const CmdLine: string);
  begin
    inherited Create(True);
    FreeOnTerminate := True;
    Priority := tpNormal;
    FCmdLine := CmdLine;
  end;
  
  procedure TFileThread.Execute;
  begin
    FProcess := TProcess.Create(nil);
    FProcess.CommandLine := FCmdLine;
    FProcess.Options := [poWaitOnExit];
    FProcess.Execute;
  end;
  
  destructor TFileThread.Destroy;
  begin
    inherited Destroy;
  end;

  { TFileProcess class }
  
  constructor TFileProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FFileName := '';
    FFileTime :=  0;
    FFileExec := '';
    FRunning := False;
    FFileIsUpdated := False;
  end;

  destructor TFileProcess.Destroy;
  begin
    FFileName := '';
    FFileTime :=  0;
    FFileExec := '';
    FRunning := False;
    FFileIsUpdated := False;
    inherited Destroy;
  end;

  procedure TFileProcess.Execute;
  begin
    if FileExists(FFileName) then
    begin
      FFileExec := GetFileExec;
      FFileTime := FileAge(FFileName);
      if FileExists(FFileExec) then
      begin
        FFileThread := TFileThread.Create(FFileExec + ' "' + FFileName + '"');
        FFileThread.OnTerminate := OnTerminate;
        FFileThread.Resume;
        FRunning := True;
      end;
    end;
  end;
  
  procedure TFileProcess.OnTerminate(Sender: TObject);
  begin
    if FFileThread.FProcess.ExitStatus = 0 then
      if FileAge(FFileName) > FFileTime then
      begin
        FFileIsUpdated := True;
      end;
    FRunning := False;
  end;
  
  procedure TFileProcess.SetFileName(Value: string);
  var
    I: integer;
  begin
    I := System.Pos('*', Value);
    while I > 0 do
    begin
      Delete(Value, I, 1);
      I := System.Pos('*', Value);
    end;

    I := System.Pos('?', Value);
    while I > 0 do
    begin
      Delete(Value, I, 1);
      I := System.Pos('?', Value);
    end;

    I := System.Pos('!', Value);
    while I > 0 do
    begin
      Delete(Value, I, 1);
      I := System.Pos('!', Value);
    end;

    I := System.Pos('"', Value);
    while I > 0 do
    begin
      Delete(Value, I, 1);
      I := System.Pos('"', Value);
    end;
    
    FFileName := Value;
    FFileTime :=  0;
    FFileExec := '';
    FRunning := False;
    FFileIsUpdated := False;
  end;
  
  function TFileProcess.GetFileExec: string;
  var
    {$IFDEF MSWINDOWS}
    P: PChar;
    Res: integer;
    Buffer: array[0..MAX_PATH] of char;
    {$ENDIF}
    OpenDialog: TOpenDialog;
  begin
    Result := '';
    {$IFDEF MSWINDOWS}
    FillChar(Buffer, SizeOf(Buffer), #0);
    Res := FindExecutable(PChar(FFileName), nil, Buffer);
    if Res > 32 then
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
        OpenDialog.Title := 'Open file with';
        OpenDialog.Options := [ofPathMustExist, ofFileMustExist];
        if OpenDialog.Execute then
        begin
          Result := OpenDialog.FileName;
          if DirectoryExists(Result) then Result := '';
        end else
          Result := '';
      finally
        OpenDialog.Free;
      end;
    end;
  end;
  
  { Register }

  procedure Register;
  begin
    RegisterComponents ('Bee Controls', [TArcProcess]);
    RegisterComponents ('Bee Controls', [TFileProcess]);
  end;

initialization

  {$i beegui_process.lrs }

end.

