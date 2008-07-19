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
  
  { TFileThread class }

  TFileThread = class(TThread)
  private
    FFileName: string;
    FRootFolder: string;
    FProcess: TProcess;
  private
    function GetFileExec: string;
  public
    constructor Create(const AFileName, ARootFolder: string);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property FileName: string read FFileName;
    property RootFolder: string read FRootFolder;
  end;
  
  { TFileProcess class }
  
  TFileProcess = class(TComponent)
  private
    FProcessList: TList;
    function GetFileThread(AIndex: integer): TFileThread;
    procedure OnTerminate(Sender: TObject);
  public
    property FileThread[AIndex: integer]: TFileThread read GetFileThread;
  public
    procedure Schedule(const FileName, RootFolder: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(Index: integer);
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
    FArcName := '';
    FArcLink := '';
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
  
  constructor TFileThread.Create(const AFileName, ARootFolder: string);
  begin
    inherited Create(True);
    FreeOnTerminate := True;
    Priority := tpNormal;
    // ---
    FFileName := AFileName;
    FRootFolder := AFileName;
    FProcess := TProcess.Create(nil);
  end;
  
  procedure TFileThread.Execute;
  var
    FFileTime: integer;
  begin
    if FileExists(FFileName) then
    begin
      FFileTime := FileAge(FFileName);
      FProcess.CommandLine := GetFileExec + ' "' + FFileName + '"';
      FProcess.Options := [poWaitOnExit];
      FProcess.Execute;
      if FileAge(FFileName) > FFileTime then
      begin
        FreeOnTerminate := False;
      end;
    end;
  end;
  
  destructor TFileThread.Destroy;
  begin
    FProcess.Free;
    FFileName := '';
    FRootFolder := '';
    inherited Destroy;
  end;
  
 function TFileThread.GetFileExec: string;
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

  { TFileProcess class }
  
  constructor TFileProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FProcessList := TList.Create;
  end;

  destructor TFileProcess.Destroy;
  var
    I: integer;
  begin
    for I := FProcessList.Count -1 downto 0 do
      if Assigned(FProcessList.Items[I]) then
      begin
        TFileThread(FProcessList.Items[I]).Free;
      end;
    FProcessList.Clear;
    FProcessList.Free;
    inherited Destroy;
  end;

  procedure TFileProcess.Execute(Index: integer);
  begin
    if Index < FProcessList.Count then
    begin
      TFileThread(FProcessList.Items[Index]).Execute;
    end;
  end;
  
  procedure TFileProcess.OnTerminate(Sender: TObject);
  var
    I: integer;
  begin
    for I := FProcessList.Count -1 downto 0 do
    begin
      if Sender = TFileThread(FProcessList.Items[I]) then
      begin


      end;
    end;
  end;
  
  procedure TFileProcess.SetFileName(Value: string);
  var
    I: integer;
  begin

    
    FFileName := Value;
    FFileTime :=  0;
    FFileExec := '';
    FRunning := False;
    FFileIsUpdated := False;
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

