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
  ExtCtrls,
  CustomTimer,
  LResources;
  
type

  { TArcProcess class }

  TArcProcess = class(TCustomTimer)
  private
    FArchiveName: string;
    FArchiveLink: string;
    FCommandLines: TStringList;
    FCurrentDirectories: TStringList;
    FProcess: TProcess;
    FOnTerminate: TNotifyEvent;
  protected
    procedure DoOnTimer; override;
  private
    procedure SetArchiveName(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Append(const CommandLine, CurrentDirectory: string);
  public
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property ArchiveLink: string read FArchiveLink;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;
  
  (*
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

  // TFPTerminateEvent = procedure(Sender: TObject; const FileName, RootFolder: string) of object;
  
  TFileProcess = class(TComponent)
  private
    FProcessList: TList;
  public
    procedure Execute(const AFileName, ARootFolder: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  *)
  
  { Register }

  procedure Register;

implementation

uses
  Bee_Common,
  BeeGui_Consts,
  BeeGui_SysUtils;

  { TArcProcess class }

  constructor TArcProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FArchiveName := '';
    FArchiveLink := '';
    FCommandLines := TStringList.Create;
    FCurrentDirectories := TStringList.Create;
    FProcess := TProcess.Create(Self);
  end;
  
  destructor TArcProcess.Destroy;
  begin
    FArchiveName := '';
    FArchiveLink := '';
    FCommandLines.Free;
    FCurrentDirectories.Free;
    FProcess.Free;
    inherited Destroy;
  end;
  
  procedure TArcProcess.DoOnTimer;
  begin
    if FProcess.Running = False then
    begin
      if FCommandLines.Count > 0 then
      begin
        FProcess.CurrentDirectory := FCurrentDirectories.Strings[0];
        FProcess.CommandLine := FCommandLines.Strings[0] + ' -0' + FArchiveLink;
        FProcess.Execute;
        
        FCurrentDirectories.Delete(0);
        FCommandLines.Delete(0);
      end else
      begin
        Enabled := False;
        if Assigned(FOnTerminate) and (FProcess.ExitStatus = 0) then;
        begin
          FOnTerminate(Self);
        end;
      end;
    end;
    inherited DoOnTimer;
  end;
  
  procedure TArcProcess.Append(const CommandLine, CurrentDirectory: string);
  begin
    if Assigned(FOnTerminate) = False then
    begin
      FCommandLines.Add(CommandLine);
      FCurrentDirectories.Add(CurrentDirectory);
    end;
  end;
  
  procedure TArcProcess.SetArchiveName(const Value: string);
  begin
    if Enabled = False then
    begin
      FArchiveName := Value;
      FArchiveLink := GenerateFileName(GetApplicationTempDir(cApplicationName));
      FCommandLines.Clear;
      FCurrentDirectories.Clear;
    end;
  end;
  
  (*
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

  procedure TFileProcess.Execute(AIndex: integer);
  begin
    if AIndex < FProcessList.Count then
    begin
      TFileThread(FProcessList.Items[AIndex]).Execute;
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
  
  function TFileProcess.GetFileThread(AIndex: integer): TFileThread;
  begin
    if AIndex < FProcessList.Count then
    begin
      Result := FProcessList.Items[AIndex];
    end;
  end;
  
  procedure TFileProcess.Schedule(const AFileName, ARootFolder: string);
  begin
    FProcessList.Add(TFileThread.Create(AFileName, ARootFolder));
  end;
  *)
  
  { Register }

  procedure Register;
  begin
    RegisterComponents('BeePackage', [TArcProcess]);
  end;

initialization

  {$I beegui_process.lrs }

end.

