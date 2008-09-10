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

  TArcProcess = class(TIdleTimer)
  private
    FArchiveName: string;
    FArchiveLink: string;
    FCommandLines: TStringList;
    FCurrentDirectories: TStringList;
    FProcess: TProcess;
    FOnTerminate: TNotifyEvent;
  protected
    procedure DoOnTimer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(const aArchiveName: string; const aArchiveLink: string);
    procedure Add(const aCommandLine :string; const aCurrentDirectory: string);
    procedure Finalize(const aOnTerminate: TNotifyEvent);
   public
    property ArchiveName: string read FArchiveName;
    property ArchiveLink: string read FArchiveLink;
    property OnTerminate: TNotifyEvent read FOnTerminate;
  end;


  { TFileProcess class }

  TFileProcess = class(TIdleTimer)
  private
    FFileName: string;
    FRootFolder: string;
    FProcess: TProcess;
  protected
    procedure DoOnTimer; override;
    function GetFileExec: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(const aFileName: string; const aRootFolder: string);
  public
    property FileName: string read FFileName;
    property RootFolder: string read FRootFolder;
  end;

  { Routines }

  function GenerateArchiveLink: string;
  
  { Register }

  procedure Register;

implementation

uses
  Bee_Common,
  BeeGui_Consts,
  BeeGui_SysUtils;

  function GenerateArchiveLink: string;
  var
    I: integer;
    Path: string;
  begin
    Path := GetApplicationTempDir(cApplicationName);
    repeat
      Result := '????????.ini';
      for I := 1 to 8 do
      begin
        Result[I] := char(byte('A') + Random(byte('Z') - byte('A')));
      end;
      Result := IncludeTrailingBackSlash(Path) + Result;
    until FileAge(Result) = -1;
  end;

  { TArcProcess class }

  constructor TArcProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FArchiveName := '';
    FArchiveLink := '';
    FCommandLines := TStringList.Create;
    FCurrentDirectories := TStringList.Create;
    FProcess := TProcess.Create(Self);
    FProcess.StartupOptions := [];
    FProcess.Options := [];
  end;
  
  destructor TArcProcess.Destroy;
  begin
    FProcess.Free;
    FArchiveName := '';
    FArchiveLink := '';
    FCommandLines.Free;
    FCurrentDirectories.Free;
    inherited Destroy;
  end;
  
  procedure TArcProcess.DoOnTimer;
  begin
    if FProcess.Running = False then
    begin
      if FCommandLines.Count > 0 then
      begin
        FProcess.CurrentDirectory := FCurrentDirectories.Strings[0];
        FProcess.CommandLine := FCommandLines.Strings[0];
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
        FOnTerminate := nil;
      end;
    end;
    inherited DoOnTimer;
  end;
  
  procedure TArcProcess.Add(const aCommandLine :string; const aCurrentDirectory: string);
  begin
    if Assigned(FOnTerminate) = False then
    begin
      FCommandLines.Add(aCommandLine);
      FCurrentDirectories.Add(aCurrentDirectory);
    end;
  end;
  
  procedure TArcProcess.Initialize(const aArchiveName: string; const aArchiveLink: string);
  begin
    if Enabled = False then
    begin
      FOnTerminate := nil;
      FCommandLines.Clear;
      FCurrentDirectories.Clear;
      FArchiveName := aArchiveName;
      FArchiveLink := aArchiveLink;
    end;
  end;

  procedure TArcProcess.Finalize(const aOnTerminate: TNotifyEvent);
  begin
    if Assigned(FOnTerminate) = False then
    begin
      FOnTerminate := aOnTerminate;
    end;
  end;
  
  { TFileProcess class }
  
  constructor TFileProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FProcess := TProcess.Create(Self);
    FRootFolder := '';
    FFileName := '';
  end;
  
  procedure TFileProcess.Initialize(const aFileName: string; const aRootFolder: string);
  begin
    if FProcess.Running = False then
    begin
      FFileName := aFileName;
      FRootFolder := aRootFolder;
    end;
  end;

  procedure TFileProcess.DoOnTimer;
  var
    FFileTime: integer;
  begin
    if FProcess.Running = False then
    begin
      if FileExists(FFileName) then
      begin
        FFileTime := FileAge(FFileName);
        FProcess.CommandLine := GetFileExec + ' "' + FFileName + '"';
        FProcess.Options := [poWaitOnExit];
        FProcess.Execute;
        if FileAge(FFileName) > FFileTime then
        begin
          Enabled := False;
          // Aggiornare //
        end;
      end;
    end;
    inherited DoOnTimer;
  end;
  
  destructor TFileProcess.Destroy;
  begin
    FProcess.Free;
    FFileName := '';
    FRootFolder := '';
    inherited Destroy;
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
    RegisterComponents('BeePackage', [TArcProcess]);
    RegisterComponents('BeePackage', [TFileProcess]);
  end;

initialization

  {$I beegui_process.lrs }

end.

