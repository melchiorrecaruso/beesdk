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
  CustomTimer,
  LResources;
  
type

  { TArcProcess class }

  TArcProcess = class(TIdleTimer)
  private
    // FCommandLine: TCommandLine;
    FArchiveName: string;
    FCurrentDir: string;
    FProcess: TProcess;
  protected
    procedure DoOnTimer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  public
    // property CommandLine: TCommandLine read FCommandLine write FCommandLine;
    property ArchiveName: string read FArchiveName write FArchiveName;
    property CurrentDir: string read FCurrentDir write FCurrentDir;
  end;

  { TFileProcess class }

  TFileProcess = class(TIdleTimer)
  private
    FFileName: string;
    FFileTime: integer;
    FCurrentDir: string;
    FProcess: TProcess;
  protected
    procedure DoOnTimer; override;
    function GetExec: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  public
    property FileName: string read FFileName write FFileName;
    property CurrentDir: string read FCurrentDir write FCurrentDir;
  end;
  
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
    FProcess := TProcess.Create(Self);
    FProcess.StartupOptions := [];
    FProcess.Options := [];

    SetLength(FArchiveName, 0);
    SetLength(FCurrentDir, 0);
    // SetLength(FCommandLine, 0);
  end;
  
  destructor TArcProcess.Destroy;
  begin
    SetLength(FArchiveName, 0);
    SetLength(FCurrentDir, 0);
    // SetLength(FCommandLine, 0);
    FProcess.Destroy;
    inherited Destroy;
  end;

  procedure TArcProcess.Execute;
  begin
    if FProcess.Running = False then
    begin
      FProcess.CurrentDirectory := FCurrentDir;
      FProcess.StartupOptions := [];
      FProcess.Options := [];
      FProcess.Execute;
      Enabled := True;
    end;
  end;

  procedure TArcProcess.DoOnTimer;
  begin
    if FProcess.Running = False then
    begin
      Enabled := False;
    end;
    inherited DoOnTimer;
  end;
  
  { TFileProcess class }
  
  constructor TFileProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FProcess := TProcess.Create(Self);
    FProcess.StartupOptions := [];
    FProcess.Options := [];

    SetLength(FFileName, 0);
    SetLength(FCurrentDir, 0);
  end;
  
  procedure TFileProcess.Execute;
  begin
    if FProcess.Running = False then
    begin
      FFileTime := FileAge(FFileName);
      FProcess.CommandLine := GetExec + ' "' + FFileName  + '"';
      FProcess.CurrentDirectory := FCurrentDir;
      FProcess.StartupOptions := [];
      FProcess.Options := [];
      FProcess.Execute;
      Enabled := True;
    end;
  end;

  procedure TFileProcess.DoOnTimer;
  begin
    if FProcess.Running = False then
    begin
      Enabled := False;
      if FileAge(FFileName) > FFileTime then
      begin

      end;
    end;
    inherited DoOnTimer;
  end;
  
  destructor TFileProcess.Destroy;
  begin
    SetLength(FCurrentDir, 0);
    SetLength(FFileName, 0);
    FreeAndNil(FProcess);
    inherited Destroy;
  end;
  
 function TFileProcess.GetExec: string;
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

