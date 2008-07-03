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
  
  TFileProcess = class(TProcess)
  private
    FFileName: string;
    FFileTime: integer;
    FFileExec: string;
    function GetFileName: string;
    function GetFileExec: string;
    procedure SetFileName(Value: string);
  public
    property FileName: string read GetFileName write SetFileName;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
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

  { TFileProcess class }
  
  constructor TFileProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FFileName := '';
    FFileTime :=  0;
    FFileExec := '';
  end;

  destructor TFileProcess.Destroy;
  begin
    FFileName := '';
    FFileTime :=  0;
    FFileExec := '';
    inherited Destroy;
  end;

  procedure TFileProcess.Execute;
  begin
    inherited Execute;
  end;
  
  procedure TFileProcess.SetFileName(Value: string);
  begin
    FFileName := '';
    FFileTime :=  0;
    FFileExec := '';
    if FileExists(Value) then
    begin
      FFileName := '"' + Value + '"';
      FFileTime := FileAge(Value);
      FFileExec := GetFileExec;
    end;
  end;
  
  function TFileProcess.GetFileName: string;
  var
    I: integer;
  begin
    Result := FFileName;
    I := Pos('"', Result);
    while I > 0 do
    begin
      Delete(Result, I, 1);
      I := Pos('"', Result);
    end;
  end;
  
  function TFileProcess.GetFileExec: string;
  var
    {$IFDEF MSWINDOWS}
    P: PChar;
    Res: HINST;
    Buffer: array[0..MAX_PATH] of char;
    {$ENDIF}
    OpenDialog: TOpenDialog;
  begin
    Result := '';
    {$IFDEF MSWINDOWS}
    P := nil;
    FillChar(Buffer, SizeOf(Buffer), #0);
    Res := FindExecutable(PChar(FFileName), P, Buffer);
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
    if Result = '' then
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

