{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

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

    TAppViewer class.

    Modifyed:

    v1.0.3 build 0020 - 2006/11/25 by Melchiorre Caruso.
}

unit BeeGui_AppViewer;

interface

uses
  SysUtils,
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Classes,
  Dialogs,
  Process;

type
  TAppViewer = class(TThread)
  private
    FFileName: string;
    FFileTime: integer;
    FFileExec: string;
    function GetFileExec: string;
    function GetFileName: string;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property FileName : string read GetFileName;
    property FileTime: integer read FFileTime;
  end;

implementation

  { TAppViewer }
  
  constructor TAppViewer.Create(const AFileName: string);
  begin
    FFileTime := FileAge(AFileName);
    FFileName := '"' + AFileName + '"';
    FFileExec := GetFileExec;
    FreeOnTerminate := True;
    inherited Create(True);
  end;
  
  destructor TAppViewer.Destroy;
  begin
    inherited Destroy;
  end;

  function TAppViewer.GetFileExec: string;
  var
    {$IFDEF WIN32}
    P: PChar;
    Res: HINST;
    Buffer: array[0..MAX_PATH] of char;
    {$ENDIF}
    OpenDialog: TOpenDialog;
  begin
    Result := '';
    {$IFDEF WIN32}
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

  function TAppViewer.GetFileName: string;
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

  procedure TAppViewer.Execute;
  var
    Process: TProcess;
  begin
    if (FFileExec <> '') then
    begin
      Process := TProcess.Create(nil);
      try
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Active := False;
        Process.CommandLine := FFileExec + ' ' + FFileName;
        Process.Execute;
      finally
        Process.Free;
      end;
    end;
  end;
  
end.

