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

unit BeeGui_AppViewer;

{$I Compiler.inc}

interface

uses
  Classes,
  Dialogs,
  Windows,
  ShellApi,
  SysUtils;

type
  TAppViewer = class(TThread)
  private
    FFileName: string;
    FFileTime: integer;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property FileName: string Read FFileName;
    property FileTime: integer Read FFileTime;
  end;

implementation

{ TAppViewer }

constructor TAppViewer.Create(const AFileName: string);
begin
  FFileTime := FileAge(AFileName);
  FFileName := AFileName;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TAppViewer.Destroy;
begin
  inherited Destroy;
end;

procedure TAppViewer.Execute;
var
  SEI: TShellExecuteInfo;
begin
  FillChar(SEI, SizeOf(SEI), #0);
  SEI.cbSize := SizeOf(SEI);
  SEI.fMask  := SEE_MASK_NOCLOSEPROCESS;
  // ---
  SEI.lpFile := PChar(FileName);
  SEI.lpDirectory := PChar(ExtractFileDir(FileName));
  SEI.nShow  := SW_NORMAL;
  // ---
  if ShellExecuteEx(@SEI) then
  begin
    WaitForSingleObject(SEI.hProcess, INFINITE);
    CloseHandle(SEI.hProcess);
  end;
end;

end.
