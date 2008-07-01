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

    TAppProcess class.

    Modifyed:
}

unit BeeGui_ArchiveProcess;

{$I compiler.inc}

interface

uses
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
  public
    property FileName: string read FFileName write FFileName;
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
  end;

  destructor TFileProcess.Destroy;
  begin
    FFileName := '';
    inherited Destroy;
  end;

  procedure TFileProcess.Execute;
  begin
    inherited Execute;
  end;
  
  { Register }

  procedure Register;
  begin
    RegisterComponents ('BeePackage', [TArcProcess]);
  end;

initialization

  {$i beegui_archiveprocess.lrs }

end.

