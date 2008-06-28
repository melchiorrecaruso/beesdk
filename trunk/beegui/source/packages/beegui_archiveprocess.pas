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

  { TArchiveProcess class }

  TArchiveProcess = class(TProcess)
  private
    FArchiveContents: TStringList;
    FArchiveLink: string;
    FArchiveName: string;
  private
  public
    property ArchiveName: string read FArchiveName write FArchiveName;
    property ArchiveLink: string read FArchiveLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  published
  end;
  
  { Register }

  procedure Register;

implementation

uses
  Bee_Common,
  BeeGui_SysUtils;

  { TAppProcess class }

  constructor TArchiveProcess.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FArchiveContents := TStringList.Create;
    FArchiveLink := '';
    FArchiveName := '';
  end;
  
  destructor TArchiveProcess.Destroy;
  begin
    FArchiveContents.Free;
    FArchiveLink := '';
    FArchiveName := '';
    inherited Destroy;
  end;
  
  procedure TArchiveProcess.Execute;
  begin
    FArchiveLink :=
    // IncludeTrailingBackSlash(GetApplicationTempDir('BeeGui')) +
    'C:\Documents and Settings\quacquero\Desktop\' +
      ExtractFileName(ChangeFileExt(FArchiveName, '.ini'));

    CommandLine := CommandLine + ' -0"' + FArchiveLink +'"';
    inherited Execute;
  end;

  { Register }

  procedure Register;
  begin
    RegisterComponents ('BeePackage', [TArchiveProcess]);
  end;

initialization

  {$i beegui_archiveprocess.lrs }

end.

