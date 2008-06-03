{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU GeneralPage Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU GeneralPage Public License for more details.

  You should have received a copy of the GNU GeneralPage Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

  Modifyed:

}

unit BeeGui_Messages;

{$I compiler.inc}

interface

uses
  Classes,
  IniFiles,
  SysUtils;

var
  rsConfirmation:         string = 'Confirmation';
  rsConfirmAbort:         string = 'Do you want to abort current process?';
  rsProcessStatus:        string  = '%d%% Processing...';
  rsProcessTerminated:    string = 'Process terminated';
  rsProcessAborted:       string = 'Process aborted';
  rsProcessPaused:        string = 'Process paused';
  rsRenameFile:           string = 'Rename file';
  rsRenameArchive:        string = 'Rename archive';
  rsWarning:              string = 'Warning';
  rsPasswordNotConfirmed: string = 'Password not confirmed.';
  rsAbortProcess:         string = 'Active proces. Terminate it?';
  rsSelectFolder:         string = 'Select folder';

implementation

  procedure LoadMessages;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('messages.ini'));
      rsConfirmation         := Storage.ReadString('ResourceString', 'rsConfirmation', rsConfirmation);
      rsConfirmAbort         := Storage.ReadString('ResourceString', 'rsConfirmAbort', rsConfirmAbort);
      rsProcessStatus        := Storage.ReadString('ResourceString', 'rsProcessStatus', rsProcessStatus);
      rsProcessPaused        := Storage.ReadString('ResourceString', 'rsProcessPaused', rsProcessPaused);
      rsProcessAborted       := Storage.ReadString('ResourceString', 'rsProcessAborted', rsProcessAborted);
      rsProcessTerminated    := Storage.ReadString('ResourceString', 'rsProcessTerminated', rsProcessTerminated);
      rsRenameFile           := Storage.ReadString('ResourceString', 'rsRenameFile', rsRenameFile);
      rsRenameArchive        := Storage.ReadString('ResourceString', 'rsRenameArchive', rsRenameArchive);
      rsWarning              := Storage.ReadString('ResourceString', 'rsWarning', rsWarning);
      rsPasswordNotConfirmed := Storage.ReadString('ResourceString', 'rsPasswordNotConfirmed', rsPasswordNotConfirmed);
      rsAbortProcess         := Storage.ReadString('ResourceString', 'rsAbortProcess', rsAbortProcess);
      rsSelectFolder         := Storage.ReadString('ResourceString', 'rsSelectFolder', rsSelectFolder);
      Storage.Free;
    end;
  end;

  procedure SaveMessages;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('messages.ini'));
      Storage.WriteString('ResourceString', 'rsConfirmation', rsConfirmation);
      Storage.WriteString('ResourceString', 'rsConfirmAbort', rsConfirmAbort);
      Storage.WriteString('ResourceString', 'rsProcessStatus', rsProcessStatus);
      Storage.WriteString('ResourceString', 'rsProcessPaused', rsProcessPaused);
      Storage.WriteString('ResourceString', 'rsProcessAborted', rsProcessAborted);
      Storage.WriteString('ResourceString', 'rsProcessTerminated', rsProcessTerminated);
      Storage.WriteString('ResourceString', 'rsRenameFile', rsRenameFile);
      Storage.WriteString('ResourceString', 'rsRenameArchive', rsRenameArchive);
      Storage.WriteString('ResourceString', 'rsWarning', rsWarning);
      Storage.WriteString('ResourceString', 'rsPasswordNotConfirmed', rsPasswordNotConfirmed);
      Storage.WriteString('ResourceString', 'rsAbortProcess', rsAbortProcess);
      Storage.WriteString('ResourceString', 'rsSelectFolder', rsSelectFolder);
      Storage.Free;
    end;
  end;
  
initialization

  LoadMessages;

finalization

  SaveMessages;
  
end.

