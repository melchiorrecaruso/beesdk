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
  SysUtils,
  IniFiles;

var
  rsWarning:              string = 'Warning';
  rsConfirmation:         string = 'Confirmation';

  rsSelectFolder:         string = 'Select folder';
  rsSelectFrmCaption:     string = 'Select';
  rsDeselectFrmCaption:   string = 'Deselect';
  
  rsConfirmDeleteArc:     string = 'Do you want to delete archive?';
  rsConfirmDeleteFiles:   string = 'Do you want to delete selected files?';
  rsConfirmAbortProcess:  string = 'Do you want to abort current process?';
  rsConfirmDefault:       string = 'Do you want return to default setting?';
  
  rsProcessTerminated:    string = 'Process terminated';
  rsProcessAborted:       string = 'Process aborted';
  rsProcessPaused:        string = 'Process paused';
  rsProcessStatus:        string  = '%d%% Processing...';
  rsProcessExists:        string = 'An active process exists.';

  rsMoveArcTo:            string = 'Move archive to:';
  rsCopyArcTo:            string = 'Copy archive to:';
  rsRenameArc:            string = 'Rename archive';
  rsRenameFile:           string = 'Rename file';
  
  rseConfirmPassword:     string = 'Password not confirmed.';
  rseReadArcProperty:     string = 'Error reading archive infomations.';
  rseDeleteArc:           string = 'Error deleting archive.';
  rseMoveArcTo:           string = 'Error moving archive.';
  rseCopyArcTo:           string = 'Error copying archive.';
  rseRenameArc:           string = 'Error renaming archive.';
  rseRenameFile:          string = 'Error renaming file.';
  
  
  
  
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
      begin
      rsWarning              := Storage.ReadString('ResourceString', 'rsWarning', rsWarning);
      rsConfirmation         := Storage.ReadString('ResourceString', 'rsConfirmation', rsConfirmation);
      
      rsSelectFolder         := Storage.ReadString('ResourceString', 'rsSelectFolder', rsSelectFolder);
      rsSelectFrmCaption     := Storage.ReadString('ResourceString', 'rsSelectFrmCaption', rsSelectFrmCaption);
      rsDeSelectFrmCaption   := Storage.ReadString('ResourceString', 'rsDeSelectFrmCaption', rsDeSelectFrmCaption);

      rsConfirmDeleteArc     := Storage.ReadString('ResourceString', 'rsConfirmDeleteArc', rsConfirmDeleteArc);
      rsConfirmDeleteFiles   := Storage.ReadString('ResourceString', 'rsConfirmDeleteFiles', rsConfirmDeleteFiles);
      rsConfirmAbortProcess  := Storage.ReadString('ResourceString', 'rsConfirmAbortProcess', rsConfirmAbortProcess);
      rsConfirmDefault       := Storage.ReadString('ResourceString', 'rsConfirmDefault', rsConfirmDefault);

      rsProcessTerminated    := Storage.ReadString('ResourceString', 'rsProcessTerminated', rsProcessTerminated);
      rsProcessAborted       := Storage.ReadString('ResourceString', 'rsProcessAborted', rsProcessAborted);
      rsProcessPaused        := Storage.ReadString('ResourceString', 'rsProcessPaused', rsProcessPaused);
      rsProcessStatus        := Storage.ReadString('ResourceString', 'rsProcessStatus', rsProcessStatus);
      rsProcessExists        := Storage.ReadString('ResourceString', 'rsProcessExists', rsProcessExists);

      rsMoveArcTo            := Storage.ReadString('ResourceString', 'rsMoveArcTo', rsMoveArcTo);
      rsCopyArcTo            := Storage.ReadString('ResourceString', 'rsCopyArcTo', rsCopyArcTo);
      rsRenameArc            := Storage.ReadString('ResourceString', 'rsRenameArc', rsRenameArc);
      rsRenameFile           := Storage.ReadString('ResourceString', 'rsRenameFile', rsRenameFile);

      rseConfirmPassword     := Storage.ReadString('ResourceString', 'rseConfirmPassword', rseConfirmPassword);
      rseReadArcProperty     := Storage.ReadString('ResourceString', 'rseReadArcProperty', rseReadArcProperty);
      rseDeleteArc           := Storage.ReadString('ResourceString', 'rseDeleteArc', rseDeleteArc);
      rseMoveArcTo           := Storage.ReadString('ResourceString', 'rseMoveArcTo', rseMoveArcTo);
      rseCopyArcTo           := Storage.ReadString('ResourceString', 'rseCopyArcTo', rseCopyArcTo);
      rseRenameArc           := Storage.ReadString('ResourceString', 'rseRenameArc', rseRenameArc);
      rseRenameFile          := Storage.ReadString('ResourceString', 'rseRenameFile', rseRenameFile);
      end;
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
      begin
      Storage.WriteString('ResourceString', 'rsWarning', rsWarning);
      Storage.WriteString('ResourceString', 'rsConfirmation', rsConfirmation);

      Storage.WriteString('ResourceString', 'rsSelectFolder', rsSelectFolder);
      Storage.WriteString('ResourceString', 'rsSelectFrmCaption', rsSelectFrmCaption);
      Storage.WriteString('ResourceString', 'rsDeselectFrmCaption', rsDeselectFrmCaption);

      Storage.WriteString('ResourceString', 'rsConfirmDeleteArc', rsConfirmDeleteArc);
      Storage.WriteString('ResourceString', 'rsConfirmDeleteFiles', rsConfirmDeleteFiles);
      Storage.WriteString('ResourceString', 'rsConfirmAbortProcess', rsConfirmAbortProcess);
      Storage.WriteString('ResourceString', 'rsConfirmDefault', rsConfirmDefault);

      Storage.WriteString('ResourceString', 'rsProcessTerminated', rsProcessTerminated);
      Storage.WriteString('ResourceString', 'rsProcessAborted', rsProcessAborted);
      Storage.WriteString('ResourceString', 'rsProcessPaused', rsProcessPaused);
      Storage.WriteString('ResourceString', 'rsProcessStatus', rsProcessStatus);
      Storage.WriteString('ResourceString', 'rsProcessExists', rsProcessExists);

      Storage.WriteString('ResourceString', 'rsMoveArcTo', rsMoveArcTo);
      Storage.WriteString('ResourceString', 'rsCopyArcTo', rsCopyArcTo);
      Storage.WriteString('ResourceString', 'rsRenameArc', rsRenameArc);
      Storage.WriteString('ResourceString', 'rsRenameFile', rsRenameFile);

      Storage.WriteString('ResourceString', 'rseConfirmPassword', rseConfirmPassword);
      Storage.WriteString('ResourceString', 'rseReadArcProperty', rseReadArcProperty);
      Storage.WriteString('ResourceString', 'rseDeleteArc', rseDeleteArc);
      Storage.WriteString('ResourceString', 'rseMoveArcTo', rseMoveArcTo);
      Storage.WriteString('ResourceString', 'rseCopyArcTo', rseCopyArcTo);
      Storage.WriteString('ResourceString', 'rseRenameArc', rseRenameArc);
      Storage.WriteString('ResourceString', 'rseRenameFile', rseRenameFile);
      end;
      Storage.Free;
    end;
  end;
  
initialization

  LoadMessages;

finalization

  SaveMessages;
  
end.

