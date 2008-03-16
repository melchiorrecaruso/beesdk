unit BeeGui_Messages;

{$I compiler.inc}

interface

uses
  Classes,
  IniFiles,
  SysUtils;

var
  rsConfirmation: string = 'Confirmation';
  rsConfirmAbort: string = 'Do you want to abort current process?';
  rsProcessStatus: string = '%d%% Processing...';
  rsProcessTerminated: string = 'Process terminated';

  rsRenameFile: string = 'Rename file';
  rsRenameArchive: string = 'Rename archive';
  
  rsWarning: string = 'Warning';
  rsPasswordNotConfirmed: string = 'Password not confirmed.';

implementation

  procedure LoadMessages;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin

    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');;
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('messages.ini'));

      // ---

      rsConfirmation      := Storage.ReadString('ResourceString', 'rsConfirmation', rsConfirmation);
      rsConfirmAbort      := Storage.ReadString('ResourceString', 'rsConfirmAbort', rsConfirmAbort);
      rsProcessStatus     := Storage.ReadString('ResourceString', 'rsProcessStatus', rsProcessStatus);
      rsProcessTerminated := Storage.ReadString('ResourceString', 'rsProcessTerminated', rsProcessTerminated);
      
      rsRenameFile        := Storage.ReadString('ResourceString', 'rsRenameFile', rsRenameFile);
      rsRenameArchive     := Storage.ReadString('ResourceString', 'rsRenameArchive', rsRenameArchive);
      
      rsWarning              := Storage.ReadString('ResourceString', 'rsWarning', rsWarning);
      rsPasswordNotConfirmed := Storage.ReadString('ResourceString', 'rsPasswordNotConfirmed', rsPasswordNotConfirmed);

      // ---

      Storage.Free;
    end;
  end;

  procedure SaveMessages;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin

    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');;
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('messages.ini'));

      // ---

      Storage.WriteString('ResourceString', 'rsConfirmation', rsConfirmation);
      Storage.WriteString('ResourceString', 'rsConfirmAbort', rsConfirmAbort);
      Storage.WriteString('ResourceString', 'rsProcessStatus', rsProcessStatus);
      Storage.WriteString('ResourceString', 'rsProcessTerminated', rsProcessTerminated);
      
      Storage.WriteString('ResourceString', 'rsRenameFile', rsRenameFile);
      Storage.WriteString('ResourceString', 'rsRenameArchive', rsRenameArchive);
      
      Storage.WriteString('ResourceString', 'rsWarning', rsWarning);
      Storage.WriteString('ResourceString', 'rsPasswordNotConfirmed', rsPasswordNotConfirmed);

      // ---

      Storage.Free;
    end;
  end;
  
initialization

  LoadMessages;

finalization

  SaveMessages;
  
end.

