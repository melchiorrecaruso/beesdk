{
  Copyright (c) 2005-2012 Andrew Filinsky and Melchiorre Caruso

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

  Modifyed:

}

unit Bee_Interface;

{$I compiler.inc}

interface


const
  ecNoError           = 0;
  emNoError           = 'Everything went ok - time elapsed %s seconds.';

  ecUnknowError       = 1;
  emUnknowError       = 'Unknow error (exit code #001) - time elapsed %s seconds.';

  ecCmdLineError      = 7;
  emCmdLineError      = 'Command line error (exit code #007) - time elapsed %s seconds.';

  ecAllocMemError     = 8;
  emAllocMemError     = 'Allocation memory error (exit code #008) - time elapsed %s seconds.';

  ecCreateStreamError = 13;
  emCreateStreamError = 'Create stream error (exit code #013) - time elapsed %s seconds.';

  ecOpenStreamError   = 12;
  emOpenStreamError   = 'Open stream error (exit code #012) - time elapsed %s seconds.';

  ecFillStreamError   = 9;
  emFillStreamError   = 'Fill stream error (exit code #009) - time elapsed %s seconds.';

  ecFlushStreamError  = 10;
  emFlushStreamError  = 'Flush stream error (exit code #010) - time elapsed %s seconds.';

  ecResizeStreamError = 11;
  emResizeStreamError = 'Resize stream error (exit code #011) - time elapsed %s seconds.';

  ecSplitStreamError  = 15;
  emSplitStreamError  = 'Split stream error (exit code #015) - time elapsed %s seconds.';

  ecArchiveTypeError  = 14;
  emArchiveTypeError  = 'Archive type error (exit code #014) - time elapsed %s seconds.';

  ecUserAbortError    = 255;
  emUserAbortError    = 'User abort error (exit code #255) - time elapsed %s seconds.';

















const
















  emOpenFileError     = 'Error: can''t open file "%s"';
  emOpenTempError     = 'Error: can''t open temp file';
  emCreateSwapError   = 'Error: can''t create swap file';
  emOpenSwapError     = 'Error: can''t open swap file';
  emStrmReadError     = 'Error: can''t read data from stream';
  emStrmWriteError    = 'Error: can''t write data to stream';

  emSplitArcError     = 'Error: can''t split temporary archive "%s"';
  emRenameFileError   = 'Error: can''t rename file "%s" to "%s"';
  emSequenceError     = 'Error: can''t decode file "%s"';
  emActionError       = 'Error: internal error';
  emStreamError       = 'Error: can''t open stream';
  emTestPswError      = 'Error: wrong password';
  emCrcError          = 'Error: wrong CRC decoding "%s"';

  emConfigError       = 'Error: configuration file "%s" not found, data will be stored';
  emFileExistsWarning = 'Warning: file "%s" already exists';
  emNoFilesWarning    = 'Warning: no files to process';

var
  // ExitCode: longint = ecNoError;
  ExitMessage:  string = '';

  procedure SetExitCode(Value: longint);

implementation

  procedure SetExitCode(Value: longint);
  begin
    if ExitCode = ecNoError then
      ExitCode := Value;
  end;

  procedure SetExitMessage(const Value: string);
  begin
    if ExitMessage = '' then
      ExitMessage := Value;
  end;

end.
