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
  ecNoError           = 0;  { 1 No error                         }
  ecUnknowError       = 1;
  ecCmdLineError      = 7;   { 7 Command line error              }
  ecMemError          = 8;   { 8 Not enough memory for operation }

  ecArchiveTypeError  = 14;
  ecCreateStreamError = 13;








  ecOpenStreamError   = 12;
  ecFillStreamError   = 9;
  ecFlushStreamError  = 10;
  ecResizeStreamError = 11;
  ecSplittingError    = 15;


  ecUserAbort         = 255; { 255 User stopped the process      }


const
  emNoError           = 'Everything went ok - time elapsed %s seconds.';
  emUnknowError       = 'Unknow error (exit code #%.5u) - time elapsed %s seconds.';
  emCmdLineError      = 'Command line error (exit code #%.5u) - time elapsed %s seconds.';
  emMemError          = 'Allocation memory error (exit code #%.5u) - time elapsed %s seconds.';
  emArcTypeError      = 'Archive type error (exit code #%.5u) - time elapsed %s seconds.';


  emOpenArcError      = 'Error: can''t open archive "%s"';


  emUserAbort         = 'User stopped the process - time elapsed %s seconds.';





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

  procedure SetExitCode(Value: longint);

implementation

  procedure SetExitCode(Value: longint);
  begin
    if ExitCode = ecNoError then
    begin
      ExitCode := Value;
    end;
  end;

end.
