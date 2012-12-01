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
  ecCmdError          = 7;   { 7 Command line error              }
  ecMemError          = 8;   { 8 Not enough memory for operation }
  ecCustError         = 9;
  ecUserAbort         = 255; { 255 User stopped the process      }


const
  emUnknow            = 'Process aborted (error #%u) - time elapsed %s seconds.';
  emSuccesful         = 'Everything went ok - time elapsed %s seconds.';
  emWarning           = 'Warning occurred - time elapsed %s seconds.';
  emUserAbort         = 'User stopped the process - time elapsed %s seconds.';
  emCmdError          = 'Command line error.';

  emMemError          = 'Error: not enough memory for operation';
  emArcTypeError      = 'Error: archive type unsupported';
  emOpenArcError      = 'Error: can''t open archive "%s"';
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

implementation

end.
