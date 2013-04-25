{
  Copyright (c) 2013 Melchiorre Caruso.

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

    ExitStaus, run time application exit status.

  Modifyed:

    v0.8.0 build 1864 - 2013.02.15 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I bee_compiler.inc}

interface

const
  esNoError           =   0;  emNoError           = 'Everything went ok - time elapsed %s seconds.';
  esUnknowError       = 101;  emUnknowError       = 'Unknow error (exit code #101) - time elapsed %s seconds.';
  esCmdLineError      = 102;  emCmdLineError      = 'Command line error (exit code #102) - time elapsed %s seconds.';
  esAllocMemError     = 103;  emAllocMemError     = 'Allocation memory error (exit code #103) - time elapsed %s seconds.';
  esUserAbortError    = 104;  emUserAbortError    = 'User abort error (exit code #104) - time elapsed %s seconds.';

  esCreateStreamError = 201;  emCreateStreamError = 'Create stream error (exit code #201) - time elapsed %s seconds.';
  esOpenStreamError   = 202;  emOpenStreamError   = 'Open stream error (exit code #202) - time elapsed %s seconds.';
  esFillStreamError   = 203;  emFillStreamError   = 'Fill stream error (exit code #203) - time elapsed %s seconds.';
  esFlushStreamError  = 204;  emFlushStreamError  = 'Flush stream error (exit code #204) - time elapsed %s seconds.';
  esResizeStreamError = 205;  emResizeStreamError = 'Resize stream error (exit code #205) - time elapsed %s seconds.';
  esSplitStreamError  = 206;  emSplitStreamError  = 'Split stream error (exit code #206) - time elapsed %s seconds.';
  esRenameTempError   = 207;  emRenameTempError   = 'Rename temporary archive error (exit code #207) - time elapsed %s seconds.';
  esRequestDiskError  = 208;  emRequestDiskError  = 'Request disk error (exit code #208) - time elapsed %s seconds.';

  esArchiveTypeError  = 301;  emArchiveTypeError  = 'Archive type error (exit code #301) - time elapsed %s seconds.';
  esArchiveVerError   = 302;  emArchiveVerError   = 'Archive version error (exit code #302) - time elapsed %s seconds.';
  esArchiveCDError    = 303;  emArchiveCDError    = 'Archive central directory error (exit code #303) - time elapsed %s seconds.';

  esHashError         = 401;  emHashError         = 'Check integrity error (exit code #401) - time elapsed %s seconds.';

  esLoadConfigError   = 501;  emLoadConfigError   = 'Load configuration error (exit code #501) - time elapsed %s seconds.';


var
  ExitStatus: longint = esNoError;

  procedure SetExitStatus(aExitStatus: longint);

implementation

  procedure SetExitStatus(aExitStatus: longint);
  begin
    if ExitStatus = esNoError then
      ExitStatus := aExitStatus;
  end;

end.
