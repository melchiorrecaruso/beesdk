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
  esNoError           =   0;  emNoError           = 'Everything went ok - time elapsed %s seconds.';
  esUnknowError       =   1;  emUnknowError       = 'Unknow error (exit code #001) - time elapsed %s seconds.';
  esCmdLineError      =   7;  emCmdLineError      = 'Command line error (exit code #007) - time elapsed %s seconds.';

  esAllocMemError     =   8;  emAllocMemError     = 'Allocation memory error (exit code #008) - time elapsed %s seconds.';

  esCreateStreamError =  13;  emCreateStreamError = 'Create stream error (exit code #013) - time elapsed %s seconds.';
  esOpenStreamError   =  12;  emOpenStreamError   = 'Open stream error (exit code #012) - time elapsed %s seconds.';
  esFillStreamError   =   9;  emFillStreamError   = 'Fill stream error (exit code #009) - time elapsed %s seconds.';
  esFlushStreamError  =  10;  emFlushStreamError  = 'Flush stream error (exit code #010) - time elapsed %s seconds.';
  esResizeStreamError =  11;  emResizeStreamError = 'Resize stream error (exit code #011) - time elapsed %s seconds.';
  esSplitStreamError  =  15;  emSplitStreamError  = 'Split stream error (exit code #015) - time elapsed %s seconds.';
  esRenameTempError   =  21;  emRenameTempError   = 'Rename temporary archive error (exit code #021) - time elapsed %s seconds.';





  esCRCError          =  18;  emCRCError          = 'Data CRC error (exit code #018) - time elapsed %s seconds.';

  esArchiveTypeError  =  14;  emArchiveTypeError  = 'Archive type error (exit code #014) - time elapsed %s seconds.';




  esUserAbortError    = 255;  emUserAbortError    = 'User abort error (exit code #255) - time elapsed %s seconds.';



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
