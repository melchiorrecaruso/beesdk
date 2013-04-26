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

    v0.8.0 build 1970 - 2013.04.26 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I bee_compiler.inc}

interface

const
  esNoError           =   0;
  esCmdLineError      = 101;
  esAllocMemError     = 102;
  esUnknowError       = 103;

  esArchiveTypeError  = 111;
  esArchiveVerError   = 112;
  esArchiveCDError    = 113;

  esHashError         = 131;
  esConfigError       = 132;

  esCreateStreamError = 201;
  esOpenStreamError   = 202;
  esFillStreamError   = 203;
  esFlushStreamError  = 204;
  esResizeStreamError = 205;
  esSplitStreamError  = 206;
  esRenameTempError   = 207;
  esRequestDiskError  = 208;

  esUserAbortError    = 255;

var
  ExitStatus: longint = esNoError;

  function  GetExitMessage: string;
  procedure SetExitStatus(aExitStatus: longint);

implementation

  function GetExitMessage: string;
  begin
    case ExitStatus of
      esNoError:           Result := 'Everything went ok - time elapsed %s seconds.';
      esCmdLineError:      Result := 'Command line error (exit code #101) - time elapsed %s seconds.';
      esAllocMemError:     Result := 'Allocation memory error (exit code #102) - time elapsed %s seconds.';
      esUnknowError:       Result := 'Unknow error (exit code #103) - time elapsed %s seconds.';

      esArchiveTypeError:  Result := 'Archive type error (exit code #111) - time elapsed %s seconds.';
      esArchiveVerError:   Result := 'Archive version error (exit code #112) - time elapsed %s seconds.';
      esArchiveCDError:    Result := 'Archive central directory error (exit code #113) - time elapsed %s seconds.';

      esHashError:         Result := 'Check integrity error (exit code #131) - time elapsed %s seconds.';
      esConfigError:       Result := 'Load configuration error (exit code #132) - time elapsed %s seconds.';

      esCreateStreamError: Result := 'Create stream error (exit code #201) - time elapsed %s seconds.';
      esOpenStreamError:   Result := 'Open stream error (exit code #202) - time elapsed %s seconds.';
      esFillStreamError:   Result := 'Fill stream error (exit code #203) - time elapsed %s seconds.';
      esFlushStreamError:  Result := 'Flush stream error (exit code #204) - time elapsed %s seconds.';
      esResizeStreamError: Result := 'Resize stream error (exit code #205) - time elapsed %s seconds.';
      esSplitStreamError:  Result := 'Split stream error (exit code #206) - time elapsed %s seconds.';
      esRenameTempError:   Result := 'Rename temporary archive error (exit code #207) - time elapsed %s seconds.';
      esRequestDiskError:  Result := 'Request disk error (exit code #208) - time elapsed %s seconds.';

      esUserAbortError:    Result := 'User abort error (exit code #255) - time elapsed %s seconds.';
      else                 Result := 'Unknow exit code (exit code #???) - time elapsed %s seconds.';
    end;
  end;

  procedure SetExitStatus(aExitStatus: longint);
  begin
    if ExitStatus = esNoError then
    begin
      ExitStatus := aExitStatus;
    end;
  end;

end.
