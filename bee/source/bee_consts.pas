{
  Copyright (c) 2003-2010 Andrew Filinsky and Melchiorre Caruso

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

    Consts definitions

  Modifyed:

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
}

unit Bee_Consts;

{$I compiler.inc}

interface

const
  Cr = #13#10;

  { Id marker }

  Marker: longint     = $1A656542;

  { Default file names }

  DefaultCfgName      = 'bee.ini';
  DefaultSfxName      = 'bee.sfx';

  { Core Get/Set messages }

  csmExecute          = 101;
  csmSuspend          = 102;
  csmResume           = 103;
  csmTerminate        = 104;
  csmDestroy          = 105;

  csmStatus           = 201;
  csmCode             = 202;
  csmSpeed            = 203;
  csmPercentage       = 204;
  csmTime             = 205;
  csmRemainingTime    = 206;

  csmSize             = 301;
  csmProcessedSize    = 302;

  csmMessage          = 401;
  csmItem             = 402;

  csmWaitingOverwrite  = 501;
  csmWaitingRename     = 502;
  csmWaitingPassword   = 503;
  csmWaitingRequest    = 504;
  csmWaitingList       = 505;

  csmPriority             = 601;
  csmPriorityIdle         = 602;
  csmPriorityLowest       = 603;
  csmPriorityLower        = 604;
  csmPriorityNormal       = 605;
  csmPriorityHigher       = 606;
  csmPriorityHighest      = 607;
  csmPriorityTimeCritical = 608;

  { CoreStatus }

  csReady             = $00000000;
  csExecuting         = $00000001;
  csWaitingOverwrite  = $00000002;
  csWaitingRename     = $00000003;
  csWaitingPassword   = $00000004;
  csWaitingRequest    = $00000005;
  csWaitingList       = $00000006;
  csTerminated        = $00000007;
  csSuspended         = $00000008;

  { CoreCode }

  ccSuccesful         = $00000000; { 0 No error                                               }
  ccWarning           = $00000001; { 1 Warning (Non fatal error(s)). For example, one or more }
                                   {   files were locked by some other application, so they   }
                                   {   were not compressed                                    }
  ccError             = $00000002; { 2 Fatal error                                            }
  ccCmdError          = $00000007; { 7 Command line error                                     }
  ccMemError          = $00000008; { 8 Not enough memory for operation                        }
  ccUserAbort         = $000000FF; { 255 User stopped the process                             }

  { CoreMessage }

  cmOpening           = 'Opening    %s';
  cmScanning          = 'Scanning   %s';
  cmAdding            = 'Adding     %s';
  cmUpdating          = 'Updating   %s';
  cmCopying           = 'Copying    %s';
  cmEncoding          = 'Encoding   %s';
  cmExtracting        = 'Extracting %s';
  cmTesting           = 'Testing    %s';
  cmDecoding          = 'Decoding   %s';
  cmCrcError          = 'CRC error  %s';
  cmDeleting          = 'Deleting   %s';
  cmChecking          = 'Checking   %s';
  cmListing           = 'Listing    %s';

  cmUnknow            = 'Process aborted, unknow error - time elapsed %s seconds.';
  cmSuccesful         = 'Everything went ok - time elapsed %s seconds.';
  cmWarning           = 'Warning occurred - time elapsed %s seconds.';
  cmError             = 'Process aborted - time elapsed %s seconds.';
  cmUserAbort         = 'User stopped the process - time elapsed %s seconds.';

  cmCmdError          = 'Error: command line error';
  cmMemError          = 'Error: not enough memory for operation';
  cmArcTypeError      = 'Error: archive type unsupported';
  cmOpenArcError      = 'Error: can''t open archive "%s"';
  cmOpenFileError     = 'Error: can''t open file "%s"';
  cmOpenTempError     = 'Error: can''t open temp file';
  cmCreateSwapError   = 'Error: can''t create swap file';
  cmOpenSwapError     = 'Error: can''t open swap file';
  cmStrmReadError     = 'Error: can''t read data from stream';
  cmRenameFileError   = 'Error: can''t rename file "%s" to "%s"';
  cmSequenceError     = 'Error: can''t decode file "%s"';
  cmActionError       = 'Error: internal error';
  cmStreamError       = 'Error: can''t open stream';
  cmTestPswError      = 'Error: wrong password';

  cmConfigWarning     = 'Warning: configuration file "%s" not found, data will be stored';
  cmFileExistsWarning = 'Warning: file "%s" already exists';
  cmNoFilesWarning    = 'Warning: no files to process';

implementation

end.
