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

    v0.8.0 build 1110 - 2010.01.17 by Melchiorre Caruso.
}

unit Bee_Consts;

{$I compiler.inc}

interface

const
  Cr = #13#10;

  { Id marker }

  Marker:longint = $1A656542;

  { Default file names }

  DefaultCfgName = 'bee.ini';
  DefaultSfxName = 'bee.sfx';

  { Display message strings }

  msgUpdating    = 'Updating   %s';
  msgFreshing    = 'Freshing   %s';
  msgReplacing   = 'Replacing  %s';
  msgExtracting  = 'Extracting %s';
  msgTesting     = 'Testing    %s';
  msgSkipping    = 'Skipping   %s';
  msgEncoding    = 'Encoding   %s';
  msgDecoding    = 'Decoding   %s';
  msgCopying     = 'Copying    %s';
  msgMoving      = 'Moving     %s';
  msgDeleting    = 'Deleting   %s';
  msgScanning    = 'Scanning   %s';
  msgOpening     = 'Opening    %s';
  msgListing     = 'Listing    %s';
  msgRenaming    = 'Renaming   %s';
  msgRename      = 'Rename     %s';
  msgAdding      = 'Adding     %s';
  msgCRCERROR    = 'CRC-ERROR  %s';
  msgFailed      = 'Failed     %s';

  { CoreStatus }

  csUnknow           = $FFFFFFFF;
  csReady            = $00000000;
  csExecuting        = $00000001;
  csWaitingOverwrite = $00000002;
  csWaitingRename    = $00000003;
  csWaitingPassword  = $00000004;
  csWaitingRequest   = $00000005;
  csWaitingList      = $00000006;
  csTerminated       = $00000007;
  csSuspended        = $00000008;

  { CoreCode }

  ccUnknow           = $FFFFFFFF;
  ccSuccesful        = $00000000; { 0 No error                                               }
  ccWarning          = $00000001; { 1 Warning (Non fatal error(s)). For example, one or more }
                                  {   files were locked by some other application, so they   }
                                  {   were not compressed                                    }
  ccError            = $00000002; { 2 Fatal error                                            }
  ccCmdError         = $00000007; { 7 Command line error                                     }
  ccMemError         = $00000008; { 8 Not enough memory for operation                        }
  ccUserAbort        = $000000FF; { 255 User stopped the process                             }

  { CoreMessage }

  cmUnknow           = 'Process aborted, unknow error - time elapsed %s seconds.';
  cmSuccesful        = 'Archive size %s bytes, time elapsed %s seconds.';
  cmWarning          = 'Warning occurred - archive size %s bytes, time elapsed %s seconds.';
  cmError            = 'Process aborted - time elapsed %s seconds.';
  cmCmdError         = 'Process aborted, command line error - time elapsed %s seconds.';
  cmMemError         = 'Process aborted, not enough memory for operation - time elapsed %s seconds.';
  cmUserAbort        = 'Process aborted, user stopped the process - time elapsed %s seconds.';

  cmwConfigFile      = 'Warning: configuration file "%s" not found, data will be stored';
  cmwFileExists      = 'Warning: file "%s" already exists';

  cmeArcType         = 'Error: archive type unsupported';
  cmeArcOpen         = 'Error: can''t open archive "%s"';
  cmeSwapOpen        = 'Error: can''t open swap file';
  cmeFileOpen        = 'Error: can''t open file "%s"';
  cmeStrmOpen        = 'Error: cant'' open stream';
  cmeTempOpen        = 'Error: can''t open temp file';
  cmeRename          = 'Error: can''t rename file "%s" to "%s"';
  cmeSwap            = 'Error: can''t decode solid sequence';

  { CorePriority }

  cpUnknow           = $FFFFFFFF;
  cpIdle             = $00000000;
  cpLowest           = $00000001;
  cpLower            = $00000002;
  cpNormal           = $00000003;
  cpHigher           = $00000004;
  cpHighest          = $00000005;
  cpTimeCritical     = $00000006;

implementation

end.
