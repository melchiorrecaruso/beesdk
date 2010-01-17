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

  msgUpdating    = 'Updating   ';
  msgFreshing    = 'Freshing   ';
  msgReplacing   = 'Replacing  ';
  msgExtracting  = 'Extracting ';
  msgTesting     = 'Testing    ';
  msgSkipping    = 'Skipping   ';
  msgEncoding    = 'Encoding   ';
  msgDecoding    = 'Decoding   ';
  msgCopying     = 'Copying    ';
  msgMoving      = 'Moving     ';
  msgDeleting    = 'Deleting   ';
  msgScanning    = 'Scanning   ';
  msgOpening     = 'Opening    ';
  msgListing     = 'Listing    ';
  msgRenaming    = 'Renaming   ';
  msgRename      = 'Rename     ';
  msgAdding      = 'Adding     ';
  msgCRCERROR    = 'CRC-ERROR  ';
  msgFailed      = 'Failed     ';

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
