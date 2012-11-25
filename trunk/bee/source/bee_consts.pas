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

    v0.8.0 build 1280 - 2011.04.12 by Melchiorre Caruso.
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

  { Messages }

  cmOpening           = 'Opening    %s';
  cmScanning          = 'Scanning   %s';
  cmAdding            = 'Adding     %s';
  cmUpdating          = 'Updating   %s';
  cmCopying           = 'Copying    %s';
  cmSplitting         = 'Splitting  %s';
  cmEncoding          = 'Encoding   %s';
  cmExtracting        = 'Extracting %s';
  cmTesting           = 'Testing    %s';
  cmDecoding          = 'Decoding   %s';
  cmDeleting          = 'Deleting   %s';
  cmChecking          = 'Checking   %s';
  cmListing           = 'Listing    %s';
  cmSwapping          = 'Swapping   %s';
  cmRenaming          = 'Renaming   %s';

  cmUnknow            = 'Process aborted, unknow error - time elapsed %s seconds.';
  cmSuccesful         = 'Everything went ok - time elapsed %s seconds.';
  cmWarning           = 'Warning occurred - time elapsed %s seconds.';
  cmError             = 'Process aborted - time elapsed %s seconds.';
  cmUserAbort         = 'User stopped the process - time elapsed %s seconds.';
  cmCmdError          = 'Command line error.';

  cmMemError          = 'Error: not enough memory for operation';
  cmArcTypeError      = 'Error: archive type unsupported';
  cmOpenArcError      = 'Error: can''t open archive "%s"';
  cmOpenFileError     = 'Error: can''t open file "%s"';
  cmOpenTempError     = 'Error: can''t open temp file';
  cmCreateSwapError   = 'Error: can''t create swap file';
  cmOpenSwapError     = 'Error: can''t open swap file';
  cmStrmReadError     = 'Error: can''t read data from stream';
  cmStrmWriteError    = 'Error: can''t write data to stream';

  cmSplitArcError     = 'Error: can''t split temporary archive "%s"';
  cmRenameFileError   = 'Error: can''t rename file "%s" to "%s"';
  cmSequenceError     = 'Error: can''t decode file "%s"';
  cmActionError       = 'Error: internal error';
  cmStreamError       = 'Error: can''t open stream';
  cmTestPswError      = 'Error: wrong password';
  cmCrcError          = 'Error: wrong CRC decoding "%s"';

  cmConfigError       = 'Error: configuration file "%s" not found, data will be stored';
  cmFileExistsWarning = 'Warning: file "%s" already exists';
  cmNoFilesWarning    = 'Warning: no files to process';

implementation

end.
