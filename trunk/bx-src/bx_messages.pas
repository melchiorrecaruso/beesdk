{
  Copyright (c) 2012-2014 Melchiorre Caruso.

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

{
  Contains:

    Messages and exit status run-time routines.

  Modifyed:

    v1.0.0 build 2225 - 2014.01.26 by Melchiorre Caruso.

}

unit bx_messages;

{$I bx_compiler.inc}

interface

const
  { Messages }

  cmCreating          = 'Creating new archive %s';
  cmListing           = 'Listing %s';
  cmOpening           = 'Opening archive %s';
  cmScanning          = 'Scanning files';

  cmAdding            = 'Adding %s';
  cmCopying           = 'Copying %s';
  cmDecoding          = 'Decoding %s';
  cmDeleting          = 'Deleting %s';
  cmEncoding          = 'Encoding %s';
  cmExtracting        = 'Extracting %s';

  cmRenaming          = 'Renaming %s';
  cmSplitting         = 'Splitting %s';
  cmSwapping          = 'Swapping %s';
  cmTesting           = 'Testing %s';
  cmUpdating          = 'Updating %s';

  { ExitStatus }

  esNoError           =   0;

  esUnknowError       = 101;
  esAllocMemError     = 102;
  esCaseError         = 103;
  esOverrideError     = 104;

  esArchiveTypeError  = 111;
  esArchiveVerError   = 112;
  esArchiveCDError    = 113;

  esHashError         = 131;
  esConfigError       = 132;

  esCmdLineError      = 141;
  esCmdLineACCError   = 142;
  esCmdLineBError     = 143;
  esCmdLineCError     = 144;
  esCmdLineCCError    = 145;
  esCmdLineCDError    = 146;
  esCmdLineCIError    = 147;
  esCmdLineEError     = 148;
  esCmdLineIError     = 149;
  esCmdLineLError     = 150;
  esCmdLinePError     = 151;
  esCmdLineRError     = 152;
  esCmdLineRXError    = 153;
  esCmdLineSFXError   = 154;
  esCmdLineSLError    = 155;
  esCmdLineSSError    = 156;
  esCmdLineTError     = 157;
  esCmdLineUError     = 158;
  esCmdLineVError     = 159;
  esCmdLineVBError    = 160;
  esCmdLineWError     = 161;
  esCmdLineXError     = 162;
  esCmdLineYError     = 163;
  esCmdLineArcError   = 164;
  esCmdLineMaskError  = 165;

  esCreateStreamError = 201;
  esOpenStreamError   = 202;
  esReadStreamError   = 203;
  esWriteStreamError  = 204;
  esFillStreamError   = 205;
  esFlushStreamError  = 206;
  esResizeStreamError = 207;
  esSplitStreamError  = 208;
  esRenameTempError   = 209;
  esRequestDiskError  = 210;
  esStreamTypeError   = 211;
  esCreateDirError    = 212;
  esCreateLinkError   = 213;

  esBeeFilterError    = 231;

  esUserAbortError    = 255;

var
  ExitStatus: longint = esNoError;

  function  GetExitMessage: string;
  procedure SetExitStatus(aExitStatus: longint);

implementation

uses
  SysUtils;

function GetExitMessage: string;
begin
  case ExitStatus of
    esNoError:           Result := 'Everything went ok - time elapsed %s seconds.';

    esUnknowError:       Result := 'Unknow error (exit code #'                      + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esAllocMemError:     Result := 'Allocation memory error (exit code #'           + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCaseError:         Result := 'Case statement error (exit code #'              + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esOverrideError:     Result := 'Override items error (exit code #'              + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';

    esArchiveTypeError:  Result := 'Archive type error (exit code #'                + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esArchiveVerError:   Result := 'Archive version error (exit code #'             + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esArchiveCDError:    Result := 'Archive central directory error (exit code #'   + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';

    esHashError:         Result := 'Check integrity error (exit code #'             + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esConfigError:       Result := 'Load configuration error (exit code #'          + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';

    esCmdLineError:      Result := 'Command line error (exit code #'                + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineACCError:   Result := 'Command line "ACC" switch error (exit code #'   + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineBError:     Result := 'Command line "B" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineCError:     Result := 'Command line "C" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineCCError:    Result := 'Command line "CC" switch error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineCDError:    Result := 'Command line "CD" switch error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineCIError:    Result := 'Command line "CI" switch error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineEError:     Result := 'Command line "E" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineIError:     Result := 'Command line "I" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineLError:     Result := 'Command line "L" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLinePError:     Result := 'Command line "P" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineRError:     Result := 'Command line "R" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineRXError:    Result := 'Command line "RX" switch error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineSFXError:   Result := 'Command line "SFX" switch error (exit code #'   + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineSLError:    Result := 'Command line "SL" switch error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineSSError:    Result := 'Command line "SS" switch error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineTError:     Result := 'Command line "T" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineUError:     Result := 'Command line "U" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineVError:     Result := 'Command line "V" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineVBError:    Result := 'Command line "VB" switch error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineWError:     Result := 'Command line "W" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineXError:     Result := 'Command line "X" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineYError:     Result := 'Command line "Y" switch error (exit code #'     + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineArcError:   Result := 'Command line "archive name" error (exit code #' + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCmdLineMaskError:  Result := 'Command line "file mask" error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';

    esCreateStreamError: Result := 'Create stream error (exit code #'               + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esOpenStreamError:   Result := 'Open stream error (exit code #'                 + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esReadStreamError:   Result := 'Read stream error (exit code #'                 + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esWriteStreamError:  Result := 'Write stream error (exit code #'                + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';

    esFillStreamError:   Result := 'Fill stream error (exit code #'                 + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esFlushStreamError:  Result := 'Flush stream error (exit code #'                + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esResizeStreamError: Result := 'Resize stream error (exit code #'               + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esSplitStreamError:  Result := 'Split stream error (exit code #'                + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esRenameTempError:   Result := 'Rename temporary archive error (exit code #'    + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esRequestDiskError:  Result := 'Request disk error (exit code #'                + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esStreamTypeError:   Result := 'Stream type error (exit code #'                 + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCreateDirError:    Result := 'Create directory error (exit code #'            + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    esCreateLinkError:   Result := 'Create link error (exit code #'                 + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';

    esUserAbortError:    Result := 'User abort error (exit code #'                  + IntToStr(ExitStatus) + ') - time elapsed %s seconds.';
    else                 Result := 'Unknow exit code (exit code #???) - time elapsed %s seconds.';
  end;
end;

procedure SetExitStatus(aExitStatus: longint);
begin
  if ExitStatus = esNoError then
    ExitStatus := aExitStatus;
end;

end.