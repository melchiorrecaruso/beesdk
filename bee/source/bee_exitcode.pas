{
  Copyright (c) 2005-2009 Andrew Filinsky and Melchiorre Caruso

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

    TApp class

  Modifyed:

    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

const
  ecSuccesful         = $00000000; { 0 No error                                               }
  ecWarning           = $00000001; { 1 Warning (Non fatal error(s)). For example, one or more }
                                   {   files were locked by some other application, so they   }
                                   {   were not compressed                                    }
  ecError             = $00000002; { 2 Fatal error                                            }
  ecCmdError          = $00000007; { 7 Command line error                                     }
  ecMemError          = $00000008; { 8 Not enough memory for operation                        }
  ecUserAbort         = $000000FF; { 255 User stopped the process                             }


var
  BeeExitCode: longint;

function SetBeeExitCode(Code: longint):longint;

implementation

function SetBeeExitCode(Code: longint):longint;
begin
  if Code > BeeExitCode then
    BeeExitCode := Cose;
end;

end.
