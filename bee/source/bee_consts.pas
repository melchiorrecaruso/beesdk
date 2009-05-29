{
  Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1030 - 2009.04.19 by Melchiorre Caruso.
}

unit Bee_Consts;

{$I compiler.inc}

interface

const
  // Id marker

  Marker: longint = 442852674;

const
  // Version number

  ver02 = 0; // Bee 0.2.xx
  ver03 = 1; // Bee 0.3.xx
  ver04 = 2; // Bee 0.8.xx

const
  // CoreStatus ...

  csUnknow           = -1;
  csReady            =  0;
  csExecuting        =  1;
  csWaitingOverwrite =  2;
  csWaitingRename    =  3;
  csWaitingPassword  =  4;
  csWaitingRequest   =  5;
  csTerminated       =  6;

const
  // CoreCode

  ccUnknow    = -1;
  ccSuccesful =  0;
  ccWarning   =  1;
  ccError     =  2;

const
  // CorePriority

  cpUnknow       = -1;
  cpIdle         =  0;
  cpLowest       =  1;
  cpLower        =  2;
  cpNormal       =  3;
  cpHigher       =  4;
  cpHighest      =  5;
  cpTimeCritical =  6;

implementation

end.
