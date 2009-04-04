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

  Modifyed:

    v0.7.9 build 0990 - 2009.04.03 by Melchiorre Caruso;
}

unit Bee_Types;

{$I compiler.inc}

interface

type
  // PFileInfo record ...

  PFileInfo = ^TFileInfo;

  TFileInfo = record
    FileName: PChar;
    FilePath: PChar;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
  end;

type
  // PFileInfoExtra record ...

  PFileInfoExtra = ^TFileInfoExtra;

  TFileInfoExtra = record
    FileName:     PChar;
    FilePath:     PChar;
    FileSize:     cardinal;
    FileTime:     integer;
    FileAttr:     integer;
    FilePacked:   cardinal;
    FileRatio:    cardinal;
    FileComm:     PChar;
    FileCrc:      cardinal;
    FileMethod:   PChar;
    FileVersion:  PChar;
    FilePassword: PChar;
    FilePosition: cardinal;
  end;

type
  // TEvents procedure ...

  TCustomEvent  = procedure of object;
  TMessageEvent = procedure(const aMessage: string) of object;
  TRequestEvent = procedure(const aFileInfo: TFileInfo; var Result: string) of object;
  TListEvent    = procedure(const aFileInfo: TFileInfoExtra) of object;

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

function StringToPChar(const aValue: string): PChar;
function PCharToString(aValue: PChar): string;

implementation

uses
  Classes,
  SysUtils;

function StringToPChar(const aValue: string): PChar;
begin
  Result := StrAlloc(Length(aValue) + 1);
  Result := StrPCopy(Result, aValue);
end;

function PCharToString(aValue: PChar): string;
var
  I: integer;
begin
  SetLength(Result, 0);
  if aValue <> nil then
  begin
    I := StrLen(aValue);
    if I <> 0 then
    begin
      SetLength(Result, I);
      Move(aValue^, Result[1], I);
    end;
  end;
end;

end.
