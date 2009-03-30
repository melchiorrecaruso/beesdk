{
  Copyright (c) 2005-2008 Andrew Filinsky and Melchiorre Caruso

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

  v0.7.9 build 0890 - 2008.10.18 by Melchiorre Caruso;
}

unit Bee_Types;

{$I compiler.inc}

interface

type
  // TFileInfo record ...
  TFileInfo = record
    FileName: PChar;
    FilePath: PChar;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
  end;
  PFileInfo = ^TFileInfo;

  // TFileInfoExtra record ...
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
  PFileInfoExtra = ^TFileInfoExtra;

  // TEvents procedure ...

  TCustomEvent   = procedure of object;
  TMessageEvent  = procedure(const aMessage: string) of object;
  TRequestEvent  = procedure(const aFileInfo: TFileInfo; var Result: string) of object;
  TListEvent     = procedure(const aFileInfo: TFileInfoExtra) of object;

const
  // CoreStatus ...

  csUnknow           = -1;
  csReady            =  0;
  csExecuting        =  1;
  csWaitingOverwrite =  2;
  csWaitingRename    =  3;
  csWaitingKey       =  4;
  csWaitingRequest   =  5;
  csTerminated       =  6;

const
  // CoreExitCode

  esUnknow    = -1;
  esSuccesful =  0;
  esWarning   =  1;
  esError     =  2;

function StringToPChar(const Value: string): PChar;
function PCharToString(Value: PChar): string;

implementation

uses
  Classes,
  SysUtils;

function StringToPChar(const Value: string): PChar;
begin
  Result := StrAlloc(Length(Value) + 1);
  Result := StrPCopy(Result, Value);
end;

function PCharToString(Value: PChar): string;
var
  I: integer;
begin
  Result := '';
  if Value <> nil then
  begin
    I := StrLen(Value);
    if I > 0 then
    begin
      SetLength(Result, I);
      Move(Value[0], Result[1], I);
    end;
  end;
end;

end.
