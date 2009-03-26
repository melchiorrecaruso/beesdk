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

uses
  Classes;

type
  // TFileInfoRec record ...

  TFileInfoA = record
    FileName: string;
    FilePath: string;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
  end;

  // PFileInfoRec record ...

  PCharFileInfoA = record
    FileName: PChar;
    FilePath: PChar;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
  end;

  PPCharFileInfoA = ^PCharFileInfoA;

  // TFileFullInfoRec record ...

  TFileInfoB = record
    FileName:     string;
    FilePath:     string;
    FileSize:     cardinal;
    FileTime:     integer;
    FileAttr:     integer;
    FilePacked:   cardinal;
    FileRatio:    cardinal;
    FileComm:     string;
    FileCrc:      cardinal;
    FileMethod:   string;
    FileVersion:  string;
    FilePassword: string;
    FilePosition: cardinal;
  end;

  // PFileFullInfoRec record ...

  PCharFileInfoB = record
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

  PPCharFileInfoB = ^PCharFileInfoB;

  // TEvents procedure ...

  TOnCustomEvent    = procedure of object;
  TOnMessageEvent   = procedure(const aMessage: string) of object;
  TOnListEvent      = procedure(const aFileInfo: TFileInfoB) of object;
  TOnOverWriteEvent = procedure(const aFileInfo: TFileInfoA; var Result: char) of object;
  TOnRenameEvent    = procedure(const aFileInfo: TFileInfoA; var Result: string) of object;
  TOnKeyEvent       = procedure(const aFileInfo: TFileInfoA; var Result: string) of object;

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
  Strings;

function StringToPChar(const Value: string): PChar;
begin
  Result := stralloc(Length(Value) + 1);
  Result := strpcopy(Result, Value);
end;

function PCharToString(Value: PChar): string;
begin
  Result := strpas(Value);
end;

end.
