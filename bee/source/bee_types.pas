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

    Types definitions

  Modifyed:

    v0.8.0 build 1083 - 2009.11.16 by Melchiorre Caruso;
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
    FileSize: int64;
    FileTime: longint;
    FileAttr: longint;
  end;

type
  // PFileInfoExtra record ...

  PFileInfoExtra = ^TFileInfoExtra;

  TFileInfoExtra = record
    FileName:     PChar;
    FilePath:     PChar;
    FileSize:     int64;
    FileTime:     longint;
    FileAttr:     longint;
    FilePacked:   int64;
    FileRatio:    longint;
    FileComm:     PChar;
    FileCrc:      longword;
    FileMethod:   PChar;
    FileVersion:  PChar;
    FilePassword: PChar;
    FilePosition: longint;
  end;

function StringToPChar(const aValue: string): PChar;
function PCharToString(aValue: PChar): string;
procedure FreePChar(var aValue: PChar);

implementation

uses
  SysUtils;

function StringToPChar(const aValue: string): PChar; inline;
begin
  Result := StrAlloc(Length(aValue) + 1);
  Result := StrPCopy(Result, aValue);
end;

function PCharToString(aValue: PChar): string; inline;
var
  I: longint;
begin
  SetLength(Result, 0);
  if aValue <> nil then
  begin
    I := StrLen(aValue);
    if I > 0 then
    begin
      SetLength(Result, I);
      Move(aValue^, Result[1], I);
    end;
  end;
end;

procedure FreePChar(var aValue: PChar); inline;
begin
  StrDispose(aValue);
  aValue := nil;
end;

end.
