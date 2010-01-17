{
  Copyright (c) 2005-2010 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1110 - 2010.01.17 by Melchiorre Caruso.
}

unit Bee_Types;

{$I compiler.inc}

interface

type
  { Recursive Mode:
  {  rmNone      No resurse filename                      }
  {  rmWildcard  Recurse olny filename with wildcard      }
  {  rmFull      Recurse all filename                     }

  TRecursiveMode = (rmNone, rmWildCard, rmFull);

  { Update Mode:
  {  umAdd           Add only new files                   }
  {  umUpdate        Update only existing files           }
  {  umReplace       Replace only existing files          }
  {  umAddUpdate     Add and update existing files        }
  {  umAddReplace    Add and replace existing files       }
  {  umAddAutoRename                                      }
  {  umAddQuery      Add and query if file already exists }

  TUpdateMode = (umAdd, umUpdate, umReplace, umAddUpdate,
    umAddReplace, umAddAutoRename, umAddQuery);

  { Overwrite Mode:                                       }
  {  omUpdate                                             }
  {  omAddUpdate                                          }
  {  omReplace                                            }
  {  omAddReplace                                         }
  {  omSkip                                               }
  {  omAddSkip                                            }
  {  omRename                                             }
  {  omAddAutoRename                                      }
  {  omQuit                                               }

  TOverwriteMode = (omAdd, omUpdate, omReplace, omRename,
    omAddUpdate, omAddReplace, omAddAutoRename, omSkip, omQuit);

  { PFileInfo packed record }

  PFileInfo = ^TFileInfo;

  TFileInfo = packed record
    FileName: PChar;
    FilePath: PChar;
    FileSize: int64;
    FileTime: longint;
    FileAttr: longint;
  end;

  { PFileInfoExtra packed record }

  PFileInfoExtra = ^TFileInfoExtra;

  TFileInfoExtra = packed record
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

function  StringToPChar(const aValue: string): PChar;
function  PCharToString(aValue: PChar): string;
procedure FreePChar(var aValue: PChar);

implementation

uses
  SysUtils;

function StringToPChar(const aValue: string): PChar; inline;
begin
  Result := StrAlloc(Length(aValue) + 1);
  Result := StrPCopy(Result, aValue);
end;

{ TODO : DA CONTROLLARE E OTTIMIZZARE }
function PCharToString(aValue: PChar): string; inline;
var
  i: longint;
begin
  if aValue <> nil then
  begin
    i := StrLen(aValue);
    if i > 0 then
    begin
      SetLength(Result, i);
      Move(aValue^, Result[1], i);
    end;
  end else
    SetLength(Result, 0);
end;

procedure FreePChar(var aValue: PChar); inline;
begin
  StrDispose(aValue);
  aValue := nil;
end;

end.
