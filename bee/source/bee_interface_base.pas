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

unit Bee_Interface_Base;

{$I compiler.inc}

interface

uses
  Classes;

type
  // TFileInfoRec record ...

  TFileInfoRec = record
    FileName: string;
    FilePath: string;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
  end;

  // TFileFullInfoRec record ...

  TFileFullInfoRec = record
    FileName: string;
    FilePath: string;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
    FilePacked: cardinal;
    FileRatio: cardinal;
    FileComm: string;
    FileCrc: cardinal;
    FileMethod: string;
    FileVersion: string;
    FilePassword: string;
    FilePosition: cardinal;
  end;

  // TEvents procedure ...

  TOnCustomEvent    = procedure of object;
  TOnMessageEvent   = procedure(const aMessage: string) of object;
  TOnListEvent      = procedure(const aFileInfo: TFileFullInfoRec) of object;
  TOnOverWriteEvent = procedure(const aFileInfo: TFileInfoRec; var Result: char) of object;
  TOnRenameEvent    = procedure(const aFileInfo: TFileInfoRec; var Result: string) of object;
  TOnKeyEvent       = procedure(const aFileInfo: TFileInfoRec; var Result: string) of object;

  // TCoreStatus ...

  TCoreStatus = (csReady, csExecuting, csWaitingOverwrite,
    csWaitingRename, csWaitingKey, csWaitingRequest, csTerminated);

implementation

end.
