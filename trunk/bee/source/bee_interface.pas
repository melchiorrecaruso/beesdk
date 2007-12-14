{
  Copyright (c) 2005-2007 Andrew Filinsky and Melchiorre Caruso

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

  TApp, abstract class of Thread.

  Modifyed:

  v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

  v0.7.9 build 0527 - 2007.12.03 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  {$IFDEF THREADSYNCHRONIZER}
  Bee_Interface_Base,
  {$ENDIF}
  Classes;

// TAppItem class

type
  TAppItem = class
  public
    FileName: string;
    FilePath: string;
    FileType: string;
    FileSize: integer;
    FilePacked: integer;
    FileRatio: integer;
    FileAttr: integer;
    FileTime: integer;
    FileComm: string;
    FileCrc:  cardinal;
    FileMethod: string;
    FileVersion: string;
    FilePassword: string;
    FilePosition: integer;
    FileIcon: integer;
  end;

// TAppInterface class

type
  TAppInterface = class
  public
    cMsg:  string;
    cList: TList;
    cFileName: string;
    cFileSize: integer;
    cFileTime: integer;
    cPercentage: integer;
  public
    OnFatalError: TThreadMethod;
    OnOverWrite: TThreadMethod;
    OnRename: TThreadMethod;
    OnWarning: TThreadMethod;
    OnDisplay: TThreadMethod;
    OnRequest: TThreadMethod;
    OnError: TThreadMethod;
    OnList: TThreadMethod;
    OnTick: TThreadMethod;
    OnClear: TThreadMethod;
    OnKey: TThreadMethod;
  end;
  
// TApp class

type
  {$IFDEF THREADSYNCHRONIZER}
  TApp = class(TThreadEx)
  {$ELSE}
  TApp = class(TThread)
  {$ENDIF}
  protected
    AppParams: TStringList;
  public
    AppInterface: TAppInterface;
  public
    constructor Create(aAppInterface: TAppInterface;
      aAppParams: TStringList; aAppTerminate: TNotifyEvent);
    procedure Syn(aMethod: TThreadMethod);
    destructor Destroy; override;
  end;

implementation

// TApp class ...

constructor TApp.Create(aAppInterface: TAppInterface;
  aAppParams: TStringList; aAppTerminate: TNotifyEvent);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  AppParams := aAppParams;
  AppInterface := aAppInterface;
  if Assigned(aAppTerminate) then
  begin
    OnTerminate := aAppTerminate;
  end;
end;

destructor TApp.Destroy;
begin
  AppInterface := nil;
  AppParams := nil;
  inherited Destroy;
end;

procedure TApp.Syn(aMethod: TThreadMethod);
begin
  {$IFDEF THREADSYNCHRONIZER}
    Synchronizer.Synchronize(aMethod);
  {$ELSE}
  inherited Synchronize(aMethod);
  {$ENDIF}
end;

end.
