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

  v0.7.9 build 0551 - 2007.12.18 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  {$IFDEF THREADSYNCHRONIZER}
  Bee_Interface_Base,
  {$ENDIF}
  Classes;
  
// TAppEventData records

type
  TAppFatalErrorData = record
    Msg: string;
  end;

  TAppOverWriteData = record
    FileName: string;
    FilePath: string;
    FileTime: integer;
    FileSize: integer;
  end;
  
  TAppRenameData = record
    FileName: string;
    FilePath: string;
    FileTime: integer;
    FileSize: integer;
  end;
  
  TAppWarningData = record
    Msg: string;
  end;
  
  TAppDisplayData = record
    Msg: string;
  end;
  
  TAppRequestData = record
    Msg: string;
  end;
  
  TAppErrorData = packed record
    Msg: string;
  end;
  
  TAppListData = record
    FileName: string;
    FilePath: string;
    FileSize: integer;
    FilePack: integer;
    FileAttr: integer;
    FileTime: integer;
    FileComm: string;
    FileCrc: cardinal;
    FileMethod: string;
    FileVersion: string;
    FilePassword: string;
    FilePosition: integer;
  end;
  
  TAppTickData = record
    Percentage: integer;
  end;
  
  TAppClearData = record
    Msg: string;
  end;
  
  TAppKeyData =  record
    FileName: string;
    FilePath: string;
    FileTime: integer;
    FileSize: integer;
  end;
  
// TAppEvent records
  
type
  TAppFatalErrorEvent = record
    Method: TThreadMethod;
    Data: TAppFatalErrorData;
    Answer: string;
  end;
  
  TAppOverWriteEvent = record
    Method: TThreadMethod;
    Data: TAppOverWriteData;
    Answer: char;
  end;
  
  TAppRenameEvent = record
    Method: TThreadMethod;
    Data: TAppRenameData;
    Answer: string;
  end;

  TAppWarningEvent = record
    Method: TThreadMethod;
    Data: TAppWarningData;
    Answer: string;
  end;

  TAppDisplayEvent = record
    Method: TThreadMethod;
    Data: TAppDisplayData;
    Answer: string;
  end;
  
  TAppRequestEvent = record
    Method: TThreadMethod;
    Data: TAppRequestData;
    Answer: string;
  end;
  
  TAppErrorEvent = record
    Method: TThreadMethod;
    Data: TAppErrorData;
    Answer: string;
  end;
  
  TAppListEvent = record
    Method: TThreadMethod;
    Data: TAppListData;
    Answer: string;
  end;
  
  TAppTickEvent = record
    Method: TThreadMethod;
    Data: TAppTickData;
    Answer: string;
  end;
  
  TAppClearEvent = record
    Method: TThreadMethod;
    Data: TAppClearData;
    Answer: string;
  end;
  
  TAppKeyEvent = record
    Method: TThreadMethod;
    Data: TAppKeyData;
    Answer: string;
  end;
  
// TAppInterface record
  
type
  TAppInterface = class
  public
    constructor Create;
    destructor Destroy; override;
  public
    OnFatalError: TAppFatalErrorEvent;
    OnOverWrite: TAppOverWriteEvent;
    OnRename: TAppRenameEvent;
    OnWarning: TAppWarningEvent;
    OnDisplay: TAppDisplayEvent;
    OnRequest: TAppRequestEvent;
    OnError: TAppErrorEvent;
    OnList: TAppListEvent;
    OnTick: TAppTickEvent;
    OnClear: TAppClearEvent;
    OnKey: TAppKeyEvent;
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
    constructor Create(aAppInterface: TAppInterface; aAppParams: TStringList);
    procedure Sync(aMethod: TThreadMethod);
    destructor Destroy; override;
  end;

implementation

// TAppInterface class

constructor TAppInterface.Create;
begin
  inherited Create;
end;

destructor TAppInterface.Destroy;
begin
  inherited Destroy;
end;

// TApp class

constructor TApp.Create(aAppInterface: TAppInterface; aAppParams: TStringList);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  // ---
  AppInterface := aAppInterface;
  AppParams := aAppParams;
end;

destructor TApp.Destroy;
begin
  AppInterface := nil;
  AppParams := nil;
  inherited Destroy;
end;

procedure TApp.Sync(aMethod: TThreadMethod);
begin
  {$IFDEF THREADSYNCHRONIZER}
  Synchronizer.Synchronize(aMethod);
  {$ELSE}
  Synchronize(aMethod);
  {$ENDIF}
end;

end.
