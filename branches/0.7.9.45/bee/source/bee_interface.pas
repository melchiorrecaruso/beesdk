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
  TAppFatalErrorData = packed record
    Msg: string;
  end;

  TAppOverWriteData = packed record
    FileName: string;
    FilePath: string;
    FileTime: integer;
    FileSize: integer;
  end;
  
  TAppRenameData = packed record
    FileName: string;
    FilePath: string;
    FileTime: integer;
    FileSize: integer;
  end;
  
  TAppWarningData = packed record
    Msg: string;
  end;
  
  TAppDisplayData = packed record
    Msg: string;
  end;
  
  TAppRequestData = packed record
    Msg: string;
  end;
  
  TAppErrorData = packed record
    Msg: string;
  end;
  
  TAppListData = packed record
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
  
  TAppTickData = packed record
    Percentage: integer;
  end;
  
  TAppClearData = packed record
    Msg: string;
  end;
  
  TAppKeyData = packed record
    FileName: string;
    FilePath: string;
    FileTime: integer;
    FileSize: integer;
  end;
  
  
// TAppEvent records
  
type
  TAppFatalErrorEvent = packed record
    Method: TThreadMethod;
    Data: TAppFatalErrorData;
    Answer: string;
  end;
  
  TAppOverWriteEvent = packed record
    Method: TThreadMethod;
    Data: TAppOverWriteData;
    Answer: char;
  end;
  
  TAppRenameEvent = packed record
    Method: TThreadMethod;
    Data: TAppRenameData;
    Answer: string;
  end;

  TAppWarningEvent = packed record
    Method: TThreadMethod;
    Data: TAppWarningData;
    Answer: string;
  end;

  TAppDisplayEvent = packed record
    Method: TThreadMethod;
    Data: TAppDisplayData;
    Answer: string;
  end;
  
  TAppRequestEvent = packed record
    Method: TThreadMethod;
    Data: TAppRequestData;
    Answer: string;
  end;
  
  TAppErrorEvent = packed record
    Method: TThreadMethod;
    Data: TAppErrorData;
    Answer: string;
  end;
  
  TAppListEvent = packed record
    Method: TThreadMethod;
    Data: TAppListData;
    Answer: string;
  end;
  
  TAppTickEvent = packed record
    Method: TThreadMethod;
    Data: TAppTickData;
    Answer: string;
  end;
  
  TAppClearEvent = packed record
    Method: TThreadMethod;
    Data: TAppClearData;
    Answer: string;
  end;
  
  TAppKeyEvent = packed record
    Method: TThreadMethod;
    Data: TAppKeyData;
    Answer: string;
  end;
  
// TAppInterface record
  
type
  TAppInterface = packed record
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
  
type
  TAppInterfacePtr = ^TAppInterface;

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
    AppInterface: TAppInterfacePtr;
  public
    constructor Create(aAppInterface: TAppInterfacePtr; const aAppParams: string);
    procedure Sync(aMethod: TThreadMethod);
    destructor Destroy; override;
  end;

implementation

// TApp class

constructor TApp.Create(aAppInterface: TAppInterfacePtr; const aAppParams: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  // ---
  AppInterface := aAppInterface;
  // ---
  AppParams := TStringList.Create;
  AppParams.Text := aAppParams;
end;

destructor TApp.Destroy;
begin
  AppParams.Destroy;
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
