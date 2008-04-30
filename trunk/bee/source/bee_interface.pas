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

    TApp, abstract class of Thread.

  Modifyed:

    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

    v0.7.9 build 0627 - 2008.02.11 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes;
  
type

  // TAppInterface class

  TAppInterface = class
    OnFatalError: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        Msg: string;
      end;
    end;
    OnOverWrite: record
      Method: TThreadMethod;
      Answer: char;
      Data: record
        FileName: string;
        FilePath: string;
        FileTime: integer;
        FileSize: integer;
      end;
    end;
    OnRename: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        FileName: string;
        FilePath: string;
        FileTime: integer;
        FileSize: integer;
        FileAttr: integer;
      end;
    end;
    OnWarning: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        Msg: string;
      end;
    end;
    OnDisplay: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        Msg: string;
      end;
    end;
    OnRequest: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        Msg: string;
      end;
    end;
    OnError: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        Msg: string;
      end;
    end;
    OnList: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        FileName: string;
        FilePath: string;
        FileSize: integer;
        FilePacked: integer;
        FileRatio: integer;
        FileAttr: integer;
        FileTime: integer;
        FileComm: string;
        FileCrc: cardinal;
        FileMethod: string;
        FileVersion: string;
        FilePassword: string;
        FilePosition: integer;
      end;
    end;
    OnTick: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        GeneralSize: integer;
        ProcessedSize: integer;
        Percentage: integer;
      end;
    end;
    OnClear: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        Msg: string;
      end;
    end;
    OnKey: record
      Method: TThreadMethod;
      Answer: string;
      Data: record
        FileName: string;
        FilePath: string;
        FileTime: integer;
        FileSize: integer;
      end;
    end;
  end;



type

  // TApp class

  TApp = class(TThread) 
  protected
    AppParams: TStringList;
  public
    AppInterface: TAppInterface;
    AppPause: boolean;
  public
    constructor Create(aAppInterface: TAppInterface; aAppParams: TStringList);
    procedure Sync(aMethod: TThreadMethod);
    destructor Destroy; override;
  end;

implementation

// TApp class

constructor TApp.Create(aAppInterface: TAppInterface; aAppParams: TStringList);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpNormal;
  // ---
  AppInterface := aAppInterface;
  AppParams := aAppParams;
  AppPause := False;
end;

destructor TApp.Destroy;
begin
  AppInterface := nil;
  AppParams := nil;
  inherited Destroy;
end;

procedure TApp.Sync(aMethod: TThreadMethod);
begin
  Synchronize(aMethod);
end;

end.
