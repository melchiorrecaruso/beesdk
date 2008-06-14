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

    v0.7.9 build 0755 - 2008.05.19 by Melchiorre Caruso.
}

unit Bee_Interface;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils;
  
type

  // TInterfaces class

  TInterfaces = class
  public
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
        TotalSize: integer;
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
    Terminated: boolean;
    Suspended: boolean;
    ExitCode: integer;
    // Exit code value
    //  0   = No Error
    //  1   = Warning
    //  2   = Fatal Error
    //  7   = Command line error
    //  8   = No Memory
    //  255 = User stopped process
  end;
  
type

  // TParams ...
  
  TParams = TStringList;
  
type

  // TSynchronizer ...

  TSynchronizer =  procedure (aMethod: TThreadMethod) of object;

type

  // TApp class ...

  TApp = class (TThread)
  protected
    Interfaces: TInterfaces;
    Params: TStringList;
  protected
    procedure DoTerminate; override;
    procedure SetExitCode(Code: integer);
  public
    constructor Create(aInterfaces: TInterfaces; aParams: TParams);
    procedure Synchronize(aMethod: TThreadMethod); overload;
    destructor Destroy; override;
  end;

implementation

// TApp class ...

constructor TApp.Create(aInterfaces: TInterfaces; aParams: TParams);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpNormal;
  // ---
  Params := aParams;
  Interfaces := aInterfaces;
  Interfaces.ExitCode := 0;
  Interfaces.Terminated := False;
  Interfaces.Suspended := False;
  Interfaces.OnTick.Data.TotalSize := 0;
  Interfaces.OnTick.Data.ProcessedSize := 0;
end;

destructor TApp.Destroy;
begin
  Params := nil;
  Interfaces := nil;
  inherited Destroy;
end;

procedure TApp.DoTerminate;
begin
  ExitCode := Interfaces.ExitCode;
  Interfaces.Terminated := True;
  Interfaces.Suspended := False;
  inherited DoTerminate;
end;

procedure TApp.Synchronize(aMethod: TThreadMethod);
begin
  inherited Synchronize(aMethod);
end;

procedure TApp.SetExitCode(Code: integer);
begin
  with Interfaces do
    if ExitCode < Code then
      ExitCode := Code;
end;

end.
