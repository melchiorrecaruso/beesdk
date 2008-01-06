{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

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

    BeeCore application.

  Modifyed:

}

program BeeCore;

uses
  {$IFDEF UNIX}
  cThreads,
  {$ENDIF}
  Interfaces,
  Classes,
  Forms,
  // ---
  Bee_App,
  Bee_Interface,
  // ---
  BeeCore_AddFrm,
  BeeCore_ViewFrm,
  BeeCore_AboutFrm,
  BeeCore_RenameFrm,
  BeeCore_ExtractFrm,
  BeeCore_PasswordFrm,
  BeeCore_OverwriteFrm;

type

  TCore = class
  private
    FInterface: TAppInterface;

    App: TBeeApp;
    AppKey: string;
    AppInterface: TAppInterface;
    AppParams: TStringList;

    procedure OnFatalError;
    procedure OnOverWrite;
    procedure OnWarning;
    procedure OnDisplay;
    procedure OnRequest;
    procedure OnRename;
    procedure OnError;
    procedure OnClear;
    procedure OnList;
    procedure OnTick;
    procedure OnKey;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;
  
  // implementation
  
  constructor TCore.Create;
  begin
    inherited Create;
  end;
  
  destructor TCore.Destroy;
  begin
    inherited Destroy;
  end;
  
  procedure TCore.Execute;
  begin

  end;
  
var
  Core: TCore;

begin
  Core := TCore.Create;
  Core.Execute;
  Core.Free;
end.

