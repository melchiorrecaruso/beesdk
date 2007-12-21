{
  Copyright (c) 2003-2007 Andrew Filinsky and Melchiorre Caruso

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

  BeeCore archiver library.

  Modifyed:

  v0.1.0 build 0050 - 2007.12.18 by Melchiorre Caruso.
}

library BeeCore;

{$I compiler.inc}

uses
  Classes,
  Bee_App,
  Bee_Interface;


function GetVersion: integer;
begin
  Result := 10;
end;

function CreateObject(aAppInterface: TAppInterfacePtr; const aAppParams: string): TApp;
begin
  Result := TBeeApp.Create(aAppInterface, aAppParams);
end;

{$I LibSyncMgr.inc}

exports CreateObject;

begin
end.

