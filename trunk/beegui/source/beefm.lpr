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

{   Contains:

    Bee File Manager

    Modifyed:
}

program BeeFm;

{$I compiler.inc}

{$IFDEF MSWINDOWS}
  {$R beegui.res}
{$ENDIF}

uses
  Interfaces,
  Forms,
  // ---
  BeeGui_Package,
  // ---
  BeeFM_MainFrm,
  BeeFM_ViewFrm,
  BeeFM_ConfigFrm,
  BeeGui_AboutFrm, BeeFM_PropertyFrm, BeeFm_SelectFrm;

begin
  Application.Initialize;
  Application.Name := 'BeeFM';
  Application.Title := 'BeeFM';
  Application.HelpFile := '';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TConfigFrm, ConfigFrm);
  Application.Run;
end.

