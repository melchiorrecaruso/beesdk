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
  along with this program; if not, write To To the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

    BeeCore Rename form.

  Modifyed:

}

unit BeeGui_RenameFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Classes,
  Dialogs,
  Buttons,
  IniFiles,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  LResources;

type

  { TRenameFrm }

  TRenameFrm = class(TForm)
    FromFNLabel: TLabel;
    FromFN: TLabel;
    ToFNLabel: TLabel;
    ToFN: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { public declarations }
  public
    { public declarations }
  end;

implementation

uses
  Bee_Common,
  BeeGui_SysUtils,
  BeeGui_Messages;

  { TRenameFrm class }

  procedure TRenameFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_renamefrm_loadlanguage.inc}
    {$I beegui_renamefrm_loadproperty.inc}
  end;

  procedure TRenameFrm.FormShow(Sender: TObject);
  begin
    // Fix Laz bug
    if Constraints.MaxWidth = 0 then
    begin
      Constraints.MaxWidth  := Width;
      Constraints.MaxHeight := Height;
      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;
    end;
  end;

  procedure TRenameFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$IFDEF DEBUG}
      {$I beegui_renamefrm_savelanguage.inc}
    {$ENDIF}
    {$I beegui_renamefrm_saveproperty.inc}
  end;
  
initialization

  {$I beegui_renamefrm.lrs}

end.
