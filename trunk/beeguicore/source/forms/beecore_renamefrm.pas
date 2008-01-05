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

    BeeGui Rename form.

  Modifyed:

}

unit BeeCore_RenameFrm;

interface

uses
  Forms,
  Classes,
  Dialogs,
  Buttons,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  LResources,
  XMLPropStorage;

type

  { TRenameFrm }

  TRenameFrm = class(TForm)
    Storage: TXMLPropStorage;
    RenameFromLabel: TLabel;
    RenameFrom: TLabel;
    RenameToLabel: TLabel;
    RenameTo: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
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
  BeeCore_SysUtils;

  { TRenameFrm class }

  procedure TRenameFrm.FormShow(Sender: TObject);
  begin
    RenameFrom.Caption := RenameTo.Caption;
    RenameTo.SetFocus;
  end;

  procedure TRenameFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := IncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder + ('renamefrm.xml');
    end;
    {$I beecore_renamefrm.inc}
    Storage.Restore;
  end;
  
initialization

  {$I beecore_renamefrm.lrs}

end.
