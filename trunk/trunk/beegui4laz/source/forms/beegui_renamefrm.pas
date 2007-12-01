{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

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

    BeeGui Rename form.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;
  
    v1.0.3 build 0360 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_RenameFrm;

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
    RenameFrm_Storage: TXMLPropStorage;
    RenameFrm_From_: TLabel;
    RenameFrm_From: TLabel;
    RenameFrm_To_: TLabel;
    RenameFrm_To: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

implementation

uses
  BeeGui_SysUtils;

  { TRenameFrm class }

  procedure TRenameFrm.FormShow(Sender: TObject);
  begin
    RenameFrm_From.Caption := RenameFrm_To.Text;
    RenameFrm_To.SetFocus;
  end;

  procedure TRenameFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      RenameFrm_Storage.FileName := CfgFolder+ ('renamefrm.xml');
    end;
    {$I beegui_renamefrm.inc}
    RenameFrm_Storage.Restore;
  end;
  
initialization

  {$I beegui_renamefrm.lrs}

end.
