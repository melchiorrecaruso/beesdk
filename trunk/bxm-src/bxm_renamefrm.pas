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

    BeeGui Rename form.

  Modifyed:

}

unit bxm_RenameFrm;

{$I bxm_compiler.inc}

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
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    FromFileNameLabel: TLabel;
    FromFileName: TLabel;
    ToFileNameLabel: TLabel;
    ToFileName:  TEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { public declarations }
  public
    { public declarations }
  end;

var
  RenameFrm: TRenameFrm;

implementation

uses
  bx_Common,
  bxm_Consts,
  bxm_Messages,
  bxm_SysUtils;

{ TRenameFrm class }

procedure TRenameFrm.FormCreate(Sender: TObject);
begin

end;

procedure TRenameFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TRenameFrm.FormShow(Sender: TObject);
begin
  if ToFileName.CanFocus then
  begin
    ToFileName.SetFocus;
  end;
end;

initialization

  {$I bxm_renamefrm.lrs}

end.