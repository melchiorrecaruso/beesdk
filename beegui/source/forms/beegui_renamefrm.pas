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
  private
    { public declarations }
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  end;

implementation

uses
  Bee_Common,
  BeeGui_Consts,
  BeeGui_Messages,  
  BeeGui_SysUtils;

  { TRenameFrm class }
  
  {$I beegui_renamefrm_saveproperty.inc}
  {$I beegui_renamefrm_loadproperty.inc}
  {$I beegui_renamefrm_savelanguage.inc}
  {$I beegui_renamefrm_loadlanguage.inc}

  procedure TRenameFrm.FormCreate(Sender: TObject);
  begin
    LoadLanguage;
    LoadProperty;
  end;

  procedure TRenameFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    {$IFDEF DEBUG}
      SaveLanguage;
    {$ENDIF}
    SaveProperty;
  end;
  
initialization

  {$I beegui_renamefrm.lrs}

end.
