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

    BeeGui View form.

  Modifyed:

}

unit BeeGui_ViewFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Buttons,
  Classes,
  Dialogs,
  Graphics,
  IniFiles,
  ComCtrls,
  StdCtrls,
  Controls,
  SysUtils,
  LResources;

type

  { TViewFrm }

  TViewFrm = class(TForm)
    FontDialog: TFontDialog;
    SaveDialog: TSaveDialog;
    Memo: TMemo;
    BtnFont: TBitBtn;
    BtnSave: TBitBtn;
    BtnOk: TBitBtn;
    procedure BtnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  public
    { public declarations }
  private
    { private declarations }
  end;

implementation

uses
  Bee_Common,
  BeeGui_SysUtils;

  { TViewFrm class }

  procedure TViewFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_viewfrm_loadlanguage.inc}
    {$I beegui_viewfrm_loadproperty.inc}
  end;
  
  procedure TViewFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_viewfrm_savelanguage.inc}
    {$I beegui_viewfrm_saveproperty.inc}
  end;

  procedure TViewFrm.BtnOkClick(Sender: TObject);
  begin
    Close;
  end;

  procedure TViewFrm.BtnFontClick(Sender: TObject);
  begin
    FontDialog.Font := Memo.Font;
    if FontDialog.Execute then
    begin
      Memo.Font := FontDialog.Font;
    end;
  end;
  
  procedure TViewFrm.BtnSaveClick(Sender: TObject);
  begin
    SaveDialog.FileName := '';
    SaveDialog.Filter :=
      'Log files (*.log)|*.log|' +
      'Txt files (*.txt)|*.txt|' +
      'All files (*.*) |*.*|';

    if SaveDialog.Execute then
    begin


    end;
  end;
  
initialization

  {$I beegui_viewfrm.lrs}

end.
