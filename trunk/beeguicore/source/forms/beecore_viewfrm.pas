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

unit BeeCore_ViewFrm;

interface

uses
  Forms,
  Buttons,
  Classes,
  Dialogs,
  ComCtrls,
  StdCtrls,
  Controls,
  LResources,
  XMLPropStorage;

type

  { TViewFrm }

  TViewFrm = class(TForm)
    Storage: TXMLPropStorage;
    FontDialog: TFontDialog;
    SaveDialog: TSaveDialog;
    Memo: TMemo;
    BtnFont: TBitBtn;
    BtnSave: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
  public
    { public declarations }
  private
    { private declarations }
  end;
  
var
  ViewFrm: TViewFrm;

implementation

uses
  BeeCore_SysUtils,
  Bee_Common;

  { TViewFrm class }

  procedure TViewFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder + ('viewfrm.xml');
    end;
    {$I beecore_viewfrm.inc}
    Storage.Restore;
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

  end;

    procedure TViewFrm.BtnOkClick(Sender: TObject);
  begin

  end;

initialization

  {$I beecore_viewfrm.lrs}

end.
