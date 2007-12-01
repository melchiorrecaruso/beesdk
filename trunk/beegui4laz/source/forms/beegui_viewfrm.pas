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

    BeeGui View form.

    Modifyed:

    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;
  
    v1.0.3 build 0360 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_ViewFrm;

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
    ViewFrm_ViewMemo: TMemo;
    ViewFrm_FontDialog: TFontDialog;
    ViewFrm_Storage: TXMLPropStorage;
    
    BtnFont: TBitBtn;
    BtnOk: TBitBtn;

    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnFontClick(Sender: TObject);
  private
  public
  end;

implementation

uses
  BeeGui_SysUtils;

  { TViewFrm class }

  procedure TViewFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := ExtractFilePath(ParamStr(0)) + GetConfigDir;
    if ForceDirectories(CfgFolder) then
    begin
      ViewFrm_Storage.FileName := CfgFolder + ('viewfrm.xml');
    end;
    {$I beegui_viewfrm.inc}
    ViewFrm_Storage.Restore;
  end;

  procedure TViewFrm.FormShow(Sender: TObject);
  begin
    with ViewFrm_ViewMemo do
    begin
      Height := ViewFrm.Height - 70;
      ViewFrm.BtnOk.Top := Height + 10;
      ViewFrm.BtnFont.Top := Height + 10;

      ViewFrm_StatusBar.Left := ViewFrm.Width - 43;
      ViewFrm_StatusBar.Top := ViewFrm.Height - 51;
    end;
  end;

  procedure TViewFrm.BtnFontClick(Sender: TObject);
  begin
    ViewFrm_FontDialog.Font := ViewFrm_ViewMemo.Font;
    if ViewFrm_FontDialog.Execute then
    begin
      ViewFrm_ViewMemo.Font := ViewFrm_FontDialog.Font;
    end;
  end;

initialization

  {$I beegui_viewfrm.lrs}

end.
