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

      BeeFm View form.

    Modifyed:
    
      v1.0.5 build 0450 - 2008.07.10 by Melchiorre Caruso.
}

unit BeeFM_ViewFrm;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  private
    { private declarations }
  end;

var
  ViewFrm: TViewFrm;

implementation

uses
  BeeGui_Consts,
  BeeGui_Messages,
  BeeGui_SysUtils;

  { TViewFrm class }
  
  {$I beefm_viewfrm_saveproperty.inc}
  {$I beefm_viewfrm_loadproperty.inc}
  {$I beefm_viewfrm_savelanguage.inc}
  {$I beefm_viewfrm_loadlanguage.inc}

  procedure TViewFrm.FormCreate(Sender: TObject);
  begin
    LoadLanguage;
    LoadProperty;
  end;
  
  procedure TViewFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    {$IFDEF DEBUG}
      SaveLanguage;
    {$ENDIF}
    SaveProperty;
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
  var
    FileName: string;
  begin
    SaveDialog.FileName := '';
    if SaveDialog.Execute then
    begin
      FileName := SaveDialog.FileName;
      case SaveDialog.FilterIndex of
        1: FileName := ChangeFileExt(FileName, '.log');
        2: FileName := ChangeFileExt(FileName, '.txt');
      end;
      Memo.Lines.SaveToFile(FileName);
    end;
  end;
  
initialization

  {$I beefm_viewfrm.lrs}

end.
