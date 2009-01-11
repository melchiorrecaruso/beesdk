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

      BeeFm Viewer form.

    Modifyed:
    
      v1.0.5 build 0525 - 2009.01.11 by Melchiorre Caruso.
}

unit BeeFm_ViewFrm;

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
    BtnClose: TBitBtn;
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  public
    { private declarations }
    procedure LoadFile(const aFileName: string);
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

    Memo.Font := FontDialog.Font;
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
        1: FileName := ChangeFileExt(FileName, '.txt');
        2: FileName := ChangeFileExt(FileName, '.log');
      end;
      LoadFile(FileName);
    end;
  end;

  procedure TViewFrm.LoadFile(const aFileName: string);
  begin
    Memo.Clear;
    Memo.Enabled := True;
    Memo.Color := clWindow;
    try
      Memo.Lines.LoadFromFile(aFileName);
    except
      Memo.Clear;
      Memo.Enabled := False;
      Memo.Color := clInactiveBorder;
    end;
  end;
  
initialization

  {$I beefm_viewfrm.lrs}

end.



