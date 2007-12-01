{
    Copyright (c) 2005-2007 Andrew Filinsky and Melchiorre Caruso

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

unit BeeGui_IntViewerFrm;

{$I Compiler.inc}

interface

uses
  Forms,
  Dialogs,
  Classes,
  Buttons,
  Windows,
  StdCtrls,
  Messages,
  SysUtils,
  Graphics,
  Controls,
  // ---
  BeeGui_FormStorage;

type
  TIntViewerFrm = class(TForm)
    IntViewerFrm_FontDialog: TFontDialog;
    IntViewerFrm_SaveDialog: TSaveDialog;
    IntViewerFrm_Viewer: TMemo;
    BtnFont: TBitBtn;
    BtnSave: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FFormStorage: TFormStorage;
  public
    { Public declarations }
  end;

var
  IntViewerFrm: TIntViewerFrm;

implementation

{$R *.dfm}

uses
  BeeGui_IntViewerFrmRes,
  BeeGui_SysUtils;

// TIntViewerFrm class

procedure TIntViewerFrm.FormCreate(Sender: TObject);
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SIntViewerFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SIntViewerFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SIntViewerFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
end;

procedure TIntViewerFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SIntViewerFrm_PropertyFull
  else
    FFormStorage.Properties := _SIntViewerFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
end;

procedure TIntViewerFrm.BtnFontClick(Sender: TObject);
begin
  IntViewerFrm_FontDialog.Font := IntViewerFrm_Viewer.Font;
  if IntViewerFrm_FontDialog.Execute then
  begin
    IntViewerFrm_Viewer.Font := IntViewerFrm_FontDialog.Font;
  end;
end;

procedure TIntViewerFrm.BtnSaveClick(Sender: TObject);
var
  FileName: string;
begin
  IntViewerFrm_SaveDialog.FileName := '';
  if IntViewerFrm_SaveDialog.Execute then
  begin
    FileName := IntViewerFrm_SaveDialog.FileName;
    case IntViewerFrm_SaveDialog.FilterIndex of
      1: FileName := ChangeFileExt(FileName, '.txt');
    end;
    IntViewerFrm_Viewer.Lines.SaveToFile(FileName);
  end;
end;

end.
