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

{   Contains:

    BeeGui Internal Viewer form.

    Modifyed:

    v1.0.1 build 9155 - 2005.07.15 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_IntViewerFrm;

{$R-,Q-,S-}

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
  Controls;

type
  TIntViewerCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type
  TIntViewerFrm = class (TIntViewerCustomFrm)
    IntViewerFrm_FontDialog: TFontDialog;
    IntViewerFrm_SaveDialog: TSaveDialog;
    IntViewerFrm_Viewer: TMemo;
    BtnFont: TBitBtn;
    BtnSave: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormShow (Sender: TObject);
    procedure BtnFontClick (Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IntViewerFrm: TIntViewerFrm;

implementation

{$R *.DFM}

uses
  BeeGui_ConfigFrm;

/// TIntViewerCustomFrm class
  
  constructor TIntViewerCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TIntViewerCustomFrm.Destroy;
  var
    I: integer;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      if TControl (Self.Components [I]).ClassType = TFontDialog then
        BeeGui_Configuration.Save_TControl (TControl (Self.Components [I]));
    inherited Destroy;
  end;

  procedure TIntViewerCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TIntViewerFrm class

  procedure TIntViewerFrm.BtnFontClick (Sender: TObject);
  begin
    if IntViewerFrm_FontDialog.Execute then
      IntViewerFrm_Viewer.Font := IntViewerFrm_FontDialog.Font;
  end;

  procedure TIntViewerFrm.FormShow (Sender: TObject);
  begin
    IntViewerFrm_Viewer.Font := IntViewerFrm_FontDialog.Font;
  end;

  procedure TIntViewerFrm.BtnSaveClick(Sender: TObject);
  var
    FileName: ansistring;
  begin
    IntViewerFrm_SaveDialog.FileName := '';
    if IntViewerFrm_SaveDialog.Execute then
    begin
      FileName := IntViewerFrm_SaveDialog.FileName;
      case IntViewerFrm_SaveDialog.FilterIndex of
        1: FileName := ChangeFileExt (FileName, '.txt');
      end;
      IntViewerFrm_Viewer.Lines.SaveToFile (FileName);
    end;
  end;

  
end.
