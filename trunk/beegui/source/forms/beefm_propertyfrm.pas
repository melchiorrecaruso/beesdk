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

{ Contains:

    BeeFM Information form.

  Modifyed:
}

unit BeeFM_PropertyFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Dialogs,
  Buttons,
  Classes,
  StdCtrls,
  ExtCtrls,
  Graphics,
  Controls,
  ComCtrls,
  SysUtils,
  IniFiles,
  LResources, Spin,
  // ---
  BeeGui_SysUtils,
  BeeGui_ArchiveListViewMgr;

type
  { TInfoFrm }

  TInfoFrm = class(TForm)
    AArcSize: TLabel;
    AArcSizeValue: TLabel;
    ACrypted: TLabel;
    F: TImage;
    AFiles: TLabel;
    AFilesCryptedValue: TLabel;
    AFilesValue: TLabel;
    AModified: TLabel;
    AModifiedValue: TLabel;
    AName: TLabel;
    ANameValue: TLabel;
    APacked: TLabel;
    APackedValue: TLabel;
    R: TLabel;
    AR1: TLabel;
    AR2: TLabel;
    ARatio: TLabel;
    ARatioValue: TLabel;
    ASize: TLabel;
    ASizeValue: TLabel;
    AVersion: TLabel;
    AVersionValue: TLabel;
    Bevel01: TBevel;
    Bevel02: TBevel;
    Bevel03: TBevel;
    Bevel04: TBevel;
    E: TImage;
    E1: TImage;
    ED: TImage;
    EMPTYPanel: TPanel;
    EU: TImage;
    EU1: TImage;
    F1: TImage;
    FAttribute: TLabel;
    FAttributeValue: TLabel;
    FD: TImage;
    FD1: TImage;
    FMethod: TLabel;
    FMethodValue: TLabel;
    FModified: TLabel;
    FModifiedValue: TLabel;
    FName: TLabel;
    FNameValue: TLabel;
    FPacked: TLabel;
    FPackedValue: TLabel;
    FPassword: TLabel;
    FPasswordValue: TLabel;
    FRatio: TLabel;
    FRatioValue: TLabel;
    FSize: TLabel;
    FSizeValue: TLabel;
    FU: TImage;
    FULLPanel: TPanel;
    FVersion: TLabel;
    FVersionValue: TLabel;
    L: TImage;
    MIDDLEPanel: TPanel;
    BtnOk:   TBitBtn;
    ArchivePanel: TPanel;
    FilePanel: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    FPercentage: longint;
    procedure SetPercentage(Value: longint);
  public
    { public declarations }
    procedure UpdateProgressBar;
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  public
    property Percentage: longint read FPercentage write SetPercentage;
  end;

var
  PropertyFrm: TInfoFrm;

implementation

uses
  BeeGui_Consts,
  BeeGui_Messages,
  BeeFM_ConfigFrm;


procedure TInfoFrm.SetPercentage(Value: longint);
begin
  if Value in [0..100] then
  begin
    FPercentage := Value;
  end;
end;

procedure TInfoFrm.UpdateProgressBar;
var
  Min, Max: longint;
begin
  EMPTYPanel .Visible := False;
  MIDDLEPanel.Visible := False;
  FULLPanel  .Visible := False;

  R .Caption  := IntToStr(FPercentage) + '%';
  AR1.Caption := IntToStr(FPercentage) + '%';
  AR2.Caption := IntToStr(FPercentage) + '%';

  case FPercentage of
    0: EMPTYPanel.Visible := True;
    1..99:
    begin
      Min := MIDDLEPanel.Height - L.Height - FD.Height;
      Max := EU.Height;

      L.Top := Min + Trunc((Max - Min) * FPercentage / 100);

      MIDDLEPanel.Visible := True;
    end;
    100: FULLPanel  .Visible := True;
  end;
end;

{ TInfoFrm class }

  {$I beefm_propertyfrm_saveproperty.inc}
  {$I beefm_propertyfrm_loadproperty.inc}
  {$I beefm_propertyfrm_savelanguage.inc}
  {$I beefm_propertyfrm_loadlanguage.inc}

procedure TInfoFrm.FormCreate(Sender: TObject);
begin
  LoadLanguage;
  LoadProperty;
end;

procedure TInfoFrm.FormResize(Sender: TObject);
begin
  UpdateProgressBar;
end;

procedure TInfoFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFDEF SAVELANGUAGE}
  SaveLanguage;
  {$ENDIF}
  SaveProperty;
end;

initialization

  {$I beefm_propertyfrm.lrs}

end.
