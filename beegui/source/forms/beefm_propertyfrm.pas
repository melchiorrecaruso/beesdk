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
  LResources,
  // ---
  BeeGui_SysUtils,
  BeeGui_ArchiveListViewMgr;

type
  { TInfoFrm }

  TInfoFrm = class(TForm)
    Pages:   TPageControl;
    APanel:  TPanel;
    FPanel:  TPanel;
    APage:   TTabSheet;
    FPage:   TTabSheet;
    AArcSizeValue: TLabel;
    AArcSize: TLabel;
    AED:     TImage;
    AFD:     TImage;
    AE:      TImage;
    AFilesValue: TLabel;
    AFilesCryptedValue: TLabel;
    ACrypted: TLabel;
    AFiles:  TLabel;
    AF:      TImage;
    AL:      TImage;
    AModifiedValue: TLabel;
    AModified: TLabel;
    ANameValue: TLabel;
    AName:   TLabel;
    APackedValue: TLabel;
    APacked: TLabel;
    AR:      TLabel;
    ARatioValue: TLabel;
    ARatio:  TLabel;
    ASizeValue: TLabel;
    ASize:   TLabel;
    AEU:     TImage;
    AFU:     TImage;
    AVersionValue: TLabel;
    AVersion: TLabel;
    Bevel01: TBevel;
    Bevel02: TBevel;
    Bevel04: TBevel;
    Bevel03: TBevel;
    FAttributeValue: TLabel;
    FAttribute: TLabel;
    FModifiedValue: TLabel;
    FModified: TLabel;
    FED:     TImage;
    FFD:     TImage;
    FE:      TImage;
    FF:      TImage;
    FL:      TImage;
    FMethodValue: TLabel;
    FMethod: TLabel;
    FNameValue: TLabel;
    FName:   TLabel;
    FPackedValue: TLabel;
    FPacked: TLabel;
    FPasswordValue: TLabel;
    FPassword: TLabel;
    FR:      TLabel;
    FRatioValue: TLabel;
    FRatio:  TLabel;
    FSizeValue: TLabel;
    FSize:   TLabel;
    FEU:     TImage;
    FFU:     TImage;
    FVersionValue: TLabel;
    FVersion: TLabel;
    BtnOk:   TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
    procedure PaintProgressBar(EU, FU, E, L, F, FD, ED: TImage; R: TLabel);
  public
    { public declarations }
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  end;

var
  PropertyFrm: TInfoFrm;

implementation

uses
  BeeGui_Consts,
  BeeGui_Messages,
  BeeFM_ConfigFrm;

procedure TInfoFrm.PaintProgressBar(EU, FU, E, L, F, FD, ED: TImage; R: TLabel);
var
  S: string;
  Percentage: integer;
begin
  S := R.Caption;
  while Pos('%', S) > 0 do
  begin
    Delete(S, Pos('%', S), 1);
  end;
  TryStrToInt(S, PErcentage);

  EU.Transparent := True;
  EU.Visible := False;
  FU.Transparent := True;
  FU.Visible := False;
  FU.Left    := EU.Left;
  E.Transparent := True;
  E.Visible  := False;
  E.Left     := EU.Left;
  L.Transparent := True;
  L.Visible  := False;
  L.Left     := EU.Left;
  F.Transparent := True;
  F.Visible  := False;
  F.Left     := EU.Left;
  FD.Transparent := True;
  FD.Visible := False;
  FD.Left    := EU.Left;
  ED.Transparent := True;
  ED.Visible := False;
  ED.Left    := EU.Left;

  FU.Top := EU.Top;
  FD.Top := ED.Top + ED.Height - FD.Height;

  R.Caption := IntToStr(Percentage) + '%';
  if Percentage > 100 then
    Percentage := 100;
  case Percentage of
    0..6:
    begin
      E.Top    := EU.Top + EU.Height;
      E.Height := ED.Top - E.Top;

      EU.Visible := True;
      E.Visible  := True;
      ED.Visible := True;

      R.Top := ED.Top - 3;
    end;
    7..99:
    begin
      case Percentage of
        97..99: Percentage := 96;
      end;
      F.Height := 1 + (FD.Top - EU.Top - EU.Height - L.Height - 1) *
        (Percentage - 7) div (96 - 7);

      F.Top    := FD.Top - F.Height;
      L.Top    := F.Top - L.Height;
      E.Top    := EU.Top + EU.Height;
      E.Height := L.Top - E.Top;

      EU.Visible := True;
      E.Visible  := True;
      L.Visible  := True;
      F.Visible  := True;
      FD.Visible := True;

      R.Top := L.Top;
    end;
    100:
    begin
      F.Top    := FU.Top + FU.Height;
      F.Height := FD.Top - F.Top;

      FU.Visible := True;
      F.Visible  := True;
      FD.Visible := True;

      R.Top := FU.Top;
    end;
  end;
end;

{ TInfoFrm class }

  {$I beefm_propertyfrm_saveproperty.inc}
  {$I beefm_propertyfrm_loadproperty.inc}
  {$I beefm_propertyfrm_savelanguage.inc}
  {$I beefm_propertyfrm_loadlanguage.inc}

procedure TInfoFrm.FormPaint(Sender: TObject);
begin
  if APage.TabVisible then
    PaintProgressBar(AEU, AFU, AE, AL, AF, AFD, AED, AR);

  if FPage.TabVisible then
    PaintProgressBar(FEU, FFU, FE, FL, FF, FFD, FED, FR);
end;

procedure TInfoFrm.FormCreate(Sender: TObject);
begin
  LoadLanguage;
  LoadProperty;
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