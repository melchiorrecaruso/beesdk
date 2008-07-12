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
    AArcSizeValue: TLabel;
    AArcSize: TLabel;
    ADownEmpty: TImage;
    ADownFull: TImage;
    AEmpty: TImage;
    AFilesValue: TLabel;
    AFilesCryptedValue: TLabel;
    ACrypted: TLabel;
    AFiles: TLabel;
    AFull: TImage;
    ALevel: TImage;
    AModifiedValue: TLabel;
    AModified: TLabel;
    ANameValue: TLabel;
    AName: TLabel;
    APackedValue: TLabel;
    APacked: TLabel;
    AR: TLabel;
    ARatioValue: TLabel;
    ARatio: TLabel;
    ASizeValue: TLabel;
    ASize: TLabel;
    AUpEmpty: TImage;
    AUpFull: TImage;
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
    FDownEmpty: TImage;
    FDownFull: TImage;
    FEmpty: TImage;
    FFull: TImage;
    FLevel: TImage;
    FMethodValue: TLabel;
    FMethod: TLabel;
    FNameValue: TLabel;
    FName: TLabel;
    FPackedValue: TLabel;
    FPacked: TLabel;
    FPasswordValue: TLabel;
    FPassword: TLabel;
    FR: TLabel;
    FRatioValue: TLabel;
    FRatio: TLabel;
    FSizeValue: TLabel;
    FSize: TLabel;
    FUpEmpty: TImage;
    FUpFull: TImage;
    FVersionValue: TLabel;
    FVersion: TLabel;
    FPage: TTabSheet;

    Pages: TPageControl;
    APage: TTabSheet;




    BtnOk: TBitBtn;
    APanel: TPanel;
    FPanel: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    Ratio : integer;
  public
    function UpdateAInfo(const ArcName: string; AInfo: TArcDetails): boolean;
    function UpdateFInfo(FInfo: TArcItem): boolean;
  end;
  
implementation

uses
  BeeGui_Consts,
  BeeGui_Messages,
  BeeFM_ConfigFrm;

  // ---

  procedure UpdateProgressBar(UpEmpty, UpFull, Empty, Level, Full, DownFull, DownEmpty: TImage; R:TLabel; Percentage: integer);
  begin
    UpEmpty  .Transparent := True;
    UpFull   .Transparent := True;
    Empty    .Transparent := True;
    Level    .Transparent := True;
    Full     .Transparent := True;
    DownFull .Transparent := True;
    DownEmpty.Transparent := True;

    UpEmpty  .Visible := False;
    UpFull   .Visible := False;
    Empty    .Visible := False;
    Level    .Visible := False;
    Full     .Visible := False;
    DownFull .Visible := False;
    DownEmpty.Visible := False;

    UpFull   .Left := UpEmpty.Left;
    Empty    .Left := UpEmpty.Left;
    Level    .Left := UpEmpty.Left;
    Full     .Left := UpEmpty.Left;
    DownFull .Left := UpEmpty.Left;
    DownEmpty.Left := UpEmpty.Left;

    UpFull  .Top   := UpEmpty.Top;
    DownFull.Top   := DownEmpty.Top + DownEmpty.Height - DownFull.Height;

    R.Caption := IntToStr(Percentage) + '%';
    if Percentage > 100 then Percentage := 100;
    case Percentage of
    0..6 : begin
             Empty.Top    := UpEmpty  .Top + UpEmpty.Height;
             Empty.Height := DownEmpty.Top - Empty.Top;

             UpEmpty  .Visible := True;
             Empty    .Visible := True;
             DownEmpty.Visible := True;

             R.Top := DownEmpty.Top - 3;
           end;
    7..99: begin
             case Percentage of
             97..99: Percentage := 96;
             end;
             Full.Height := 1 + (DownFull.Top - UpEmpty.Top - UpEmpty.Height - Level.Height-1) * (Percentage - 7) div (96 - 7);

             Full .Top    := DownFull.Top - Full.Height;
             Level.Top    := Full    .Top - Level.Height;
             Empty.Top    := UpEmpty .Top + UpEmpty.Height;
             Empty.Height := Level   .Top - Empty.Top;

             UpEmpty .Visible := True;
             Empty   .Visible := True;
             Level   .Visible := True;
             Full    .Visible := True;
             DownFull.Visible := True;

             R.Top := Level.Top;
           end;
      100: begin
             Full.Top    := UpFull  .Top + UpFull.Height;
             Full.Height := DownFull.Top - Full.Top;

             UpFull  .Visible := True;
             Full    .Visible := True;
             DownFull.Visible := True;

             R.Top := UpFull.Top;
           end;
    end;
  end;

  { TInfoFrm class }

  procedure TInfoFrm.FormPaint(Sender: TObject);
  begin
    if Assigned(APage) then
    begin
      UpdateProgressBar(
        AUpEmpty,
        AUpFull,
        AEmpty,
        ALevel,
        AFull,
        ADownFull,
        ADownEmpty,
        AR,
        Ratio);
    end;
    if Assigned(FPage) then
    begin
      UpdateProgressBar(
        FUpEmpty,
        FUpFull,
        FEmpty,
        FLevel,
        FFull,
        FDownFull,
        FDownEmpty,
        FR,
        Ratio);
    end;
  end;

  procedure TInfoFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beefm_propertyfrm_loadlanguage.inc}
    {$I beefm_propertyfrm_loadproperty.inc}
  end;

  procedure TInfoFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$IFDEF DEBUG}
      {$I beefm_propertyfrm_savelanguage.inc}
    {$ENDIF}
    {$I beefm_propertyfrm_saveproperty.inc}
  end;

  function TInfoFrm.UpdateAInfo(const ArcName: string; AInfo: TArcDetails): boolean;
  begin
    Result := True;
    if Assigned(AInfo) then
    try
      if AInfo.FilesSize = 0 then
        Ratio  := 0
      else
        Ratio := Round(100 * (AInfo.FilesPacked / AInfo.FilesSize));

      ANameValue.Caption         := ExtractFileName(ArcName);
      AVersionValue.Caption      := FloatToStr(AInfo.Version);
      AFilesValue.Caption        := IntToStr(AInfo.FilesCount);
      ASizeValue.Caption         := SizeToStr(AInfo.FilesSize);
      APackedValue.Caption       := SizeToStr(AInfo.FilesPacked);
      ARatioValue.Caption        := IntToStr(Ratio) + '%';
      AFilesCryptedValue.Caption := IntToStr(AInfo.FilesCrypted);
      AArcSizeValue.Caption      := SizeToStr(SizeOfFile (ArcName));
      AModifiedValue.Caption     := DateTimeToStr(FileDateToDateTime(FileAge(ArcName)));
    except
      Result := False;
    end;
    if Assigned(FPage) then FreeAndNil(FPage);
  end;
  
  function TInfoFrm.UpdateFInfo(FInfo: TArcItem): boolean;
  begin
    Result := True;
    if Assigned(FInfo) then
    try
      if TArcItem(FInfo).FileSize = 0 then
        Ratio  := 0
      else
        Ratio := Round(100 * (TArcItem(FInfo).FilePacked / TArcItem(FInfo).FileSize));

      FNameValue.Caption         := TArcItem(FInfo).FileName;
      FVersionValue.Caption      := TArcItem(FInfo).FileVersion;
      FSizeValue.Caption         := SizeToStr(TArcItem(FInfo).FileSize);
      FPackedValue.Caption       := SizeToStr(TArcItem(FInfo).FilePacked);
      FRatioValue.Caption        := IntToStr(Ratio) + '%';
      FAttributeValue.Caption    := AttrToStr(TArcItem(FInfo).FileAttr);
      FPasswordValue.Caption     := TArcItem(FInfo).FilePassword;
      FMethodValue.Caption       := TArcItem(FInfo).FileMethod;
      FModifiedValue.Caption     := DateTimeToStr(FileDateToDateTime(TArcItem(FInfo).FileTime));
    except
      Result := False;
    end;
    if Assigned(APage) then FreeAndNil(APage);
  end;
  
initialization

  {$I beefm_propertyfrm.lrs}
  
end.
