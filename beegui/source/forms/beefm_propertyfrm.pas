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
    InfoFrm_AArcSize: TLabel;
    InfoFrm_AArcSize_: TLabel;
    InfoFrm_ADownEmpty: TImage;
    InfoFrm_ADownFull: TImage;
    InfoFrm_AEmpty: TImage;
    InfoFrm_AFiles: TLabel;
    InfoFrm_AFilesCrypted: TLabel;
    InfoFrm_AFilesCrypted_: TLabel;
    InfoFrm_AFiles_: TLabel;
    InfoFrm_AFull: TImage;
    InfoFrm_ALevel: TImage;
    InfoFrm_AModified: TLabel;
    InfoFrm_AModified_: TLabel;
    InfoFrm_AName: TLabel;
    InfoFrm_AName_: TLabel;
    InfoFrm_APacked: TLabel;
    InfoFrm_APacked_: TLabel;
    InfoFrm_AR: TLabel;
    InfoFrm_ARatio: TLabel;
    InfoFrm_ARatio_: TLabel;
    InfoFrm_ASize: TLabel;
    InfoFrm_ASize_: TLabel;
    InfoFrm_AUpEmpty: TImage;
    InfoFrm_AUpFull: TImage;
    InfoFrm_AVersion: TLabel;
    InfoFrm_AVersion_: TLabel;
    InfoFrm_Bevel_1: TBevel;
    InfoFrm_Bevel_2: TBevel;
    InfoFrm_Bevel_4: TBevel;
    InfoFrm_Bevel_5: TBevel;
    InfoFrm_FAttribute: TLabel;
    InfoFrm_FAttribute_: TLabel;
    InfoFrm_FModified: TLabel;
    InfoFrm_FModified_: TLabel;
    InfoFrm_FDownEmpty: TImage;
    InfoFrm_FDownFull: TImage;
    InfoFrm_FEmpty: TImage;
    InfoFrm_FFull: TImage;
    InfoFrm_FLevel: TImage;
    InfoFrm_FMethod: TLabel;
    InfoFrm_FMethod_: TLabel;
    InfoFrm_FName: TLabel;
    InfoFrm_FName_: TLabel;
    InfoFrm_FPacked: TLabel;
    InfoFrm_FPacked_: TLabel;
    InfoFrm_FPassword: TLabel;
    InfoFrm_FPassword_: TLabel;
    InfoFrm_FR: TLabel;
    InfoFrm_FRatio: TLabel;
    InfoFrm_FRatio_: TLabel;
    InfoFrm_FSize: TLabel;
    InfoFrm_FSize_: TLabel;
    InfoFrm_FUpEmpty: TImage;
    InfoFrm_FUpFull: TImage;
    InfoFrm_FVersion: TLabel;
    InfoFrm_FVersion_: TLabel;
    InfoFrm_FPage: TTabSheet;

    InfoFrm_ArchiveName_1: TLabel;
    InfoFrm_Bevel_3: TBevel;
    InfoFrm_DownFull1: TImage;
    InfoFrm_Empty1: TImage;
    InfoFrm_Files1: TLabel;
    InfoFrm_Files_1: TLabel;
    InfoFrm_Full1: TImage;
    InfoFrm_Level1: TImage;
    InfoFrm_Modified1: TLabel;
    InfoFrm_Modified_1: TLabel;
    InfoFrm_Packed1: TLabel;
    InfoFrm_Packed_1: TLabel;
    InfoFrm_PageGeneral1: TTabSheet;
    InfoFrm_Pages: TPageControl;
    InfoFrm_APage: TTabSheet;

    InfoFrm_Ratio1: TLabel;
    InfoFrm_Ratio_1: TLabel;
    InfoFrm_Size1: TLabel;
    InfoFrm_Size_1: TLabel;
    InfoFrm_UpEmpty1: TImage;


    InfoFrm_Version_1: TLabel;

    BtnOk: TBitBtn;
    InfoFrm_APanel: TPanel;
    InfoFrm_FPanel: TPanel;
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
    if Assigned(InfoFrm_APage) then
    begin
      UpdateProgressBar(
        InfoFrm_AUpEmpty,
        InfoFrm_AUpFull,
        InfoFrm_AEmpty,
        InfoFrm_ALevel,
        InfoFrm_AFull,
        InfoFrm_ADownFull,
        InfoFrm_ADownEmpty,
        InfoFrm_AR,
        Ratio);
    end;
    if Assigned(InfoFrm_FPage) then
    begin
      UpdateProgressBar(
        InfoFrm_FUpEmpty,
        InfoFrm_FUpFull,
        InfoFrm_FEmpty,
        InfoFrm_FLevel,
        InfoFrm_FFull,
        InfoFrm_FDownFull,
        InfoFrm_FDownEmpty,
        InfoFrm_FR,
        Ratio);
    end;
  end;

  procedure TInfoFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin

  end;

  procedure TInfoFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin

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

      InfoFrm_AName.Caption         := ExtractFileName(ArcName);
      InfoFrm_AVersion.Caption      := FloatToStr(AInfo.Version);
      InfoFrm_AFiles.Caption        := IntToStr(AInfo.FilesCount);
      InfoFrm_ASize.Caption         := SizeToStr(AInfo.FilesSize);
      InfoFrm_APacked.Caption       := SizeToStr(AInfo.FilesPacked);
      InfoFrm_ARatio.Caption        := IntToStr(Ratio) + '%';
      InfoFrm_AFilesCrypted.Caption := IntToStr(AInfo.FilesCrypted);
      InfoFrm_AArcSize.Caption      := SizeToStr(SizeOfFile (ArcName));
      InfoFrm_AModified.Caption     := DateTimeToStr(FileDateToDateTime(FileAge(ArcName)));
    except
      Result := False;
    end;
    if Assigned(InfoFrm_FPage) then InfoFrm_FPage.Free;
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

      InfoFrm_FName.Caption         := TArcItem(FInfo).FileName;
      InfoFrm_FVersion.Caption      := TArcItem(FInfo).FileVersion;
      InfoFrm_FSize.Caption         := SizeToStr(TArcItem(FInfo).FileSize);
      InfoFrm_FPacked.Caption       := SizeToStr(TArcItem(FInfo).FilePacked);
      InfoFrm_FRatio.Caption        := IntToStr(Ratio) + '%';
      InfoFrm_FAttribute.Caption    := AttrToStr(TArcItem(FInfo).FileAttr);
      InfoFrm_FPassword.Caption     := TArcItem(FInfo).FilePassword;
      InfoFrm_FMethod.Caption       := TArcItem(FInfo).FileMethod;
      InfoFrm_FModified.Caption     := DateTimeToStr(FileDateToDateTime(TArcItem(FInfo).FileTime));
    except
      Result := False;
    end;
    if Assigned(InfoFrm_APage) then InfoFrm_APage.Free;
  end;
  
initialization

  {$I beefm_propertyfrm.lrs}
  
end.
