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

unit BeeGui_InfoFrm;

{$I Compiler.inc}

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
  // --
  BeeGui_ArchiveListManager,
  BeeGui_FormStorage;

type
  TInfoFrm = class(TForm)
    InfoFrm_Pages: TPageControl;
    InfoFrm_PageGeneral: TTabSheet;
    InfoFrm_Title: TLabel;
    InfoFrm_ArchiveName_: TLabel;
    InfoFrm_ArchiveName: TLabel;
    InfoFrm_Version_: TLabel;
    InfoFrm_Version: TLabel;
    InfoFrm_Packed_: TLabel;
    InfoFrm_Packed: TLabel;
    InfoFrm_Ratio_: TLabel;
    InfoFrm_Ratio: TLabel;
    InfoFrm_R: TLabel;
    InfoFrm_Size_: TLabel;
    InfoFrm_Size: TLabel;
    InfoFrm_Files_: TLabel;
    InfoFrm_Files: TLabel;
    InfoFrm_Modified_: TLabel;
    InfoFrm_Modified: TLabel;
    InfoFrm_FilesCrypted_: TLabel;
    InfoFrm_FilesCrypted: TLabel;
    InfoFrm_ArcSize_: TLabel;
    InfoFrm_ArcSize: TLabel;
    InfoFrm_UpEmpty: TImage;
    InfoFrm_DownFull: TImage;
    InfoFrm_Level: TImage;
    InfoFrm_Empty: TImage;
    InfoFrm_Full: TImage;
    InfoFrm_DownEmpty: TImage;
    InfoFrm_UpFull: TImage;
    InfoFrm_Bevel_1: TBevel;
    InfoFrm_Bevel2: TBevel;
    BtnOk: TBitBtn;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FRatio: integer;
    FFormStorage: TFormStorage;
  public
    { Public declarations }
    procedure UpdateProgressBar;
    function UpdateInfo(const ArcName: string; Info: TArchiveListDetails): boolean;
  end;

var
  InfoFrm: TInfoFrm;

implementation

{$R *.dfm}

uses
  BeeGui_InfoFrmRes,
  BeeGui_SysUtils;

// TInfoFrm class

procedure TInfoFrm.FormCreate(Sender: TObject);
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SInfoFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  // Load Language
  FFormStorage.Properties := _SInfoFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SInfoFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
end;

procedure TInfoFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SInfoFrm_PropertyFull
  else
    FFormStorage.Properties := _SInfoFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
end;

procedure TInfoFrm.FormResize(Sender: TObject);
begin
  UpdateProgressBar;
end;

procedure TInfoFrm.UpdateProgressBar;
var
  Percentage: integer;
begin
  Percentage := FRatio;
  // ---
  InfoFrm_UpEmpty.Visible := False;
  InfoFrm_UpFull.Visible := False;
  InfoFrm_Empty.Visible := False;
  InfoFrm_Level.Visible := False;
  InfoFrm_Full.Visible := False;
  InfoFrm_DownFull.Visible := False;
  InfoFrm_DownEmpty.Visible := False;
  // ---
  InfoFrm_UpFull.Left := InfoFrm_UpEmpty.Left;
  InfoFrm_Empty.Left := InfoFrm_UpEmpty.Left;
  InfoFrm_Level.Left := InfoFrm_UpEmpty.Left;
  InfoFrm_Full.Left := InfoFrm_UpEmpty.Left;
  InfoFrm_DownFull.Left := InfoFrm_UpEmpty.Left;
  InfoFrm_DownEmpty.Left := InfoFrm_UpEmpty.Left;

  InfoFrm_UpFull.Top := InfoFrm_UpEmpty.Top;
  InfoFrm_DownFull.Top := InfoFrm_DownEmpty.Top +
    InfoFrm_DownEmpty.Height -
    InfoFrm_DownFull.Height;

  InfoFrm_R.Caption := IntToStr(Percentage) + '%';
  if Percentage > 100 then
    Percentage := 100;
  case Percentage of
    0..6:
    begin
      InfoFrm_Empty.Top :=
        InfoFrm_UpEmpty.Top + InfoFrm_UpEmpty.Height;

      InfoFrm_Empty.Height :=
        InfoFrm_DownEmpty.Top - InfoFrm_Empty.Top;

      InfoFrm_UpEmpty.Visible := True;
      InfoFrm_Empty.Visible := True;
      InfoFrm_DownEmpty.Visible := True;

      InfoFrm_R.Top := InfoFrm_DownEmpty.Top - 3;
    end;
    7..99:
    begin
      case Percentage of
        97..99: Percentage := 96;
      end;
      InfoFrm_Full.Height :=
        1 + (InfoFrm_DownFull.Top - InfoFrm_UpEmpty.Top -
        InfoFrm_UpEmpty.Height - InfoFrm_Level.Height - 1) *
        (Percentage - 7) div (96 - 7);

      InfoFrm_Full.Top  := InfoFrm_DownFull.Top - InfoFrm_Full.Height;
      InfoFrm_Level.Top := InfoFrm_Full.Top - InfoFrm_Level.Height;
      InfoFrm_Empty.Top := InfoFrm_UpEmpty.Top + InfoFrm_UpEmpty.Height;
      InfoFrm_Empty.Height := InfoFrm_Level.Top - InfoFrm_Empty.Top;

      InfoFrm_UpEmpty.Visible := True;
      InfoFrm_Empty.Visible := True;
      InfoFrm_Level.Visible := True;
      InfoFrm_Full.Visible  := True;
      InfoFrm_DownFull.Visible := True;

      InfoFrm_R.Top := InfoFrm_Level.Top;
    end;
    100:
    begin
      InfoFrm_Full.Top := InfoFrm_UpFull.Top + InfoFrm_UpFull.Height;
      InfoFrm_Full.Height := InfoFrm_DownFull.Top - InfoFrm_Full.Top;

      InfoFrm_UpFull.Visible := True;
      InfoFrm_Full.Visible := True;
      InfoFrm_DownFull.Visible := True;

      InfoFrm_R.Top := InfoFrm_UpFull.Top;
    end;
  end;
end;

function TInfoFrm.UpdateInfo(const ArcName: string;
  Info: TArchiveListDetails): boolean;
begin
  Result := True;
  if Assigned(Info) then
    try
      if Info.FilesSize = 0 then
        FRatio := 0
      else
        FRatio := Round(100 * (Info.FilesPacked / Info.FilesSize));

      InfoFrm_ArchiveName.Caption := ExtractFileName(ArcName);
      InfoFrm_Version.Caption := FloatToStr(Info.Version);
      InfoFrm_Files.Caption := IntToStr(Info.FilesCount);
      InfoFrm_Size.Caption  := SizeToStr(Info.FilesSize);
      InfoFrm_Packed.Caption := SizeToStr(Info.FilesPacked);
      InfoFrm_Ratio.Caption := IntToStr(FRatio) + '%';
      InfoFrm_FilesCrypted.Caption := IntToStr(Info.FilesCrypted);
      InfoFrm_ArcSize.Caption := SizeToStr(SizeOfFile(ArcName));
      InfoFrm_Modified.Caption :=
        DateTimeToString(FileDateToDateTime(FileAge(ArcName)));
    except
      Result := False;
    end;
end;

end.
