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

    BeeGui Information form.

    Modifyed:

    v1.0.1 build 9155 - 2005.07.15 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeGui_InfoFrm;

{$R-,Q-,S-}

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

  BeeGui_Components;

type
  TInfoCustomFrm = class (TForm)
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   LoadLanguage;
  end;

type
  TInfoFrm = class (TInfoCustomFrm)
    InfoFrm_Pages: TPageControl;
    InfoFrm_PageGeneral: TTabSheet;
    InfoFrm_ArchiveName_: TLabel;
    InfoFrm_Version_: TLabel;
    InfoFrm_Files_: TLabel;
    InfoFrm_Packed_: TLabel;
    InfoFrm_Packed: TLabel;
    InfoFrm_Ratio_: TLabel;
    InfoFrm_R: TLabel;
    InfoFrm_Ratio: TLabel;
    InfoFrm_Size_: TLabel;
    InfoFrm_Size: TLabel;
    InfoFrm_Files: TLabel;
    InfoFrm_Modified_: TLabel;
    InfoFrm_Modified: TLabel;
    InfoFrm_UpEmpty: TImage;
    InfoFrm_DownFull: TImage;
    InfoFrm_Level: TImage;
    InfoFrm_Empty: TImage;
    InfoFrm_Full: TImage;
    InfoFrm_Bevel_1: TBevel;
    InfoFrm_FilesCrypted_: TLabel;
    InfoFrm_FilesCrypted: TLabel;
    InfoFrm_Bevel2: TBevel;
    InfoFrm_Title: TLabel;
    InfoFrm_Version: TLabel;
    InfoFrm_ArchiveName: TLabel;
    InfoFrm_DownEmpty: TImage;
    InfoFrm_UpFull: TImage;
    InfoFrm_ArcSize_: TLabel;
    InfoFrm_ArcSize: TLabel;
    BtnOk: TBitBtn;
    procedure FormResize (Sender: TObject);
  private
    { Private declarations }
    Ratio : integer;
  public
    { Public declarations }
    function  UpdateInfo (const ArcName: string; Info: TBeeGui_List_Info): boolean;
    procedure UpdateProgressBar;
  end;

var
  InfoFrm: TInfoFrm;

implementation

{$R *.DFM}

uses
  BeeGui_Common,
  BeeGui_ConfigFrm;

/// TInfoCustomFrm class

  constructor TInfoCustomFrm.Create;
  var
    I: integer;
  begin
    inherited Create (AOwner);

    LoadLanguage;
    BeeGui_Configuration.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Configuration.Load_TControl (TControl (Self.Components [I]));
  end;

  destructor TInfoCustomFrm.Destroy;
  begin
    BeeGui_Configuration.Save_TForm (Self);
    inherited Destroy;
  end;

  procedure TInfoCustomFrm.LoadLanguage;
  var
    I: integer;
  begin
    BeeGui_Language.Load_TForm (Self);
    for I := 0 to Self.ComponentCount - 1 do
      BeeGui_Language.Load_TControl (TControl (Self.Components [I]));
  end;

/// TInfoFrm class

  procedure TInfoFrm.UpdateProgressBar;
  var
    Percentage: integer;
  begin
    Percentage := Ratio;

    InfoFrm_UpEmpty.Visible := False;
    InfoFrm_UpFull.Visible := False;
    InfoFrm_Empty.Visible := False;
    InfoFrm_Level.Visible := False;
    InfoFrm_Full.Visible := False;
    InfoFrm_DownFull.Visible := False;
    InfoFrm_DownEmpty.Visible := False;

    InfoFrm_UpFull.Left := InfoFrm_UpEmpty.Left;
    InfoFrm_Empty.Left := InfoFrm_UpEmpty.Left;
    InfoFrm_Level.Left := InfoFrm_UpEmpty.Left;
    InfoFrm_Full.Left := InfoFrm_UpEmpty.Left;
    InfoFrm_DownFull.Left := InfoFrm_UpEmpty.Left;
    InfoFrm_DownEmpty.Left := InfoFrm_UpEmpty.Left;

    InfoFrm_UpFull.Top := InfoFrm_UpEmpty.Top;
    InfoFrm_DownFull.Top := InfoFrm_DownEmpty.Top + InfoFrm_DownEmpty.Height - InfoFrm_DownFull.Height;

    InfoFrm_R.Caption := IntToStr(Percentage) + '%';
    if Percentage > 100 then Percentage := 100;
    case Percentage of
    0..6 : begin
             InfoFrm_Empty.Top := InfoFrm_UpEmpty.Top + InfoFrm_UpEmpty.Height;
             InfoFrm_Empty.Height := InfoFrm_DownEmpty.Top - InfoFrm_Empty.Top;

             InfoFrm_UpEmpty.Visible := True;
             InfoFrm_Empty.Visible := True;
             InfoFrm_DownEmpty.Visible := True;

             InfoFrm_R.Top := InfoFrm_DownEmpty.Top - 3;
           end;
    7..99: begin
             case Percentage of
             97..99: Percentage := 96;
             end;
             InfoFrm_Full.Height := 1 + (InfoFrm_DownFull.Top - InfoFrm_UpEmpty.Top - InfoFrm_UpEmpty.Height - InfoFrm_Level.Height-1) * (Percentage - 7) div (96 - 7);

             InfoFrm_Full.Top := InfoFrm_DownFull.Top - InfoFrm_Full.Height;
             InfoFrm_Level.Top := InfoFrm_Full.Top - InfoFrm_Level.Height;
             InfoFrm_Empty.Top := InfoFrm_UpEmpty.Top + InfoFrm_UpEmpty.Height;
             InfoFrm_Empty.Height := InfoFrm_Level.Top - InfoFrm_Empty.Top;

             InfoFrm_UpEmpty.Visible := True;
             InfoFrm_Empty.Visible := True;
             InfoFrm_Level.Visible := True;
             InfoFrm_Full.Visible := True;
             InfoFrm_DownFull.Visible := True;

             InfoFrm_R.Top := InfoFrm_Level.Top;
           end;
      100: begin
             InfoFrm_Full.Top := InfoFrm_UpFull.Top + InfoFrm_UpFull.Height;
             InfoFrm_Full.Height := InfoFrm_DownFull.Top - InfoFrm_Full.Top;

             InfoFrm_UpFull.Visible := True;
             InfoFrm_Full.Visible := True;
             InfoFrm_DownFull.Visible := True;

             InfoFrm_R.Top := InfoFrm_UpFull.Top;
           end;
    end;
  end;

  function TInfoFrm.UpdateInfo;
  begin
    Result := True;
    if Assigned (Info) then
    try
      if Info.FilesSize = 0 then
        Ratio  := 0
      else
        Ratio := Round (100 * (Info.FilesPacked / Info.FilesSize));

      InfoFrm_ArchiveName.Caption  := ExtractFileName (ArcName);
      InfoFrm_Version.Caption      := FloatToStr (Info.Version);
      InfoFrm_Files.Caption        := IntToStr (Info.FilesCount);
      InfoFrm_Size.Caption         := SizeToStr (Info.FilesSize);
      InfoFrm_Packed.Caption       := SizeToStr (Info.FilesPacked);
      InfoFrm_Ratio.Caption        := IntToStr (Ratio) + '%';
      InfoFrm_FilesCrypted.Caption := IntToStr (Info.FilesCrypted);
      InfoFrm_ArcSize.Caption      := SizeToStr (SizeOfFile (ArcName));
      InfoFrm_Modified.Caption     := DateTimeToString (FileDateToDateTime (FileAge (ArcName)));
    except
      Result := False;
    end;
  end;

  procedure TInfoFrm.FormResize (Sender: TObject);
  begin
    UpdateProgressBar;
  end;

  
end.
