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

{ Contains:

    BeeCore Tick form.

  Modifyed:

}

unit BeeCore_TickFrm;

interface

uses
  Forms,
  Classes,
  Dialogs,
  Buttons,
  SysUtils,
  Graphics,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  LResources,
  XMLPropStorage, Spin, EditBtn, FileCtrl;

type

  { TTickFrm }

  TTickFrm = class(TForm)
    BtnPause: TBitBtn;
    GeneralSize: TLabel;
    GeneralSizeLabel: TLabel;
    ProcessedSizeLabel: TLabel;
    SpeedLabel: TLabel;
    TimeLabel: TLabel;
    RemainingTimeLabel: TLabel;
    Time: TLabel;
    RemainingTime: TLabel;
    SpeedUnit: TLabel;
    ProcessedSizeUnit: TLabel;
    GeneralSizeUnit: TLabel;
    ProcessedSize: TLabel;
    Speed: TLabel;
    Storage: TXMLPropStorage;
    Tick: TProgressBar;
    Msg: TLabel;
    BtnForeground: TBitBtn;
    BtnBackGround: TBitBtn;
    BtnCancel: TBitBtn;
    BtnRun: TBitBtn;
    procedure BtnPauseClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnBackGroundClick(Sender: TObject);
    procedure BtnForegroundClick(Sender: TObject);
    constructor Create(AOwner: TComponent; AThread: TThread);
  private
    { public declarations }
    FThread: TThread;
  public
    { public declarations }

  end;

implementation

uses
  Bee_Common,
  BeeCore_SysUtils;

  { TTickFrm class }
  
  constructor TTickFrm.Create(AOwner: TComponent; AThread: TThread);
  begin
    inherited Create(AOwner);
    FThread := AThread;
  end;

  procedure TTickFrm.FormCreate(Sender: TObject);
  var
    CfgFolder: string;
  begin
    CfgFolder := IncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder + ('tickfrm.xml');
    end;
    {$I beecore_tickfrm.inc}
    Storage.Restore;
    // ---
    BtnRun.Top := BtnPause.Top;
    BtnRun.Left := BtnPause.Left;
    BtnForeground.Top := BtnBackground.Top;
    BtnForeground.Left := BtnBackground.Left;
  end;

  procedure TTickFrm.BtnRunClick(Sender: TObject);
  begin
    FThread.Suspended := False;
    BtnPause.Visible := True;
    BtnRun.Visible := False;
  end;

  procedure TTickFrm.BtnPauseClick(Sender: TObject);
  begin
    FThread.Suspended := True;
    BtnPause.Visible := False;
    BtnRun.Visible := True;
  end;

  procedure TTickFrm.BtnBackGroundClick(Sender: TObject);
  begin
    FThread.Priority := tpIdle;
    BtnBackground.Visible := False;
    BtnForeground.Visible := True;
  end;

  procedure TTickFrm.BtnForegroundClick(Sender: TObject);
  begin
    FThread.Priority := tpNormal;
    BtnBackground.Visible := True;
    BtnForeground.Visible := False;
  end;
  
initialization

  {$I beecore_tickfrm.lrs}

end.
