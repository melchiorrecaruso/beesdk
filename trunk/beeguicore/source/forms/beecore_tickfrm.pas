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
  XMLPropStorage;

type

  { TTickFrm }

  TTickFrm = class(TForm)
    Storage: TXMLPropStorage;
    TimeLabel: TLabel;
    Time: TLabel;
    RemainingTimeLabel: TLabel;
    RemainingTime: TLabel;
    GeneralSize: TLabel;
    GeneralSizeLabel: TLabel;
    GeneralSizeUnit: TLabel;
    ProcessedSizeLabel: TLabel;
    ProcessedSize: TLabel;
    ProcessedSizeUnit: TLabel;
    SpeedLabel: TLabel;
    Speed: TLabel;
    SpeedUnit: TLabel;
    Msg: TLabel;
    Tick: TProgressBar;
    BtnForeground: TBitBtn;
    BtnBackGround: TBitBtn;
    BtnCancel: TBitBtn;
    BtnPause: TBitBtn;
    BtnRun: TBitBtn;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure BtnBackGroundClick(Sender: TObject);
    procedure BtnForegroundClick(Sender: TObject);
    procedure BtnPauseClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private
    { public declarations }
  public
    { public declarations }
    Thread: TThread;
  end;
  
var
  TickFrm: TTickFrm;

implementation

uses
  BeeCore_SysUtils;

  { TTickFrm class }

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
    Thread := nil;
  end;

  procedure TTickFrm.BtnRunClick(Sender: TObject);
  begin
    Timer.Enabled := True;
    if Assigned(Thread) then
    begin
      Thread.Suspended := False;
      BtnPause.Visible := True;
      BtnRun.Visible := False;
    end;
  end;

  procedure TTickFrm.BtnPauseClick(Sender: TObject);
  begin
    Timer.Enabled := False;
    if Assigned(Thread) then
    begin
      Thread.Suspended := True;
      BtnPause.Visible := False;
      BtnRun.Visible := True;
    end;
  end;

  procedure TTickFrm.BtnBackGroundClick(Sender: TObject);
  begin
    if Assigned(Thread) then
    begin
      Thread.Priority := tpIdle;
      BtnBackground.Visible := False;
      BtnForeground.Visible := True;
    end;
  end;

  procedure TTickFrm.BtnForegroundClick(Sender: TObject);
  begin
    if Assigned(Thread) then
    begin
      Thread.Priority := tpNormal;
      BtnBackground.Visible := True;
      BtnForeground.Visible := False;
    end;
  end;
  
  procedure TTickFrm.BtnCancelClick(Sender: TObject);
  begin
    if Assigned(Thread) then
    begin
      if Thread.Suspended then
      begin
        Thread.Resume;
      end;
      Thread.Terminate;
    end;
  end;
  
initialization

  {$I beecore_tickfrm.lrs}

end.
