{
  Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU GeneralPage Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU GeneralPage Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

  Modifyed:

}

unit bxm_TickFrm;

{$I bxm_compiler.inc}

interface

uses
  Menus,
  Forms,
  Classes,
  Dialogs,
  Buttons,
  IniFiles,
  SysUtils,
  Graphics,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  LResources;

type

  { TTickFrm }

  TTickFrm = class(TForm)



    Bevel: TBevel;
    ElapsedTime: TLabel;
    ElapsedTimeLabel: TLabel;
    FontDialog: TFontDialog;
    GeneralSize: TLabel;
    GeneralSizeLabel: TLabel;
    GeneralSizeUnit: TLabel;
    GeneralPanel: TPanel;
    Msg: TLabel;
    ProcessedSize: TLabel;
    ProcessedSizeLabel: TLabel;
    ProcessedSizeUnit: TLabel;
    RemainingTime: TLabel;
    RemainingTimeLabel: TLabel;
    Report: TMemo;
    Speed: TLabel;
    SpeedLabel: TLabel;
    SpeedUnit: TLabel;
    Popup_Idle: TMenuItem;
    Popup_TimeCritical: TMenuItem;
    Popup_Higher: TMenuItem;
    Popup_Normal: TMenuItem;
    Popup:   TPopupMenu;
    SaveDialog: TSaveDialog;
    Tick: TProgressBar;
    Timer:   TIdleTimer;
    // ---
    BtnSave: TBitBtn;
    BtnFont: TBitBtn;
    BtnPriority: TBitBtn;
    BtnPauseRun: TBitBtn;
    BtnCancel: TBitBtn;
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    // ---
    procedure PanelChanged(Sender: TObject);
    // ---
    procedure PopupClick(Sender: TObject);
    // ---
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnPriorityClick(Sender: TObject);
    procedure BtnPauseRunClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    // ---
    procedure OnTimer(Sender: TObject);
    procedure OnTerminate;
    procedure OnExecute;
    procedure OnRename;
    procedure OnList;
  private
    { private declarations }
    FCanClose:  boolean;
    FSuspended: boolean;
    FProgressOnTitle: boolean;
  private
    { private declarations }
    function GetFrmCanShow: boolean;
    function GetFrmCanClose: boolean;
  public
    { public declarations }
    property FrmCanShow: boolean Read GetFrmCanShow;
    property FrmCanClose: boolean Read GetFrmCanClose;
    property ProgressOnTitle: boolean Read FProgressOnTitle Write FProgressOnTitle;
  public
    { public declarations }
  public
    { public declarations }
  end;

var
  TickFrm: TTickFrm;

implementation

uses
  bxm_SysUtils;

var
  rsBtnPauseCaption:  string = 'Pause';
  rsBtnRunCaption:    string = 'Run';
  rsBtnCancelCaption: string = 'Cancel';
  rsBtnCloseCaption:  string = 'Close';

{ TTickFrm class }

procedure TTickFrm.FormCreate(Sender: TObject);
begin


  FCanClose    := False;
  FSuspended   := False;

  {$IFDEF UNIX}
  Tick.Smooth := True;
  {$ENDIF}
  FProgressOnTitle := False;
end;

procedure TTickFrm.FormDestroy(Sender: TObject);
begin

end;

procedure TTickFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TTickFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  I: boolean;
begin

end;

function TTickFrm.GetFrmCanClose: boolean;
begin

end;

function TTickFrm.GetFrmCanShow: boolean;
begin

end;

procedure TTickFrm.PopupClick(Sender: TObject);
begin
  Popup_Idle.Checked         := Sender = Popup_Idle;
  Popup_Normal.Checked       := Sender = Popup_Normal;
  Popup_Higher.Checked       := Sender = Popup_Higher;
  Popup_TimeCritical.Checked := Sender = Popup_TimeCritical;


end;

procedure TTickFrm.PanelChanged(Sender: TObject);
var
  I: integer;
begin







  if GeneralPanel.Visible then
  begin
    BtnSave.Enabled   := False;
    BtnFont.Enabled   := False;
    BtnCancel.Kind    := bkCancel;
    BtnCancel.Caption := rsBtnCancelCaption;

    BtnPriority.Enabled := True;
    BtnPauseRun.Enabled := True;
    BtnPauseRun.Caption := rsBtnPauseCaption;
  end else
  begin
    BtnSave.Enabled   := True;
    BtnFont.Enabled   := True;
    BtnCancel.Kind    := bkClose;
    BtnCancel.Caption := rsBtnCloseCaption;

    BtnPriority.Enabled := False;
    BtnPauseRun.Enabled := False;
    BtnPauseRun.Caption := rsBtnPauseCaption;
  end;
  BtnCancel.Cancel := True;
  ActiveControl    := BtnCancel;
end;

 // ------------------------------------------------------------------------ //
 //                                                                          //
 // Timer Events Routines                                                    //
 //                                                                          //
 // ------------------------------------------------------------------------ //

procedure TTickFrm.OnTimer(Sender: TObject);
begin



end;

procedure TTickFrm.OnExecute;
begin


end;

procedure TTickFrm.OnTerminate;
var
  P: PChar;
  I: integer;
begin

end;





 // ------------------------------------------------------------------------ //
 //                                                                          //
 // Buttons Click Routines                                                   //
 //                                                                          //
 // ------------------------------------------------------------------------ //

procedure TTickFrm.BtnPauseRunClick(Sender: TObject);
begin

end;

procedure TTickFrm.BtnPriorityClick(Sender: TObject);
begin

end;

procedure TTickFrm.BtnFontClick(Sender: TObject);
begin
  FontDialog.Font := Report.Font;
  if FontDialog.Execute then
  begin
    Report.Font := FontDialog.Font;
  end;
end;

procedure TTickFrm.BtnSaveClick(Sender: TObject);
var
  FileName: string;
begin
  SaveDialog.FileName := '';
  SaveDialog.Filter   :=
    'Txt file  (*.txt)|*.txt|' + 'Log file  (*.log)|*.log|' +
    'All files (*.*)|*.*|';

  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    case SaveDialog.FilterIndex of
      1: FileName := ChangeFileExt(FileName, '.txt');
      2: FileName := ChangeFileExt(FileName, '.log');
    end;
    Report.Lines.SaveToFile(FileName);
  end;
end;

procedure TTickFrm.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

 // ------------------------------------------------------------------------ //
 //                                                                          //
 // BeeApp Events Routines                                                   //
 //                                                                          //
 // ------------------------------------------------------------------------ //

  (*

  procedure TTickFrm.OnOverwrite;
  var
    F: TOverWriteFrm;
  begin
    if FInterfaces.Terminated = False then
    begin;
      F := TOverWriteFrm.Create(Application);
      with FInterfaces.OnOverWrite do
      begin
        F.SetFileName(Data.FileName);
        F.SetNewFileTime(Data.FileTime);
        F.SetNewFileSize(Data.FileSize);
        F.SetOldFileTime(FileAge(Data.FileName));
        F.SetOldFileSize(SizeOfFile(Data.FileName));

        case F.ShowModal of
          mrAbort   : Answer := 'Q';
          mrNoToAll : Answer := 'S';
          mrYesToAll: Answer := 'A';
          mrNo      : Answer := 'N';
          mrYes     : Answer := 'Y';
          else        Answer := 'N';
        end;
      end;
      F.Free;
    end;
  end;
  
  procedure TTickFrm.OnFatalError;
  begin
    FCommandLine.Log := True;
    with FInterfaces.OnFatalError do
    begin
      Report.Append(Data.Msg);
    end;
  end;
  
  procedure TTickFrm.OnError;
  begin
    FCommandLine.Log := True;
    with FInterfaces.OnError do
    begin
      Report.Append(Data.Msg);
    end;
  end;
  
  procedure TTickFrm.OnWarning;
  begin
    FCommandLine.Log := True;
    with FInterfaces.OnWarning do
    begin
      Report.Append(Data.Msg);
    end;
  end;

  procedure TTickFrm.OnDisplay;
  begin
    with FInterfaces.OnDisplay do
    begin
      if FCommandLine.Log then
      begin
        Report.Append(Data.Msg);
      end;
      Msg.Caption := Data.Msg;
    end;
  end;
  
  procedure TTickFrm.OnRequest;
  begin
    with FInterfaces.OnRequest do
    begin
      if MessageDlg(Data.Msg, mtConfirmation, [mbYes], 0) = mrYes then
      begin
        Answer := 'Y';
      end;
    end;
  end;

  procedure TTickFrm.OnClear;
  begin
    // nothing to do!
  end;


  
  procedure TTickFrm.OnStart;
  begin
    FInterfaces.OnTick.Method := OnTick;
    Timer.Enabled := True;
    OnTick;
  end;

  procedure TTickFrm.OnTick;
  begin
    Tick.Position := FInterfaces.OnTick.Data.Percentage;
    Caption := Format(rsProcessStatus, [FInterfaces.OnTick.Data.Percentage]);
    if FOnlyAForm then
    begin
      Application.Title := Caption;
    end;
  end;
  
  procedure TTickFrm.OnKey;
  var
    F: TPasswordFrm;
  begin
    if FInterfaces.Terminated = False then
    begin
      if FPassword = '' then
      begin
        F := TPasswordFrm.Create(Application);
        F.SetPassword(FPassword);
        if F.ShowModal = mrOK then
        begin
          FPassword := F.Password.Text;
        end;
        F.Free;
      end;
      FInterfaces.OnKey.Answer := FPassword;
    end;
  end;

  *)

initialization

  {$I bxm_tickfrm.lrs}

end.