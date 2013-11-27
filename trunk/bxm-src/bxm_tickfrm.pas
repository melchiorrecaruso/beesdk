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

    Tick form class.

  Modifyed:

}

unit bxm_TickFrm;

{$I bxm_compiler.inc}

interface

uses
  Buttons,
  Dialogs,
  Classes,
  ComCtrls,
  Controls,
  Graphics,
  ExtCtrls,
  Forms,
  IniFiles,
  LResources,
  Menus,
  StdCtrls, Arrow,
  SysUtils;

type

  { TTickFrm }

  TTickFrm = class(TForm)
    Arrow1: TArrow;



    ElapsedTime: TLabel;
    ElapsedTimeLabel: TLabel;
    GeneralSize: TLabel;
    GeneralSizeLabel: TLabel;
    GeneralSizeUnit: TLabel;
    GeneralPanel: TPanel;
    Label1: TLabel;
    Msg: TLabel;
    ReportPanel: TPanel;
    ProcessedSize: TLabel;
    ProcessedSizeLabel: TLabel;
    ProcessedSizeUnit: TLabel;
    RemainingTime: TLabel;
    RemainingTimeLabel: TLabel;
    Report: TMemo;
    Speed: TLabel;
    SpeedLabel: TLabel;
    SpeedUnit: TLabel;
    Tick: TProgressBar;
    Timer:   TIdleTimer;
    BtnCancel: TBitBtn;
    // ---
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    // ---
    procedure PanelChanged(Sender: TObject);
    // ---
    procedure OnTimer(Sender: TObject);
    procedure OnTerminate;
    procedure OnExecute;
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

  function TickShowModal: longint;

implementation

{$R *.lfm}

uses
  bxm_SysUtils;

var
  rsBtnPauseCaption:  string = 'Pause';
  rsBtnRunCaption:    string = 'Run';
  rsBtnCancelCaption: string = 'Cancel';
  rsBtnCloseCaption:  string = 'Close';

function TickShowModal: longint;
var
  Tick: TTickFrm;
begin
  Tick := TTickFrm.Create(nil);
  Result := Tick.ShowModal;
  if Result = mrOk then
  begin

  end;
end;

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

procedure TTickFrm.PanelChanged(Sender: TObject);
var
  I: integer;
begin







  if GeneralPanel.Visible then
  begin

    BtnCancel.Kind    := bkCancel;
    BtnCancel.Caption := rsBtnCancelCaption;



  end else
  begin

    BtnCancel.Kind    := bkClose;
    BtnCancel.Caption := rsBtnCloseCaption;


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

end.