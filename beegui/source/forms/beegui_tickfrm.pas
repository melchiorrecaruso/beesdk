{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU GeneralPage Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU GeneralPage Public License for more details.

  You should have received a copy of the GNU GeneralPage Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

    BeeGui Tick form.

  Modifyed:

    v1.0.5 build 0559 - 2008.12.22 by Melchiorre Caruso.
}

unit BeeGui_TickFrm;

{$I compiler.inc}

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
  LResources,
  // ---
  Bee_Types,
  Bee_Common,
  // ---
  BeeGui_RenameFrm,
  BeeGui_PasswordFrm,
  BeeGui_OverwriteFrm,
  // ---
  BeeGui_CommandLine,
  BeeGui_ArchiveListViewMgr;

type

  { TTickFrm }

  TTickFrm = class(TForm)
    FontDialog: TFontDialog;
    GeneralSize: TLabel;
    GeneralSizeLabel: TLabel;
    GeneralSizeUnit: TLabel;
    ProcessedSizeLabel: TLabel;
    SizeLabelPanel: TPanel;
    SpeedLabel: TLabel;
    TimePanel: TPanel;
    RemainingTime: TLabel;
    Time:    TLabel;
    UnitPanel: TPanel;
    ProcessedSize: TLabel;
    ProcessedSizeUnit: TLabel;
    SizePanel: TPanel;
    Speed:   TLabel;
    SpeedUnit: TLabel;
    TimeLabelPanel: TPanel;
    Popup_Idle: TMenuItem;
    Popup_TimeCritical: TMenuItem;
    Popup_Higher: TMenuItem;
    Popup_Normal: TMenuItem;
    Popup:   TPopupMenu;
    RemainingTimeLabel: TLabel;
    SaveDialog: TSaveDialog;
    TimeLabel: TLabel;
    Timer:   TIdleTimer;
    // ---
    Notebook: TNotebook;
    GeneralPage: TPage;
    Msg:     TLabel;
    Tick:    TProgressBar;
    ReportPage: TPage;
    Report:  TMemo;
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
    procedure NotebookPageChanged(Sender: TObject);
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
    procedure OnExecuting;
    procedure OnTerminate;
    procedure OnList;
  private
    { private declarations }
    FList:      TList;
    FPassword:  string;
    FCoreID:    integer;
    FCanClose:  boolean;
    FSuspended: boolean;
    FProgressOnTitle: boolean;
    FCommandLine: TCustomCommandLine;
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
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  public
    { public declarations }
    procedure Execute(aCommandLine: TCustomCommandLine; aList: TList);
  end;

var
  TickFrm: TTickFrm;

implementation

uses
  BeeGui_Consts,
  BeeGui_LibLink,
  BeeGui_Messages,
  BeeGui_SysUtils;

var
  rsBtnPauseCaption:  string = 'Pause';
  rsBtnRunCaption:    string = 'Run';
  rsBtnCancelCaption: string = 'Cancel';
  rsBtnCloseCaption:  string = 'Close';

{ TTickFrm class }

  {$I beegui_tickfrm_saveproperty.inc}
  {$I beegui_tickfrm_loadproperty.inc}
  {$I beegui_tickfrm_savelanguage.inc}
  {$I beegui_tickfrm_loadlanguage.inc}

procedure TTickFrm.FormCreate(Sender: TObject);
begin
  FCommandLine := nil;
  FPassword  := '';
  FCoreID    := 0;
  FList      := nil;
  FCanClose  := False;
  FSuspended := False;

  LoadLanguage;
  LoadProperty;
  {$IFDEF UNIX}
  Tick.Smooth := True;
  {$ENDIF}
  FProgressOnTitle := False;
  NotebookPageChanged(Sender);
end;

procedure TTickFrm.FormDestroy(Sender: TObject);
begin
  FCommandLine := nil;
  FPassword := '';
  FCoreID := 0;
  FList := nil;
end;

procedure TTickFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFDEF SAVELANGUAGE}
  SaveLanguage;
  {$ENDIF}
  SaveProperty;
end;

procedure TTickFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  I: boolean;
begin
  CanClose := CoreGetStatus(FCoreID) = csTerminated;
  if CanClose = False then
  begin
    I := FSuspended;
    if I = False then BtnPauseRun.Click;
    if MessageDlg(rsConfirmation, rsConfirmAbortProcess, mtConfirmation, [mbYes, mbNo], '') = mrYes then
    begin
      CoreTerminate(FCoreID);
      BtnPauseRun.Click;
    end else
      if I = False then BtnPauseRun.Click;
  end;
end;

procedure TTickFrm.Execute(aCommandLine: TCustomCommandLine; aList: TList);
begin
  FList := aList;
  FCommandLine := aCommandLine;
  FCoreID := CoreExecute(CoreCreate(FCommandLine.Params.Text));

  {$IFDEF MSWINDOWS}
  BtnPriority.Enabled := True;
  {$ENDIF}
  BtnPauseRun.Enabled := True;

  Timer.Enabled := True;
end;

function TTickFrm.GetFrmCanClose: boolean;
begin
  Result := FCanClose;
end;

function TTickFrm.GetFrmCanShow: boolean;
begin
  Result := CoreGetRemainingTime(FCoreID) > 0;
end;

procedure TTickFrm.PopupClick(Sender: TObject);
begin
  Popup_Idle.Checked   := Sender = Popup_Idle;
  Popup_Normal.Checked := Sender = Popup_Normal;
  Popup_Higher.Checked := Sender = Popup_Higher;
  Popup_TimeCritical.Checked := Sender = Popup_TimeCritical;

  if CoreGetStatus(FCoreID) <> csTerminated then
  begin
    if Popup_Idle.Checked         then CoreSetPriority(FCoreID, tpIdle);
    if Popup_Normal.Checked       then CoreSetPriority(FCoreID, tpNormal);
    if Popup_Higher.Checked       then CoreSetPriority(FCoreID, tpHigher);
    if Popup_TimeCritical.Checked then CoreSetPriority(FCoreID, tpTimeCritical);
  end;
end;

procedure TTickFrm.NotebookPageChanged(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to Notebook.PageCount -1 do
  begin
    Notebook.Page[I].TabVisible := True;
  end;
  Notebook.Page[Notebook.PageIndex].TabVisible := True;

  if Notebook.PageIndex = 0 then
  begin
    BtnSave.Enabled   := False;
    BtnFont.Enabled   := False;
    BtnCancel.Kind    := bkCancel;
    BtnCancel.Caption := rsBtnCancelCaption;

    BtnPriority.Enabled := True;
    BtnPauseRun.Enabled := True;
    BtnPauseRun.Caption := rsBtnPauseCaption;
  end
  else
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
var
  FCoreStatus: integer;
begin
  FCoreStatus := CoreGetStatus(FCoreID);
  case FCoreStatus of
    csTerminated: OnTerminate;
    csExecuting:  OnExecuting;
  else
    Report.Lines.Add(IntToStr(FCoreStatus));
  end;
end;

procedure TTickFrm.OnExecuting;
var
  FTotalSize:     int64;
  FProcessedSize: int64;
  FPercentes:     integer;
begin
  FTotalSize := CoreGetTotalSize(FCoreID);
  if FTotalSize < (1024) then
  begin
    GeneralSize.Caption     := IntToStr(FTotalSize);
    GeneralSizeUnit.Caption := 'B';
  end
  else
  if FTotalSize < (1024 * 1024) then
  begin
    GeneralSize.Caption     := IntToStr(FTotalSize shr 10);
    GeneralSizeUnit.Caption := 'KB';
  end
  else
  begin
    GeneralSize.Caption     := IntToStr(FTotalSize shr 20);
    GeneralSizeUnit.Caption := 'MB';
  end;

  FProcessedSize := CoreGetProcessedSize(FCoreID);
  if FProcessedSize < (1024) then
  begin
    ProcessedSize.Caption     := IntToStr(FProcessedSize);
    ProcessedSizeUnit.Caption := 'B';
  end
  else
  if FProcessedSize < (1024 * 1024) then
  begin
    ProcessedSize.Caption     := IntToStr(FProcessedSize shr 10);
    ProcessedSizeUnit.Caption := 'KB';
  end
  else
  begin
    ProcessedSize.Caption     := IntToStr(FProcessedSize shr 20);
    ProcessedSizeUnit.Caption := 'MB';
  end;

  FPercentes := CoreGetPercentes(FCoreID);
  if not FSuspended then
    Caption := Format(rsProcessStatus, [FPercentes])
  else
    Caption := rsProcessPaused;
  Tick.Position := FPercentes;

  if FProgressOnTitle then
  begin
    Application.Title := Caption;
  end;

  RemainingTime.Caption := TimeToStr(CoreGetRemainingTime(FCoreID));
  Time.Caption := TimeToStr(CoreGetElapsedTime(FCoreID));

  Speed.Caption := IntToStr(CoreGetSpeed(FCoreID) shr 10);
  Msg.Caption   := CoreGetMessage(FCoreID);
end;

procedure TTickFrm.OnTerminate;
begin
  Timer.Enabled := False;

  ExitCode := CoreGetExitCode(FCoreID);
  case ExitCode of
    0: Caption := rsProcessTerminated;
    1: Caption := rsProcessTerminated;
    2: Caption := rsProcessAborted;
  else Caption := rsProcessAborted;
  end;

  if FProgressOnTitle then
  begin
    Application.Title := Caption;
  end;

  Report.Lines.Clear;
  if FCommandLine.Log or (ExitCode > 0) then
  begin
    Report.Lines.Text := CoreGetMessages(FCoreID);
  end;
  OnList;

  CoreDestroy(FCoreID);
  FCoreID   := 0;
  FCanClose := True;

  if Report.Lines.Count > 0 then
    Notebook.ActivePageComponent := ReportPage
  else
    Close;
end;

procedure TTickFrm.OnList;
var
  I: integer;
  Node: TArchiveItem;
  Rec: TFileFullInfoRec;
begin
  if Assigned(FList) then
  begin
    for I := 0 to CoreGetItemsCount(FCoreID) -1 do
    begin
      Rec  := CoreGetItems(FCoreID, I);
      Node := TArchiveItem.Create;
      try
        Node.FileName     := Rec.FileName;
        Node.FilePath     := Rec.FilePath;
        Node.FileSize     := Rec.FileSize;
        Node.FilePacked   := Rec.FilePacked;
        Node.FileRatio    := Rec.FileRatio;
        Node.FileAttr     := Rec.FileAttr;
        Node.FileTime     := Rec.FileTime;
        Node.FileComm     := Rec.FileComm;
        Node.FileCrc      := Rec.FileCrc;
        Node.FileMethod   := Rec.FileMethod;
        Node.FileVersion  := Rec.FileVersion;
        Node.FilePassword := Rec.FilePassword;
        Node.FilePosition := Rec.FilePosition;
      finally
        FList.Add(Node);
      end;
    end;
  end;
 end;

 // ------------------------------------------------------------------------ //
 //                                                                          //
 // Buttons Click Routines                                                   //
 //                                                                          //
 // ------------------------------------------------------------------------ //

procedure TTickFrm.BtnPauseRunClick(Sender: TObject);
begin
  if CoreGetStatus(FCoreID) <> csTerminated then
  begin
    FSuspended := not FSuspended;
    if FSuspended then
    begin
      BtnPauseRun.Caption := rsBtnRunCaption;
      CoreSuspended(FCoreID, True);
    end
    else
    begin
      BtnPauseRun.Caption := rsBtnPauseCaption;
      CoreSuspended(FCoreID, False);
    end;
  end;
end;

procedure TTickFrm.BtnPriorityClick(Sender: TObject);
var
  X, Y: integer;
begin
  Popup_Idle.Checked         := CoreGetPriority(FCoreID) = tpIdle;
  Popup_Normal.Checked       := CoreGetPriority(FCoreID) = tpNormal;
  Popup_Higher.Checked       := CoreGetPriority(FCoreID) = tpHigher;
  Popup_TimeCritical.Checked := CoreGetPriority(FCoreID) = tpTimeCritical;

  X := Left + BtnPriority.Left;
  Y := Top + BtnPriority.Top + BtnPriority.Height;
    {$IFDEF MSWINDOWS}
  Inc(X, 3);
  Inc(Y, 23);
    {$ELSE}
  Inc(X, 6);
  Inc(Y, 26);
    {$ENDIF}
  Popup.PopUp(X, Y);
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
  
  procedure TTickFrm.OnRename;
  var
    F: TRenameFrm;
  begin
    if FInterfaces.Terminated = False then
    begin;
      F := TRenameFrm.Create(Application);
      F.Caption := rsRenameFile;
      with FInterfaces.OnRename do
      begin
        F.ToFN.Text      := Data.FilePath + Data.FileName;
        F.FromFN.Caption := Data.FilePath + Data.FileName;

        if F.ShowModal = mrOk then
          Answer := F.ToFN.Text
        else
          Answer := '';
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

  {$I beegui_tickfrm.lrs}

end.
