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
  Bee_Consts,
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
    procedure OnTerminate;
    procedure OnOverWrite;
    procedure OnPassword;
    procedure OnExecute;
    procedure OnRename;
    procedure OnList;
  private
    { private declarations }
    FList:      TList;
    FPassword:  string;
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
  FPassword    := '';
  FList        := nil;
  FCanClose    := False;
  FSuspended   := False;

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
  FPassword    := '';
  FList        := nil;
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
  CanClose := (CoreGetStatus = csTerminated) or (CoreGetStatus = csUnknow);

  if CanClose = False then
  begin
    I := FSuspended;
    if I = False then BtnPauseRun.Click;
    if MessageDlg(rsConfirmation, rsConfirmAbortProcess, mtConfirmation, [mbYes, mbNo], '') = mrYes then
    begin
      CoreTerminate;
      BtnPauseRun.Click;
    end else
      if I = False then BtnPauseRun.Click;
  end;
end;

procedure TTickFrm.Execute(aCommandLine: TCustomCommandLine; aList: TList);
var
  P: PChar;
begin
  FList := aList;
  FCommandLine := aCommandLine;

  P := StringToPChar(FCommandLine.Params.Text);
  CoreCreate(P);
  if CoreExecute then
  begin
    BtnPauseRun.Enabled := True;
    {$IFDEF MSWINDOWS}
    BtnPriority.Enabled := True;
    {$ENDIF}
    Timer.Enabled := True;
  end;
  StrDispose(P);
end;

function TTickFrm.GetFrmCanClose: boolean;
begin
  Result := FCanClose;
end;

function TTickFrm.GetFrmCanShow: boolean;
begin
  Result := CoreGetTime > 0;
end;

procedure TTickFrm.PopupClick(Sender: TObject);
begin
  Popup_Idle.Checked         := Sender = Popup_Idle;
  Popup_Normal.Checked       := Sender = Popup_Normal;
  Popup_Higher.Checked       := Sender = Popup_Higher;
  Popup_TimeCritical.Checked := Sender = Popup_TimeCritical;

  if CoreGetStatus <> csTerminated then
  begin
    if Popup_Idle.Checked         then CoreSetPriority(cpIdle);
    if Popup_Normal.Checked       then CoreSetPriority(cpNormal);
    if Popup_Higher.Checked       then CoreSetPriority(cpHigher);
    if Popup_TimeCritical.Checked then CoreSetPriority(cpTimeCritical);
  end;
end;

procedure TTickFrm.NotebookPageChanged(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to Notebook.PageCount -1 do
  begin
    Notebook.Page[I].TabVisible := False;
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
begin
  Timer.Enabled := False;
  case CoreGetStatus of
    csTerminated: OnTerminate;
    csExecuting: OnExecute;
    csWaitingRename: OnRename;
    csWaitingPassword: OnPassword;
    csWaitingOverWrite: OnOverwrite;
  end;
  Timer.Enabled := CoreGetStatus <> csUnknow;
end;

procedure TTickFrm.OnExecute;
var
  I: int64;
  P: PChar;
begin
  I := CoreGetTotalSize;
  if I < (1024) then
  begin
    GeneralSize.Caption     := IntToStr(I);
    GeneralSizeUnit.Caption := 'B';
  end else
    if I < (1024 * 1024) then
    begin
      GeneralSize.Caption     := IntToStr(I shr 10);
      GeneralSizeUnit.Caption := 'KB';
    end else
    begin
      GeneralSize.Caption     := IntToStr(I shr 20);
      GeneralSizeUnit.Caption := 'MB';
    end;

  I := CoreGetSize;
  if I < (1024) then
  begin
    ProcessedSize.Caption     := IntToStr(I);
    ProcessedSizeUnit.Caption := 'B';
  end else
    if I < (1024 * 1024) then
    begin
      ProcessedSize.Caption     := IntToStr(I shr 10);
      ProcessedSizeUnit.Caption := 'KB';
    end else
    begin
      ProcessedSize.Caption     := IntToStr(I shr 20);
      ProcessedSizeUnit.Caption := 'MB';
    end;

  Tick.Position := CoreGetPercentes;
  if FSuspended = False then
    Caption := Format(rsProcessStatus, [Tick.Position])
  else
    Caption := rsProcessPaused;

  if FProgressOnTitle then
  begin
    Application.Title := Caption;
  end;

  Time.Caption          := TimeToStr(CoreGetTotalTime);
  RemainingTime.Caption := TimeToStr(CoreGetTime);
  Speed.Caption         := IntToStr (CoreGetSpeed shr 10);

  P := CoreGetMessage;
  if P <> nil then
  begin
    Msg.Caption := PCharToString(P);
    CoreFreePChar(P);
  end;
end;

procedure TTickFrm.OnTerminate;
var
  P: PChar;
begin
  ExitCode := CoreGetCode;
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
    P := CoreGetMessages;
    if P <> nil then
    begin
      Report.Lines.Text := PCharToString(P);
      CoreFreePChar(P);
    end;
  end;
  OnList;

  if CoreDestroy then
  begin
    FCanClose := True;
  end;

  if Report.Lines.Count > 0 then
    Notebook.ActivePageComponent := ReportPage
  else
    Close;
end;

procedure TTickFrm.OnRename;
var
  F: TRenameFrm;
  FI: PFileInfo;
  P: PChar;
begin
  F := TRenameFrm.Create(Application);
  F.Caption := rsRenameFile;
  try
    FI := CoreGetRequestItem;

    F.ToFN.Text      := PCharToString(FI^.FilePath) + PCharToString(FI^.FileName);
    F.FromFN.Caption := PCharToString(FI^.FilePath) + PCharToString(FI^.FileName);

    case F.ShowModal of
      mrOk:
      begin
        P := StringToPChar(F.ToFN.Text);
        CoreSetRequest(P);
        StrDispose(P);
      end;
      mrAbort: CoreTerminate;
      else CoreSetRequest(nil);
    end;
    FI := nil;
  finally
    FreeAndNil(F);
  end;
end;

procedure TTickFrm.OnOverwrite;
var
  F: TOverWriteFrm;
  FI: PFileInfo;
  P: PChar;
begin
  F := TOverWriteFrm.Create(Application);
  try
    FI := CoreGetRequestItem;

    F.SetFileName(PCharToString(FI^.FileName));
    F.SetNewFileTime(FI^.FileTime);
    F.SetNewFileSize(FI^.FileSize);
    F.SetOldFileTime(FileAge(FI^.FileName));
    F.SetOldFileSize(SizeOfFile(FI^.FileName));

    case F.ShowModal of
      mrAbort   : P := StringToPChar('Q');
      mrNoToAll : P := StringToPChar('S');
      mrYesToAll: P := StringToPChar('A');
      mrNo      : P := StringToPChar('N');
      mrYes     : P := StringToPChar('Y');
      else        P := StringToPChar('N');
    end;
    CoreSetRequest(P);
    StrDispose(P);

    FI := nil;
  finally
    FreeAndNil(F);
  end;
 end;

procedure TTickFrm.OnPassword;
var
  F: TPasswordFrm;
  FI: PFileInfo;
  P: PChar;
begin
  try
    if FPassword = '' then
    begin
      F := TPasswordFrm.Create(Application);
      F.SetPassword(FPassword);
      if F.ShowModal = mrOK then
      begin
        FPassword := F.Password.Text;
      end;
      FreeAndNil(F);
    end;
  finally
    StringToPChar(FPassword);
  end;
end;

procedure TTickFrm.OnList;
var
  I: integer;
  P: PFileInfoExtra;
  Node: TArchiveItem;
begin
  if Assigned(FList) then
  begin
    for I := 0 to CoreGetItemsCount -1 do
    begin
      P := CoreGetItems(I);
      if P <> nil then
      begin
        Node := TArchiveItem.Create;
        try
          Node.FileName     := PCharToString(P.FileName);
          Node.FilePath     := PCharToString(P.FilePath);
          Node.FileSize     := P.FileSize;
          Node.FilePacked   := P.FilePacked;
          Node.FileRatio    := P.FileRatio;
          Node.FileAttr     := P.FileAttr;
          Node.FileTime     := P.FileTime;
          Node.FileComm     := PCharToString(P.FileComm);
          Node.FileCrc      := P.FileCrc;
          Node.FileMethod   := PCharToString(P.FileMethod);
          Node.FileVersion  := PCharToString(P.FileVersion);
          Node.FilePassword := PCharToString(P.FilePassword);
          Node.FilePosition := P.FilePosition;
        finally
          FList.Add(Node);
        end;
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
  if (CoreGetStatus <> csTerminated) then
  begin
    FSuspended := not FSuspended;
    if FSuspended then
    begin
      BtnPauseRun.Caption := rsBtnRunCaption;
      CoreSuspended(True);
    end else
    begin
      BtnPauseRun.Caption := rsBtnPauseCaption;
      CoreSuspended(False);
    end;
  end;
end;

procedure TTickFrm.BtnPriorityClick(Sender: TObject);
var
  X, Y: integer;
  FValue: integer;
begin
  FValue := CoreGetPriority;
  if FValue <> -1 then
  begin
    Popup_Idle.Checked         := FValue = cpIdle;
    Popup_Normal.Checked       := FValue = cpNormal;
    Popup_Higher.Checked       := FValue = cpHigher;
    Popup_TimeCritical.Checked := FValue = cpTimeCritical;

    X := Left + BtnPriority.Left;
    Y := Top  + BtnPriority.Top + BtnPriority.Height;
    {$IFDEF MSWINDOWS}
    Inc(X, 3);
    Inc(Y, 23);
    {$ELSE}
    Inc(X, 6);
    Inc(Y, 26);
    {$ENDIF}
    Popup.PopUp(X, Y);
  end;
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

  {$I beegui_tickfrm.lrs}

end.
