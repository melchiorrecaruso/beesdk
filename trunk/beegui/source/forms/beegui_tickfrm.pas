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
  Bee_App,
  Bee_Common,
  Bee_Interface,
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
    Time: TLabel;
    UnitPanel: TPanel;
    ProcessedSize: TLabel;
    ProcessedSizeUnit: TLabel;
    SizePanel: TPanel;
    Speed: TLabel;
    SpeedUnit: TLabel;
    TimeLabelPanel: TPanel;
    Popup_Idle: TMenuItem;
    Popup_TimeCritical: TMenuItem;
    Popup_Higher: TMenuItem;
    Popup_Normal: TMenuItem;
    Popup: TPopupMenu;
    RemainingTimeLabel: TLabel;
    SaveDialog: TSaveDialog;
    TimeLabel: TLabel;
    Timer: TIdleTimer;
    // ---
    Notebook: TNotebook;
    GeneralPage: TPage;
    Msg: TLabel;
    Tick: TProgressBar;
    ReportPage: TPage;
    Report: TMemo;
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

    // ---
    procedure PopupClick(Sender: TObject);
    // ---
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnPriorityClick(Sender: TObject);
    procedure BtnPauseRunClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    // ---
    procedure OnStartTimer(Sender: TObject);
    procedure OnStopTimer(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure OnIdleTimer(Sender: TObject);
    procedure OnTerminate(Sender: TObject);
  private
    { private declarations }
    FCommandLine: TCustomCommandLine;
    FList: TList;
    FPassword: string;
    FCoreID: pointer;
    FCanClose: boolean;
    FOnlyAForm: boolean;
  private
    { private declarations }
    function GetFrmCanShow: boolean;
    function GetFrmCanClose: boolean;
  public
    { public declarations }
    property FrmCanShow: boolean read GetFrmCanShow;
    property FrmCanClose: boolean read GetFrmCanClose;
    property OnlyAForm: boolean read FOnlyAForm write FOnlyAForm;
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  public
    { public declarations }
    procedure Execute(aCommandLine: TCustomCommandLine; aList: TList);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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

  constructor TTickFrm.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FCommandLine := nil;
    FPassword    := '';
    FCoreID      := nil;
    FList        := nil;
    FOnlyAForm := False;
    FCanClose  := False;
    {$IFDEF UNIX}
    Tick.Smooth := True;
    {$ENDIF}
  end;

  destructor TTickFrm.Destroy;
  begin
    FCommandLine := nil;
    FPassword    := '';
    FCoreID      := nil;
    FList        := nil;
    inherited Destroy;
  end;

  procedure TTickFrm.FormCreate(Sender: TObject);
  begin
    LoadLanguage;
    LoadProperty;

    ActiveControl := BtnCancel;
    Notebook.ActivePageComponent := GeneralPage;

    BtnPauseRun.Caption := rsBtnPauseCaption;
    BtnCancel.Caption   := rsBtnCancelCaption;

    BtnCancel.Enabled   := True;
    BtnPriority.Enabled := False;
    BtnPauseRun.Enabled := False;
    BtnSave.Enabled     := False;
    BtnFont.Enabled     := False;
  end;
  
  procedure TTickFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    {$IFDEF SAVELANGUAGE}
    SaveLanguage;
    {$ENDIF}
    SaveProperty;
  end;

  procedure TTickFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
  begin
    CanClose := GetCoreStatus(FCoreID) = csTerminated;
    if CanClose = False then
    begin
      if MessageDlg(rsConfirmation, rsConfirmAbortProcess, mtConfirmation, [mbYes, mbNo], '') = mrYes then
      begin
        CoreTerminate(FCoreID);
      end;
    end;
  end;

  procedure TTickFrm.Execute(aCommandLine: TCustomCommandLine; aList: TList);
  begin
    FList := aList;
    FCommandLine := aCommandLine;
    FCoreID := CoreExecute(CoreCreate(FCommandLine.Params.Text));
  end;

  function TTickFrm.GetFrmCanClose: boolean;
  begin
    Result := FCanClose;
  end;

  function TTickFrm.GetFrmCanShow: boolean;
  begin
    Result := GetCoreRemainingTime(FCoreID)  > 0;
  end;
  
  procedure TTickFrm.PopupClick(Sender: TObject);
  begin
    Popup_Idle        .Checked := Sender = Popup_Idle;
    Popup_Normal      .Checked := Sender = Popup_Normal;
    Popup_Higher      .Checked := Sender = Popup_Higher;
    Popup_TimeCritical.Checked := Sender = Popup_TimeCritical;

    if GetCoreStatus(FCoreID) <> csTerminated then
    begin
      if Popup_Idle        .Checked then SetCorePriority(FCoreID, tpIdle);
      if Popup_Normal      .Checked then SetCorePriority(FCoreID, tpNormal);
      if Popup_Higher      .Checked then SetCorePriority(FCoreID, tpHigher);
      if Popup_TimeCritical.Checked then SetCorePriority(FCoreID, tpTimeCritical);
    end;
  end;
  
  // ------------------------------------------------------------------------ //
  //                                                                          //
  // Timer Events Routines                                                    //
  //                                                                          //
  // ------------------------------------------------------------------------ //

  procedure TTickFrm.OnIdleTimer(Sender: TObject);
  begin
    if FCoreID <> nil then
    begin
      if GetCoreStatus(FCoreID) <> csTerminated then
      begin
        OnStartTimer(Sender);
        OnTimer(Sender);


      end else
      begin
        OnStopTimer(Sender);
        CoreDestroy(FCoreID);
        FCoreID := nil;
        OnTerminate(Sender);
      end;
    end;
    Timer.Enabled := FCoreID <> nil;
  end;

  procedure TTickFrm.OnStartTimer(Sender: TObject);
  var
    FTotalSize: int64;
  begin
    FTotalSize := GetCoreTotalSize(FCoreID);
    if FTotalSize < (1024) then
    begin
      GeneralSize.Caption := IntToStr(FTotalSize);
      GeneralSizeUnit.Caption := 'B';
    end else
      if FTotalSize < (1024*1024) then
      begin
        GeneralSize.Caption := IntToStr(FTotalSize shr 10);
        GeneralSizeUnit.Caption := 'KB';
      end else
      begin
        GeneralSize.Caption := IntToStr(FTotalSize shr 20);
        GeneralSizeUnit.Caption := 'MB';
      end;

    {$IFDEF MSWINDOWS}
    BtnPriority.Enabled := True;
    {$ENDIF}
    BtnPauseRun.Enabled := True;
  end;

  procedure TTickFrm.OnTimer(Sender: TObject);
  var
    FProcessedSize: int64;
  begin
    FProcessedSize := GetCoreProcessedSize(FCoreID);
    if FProcessedSize < (1024) then
    begin
      ProcessedSize.Caption := IntToStr(FProcessedSize);
      ProcessedSizeUnit.Caption := 'B';
    end else
      if FProcessedSize < (1024*1024) then
      begin
        ProcessedSize.Caption := IntToStr(FProcessedSize shr 10);
        ProcessedSizeUnit.Caption := 'KB';
      end else
      begin
        ProcessedSize.Caption := IntToStr(FProcessedSize shr 20);
        ProcessedSizeUnit.Caption := 'MB';
      end;

    Time.Caption := TimeToStr(GetCoreElapsedTime(FCoreID));
    RemainingTime.Caption := TimeToStr(GetCoreRemainingTime(FCoreID));
    Speed.Caption := IntToStr(GetCoreSpeed(FCoreID));

    Tick.Position := GetCorePercentes(FCoreID);
    Msg.Caption := GetCoreMessage(FCoreID);
  end;
  
  procedure TTickFrm.OnStopTimer(Sender: TObject);
  begin
    if GetCoreStatus(FCoreID) = csTerminated then
    begin
      if GetCoreExitCode(FCoreID) < 2 then
        Caption := rsProcessTerminated
      else
        Caption := rsProcessAborted;

      CoreDestroy(FCoreID);
      FCoreID := nil;
    end else
      Caption := rsProcessPaused;

    if FOnlyAForm then
    begin
      Application.Title := Caption;
    end;
    {$IFDEF MSWINDOWS}
    BtnPriority.Enabled := False;
    {$ENDIF}
    BtnPauseRun.Enabled := True;
  end;

  // ------------------------------------------------------------------------ //
  //                                                                          //
  // Buttons Click Routines                                                   //
  //                                                                          //
  // ------------------------------------------------------------------------ //

  procedure TTickFrm.BtnPauseRunClick(Sender: TObject);
  begin
    if GetCoreStatus(FCoreID) <> csTerminated then
    begin
      Timer.Enabled := not Timer.Enabled;
      if Timer.Enabled then
      begin
        CoreSuspended(FCoreID, False);
        BtnPauseRun.Caption := rsBtnPauseCaption;
      end else
      begin
        CoreSuspended(FCoreID, True);
        BtnPauseRun.Caption := rsBtnRunCaption;
      end;
    end;
  end;

  procedure TTickFrm.BtnPriorityClick(Sender: TObject);
  var
    X, Y: integer;
  begin
    Popup_Idle        .Checked := GetCorePriority(FCoreID) = tpIdle;
    Popup_Normal      .Checked := GetCorePriority(FCoreID) = tpNormal;
    Popup_Higher      .Checked := GetCorePriority(FCoreID) = tpHigher;
    Popup_TimeCritical.Checked := GetCorePriority(FCoreID) = tpTimeCritical;

    X := Left + BtnPriority.Left;
    Y := Top  + BtnPriority.Top + BtnPriority.Height;
    {$IFDEF MSWINDOWS}
    Inc(X, 3); Inc(Y, 23);
    {$ELSE}
    Inc(X, 6); Inc(Y, 26);
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
    SaveDialog.Filter :=
      'Txt file  (*.txt)|*.txt|' +
      'Log file  (*.log)|*.log|' +
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

  procedure TTickFrm.OnTerminate(Sender: TObject);
  begin
    if FCoreID = nil then
    begin
      FCanClose := True;
      Timer.Enabled := False;

      BtnPriority.Enabled := False;
      BtnPauseRun.Enabled := False;

      BtnCancel.Kind    := bkClose;
      BtnCancel.Caption := rsBtnCloseCaption;
      BtnCancel.Cancel  := True;

      if Report.Lines.Count > 0 then
      begin
        ActiveControl := BtnCancel;
        GeneralPage.TabVisible := False;

        BtnSave.Enabled := True;
        BtnFont.Enabled := True;
      end else
      begin;
        Close;
      end;
    end;
  end;

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

  procedure TTickFrm.OnList;
  var
    Node: TArchiveItem;
  begin
    if Assigned(FArchiveList) then
    begin
      Node := TArchiveItem.Create;
      try
        Node.FileName     := FInterfaces.OnList.Data.FileName;
        Node.FilePath     := FInterfaces.OnList.Data.FilePath;
        Node.FileSize     := FInterfaces.OnList.Data.FileSize;
        Node.FilePacked   := FInterfaces.OnList.Data.FilePacked;
        Node.FileRatio    := FInterfaces.OnList.Data.FileRatio;
        Node.FileAttr     := FInterfaces.OnList.Data.FileAttr;
        Node.FileTime     := FInterfaces.OnList.Data.FileTime;
        Node.FileComm     := FInterfaces.OnList.Data.FileComm;
        Node.FileCrc      := FInterfaces.OnList.Data.FileCrc;
        Node.FileMethod   := FInterfaces.OnList.Data.FileMethod;
        Node.FileVersion  := FInterfaces.OnList.Data.FileVersion;
        Node.FilePassword := FInterfaces.OnList.Data.FilePassword;
        Node.FilePosition := FInterfaces.OnList.Data.FilePosition;
      finally
        FArchiveList.Add(Node);
      end;
    end;

    if FCommandLine.Log then
    begin
      with FInterfaces.OnList.Data do
      begin
        Report.Append(FilePath + FileName);
      end;
    end;
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
