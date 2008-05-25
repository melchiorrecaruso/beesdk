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

    BeeCore Tick form.

  Modifyed:

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
  Bee_Assembler,
  Bee_Interface,
  // ---
  BeeGui_CmdLine,
  // ---
  BeeGui_ViewFrm,
  BeeGui_RenameFrm,
  BeeGui_PasswordFrm,
  BeeGui_OverwriteFrm;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    // ---
    procedure HandleClick(Sender: TObject);
    procedure PopupClick(Sender: TObject);
    // --- OK
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnPriorityClick(Sender: TObject);
    procedure BtnPauseRunClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    // --- OK
    procedure OnStartTimer(Sender: TObject);
    procedure OnStopTimer(Sender: TObject);
    procedure OnTimer(Sender: TObject);
  private
    // --- OK
    procedure OnTerminate(Sender: TObject);
    procedure OnFatalError;
    procedure OnOverWrite;
    procedure OnWarning;
    procedure OnDisplay;
    procedure OnRequest;
    procedure OnRename;
    procedure OnError;
    procedure OnClear;
    procedure OnList;
    procedure OnTick;
    procedure OnKey;
  private
    { private declarations }
    FInterfaces: TInterfaces;
    FContents: TStringList;
    FCmdLine: TCmdLine;
    FPassword: string;
    FApp: TBeeApp;
    FElapsedTime: integer;
    FRemainingTime: integer;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure InitInterfaces(AInterfaces: TInterfaces);
    procedure InitCmdLine(ACmdLine: TCmdLine);
    destructor Destroy; override;
  end;
  
implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  BeeGui_SysUtils,
  BeeGui_Messages;
  
var
  rsBtnPauseCaption:  string = 'Pause';
  rsBtnRunCaption:    string = 'Run';
  rsBtnCancelCaption: string = 'Cancel';
  rsBtnCloseCaption:  string = 'Close';
  
  { TTickFrm class }
  
  constructor TTickFrm.Create(AOwner: TComponent); //OK
  begin
    inherited Create(AOwner);
    FPassword := '';
    FCmdLine := nil;
    FInterfaces := nil;
    FContents := TStringList.Create;
    {$IFDEF UNIX}
    BtnPriority.Enabled := False;
    {$ENDIF}
    FElapsedTime := 0;
    FRemainingTime := 0;
  end;

  destructor TTickFrm.Destroy; //OK
  begin
    FPassword := '';
    FContents.Free;
    FContents := nil;
    FInterfaces := nil;
    FCmdLine := nil;
    inherited Destroy;
  end;

  procedure TTickFrm.InitInterfaces(AInterfaces: TInterfaces);
  begin
    FInterfaces := AInterfaces;
    FInterfaces.OnFatalError.Method := OnFatalError;
    FInterfaces.OnOverWrite.Method := OnOverWrite;
    FInterfaces.OnWarning.Method := OnWarning;
    FInterfaces.OnDisplay.Method := OnDisplay;
    FInterfaces.OnRequest.Method := OnRequest;
    FInterfaces.OnRename.Method := OnRename;
    FInterfaces.OnClear.Method := OnClear;
    FInterfaces.OnError.Method := OnError;
    FInterfaces.OnList.Method := OnList;
    FInterfaces.OnTick.Method := OnTick;
    FInterfaces.OnKey.Method := OnKey;
  end;
  
  procedure TTickFrm.InitCmdLine(ACmdLine: TCmdLine);
  begin
    FCmdLine := ACmdLine;
    if FCmdLine.Log then
    begin
      // completare
    end;
    FApp := TBeeApp.Create(FInterfaces, FCmdLine.Params);
    FApp.OnTerminate := OnTerminate;
    FApp.Resume;
    // ---
    Timer.Enabled := True;
  end;
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  procedure TTickFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_tickfrm_loadlanguage.inc}
    {$I beegui_tickfrm_loadproperty.inc}
    // {$ifdef Windows}
    // TrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1));
    // {$endif}
    // TrayIcon.Hint := 'BeeCore';
    // TrayIcon.OnClick := HandleClick;
    // TrayIcon.Popup := Popup;
    // --
    Notebook.ActivePageComponent := GeneralPage;
    BtnPauseRun.Caption := rsBtnPauseCaption;
    BtnCancel.Caption   := rsBtnCancelCaption;
    ActiveControl := BtnCancel;
  end;

  procedure TTickFrm.FormWindowStateChange(Sender: TObject);
  begin
    // if WindowState = wsNormal then
    // begin
    //  TrayIcon.Visible := False;
    // end else
    // begin
    //  TrayIcon.Visible := True;
    // end;
  end;
  
  procedure TTickFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_tickfrm_savelanguage.inc}
    {$I beegui_tickfrm_saveproperty.inc}
  end;

  procedure TTickFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
  begin
    CanClose := FInterfaces.Properties.Terminated;
    if CanClose = False then
    begin
      if MessageDlg(rsConfirmation, rsConfirmAbort, mtConfirmation, [mbYes, mbNo], '') = mrYes then
      begin
        if FInterfaces.Properties.Suspended then
        begin
          FInterfaces.Properties.Suspended := False;
        end;
        FInterfaces.Properties.Aborted := True;
      end;
    end;
  end;
  

  

  
  procedure TTickFrm.HandleClick(Sender: TObject);
  begin
    WindowState := wsNormal;
  end;
  
  procedure TTickFrm.PopupClick(Sender: TObject);
  begin
    Popup_Idle        .Checked := Sender = Popup_Idle;
    Popup_Normal      .Checked := Sender = Popup_Normal;
    Popup_Higher      .Checked := Sender = Popup_Higher;
    Popup_TimeCritical.Checked := Sender = Popup_TimeCritical;

    if FInterfaces.Properties.Terminated = False then
    begin
      if Popup_Idle        .Checked then FApp.Priority := tpIdle;
      if Popup_Normal      .Checked then FApp.Priority := tpNormal;
      if Popup_Higher      .Checked then FApp.Priority := tpHigher;
      if Popup_TimeCritical.Checked then FApp.Priority := tpTimeCritical;
    end;
  end;
  

  
  // ------------------------------------------------------------------------ //
  //                                                                          //
  // Timer Events                                                             //
  //                                                                          //
  // ------------------------------------------------------------------------ //
  
  procedure TTickFrm.OnStartTimer(Sender: TObject);
  begin
    with FInterfaces.OnTick.Data do
    begin
      if TotalSize < (1024) then
      begin
        Self.GeneralSize.Caption := IntToStr(TotalSize);
        Self.GeneralSizeUnit.Caption := 'B';
      end else
        if TotalSize < (1024*1024) then
        begin
          Self.GeneralSize.Caption := IntToStr(TotalSize shr 10);
          Self.GeneralSizeUnit.Caption := 'KB';
        end else
        begin
          Self.GeneralSize.Caption := IntToStr(TotalSize shr 20);
          Self.GeneralSizeUnit.Caption := 'MB';
        end;
    end;
  end;

  procedure TTickFrm.OnTimer(Sender: TObject);
  var
    iSpeed: integer;
    iRemainSize: integer;
  begin
    with FInterfaces.OnTick.Data do
    begin
      if ProcessedSize < (1024) then
      begin
        Self.ProcessedSize.Caption := IntToStr(ProcessedSize);
        Self.ProcessedSizeUnit.Caption := 'B';
      end else
        if ProcessedSize < (1024*1024) then
        begin
          Self.ProcessedSize.Caption := IntToStr(ProcessedSize shr 10);
          Self.ProcessedSizeUnit.Caption := 'KB';
        end else
        begin
          Self.ProcessedSize.Caption := IntToStr(ProcessedSize shr 20);
          Self.ProcessedSizeUnit.Caption := 'MB';
        end;
      Inc(FElapsedTime);
      iSpeed := ProcessedSize div FElapsedTime;
      iRemainSize := TotalSize - ProcessedSize;
    end;
    Time.Caption := TimeToStr(FElapsedTime);
    Speed.Caption := IntToStr(iSpeed shr 10);

    if iSpeed > 0 then
    begin
      FRemainingTime := iRemainSize div iSpeed;
      RemainingTime.Caption := TimeToStr(FRemainingTime);
    end else
    begin
      FRemainingTime := 0;
      RemainingTime.Caption := '--:--:--';
    end;
  end;
  
  procedure TTickFrm.OnStopTimer(Sender: TObject);
  var
    F: TViewFrm;
    I: integer;
  begin
    if FInterfaces.Properties.Terminated then
    begin
      Caption := rsProcessTerminated;
      Application.Title := rsProcessTerminated;

      Time.Caption := TimeToStr(FElapsedTime);
      RemainingTime.Caption := TimeToStr(0);

      ProcessedSize.Caption := GeneralSize.Caption;
      ProcessedSizeUnit.Caption := GeneralSizeUnit.Caption;

      Speed.Caption := IntToStr(0);
      Msg.Caption := '...';
      Tick.Position := 0;
    end;
  end;

  // ------------------------------------------------------------------------ //
  //                                                                          //
  // Buttons click procedure                                                  //
  //                                                                          //
  // ------------------------------------------------------------------------ //

  procedure TTickFrm.BtnPauseRunClick(Sender: TObject);
  begin
    if FInterfaces.Properties.Terminated = False then
    begin
      if FInterfaces.Properties.Suspended then
      begin
        Timer.Enabled  := True;
        BtnPauseRun.Caption := rsBtnPauseCaption;
        FInterfaces.Properties.Suspended := False;
      end else
      begin
        Timer.Enabled  := False;
        BtnPauseRun.Caption := rsBtnRunCaption;
        FInterfaces.Properties.Suspended := True;
      end;
    end;
  end;

  procedure TTickFrm.BtnPriorityClick(Sender: TObject);
  var
    X, Y: integer;
  begin
    Popup_Idle        .Checked := FApp.Priority = tpIdle;
    Popup_Normal      .Checked := FApp.Priority = tpNormal;
    Popup_Higher      .Checked := FApp.Priority = tpHigher;
    Popup_TimeCritical.Checked := FApp.Priority = tpTimeCritical;

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
  
  // BeeApp Events
  
  procedure TTickFrm.OnTerminate(Sender: TObject);
  begin
    if FInterfaces.Properties.Terminated = True then
    begin
      Timer.Enabled       := False;

      BtnCancel.Kind      := bkClose;
      BtnCancel.Caption   := rsBtnCloseCaption;
      BtnPriority.Enabled := False;
      BtnPauseRun.Enabled := False;
      
      if Report.Lines.Count > 0 then
      begin
        ActiveControl := BtnCancel;
        Notebook.ActivePageComponent := ReportPage;

        BtnSave.Enabled := True;
        BtnFont.Enabled := True;
      end else
      begin;
        Close;
      end;
      
      if (FContents.Count > 0) and (FCmdLine.Link <> '') then
      begin
        FContents.SaveToFile(FCmdLine.Link);
      end;
    end;
  end;
















  // 0K
  
  procedure TTickFrm.OnOverwrite;
  var
    F: TOverWriteFrm;
  begin
    if FInterfaces.Properties.Terminated = False then
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
    if FInterfaces.Properties.Terminated = False then
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
    with FInterfaces.OnFatalError do
    begin
      Report.Append(Data.Msg);
    end;
  end;
  
  procedure TTickFrm.OnError;
  begin
    with FInterfaces.OnError do
    begin
      Report.Append(Data.Msg);
    end;
  end;
  
  procedure TTickFrm.OnWarning;
  begin
    with FInterfaces.OnWarning do
    begin
      Report.Append(Data.Msg);
    end;
  end;

  procedure TTickFrm.OnDisplay;
  begin
    with FInterfaces.OnDisplay do
    begin
      if FCmdLine.Log then
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
  begin
    with FInterfaces.OnList.Data do
    begin
      FContents.Add(FileName);               // 01
      FContents.Add(FilePath);               // 02
      FContents.Add(IntToStr(FileSize));     // 03
      FContents.Add(IntTostr(FilePacked));   // 04
      FContents.Add(IntTostr(FileRatio));    // 05
      FContents.Add(IntTostr(FileAttr));     // 06
      FContents.Add(IntTostr(FileTime));     // 07
      FContents.Add(FileComm);               // 08
      FContents.Add(IntTostr(FileCrc));      // 09
      FContents.Add(FileMethod);             // 10
      FContents.Add(FileVersion);            // 11
      FContents.Add(FilePassword);           // 12
      FContents.Add(IntTostr(FilePosition)); // 13
    end;
  end;

  procedure TTickFrm.OnTick;
  begin
    Tick.Position := FInterfaces.OnTick.Data.Percentage;
    Caption := Format(rsProcessStatus, [FInterfaces.OnTick.Data.Percentage]);
    Application.Title := Caption;
  end;
  
  procedure TTickFrm.OnKey;
  var
    F: TPasswordFrm;
  begin
    if FInterfaces.Properties.Terminated = False then
    begin
      if FPassword <> '' then
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
  
initialization

  {$I beegui_tickfrm.lrs}

end.
