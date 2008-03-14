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
    SaveDialog: TSaveDialog;
    Timer: TIdleTimer;
    // ---
    Notebook: TNotebook;
    GeneralPage: TPage;
    TimeLabel: TLabel;
    Time: TLabel;
    RemainingTimeLabel: TLabel;
    RemainingTime: TLabel;
    GeneralSizeLabel: TLabel;
    GeneralSize: TLabel;
    GeneralSizeUnit: TLabel;
    ProcessedSizeLabel: TLabel;
    ProcessedSize: TLabel;
    ProcessedSizeUnit: TLabel;
    SpeedLabel: TLabel;
    Speed: TLabel;
    SpeedUnit: TLabel;
    Msg: TLabel;
    Tick: TProgressBar;
    ReportPage: TPage;
    Report: TMemo;
    // ---
    BtnSave: TBitBtn;
    BtnFont: TBitBtn;
    BtnBackForeGround: TBitBtn;
    BtnPauseRun: TBitBtn;
    BtnCancel: TBitBtn;
    // ---
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure HandleClick(Sender: TObject);
    // ---
    procedure BtnFontClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnBackForeGroundClick(Sender: TObject);
    procedure BtnPauseRunClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    // ---
    procedure OnStartTimer(Sender: TObject);
    procedure OnStopTimer(Sender: TObject);
    procedure OnTimer(Sender: TObject);
  private
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
    procedure OnSwitch;
    procedure OnTick;
    procedure OnKey;
  private
    { private declarations }
    FApp: TBeeApp;
    FAppKey: string;
    FAppContents: TStringList;
    FAppInterface: TAppInterface;
    FAppTerminated: boolean;
    FTime: integer;
    FSwitch: boolean;
    FSwitchValue: integer;
    FCmdLine: TCmdLine;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Execute(ACmdLine: TCmdLine);
    destructor Destroy; override;
  public
    property Switch: boolean read FSwitch;
    property Terminated: boolean read FAppTerminated;
  end;
  
var
  TickFrm: TTickFrm;
  
implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  BeeGui_SysUtils,
  BeeGui_Messages;
  
var
  rsBtnForeGroundCaption : string = 'Foreground';
  rsBtnBackGroundCaption : string = 'Background';
  rsBtnPauseCaption : string = 'Pause';
  rsBtnRunCaption : string = 'Run';
  
  { TTickFrm class }
  
  constructor TTickFrm.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    // ---
    FAppKey := '';
    FAppContents := TStringList.Create;
    FAppTerminated := False;
    // ---
    FAppInterface := TAppInterface.Create;
    FAppInterface.OnFatalError.Method := OnFatalError;
    FAppInterface.OnOverWrite.Method := OnOverWrite;
    FAppInterface.OnWarning.Method := OnWarning;
    FAppInterface.OnDisplay.Method := OnDisplay;
    FAppInterface.OnRequest.Method := OnRequest;
    FAppInterface.OnRename.Method := OnRename;
    FAppInterface.OnClear.Method := OnClear;
    FAppInterface.OnError.Method := OnError;
    FAppInterface.OnList.Method := OnList;
    FAppInterface.OnTick.Method := OnSwitch;
    FAppInterface.OnKey.Method := OnKey;
    // ---
    FTime := 0;
    FSwitch := False;
    FSwitchValue := $FFFF;
    // ---
    FCmdLine := nil;
  end;
  
  destructor TTickFrm.Destroy;
  begin
    FAppKey := '';
    FAppContents.Free;
    FAppInterface.Free;
    FCmdLine := nil;
    inherited Destroy;
  end;
  
  procedure TTickFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_tickfrm_loadlanguage.inc}
    {$I beegui_tickfrm_loadproperty.inc}
    //{$ifdef Windows}
    //TrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1));
    //{$endif}
    //TrayIcon.Hint := 'BeeCore';
    //TrayIcon.OnClick := HandleClick;
    //TrayIcon.PopUpMenu := PopupMenu;
    // --
    ActiveControl := BtnCancel;
    Notebook.ActivePageComponent := GeneralPage;
    // ---
    BtnBackForeGround.Caption := rsBtnBackGroundCaption;
    BtnPauseRun.Caption := rsBtnPauseCaption;
  end;

  procedure TTickFrm.FormWindowStateChange(Sender: TObject);
  begin
    if WindowState = wsNormal then
    begin
      //TrayIcon.Visible := False;
    end else
    begin
      //TrayIcon.Visible := True;
    end;
  end;
  
  procedure TTickFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
  begin
    if FAppTerminated = False then
    begin
      CanClose := MessageDlg(rsConfirm, rsConfirmAbort,
        mtConfirmation, [mbYes, mbNo], '') = mrYes;
      
      if CanClose and (FAppTerminated = False) then
      begin
        Timer.Enabled := True;
        if FApp.Suspended then
        begin
          FApp.Resume;
        end;
        FApp.Terminate;
      end;
    end else
      CanClose := True;
  end;
  
  procedure TTickFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {$I beegui_tickfrm_savelanguage.inc}
    {$I beegui_tickfrm_saveproperty.inc}
  end;

  procedure TTickFrm.HandleClick(Sender: TObject);
  begin
    WindowState := wsNormal;
  end;
  
  procedure TTickFrm.Execute(ACmdLine: TCmdLine);
  begin
    FCmdLine := ACmdLine;
    if FCmdLine.Log then
    begin
      FSwitchValue := -1;
    end;
    // ---
    FApp := TBeeApp.Create(FAppInterface, FCmdLine.Params);
    FApp.OnTerminate := OnTerminate;
    FApp.Suspended := False;
  end;
  
  // Timer Events
  
  procedure TTickFrm.OnStartTimer(Sender: TObject);
  begin
    if FAppInterface.OnTick.Data.GeneralSize < 1024 then
    begin
      GeneralSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.GeneralSize);
      GeneralSizeUnit.Caption := 'B';
    end else
      if FAppInterface.OnTick.Data.GeneralSize < 1024 * 1024 then
      begin
        GeneralSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.GeneralSize div 1024);
        GeneralSizeUnit.Caption := 'KB';
      end else
        if FAppInterface.OnTick.Data.GeneralSize < 1024 * 1024 * 1024 then
        begin
          GeneralSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.GeneralSize div (1024 * 1024));
          GeneralSizeUnit.Caption := 'MB';
        end;
  end;

  procedure TTickFrm.OnTimer(Sender: TObject);
  var
    iSpeed: integer;
    iRemainSize: integer;
  begin
    if FAppInterface.OnTick.Data.ProcessedSize < 1024 then
    begin
      ProcessedSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.ProcessedSize);
      ProcessedSizeUnit.Caption := 'B';
    end else
      if FAppInterface.OnTick.Data.ProcessedSize < 1024 * 1024 then
      begin
        ProcessedSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.ProcessedSize div 1024);
        ProcessedSizeUnit.Caption := 'KB';
      end else
        if FAppInterface.OnTick.Data.ProcessedSize < 1024 * 1024 * 1024 then
        begin
          ProcessedSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.ProcessedSize div (1024 * 1024));
          ProcessedSizeUnit.Caption := 'MB';
        end;

    Inc(FTime);
    with FApp.AppInterface.OnTick.Data do
    begin
      iSpeed := ProcessedSize div FTime;
      iRemainSize := GeneralSize - ProcessedSize;
    end;
    Time.Caption := TimeToStr(FTime);
    Speed.Caption := IntToStr(iSpeed div 1024);
    RemainingTime.Caption := TimeToStr(iRemainSize div iSpeed);
  end;
  
  procedure TTickFrm.OnStopTimer(Sender: TObject);
  var
    F: TViewFrm;
    I: integer;
  begin
    if FAppTerminated then
    begin
      ProcessedSize.Caption := GeneralSize.Caption;
      ProcessedSizeUnit.Caption := GeneralSizeUnit.Caption;

      Time.Caption := TimeToStr(FTime);
      Speed.Caption := IntToStr(0);
      RemainingTime.Caption := TimeToStr(0);

      Tick.Position := 100;
      Caption := rsProcessTerminated;
      Application.Title := Caption;
    end;
  end;

  // Buttons Click procedure

  procedure TTickFrm.BtnPauseRunClick(Sender: TObject);
  begin
    if FAppTerminated = False then
    begin
      if FApp.Suspended then
      begin
        Timer.Enabled  := True;
        FApp.Suspended := False;
        BtnPauseRun.Caption := rsBtnPauseCaption;
      end else
      begin
        Timer.Enabled  := False;
        FApp.Suspended := True;
        BtnPauseRun.Caption := rsBtnRunCaption
      end
    end;
  end;

  procedure TTickFrm.BtnBackForeGroundClick(Sender: TObject);
  begin
    if FAppTerminated = False then
    begin
      if FApp.Priority = tpIdle then
      begin
        FApp.Priority := tpNormal;
        BtnBackForeGround.Caption := rsBtnBackGroundCaption;
      end else
      begin
        FApp.Priority := tpIdle;
        BtnBackForeGround.Caption := rsBtnForeGroundCaption;
      end;
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
    SaveDialog.Filter := 'Txt file  (*.txt)|*.txt|' +
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
  var
    F: TViewFrm;
  begin
    if FAppTerminated = False then
    begin
      FAppTerminated := True;
      Timer.Enabled := False;

      BtnBackForeGround.Enabled := False;
      BtnPauseRun.Enabled := False;
      BtnCancel.Kind := bkClose;

      if Report.Lines.Count > 0 then
      begin
        BtnSave.Enabled := True;
        BtnFont.Enabled := True;

        Notebook.ActivePageComponent := ReportPage;
      end else
      begin;
        Close;
      end;
      
      if (FAppContents.Count > 0) and (FCmdLine.Link <> '') then
      begin
        FAppContents.SaveToFile(FCmdLine.Link);
      end;
    end;
  end;

  procedure TTickFrm.OnOverwrite;
  var
    F: TOverWriteFrm;
  begin
    if FApp.Suspended = False then
    begin;
      FApp.Suspended := True;
      F := TOverWriteFrm.Create(nil);
      with FAppInterface.OnOverWrite.Data do
      begin
        F.TheFolder.Caption := F.TheFolder.Caption + ' "' + FileName + '".';
        F.NewSize  .Caption := F.NewSize  .Caption + '  ' + SizeToStr(FileSize);
        F.NewDate  .Caption := F.NewDate  .Caption + '  ' + DateTimeToStr(FileDateToDateTime(FileTime));

        F.OldSize  .Caption := F.OldSize  .Caption + '  ' + SizeToStr(SizeOfFile(FileName));
        F.OldDate  .Caption := F.OldDate  .Caption + '  ' + DateTimeToStr(FileDateToDateTime(FileAge(FileName)));
      end;
      case F.ShowModal of
        mrAbort   : FAppInterface.OnOverWrite.Answer := 'Q';
        mrNoToAll : FAppInterface.OnOverWrite.Answer := 'S';
        mrYesToAll: FAppInterface.OnOverWrite.Answer := 'A';
        mrNo      : FAppInterface.OnOverWrite.Answer := 'N';
        mrYes     : FAppInterface.OnOverWrite.Answer := 'Y';
      end;
      F.Free;
      FApp.Suspended := False;
    end;
  end;

  procedure TTickFrm.OnRename;
  var
    F: TRenameFrm;
  begin
    if FApp.Suspended = False then
    begin;
      FApp.Suspended := True;
      F := TRenameFrm.Create(nil);
      F.Caption := rsRenameFile;
      with FAppInterface.OnRename.Data do
      begin
        F.ToFN.Text := FilePath + FileName;
      end;
      
      if F.ShowModal = mrOk then
        FAppInterface.OnRename.Answer := F.ToFN.Text
      else
        FAppInterface.OnRename.Answer := '';

      F.Free;
      FApp.Suspended := False;
    end;
  end;

  procedure TTickFrm.OnFatalError;
  begin
    FSwitchValue := -1;
    Report.Append(FAppInterface.OnFatalError.Data.Msg);
  end;

  procedure TTickFrm.OnError;
  begin
    FSwitchValue := -1;
    Report.Append(FAppInterface.OnError.Data.Msg);
  end;

  procedure TTickFrm.OnWarning;
  begin
    FSwitchValue := -1;
    Report.Append(FAppInterface.OnWarning.Data.Msg);
  end;

  procedure TTickFrm.OnDisplay;
  begin
    if FCmdLine.Log then
    begin
      Report.Append(FAppInterface.OnDisplay.Data.Msg);
    end;
    Msg.Caption := FAppInterface.OnDisplay.Data.Msg;
  end;

  procedure TTickFrm.OnRequest;
  begin
    if MessageDlg(FAppInterface.OnRequest.Data.Msg,
      mtConfirmation, [mbYes], 0) = mrYes then
    begin

    end;
  end;

  procedure TTickFrm.OnClear;
  begin
    // nothing to do!
  end;

  procedure TTickFrm.OnList;
  begin
    with FAppInterface.OnList.Data do
    begin
      FAppContents.Add(FileName);               // 01
      FAppContents.Add(FilePath);               // 02
      FAppContents.Add(IntToStr(FileSize));     // 03
      FAppContents.Add(IntTostr(FilePacked));   // 04
      FAppContents.Add(IntTostr(FileRatio));    // 05
      FAppContents.Add(IntTostr(FileAttr));     // 06
      FAppContents.Add(IntTostr(FileTime));     // 07
      FAppContents.Add(FileComm);               // 08
      FAppContents.Add(IntTostr(FileCrc));      // 09
      FAppContents.Add(FileMethod);             // 10
      FAppContents.Add(FileVersion);            // 11
      FAppContents.Add(FilePassword);           // 12
      FAppContents.Add(IntTostr(FilePosition)); // 13
    end;
  end;

  procedure TTickFrm.OnSwitch;
  begin
    if FAppInterface.OnTick.Data.GeneralSize > FSwitchValue then
    begin
      FAppInterface.OnTick.Method := OnTick;
      Timer.Enabled := True;
      FSwitch := True;
    end;
    OnTick;
  end;

  procedure TTickFrm.OnTick;
  begin
    Tick.Position := FAppInterface.OnTick.Data.Percentage;
    Caption := Format(rsProcessStatus, [FApp.AppInterface.OnTick.Data.Percentage]);
    Application.Title := Caption;
  end;
  
  procedure TTickFrm.OnKey;
  var
    F: TPasswordFrm;
  begin
    if FApp.Suspended = False then
    begin
      FApp.Suspended := True;
      if Length(FAppKey) = 0 then
      begin
        F := TPasswordFrm.Create(Application);
        F.SetKey(FAppKey);
        if F.ShowModal = mrOK then
        begin
          FAppKey := F.Key.Text;
        end;
        F.Free;
      end;
      FAppInterface.OnKey.Answer := FAppKey;
      FApp.Suspended := False;
    end;
  end;
  
initialization

  {$I beegui_tickfrm.lrs}

end.
