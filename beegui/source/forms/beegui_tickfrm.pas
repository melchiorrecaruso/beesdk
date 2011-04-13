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
  Bee_Consts,
  // ---
  BeeGui_RenameFrm,
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
    MainMenu1: TMainMenu;
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
    Speed:   TLabel;
    SpeedUnit: TLabel;
    TimeLabelPanel: TPanel;
    Popup_Idle: TMenuItem;
    Popup_TimeCritical: TMenuItem;
    Popup_Higher: TMenuItem;
    Popup_Normal: TMenuItem;
    Popup:   TPopupMenu;
    RemainingTimeLabel: TLabel;
    TimeLabel: TLabel;
    SaveDialog: TSaveDialog;
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
    procedure OnExecute;
    procedure OnRename;
    procedure OnList;
  private
    { private declarations }
    FID:        Pointer;
    FList:      TList;
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
  {$IFDEF USEDLL}
  BeeLib_Link,
  {$ELSE}
  Bee_Library,
  {$ENDIF}
  BeeGui_Messages,
  BeeGui_SysUtils,

  BeeFm_ViewFrm;

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
  CanClose := (CoreGetStatus(FID) = csTerminated) or (CoreGetStatus(FID) = -1);

  if CanClose = False then
  begin
    I := FSuspended;
    if I = False then BtnPauseRun.Click;

    if MessageDlg(rsConfirmation, rsConfirmAbortProcess, mtConfirmation, [mbYes, mbNo], '') = mrYes then
    begin
      CoreTerminate(FID);
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
  P := StringToPChar(FCommandLine.CommandLine);
  FID := CoreCreate(P);
  if CoreExecute(FID) then
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
  Result := CoreGetRemainingTime(FID) > 0;
end;

procedure TTickFrm.PopupClick(Sender: TObject);
begin
  Popup_Idle.Checked         := Sender = Popup_Idle;
  Popup_Normal.Checked       := Sender = Popup_Normal;
  Popup_Higher.Checked       := Sender = Popup_Higher;
  Popup_TimeCritical.Checked := Sender = Popup_TimeCritical;

  if CoreGetStatus(FID) <> csTerminated then
  begin
    if Popup_Idle.Checked         then CoreSetPriority(FID, Ord(tpIdle));
    if Popup_Normal.Checked       then CoreSetPriority(FID, Ord(tpNormal));
    if Popup_Higher.Checked       then CoreSetPriority(FID, Ord(tpHigher));
    if Popup_TimeCritical.Checked then CoreSetPriority(FID, Ord(tpTimeCritical));
  end;
end;

procedure TTickFrm.NotebookPageChanged(Sender: TObject);
var
  I: integer;
begin
  //for I := 0 to Notebook.PageCount -1 do
  //begin
  //  Notebook.Page[I].Visible := False;
  //end;
  //Notebook.Page[Notebook.PageIndex].Visible := True;

  if Notebook.PageIndex = 0 then
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
  Timer.Enabled := FALSE;

   ShowMessage('DEBUG 1');

  case CoreGetStatus(FID) of
    csTerminated: OnTerminate;
    csExecuting:  OnExecute;
    csWaitRename: OnRename;
    // csWaitOverwrite:
    // csWaitPassword:
    // csWaitRequest:
    // csWaitList:
  end;
  Timer.Enabled := CoreGetStatus(FID) <> -1;
end;

procedure TTickFrm.OnExecute;
var
  I: int64;
  P: PChar;
begin
  I := CoreGetSize(FID);
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

  I := CoreGetProcessedSize(FID);
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

  Tick.Position := CoreGetPercentage(FID);
  if FSuspended = False then
    Caption := Format(rsProcessStatus, [Tick.Position])
  else
    Caption := rsProcessPaused;

  if FProgressOnTitle then
  begin
    Application.Title := Caption;
  end;

  ShowMessage('DEBUG 1');

  Time.Caption   := TimeToStr(CoreGetElapsedTime(FID));
  RemainingTime.Caption := TimeToStr(CoreGetRemainingTime(FID));
  Speed.Caption         := IntToStr (CoreGetSpeed(FID) shr 10);

  P := CoreGetMessage(FID);
  if P <> nil then
  begin
    Msg.Caption := PCharToString(P);
  end;
end;

procedure TTickFrm.OnTerminate;
var
  P: PChar;
  I: integer;
begin
  ExitCode := CoreGetCode(FID);
  case ExitCode of
    ccSuccesful: Caption := rsProcessTerminated;
    ccWarning:   Caption := rsProcessTerminated;
    ccError:     Caption := rsProcessAborted;
    else         Caption := rsProcessAborted;
  end;

  if FProgressOnTitle then
  begin
    Application.Title := Caption;
  end;


  ShowMessage('LOG');

  Report.Lines.Clear;
  if FCommandLine.Log or (ExitCode > 0) then
    for I := 0 to CoreGetItemCount(FID) - 1 do
    begin
      P := CoreGetMessages(FID, I);
      if P <> nil then
      begin
        Report.Lines.Add(PCharToString(P));
      end;
    end;

  ShowMessage('LIST');

  OnList;



  FCanClose := CoreDestroy(FID);
  FID := nil;

  if Report.Lines.Count > 0 then
  begin
    Notebook.PageIndex := 1;
    NotebookPageChanged(Notebook);
  end else
    Close;
end;

procedure TTickFrm.OnRename;
var
  F: TRenameFrm;
  FI: PFileInfo;
  P: PChar;
begin
  FI := CoreGetItemPending(FID, csWaitRename);
  if FI <> nil then
  begin
    F                := TRenameFrm.Create(Application);
    F.Caption        := rsRenameFile;
    F.ToFN.Text      := PCharToString(FI^.Name);
    F.FromFN.Caption := PCharToString(FI^.Name);

    case F.ShowModal of
      mrAbort: CoreTerminate(FID);
      mrOk: begin
        P := StringToPChar(F.ToFN.Text);
        CoreSetItemPending(FID, csWaitRename, P);
        FreePChar(P);
      end;
    end;
    F.Free;
    FI := nil;
  end;
end;

procedure TTickFrm.OnList;
var
  I: integer;
  P: PFileInfo;
  Node: TArchiveItem;
begin
  if Assigned(FList) then
  begin
    for I := 0 to  CoreGetItemCount(FID) - 1 do
    begin
      P := CoreGetItems(FID, I);
      if P <> nil then
      begin
        Node := TArchiveItem.Create;
        try
          Node.FileName     := ExtractFileName(PCharToString(P^.Name));
          Node.FilePath     := ExtractFilePath(PCharToString(P^.Name));
          Node.FileSize     := P^.Size;
          Node.FilePacked   := P^.PackedSize;

         if Node.FileSize > 0 then
           Node.FileRatio   := Trunc(100 * (Node.FilePacked / Node.FileSize ))
         else
           Node.FileRatio   := 0;

          Node.FileAttr     := P^.Attr;
          Node.FileTime     := P^.Time;
          Node.FileComm     := PCharToString(P^.Comm);
          Node.FileCrc      := P^.Crc;
          Node.FileMethod   := PCharToString(P^.Method);
          Node.FileVersion  := PCharToString(P^.Version);
          Node.FilePassword := PCharToString(P^.Password);
          Node.FilePosition := P^.Position;
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
  if CoreGetStatus(FID) <> csTerminated then
  begin
    FSuspended := not FSuspended;
    if FSuspended then
    begin
      BtnPauseRun.Caption := rsBtnRunCaption;
      CoreSuspend(FID, TRUE);
    end else
    begin
      BtnPauseRun.Caption := rsBtnPauseCaption;
      CoreSuspend(FID, FALSE);
    end;
  end;
end;

procedure TTickFrm.BtnPriorityClick(Sender: TObject);
var
  X, Y: longint;
  FValue: longint;
begin
  FValue := CoreGetPriority(FID);
  if FValue <> -1 then
  begin
    Popup_Idle.Checked         := FValue = Ord(tpIdle);
    Popup_Normal.Checked       := FValue = Ord(tpNormal);
    Popup_Higher.Checked       := FValue = Ord(tpHigher);
    Popup_TimeCritical.Checked := FValue = Ord(tpTimeCritical);

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

  *)

initialization

  {$I beegui_tickfrm.lrs}

end.
