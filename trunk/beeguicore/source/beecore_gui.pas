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

    BeeCore Gui class.

  Modifyed:

}

unit BeeCore_Gui;

{$I compiler.inc}

interface

uses
  Forms,
  Dialogs,
  Classes,
  Controls,
  SysUtils,
  // ---
  Bee_App,
  Bee_Common,
  Bee_Interface,
  // ---
  BeeCore_CmdLine,
  // ---
  BeeCore_TickFrm,
  BeeCore_RenameFrm,
  BeeCore_PasswordFrm,
  BeeCore_OverwriteFrm;
  
type

  { TGui Application class }

  TGui = class
  private
    FApp: TBeeApp;
    FAppKey: string;
    FAppInterface: TAppInterface;
    FAppTerminated: boolean;
    FTickFrm: TTickFrm;
    FCmdLine: TCmdLine;
    FSwitch: boolean;
    FTime: integer;
  private
    procedure OnStartTimer(Sender: TObject);
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
  public
    property Switch: boolean read FSwitch;
    property Terminated: boolean read FAppTerminated;
  public
    constructor Create(ATickFrm: TTickFrm; ACmdLine: TCmdLine);
    destructor Destroy; override;
  end;

implementation

  constructor TGui.Create(ATickFrm: TTickFrm; ACmdLine: TCmdLine);
  var
    i: integer;
  begin
    inherited Create;
    // ---
    FTime := 0;
    FSwitch := False;
    FCmdLine := ACmdLine;
    // ---
    FTickFrm := ATickFrm;
    FTickFrm.Timer.OnStartTimer := OnStartTimer;
    FTickFrm.Timer.OnTimer := OnTimer;
    // ---
    FAppKey := '';
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
    FApp := TBeeApp.Create(FAppInterface, FCmdLine.Params);
    FApp.OnTerminate := OnTerminate;
    FAppTerminated := False;
    FApp.Suspended := False;
  end;

  destructor TGui.Destroy;
  begin
    FCmdLine := nil;
    FAppInterface.Free;
    inherited Destroy;
  end;

  procedure TGui.OnStartTimer(Sender: TObject);
  begin
    if FApp.AppInterface.OnTick.Data.GeneralSize < 1024 then
    begin
      FTickFrm.GeneralSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.GeneralSize);
      FTickFrm.GeneralSizeUnit.Caption := 'B';
    end else
      if FApp.AppInterface.OnTick.Data.GeneralSize < 1024 * 1024 then
      begin
        FTickFrm.GeneralSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.GeneralSize div 1024);
        FTickFrm.GeneralSizeUnit.Caption := 'KB';
      end else
        if FApp.AppInterface.OnTick.Data.GeneralSize < 1024 * 1024 * 1024 then
        begin
          FTickFrm.GeneralSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.GeneralSize div (1024 * 1024));
          FTickFrm.GeneralSizeUnit.Caption := 'MB';
        end;
  end;

  procedure TGui.OnTimer(Sender: TObject);
  var
    iSpeed: integer;
    iRemainSize: integer;
  begin
    if FApp.AppInterface.OnTick.Data.ProcessedSize < 1024 then
    begin
      FTickFrm.ProcessedSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.ProcessedSize);
      FTickFrm.ProcessedSizeUnit.Caption := 'B';
    end else
      if FApp.AppInterface.OnTick.Data.ProcessedSize < 1024 * 1024 then
      begin
        FTickFrm.ProcessedSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.ProcessedSize div 1024);
        FTickFrm.ProcessedSizeUnit.Caption := 'KB';
      end else
        if FApp.AppInterface.OnTick.Data.ProcessedSize < 1024 * 1024 * 1024 then
        begin
          FTickFrm.ProcessedSize.Caption := IntToStr(FApp.AppInterface.OnTick.Data.ProcessedSize div (1024 * 1024));
          FTickFrm.ProcessedSizeUnit.Caption := 'MB';
        end;

    Inc(FTime);
    with FApp.AppInterface.OnTick.Data do
    begin
      iSpeed := ProcessedSize div FTime;
      iRemainSize := GeneralSize - ProcessedSize;
    end;
    FTickFrm.Time.Caption := TimeToStr(FTime);
    FTickFrm.Speed.Caption := IntToStr(iSpeed div 1024);
    FTickFrm.RemainingTime.Caption := TimeToStr(iRemainSize div iSpeed);

    FTickFrm.Tick.Position := FAppInterface.OnTick.Data.Percentage;
    FTickFrm.Caption := Format('%d%% Processing...', [FApp.AppInterface.OnTick.Data.Percentage]);
  end;

  procedure TGui.OnTerminate(Sender: TObject);
  begin
    if FAppTerminated = False then
    begin
      FAppTerminated := True;
      FTickFrm.Close;
    end;
  end;

  procedure TGui.OnOverwrite;
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

  procedure TGui.OnRename;
  var
    F: TRenameFrm;
  begin
    if FApp.Suspended = False then
    begin;
      FApp.Suspended := True;
      F := TRenameFrm.Create(nil);
      F.Caption := 'Rename file';
      F.RenameTo.Text :=
        FAppInterface.OnRename.Data.FilePath +
        FAppInterface.OnRename.Data.FileName;

      if F.ShowModal = mrOk then
        FAppInterface.OnRename.Answer := F.RenameTo.Text
      else
        FAppInterface.OnRename.Answer := '';

      F.Free;
      FApp.Suspended := False;
    end;
  end;

  procedure TGui.OnFatalError;
  begin
    if Assigned(FCmdLine.LogPtr) then
      TStringList(FCmdLine.LogPtr).Add(FAppInterface.OnFatalError.Data.Msg);
  end;

  procedure TGui.OnError;
  begin
    if Assigned(FCmdLine.LogPtr) then
      TStringList(FCmdLine.LogPtr).Add(FAppInterface.OnError.Data.Msg);
  end;

  procedure TGui.OnWarning;
  begin
    if Assigned(FCmdLine.LogPtr) then
      TStringList(FCmdLine.LogPtr).Add(FAppInterface.OnWarning.Data.Msg);
  end;

  procedure TGui.OnDisplay;
  begin
    if FCmdLine.Log then
    begin
      TStringList(FCmdLine.LogPtr).Add(FAppInterface.OnDisplay.Data.Msg);
    end;
    FTickFrm.Msg.Caption := FAppInterface.OnDisplay.Data.Msg;
  end;

  procedure TGui.OnRequest;
  begin
    if MessageDlg(FAppInterface.OnRequest.Data.Msg,
      mtConfirmation, [mbYes], 0) = mrYes then
    begin
    
    end;
  end;

  procedure TGui.OnClear;
  begin
  end;

  procedure TGui.OnList;
  begin
    if Assigned(FCmdLine.ListPtr) then
    begin
    
    end;
  end;

  procedure TGui.OnKey;
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

  procedure TGui.OnSwitch;
  begin
    if FAppInterface.OnTick.Data.GeneralSize > $FFFF then
    begin
      FAppInterface.OnTick.Method := OnTick;
      FTickFrm.Timer.Enabled := True;
      FTickFrm.Thread := FApp;
      FSwitch := True;
    end;
  end;

  procedure TGui.OnTick;
  begin
    Application.Title := FTickFrm.Caption;
    // ---
    FTickFrm.Tick.Position := FAppInterface.OnTick.Data.Percentage;
    FTickFrm.Caption := Format('%d%% Processing...', [FApp.AppInterface.OnTick.Data.Percentage]);
  end;

end.

