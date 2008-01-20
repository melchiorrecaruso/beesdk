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

    BeeCore application.

  Modifyed:

}

program BeeCore;

{$R beecore.ico.res}

uses
  {$IFDEF UNIX}
  cThreads,
  {$ENDIF}
  Interfaces,

  SysUtils,
  Controls,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Classes,
  Dialogs,
  Forms,
  // ---
  Bee_App,
  Bee_Common,
  Bee_Interface,
  // ---
  BeeCore_AddFrm,
  BeeCore_TickFrm,
  BeeCore_ViewFrm,
  BeeCore_AboutFrm,
  BeeCore_RenameFrm,
  BeeCore_ExtractFrm,
  BeeCore_PasswordFrm,
  BeeCore_OverwriteFrm;

type

  { TGui Application class }

  TGui = class
  private
    FApp: TBeeApp;
    FAppKey: string;
    FAppLog: TStringList;
    FAppInterface: TAppInterface;
    FAppParams: TStringList;
    FAppTerminated: boolean;
    FTickFrm: TTickFrm;
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
  public
    constructor Create(ATickFrm: TTickFrm);
    destructor Destroy; override;
  end;
  
  // implementation

  constructor TGui.Create(ATickFrm: TTickFrm);
  var
    i: integer;
  begin
    inherited Create;
    FTickFrm := ATickFrm;
    FTickFrm.Timer.OnStartTimer := OnStartTimer;
    FTickFrm.Timer.OnTimer := OnTimer;

    FSwitch := False;
    FTime := 0;

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

    FAppKey := '';
    FAppLog := TStringList.Create;
    FAppParams := TStringList.Create;
    for i := 1 to ParamCount do
    begin
      FAppParams.Add(ParamStr(i));
    end;
    FApp := TBeeApp.Create(FAppInterface, FAppParams);
    FApp.OnTerminate := OnTerminate;
    FAppTerminated := False;
    FApp.Resume;
  end;
  
  destructor TGui.Destroy;
  begin
    FAppLog.Free;
    FAppParams.Free;
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

  procedure TGui.OnFatalError;
  begin
    FAppLog.Add(FAppInterface.OnFatalError.Data.Msg);
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
  
  procedure TGui.OnWarning;
  begin
    FAppLog.Add(FAppInterface.OnWarning.Data.Msg);
  end;
  
  procedure TGui.OnDisplay;
  begin
    FTickFrm.Msg.Caption := FAppInterface.OnDisplay.Data.Msg;
  end;
  
  procedure TGui.OnRequest;
  begin
    FAppLog.Add(FAppInterface.OnRequest.Data.Msg);
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
  
  procedure TGui.OnError;
  begin
    FAppLog.Add(FAppInterface.OnError.Data.Msg);
  end;
  
  procedure TGui.OnClear;
  begin
    // nothing to do
  end;
  
  procedure TGui.OnList;
  begin

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
    FTickFrm.Tick.Position := FAppInterface.OnTick.Data.Percentage;
    FTickFrm.Caption := Format('%d%% Processing...', [FApp.AppInterface.OnTick.Data.Percentage]);
    // ---
    Application.Title := FTickFrm.Caption;
  end;
  
  procedure TGui.OnKey;
  begin
  
  end;

  // -- implemenattion -- //

var
  Gui: TGui;
  S: string;
  k: integer;

  Command: char;
  ArcName: string;
  Options: TStringList;
  FileMasks: TStringList;
  
begin
  // Process command line
  Command := ' ';
  ArcName := '';
  Options := TStringList.Create;
  FileMasks := TStringList.Create;
  for k := 1 to ParamCount do
  begin
    S := ParamStr(k);
    if (Length(S) > 1) and (S[1] = '-') then
    begin
      Options.Add(S);
    end else
    begin
      if Command = ' ' then
      begin
        if Length(S) = 1 then
          Command := UpCase(S[1])
        else
          Command := '?';
      end else
        if ArcName = '' then
        begin
          ArcName := S;
          if ExtractFileExt(ArcName) = '' then
          begin
            ArcName := ChangeFileExt(ArcName, '.bee');
          end;
        end else
          FileMasks.Add(Bee_Common.DoDirSeparators(S));
    end;
  end;
  FileMasks.Free;
  Options.Free;

  // Application run
  Application.Initialize;
  if Command = '?' then
  begin
     Application.CreateForm(TAboutFrm, AboutFrm);
     Application.Run;
  end else
  begin
    Application.CreateForm(TTickFrm, TickFrm);
    Gui := TGui.Create(TickFrm);
    repeat
      if Gui.FAppTerminated then
        Break
      else
        Application.ProcessMessages;
    until Gui.Switch;
  
    if Gui.Switch then
    begin
      Application.Run;
    end;
    Gui.Free;
  end;
end.

