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
    FTickBool: boolean;
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
    procedure On1Tick;
    procedure On2Tick;
    procedure OnKey;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  
  // implementation

  constructor TGui.Create;
  var
    i: integer;
  begin
    inherited Create;

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
    FAppInterface.OnTick.Method := On1Tick;
    FAppInterface.OnKey.Method := OnKey;

    FAppKey:= '';
    FAppLog := TStringList.Create;
    FAppParams := TStringList.Create;
    for i := 1 to ParamCount do
    begin
      FAppParams.Add(ParamStr(i));
    end;

    FApp := TBeeApp.Create(FAppInterface, FAppParams);
    FApp.OnTerminate := OnTerminate;
    FAppTerminated := False;
    // ---
    FTickFrm := TTickFrm.Create(Application, FApp);
    FTickBool := False;
    // ---
    FApp.Resume;
  end;
  
  destructor TGui.Destroy;
  begin
    FAppKey := '';

    FAppLog.Free;
    FAppParams.Free;
    FAppInterface.Free;
    inherited Destroy;
  end;

  procedure TGui.OnTerminate(Sender: TObject);
  begin
    if FAppTerminated = False then
    begin
      FAppTerminated := True;
      if FTickFrm.BtnCancel.ModalResult = mrCancel then
      begin
        FTickFrm.BtnCancel.ModalResult := mrOk;
        FTickFrm.BtnCancel.Click;
      end;
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
  
  procedure TGui.On1Tick;
  begin
    if FAppInterface.OnTick.Data.Bytes > $FFFF then
    begin
      FAppInterface.OnTick.Method := On2Tick;
      FTickBool := True;
    end;
  end;
  
  procedure TGui.On2Tick;
  begin
    FTickFrm.Tick.Position := FAppInterface.OnTick.Data.Percentage;
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
  // ---
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
  // ---

  Application.Initialize;
  if Command = '?' then
  begin
     AboutFrm := TAboutFrm.Create(Application);
     AboutFrm.ShowModal;
  end else
  begin
    Gui := TGui.Create;
    repeat
      if Gui.FAppTerminated then
        Break
      else
        Application.ProcessMessages;
    until Gui.FTickBool;
  
    if Gui.FTickBool then
    begin
      if Gui.FTickFrm.ShowModal = mrCancel then
      begin
        Gui.FApp.Terminate;
      end;
    end;
    Gui.Free;
  end;
end.

