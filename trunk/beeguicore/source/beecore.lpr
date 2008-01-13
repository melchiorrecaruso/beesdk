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
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Classes,
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

  TCore = class (TThread)
  private
    App: TBeeApp;
    AppKey: string;
    AppLog: TStringList;
    AppInterface: TAppInterface;
    AppParams: TStringList;
    AppTerminated: boolean;
    AppShowTick: boolean;
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
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;
  
  // implementation

  constructor TCore.Create;
  var
    i: integer;
  begin
    inherited Create(True);
    
    AppLog := TStringList.Create;

    AppInterface := TAppInterface.Create;
    AppInterface.OnFatalError.Method := OnFatalError;
    AppInterface.OnOverWrite.Method := OnOverWrite;
    AppInterface.OnWarning.Method := OnWarning;
    AppInterface.OnDisplay.Method := OnDisplay;
    AppInterface.OnRequest.Method := OnRequest;
    AppInterface.OnRename.Method := OnRename;
    AppInterface.OnClear.Method := OnClear;
    AppInterface.OnError.Method := OnError;
    AppInterface.OnList.Method := OnList;
    AppInterface.OnTick.Method := OnTick;
    AppInterface.OnKey.Method := OnKey;

    SetLength(AppKey, 0);
    AppParams := TStringList.Create;
    for i := 1 to ParamCount do
    begin
      AppParams.Add(ParamStr(i));
    end;
    App := TBeeApp.Create(AppInterface, AppParams);
    AppTerminated := False;
    AppShowTick := False;
  end;
  
  destructor TCore.Destroy;
  begin
    AppKey := '';
    // ---
    AppLog.Free;
    AppParams.Free;
    AppInterface.Free;
    inherited Destroy;
  end;
  
  procedure TCore.Execute;
  begin
    App.Execute;
  end;

  procedure TCore.DoTerminate;
  begin
    if TickFrm.BtnCancel.ModalResult = mrCancel then
    begin
      TickFrm.BtnCancel.ModalResult := mrOk;
      TickFrm.BtnCancel.Click;
      
      TickFrm.Visible := False;
    end;
    inherited DoTerminate;
  end;

  procedure TCore.OnFatalError;
  begin
    AppLog.Add(AppInterface.OnFatalError.Data.Msg);
  end;
  
  procedure TCore.OnOverwrite;
  var
    F: TOverWriteFrm;
  begin
    if App.Suspended = False then
    begin;
      App.Suspended := True;
      // ---
      F := TOverWriteFrm.Create(nil);
      with AppInterface.OnOverWrite.Data do
      begin
        F.TheFolder.Caption := F.TheFolder.Caption + ' "' + FileName + '".';
        F.NewSize  .Caption := F.NewSize  .Caption + '  ' + SizeToStr(FileSize);
        F.NewDate  .Caption := F.NewDate  .Caption + '  ' + DateTimeToStr(FileDateToDateTime(FileTime));

        F.OldSize  .Caption := F.OldSize  .Caption + '  ' + SizeToStr(SizeOfFile(FileName));
        F.OldDate  .Caption := F.OldDate  .Caption + '  ' + DateTimeToStr(FileDateToDateTime(FileAge(FileName)));
      end;
      case F.ShowModal of
        mrAbort   : AppInterface.OnOverWrite.Answer := 'Q';
        mrNoToAll : AppInterface.OnOverWrite.Answer := 'S';
        mrYesToAll: AppInterface.OnOverWrite.Answer := 'A';
        mrNo      : AppInterface.OnOverWrite.Answer := 'N';
        mrYes     : AppInterface.OnOverWrite.Answer := 'Y';
      end;
      F.Free;
      // ---
      App.Suspended := False;
    end;
  end;
  
  procedure TCore.OnWarning;
  begin
    AppLog.Add(AppInterface.OnWarning.Data.Msg);
  end;
  
  procedure TCore.OnDisplay;
  begin
    TickFrm.Msg.Caption := AppInterface.OnDisplay.Data.Msg;
  end;
  
  procedure TCore.OnRequest;
  begin
    AppLog.Add(AppInterface.OnRequest.Data.Msg);
  end;
  
  procedure TCore.OnRename;
  var
    F: TRenameFrm;
  begin
    if App.Suspended = False then
    begin;
      App.Suspended := True;
      F := TRenameFrm.Create(nil);
      F.Caption := 'Rename file';
      F.RenameTo.Text :=
        AppInterface.OnRename.Data.FilePath +
        AppInterface.OnRename.Data.FileName;
        
      if F.ShowModal = mrOk then
        AppInterface.OnRename.Answer := F.RenameTo.Text
      else
        AppInterface.OnRename.Answer := '';

      F.Free;
      App.Suspended := False;
    end;
  end;
  
  procedure TCore.OnError;
  begin
    AppLog.Add(AppInterface.OnError.Data.Msg);
  end;
  
  procedure TCore.OnClear;
  begin
    // nothing to do
  end;
  
  procedure TCore.OnList;
  begin

  end;
  
  procedure TCore.OnTick;
  begin
    TickFrm.Tick.Position := AppInterface.OnTick.Data.Percentage;
    if AppInterface.OnTick.Data.Bytes > 1024 then
    begin
      AppShowTick := True;
    end;
  end;
  
  procedure TCore.OnKey;
  begin
  
  end;

  // -- implemenattion -- //

var
  Core: TCore;


begin
  Application.Initialize;
  // ---
  TickFrm := TTickFrm.Create(Application);
  // ---
  Core := TCore.Create;
  Core.Resume;

  repeat
    if Core.AppTerminated then
      Break
    else
      Application.ProcessMessages;
  until Core.AppShowTick;
  
  if Core.AppShowTick then
    case TickFrm.ShowModal of
      mrCancel: begin
                  ShowMessage('Cancel');
                end;
      mrOk:     begin
                  ShowMessage('Ok');
                end;
    end;
  Core.Free;
end.

