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

{$R beegui.ico.res}

uses
  {$IFDEF UNIX}
  cThreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Interfaces,
  SysUtils,
  Controls,
  Classes,
  Forms,
  
  Dialogs,
  
  // Bee project units
  Bee_App,
  Bee_Common,
  Bee_Interface,
  // BeeCore project units
  BeeCore_AddFrm,
  BeeCore_TickFrm,
  BeeCore_ViewFrm,
  BeeCore_AboutFrm,
  BeeCore_RenameFrm,
  BeeCore_ExtractFrm,
  BeeCore_PasswordFrm,
  BeeCore_OverwriteFrm;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  TCommandLine Application class                                        //
  //                                                                        //
  // ---------------------------------------------------------------------- //

type

  { TCommandLine Application class }

  TCmdLine = class
  private
    FCommand: char;
    FrOption: boolean;
    FuOption: boolean;
    FfOption: boolean;
    FeOption: string;
    FsOption: boolean;
    FaOption: string;
    FoOption: char;
    FmOption: integer;
    FdOption: integer;
    FxOption: TStringList;
    FtOption: boolean;
    FlOption: boolean;
    FyOption: string;
    FkOption: boolean;
    FcdOption: string;
    FcfgOption: string;
    FpriOption: integer;
    FArcName:  string;
    FFileMasks: TStringList;
  private
    FRun: boolean;
    F0Option: pointer;
    F1Option: boolean;
    F2Option: boolean;
    FParams: TStringList;
  private
    procedure ProcessOptions;
    procedure ProcessParams;
    function ConfirmAdd: boolean;
    function ConfirmExtract: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Command: char read FCommand;
    property ArcName: string read FArcName;
    property Params: TStringList read FParams;
    property Ptr: Pointer read F0Option;
    property Log: boolean read F1Option;
    property Run: boolean read FRun;
  end;

  constructor TCmdLine.Create;
  var
    i: integer;
    S: string;
  begin
    inherited Create;
    // ---
    FCommand := ' ';
    FrOption := False;
    FuOption := False;
    FfOption := False;
    FeOption := '';
    FsOption := False;
    FaOption := '';
    FoOption := 'Y';
    FmOption := 1;
    FdOption := 3;
    FxOption := TStringList.Create;
    FtOption := False;
    FlOption := False;
    FyOption := '';
    FkOption := False;
    FcdOption := '';
    FcfgOption := '';
    FpriOption := 2;
    FArcName  := '';
    FFileMasks := TStringList.Create;
    // ---
    FRun := False;
    F0Option := nil;
    F1Option := False;
    F2Option := False;
    FParams := TStringList.Create;
    // ---
    ProcessOptions;
  end;
  
  procedure TCmdLine.ProcessOptions;
  var
    S: string;
    i: integer;
  begin
    // catch options, command, archive name and name of files
    for i := 1 to ParamCount do
    begin
      S := ParamStr(i);
      if (Length(S) > 1) and (S[1] = '-') then
      begin
        // options...
        case UpCase(S[2]) of
          '0': begin
                 System.Delete(S, 1, 2);
                 F0Option := Pointer(StrToInt(S));
               end;
          '1': begin
                 F1Option := True;
               end;
          '2': begin
                 F2Option := True;
               end;
          'R': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FrOption := True
                 else
                   if (S = '-') then FrOption := False;
               end;
          'U': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FuOption := True
                 else
                 if (S = '-') then FuOption := False;
               end;
          'F': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FfOption := True
                 else
                   if (S = '-') then FfOption := False;
               end;
          'E': begin
                 System.Delete(S, 1, 2);
                 if ExtractFileExt('.' + S) <> '.' then
                 begin
                   FeOption := ExtractFileExt('.' + S);
                 end;
               end;
          'S': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FsOption := True
                 else
                   if (S = '-') then FsOption := False;
               end;
          'A': begin
                 System.Delete(S, 1, 2);
                 if (S = 'GUI') or (Length(S) = 0) then
                   FaOption := 'beecore.sfx'
                 else
                   if (S = 'CMD') then
                     FaOption := 'bee.sfx'
                   else
                     FaOption := 'nul';
               end;
          'O': begin
                 System.Delete(S, 1, 2);
                 if (Length(S) = 1) and (UpCase(S[1]) in ['A', 'S', 'Q']) then
                 begin
                   FoOption := UpCase(S[1]);
                 end;
               end;
          'M': begin
                 System.Delete(S, 1, 2);
                 if (Length(S)= 1) and (S[1] in ['0'..'3']) then
                 begin
                   FmOption := StrToInt(S[1]);
                 end;
               end;
          'D': begin
                 System.Delete(S, 1, 2);
                 if (Length(S)= 1) and (S[1] in ['0'..'9']) then
                 begin
                   FdOption := StrToInt(S[1]);
                 end;
               end;
          'X': begin
                 System.Delete(S, 1, 2);
                 if Length(S) > 0 then
                 begin
                   FxOption.Add(S);
                 end;
               end;
          'T': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FtOption := True
                 else
                   if (S = '-') then FtOption := False;
               end;
          'L': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FlOption := True
                 else
                   if (S = '-') then FlOption := False;
               end;
          'Y': begin
                 System.Delete(S, 1, 2);
                 if DirectoryExists(ExcludeTrailingBackslash(S)) then
                 begin
                   FyOption := ExcludeTrailingBackslash(S);
                 end;
               end;
          'K': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FkOption := True
                 else
                   if (S = '-') then FkOption := False;
                end;
          else  if FileNamePos('-cd', S) = 1 then
                begin
                  System.Delete(S, 1, 3);
                  if Length(S) > 0 then
                  begin
                    FcdOption := IncludeTrailingBackslash(FixDirName(S));
                  end;
                end else
                begin
                  if FileNamePos('-cfg', S) = 1 then
                  begin
                    System.Delete(S, 1, 4);
                    if (Length(S) > 0) and FileExists(S) then
                    begin
                      FcfgOption := S;
                    end;
                  end else
                  begin
                    if FileNamePos('-pri', S) = 1 then
                    begin
                      System.Delete(S, 1, 4);
                      if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
                      begin
                        FpriOption := StrToInt(S[1]);
                      end;
                    end
                  end;
                end;
        end; // end case
      end else
      begin
        // command or filenames...
        if FCommand = ' ' then
        begin
          if Length(S) = 1 then
            FCommand := UpCase(S[1])
          else
            FCommand := '?';
        end else
          if FArcName = '' then
          begin
            FArcName := ExpandFileName(S);
            if ExtractFileExt(FArcName) = '' then
            begin
              FArcName := ChangeFileExt(FArcName, '.bee');
            end;
          end else
            FFileMasks.Add(DoDirSeparators(S));
      end;
    end; // end for loop
    ProcessParams;
  end;

  destructor TCmdLine.Destroy;
  begin
    FxOption.Free;
    FFileMasks.Free;

    FParams.Free;
    F0Option := nil;
    inherited Destroy;
  end;

  procedure TCmdLine.ProcessParams;
  var
    i: integer;
  begin
    FRun := True;
    if F2Option then
    begin
      case FCommand of
        'A': FRun := ConfirmAdd;
        'E': FRun := ConfirmExtract;
        'X': FRun := ConfirmExtract;
        'T': FRun := ConfirmAdd;
        else FRun := False;
      end;
    end;
    
    if FRun then
    begin
      FParams.Add(FCommand);

      if FrOption then FParams.Add('-r+') else FParams.Add('-r-');
      if FuOption then FParams.Add('-u+') else FParams.Add('-u-');
      if FfOption then FParams.Add('-f+') else FParams.Add('-f-');

      if Length(FeOption) > 0 then
        FParams.Add('-e' + FeOption);

      if FsOption then
        FParams.Add('-s+')
      else
        FParams.Add('-s-');
      
      if Length(FaOption) > 0 then
        FParams.Add('-a' + FaOption);

      FParams.Add('-o'+ FoOption);
      FParams.Add('-m'+ IntToStr(FmOption));
      FParams.Add('-d'+ IntToStr(FdOption));
      
      for i := 0 to FxOption.Count - 1 do
        FParams.Add('-x' + FxOption.Strings[i]);

      if FtOption then FParams.Add('-t+') else FParams.Add('-t-');
      if FlOption then FParams.Add('-l+') else FParams.Add('-l-');

      if Length(fyOption) > 0 then
        FParams.Add('-y' + FyOption);
        
      if FkOption then
        FParams.Add('-k+')
      else
        FParams.Add('-k-');

      if Length(FcdOption) > 0 then
        FParams.Add('-cd' + FcdOption);

      if Length(FcfgOption) > 0 then
        FParams.Add('-cfg' +  FcfgOption);

      FParams.Add('-pri' + IntTostr(FpriOption));
      FParams.Add(FArcName);

      for i := 0 to FFileMasks.Count - 1 do
        FParams.Add(FFileMasks.Strings[i]);
    end;
  end;
  
  function TCmdLine.ConfirmAdd: boolean;
  var
    i: integer;
    F: TAddFrm;
  begin
    F := TAddFrm.Create(Application);
    F.rOption.Checked := FrOption;
    if (FuOption xor FfOption) then
    begin
      if FuOption then
        F.ufOption.ItemIndex := 0
      else
        F.ufOption.ItemIndex := 1;
    end else
    begin
      F.ufOption.ItemIndex := 2;
    end;
    F.eOption.Text := FeOption;
    F.sOption.Checked := FsOption;

    if Length(FaOption) > 0 then
    begin
      F.aOptionCheck.Checked := True;
      if FaOption = 'beecore.sfx' then
        F.aOption.ItemIndex := 0
      else
        if FaOption = 'bee.sfx' then
          F.aOption.ItemIndex := 1
        else
          if FaOption = 'nul' then
            F.aOption.ItemIndex := 2;
    end else
      F.aOptionCheck.Checked := False;

    //FoOption nothing to do
    
    F.mOption.ItemIndex := FmOption;
    F.dOption.ItemIndex := FdOption;

    for i := 0 to FxOption.Count -1  do
      F.FilesMgr.PlusMinus(F.FilesMgr.AddFile(
        ExpandFileName(FxOption.Strings[i])));

    F.tOption.Checked := FtOption;
    F.lOption.Checked := FlOption;

    if Length(fyOption) > 0 then
      F.yOption.Text := FyOption;

    F.kOption.Checked := FkOption;

    if Length(FcdOption) > 0 then
      F.cdOption.Text := FcdOption;

    if Length(FcfgOption) > 0 then
      F.cfgOption.Text := FcfgOption;

    F.priOption.ItemIndex := FpriOption;
    F.ArchivePath := ExtractFilePath(ExpandFileName(FArcName));
    F.ArchiveName.Text := ExtractFileName(ExpandFileName(FArcName));

    for i := 0 to FFileMasks.Count - 1 do
      F.FilesMgr.AddFile(ExpandFileName(FFileMasks.Strings[i]));

    // Form.ShowModal
    if F.ShowModal = mrOk then
    begin
      (*
      FrOption := F.rOption.Checked;

      case F.ufOption.ItemIndex of
        0: begin
             FuOption := True;
             FfOption := False;
           end;
        1: begin
             FuOption := False;
             FfOption := True;
           end;
        2: begin
             FuOption := True;
             FfOption := True;
           end;
      end;

      FeOption := F.eOption.Text;
      FsOption := F.sOption.Checked;

      if F.aOptionCheck.Checked then
      begin
        case F.aOption.ItemIndex of
          0: FaOption := 'beecore.sfx';
          1: FaOption := 'bee.sfx';
          2: FaOption := 'nul';
        end;
      end;

      // FoOption nothing to do
      FmOption := F.mOption.ItemIndex;
      FdOption := F.dOption.ItemIndex;

      FxOption.Clear;
      for i := 0 to F.FilesMgr.Count - 1 do
        if F.FilesMgr.Excluded[i] then
          FxOption.Add(F.FilesMgr.Items[i]);

      FtOption := F.tOption.Checked;
      FlOption := F.lOption.Checked;
      FyOption := F.yOption.Text;
      FkOption := F.kOption.Checked;

      FcdOption := F.cdOption.Text;
      FcfgOption := F.cfgOption.Text;
      FpriOption := F.priOption.ItemIndex;

      FArcName :=  F.ArchiveName.Text;

      FFileMasks.Clear;
      for i := 0 to F.FilesMgr.Count - 1 do
        if F.FilesMgr.Excluded[i] = False then
          FFileMasks.Add(F.FilesMgr.Items[i]);
      
      if Length(F.FilesMgr.RootValue) > 0 then
        SetCurrentDir(F.FilesMgr.RootValue);
      *)
      Result := True;
    end else
      Result := False;

    FreeAndNil(F);
  end;
  
  function TCmdLine.ConfirmExtract: boolean;
  var
    F: TExtractFrm;
  begin
    F := TExtractFrm.Create(Application);
    if F.ShowModal = mrOk then
    begin
      Result := True;
    end else
      Result := False;
    FreeAndNil(F);
  end;
  
  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  TGui Application class                                                //
  //                                                                        //
  // ---------------------------------------------------------------------- //

type

  { TGui Application class }

  TGui = class
  private
    FApp: TBeeApp;
    FAppKey: string;
    FAppLog: TStringList;
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
  public
    constructor Create(ATickFrm: TTickFrm; ACmdLine: TCmdLine);
    destructor Destroy; override;
  end;

  constructor TGui.Create(ATickFrm: TTickFrm; ACmdLine: TCmdLine);
  var
    i: integer;
  begin
    inherited Create;
    FTickFrm := ATickFrm;
    FTickFrm.Timer.OnStartTimer := OnStartTimer;
    FTickFrm.Timer.OnTimer := OnTimer;
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

    FTime := 0;
    FSwitch := False;
    FCmdLine := ACmdLine;
    // ---
    FAppKey := '';
    FAppLog := TStringList.Create;
    // ---
    FApp := TBeeApp.Create(FAppInterface, FCmdLine.Params);
    FApp.OnTerminate := OnTerminate;
    FAppTerminated := False;
    FApp.Suspended := False;
  end;

  destructor TGui.Destroy;
  begin
    FAppLog.Free;
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
    ShowMEssage(FAppInterface.OnFatalError.Data.Msg);
    FAppLog.Add(FAppInterface.OnFatalError.Data.Msg);
  end;

  procedure TGui.OnError;
  begin
    FAppLog.Add(FAppInterface.OnError.Data.Msg);
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

  procedure TGui.OnClear;
  begin
    // nothing to do
  end;

  procedure TGui.OnList;
  begin

  end;

  procedure TGui.OnKey;
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

  // ---------------------------------------------------------------------- //
  //                                                                        //
  //  Main Block                                                            //
  //                                                                        //
  // ---------------------------------------------------------------------- //

var
  Gui: TGui;
  CmdLine: TCmdLine;

begin
  Application.Initialize;
  Application.Title := 'BeeCore';
  CmdLine := TCmdLine.Create;
  if CmdLine.Run then
  begin
    if CmdLine.Command in [' ', '?'] then
    begin
       Application.CreateForm(TAboutFrm, AboutFrm);
       Application.Run;
    end else
    begin
      Application.CreateForm(TTickFrm, TickFrm);
      Gui := TGui.Create(TickFrm, CmdLine);
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
      FreeAndNil(Gui);
    end;
    FreeAndNil(CmdLine);
  end;
end.

