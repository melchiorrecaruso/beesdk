{
  Copyright (c) 2013 Melchiorre Caruso

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

    Tick form class.

  Modifyed:

}

unit bxm_TickFrm;

{$I bxm_compiler.inc}

interface

uses
  Arrow,
  Buttons,
  Dialogs,
  Classes,
  ComCtrls,
  Controls,
  Graphics,
  ExtCtrls,
  Forms,
  IniFiles,
  LResources,
  Menus,
  StdCtrls,
  SysUtils,
  // ---
  bxm_Plugins;

type

  { TTickFrm }

  TTickFrm = class(TForm)
    BtnCancel: TBitBtn;
    DetailsArrow: TArrow;
    DetailsLabel: TLabel;
    Image: TImage;
    ActionLabel: TLabel;
    ArchiveLabel: TLabel;
    ArchiveNameLabel: TLabel;
    Report: TMemo;
    ReportPanel: TPanel;
    TickLabel: TLabel;
    TickProgressBar: TProgressBar;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure DetailsArrowClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure OnTerminate;
    procedure OnExecute;
  private
    { private declarations }
    Parser: TParser;
    procedure DoTerminate(Sender: TObject);
  public
    { public declarations }
    function ShowModal(Parser: TParser): longint; overload;
  end;

var
  TickFrm: TTickFrm;

implementation

{$R *.lfm}

uses
  bxm_SysUtils;

{ TTickFrm class }

procedure TTickFrm.FormCreate(Sender: TObject);
begin
  DetailsArrow.ArrowType := atRight;

  {$IFDEF MSWINDOWS}
  TickProgressBar.Smooth := FALSE;
  {$ELSE}
  TickProgressBar.Smooth := TRUE;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  Constraints.MinHeight  := 185;
  Constraints.MaxHeight  := 185;
  {$ELSE}
  Constraints.MinHeight  := 205;
  Constraints.MaxHeight  := 205;
  {$ENDIF}
  Constraints.MinWidth   := 450;
  Constraints.MaxWidth   := 450;
end;

procedure TTickFrm.FormDestroy(Sender: TObject);
begin

end;

procedure TTickFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TTickFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TTickFrm.DoTerminate(Sender: TObject);
var
  i: longint;
begin
  if TParser(Sender).Count > 0 then
  begin
    for i := 0 to TParser(Sender).Count - 1 do
      Report.Append(TParser(Sender).Messages[i]);

    if ReportPanel.Visible = FALSE then
      DetailsArrowClick(Self);
  end;
end;

function TTickFrm.ShowModal(Parser: TParser): longint;
var
  i: longint;
begin
  Report.Clear;

  Parser.OnTerminate := DoTerminate;
  Parser.Execute;



  Result := ShowModal;
end;

procedure TTickFrm.DetailsArrowClick(Sender: TObject);
begin
  ReportPanel.Visible := not ReportPanel.Visible;
  if ReportPanel.Visible then
  begin
    DetailsArrow.ArrowType := atDown;
    Constraints.MaxHeight := Constraints.MaxHeight + ReportPanel.Height;
    Constraints.MinHeight := Constraints.MaxHeight;
  end else
  begin
    DetailsArrow.ArrowType := atRight;
    Constraints.MinHeight := Constraints.MinHeight - ReportPanel.Height;
    Constraints.MaxHeight := Constraints.MinHeight;
  end;
  Height := Constraints.MaxHeight;
end;

 // ------------------------------------------------------------------------ //
 //                                                                          //
 // Timer Events Routines                                                    //
 //                                                                          //
 // ------------------------------------------------------------------------ //

procedure TTickFrm.OnTimer(Sender: TObject);
begin



end;

procedure TTickFrm.OnExecute;
begin


end;

procedure TTickFrm.OnTerminate;
begin

end;

 // ------------------------------------------------------------------------ //
 //                                                                          //
 // Buttons Click Routines                                                   //
 //                                                                          //
 // ------------------------------------------------------------------------ //

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
      TickLabel.Caption := Data.Msg;
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
    TickProgressBar.Position := FInterfaces.OnTick.Data.Percentage;
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

end.