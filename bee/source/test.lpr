program Test;

{$mode delphi}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,

  Bee_Files,
  BeeSdk_Archive;

type
  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    FArchiveUpdater: TArchiveUpdater;
    procedure OnProgress(Value: longint);
    procedure OnMessage(const Message: string);
    procedure OnFailure(const ErrorMessage: string; ErrorCode: longint);
    procedure OnRename(Item: TArchiveItem; var RenameAs: string; var Confirm: TArchiveConfirm);
    procedure OnExtract(Item: TArchiveItem; var ExtractAs: string; var Confirm: TArchiveConfirm);
    procedure OnErase(Item: TArchiveItem; var Confirm: TArchiveConfirm);
    procedure OnUpdate(SearchRec: TCustomSearchRec; var UpdateAs; var Confirm: TArchiveConfirm);

    procedure OnRequestBlankDisk(var Abort : Boolean);
    procedure OnRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  Scanner: TFileScanner;
begin
  writeln('BEE SDK Tester - B.0001');
  // quick check parameters
  ErrorMsg := CheckOptions('h','help');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;


  Scanner := TFileScanner.Create;
  Scanner.Scan('test.exe');


  FArchiveUpdater := TArchiveUpdater.Create;
  FArchiveUpdater.OnUpdate           := OnUpdate;
  FArchiveUpdater.OnProgress         := OnProgress;
  FArchiveUpdater.OnMessage          := OnMessage;
  FArchiveUpdater.OnFailure          := OnFailure;
  FArchiveUpdater.OnRequestImage     := OnRequestImage;
  FArchiveUpdater.OnRequestBlankDisk := OnRequestBlankDisk;






  FArchiveUpdater.Destroy;
  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExtractFileName(ExeName),' -h');
end;

procedure TMyApplication.OnProgress(Value: longint);
begin
end;

procedure TMyApplication.OnMessage(const Message: string);
begin
end;

procedure TMyApplication.OnFailure(const ErrorMessage: string; ErrorCode: longint);
begin
end;

procedure TMyApplication.OnRename(Item: TArchiveItem;
  var RenameAs: string; var Confirm: TArchiveConfirm);
begin
end;

procedure TMyApplication.OnExtract(Item: TArchiveItem;
  var ExtractAs: string; var Confirm: TArchiveConfirm);
begin
end;

procedure TMyApplication.OnErase(Item: TArchiveItem;
  var Confirm: TArchiveConfirm);
begin
end;

procedure TMyApplication.OnUpdate(SearchRec: TCustomSearchRec;
  var UpdateAs; var Confirm: TArchiveConfirm);
begin
end;

procedure TMyApplication.OnRequestBlankDisk(var Abort : Boolean);
begin
end;

procedure TMyApplication.OnRequestImage(ImageNumber: longint; var ImageName: string; var Abort: boolean);
begin
end;

var
  Application: TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='BEE SDK Tester';
  Application.Run;
  Application.Free;
end.

