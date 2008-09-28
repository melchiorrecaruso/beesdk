library BeeCore;

uses
  Classes,

  Bee_Interface,
  Bee_App;

type

  TCore = class
  private
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
  public
   constructor Create(aParams: string);
   destructor Destroy;
  public
    Interfaces: TInterfaces;
    Params: TParams;
    AppKey: string;
    App: TApp;
  end;

  constructor TCore.Create(aParams: string);
  begin
    inherited Create;
    Interfaces := TInterfaces.Create;
    Interfaces.OnFatalError.Method := OnFatalError;
    Interfaces.OnOverWrite.Method := OnOverWrite;
    Interfaces.OnWarning.Method := OnWarning;
    Interfaces.OnDisplay.Method := OnDisplay;
    Interfaces.OnRequest.Method := OnRequest;
    Interfaces.OnRename.Method := OnRename;
    Interfaces.OnClear.Method := OnClear;
    Interfaces.OnError.Method := OnError;
    Interfaces.OnList.Method := OnList;
    Interfaces.OnTick.Method := OnTick;
    Interfaces.OnKey.Method := OnKey;

    Params := TStringList.Create;
    Params.Text := aParams;

    SetLength(AppKey, 0);

    App := TBeeApp.Create(Interfaces, Params);
  end;

  destructor TCore.Destroy;
  begin
    SetLength(AppKey, 0);

    App.Free;
    Params.Free;
    Interfaces.Free;
    inherited Destroy;
  end;

  procedure TCore.OnFatalError;
  begin
  end;

  procedure TCore.OnOverWrite;
  begin
  end;

  procedure TCore.OnKey;
  begin
  end;

  procedure TCore.OnRename;
  begin
  end;

  procedure TCore.OnWarning;
  begin
  end;

  procedure TCore.OnDisplay;
  begin
  end;

  procedure TCore.OnRequest;
  begin
  end;

  procedure TCore.OnError;
  begin
  end;

  procedure TCore.OnList;
  begin
  end;

  procedure TCore.OnTick;
  begin
  end;

  procedure TCore.OnClear;
  begin
  end;


// -- Interfaces -- //

var
  Core: TCore;

function CreateCore(aParams: string): boolean;
begin
  if Core = nil then
  begin
    Core := TCore.Create(aParams);
  end;
end;

procedure ExecuteCore;
begin
  Core.App.Resume;
end;

function DestroyCore(Index: integer): boolean;
begin
  if Core <> nil then
  begin
    if Core.Interfaces.Terminated then
    begin;
      Core.Free;
      Core := nil;
    end;
  end;
  Result := Core = nil;
end;

procedure StopCore;
begin
  Core.Interfaces.Stop := True;
end;

procedure SuspendCore;
begin
  Core.Interfaces.Suspend := True;
end;

exports CreateCore;
exports ExecuteCore;
exports DestroyCore;
exports StopCore;
exports SuspendCore;

begin
  Core:= nil;
end.

