library BeeCore;

uses
  ShareMem,
  Classes,
  Bee_Interface,
  Bee_App;

type

  TCore = class
  private
    function Split(aParams: string): TStringList;
  private
    procedure OnTerminate(Sender:TObject);
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
    App: TBeeApp;
  end;

  constructor TCore.Create(aParams: string);
  var
    I: integer;
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

    Params := Split(aParams);
    SetLength(AppKey, 0);

    App := TBeeApp.Create(Interfaces, Params);
    App.OnTerminate := OnTerminate;
  end;

  function TCore.Split(aParams: string): TStringList;
  var
    I: integer;
    P: string;
  begin
    Result := TStringList.Create;

    I := 1;
    while I <= Length(aParams) do
    begin
      if aParams[I] = '"' then
      begin
        Inc(I);
        while I <= Length(aParams) do
        begin
          if aParams[I] <> '"' then
            P := P + aParams[I]
          else
            Break;
          Inc(I);
        end;
      end else
      begin
        if aParams[I] = ' ' then
        begin
          if P <> '' then
          begin
            Result.Add(P);
            P := '';

          end;
        end else
          P := P + aParams[I];
      end;
      Inc(I);
    end;
    if P <> '' then Result.Add(P);
  end;

  destructor TCore.Destroy;
  begin
    SetLength(AppKey, 0);

    App.Free;
    Params.Free;
    Interfaces.Free;
    inherited Destroy;
  end;

  procedure TCore.OnTerminate(Sender: TObject);
  begin
    Interfaces.OnDisplay.Data.Msg := 'Finished';
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
  Core: TCore = nil;

function CreateCore(aParams: string): boolean; stdcall;
begin
  if Core = nil then
  begin
    Result := True;
    Core := TCore.Create(aParams);
  end else
    Result := False;
end;

procedure ExecuteCore; stdcall;
begin
  if Core <> nil then
  begin
    Core.App.Execute;
  end;
end;

procedure DestroyCore; stdcall;
begin
  if Core <> nil then
  begin
    if Core.Interfaces.Terminated then
    begin;
      Core.Free;
      Core := nil;
    end;
  end;
end;

procedure StopCore; stdcall;
begin
  if Core <> nil then
  begin
    Core.Interfaces.Stop := True;
  end;
end;

procedure SuspendCore; stdcall;
begin
  if Core <> nil then
  begin
    Core.Interfaces.Suspend := True;
  end;
end;

function GetMessage: string; stdcall;
begin
  if Core <> nil then
  begin
    Result := Core.Interfaces.OnDisplay.Data.Msg;
  end else
    Result := '';
end;

function GetPercentage: integer; stdcall;
begin
  if Core <> nil then
  begin
    Result := Core.Interfaces.OnTick.Data.Percentage;
  end else
    Result := 0;
end;

exports
  CreateCore,
  ExecuteCore,
  DestroyCore,
  StopCore,
  SuspendCore,
  GetMessage,
  GetPercentage;

begin

end.

