program ZipApp;

{$MODE Delphi}

{ Contains:

  The data compression utility.

  Modifyed:

  v0.7.9 build 0112 - 2007.11.17 by Melchiorre Caruso;
}

uses
  Classes,
  SysUtils,
  // ---
  Bee_Common,
  Bee_Interface,
  // ---
  BeeGui_ZipApp;

type
  TConsole = class
  private
    App: TZipApp;
    AppKey: string;
    AppInterface: TAppInterface;
    AppParams: TStringList;
    procedure OnOverWrite;
    procedure OnWarning;
    procedure OnDisplay;
    procedure OnRename;
    procedure OnError;
    procedure OnClear;
    procedure OnList;
    procedure OnTick;
    procedure OnKey;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

  /// implementation ///

  // TConsole

  constructor TConsole.Create;
  var
    I: integer;
  begin
    AppInterface := TAppInterface.Create;
    AppInterface.OnOverWrite := OnOverWrite;
    AppInterface.OnWarning := OnWarning;
    AppInterface.OnDisplay := OnDisplay;
    AppInterface.OnRename := OnRename;
    AppInterface.OnClear := OnClear;
    AppInterface.OnError := OnError;
    AppInterface.OnList := OnList;
    AppInterface.OnTick := OnTick;
    AppInterface.OnKey := OnKey;

    AppKey := '';
    AppParams := TStringList.Create;
    for I := 1 to ParamCount do
      AppParams.Add(ParamStr(I));
    App := TZipApp.Create(AppInterface, AppParams);
  end;

  destructor TConsole.Destroy;
  begin
    AppInterface.Free;
    AppParams.Free;
    AppKey := '';
  end;

  procedure TConsole.Execute;
  begin
    App.Execute;
  end;

  procedure TConsole.OnOverWrite;
  begin
    Writeln;
    Writeln('"' + ParamToOem(AppInterface.cFileName) + '" already exists.');
    Write('Overwrite it?  [Yes/No/Rename/All/Skip/Quit]: ');
    Readln(AppInterface.cMsg);
    Writeln;
    // convert oem to param
    AppInterface.cMsg := UpperCase(OemToParam(AppInterface.cMsg));
  end;

  procedure TConsole.OnKey;
  begin
    if Length(AppKey) = 0 then
    begin
      Writeln;
      Write('Insert a key (min length 4 char): ');
      Readln(AppKey);
      Writeln;
      // convert oem to param
      AppKey := OemToParam(AppKey);
    end;
    AppInterface.cMsg := AppKey;
  end;

  procedure TConsole.OnRename;
  begin
    Writeln;
    Write('Rename file "' + ParamToOem(AppInterface.cFileName) + '" as (empty to skip):');
    Readln(AppInterface.cMsg);
    Writeln;
    // convert oem to param
    AppInterface.cMsg := OemToParam(AppInterface.cMsg);
  end;

  procedure TConsole.OnWarning;
  begin
    Writeln(ParamToOem(AppInterface.cMsg));
  end;

  procedure TConsole.OnDisplay;
  begin
    Writeln(ParamToOem(AppInterface.cMsg));
  end;

  procedure TConsole.OnError;
  begin
    Writeln(ParamToOem(AppInterface.cMsg));
  end;

  procedure TConsole.OnList;
  begin
    AppInterface.cList := nil;
  end;

  procedure TConsole.OnTick;
  begin
    // not convert oem to param
    Write(Format('  (%d%%)', [AppInterface.cPercentage]));
  end;

  procedure TConsole.OnClear;
  begin
    Write(#13, #13: 80);
  end;

  /// main block ///

var
  Console: TConsole;

begin
  Console := TConsole.Create;
  Console.Execute;
  Console.Free;
end.
