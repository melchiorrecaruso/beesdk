program BeeSfx;{ Contains:

  The SFX module for generic platform.

  Features:

  1. Uses context modelling and some variant of arithmetic encoding.
  2. Uses integer arithmetic only.
  3. Uses half-byte alphabet.

  (C) 1999-2006 Melchiorre Caruso.

  Modifyed:

  v0.1.0 build 0062 - 2007/01/27 by Melchiorre Caruso.
}{$R-,Q-,S-}{$R BeeSfx.ico.res}

uses
  Classes,
  SysUtils,
  Bee_App,
  Bee_Common,
  Bee_Interface;

type
  TSfx = class
  private
    App: TBeeApp;
    AppKey: string;
    AppParams: TStringList;
    AppInterface: TAppInterface;

    procedure OnOverWrite;
    procedure OnWarning;
    procedure OnDisplay;
    procedure OnRename;
    procedure OnError;
    procedure OnClear;
    procedure OnTick;
    procedure OnList;
    procedure OnKey;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

  constructor TSfx.Create;
  begin
    AppKey := '';

    AppParams := TStringList.Create;
    AppParams.Add('x');
    AppParams.Add(ParamStr(0));
    AppParams.Add(IncludeDelimiter('*') + '*.*');

    AppInterface := TAppInterface.Create;
    AppInterface.OnOverWrite := OnOverWrite;
    AppInterface.OnWarning := OnWarning;
    AppInterface.OnDisplay := OnDisplay;
    AppInterface.OnRename := OnRename;
    AppInterface.OnError := OnError;
    AppInterface.OnClear := OnClear;
    AppInterface.OnList := OnList;
    AppInterface.OnTick := OnTick;
    AppInterface.OnKey := OnKey;

    App := TBeeApp.Create(AppInterface, AppParams);
  end;

  destructor TSfx.Destroy;
  begin
    AppKey := '';
    AppParams.Free;
    AppInterface.Free;
  end;

  procedure TSfx.Execute;
  begin
    App.Execute;
  end;

  procedure TSfx.OnOverWrite;
  begin
    Writeln;
    Writeln('"' + AppInterface.CFileName + '" already exists.');
    Write('Overwrite it?  [Yes/No/Rename/All/Skip/Quit]: ');
    Readln(AppInterface.cMsg);
    Writeln;
  end;

  procedure TSfx.OnRename;
  begin
    Writeln;
    Writeln('Rename' + '"' + AppInterface.cFileName + '" as (empty to skip):');
    Readln(AppInterface.cMsg);
    Writeln;
  end;

  procedure TSfx.OnWarning;
  begin
    // Writeln (AppInterface.cMsg); - hide cfg warning !
  end;

  procedure TSfx.OnDisplay;
  begin
    Writeln(AppInterface.cMsg);
  end;

  procedure TSfx.OnError;
  begin
    Writeln(AppInterface.cMsg);
  end;

  procedure TSfx.OnList;
  begin
    AppInterface.cList := nil;
  end;

  procedure TSfx.OnTick;
  begin
    Write(Format(#8#8#8#8#8#8#8 + '  (%d', [AppInterface.cPercentage]) + '%)');
  end;

  procedure TSfx.OnClear;
  begin
    DelLine;
  end;

  procedure TSfx.OnKey;
  begin
    if Length(AppKey) = 0 then
    begin
      Writeln;
      Write('Insert a key (min length 4 char): ');
      Readln(AppKey);
      Writeln;
    end;
    AppInterface.cMsg := AppKey;
  end;

  /// Main block

var
  ExtractionPath: string = '';
  Sfx: TSfx = nil;
  C: char;

begin
  Writeln('The Bee self-extractor 0.1.0 build 0062 archiver utility, Jan 2007.' +
    Cr + '(C) 2005-2007 Andrew Filinsky and Melchiorre Caruso.');

  repeat
    Write(Cr + 'Do you continue with files extraction? [Y, N] ');
    Readln(C);
  until C in ['Y', 'y', 'N', 'n'];

  if C in ['Y', 'y'] then
  begin
    Write(Cr + 'Set extraction path (empty for current path): ');
    Readln(ExtractionPath);

    Bee_Common.ForceDirectories(ExtractionPath);
    if SetCurrentDir(ExtractionPath) then
    begin
      Write(Cr + 'Start extractor module ...' + Cr);
      Sfx := TSfx.Create;
      Sfx.Execute;
      Sfx.Free;
    end else
      Write(Cr + 'Error: can''t set extraction path');
  end;
end.
