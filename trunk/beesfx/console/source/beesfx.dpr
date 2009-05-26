program BeeSfx;

{ Contains:

  The SFX module for generic platform.

  Features:

  1. Uses context modelling and some variant of arithmetic encoding.
  2. Uses integer arithmetic only.
  3. Uses half-byte alphabet.

  (C) 1999-2009 Melchiorre Caruso.

  Modifyed:

  v0.1.0 build 0062 - 2007/01/27 by Melchiorre Caruso.
}

{$I compiler.inc}

{$R BeeSfx.ico.res}

uses
  {$IFDEF CONSOLEAPPLICATION}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$ENDIF}
  Classes,
  SysUtils,
  // ---
  Bee_App,
  Bee_Types,
  Bee_Common,
  Bee_Interface;

type
  TConsole = class
  private
    FApp: TBeeApp;
    FKey: string;
    FParams: TStringList;
  private
    procedure ProcessFatalError(const aMessage: string);
    procedure ProcessError(const aMessage: string);
    procedure ProcessWarning(const aMessage: string);
    procedure ProcessMessage(const aMessage: string);
    procedure ProcessOverwrite(const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessRename(const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessList(const aFileInfo: TFileInfoExtra);
    procedure ProcessPassword(const aFileInfo: TFileInfo; var Result: string);
    procedure ProcessRequest(const aMessage: string);
    procedure ProcessProgress;
    procedure ProcessClear;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

  constructor TConsole.Create;
  var
    I: longint;
  begin
    inherited Create;
    SetLength(FKey, 0);
    FParams := TStringList.Create;
    for I := 1 to ParamCount do
    begin
      FParams.Add(ParamStr(I));
    end;
    FApp := TBeeApp.Create(FParams);
    FApp.OnFatalError := ProcessFatalError;
    FApp.OnError      := ProcessError;
    FApp.OnWarning    := ProcessWarning;
    FApp.OnMessage    := ProcessMessage;
    FApp.OnOverwrite  := ProcessOverwrite;
    FApp.OnRename     := ProcessRename;
    FApp.OnList       := ProcessList;
    FApp.OnPassword   := ProcessPassword;
    FApp.OnRequest    := ProcessRequest;
    FApp.OnProgress   := ProcessProgress;
    FApp.OnClear      := ProcessClear;
  end;

  destructor TConsole.Destroy;
  begin
    FApp.Destroy;
    FParams.Destroy;
    SetLength(FKey, 0);
    inherited Destroy;
  end;

  procedure TConsole.Execute;
  begin
    FApp.Execute;
    ExitCode := FApp.Code;
  end;

  procedure TConsole.ProcessFatalError(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessError(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessWarning(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessMessage(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessOverwrite(const aFileInfo: TFileInfo; var Result: string);
  begin
    with aFileInfo do
    begin
      Writeln('Warning: file "',
        ParamToOem(PCharToString(FilePath)),
        ParamToOem(PCharToString(FileName)), '" already exists.');

      Write('Overwrite it?  [Yes/No/Rename/All/Skip/Quit]: ');
    end;
    // not convert oem to param
    Readln(Result);
  end;

  procedure TConsole.ProcessRename(const aFileInfo: TFileInfo; var Result: string);
  begin
    with aFileInfo do
    begin
      Write('Rename file "',
        ParamToOem(PCharToString(FilePath)),
        ParamToOem(PCharToString(FileName)), '" as (empty to skip):');
    end;
    Readln(Result);
    // convert oem to param
    Result := OemToParam(Result);
  end;

  procedure TConsole.ProcessList(const aFileInfo: TFileInfoExtra);
  begin
    // nothing to do
  end;

  procedure TConsole.ProcessPassword(const aFileInfo: TFileInfo; var Result: string);
  var
    S: string;
  begin
    if Length(FKey) = 0 then
    begin
      Write('Insert a key (min length 4 char): ');
      Readln(Result);
      // convert oem to param
      Result := OemToParam(Result);
      // store password
      Write('Do you want to use password for this session? [Yes, No]: ');
      Readln(S);
      if (Length(S)= 1) and (Upcase(S) = 'Y') then FKey := Result;
    end else
      Result := FKey;
  end;

  procedure TConsole.ProcessRequest(const aMessage: string);
  begin
    Writeln(ParamToOem(aMessage));
  end;

  procedure TConsole.ProcessProgress;
  begin
    // not convert oem to param
    Write(#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8#8 +
      Format('%5d KB/s %3d%%', [FApp.Speed shr 10, FApp.Percentes]));
  end;

  procedure TConsole.ProcessClear;
  begin
    Write(#13, #13: 80);
  end;

  /// Main block

var
  ExtractionPath: string = '';
  Console: TConsole = nil;
  C: char;

  /// control+c event ///

  {$IFDEF MSWINDOWS}
  function CtrlHandler(CtrlType: longword): longbool;
  begin
    case CtrlType of
      CTRL_C_EVENT:        Console.FApp.Terminated := True;
      CTRL_BREAK_EVENT:    Console.FApp.Terminated := True;
      CTRL_CLOSE_EVENT:    Console.FApp.Terminated := True;
      CTRL_LOGOFF_EVENT:   Console.FApp.Terminated := True;
      CTRL_SHUTDOWN_EVENT: Console.FApp.Terminated := True;
    end;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  procedure CtrlHandler(sig: cint);
  begin
    case sig of
      SIGINT:  Console.FApp.Terminated := True;
      SIGQUIT: Console.FApp.Terminated := True;
      SIGKILL: Console.FApp.Terminated := True;
      SIGSTOP: Console.FApp.Terminated := True;
    end;
  end;
  {$ENDIF}

  begin
    SetCtrlCHandler(@CtrlHandler);
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
        Console := TConsole.Create;
        Console.Execute;
        Console.Free;
      end else
        Write(Cr + 'Error: can''t set extraction path');
    end;
  end.
