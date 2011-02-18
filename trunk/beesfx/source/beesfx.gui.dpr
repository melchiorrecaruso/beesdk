{
  Copyright (c) 2005-2011 Andrew Filinsky and Melchiorre Caruso

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

    The SFX module for Windows platform.

  Features:

    1. Uses context modelling and some variant of arithmetic encoding;
    2. Uses integer arithmetic only;
    3. Uses half-byte alphabet.

    (C) 2005-2006 Melchiorre Caruso.

  Modifyed:

    v0.1.0 build 0060 - 2006.01.05 by Melchiorre Caruso;

    v0.1.0 build 0080 - 2009.09.18 by Melchiorre Caruso.
}


program BeeSfx;

{$I compiler.inc}

uses
  Classes,
  Windows,
  SysUtils,
  Messages,


  Bee_App,
  Bee_Types,
  Bee_Common,
  Bee_BlowFish,
  Bee_Interface;

{$R main.gui.res}

{$R beesfx.ico.res}

var
  KEY:  string = '';
  PATH: string = '';

  MSG: string = '';
  PERCENTAGE: integer;

  RETURN: boolean;

  // MAIN_HW:     integer = -1;
  PROGRESS_HW: integer = -1;
  // PASSWORD_HW: integer = -1;

  MAIN_FUNC:     pointer = nil;
  PROGRESS_FUNC: pointer = nil;
  PASSWORD_FUNC: pointer = nil;

type

  { TCustomBeeApp class }

  TCustomBeeApp = class(TBeeApp)
  public
    procedure OnMessage(const aMessage: string); override;
    procedure OnRequest(const aMessage: string); override;
    function  OnRename(const aItem: THeader; const aValue: string): string; override;
    procedure OnList(const aItem: THeader); override;
    procedure OnProgress; override;
    procedure OnClear; override;
  end;

  procedure TConsole.ProcessMessage(const aMessage: string);
  begin
    MSG := aMessage;
    SendMessage(PROGRESS_HW, WM_SETTEXT, 0, 0);
  end;

  procedure TConsole.ProcessRequest(const aMessage: string);
  begin
    MessageBox(0, PChar(aMessage), PChar('Bee message'), MB_OK);
  end;

  procedure TConsole.ProcessRename(const aFileInfo: TFileInfo; var Result: string);
  begin
    (* nothing to do *)
  end;

  procedure TConsole.ProcessList(const aFileInfo: TFileInfoExtra; aVerbose: boolean);
  begin
    (* nothing to do *)
  end;

  procedure TConsole.ProcessProgress;
  begin
    PERCENTAGE := FApp.Percentes;
    SendMessage(PROGRESS_HW, WM_SETTEXT, 1, 1);
  end;

  procedure TConsole.ProcessClear;
  begin
    (* nothing to do *)
  end;

  procedure TConsole.ProcessPassword(const aFileInfo: TFileInfo; var Result: string);
  begin
    if Length(Key) < MinKeyLength then
    begin
      RETURN := False;
      DialogBox(hInstance, MAKEINTRESOURCE(200), PROGRESS_HW, PASSWORD_FUNC);
      while RETURN = False do
      begin
        (* wait password... *)
      end;
    end;
    Result := KEY;
  end;

  procedure TConsole.DoTerminate;
  begin
    SendMessage(PROGRESS_HW, WM_CLOSE, 0, 0);
    inherited DoTerminate;
  end;







  constructor TConsole.Create;
  begin
    FParams := TStringList.Create;
    FParams.Add('x');
    FParams.Add('-oA');
    FParams.Add('-R+');
    FParams.Add(ParamStr(0));
    FParams.Add('*');

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





















  /// main function

var
  Console: TConsole = nil;

  function PROGRESS_STEP(HW: hwnd; umsg: dword; wparam: wparam;
    lparam: lparam): bool; stdcall;
  begin
    PROGRESS_HW := HW;
    Result := True;
    case umsg of
      WM_CLOSE: EndDialog(HW, 0);
      WM_SETTEXT:
      begin
        if wparam = 0 then
          SetDlgItemText(HW, 301, PChar('Extracting: ' + ExtractFileName(Msg)))
        else
          SetDlgItemText(HW, 302, PChar('Total progress: ' + IntToStr(PERCENTAGE) + '%'));
      end;

      WM_COMMAND: if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: SendMessage(HW, WM_CLOSE, 0, 0);
          end;
      else
        Result := False;
    end;
  end;

  function PASSWORD_STEP(HW: hwnd; umsg: dword; wparam: wparam;
    lparam: lparam): bool; stdcall;
  begin
    // PASSWORD_HW := HW;
    Result := True;
    case umsg of
      WM_CLOSE:
      begin
        RETURN := True;
        EndDialog(HW, 0);
      end;
      WM_COMMAND: if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: SendMessage(HW, WM_CLOSE, 0, 0);
            idOk:
            begin
              SendMessage(HW, WM_CLOSE, 0, 0);

              SetLength(KEY, MAX_PATH);
              SetLength(KEY, GetDlgItemText(HW,
                202, PChar(KEY), MAX_PATH));
            end;
          end;
      else
        Result := False;
    end;
  end;

  function MAIN_STEP(HW: hwnd; umsg: dword; wparam: wparam;
    lparam: lparam): bool; stdcall;
  begin
    // MAIN_HW := HW;
    Result  := True;
    case umsg of
      WM_CLOSE: EndDialog(HW, 0);
      WM_COMMAND: if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: SendMessage(HW, WM_CLOSE, 0, 0);
            idOk: if Console = nil then
              begin
                SendMessage(HW, WM_CLOSE, 0, 0);
                SetLength(PATH, MAX_PATH);
                SetLength(PATH,
                  GetDlgItemText(HW, 104, PChar(PATH), MAX_PATH));
                Bee_Common.ForceDirectories(PATH);
                SetCurrentDir(PATH);

                Console := TConsole.Create;
                Console.Execute;

                DialogBox(hInstance, MAKEINTRESOURCE(300), 0, PROGRESS_FUNC);
              end;
          end;
      else
        Result := False;
    end;
  end;

  /// main block

begin
  MAIN_FUNC     := @MAIN_STEP;
  PROGRESS_FUNC := @PROGRESS_STEP;
  PASSWORD_FUNC := @PASSWORD_STEP;

  DialogBox(hInstance, MAKEINTRESOURCE(100), 0, MAIN_FUNC);
end.
