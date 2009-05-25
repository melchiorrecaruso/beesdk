program BeeSfx;

{ Contains:

  The SFX module for Windows platform.

  Features:

  1. Uses context modelling and some variant of arithmetic encoding;
  2. Uses integer arithmetic only;
  3. Uses half-byte alphabet.

  (C) 2005-2006 Melchiorre Caruso.

  Modifyed:

  v0.1.0 build 0060 - 2006/01/05 by Melchiorre Caruso.
}

{$R-,Q-,S-}

uses
  Classes,
  Windows,
  SysUtils,
  Messages,
  Bee_App,
  Bee_Common,
  Bee_BlowFish,
  Bee_Interface;

{$R main.res}

{$R BeeSfx.ico.res}

var
  KEY:  string = '';
  PATH: string = '';

  MSG: string = '';
  PERCENTAGE: integer;

  RETURN: boolean;

  MAIN_HW: integer = -1;
  PROGRESS_HW: integer = -1;
  PASSWORD_HW: integer = -1;

  MAIN_FUNC: pointer = nil;
  PROGRESS_FUNC: pointer = nil;
  PASSWORD_FUNC: pointer = nil;

type

  TSFX = class
  private
    App: TBeeApp;
    AppParams: TStringList;
    AppInterface: TAppInterface;
    procedure OnTerminate(Sender: TObject);
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
    procedure Run;
  end;

  constructor TSfx.Create;
  begin
    AppParams := TStringList.Create;
    AppParams.Add('x');
    AppParams.Add('-oA');
    AppParams.Add(ParamStr(0));
    AppParams.Add('*\*.*');

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
    App.OnTerminate := OnTerminate;
  end;

  destructor TSFX.Destroy;
  begin
    AppParams.Free;
    AppInterface.Free;
  end;

  procedure TSFX.Run;
  begin
    App.Resume;
  end;

  procedure TSFX.OnRename;
  begin (* nothing to do *)
  end;

  procedure TSFX.OnList;
  begin (* nothing to do *)
  end;

  procedure TSFX.OnClear;
  begin (* nothing to do *)
  end;

  procedure TSFX.OnOverWrite;
  begin (* nothing to do *)
  end;

  procedure TSFX.OnWarning;
  begin (* nothing to do *)
  end;

  procedure TSFX.OnError;
  begin
    MessageBox(0, PChar(AppInterface.cMsg), PChar('Bee message'), MB_OK);
  end;

  procedure TSFX.OnKey;
  begin
    if Length(Key) < MinKeyLength then
    begin
      RETURN := False;
      DialogBox(hInstance, MAKEINTRESOURCE(200), PROGRESS_HW, PASSWORD_FUNC);
      while RETURN = False do
      begin (* wait password... *)
      end;
    end;
    AppInterface.cMsg := KEY;
  end;

  procedure TSFX.OnDisplay;
  begin
    MSG := AppInterface.cMsg;
    SendMessage(PROGRESS_HW, WM_SETTEXT, 0, 0);
  end;

  procedure TSFX.OnTick;
  begin
    PERCENTAGE := AppInterface.cPercentage;
    SendMessage(PROGRESS_HW, WM_SETTEXT, 1, 1);
  end;

  procedure TSFX.OnTerminate;
  begin
    SendMessage(PROGRESS_HW, WM_CLOSE, 0, 0);
  end;

  /// main function

var
  Sfx: TSfx = nil;

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
          SetDlgItemText(HW, 301,
            PChar('Extracting: ' + ExtractFileName(Msg)))
        else
          SetDlgItemText(HW, 302,
            PChar('Total progress: ' + IntToStr(PERCENTAGE) + '%'));
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
    PASSWORD_HW := HW;
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
    MAIN_HW := HW;
    Result  := True;
    case umsg of
      WM_CLOSE: EndDialog(HW, 0);
      WM_COMMAND: if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: SendMessage(HW, WM_CLOSE, 0, 0);
            idOk: if Sfx = nil then
              begin
                SendMessage(HW, WM_CLOSE, 0, 0);
                SetLength(PATH, MAX_PATH);
                SetLength(PATH,
                  GetDlgItemText(HW, 104, PChar(PATH), MAX_PATH));
                Bee_Common.ForceDirectories(PATH);
                SetCurrentDir(PATH);
                Sfx := TSfx.Create;
                Sfx.Run;

                DialogBox(hInstance, MAKEINTRESOURCE(300),
                  0, PROGRESS_FUNC);
              end;
          end;
      else
        Result := False;
    end;
  end;


  /// main block

begin
  MAIN_FUNC := @MAIN_STEP;
  PROGRESS_FUNC := @PROGRESS_STEP;
  PASSWORD_FUNC := @PASSWORD_STEP;

  DialogBox(hInstance, MAKEINTRESOURCE(100), 0, MAIN_FUNC);
end.
