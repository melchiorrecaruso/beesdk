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
  Windows,
  SysUtils,

  Bee_Files,
  Bee_Common,
  Bee_Headers,
  Bee_MainPacker;

{$R main-gui.res}
{$R beesfx-ico.res}

var
  CODE: boolean = TRUE;
  PERCENTAGE: longint;
  MESSAGE:  string = '';
  PASSWORD: string = '';

  ArchReader: TFileReader;
  Decoder: THeaderDecoder;
  Headers: THeaders;

  function PROGRESS_FUNC(HW: hwnd; umsg: dword; wparam: wparam; lparam: lparam): bool; stdcall;
  begin
    Result := TRUE;
    case umsg of
      WM_CLOSE:   EndDialog(HW, 0);
      WM_SETTEXT: begin
        case wparam of
          0: SetDlgItemText(HW, 301, PChar('Extracting: ' + ExtractFileName(MESSAGE)));
          1: SetDlgItemText(HW, 302, PChar('Total progress: ' + IntToStr(PERCENTAGE) + '%'));
        end;
      end;
      WM_COMMAND: begin
        if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: SendMessage(HW, WM_CLOSE, 0, 0);
          end
      end;
      else Result := False;
    end;
  end;

  function PASSWORD_FUNC(HW: hwnd; umsg: dword; wparam: wparam; lparam: lparam): bool; stdcall;
  begin
    Result := TRUE;
    case umsg of
      WM_CLOSE:   EndDialog(HW, 0);
      WM_COMMAND: begin
        if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: begin
              SendMessage(HW, WM_CLOSE, 0, 0);
              CODE := FALSE;
            end;
            IDOk: begin
              SendMessage(HW, WM_CLOSE, 0, 0);
              SetLength(PASSWORD, MAX_PATH);
              SetLength(PASSWORD, GetDlgItemText(HW, 202, PChar(PASSWORD), MAX_PATH));
            end;
          end
      end;
      else Result := FALSE;
    end;
  end;

  function MAIN_FUNC(HW: hwnd; umsg: dword; wparam: wparam; lparam: lparam): bool; stdcall;
  begin
    Result := TRUE;
    case umsg of
      WM_CLOSE:   EndDialog(HW, 0);
      WM_COMMAND: begin
        if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: begin
              SendMessage(HW, WM_CLOSE, 0, 0);
              CODE := FALSE;
            end;
            IDOk: SendMessage(HW, WM_CLOSE, 0, 0);
          end
      end;
      else Result := FALSE;
    end;
  end;

  function UserAbort: boolean;
  begin
    Result := not Code;
  end;

/// main block ///

var
  I: longint;

begin
  Headers    := THeaders.Create(nil);
  ArchReader := CreateTFileReader(ParamStr(0), fmOpenRead + fmShareDenyWrite);
  if Assigned(ArchReader) then
  begin
    Headers.Read(ArchReader);
    if Headers.Count > 0 then
    begin
      DialogBox(hInstance, MAKEINTRESOURCE(100), 0, @MAIN_FUNC);
      if CODE then
      begin
        if Headers.GetNext(0, foPassword) <> -1 then
          DialogBox(hInstance, MAKEINTRESOURCE(200), 0, @PASSWORD_FUNC);

        if CODE then
        begin
          DialogBox(hInstance, MAKEINTRESOURCE(300), 0, @PROGRESS_FUNC);
          Decoder := THeaderDecoder.Create(ArchReader);
          Decoder.Password := PASSWORD;

          for I := 0 to Headers.Count - 1 do
            if CODE then
            begin
              Decoder.Initialize(Headers.Items[I]);
              CODE := Decoder.ReadToFile(Headers.Items[I]);
            end;
          Decoder.Free;

          if CODE = FALSE then MessageBox(0, PChar('CRC fatal error'), PChar('BeeSFX message'), MB_OK);
        end;
      end;
    end;
    ArchReader.Free;
  end;
  Headers.Destroy;
end.
