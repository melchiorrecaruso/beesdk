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
  PASSWORD: string = '';
  EXTRACTIONPATH: string = '';
  OVERWRITEALL: boolean = FALSE;

  LASTMESSAGE: string = '';
  LASTPERCENTAGE: longint = 0;

  MAIN_FUNC:     pointer = nil;
  PROGRESS_FUNC: pointer = nil;

  MAIN_HW:       integer = -1;
  PROGRESS_HW:   integer = -1;

  ArchReader: TFileReader;
  Decoder: THeaderDecoder;
  Headers: THeaders;

  procedure Execute;
  var
    I: longint;
    P: THeader;
    CODE: boolean;
  begin
    Headers   := THeaders.Create(nil);
    ArchReader := CreateTFileReader(ParamStr(0), fmOpenRead + fmShareDenyWrite);
    if Assigned(ArchReader) then
    begin
      Headers.Read(ArchReader);
      Decoder := THeaderDecoder.Create(ArchReader);
      Decoder.Password := PASSWORD;

      CODE := TRUE;
      for I := 0 to Headers.Count - 1 do
        if CODE then
        begin
          P := Headers.Items[I];
          Decoder.Initialize(P);
          if OVERWRITEALL or (FileExists(P.Name) = FALSE) then
          begin
            CODE := Decoder.Read(P);
          end;

          if CODE = FALSE then
          begin
            MessageBox(0, PChar('CRC Error'), PChar('BeeSFX message'), MB_OK);
          end;
        end;
      Decoder.Destroy;
      ArchReader.Destroy;
    end;
    Headers.Destroy;
  end;

  /// main function

  function PROGRESS_STEP(HW: hwnd; umsg: dword; wparam: wparam; lparam: lparam): bool; stdcall;
  begin
    Result := True;
    PROGRESS_HW := HW;
    case umsg of
      WM_CLOSE: EndDialog(HW, 0);
      WM_SETTEXT:
      begin
        if wparam = 0 then
          SetDlgItemText(HW, 301, PChar('Extracting: ' + ExtractFileName(LASTMESSAGE)))
        else
          SetDlgItemText(HW, 302, PChar('Total progress: ' + IntToStr(LASTPERCENTAGE) + '%'));
      end;
      WM_COMMAND:
      begin
        if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: SendMessage(HW, WM_CLOSE, 0, 0);
          end
        else
          Result := False;
      end;
    end;
  end;

  function MAIN_STEP(HW: hwnd; umsg: dword; wparam: wparam; lparam: lparam): bool; stdcall;
  begin
    Result  := True;
    MAIN_HW := HW;
    case umsg of
      WM_CLOSE:   EndDialog(HW, 0);
      WM_COMMAND: if hiword(wparam) = BN_CLICKED then
          case loword(wparam) of
            IDCLOSE: SendMessage(HW, WM_CLOSE, 0, 0);
            IDOk:
            begin
              SendMessage(HW, WM_CLOSE, 0, 0);
              SetLength(EXTRACTIONPATH, MAX_PATH);
              SetLength(EXTRACTIONPATH, GetDlgItemText(HW, 104, PChar(EXTRACTIONPATH), MAX_PATH));
              Bee_Common.ForceDirectories(EXTRACTIONPATH);
              if SetCurrentDir(EXTRACTIONPATH) then
              begin

                Execute;

                DialogBox(hInstance, MAKEINTRESOURCE(300), 0, PROGRESS_FUNC);
              end;
            end;
          end;
      else Result := False;
    end;
  end;

  /// main block

begin
  MAIN_FUNC     := @MAIN_STEP;
  PROGRESS_FUNC := @PROGRESS_STEP;

  DialogBox(hInstance, MAKEINTRESOURCE(100), 0, MAIN_FUNC);
end.
