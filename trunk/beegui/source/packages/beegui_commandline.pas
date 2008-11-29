{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

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

    BeeGui CmdLine class.

  Modifyed:
}

unit BeeGui_CommandLine;

{$I compiler.inc}

interface

uses
  Forms,
  Dialogs,
  Classes,
  Controls,
  SysUtils,
  // ---
  Bee_CommandLine;

type
   
  { TCustomCommandLine Application class }

  TCustomCommandLine = class (TCommandLine)
  private
    F0Option: string;
    F1Option: boolean;
    F2Option: boolean;
    FParams: TStringList;
  private
    function GetRun: boolean;
    function GetParams: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process(AParams: TStringList); override;
  public
    property Run: boolean read GetRun;
    property Link: string read F0Option;
    property Log: boolean read F1Option write F1Option;
    property Params: TStringList read GetParams;
  public

  end;

implementation

uses
  Bee_Common,
  // ---
  BeeGui_Forms;

  constructor TCustomCommandLine.Create;
  var
    I: integer;
  begin
    inherited Create;
    F0Option := '';
    F1Option := False;
    F2Option := False;
    FParams  := TStringList.Create;

    for I := 1 to ParamCount do
    begin
      FParams.Add(ParamStr(I));
    end;
    Process(FParams);
  end;

  procedure TCustomCommandLine.Process(AParams: TStringList);
  var
    S: string;
    I: integer;
    K: integer;
  begin
    K := 0;
    // catch options, command, archive name and name of files
    for I := 1 to Params.Count -1 do
    begin
      S := Params.Strings[I];
      if (K < 2) and (Length(S) > 1) and (S[1] = '-') then
      begin
        case UpCase(S[2]) of
          '0': begin
                 Delete(S, 1, 2);
                 if Length(S) > 0 then
                   F0Option := FixFileName(S);
               end;
          '1': ProcessOption(S, F1Option);
          '2': ProcessOption(S, F2Option);
        end;
      end else
      begin
        Inc(K);
      end;
    end; // end for loop
    inherited Process(Params);
  end;

  destructor TCustomCommandLine.Destroy;
  begin
    FParams.Clear;
    FParams.Destroy;
    inherited Destroy;
  end;

  function TCustomCommandLine.GetParams: TStringList;
  begin
    Result := FParams;
  end;

  function TCustomCommandLine.GetRun: boolean;
  var
    i: integer;
  begin
    Result := True;

    FParams.Clear;
    if F2Option then
    begin
      case Command of
        'A': Result := ConfirmAdd(Self);
        'E': Result := ConfirmExtract(Self);
        'X': Result := ConfirmExtract(Self);
      end;
    end;

    if Result then
    begin
      FParams.Add(Command);

      if rOption then FParams.Add('-R+') else FParams.Add('-R-');
      if uOption then FParams.Add('-U+') else FParams.Add('-U-');
      if fOption then FParams.Add('-F+') else FParams.Add('-F-');

      if Length(eOption) > 0 then
        FParams.Add('-E' + eOption);

      if sOption then FParams.Add('-S+') else FParams.Add('-S-');

      if Length(aOption) > 0 then
        FParams.Add('-A' + aOption);

      FParams.Add('-O'+ oOption);
      FParams.Add('-M'+ IntToStr(mOption));
      FParams.Add('-D'+ IntToStr(dOption));

      for i := 0 to xOption.Count - 1 do
        FParams.Add('-X' + xOption.Strings[i]);

      if tOption then FParams.Add('-T+') else FParams.Add('-T-');
      if lOption then FParams.Add('-L+') else FParams.Add('-L-');

      if Length(yOption) > 0 then
        FParams.Add('-Y' + yOption);

      if kOption then FParams.Add('-K+') else FParams.Add('-K-');

      if Length(cdOption) > 0 then
        FParams.Add('-CD' + cdOption);

      if Length(cfgOption) > 0 then
        FParams.Add('-CFG' +  cfgOption);

      FParams.Add('-PRI' + IntTostr(priOption));
      FParams.Add(ArchiveName);

      for i := 0 to FileMasks.Count - 1 do
        FParams.Add(FileMasks.Strings[i]);
    end;
  end;

end.

