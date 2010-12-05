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

    BeeGui command line class.

  Modifyed:

  v1.0.5 build 559 - 2008.12.22 by Melchiorre Caruso.
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

  TCustomCommandLine = class(TCommandLine)
  protected
    F1Option: boolean;
    F2Option: boolean;
    function GetRun: boolean;
    function GetCommandLine: string; override;
    procedure SetCommandLine(const aValue: string); override;
    procedure SetF1Option(Value: boolean);
    procedure SetF2Option(Value: boolean);
  public
    constructor Create(UseParams: boolean);
    destructor Destroy; override;
    procedure Clear;
    property Run: boolean Read GetRun;
    property Log: boolean Read F1Option Write SetF1Option;
    property Confirm: boolean Read F2Option Write SetF2Option;
    property CommandLine: string read GetCommandLine write SetCommandLine;
  public

  end;

implementation

uses
  Bee_Types,
  BeeGui_Forms;

constructor TCustomCommandLine.Create(UseParams: boolean);
var
  I: integer;
  Params: TStringList;
begin
  inherited Create;
  F1Option := False;
  F2Option := False;
  if UseParams then
  begin
    Params := TStringList.Create;
    for I := 1 to ParamCount do
    begin
      Params.Add(ParamStr(I));
    end;
    SetCommandLine(Params.Text);
    Params.Destroy;
  end;
end;

procedure TCustomCommandLine.Clear;
begin
  inherited Clear;
  F1Option := False;
  F2Option := False;
end;

procedure TCustomCommandLine.SetCommandLine(const aValue: string);
var
  S:    string;
  I, J: integer;
  Params: TStringList;
begin
  inherited SetCommandLine(aValue);
  Params := TStringList.Create;
  Params.Text := aValue;

  FssOption := False;
  for I := 1 to Params.Count - 1 do
  begin
    S := Params.Strings[I];
    if (not FssOption) and (Length(S) <> 1) and (S[1] = '-') then
      case UpCase(S[2]) of
        '-': ProcessOption(S, FssOption);
        '1': ProcessOption(S, F1Option);
        '2': ProcessOption(S, F2Option);
      end;
  end;
  Params.Destroy;
end;

destructor TCustomCommandLine.Destroy;
begin
  inherited Destroy;
end;

function TCustomCommandLine.GetCommandLine: string;
var
  S: string;
  C: Char;
begin
  S := inherited GetCommandLine;

  Result := S[1];
  if F1Option then
    Result := Result + ' -F1+'
  else
    Result := Result + ' -F1-';

  if F2Option then
    Result := Result + ' -F2+'
  else
    Result := Result + ' -F2-';

  Delete(S, 1, 1);
  Result := Result + S;
end;

function TCustomCommandLine.GetRun: boolean;
var
  i: integer;
begin
  Result := True;
  if F2Option then
  begin
    case Command of
      ccAdd:      Result := ConfirmAdd(Self);
      ccExtract:  Result := ConfirmExtract(Self);
      ccXextract: Result := ConfirmExtract(Self);
    end;
  end;
end;

procedure TCustomCommandLine.SetF1Option(Value: boolean);
begin
  F1Option := Value;
end;

procedure TCustomCommandLine.SetF2Option(Value: boolean);
begin
  F2Option := Value;
end;

end.
