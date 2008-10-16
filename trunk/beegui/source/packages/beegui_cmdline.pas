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

unit BeeGui_CmdLine;

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
    FRun: boolean;
    F0Option: string;
    F1Option: boolean;
    F2Option: boolean;
    FParams: TStringList;
  private
    function GetParams: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Process(Params: TStringList); override;
    procedure Clear; override;
  public
    property Run: boolean read FRun;
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
  begin
    inherited Create;
    Clear;
  end;

  procedure TCustomCommandLine.Clear;
  begin
    FRun := False;
    F0Option := '';
    F1Option := False;
    F2Option := False;
  end;

  procedure TCustomCommandLine.Process(Params: TStringList);
  var
    S: string;
    i: integer;
  begin
    // catch options, command, archive name and name of files
    for i := 1 to ParamCount do
    begin

    end; // end for loop
  end;

  destructor TCustomCommandLine.Destroy;
  begin
    inherited Destroy;
  end;

  function TCustomCommandLine.GetParams: TStringList;
  begin
    Result := FParams;
  end;

  (*
  procedure TCustomCommandLine.ProcessParams;
  var
    i: integer;
  begin
    FRun := True;
    FParams.Clear;
    if F2Option then
    begin
      case FCommand of
        'A': FRun := ConfirmAdd(Self);
        'E': FRun := ConfirmExtract(Self);
        'X': FRun := ConfirmExtract(Self);
      end;
    end;

    if FRun then
    begin
      FParams.Add(FCommand);

      if FrOption then FParams.Add('-R+') else FParams.Add('-R-');
      if FuOption then FParams.Add('-U+') else FParams.Add('-U-');
      if FfOption then FParams.Add('-F+') else FParams.Add('-F-');

      if Length(FeOption) > 0 then
        FParams.Add('-E' + FeOption);

      if FsOption then FParams.Add('-S+') else FParams.Add('-S-');

      if Length(FaOption) > 0 then
        FParams.Add('-A' + FaOption);

      FParams.Add('-O'+ FoOption);
      FParams.Add('-M'+ IntToStr(FmOption));
      FParams.Add('-D'+ IntToStr(FdOption));

      for i := 0 to FxOption.Count - 1 do
        FParams.Add('-X' + FxOption.Strings[i]);

      if FtOption then FParams.Add('-T+') else FParams.Add('-T-');
      if FlOption then FParams.Add('-L+') else FParams.Add('-L-');

      if Length(FyOption) > 0 then
        FParams.Add('-Y' + FyOption);

      if FkOption then FParams.Add('-K+') else FParams.Add('-K-');

      if Length(FcdOption) > 0 then
        FParams.Add('-CD' + FcdOption);

      if Length(FcfgOption) > 0 then
        FParams.Add('-CFG' +  FcfgOption);

      FParams.Add('-PRI' + IntTostr(FpriOption));
      FParams.Add(FArcName);

      for i := 0 to FFileMasks.Count - 1 do
        FParams.Add(FFileMasks.Strings[i]);
    end;
  end;
  *)



end.

