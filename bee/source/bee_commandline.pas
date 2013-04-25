{
  Copyright (c) 2013 Melchiorre Caruso.

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

    TCommandLine class.

  Modifyed:

    v0.8.0 build 1864 - 2013.02.15 by Melchiorre Caruso.
}

unit Bee_CommandLine;

{$I bee_compiler.inc}

interface

uses
  Classes,
  SysUtils,
  Bee_Common;

type
  // Command:
  //   cAdd      Add files
  //   cDelete   Delete files
  //   cExtract  Extract file
  //   cHelp     Show Help
  //   cList     List files
  //   cRename   Rename files
  //   cTest     Test files
  //   cXextract Extract file with full path

  TCommand = (cAdd, cDelete, cExtract, cHelp, cList, cRename, cTest, cXextract);

  // Update Method:
  //   umAdd           Add only new files
  //   umUpdate        Update only existing files
  //   umReplace       Replace only existing files
  //   umQuery         Query always
  //   umAddUpdate     Add and update existing files
  //   umAddReplace    Add and replace existing files
  //   umAddQuery      Add and query if already exists
  //   umAddAutoRename Add and rename if already exists

  TUpdateMethod = (umAdd, umUpdate, umReplace, umQuery, umAddUpdate,
    umAddReplace, umAddQuery, umAddAutoRename);

  // Process Priority:
  //   ppIdle
  //   ppNormal
  //   ppHigh
  //   ppRealTime

  TProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime);

  // TCommandLineOptions

  TCommandLineOption = (
    clcOption, clcdOption,  clcpOption,  clckpOption, clepOption, clppOption,
    clrOption, clsfxOption, clslsOption, clssOption,  cltOption,  cluOption,
    clvOption, clwdOption,  clxOption,   clyOption);

  TCommandLineOptions = set of TCommandLineOption;

  // TCommandLine class

  TCommandLine = class
  protected
    FCommand: TCommand;
    FOptions: TCommandLineOptions;
    FacOption: string;
    FcdOption: string;
    FcpOption: string;
    FckpOption: string;
    FepOption: string;
    FppOption: TProcessPriority;
    FrOption: TRecursiveMethod;
    FsfxOption: string;
    FslsOption: boolean;
    FssOption: boolean;
    FtOption: boolean;
    FuOption: TUpdateMethod;
    FvOption: qword;
    FwdOption: string;
    FxOptions: TStringList;
    FyOption: boolean;
    FArchiveName: string;
    FFileMasks: TStringList;
    procedure ProcessCommand(const S: string);
    procedure ProcessACOption (var S: string);
    procedure ProcessCDOption (var S: string);
    procedure ProcessCKPOption(var S: string);
    procedure ProcessCPOption (var S: string);
    procedure ProcessEPOption (var S: string);
    procedure ProcessPPOption (var S: string);
    procedure ProcessROption  (var S: string);
    procedure ProcessSFXOption(var S: string);
    procedure ProcessSLSOption(var S: string);
    procedure ProcessSSOption (var S: string);
    procedure ProcessTOption  (var S: string);
    procedure ProcessUOption  (var S: string);
    procedure ProcessVOption  (var S: string);
    procedure ProcessWDOption (var S: string);
    procedure ProcessXOption  (var S: string);
    procedure ProcessYOption  (var S: string);
    procedure ProcessArchiveName(var S: string);
    procedure ProcessFileMasks(const S: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  public
    property Command: TCommand read FCommand;
    property Options: TCommandLineOptions read FOptions;
    property acOption: string read FacOption;
    property cdOption: string read FcdOption;
    property cpOption: string read FcpOption;
    property ckpOption: string read FckpOption;
    property epOption: string read FepOption;
    property ppOption: TProcessPriority read FppOption;
    property rOption: TRecursiveMethod read FrOption;
    property sfxOption: string read FsfxOption;
    property slsOption: boolean read FslsOption;
    property ssOption: boolean read FssOption;
    property tOption: boolean read FtOption;
    property uOption: TUpdateMethod read FuOption;
    property vOption: qword read FvOption;
    property wdOption: string read FwdOption;
    property xOptions: TStringList read FxOptions;
    property yOption: boolean read FyOption;
    property ArchiveName: string read FArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

  function GetUpdateMethod(const S: string): longint;
  function TryStrWithMultToQWord(var S: string; out Q : qword) : boolean;

implementation

uses
  Math,
  Bee_BlowFish,
  Bee_Interface;

function GetUpdateMethod(const S: string): longint;
begin
  if UpCase(S) = 'ADD'            then Result := 0 else
  if UpCase(S) = 'UPDATE'         then Result := 1 else
  if UpCase(S) = 'REPLACE'        then Result := 2 else
  if UpCase(S) = 'QUERY'          then Result := 3 else
  if UpCase(S) = 'ADD:UPDATE'     then Result := 4 else
  if UpCase(S) = 'ADD:REPLACE'    then Result := 5 else
  if UpCase(S) = 'ADD:QUERY'      then Result := 6 else
  if UpCase(S) = 'ADD:AUTORENAME' then Result := 7 else Result := -1;
end;

function TryStrWithMultToQWord(var S: string; out Q : qword) : boolean;
var
  I: longint;
  J: extended;
  Multiple: qword;
begin
  I := Pos('B', UpCase(S));
  if (I > 0) and (I = Length(S)) then
  begin
    SetLength(S, I - 1);
  end;

  I := Pos('K', UpCase(S));
  if (I > 0) and (I = Length(S)) then
  begin
    SetLength(S, I - 1);
    Multiple := 1024;
  end else
  begin
    I := Pos('M', UpCase(S));
    if (I > 0) and (I = Length(S)) then
    begin
      SetLength(S, I - 1);
      Multiple := 1024*1024;
    end else
    begin
      I := Pos('G', UpCase(S));
      if (I > 0) and (I = Length(S)) then
      begin
        SetLength(S, I - 1);
        Multiple := 1024*1024*1024;
      end else
      begin
        I := Pos('T', UpCase(S));
        if (I > 0) and (I = Length(S)) then
        begin
          SetLength(S, I - 1);
          Multiple := 1024*1024*1024*1024;
        end else
          Multiple := 1;
      end;
    end;
  end;

  Result := TryStrToFloat(S, J);
  if Result then
  begin
    Q := Round(J * Multiple);
  end;
end;

constructor TCommandLine.Create;
begin
  inherited Create;
  FCommand     := cHelp;
  FOptions     := [];
  FxOptions    := TStringList.Create;
  FArchiveName := '';
  FFileMasks   := TStringList.Create;
end;

destructor TCommandLine.Destroy;
begin
  FxOptions.Destroy;
  FFileMasks.Destroy;
  inherited Destroy;
end;

procedure TCommandLine.ProcessCommand(const S: string);
begin
  if Length(S) = 1 then
    case Upcase(S[1]) of
      'A': FCommand := cAdd;
      'D': FCommand := cDelete;
      'E': FCommand := cExtract;
      'H': FCommand := cHelp;
      'L': FCommand := cList;
      'R': FCommand := cRename;
      'T': FCommand := cTest;
      'X': FCommand := cXextract;
      else SetExitStatus(esCmdLineError);
    end
  else SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessACOption(var S: string);
begin
  Delete(S, 1, 3);
  FacOption := S;

  if Command in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clcOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCDOption(var S: string);
begin
  Delete(S, 1, 3);
  if Length(S) > 0 then
    FcdOption := IncludeTrailingBackSlash(S)
  else
    FcdOption := '';

  if Command in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clcdOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCPOption(var S: string);
begin
  Delete(S, 1, 3);
  FcpOption := FcpOption + ':' + S + ':';
  while Pos('::', FcpOption) > 0 do
    Delete(FcpOption, Pos('::', FcpOption), 1);

  if Command in [cAdd] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clcpOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCKPOption(var S: string);
begin
  Delete(S, 1, 4);
  FckpOption := FckpOption + ':' + S + ':';
  while Pos('::', FckpOption) > 0 do
    Delete(FckpOption, Pos('::', FckpOption), 1);

  if Command in [cAdd] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clckpOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessEPOption(var S: string);
begin
  Delete(S, 1, 3);
  FepOption := FepOption + ':' + S + ':';
  while Pos('::', FepOption) > 0 do
    Delete(FepOption, Pos('::', FepOption), 1);

  if Command in [cAdd, cDelete, cExtract, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clepOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessPPOption(var S: string);
begin
  Delete(S, 1, 3);
  if Length(S) = 1 then
    case S[1] of
      '0': FppOption := ppIdle;
      '1': FppOption := ppNormal;
      '2': FppOption := ppHigh;
      '3': FppOption := ppRealTime;
      else SetExitStatus(esCmdLineError);
    end
  else SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clppOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessROption(var S: string);
begin
  Delete(S, 1, 2);
  if Length(S) = 1 then
    case UpCase(S[1]) of
      '-': FrOption := rmNone;
      'W': FrOption := rmWildCard;
      else SetExitStatus(esCmdLineError);
    end
  else
    if S = '' then
      FrOption := rmFull
    else
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clrOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessSFXOption(var S: string);
begin
  Delete(S, 1, 4);
  FsfxOption := S;
  if FCommand = cAdd then
    if FileExists(FsfxOption) = FALSE then
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clsfxOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessSLSOption(var S: string);
begin
  Delete(S, 1, 4);
  if S = '' then
    FslsOption := TRUE
  else
    SetExitStatus(esCmdLineError);

  if FCommand in [cList] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clslsOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessSSOption(var S: string);
begin
  Delete(S, 1, 3);
  if S = '' then
    FssOption := TRUE
  else
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clssOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessTOption(var S: string);
begin
  Delete(S, 1, 2);
  if S = '' then
    FtOption := TRUE
  else
    SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, cltOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessUOption(var S: string);
var
  I: longint;
begin
  Delete(S, 1, 2);
  I := GetUpdateMethod(S);
  if I <> -1 then
    FuOption := TUpdateMethod(I)
  else
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cExtract, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, cluOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessVOption(var S: string);
var
  I: longint;
begin
  Delete(S, 1, 2);
  for I := 1 to Length(S) do
    if S[I] in ['.', ','] then
      S[I] := DecimalSeparator;

  if TryStrWithMultToQWord(S, FvOption) = FALSE then
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clvOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessWDOption(var S: string);
begin
  Delete(S, 1, 3);
  FwdOption := ExcludeTrailingBackslash(S);
  if FwdOption = '' then
    FwdOption := ExcludeTrailingBackSlash(GetTempDir)
  else
    if DirectoryExists(FwdOption) = FALSE then
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clwdOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessXOption(var S: string);
begin
  Delete(S, 1, 2);
  if Length(S) > 0 then
    FxOptions.Add(S)
  else
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clxOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessYOption(var S: string);
begin
  Delete(S, 1, 2);
  if S = '' then
    FyOption := TRUE
  else
    SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clyOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessArchiveName(var S: string);
begin
  FssOption := TRUE;
  FArchiveName := S;
  if FileExists(FArchiveName) = FALSE then
    if ExtractFileExt(FArchiveName) = '' then
      FArchiveName := ChangeFileExt(FArchiveName, '.beex');

  if FCommand in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    // check if archive exists
    if FCommand in [cDelete, cExtract, cList, cRename, cTest, cXextract] then
      if FileExists(FArchiveName) = FALSE then
        SetExitStatus(esCmdLineError);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessFileMasks(const S: string);
begin
  FFileMasks.Add(S);
  if FCommand in [cHelp] then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.Execute;
var
  I: longint;
  S: string;
begin
  // catch command
  if ParamCount > 0 then ProcessCommand(ParamStr(1));
  // catch options, archive name and name of files
  if ParamCount > 1 then
    for I := 2 to ParamCount do
    begin
      S := ParamStr(I);
      if (not FssOption) and (Length(S) > 1) and (S[1] = '-') then
      begin
        // options...
        if Pos('-AC', UpperCase(S)) = 1 then
          ProcessACOption(S)
        else
        if Pos('-CD', UpperCase(S)) = 1 then
          ProcessCDOption(S)
        else
        if Pos('-CKP', UpperCase(S)) = 1 then
          ProcessCKPOption(S)
        else
        if Pos('-CP', UpperCase(S)) = 1 then
          ProcessCPOption(S)
        else
        if Pos('-EP', UpperCase(S)) = 1 then
          ProcessEPOption(S)
        else
        if Pos('-PP', UpperCase(S)) = 1 then
          ProcessPPOption(S)
        else
        if Pos('-R', UpperCase(S)) = 1 then
          ProcessROption(S)
        else
        if Pos('-SFX', UpperCase(S)) = 1 then
          ProcessSFXOption(S)
        else
        if Pos('-SLS', UpperCase(S)) = 1 then
          ProcessSLSOption(S)
        else
        if Pos('-SS', UpperCase(S)) = 1 then
          ProcessSSOption(S)
        else
        if Pos('-T', UpperCase(S)) = 1 then
          ProcessTOption(S)
        else
        if Pos('-U', UpperCase(S)) = 1 then
          ProcessUOption(S)
        else
        if Pos('-V', UpperCase(S)) = 1 then
          ProcessVOption(S)
        else
        if Pos('-WD', UpperCase(S)) = 1 then
          ProcessWDOption(S)
        else
        if Pos('-X', UpperCase(S)) = 1 then
          ProcessXOption(S)
        else
        if Pos('-Y', UpperCase(S)) = 1 then
          ProcessYOption(S)
        else
          SetExitStatus(esCmdLineError);
      end else

        if FArchiveName = '' then
          ProcessArchiveName(S)
        else
          ProcessFileMasks(S);

    end; // end for loop

  // check file masks
  if FFileMasks.Count = 0 then
  begin
    if FCommand in [cAdd, cDelete, cRename] then
      SetExitStatus(esCmdLineError)
    else
      if FCommand in [cExtract, cList, cTest, cXextract] then
      begin
        FFileMasks.Add('*');
        FrOption := rmWildCard;
        Include(FOptions, clrOption);
      end;
  end;
end;

end.

