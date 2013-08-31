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

    v0.8.0 build 2031 - 2013.08.22 by Melchiorre Caruso.
}

unit Bee_CommandLine;

{$I bee_compiler.inc}

interface

uses
  Classes;

type
  // Command:
  //   cAdd       Add files
  //   cDelete    Delete files
  //   cExtract   Extract file
  //   cHelp      Show Help
  //   cList      List files
  //   cQuickTest Quick test files
  //   cRename    Rename files
  //   cTest      Test files
  //   cXextract  Extract files with full paths

  TCommand = (
    cAdd,
    cDelete,
    cExtract,
    cHelp,
    cList,
    cQuickTest,
    cRename,
    cTest,
    cXextract);

  // Update Method:
  //   umAdd           Add only new files
  //   umUpdate        Update only existing files
  //   umReplace       Replace only existing files
  //   umQuery         Query always
  //   umAddUpdate     Add and update existing files
  //   umAddReplace    Add and replace existing files
  //   umAddQuery      Add and query if already exists
  //   umAddAutoRename Add and rename if already exists

  TUpdateMethod = (
    umAdd,
    umUpdate,
    umReplace,
    umQuery,
    umAddUpdate,
    umAddReplace,
    umAddQuery,
    umAddAutoRename);

  // TCommandLineOptions

  TCommandLineOption = (
    claccOption,
    clbOption,
    clcOption,
    clccOption,
    clcdOption,
    clciOption,
    cleOption,
    cllOption,
    clpOption,
    clrOption,
    clrxOption,
    clsfxOption,
    clslOption,
    clssOption,
    cltOption,
    cluOption,
    clvOption,
    clvbOption,
    clwOption,
    clxOption,
    clyOption);

  TCommandLineOptions = set of TCommandLineOption;

  // TCommandLine class

  TCommandLine = class
  protected
    FCommand: TCommand;
    FOptions: TCommandLineOptions;
    FaccOption: string;
    FbOption: boolean;
    FcOption: string;
    FccOption: string;
    FcdOption: string;
    FciOption: string;
    FeOption: string;
    FlOption: longint;
    FpOption: string;
    FrOption: string;
    FrxOption: string;
    FsfxOption: string;
    FslOption: boolean;
    FssOption: boolean;
    FtOption: boolean;
    FuOption: TUpdateMethod;
    FvOption: qword;
    FvbOption: boolean;
    FwOption: string;
    FxOptions: TStringList;
    FyOption: boolean;
    FArchiveName: string;
    FFileMasks: TStringList;
    function  GetrOption (Index: longint): boolean;
    function  GetrxOption(Index: longint): boolean;
    procedure ProcessCommand(const S: string);
    procedure ProcessACCOption(var S: string);
    procedure ProcessBOption(var S: string);
    procedure ProcessCOption(var S: string);
    procedure ProcessCCOption(var S: string);
    procedure ProcessCDOption(var S: string);
    procedure ProcessCIOption(var S: string);
    procedure ProcessEOption(var S: string);
    procedure ProcessIOption(var S: string);
    procedure ProcessLOption(var S: string);
    procedure ProcessPOption(var S: string);
    procedure ProcessROption(var S: string);
    procedure ProcessRXOption(var S: string);
    procedure ProcessSFXOption(var S: string);
    procedure ProcessSLOption(var S: string);
    procedure ProcessSSOption(var S: string);
    procedure ProcessTOption(var S: string);
    procedure ProcessUOption(var S: string);
    procedure ProcessVOption(var S: string);
    procedure ProcessVBOption(var S: string);
    procedure ProcessWOption(var S: string);
    procedure ProcessXOption(var S: string);
    procedure ProcessYOption(var S: string);
    procedure ProcessArchiveName(var S: string);
    procedure ProcessFileMasks(const S: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property Command: TCommand read FCommand;
    property Options: TCommandLineOptions read FOptions;
    property accOption: string read FaccOption;
    property bOption: boolean read FbOption;
    property cOption: string read FcOption;
    property ccOption: string read FccOption;
    property cdOption: string read FcdOption;
    property ciOption: string read FciOption;
    property eOption: string read FeOption;
    property lOption: longint read FlOption;
    property pOption: string read FpOption;
    property rOption[Index: longint]: boolean read GetrOption;
    property rxOption[Index: longint]: boolean read GetrxOption;
    property sfxOption: string read FsfxOption;
    property slOption: boolean read FslOption;
    property ssOption: boolean read FssOption;
    property tOption: boolean read FtOption;
    property uOption: TUpdateMethod read FuOption;
    property vOption: qword read FvOption;
    property vbOption: boolean read FvbOption;
    property wOption: string read FwOption;
    property xOptions: TStringList read FxOptions;
    property yOption: boolean read FyOption;
    property ArchiveName: string read FArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

  function TryStrWithMultToQWord(var S: string; out Q : qword) : boolean;
  function ExtractQWord(const Params: string; const K: string): qword;
  function ExtractDWord(const Params: string; const K: string): dword;
  function ExtractStr(const Params: string; const K: string): string;
  function ExtractCompressionMethod(const Params: string): longword;
  function ExtractCompressionLevel(const Params: string): longword;
  function ExtractCompressionAuxLevel(Params: string): longword;
  function ExtractCompressionFilter(const Params: string): string;
  function ExtractCompressionAuxFilter(const Params: string): string;
  function ExtractCompressionBlock(Params: string): qword;
  function ExtractCompressionConfig(const Params: string): string;
  function ExtractEncryptionMethod(const Params: string): longword;
  function ExtractEncryptionPassword(const Params: string): string;
  function ExtractCheckMethod(Params: string): longword;
  function ExtractCheckAuxMethod(Params: string): longword;

implementation

uses
  Math,
  SysUtils,
  Bee_Common,
  Bee_Interface;

function TryStrWithMultToQWord(var S: string; out Q: qword) : boolean;
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

function ExtractQWord(const Params: string; const K: string): qword;
var
  I: longint;
  S: string;
begin
  S := '';
  if Pos(K, UpCase(Params)) > 0 then
  begin
    for I := Pos(K, UpCase(Params)) + Length(K) to Length(Params) do
      if Params[I] <> '/' then
        S := S + Params[I]
      else
        Break;
  end;
  if TryStrWithMultToQWord(S, Result) = FALSE then Result := 0;
end;

function ExtractDWord(const Params: string; const K: string): dword;
begin
  Result := ExtractQWord(Params, K);
end;

function ExtractStr(const Params: string; const K: string): string;
var
  I: longint;
begin
  Result := '';
  if Pos(K, UpCase(Params)) > 0 then
  begin
    for I := Pos(K, UpCase(Params)) + Length(K) to Length(Params) do
      if Params[I] <> '/' then
        Result := Result + Params[I]
      else
        Break;
  end;
end;

function ExtractStrFromFile(const FileName: string): string;
var
  T: TStringList;
begin
  T := TStringList.Create;
  T.LoadFromFile(FileName);
  Result := T.Text;
  T.Free;
end;

function ExtractCompressionMethod(const Params: string): longword;
var
  S: string;
begin
  Result := 1;
  if Pos('/M', UpCase(Params)) > 0 then
  begin
    S := UpCase(ExtractStr(Params,'/M'));
    if S = '0' then Result := 0 else
    if S = '1' then Result := 1 else
    if S = '2' then Result := 2 else SetExitStatus(esCmdLineError);
  end;
end;

function ExtractCompressionLevel(const Params: string): longword;
begin
  case ExtractCompressionMethod(Params) of
    1:
    begin
      Result := 1;
      if Pos('/L', UpCase(Params)) > 0 then
        Result := ExtractQWord(Params, '/L');

      if (3 < Result) or (Result < 1) then
        SetExitStatus(esCmdLineError);
    end;
    2:
    begin
      Result := 6;
      if Pos('/L', UpCase(Params)) > 0 then
        Result := ExtractQWord(Params, '/L');

      if (64 < Result) or (Result < 2) then
        SetExitStatus(esCmdLineError);
    end;
    else SetExitStatus(esCmdLineError);
  end;
end;

function ExtractCompressionAuxLevel(Params: string): longword;
var
  q: qword;
begin
  case ExtractCompressionMethod(Params) of
    1:
    begin
      Result := 3;
      if Pos('/AL', UpCase(Params)) > 0 then
        Result := ExtractQWord(Params, '/AL');

      if (9 < Result) or (Result < 0) then
        SetExitStatus(esCmdLineError);
    end;
    2:
    begin
      Result := $200000;
      if Pos('/AL', UpCase(Params)) > 0 then
        if TryStrWithMultToQWord(ExtractStr(Params, '/AL'), q) = FALSE then
           SetExitStatus(esCmdLineError);

      if ($800 < Result) or (Result < $FFFFFFDB) then
        SetExitStatus(esCmdLineError);
    end;
    else SetExitStatus(esCmdLineError);
  end;
end;

function ExtractCompressionFilter(const Params: string): string;
begin
  Result := ExtractStr(Params, '/F');
end;

function ExtractCompressionAuxFilter(const Params: string): string;
begin
  Result := ExtractStr(Params, '/AF');
end;

function ExtractCompressionBlock(Params: string): qword;
begin
  Result := ExtractQWord(Params, '/B');
end;

function ExtractCompressionConfig(const Params: string): string;
begin
  Result := ExtractStr(Params, '/C');
  if Result = '' then
    Result := SelfPath + DefaultCfgName;
end;

function ExtractEncryptionMethod(const Params: string): longword;
var
  S: string;
begin
  Result := 0;
  if ExtractEncryptionPassword(Params) <> '' then Result := 1;
  if Pos('/M', UpCase(Params)) > 0 then
  begin
    S := UpCase(ExtractStr(Params, '/M'));
    if S = '0' then Result := 0 else
    if S = '1' then Result := 1 else
    if S = '2' then Result := 2 else SetExitStatus(esCmdLineError);
  end;

  if Result <> 0 then
    if Length(ExtractEncryptionPassword(Params)) < 4 then
      SetExitStatus(esCmdLineError);
end;

function ExtractEncryptionPassword(const Params: string): string;
begin
  Result := ExtractStr(Params, '/P');
end;

function ExtractCheckMethod(Params: string): longword;
var
  S: string;
begin
  Result := 1;
  if Pos('/M', UpCase(Params)) > 0 then
  begin
    S := UpCase(ExtractStr(Params, '/M'));
    if S = '0' then Result := 0 else
    if S = '1' then Result := 1 else
    if S = '2' then Result := 2 else
    if S = '3' then Result := 3 else
    if S = '4' then Result := 4 else SetExitStatus(esCmdLineError);
  end;
end;

function ExtractCheckAuxMethod(Params: string): longword;
var
  S: string;
begin
  Result := 1;
  if Pos('/AM=', UpCase(Params)) > 0 then
  begin
    S := UpCase(ExtractStr(Params, '/AM='));
    if S = '0' then Result := 0 else
    if S = '1' then Result := 1 else
    if S = '2' then Result := 2 else
    if S = '3' then Result := 3 else
    if S = '4' then Result := 4 else SetExitStatus(esCmdLineError);
  end;
end;

// ---

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
  if UpCase(S[1]) = 'A' then FCommand := cAdd       else
  if UpCase(S[1]) = 'D' then FCommand := cDelete    else
  if UpCase(S[1]) = 'E' then FCommand := cExtract   else
  if UpCase(S[1]) = 'H' then FCommand := cHelp      else
  if UpCase(S[1]) = 'L' then FCommand := cList      else
  if UpCase(S[1]) = 'Q' then FCommand := cQuickTest else
  if UpCase(S[1]) = 'R' then FCommand := cRename    else
  if UpCase(S[1]) = 'T' then FCommand := cTest      else
  if UpCase(S[1]) = 'X' then FCommand := cXextract  else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessACCOption(var S: string);
begin
  Delete(S, 1, 5);
  if Pos('!', S) = 1 then
  begin
    Delete(S, 1, 1);
    FaccOption := S;
  end else
    if Pos('@', S) = 1 then
    begin
      Delete(S, 1, 1);
      if FileExists(S) then
      begin
        FaccOption := ExtractStrFromFile(S);
      end else
        SetExitStatus(esCmdLineError);
    end else
      SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, claccOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessBOption(var S: string);
begin
  Delete(S, 1, 2);
  if S = '' then
    FbOption := TRUE
  else
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clbOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCOption(var S: string);
begin
  Delete(S, 1, 3);
  FcOption := FcOption + '/' + S + '/';
  while Pos('//', FcOption) > 0 do
    Delete(FcOption, Pos('//', FcOption), 1);

  if Command in [cAdd] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clcOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCCOption(var S: string);
begin
  Delete(S, 1, 4);
  if Pos('!', S) = 1 then
  begin
    Delete(S, 1, 1);
    FccOption := S;
  end else
    if Pos('@', S) = 1 then
    begin
      Delete(S, 1, 1);
      if FileExists(S) then
      begin
        FccOption := ExtractStrFromFile(S);
      end else
        SetExitStatus(esCmdLineError);
    end else
      SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clccOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCDOption(var S: string);
begin
  Delete(S, 1, 4);
  if Length(S) > 0 then
    FcdOption := IncludeTrailingBackSlash(S)
  else
    SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clcdOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCIOption(var S: string);
begin
  Delete(S, 1, 4);
  FciOption := FciOption + '/' + S + '/';
  while Pos('//', FciOption) > 0 do
    Delete(FciOption, Pos('//', FciOption), 1);

  if Command in [cAdd] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clciOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessEOption(var S: string);
begin
  Delete(S, 1, 3);
  FeOption := FeOption + '/' + S + '/';
  while Pos('//', FeOption) > 0 do
    Delete(FeOption, Pos('//', FeOption), 1);

  if Command in [cAdd] then
  begin
    if ExitStatus = esNoError then
    begin
      Include(FOptions, cleOption);
      if ExtractEncryptionPassword(FeOption) <> '' then
      begin
        Include(FOptions, clpOption);
        FpOption := ExtractEncryptionPassword(FeOption);
      end;
    end;
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessIOption(var S: string);
begin
  Delete(S, 1, 3);
  if Pos('!', S) = 1 then
  begin
    Delete(S, 1, 1);
    FFileMasks.Add(S);
  end else
    if Pos('@', S) = 1 then
    begin
      Delete(S,1, 1);
      if FileExists(S) then
        FFileMasks.LoadFromFile(S)
      else
        SetExitStatus(esCmdLineError);
    end else
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    // nothing to do
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessLOption(var S: string);
var
  I: longint;
begin
  Delete(S, 1, 3);
  if TryStrToInt(S, FlOption) = FALSE then
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, cllOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessPOption(var S: string);
begin
  Delete(S, 1, 3);
  FpOption := S;

  if Command in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clpOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessROption(var S: string);
var
  I: longint;
begin
  if UpCase(S) = '-R' then
  begin
    if FrOption = '' then
      FrOption := 'A'
    else
      SetExitStatus(esCmdLineError);
  end else
    if Pos('-R/', UpCase(S)) = 1 then
    begin
      Delete(S, 1, 3);
      FrOption := FrOption + S;

      for I := 1 to Length(FrOption) do
        if (FrOption[I] in ['N', 'Y']) = FALSE then
          SetExitStatus(esCmdLineError);
    end else
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clrOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessRXOption(var S: string);
var
  I: longint;
begin
  if UpCase(S) = '-RX' then
  begin
    if FrxOption = '' then
      FrxOption := 'A'
    else
      SetExitStatus(esCmdLineError);
  end else
    if Pos('-RX/', UpCase(S)) = 1 then
    begin
      Delete(S, 1, 4);
      FrxOption := FrxOption + S;

      for I := 1 to Length(FrxOption) do
        if (FrxOption[I] in ['N', 'Y']) = FALSE then
          SetExitStatus(esCmdLineError);
    end else
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clrxOption);
  end else
    SetExitStatus(esCmdLineError);
end;

function  TCommandLine.GetrOption(Index: longint): boolean;
begin
  Result := FALSE;
  if FrOption = 'A' then
    Result := TRUE
  else
    if Index < Length(FrOption) then
      Result := FrOption[Index + 1] = 'Y';
end;

function  TCommandLine.GetrxOption(Index: longint): boolean;
begin
  Result := FALSE;
  if FrxOption = 'A' then
    Result := TRUE
  else
    if Index < Length(FrxOption) then
      Result := FrxOption[Index + 1] = 'Y';
end;

procedure TCommandLine.ProcessSFXOption(var S: string);
begin
  if UpCase(S) = '-SFX' then
  begin
    {$IFDEF MSWINDOWS}
      S := 'bxwin.sfx';
    {$ELSE}
      {$IFDEF UNIX}
        S := 'bxlinux.sfx';
      {$ELSE}
        -TODO-
      {$ENDIF}
    {$ENDIF}
  end else
    if Pos('-SFX/', UpCase(S)) = 1 then
      Delete(S, 1, 5)
    else
      SetExitStatus(esCmdLineError);

  FsfxOption := SelfPath + S;
  if FileExists(FsfxOption) = FALSE then
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clsfxOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessSLOption(var S: string);
begin
  Delete(S, 1, 3);
  if S = '' then
    FslOption := TRUE
  else
    SetExitStatus(esCmdLineError);

  if FCommand in [cList] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clslOption);
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

  if FCommand in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
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
  Delete(S, 1, 3);
  if TryStrToInt(S, I) then
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
  Delete(S, 1, 3);
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

procedure TCommandLine.ProcessVBOption(var S: string);
begin
  Delete(S, 1, 3);
  if S = '' then
    FvbOption := TRUE
  else
    SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clvbOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessWOption(var S: string);
begin
  if UpCase(S) = '-W' then
  begin
    FwOption := ExcludeTrailingBackSlash(GetTempDir)
  end else
    if Pos('-W/', UpCase(S)) = 1 then
    begin
      Delete(S, 1, 3);
      FwOption := ExcludeTrailingBackSlash(S);
      if DirectoryExists(FwOption) = FALSE then
         SetExitStatus(esCmdLineError);
    end else
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clwOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessXOption(var S: string);
begin
  Delete(S, 1, 3);
  if Pos('!', S) = 1 then
  begin
    Delete(S, 1, 1);
    FxOptions.Add(S);
  end else
    if Pos('@', S) = 1 then
    begin
      Delete(S,1, 1);
      if FileExists(S) then
        FxOptions.LoadFromFile(S)
      else
        SetExitStatus(esCmdLineError);
    end else
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
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

  if Command in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clyOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessArchiveName(var S: string);
begin
  FArchiveName := S;
  if FileExists(FArchiveName) = FALSE then
    if ExtractFileExt(FArchiveName) = '' then
      FArchiveName := ChangeFileExt(FArchiveName, '.bx');

  if FCommand in [cAdd, cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
  begin
    // check if archive exists
    if FCommand in [cDelete, cExtract, cList, cQuickTest, cRename, cTest, cXextract] then
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
      if (S[1] = '-') and (FssOption = FALSE) then
      begin
        // options...
        if Pos('-ACC/',  UpperCase(S)) = 1 then ProcessACCOption(S) else
        if Pos('-B',     UpperCase(S)) = 1 then ProcessBOption  (S) else
        if Pos('-C/',    UpperCase(S)) = 1 then ProcessCOption  (S) else
        if Pos('-CC/',   UpperCase(S)) = 1 then ProcessCCOption (S) else
        if Pos('-CD/',   UpperCase(S)) = 1 then ProcessCDOption (S) else
        if Pos('-CI/',   UpperCase(S)) = 1 then ProcessCIOption (S) else
        if Pos('-E/',    UpperCase(S)) = 1 then ProcessEOption  (S) else
        if Pos('-I/',    UpperCase(S)) = 1 then ProcessIOption  (S) else
        if Pos('-L/',    UpperCase(S)) = 1 then ProcessLOption  (S) else
        if Pos('-P/',    UpperCase(S)) = 1 then ProcessPOption  (S) else
        if Pos('-RX/',   UpperCase(S)) = 1 then ProcessRXOption (S) else
        if Pos('-RX',    UpperCase(S)) = 1 then ProcessRXOption (S) else
        if Pos('-R/',    UpperCase(S)) = 1 then ProcessROption  (S) else
        if Pos('-R',     UpperCase(S)) = 1 then ProcessROption  (S) else
        if Pos('-SFX/',  UpperCase(S)) = 1 then ProcessSFXOption(S) else
        if Pos('-SFX',   UpperCase(S)) = 1 then ProcessSFXOption(S) else
        if Pos('-SL',    UpperCase(S)) = 1 then ProcessSLOption (S) else
        if Pos('-SS',    UpperCase(S)) = 1 then ProcessSSOption (S) else
        if Pos('-T',     UpperCase(S)) = 1 then ProcessTOption  (S) else
        if Pos('-U/',    UpperCase(S)) = 1 then ProcessUOption  (S) else
        if Pos('-V/',    UpperCase(S)) = 1 then ProcessVOption  (S) else
        if Pos('-VB',    UpperCase(S)) = 1 then ProcessVBOption (S) else
        if Pos('-W/',    UpperCase(S)) = 1 then ProcessWOption  (S) else
        if Pos('-W',     UpperCase(S)) = 1 then ProcessWOption  (S) else
        if Pos('-X/',    UpperCase(S)) = 1 then ProcessXOption  (S) else
        if Pos('-Y',     UpperCase(S)) = 1 then ProcessYOption  (S) else
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
      if FCommand in [cExtract, cList, cQuickTest, cTest, cXextract] then
      begin
        FrOption := 'A';
        Include(FOptions, clrOption);
        FFileMasks.Add('*');
      end;
  end;
end;

end.
