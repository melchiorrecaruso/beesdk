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

  TCommand = (cAdd, cDelete, cExtract, cHelp, cList,
    cQuickTest, cRename, cTest, cXextract);

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
    clcOption,  clcdOption,  clcpOption,  clckpOption, clepOption, clhcOption,
    cllOption,  clppOption,  clrOption,   clsfxOption, clslsOption, clssOption,
    cltOption,  cluOption,   clvmOption,  clvsOption,  clwdOption,  clxOption,
    clxrOption, clyOption);

  TCommandLineOptions = set of TCommandLineOption;

  // TCommandLine class

  TCommandLine = class
  protected
    FCommand: TCommand;
    FOptions: TCommandLineOptions;
    FcOption: string;
    FcdOption: string;
    FcpOption: string;
    FckpOption: string;
    FepOption: string;
    FhcOption: string;
    FlOption: longword;


    FppOption: TProcessPriority;
    FrOption: string;
    FsfxOption: string;


    FslsOption: boolean;
    FssOption: boolean;
    FtOption: boolean;
    FuOption: TUpdateMethod;
    FvmOption: boolean;
    FvsOption: qword;
    FwdOption: string;
    FxOptions: TStringList;
    FxrOption: string;
    FyOption: boolean;
    FArchiveName: string;
    FFileMasks: TStringList;
    function  GetrOption(Index: longint): boolean;
    function  GetxrOption(Index: longint): boolean;
    procedure ProcessCommand(const S: string);
    procedure ProcessCOption (var S: string);
    procedure ProcessCDOption (var S: string);
    procedure ProcessCKPOption(var S: string);
    procedure ProcessCPOption (var S: string);
    procedure ProcessEPOption (var S: string);
    procedure ProcessHCOption (var S: string);
    procedure ProcessLOption  (var S: string);

    procedure ProcessPPOption (var S: string);
    procedure ProcessROption  (var S: string);
    procedure ProcessSFXOption(var S: string);
    procedure ProcessSLSOption(var S: string);
    procedure ProcessSSOption (var S: string);
    procedure ProcessTOption  (var S: string);
    procedure ProcessUOption  (var S: string);
    procedure ProcessVSOption (var S: string);
    procedure ProcessVMOption (var S: string);
    procedure ProcessWDOption (var S: string);
    procedure ProcessXOption  (var S: string);
    procedure ProcessXROption (var S: string);
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
    property cOption: string read FcOption;
    property cdOption: string read FcdOption;
    property cpOption: string read FcpOption;
    property ckpOption: string read FckpOption;
    property epOption: string read FepOption;
    property hcOption: string read FhcOption;
    property lOption: longword read FlOption;
    property ppOption: TProcessPriority read FppOption;
    property rOption[Index: longint]: boolean read GetrOption;
    property sfxOption: string read FsfxOption;
    property slsOption: boolean read FslsOption;
    property ssOption: boolean read FssOption;
    property tOption: boolean read FtOption;
    property uOption: TUpdateMethod read FuOption;
    property vmOption: boolean read FvmOption;
    property vsOption: qword read FvsOption;
    property wdOption: string read FwdOption;
    property xOptions: TStringList read FxOptions;
    property xrOption[Index: longint]: boolean read GetxrOption;
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

function LoadFromFile(const FileName: string): string;
var
  T: TStringList;
begin
  T := TStringList.Create;
  T.LoadFromFile(FileName);
  Result:= T.Text;
  T.Free;
end;

function GetUpdateMethod(const S: string): longint;
begin
  if UpCase(S) = '0' then Result := 0 else
  if UpCase(S) = '1' then Result := 1 else
  if UpCase(S) = '2' then Result := 2 else
  if UpCase(S) = '3' then Result := 3 else
  if UpCase(S) = '4' then Result := 4 else
  if UpCase(S) = '5' then Result := 5 else
  if UpCase(S) = '6' then Result := 6 else
  if UpCase(S) = '7' then Result := 7 else Result := -1;
end;

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
  if Upcase(S[1]) = 'A' then FCommand := cAdd       else
  if Upcase(S[1]) = 'D' then FCommand := cDelete    else
  if Upcase(S[1]) = 'E' then FCommand := cExtract   else
  if Upcase(S[1]) = 'H' then FCommand := cHelp      else
  if Upcase(S[1]) = 'L' then FCommand := cList      else
  if Upcase(S[1]) = 'Q' then FCommand := cQuickTest else
  if Upcase(S[1]) = 'R' then FCommand := cRename    else
  if Upcase(S[1]) = 'T' then FCommand := cTest      else
  if Upcase(S[1]) = 'X' then FCommand := cXextract  else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCOption(var S: string);
begin
  if Pos('-AC@:', UpperCase(S)) = 1 then
  begin
    Delete(S, 1, 5);
    FcOption := LoadFromFile(S);
  end else
    if Pos('-AC:', UpperCase(S)) = 1 then
    begin
      Delete(S, 1, 4);
      FcOption := S;
    end else
      SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clcOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessCDOption(var S: string);
begin
  Delete(S, 1, 4);
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
  Delete(S, 1, 4);
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
  Delete(S, 1, 6);
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
  Delete(S, 1, 4);
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

procedure TCommandLine.ProcessHCOption(var S: string);
begin
  if Pos('-FC@:', UpperCase(S)) = 1 then
  begin
    Delete(S, 1, 5);
    FhcOption := LoadFromFile(S);
  end else
    if Pos('-FC:', UpperCase(S)) = 1 then
    begin
      Delete(S, 1, 4);
      FhcOption := S;
    end else
      SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clhcOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessLOption(var S: string);
var
  Q: qword;
begin
  Delete(S, 1, 3);
  if TryStrToQWord(S, Q) then
    FlOption := Q
  else
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, cllOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessPPOption(var S: string);
begin
  Delete(S, 1, 4);
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

function  TCommandLine.GetrOption(Index: longint): boolean;
begin
  if FrOption = '' then
    Result := FALSE
  else
    if UpCase(FrOption) = 'ALL' then
      Result := TRUE
    else
      if Length(FrOption) <= (Index + 1) then
      begin
        if UpCase(FrOption[Index + 1]) in ['Y', 'N'] then
          Result := UpCase(FrOption[Index + 1]) = 'Y'
        else
          SetExitStatus(esCmdLineError);
      end;
end;

procedure TCommandLine.ProcessROption(var S: string);
begin
  if UpCase(S) = '-R' then
  begin
    Delete(S, 1, 2);
    FrOption := 'ALL';
  end else
    if Pos('-R:', UpperCase(S)) = 1 then
    begin
      Delete(S, 1, 3);
      FrOption := S;
    end else
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
  Delete(S, 1, 6);
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
    if S = '-' then
      FslsOption := FALSE
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
    if S = '-' then
      FssOption := FALSE
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
    if S = '-' then
      FtOption := FALSE
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

procedure TCommandLine.ProcessVMOption(var S: string);
begin
  Delete(S, 1, 3);
  if S = '' then
    FvmOption := TRUE
  else
    if S ='-' then
      FvmOption := FALSE
    else
      SetExitStatus(esCmdLineError);

  if Command in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clvmOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessVSOption(var S: string);
var
  I: longint;
begin
  Delete(S, 1, 4);
  for I := 1 to Length(S) do
    if S[I] in ['.', ','] then
      S[I] := DecimalSeparator;

  if TryStrWithMultToQWord(S, FvsOption) = FALSE then
    SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cRename] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clvsOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessWDOption(var S: string);
begin
  Delete(S, 1, 4);
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
  Delete(S, 1, 3);
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

function  TCommandLine.GetxrOption(Index: longint): boolean;
begin
  if FxrOption = '' then
    Result := FALSE
  else
    if UpCase(FxrOption) = 'ALL' then
      Result := TRUE
    else
      if Length(FxrOption) <= (Index + 1) then
      begin
        if UpCase(FxrOption[Index + 1]) in ['Y', 'N'] then
          Result := UpCase(FxrOption[Index + 1]) = 'Y'
        else
          SetExitStatus(esCmdLineError);
      end;
end;

procedure TCommandLine.ProcessXROption(var S: string);
begin
  if UpCase(S) = '-XR' then
  begin
    Delete(S, 1, 3);
    FxrOption := 'ALL';
  end else
    if Pos('-XR:', UpperCase(S)) = 1 then
    begin
      Delete(S, 1, 4);
      FxrOption := S;
    end else
      SetExitStatus(esCmdLineError);

  if FCommand in [cAdd, cDelete, cExtract, cList, cRename, cTest, cXextract] then
  begin
    if ExitStatus = esNoError then
      Include(FOptions, clxrOption);
  end else
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessYOption(var S: string);
begin
  Delete(S, 1, 2);
  if S = '' then
    FyOption := TRUE
  else
    if S = '-' then
      FyOption := FALSE
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
      FArchiveName := ChangeFileExt(FArchiveName, '.bx');

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
        if Pos('-C@',    UpperCase(S)) = 1 then ProcessCOption (S) else
        if Pos('-C:',    UpperCase(S)) = 1 then ProcessCOption (S) else

        if Pos('-CD:',   UpperCase(S)) = 1 then ProcessCDOption (S) else
        if Pos('-CK:',   UpperCase(S)) = 1 then ProcessCKPOption(S) else
        if Pos('-CM:',   UpperCase(S)) = 1 then ProcessCPOption (S) else
        if Pos('-CP:',   UpperCase(S)) = 1 then ProcessEPOption (S) else

        if Pos('-HC@',   UpperCase(S)) = 1 then ProcessHCOption (S) else
        if Pos('-HC:',   UpperCase(S)) = 1 then ProcessHCOption (S) else
        if Pos('-L:',    UpperCase(S)) = 1 then ProcessLOption  (S) else

        if Pos('-P:',    UpperCase(S)) = 1 then ProcessPPOption (S) else
        if Pos('-R:',    UpperCase(S)) = 1 then ProcessROption  (S) else
        if Pos('-R',     UpperCase(S)) = 1 then ProcessROption  (S) else

        if Pos('-SFX@',  UpperCase(S)) = 1 then ProcessSFXOption(S) else

        if Pos('-SLS',   UpperCase(S)) = 1 then ProcessSLSOption(S) else
        if Pos('-SS',    UpperCase(S)) = 1 then ProcessSSOption (S) else
        if Pos('-T',     UpperCase(S)) = 1 then ProcessTOption  (S) else
        if Pos('-U:',    UpperCase(S)) = 1 then ProcessUOption  (S) else
        if Pos('-VM',    UpperCase(S)) = 1 then ProcessVMOption (S) else
        if Pos('-VS:',   UpperCase(S)) = 1 then ProcessVSOption (S) else
        if Pos('-WD:',   UpperCase(S)) = 1 then ProcessWDOption (S) else

        if Pos('-XR:',   UpperCase(S)) = 1 then ProcessXROption (S) else
        if Pos('-XR',    UpperCase(S)) = 1 then ProcessXROption (S) else
        if Pos('-X:',    UpperCase(S)) = 1 then ProcessXOption  (S) else

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
      if FCommand in [cExtract, cList, cTest, cXextract] then
      begin
        FFileMasks.Add('*');
        FrOption := 'ALL';
        Include(FOptions, clrOption);
      end;
  end;
end;

end.

