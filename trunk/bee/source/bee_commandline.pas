{
  Copyright (c) 2003-2010 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_CommandLine;

{$I compiler.inc}

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

  TUpdateMethod = (umAdd, umUpdate, umReplace, umQuery,
    umAddUpdate, umAddReplace, umAddQuery, umAddAutoRename);

  // Process Priority
  //   ppIdle
  //   ppNormal
  //   ppHigh
  //   ppRealTime

  TProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime);

  // TCommandLineOptions

  TCommandLineOption = (
    clcOption, clcdOption,  clcmOption,  clemOption, clppOption,
    clrOption, clsfxOption, clslsOption, clssOption, cltOption,
    cluOption, clvOption,   clwdOption,  clxOptions);

  TCommandLineOptions = set of TCommandLineOption;

  // TCommandLine class

  TCommandLine = class
  protected
    FCommand: TCommand;
    FOptions: TCommandLineOptions;
    FcOption: string;
    FcdOption: string;
    FcmOption: string;
    FemOption: string;
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
    FArchiveName: string;
    FFileMasks: TStringList;
    procedure ProcessCommand(const S: string);
    procedure ProcessOptionC  (var S: string);
    procedure ProcessOptionCD (var S: string);
    procedure ProcessOptionCM (var S: string);
    procedure ProcessOptionEM (var S: string);
    procedure ProcessOptionPP (var S: string);
    procedure ProcessOptionR  (var S: string);
    procedure ProcessOptionSFX(var S: string);
    procedure ProcessOptionSLS(var S: string);
    procedure ProcessOptionSS (var S: string);
    procedure ProcessOptionT  (var S: string);
    procedure ProcessOptionU  (var S: string);
    procedure ProcessOptionV  (var S: string);
    procedure ProcessOptionWD (var S: string);
    procedure ProcessOptionX (var S: string);
    procedure ProcessArchiveName(var S: string);
    procedure ProcessFileMasks(const S: string);
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Command: TCommand read FCommand;
    property Options: TCommandLineOptions read FOptions;
    property cOption: string read FcOption;
    property cdOption: string read FcdOption;
    property cmOption: string read FcmOption;
    property emOption: string read FcmOption;
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
    property ArchiveName: string read FArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

  function CheckUpdateMethod(const Answer: string): longint;

implementation

uses
  Math,
  Bee_BlowFish,
  Bee_Interface;

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

procedure TCommandLine.ProcessOptionC(var S: string);
begin
  Delete(S, 1, 2);
  FcOption := S;

  Include(FOptions, clcOption);
  if (Command in [cAdd, cDelete, cRename]) = FALSE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionCD(var S: string);
begin
  Delete(S, 1, 3);
  if Length(S) > 0 then
    FcdOption := IncludeTrailingBackSlash(S)
  else
    FcdOption := '';

  Include(FOptions, clcdOption);
  if (Command in [cHelp]) = TRUE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionCM(var S: string);
begin
  Delete(S, 1, 3);
  FcmOption := S;

  Include(FOptions, clcmOption);
  if (Command in [cAdd]) = FALSE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionEM(var S: string);
begin
  Delete(S, 1, 3);
  FemOption := S;

  Include(FOptions, clemOption);
  if (Command in [cAdd]) = FALSE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionPP(var S: string);
begin
  Delete(S, 1, 4);
  if (Length(S) = 1) and (S[1] in ['0'..'3']) then
  begin
    FppOption := TProcessPriority(StrToInt(S[1]));
    Include(FOptions, clppOption);
  end else
    SetExitStatus(esCmdLineError);

  if (Command in [cHelp]) = TRUE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionR(var S: string);
begin
  Delete(S, 1, 2);
  if UpperCase(S) = 'W' then
    FrOption := rmWildCard
  else
    if S = '-' then
      FrOption := rmNone
    else
      if S = '' then
        FrOption := rmFull
      else
        SetExitStatus(esCmdLineError);

  if UpperCase(S) in ['W', '-'] then
    Include(FOptions, clrOption);

  if (FCommand in [cHelp]) = TRUE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionSFX(var S: string);
begin
  Delete(S, 1, 4);
  FsfxOption := S;

  if Command = cAdd then
    if FileExists(FsfxOption) = FALSE then
      SetExitStatus(esCmdLineError);

  if (Command in [cAdd, cDelete]) = FALSE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionSLS(var S: string);
begin
  Delete(S, 1, 4);
  if (S = '') or (S = '+') then
    FslsOption := True
  else
    if (S = '-') then
      FslsOption := False
    else
      SetExitStatus(esCmdLineError);

  if (Command in [cList]) = FALSE then
    SetExitStatus(esCmdLineError);
end;










procedure TCommandLine.ProcessOptionSS(var S: string);
begin
  Delete(S, 1, 3);
  if (S = '') or (S = '+') then
    FssOption := True
  else
    if (S = '-') then
      FssOption := False
    else
      SetExitStatus(esCmdLineError);

  if (FCommand in [cHelp]) = TRUE then
    SetExitStatus(esCmdLineError);
end;





procedure TCommandLine.ProcessOptionU(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'7']) then
    FuOption := TUpdateMode(StrToInt(S[1]))
  else
    SetExitStatus(esCmdLineError);

  if (Command in [cAdd, cExtract, cXextract]) = FALSE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionX(var S: string);
begin
  Delete(S, 1, 2);
  if Length(S) > 0 then
    FxOptions.Add(S)
  else
    SetExitStatus(esCmdLineError);

  if (FCommand in [cHelp]) = TRUE then
    SetExitStatus(esCmdLineError);
end;



procedure TCommandLine.ProcessOptionD(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'9']) then
    FdOption := TdOption(StrToInt(S[1]))
  else
    SetExitStatus(esCmdLineError);

  if (Command in [cAdd]) = FALSE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionS(var S: string);
begin
  Delete(S, 1, 2);
  if TryStrWithMultToQWord(S, FsOption) = FALSE then
    SetExitStatus(esCmdLineError);

  if (Command in [cAdd]) = FALSE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionF(var S: string);
begin
  Delete(S, 1, 2);
  if ExtractFileExt('.' + S) <> '.' then
    SetOptionF(S)
  else
    SetExitStatus(esCmdLineError);

  if (Command in [cAdd]) = FALSE then
    SetExitStatus(esCmdLineError);
end;



procedure TCommandLine.ProcessOptionP(var S: string);
begin
  Delete(S, 1, 2);
  if Length(S) >= MinBlowFishKeyLength then
    SetOptionP(S)
  else
    SetExitStatus(esCmdLineError);

  if (Command in [cRename, cList, cHelp]) = TRUE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionT(var S: string);
begin
  Delete(S, 1, 2);
  if (S = '') or (S = '+') then
    FtOption := True
  else
    if (S = '-') then
      FtOption := False
    else
      SetExitStatus(esCmdLineError);

  if (Command in [cAdd, cDelete, cRename]) = FALSE then
    SetExitStatus(esCmdLineError);
end;



procedure TCommandLine.ProcessOptionI(var S: string);
var
  I: longint;
begin
  Delete(S, 1, 2);

  for I := 1 to Length(S) do
    if S[I] in ['.', ','] then
      S[I] := DecimalSeparator;

  if TryStrWithMultToQWord(S, FiOption) = FALSE then
    SetExitStatus(esCmdLineError);

  if (Command in [cAdd, cDelete, cRename]) = FALSE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessOptionWD(var S: string);
begin
  Delete(S, 1, 3);
  FwdOption := ExcludeTrailingBackslash(S);
  if FwdOption = '' then
    FwdOption := ExcludeTrailingBackSlash(GetTempDir)
  else
    if DirectoryExists(FwdOption) = FALSE then
      SetExitStatus(esCmdLineError);

  if (Command in [cAdd, cDelete, cRename]) = FALSE then
    SetExitStatus(esCmdLineError);
end;



procedure TCommandLine.ProcessOptionCFG(var S: string);
begin
  Delete(S, 1, 4);
  if FileExists(S) then
    FcfgOption := S
  else
    SetExitStatus(esCmdLineError);

  if (Command in [cAdd]) = FALSE then
    SetExitStatus(esCmdLineError);
end;





procedure TCommandLine.ProcessArchiveName(var S: string);
begin
  FssOption    := TRUE;
  FArchiveName := S;
  if FileExists(FArchiveName) = FALSE then
    if ExtractFileExt(FArchiveName) = '' then
      FArchiveName := ChangeFileExt(FArchiveName, '.beex');

  // check if archive exists
  if (FCommand in [cHelp]) = TRUE then
    SetExitStatus(esCmdLineError)
  else
    if (FCommand in [cAdd]) = FALSE then
      if FileExists(FArchiveName) = FALSE then
        SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.ProcessFileMasks(const S: string);
begin
  FFileMasks.Add(S);

  if (FCommand in [cHelp]) = TRUE then
    SetExitStatus(esCmdLineError);
end;

procedure TCommandLine.SetCommandLine(const aValue: string);
var
  I: longint;
  S: string;
  Params: TStringList;
begin
  Clear;
  Params := TStringList.Create;
  Params.Text := aValue;
  // catch command
  if Params.Count > 0 then
    ProcessCommand(Params[0]);
  // catch options, archive name and name of files
  if Params.Count > 1 then
    for I := 1 to Params.Count - 1 do
    begin
      S := Params[I];
      if (not FssOption) and (Length(S) > 1) and (S[1] = '-') then
      begin
        // options...
        if Pos('-SLS', UpperCase(S)) = 1 then
          ProcessOptionSLS(S)
        else
        if Pos('-SFX', UpperCase(S)) = 1 then
          ProcessOptionSFX(S)
        else
        if Pos('-PRI', UpperCase(S)) = 1 then
          ProcessOptionPRI(S)
        else
        if Pos('-CFG', UpperCase(S)) = 1 then
          ProcessOptionCFG(S)
        else
        if Pos('-WD', UpperCase(S)) = 1 then
          ProcessOptionWD(S)
        else
        if Pos('-CD', UpperCase(S)) = 1 then
          ProcessOptionCD(S)
        else
        if Pos('-SS', UpperCase(S)) = 1 then
          ProcessOptionSS(S)
        else
        case UpCase(S[2]) of
          'C': ProcessOptionC (S);
          'R': ProcessOptionR (S);
          'U': ProcessOptionU (S);
          'X': ProcessOptionX (S);
          'M': ProcessOptionM (S);
          'D': ProcessOptionD (S);
          'S': ProcessOptionS (S);
          'F': ProcessOptionF (S);
          'P': ProcessOptionP (S);
          'T': ProcessOptionT (S);
          'I': ProcessOptionI (S);
          else SetExitStatus(esCmdLineError);
        end; // end case
      end else

      if FArchiveName = '' then
        ProcessArchiveName(S)
      else
        ProcessFileMasks(S);

    end; // end for loop
  Params.Free;

  // check file masks
  if FFileMasks.Count = 0 then
  begin
    case FCommand of
      cAdd:      SetExitStatus(esCmdLineError);
      cDelete:   SetExitStatus(esCmdLineError);
      cRename:   SetExitStatus(esCmdLineError);

      cExtract:  FFileMasks.Add('*');
      cxExtract: FFileMasks.Add('*');
      cTest:     FFileMasks.Add('*');
      cList:     FFileMasks.Add('*');
    end;
    FrOption := rmFull;
  end;
end;

function TCommandLine.GetCommandLine: string;
var
  I: longint;
  Params: TStringList;
begin
  Params := TStringList.Create;
  case FCommand of
    cAdd:      Params.Add('A');
    cExtract:  Params.Add('E');
    cxExtract: Params.Add('X');
    cTest:     Params.Add('T');
    cDelete:   Params.Add('D');
    cRename:   Params.Add('R');
    cList:     Params.Add('L');
    else       Params.Add(' ');
  end;

  case FrOption of
    rmFull:     Params.Add('-r+' );
    rmWildCard: Params.Add('-rw+');
    else        Params.Add('-r-' );
  end;

  case FuOption of
    umAdd:           Params.Add('-u0');
    umUpdate:        Params.Add('-u1');
    umReplace:       Params.Add('-u2');
    umAddUpdate:     Params.Add('-u3');
    umAddReplace:    Params.Add('-u4');
    umAddAutoRename: Params.Add('-u5');
  end;

  for I := 0 to FxOptions.Count - 1 do
    Params.Add('-x' + FxOptions[I]);

  Params.Add('-m' + IntToStr(Ord(FmOption)));
  Params.Add('-d' + IntToStr(Ord(FdOption)));

  if FsOption > 0 then Params.Add('-s+') else Params.Add('-s-');

  if Length(FfOption)   > 0 then Params.Add('-f'   + FfOption);
  if Length(FsfxOption) > 0 then Params.Add('-sfx' + FsfxOption);
  if Length(FpOption)   > 0 then Params.Add('-p'   + FpOption);

  if FtOption   then Params.Add('-t+')   else Params.Add('-t-');
  if FslsOption then Params.Add('-sls+') else Params.Add('-sls-');


  if Length(FwdOption)  > 0 then Params.Add('-wd'  + FwdOption);
  if Length(FcdOption)  > 0 then Params.Add('-cd'  + FcdOption);
  if Length(FcfgOption) > 0 then Params.Add('-cfg' + FcfgOption);

  Params.Add('-pri' + IntToStr(Ord(FpriOption)));

  if FssOption then Params.Add('--+') else Params.Add('---');

  Params.Add(FArchiveName);

  for I := 0 to FFileMasks.Count - 1 do
    Params.Add(FFileMasks[I]);

  begin
    Result := Params.Text;
  end;
  Params.Destroy;
end;

procedure TCommandLine.SetOptionF(const aValue: string);
begin
  if ExtractFileExt('.' + aValue) <> '.' then
    FfOption := ExtractFileExt('.' + aValue);
end;

procedure TCommandLine.SetOptionP(const aValue: string);
begin
  if Length(aValue) >= MinBlowFishKeyLength then
    FpOption := aValue;
end;

procedure TCommandLine.SetOptionSFX(const aValue: string);
begin
  if FileExists(aValue) then
    FsfxOption := aValue
  else
    FsfxOption := 'nul';
end;

procedure TCommandLine.SetOptionWD(const aValue: string);
begin
  if DirectoryExists(aValue) then
    FwdOption := aValue;
end;

procedure TCommandLine.SetOptionCD(const aValue: string);
begin
  if Length(aValue) > 0 then
    FcdOption := IncludeTrailingBackSlash(aValue);
end;

procedure TCommandLine.SetOptionCFG(const aValue: string);
begin
  if FileExists(aValue) then
    FcfgOption := aValue;
end;

procedure TCommandLine.SetArchiveName(const aValue: string);
begin
  FArchiveName := aValue;
end;

end.

