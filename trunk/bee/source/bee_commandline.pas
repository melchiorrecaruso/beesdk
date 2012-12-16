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
  { Commands:                                             }
  {   ccNone     Nul command                              }
  {   ccAdd      Add files                                }
  {   ccExtract  Extract file                             }
  {   ceXextract Extract file with full path              }
  {   ccTest     Test files                               }
  {   ccDelete   Delete files                             }
  {   ccRename   Rename files                             }
  {   ccList     List files                               }

  TCommand = (ccAdd, ccExtract, ccXextract, ccTest,
    ccDelete, ccRename, ccList, ccHelp);

  { Update Mode Option:                                   }
  {  umAdd           Add only new files                   }
  {  umUpdate        Update only existing files           }
  {  umReplace       Replace only existing files          }
  {  umQuery         Query always                         }
  {  umAddUpdate     Add and update existing files        }
  {  umAddReplace    Add and replace existing files       }
  {  umAddQuery      Add and query if already exists      }
  {  umAddAutoRename Add and rename if already exists     }

  TUpdateMode = (umAdd, umUpdate, umReplace, umQuery,
    umAddUpdate, umAddReplace, umAddQuery, umAddAutoRename);

  { Compression Method Option:                            }
  {   moStore                                             }
  {   moFast                                              }
  {   moNormal                                            }
  {   moMaximum                                           }

  TmOption = (moStore, moFast, moNormal, moMaximum);

  { Compression Dictionary Level Option:                  }
  {   do2MB                                               }
  {   do5MB                                               }
  {   ..                                                  }
  {   do1280MB                                            }

  TdOption = (do2MB, do5MB, do10MB, do20MB, do40MB,
    do80MB, do160MB, do320MB, do640MB ,do1280MB);

  { Process Priority Option:                              }
  {   prioIdle                                            }
  {   prioNormal                                          }
  {   prioHigh                                            }
  {   prioRealTime                                        }

  TpriOption = (prioIdle, prioNormal, prioHigh, prioRealTime);

  { TCommandLine }

  TCommandLine = class
  protected
    FCommand: TCommand;
    FssOption: boolean;
    FrOption: TRecursiveMode;
    FuOption: TUpdateMode;
    FxOptions: TStringList;
    FmOption: TmOption;
    FdOption: TdOption;
    FsOption: qword;
    FfOption: string;
    FsfxOption: string;
    FpOption: string;
    FtOption: boolean;
    FslsOption: boolean;
    FiOption: qword;
    FwdOption: string;
    FcdOption: string;
    FcfgOption: string;
    FpriOption: TpriOption;
    FArchiveName: string;
    FFileMasks: TStringList;
    procedure ProcessOptionSS (var S: string);
    procedure ProcessOptionR  (var S: string);
    procedure ProcessOptionU  (var S: string);
    procedure ProcessOptionX  (var S: string);
    procedure ProcessOptionM  (var S: string);
    procedure ProcessOptionD  (var S: string);
    procedure ProcessOptionS  (var S: string);
    procedure ProcessOptionF  (var S: string);
    procedure ProcessOptionSFX(var S: string);
    procedure ProcessOptionP  (var S: string);
    procedure ProcessOptionT  (var S: string);
    procedure ProcessOptionSLS(var S: string);
    procedure ProcessOptionI  (var S: string);
    procedure ProcessOptionWD (var S: string);
    procedure ProcessOptionCD (var S: string);
    procedure ProcessOptionCFG(var S: string);
    procedure ProcessOptionPRI(var S: string);
    procedure ProcessCommand(const S: string);
    procedure ProcessArchiveName(var S: string);
    procedure ProcessFileMasks(const S: string);

    function GetCommandLine: string; virtual;
    procedure SetCommandLine(const aValue: string); virtual;
    procedure SetOptionF(const aValue: string);
    procedure SetOptionP(const aValue: string);
    procedure SetOptionSFX(const aValue: string);
    procedure SetOptionWD(const aValue: string);
    procedure SetOptionCD(const aValue: string);
    procedure SetOptionCFG(const aValue: string);
    procedure SetArchiveName(const aValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property Command: TCommand read FCommand write FCommand;
    property ssOption: boolean read FssOption write FssOption;
    property rOption: TRecursiveMode read FrOption write FrOption;
    property uOption: TUpdateMode read FuOption write FuOption;
    property xOptions: TStringList read FxOptions;
    property mOption: TmOption read FmOption write FmOption;
    property dOption: TdOption read FdOption write FdOption;
    property sOption: qword read FsOption write FsOption;
    property fOption: string read FfOption write SetOptionF;
    property sfxOption: string read FsfxOption write SetOptionSFX;
    property pOption: string read FpOption write SetOptionP;
    property tOption: boolean read FtOption write FtOption;
    property slsOption: boolean read FslsOption write FslsOption;
    property iOption: qword read FiOption write FiOption;
    property wdOption: string read FwdOption write SetOptionWD;
    property cdOption: string read FcdOption write SetOptionCD;
    property cfgOption: string read FcfgOption write SetOptionCFG;
    property priOption: TpriOption read FpriOption write FpriOption;
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

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
  FxOptions := TStringList.Create;
  FFileMasks := TStringList.Create;
  Clear;
end;

procedure TCommandLine.Clear;
begin
  FCommand     := ccHelp;
  FssOption    := False;
  FrOption     := rmNone;
  FuOption     := umAddUpdate;
  FxOptions.Clear;
  FmOption     := moFast;
  FdOption     := do5MB;
  FsOption     := 0;
  FfOption     := '';
  FsfxOption   := '';
  FiOption     := 0;
  FpOption     := '';
  FtOption     := False;
  FslsOption   := False;
  FwdOption    := '';
  FcdOption    := '';
  FcfgOption   := SelfPath + DefaultCfgName;
  FpriOption   := prioNormal;
  FArchiveName := '';
  FFileMasks.Clear;
end;

destructor TCommandLine.Destroy;
begin
  FxOptions.Destroy;
  FFileMasks.Destroy;
  inherited Destroy;
end;

procedure TCommandLine.ProcessOptionSS(var S: string);
begin
  Delete(S, 1, 2);
  if (S = '') or (S = '+') then
    FssOption := True
  else
    if (S = '-') then
      FssOption := False
    else
      SetExitCode(ecCmdLineError);

  if (FCommand in [ccHelp]) = TRUE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionR(var S: string);
begin
  if Pos('-RW', UpperCase(S)) = 1 then
  begin
    Delete(S, 1, 3);
    if (S = '') or (S = '+') then
      FrOption := rmWildCard
    else
      if (S = '-') then
        FrOption := rmNone
      else
        SetExitCode(ecCmdLineError);
  end else
  begin
    Delete(S, 1, 2);
    if (S = '') or (S = '+') then
      FrOption := rmFull
    else
      if (S = '-') then
        FrOption := rmNone
      else
        SetExitCode(ecCmdLineError);
  end;

  if (FCommand in [ccHelp]) = TRUE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionU(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'7']) then
    FuOption := TUpdateMode(StrToInt(S[1]))
  else
    SetExitCode(ecCmdLineError);

  if (Command in [ccAdd, ccExtract, ccXextract]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionX(var S: string);
begin
  Delete(S, 1, 2);
  if Length(S) > 0 then
    FxOptions.Add(S)
  else
    SetExitCode(ecCmdLineError);

  if (FCommand in [ccHelp]) = TRUE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionM(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'3']) then
    FmOption := TmOption(StrToInt(S[1]))
  else
    SetExitCode(ecCmdLineError);

  if (Command in [ccAdd]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionD(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'9']) then
    FdOption := TdOption(StrToInt(S[1]))
  else
    SetExitCode(ecCmdLineError);

  if (Command in [ccAdd]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionS(var S: string);
begin
  Delete(S, 1, 2);
  if TryStrWithMultToQWord(S, FsOption) = FALSE then
    SetExitCode(ecCmdLineError);

  if (Command in [ccAdd]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionF(var S: string);
begin
  Delete(S, 1, 2);
  if ExtractFileExt('.' + S) <> '.' then
    SetOptionF(S)
  else
    SetExitCode(ecCmdLineError);

  if (Command in [ccAdd]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionSFX(var S: string);
begin
  Delete(S, 1, 4);
  if (Length(S) = 0) or (S = '+')  then
    FsfxOption := ExtractFilePath(ParamStr(0)) + DefaultSfxName
  else
    if (S = '-') then
      FsfxOption := 'nul'
    else
      FsfxOption := S;

  if Length(FsfxOption) > 0 then
    if FileExists(FsfxOption) = FALSE then
      SetExitCode(ecCmdLineError);

  if (Command in [ccAdd, ccDelete, ccRename]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionP(var S: string);
begin
  Delete(S, 1, 2);
  if Length(S) >= MinBlowFishKeyLength then
    SetOptionP(S)
  else
    SetExitCode(ecCmdLineError);

  if (Command in [ccRename, ccList, ccHelp]) = TRUE then
    SetExitCode(ecCmdLineError);
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
      SetExitCode(ecCmdLineError);

  if (Command in [ccAdd, ccDelete, ccRename]) = FALSE then
    SetExitCode(ecCmdLineError);
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
      SetExitCode(ecCmdLineError);

  if (Command in [ccList]) = FALSE then
    SetExitCode(ecCmdLineError);
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
    SetExitCode(ecCmdLineError);

  if (Command in [ccAdd, ccDelete, ccRename]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionWD(var S: string);
begin
  Delete(S, 1, 3);
  FwdOption := ExcludeTrailingBackslash(S);
  if DirectoryExists(FwdOption) = FALSE then
    SetExitCode(ecCmdLineError);

  if (Command in [ccAdd, ccDelete, ccRename]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionCD(var S: string);
begin
  Delete(S, 1, 3);
  if Length(S) > 0 then
    FcdOption := IncludeTrailingBackSlash(S)
  else
    SetExitCode(ecCmdLineError);

  if (Command in [ccHelp]) = TRUE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionCFG(var S: string);
begin
  Delete(S, 1, 4);
  if FileExists(S) then
    FcfgOption := S
  else
    SetExitCode(ecCmdLineError);

  if (Command in [ccAdd]) = FALSE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessOptionPRI(var S: string);
begin
  Delete(S, 1, 4);
  if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
    FpriOption := TpriOption(StrToInt(S[1]))
  else
    SetExitCode(ecCmdLineError);

  if (Command in [ccHelp]) = TRUE then
    SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessCommand(const S: string);
begin
  if Length(S) = 1 then
    case Upcase(S[1]) of
      'A': FCommand := ccAdd;
      'E': FCommand := ccExtract;
      'X': FCommand := ccxExtract;
      'T': FCommand := ccTest;
      'D': FCommand := ccDelete;
      'R': FCommand := ccRename;
      'L': FCommand := ccList;
      'H': FCommand := ccHelp;
      else SetExitCode(ecCmdLineError);
    end
  else SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessArchiveName(var S: string);
begin
  FssOption    := TRUE;
  FArchiveName := S;
  if FileExists(FArchiveName) = FALSE then
    if ExtractFileExt(FArchiveName) = '' then
      FArchiveName := ChangeFileExt(FArchiveName, '.beex');

  // check if archive exists
  if (FCommand in [ccHelp]) = TRUE then
    SetExitCode(ecCmdLineError)
  else
    if (FCommand in [ccAdd]) = FALSE then
      if FileExists(FArchiveName) = FALSE then
        SetExitCode(ecCmdLineError);
end;

procedure TCommandLine.ProcessFileMasks(const S: string);
begin
  FFileMasks.Add(S);

  if (FCommand in [ccHelp]) = TRUE then
    SetExitCode(ecCmdLineError);
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
        case UpCase(S[2]) of
          '-': ProcessOptionSS(S);
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
          else SetExitCode(ecCmdLineError);
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
      ccAdd:      SetExitCode(ecCmdLineError);
      ccDelete:   SetExitCode(ecCmdLineError);
      ccRename:   SetExitCode(ecCmdLineError);

      ccExtract:  FFileMasks.Add('*');
      ccxExtract: FFileMasks.Add('*');
      ccTest:     FFileMasks.Add('*');
      ccList:     FFileMasks.Add('*');
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
    ccAdd:      Params.Add('A');
    ccExtract:  Params.Add('E');
    ccxExtract: Params.Add('X');
    ccTest:     Params.Add('T');
    ccDelete:   Params.Add('D');
    ccRename:   Params.Add('R');
    ccList:     Params.Add('L');
    else        Params.Add(' ');
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
