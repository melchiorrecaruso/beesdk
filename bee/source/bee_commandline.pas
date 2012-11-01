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
  Bee_Common,
  Bee_Consts;

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

  TCommand = (ccNone, ccAdd, ccExtract, ccXextract, ccTest,
    ccDelete, ccRename,  ccList);

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
    FsOption: boolean;
    FfOption: string;
    FsfxOption: string;
    FpOption: string;
    FtOption: boolean;
    FlOption: boolean;
    FslsOption: boolean;
    FwdOption: string;
    FcdOption: string;
    FcfgOption: string;
    FpriOption: TpriOption;
    FArchiveName: string;
    FFileMasks: TStringList;
    function GetCommandLine: string; virtual;
    procedure SetCommandLine(const aValue: string); virtual;
    procedure SetfOption(const aValue: string);
    procedure SetpOption(const aValue: string);
    procedure SetsfxOption(const aValue: string);
    procedure SetwdOption(const aValue: string);
    procedure SetcdOption(const aValue: string);
    procedure SetcfgOption(const aValue: string);
    procedure SetArchiveName(const aValue: string);
    procedure ProcessrOption(var S: string);
    procedure ProcesspOption(var S: string);
    procedure ProcessuOption(var S: string);
    procedure ProcessxOption(var S: string);
    procedure ProcessmOption(var S: string);
    procedure ProcessdOption(var S: string);
    procedure ProcessfOption(var S: string);
    procedure ProcesssfxOption(var S: string);
    procedure ProcessslsOption(var S: string);
    procedure ProcesswdOption(var S: string);
    procedure ProcesscdOption(var S: string);
    procedure ProcesscfgOption(var S: string);
    procedure ProcesspriOption(var S: string);
    procedure ProcessCommand(var S: string);
    procedure ProcessArchiveName(var S: string);
    procedure ProcessOption(var S: string; var Option: boolean);
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
    property sOption: boolean read FsOption write FsOption;
    property fOption: string read FfOption write SetfOption;
    property sfxOption: string read FsfxOption write SetsfxOption;
    property pOption: string read FpOption write SetpOption;
    property tOption: boolean read FtOption write FtOption;
    property lOption: boolean read FlOption write FlOption;
    property slsOption: boolean read FslsOption write FslsOption;
    property wdOption: string read FwdOption write SetwdOption;
    property cdOption: string read FcdOption write SetcdOption;
    property cfgOption: string read FcfgOption write SetcfgOption;
    property priOption: TpriOption read FpriOption write FpriOption;
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

implementation

uses
  {$IFNDEF FPC}
  Math,
  {$ENDIF}
  Bee_BlowFish;

constructor TCommandLine.Create;
begin
  inherited Create;
  FxOptions := TStringList.Create;
  FFileMasks := TStringList.Create;
  Clear;
end;

procedure TCommandLine.Clear;
begin
  FCommand := ccNone;
  FssOption := False;
  FrOption := rmNone;
  FuOption := umAddUpdate;
  FxOptions.Clear;
  FmOption := moFast;
  FdOption := do10MB;
  FsOption := False;
  FfOption := '';
  FsfxOption := '';
  FpOption := '';
  FtOption := False;
  FlOption := False;
  FslsOption := False;
  FwdOption := '';
  FcdOption := '';
  FcfgOption := SelfPath + DefaultCfgName;
  FpriOption := prioNormal;
  FArchiveName := '';
  FFileMasks.Clear;
end;

destructor TCommandLine.Destroy;
begin
  FxOptions.Destroy;
  FFileMasks.Destroy;
  inherited Destroy;
end;

procedure TCommandLine.ProcessOption(var S: string; var Option: boolean);
begin
  if Length(S) <> 1 then
  begin
    Delete(S, 1, 2);
    if (S = '') or (S = '+') then
      Option := True
    else
      if (S = '-') then Option := False;
  end;
end;

procedure TCommandLine.ProcessrOption(var S: string);
begin
  if Pos('-RW', UpperCase(S)) = 1 then
  begin
    Delete(S, 1, 3);
    if (S = '') or (S = '+') then
      FrOption := rmWildCard
    else
      if (S = '-') then FrOption := rmNone;
  end else
    if Pos('-R', UpperCase(S)) = 1 then
    begin
      Delete(S, 1, 2);
      if (S = '') or (S = '+') then
        FrOption := rmFull
      else
        if (S = '-') then FrOption := rmNone;
    end;
end;

procedure TCommandLine.ProcessuOption(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'7']) then
  begin
    FuOption := TUpdateMode(StrToInt(S[1]));
  end;
end;

procedure TCommandLine.ProcessxOption(var S: string);
begin
  Delete(S, 1, 2);
  if Length(S) <> 0 then
  begin
    FxOptions.Add(S);
  end;
end;

procedure TCommandLine.ProcesspOption(var S: string);
begin
  Delete(S, 1, 2);
  SetpOption(S)
end;

procedure TCommandLine.ProcessmOption(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'3']) then
  begin
    FmOption := TmOption(StrToInt(S[1]));
  end;
end;

procedure TCommandLine.ProcessdOption(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'9']) then
  begin
    FdOption := TdOption(StrToInt(S[1]));
  end;
end;

procedure TCommandLine.ProcessfOption(var S: string);
begin
  Delete(S, 1, 2);
  SetfOption(S);
end;

procedure TCommandLine.ProcesssfxOption(var S: string);
begin
  Delete(S, 1, 4);
  if (S = '+') or (Length(S) = 0) then
    FsfxOption := ExtractFilePath(ParamStr(0)) + DefaultSfxName
  else
    if (S = '-') then
      FsfxOption := 'nul'
    else
      FsfxOption := ExtractFilePath(ParamStr(0)) + S;
end;

procedure TCommandLine.ProcessslsOption(var S: string);
begin
  Delete(S, 1, 4);
  if (S = '') or (S = '+') then
    FslsOption := True
  else
    if (S = '-') then
      FslsOption := False;
end;

procedure TCommandLine.ProcesswdOption(var S: string);
begin
  Delete(S, 1, 3);
  if DirectoryExists(ExcludeTrailingBackslash(S)) then
  begin
    FwdOption := ExcludeTrailingBackslash(S);
  end;
end;

procedure TCommandLine.ProcesscdOption(var S: string);
begin
  Delete(S, 1, 3);
  if Length(S) <> 0 then
  begin
    FcdOption := IncludeTrailingBackslash(FixDirName(S));
  end;
end;

procedure TCommandLine.ProcesscfgOption(var S: string);
begin
  Delete(S, 1, 4);
  if FileExists(S) then
  begin
    FcfgOption := S;
  end;
end;

procedure TCommandLine.ProcesspriOption(var S: string);
begin
  Delete(S, 1, 4);
  if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
  begin
    FpriOption := TpriOption(StrToInt(S[1]));
  end;
end;

procedure TCommandLine.ProcessCommand(var S: string);
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
      else FCommand := ccNone;
    end;
end;

procedure TCommandLine.ProcessArchiveName(var S: string);
begin
  FssOption := True;
  FArchiveName := S;
  if ExtractFileExt(FArchiveName) = '' then
  begin
    FArchiveName := ChangeFileExt(FArchiveName, '.bee');
  end;
end;

procedure TCommandLine.SetCommandLine(const aValue: string);
var
  I: longint;
  S: string;
  Params: TStringList;
begin
  Params := TStringList.Create;
  Params.Text := aValue;
  
  Clear;
  // catch options, command, archive name and name of files
  for I := 0 to Params.Count - 1 do
  begin
    S := Params[I];
    if (not FssOption) and (Length(S) <> 1) and (S[1] = '-') then
    begin
      // options...
      if Pos('-SLS', UpperCase(S)) = 1 then
        ProcessslsOption(S)
      else
      if Pos('-SFX', UpperCase(S)) = 1 then
        ProcesssfxOption(S)
      else
      if Pos('-PRI', UpperCase(S)) = 1 then
        ProcesspriOption(S)
      else
      if Pos('-CFG', UpperCase(S)) = 1 then
        ProcesscfgOption(S)
      else
      if Pos('-WD', UpperCase(S)) = 1 then
        ProcesswdOption(S)
      else
      if Pos('-CD', UpperCase(S)) = 1 then
        ProcesscdOption(S)
      else
      case UpCase(S[2]) of
        '-': ProcessOption (S, FssOption);
        'R': ProcessrOption(S);
        'U': ProcessuOption(S);
        'X': ProcessxOption(S);
        'M': ProcessmOption(S);
        'D': ProcessdOption(S);
        'S': ProcessOption (S, FsOption);
        'F': ProcessfOption(S);
        'P': ProcesspOption(S);
        'T': ProcessOption (S, FtOption);
        'L': ProcessOption (S, FlOption);
      end; // end case
    end else

    if FCommand = ccNone then
      ProcessCommand(S)
    else
      if FArchiveName = '' then
        ProcessArchiveName(S)
      else
        FFileMasks.Add(S); // command or filenames ...

  end; // end for loop
  Params.Free;

  // check file masks
  if FFileMasks.Count = 0 then
  begin
    case FCommand of
     {ccAdd: nothing to do}
     {ccDelete:  nothing to do}
      ccExtract: FFileMasks.Add('*');
      ccList: FFileMasks.Add('*');
     {ccRename: nothing to do}
      ccTest: FFileMasks.Add('*');
      ccxExtract: FFileMasks.Add('*');
     {ccHelp:  nothing to do}
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
     ccRename:  Params.Add('R');
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

  if FsOption then Params.Add('-s+') else Params.Add('-s-');

  if Length(FfOption)   > 0 then Params.Add('-f'   + FfOption);
  if Length(FsfxOption) > 0 then Params.Add('-sfx' + FsfxOption);
  if Length(FpOption)   > 0 then Params.Add('-p'   + FpOption);

  if FtOption   then Params.Add('-t+')   else Params.Add('-t-');
  if FlOption   then Params.Add('-l+')   else Params.Add('-l-');
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

procedure TCommandLine.SetfOption(const aValue: string);
begin
  if ExtractFileExt('.' + aValue) <> '.' then
  begin
    FfOption := ExtractFileExt('.' + aValue);
  end;
end;

procedure TCommandLine.SetpOption(const aValue: string);
begin
  if (Length(aValue) = 0) or (Length(aValue) >= MinBlowFishKeyLength) then
  begin
    FpOption := aValue;
  end;
end;

procedure TCommandLine.SetsfxOption(const aValue: string);
begin
  if FileExists(aValue) then
    FsfxOption := aValue
  else
    FsfxOption := 'nul';
end;

procedure TCommandLine.SetwdOption(const aValue: string);
begin
  if DirectoryExists(aValue) then
  begin
    FwdOption := aValue;
  end;
end;

procedure TCommandLine.SetcdOption(const aValue: string);
begin
  if Length(aValue) > 0 then
    FcdOption := IncludeTrailingBackSlash(FixDirName(aValue));
end;

procedure TCommandLine.SetcfgOption(const aValue: string);
begin
  if FileExists(aValue) then
  begin
    FcfgOption := aValue;
  end;
end;

procedure TCommandLine.SetArchiveName(const aValue: string);
begin
  FArchiveName := aValue;
end;

end.
