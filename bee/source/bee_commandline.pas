{
  Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1100 - 2009.12.07 by Melchiorre Caruso.
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
  //   ccNone     Nul command
  //   ccHelp     Show help informations
  //   ccAdd      Add files
  //   ccExtract  Extract file
  //   ceXextract Extract file with full path
  //   ccDelete   Delete files
  //   ccRename   Rename files
  //   ccTest     Test files
  //   ccList     List files
  //   ccOpen     Open archive

  TCommand = (ccAdd, ccExtract, ccXextract, ccDelete,
    ccRename, ccTest, ccList, ccHelp, ccOpen, ccNone);

  // Compression Method Option:
  //   moStore
  //   moFast
  //   moNormal
  //   moMaximum

  TmOption = (moStore, moFast, moNormal, moMaximum);

  // Compression Dictionary Option:
  //   do2MB
  //   do5MB
  //   ..
  //   do1280MB

  TdOption = (do2MB, do5MB, do10MB, do20MB, do40MB,
    do80MB, do160MB, do320MB, do640MB ,do1280MB);

  // Process Priority Option
  //   prioIdle
  //   prioNormal
  //   prioHigh
  //   prioRealTime

  // Header Version:
  //   hv02
  //   hv03
  //   hv04

  THeaderVersion = (hv02, hv03, hv04);  

  TpriOption = (prioIdle, prioNormal, prioHigh, prioRealTime);

  // TCommandLine ...

  TCommandLine = class
  private
    // command
    FCommand: TCommand;
    // options
    FssOption: boolean;
    FrOption: TRecursiveMode;
    FuOption: TUpdateMode;
    FxOptions: TStringList;
    FmOption: TmOption;
    FdOption: TdOption;
    FsOption: boolean;
    FfOption: string;
    FsfxOption: string;
    FpOption: boolean;
    FhvOption: THeaderVersion;
    FtOption: boolean;
    FlOption: boolean;
    FstlOption: boolean;
    FwdOption: string;
    FcdOption: string;
    FcfgOption: string;
    FpriOption: TpriOption;
    // archive name
    FArchiveName: string;
    // file masks
    FFileMasks: TStringList;
  private
    procedure Initialize;
    function GetCommandLine: string;
    procedure SetCommandLine(const aValue: string);
  private
    procedure SetfOption(const aValue: string);
    procedure SetsfxOption(const aValue: string);
    procedure SetwdOption(const aValue: string);
    procedure SetcdOption(const aValue: string);
    procedure SetcfgOption(const aValue: string);
    procedure SetArchiveName(const aValue: string);
  private
    procedure ProcessrOption(var S: string);
    procedure ProcessuOption(var S: string);
    procedure ProcessxOption(var S: string);
    procedure ProcessmOption(var S: string);
    procedure ProcessdOption(var S: string);
    procedure ProcessfOption(var S: string);
    procedure ProcesssfxOption(var S: string);
    procedure ProcesshvOption(var S: string);
    procedure ProcessstlOption(var S: string);
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
  public
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property Command: TCommand read FCommand;
    property ssOption: boolean read FssOption write FssOption;
    property rOption: TRecursiveMode read FrOption write FrOption;
    property uOption: TUpdateMode read FuOption write FuOption;
    property xOptions: TStringList read FxOptions;
    property mOption: TmOption read FmOption write FmOption;
    property dOption: TdOption read FdOption write FdOption;
    property sOption: boolean read FsOption write FsOption;
    property fOption: string read FfOption write SetfOption;
    property sfxOption: string read FsfxOption write SetsfxOption;
    property pOption: boolean read FpOption write FpOption;
    property hvOption: THeaderVersion read FhvOption write FhvOption;
    property tOption: boolean read FtOption write FtOption;
    property lOption: boolean read FlOption write FlOption;
    property stlOption: boolean read FstlOption write FstlOption;
    property wdOption: string read FwdOption write SetwdOption;
    property cdOption: string read FcdOption write SetcdOption;
    property cfgOption: string read FcfgOption write SetcfgOption;
    property priOption: TpriOption read FpriOption write FpriOption;
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

implementation

uses
  Math;

constructor TCommandLine.Create;
begin
  inherited Create;
  FxOptions := TStringList.Create;
  FFileMasks := TStringList.Create;
  Initialize;
end;

procedure TCommandLine.Initialize;
begin
  // default command
  FCommand := ccNone;
  // default options
  FssOption := False;
  FrOption := rmNone;
  FuOption := umAddUpdate;
  FxOptions.Clear;
  FmOption := moFast;
  FdOption := do5MB;
  FsOption := False;
  FfOption := '';
  FsfxOption := '';
  FpOption := False;
  FhvOption := hv04;
  FtOption := False;
  FlOption := False;
  FstlOption := False;
  FwdOption := '';
  FcdOption := '';
  FcfgOption := SelfPath + DefaultCfgName;
  FpriOption := prioNormal;
  // archive name
  FArchiveName := '';
  // file masks
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
  if Length(S) > 1 then
  begin
    Delete(S, 1, 2);
    if (S = '') or (S = '+') then
      Option := True
    else
      if (S = '-') then
        Option := False;
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
      if (S = '-') then
        FrOption := rmNone;
  end else
    if Pos('-R', UpperCase(S)) = 1 then
    begin
      Delete(S, 1, 2);
      if (S = '') or (S = '+') then
        FrOption := rmFull
      else
        if (S = '-') then
          FrOption := rmNone;
    end;
end;

procedure TCommandLine.ProcessuOption(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'5']) then
  begin
    FuOption := TUpdateMode(S[1]);
  end;
end;

procedure TCommandLine.ProcessxOption(var S: string);
begin
  Delete(S, 1, 2);
  if Length(S) > 0 then
  begin
    FxOptions.Add(S);
  end;
end;

procedure TCommandLine.ProcessmOption(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'3']) then
  begin
    FmOption := TmOption(S[1]);
  end;
end;

procedure TCommandLine.ProcessdOption(var S: string);
begin
  Delete(S, 1, 2);
  if (Length(S) = 1) and (S[1] in ['0'..'9']) then
  begin
    FdOption := TdOption(S[1]);
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
    FsfxOption := DefaultSfxName
  else
    if (S = '-') then
      FsfxOption := 'nul'
    else
      FsfxOption := S;
end;

procedure TCommandLine.ProcesshvOption(var S: string);
begin
  Delete(S, 1, 3);
  if S = '02' then
    FhvOption := hv02
  else
    if S = '03' then
      FhvOption := hv03
    else
      if S = '04' then
        FhvOption := hv04;
end;

procedure TCommandLine.ProcessstlOption(var S: string);
begin
  Delete(S, 1, 4);
  if (S = '') or (S = '+') then
    stlOption := True
  else
    if (S = '-') then
      stlOption := False;
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
  if Length(S) > 0 then
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
    FpriOption := TpriOption(S[1]);
  end;
end;

procedure TCommandLine.ProcessCommand(var S: string);
begin
  if Length(S) = 1 then
  begin
    case Upcase(S[1]) of
      '?': FCommand := ccHelp;
      'A': FCommand := ccAdd;
      'D': FCommand := ccDelete;
      'E': FCommand := ccExtract;
      'X': FCommand := ccxExtract;
      'L': FCommand := ccList;
      'T': FCommand := ccTest;
      'R': FCommand := ccRename;
      'O': FCommand := ccOpen;
      else FCommand := ccHelp;
    end;
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
  Params: TStringList;
  I: longint;
  S: string;
begin
  Params := TStringList.Create;
  Params.Text := aValue;
  
  Initialize;
  // catch options, command, archive name and name of files
  for I := 0 to Params.Count -1 do
  begin
    S := Params[I];
    if (not FssOption) and (Length(S) > 1) and (S[1] = '-') then
    begin
      // options...
      case UpCase(S[2]) of
        '-': ProcessOption (S, FssOption);
        'R': ProcessrOption(S);
        'U': ProcessuOption(S);
        'X': ProcessxOption(S);
        'M': ProcessmOption(S);
        'D': ProcessdOption(S);
        'S': ProcessOption (S, FsOption);
        'F': ProcessfOption(S);
        'P': ProcessOption (S, FpOption);
        'T': ProcessOption (S, FtOption);
        'L': ProcessOption (S, FlOption);
        else
          if Pos('-SFX', UpperCase(S)) = 1 then
            ProcesssfxOption(S)
          else
          if Pos('-HV', UpperCase(S)) = 1 then
            ProcesshvOption(S)
          else
          if Pos('-STL', UpperCase(S)) = 1 then
            ProcessstlOption(S)
          else
          if Pos('-WD', UpperCase(S)) = 1 then
            ProcesswdOption(S)
          else
          if Pos('-CD', UpperCase(S)) = 1 then
            ProcesscdOption(S)
          else
          if Pos('-CFG', UpperCase(S)) = 1 then
            ProcesscfgOption(S)
          else
          if Pos('-PRI', UpperCase(S)) = 1 then
            ProcesspriOption(S);
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
begin
  case FCommand of
    ccHelp:     Result := '?';
    ccAdd:      Result := 'A';
    ccDelete:   Result := 'D';
    ccExtract:  Result := 'E';
    ccxExtract: Result := 'X';
    ccList:     Result := 'L';
    ccTest:     Result := 'T';
    ccRename:   Result := 'R';
    ccOpen :    Result := 'O';
    else       Result := ' ';
  end;

  case FrOption of
    rmFull:     Result := Result + ' -r+';
    rmWildCard: Result := Result + ' -rw+';
    else        Result := Result + ' -r-'
  end;

  case FuOption of
    umAdd:        Result := Result + ' -u0';
    umUpdate:     Result := Result + ' -u1';
    umReplace:    Result := Result + ' -u2';
    umAddUpdate:  Result := Result + ' -u3';
    umAddReplace: Result := Result + ' -u4';
    umAddQuery:   Result := Result + ' -u5';
  end;

  for I := 0 to FxOptions.Count - 1 do
    Result := Result + ' -x' + FxOptions[I];

  Result := Result + ' -m' + IntToStr(Ord(FmOption));
  Result := Result + ' -d' + IntToStr(Ord(FdOption));

  if FsOption then
    Result := Result + ' -s+'
  else
    Result := Result + ' -s-';

  if Length(FfOption)   > 0 then Result := Result + ' -f'   + FfOption;      
  if Length(FsfxOption) > 0 then Result := Result + ' -sfx' + FsfxOption;

  if FpOption then
    Result := Result + ' -p+'
  else
    Result := Result + ' -p-';

  case FhvOption of
    hv02: Result := Result + ' -hv02';
    hv03: Result := Result + ' -hv03';
    hv04: Result := Result + ' -hv04';
  end;

  if FtOption then Result := Result + ' -t+' else Result := Result + ' -t-';
  if FlOption then Result := Result + ' -l+' else Result := Result + ' -l-';

  if FstlOption then
    Result := Result + ' -stl+'
  else
    Result := Result + ' -stl-';

  if Length(FwdOption)  > 0 then Result := Result + ' -wd'  + FwdOption;
  if Length(FcdOption)  > 0 then Result := Result + ' -cd'  + FcdOption;
  if Length(FcfgOption) > 0 then Result := Result + ' -cfg' + FcfgOption;


  Result := Result + ' -pri' + IntToStr(Ord(FpriOption));

  if FssOption then
    Result := Result + ' --+'
  else
    Result := Result + ' ---';

  Result := Result + ' ' + FArchiveName;

  for I := 0 to FFileMasks.Count - 1 do
    Result := Result + ' ' + FFileMasks[I];
end;

procedure TCommandLine.SetfOption(const aValue: string);
begin
  if ExtractFileExt('.' + aValue) <> '.' then
  begin
    FfOption := ExtractFileExt('.' + aValue);
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
  if Bee_Common.DirectoryExists(aValue) then
  begin
    FwdOption := aValue;
  end;
end;

procedure TCommandLine.SetcdOption(const aValue: string);
begin
  FcdOption := IncludeTrailingBackslash(FixDirName(aValue));
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
