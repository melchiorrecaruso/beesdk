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

    v0.8.0 build 1042 - 2009.08.20 by Melchiorre Caruso.
}

unit Bee_CommandLine;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils;

type
  TCommandLine = class
  private
    FCommand:     char;
    FrOption:     boolean;
    FuOption:     boolean;
    FfOption:     boolean;
    FeOption:     string;
    FsOption:     boolean;
    FaOption:     string;
    FoOption:     char;
    FmOption:     longint;
    FdOption:     longint;
    FxOption:     TStringList;
    FtOption:     boolean;
    FlOption:     boolean;
    FyOption:     string;
    FkOption:     boolean;
    FvOption:     boolean;
    FcdOption:    string;
    FsoOption:    boolean;
    FcfgOption:   string;
    FpriOption:   longint;
    FverOption:   byte;
    FArchiveName: string;
    FFileMasks:   TStringList;
  private
    procedure SetCommand(Value: char);
    procedure SetrOption(Value: boolean);
    procedure SetuOption(Value: boolean);
    procedure SetfOption(Value: boolean);
    procedure SeteOption(Value: string);
    procedure SetsOption(Value: boolean);
    procedure SetaOption(Value: string);
    procedure SetoOption(Value: char);
    procedure SetmOption(Value: longint);
    procedure SetdOption(Value: longint);
    procedure SettOption(Value: boolean);
    procedure SetlOption(Value: boolean);
    procedure SetyOption(Value: string);
    procedure SetkOption(Value: boolean);
    procedure SetvOption(Value: boolean);
    procedure SetcdOption(Value: string);
    procedure SetcfgOption(Value: string);
    procedure SetpriOption(Value: longint);
    procedure SetverOption(Value: byte);
    procedure SetArchiveName(Value: string);
  protected
    procedure ProcessOption(var S: string; var Option: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process(AParams: TStringList);
    procedure Clear;
  public
    property Command: char Read FCommand Write SetCommand;
    property rOption: boolean Read FrOption Write SetrOption;
    property uOption: boolean Read FuOption Write SetuOption;
    property fOption: boolean Read FfOption Write SetfOption;
    property eOption: string Read FeOption Write SeteOption;
    property sOption: boolean Read FsOption Write SetsOption;
    property aOption: string Read FaOption Write SetaOption;
    property oOption: char Read FoOption Write SetoOption;
    property mOption: longint Read FmOption Write SetmOption;
    property dOption: longint Read FdOption Write SetdOption;
    property xOption: TStringList Read FxOption;
    property tOption: boolean Read FtOption Write SettOption;
    property lOption: boolean Read FlOption Write SetlOption;
    property yOption: string Read FyOption Write SetyOption;
    property kOption: boolean Read FkOption Write SetkOption;
    property vOption: boolean Read FvOption Write SetvOption;
    property cdOption: string Read FcdOption Write SetcdOption;
    property soOption: boolean Read FsoOption;
    property cfgOption: string Read FcfgOption Write SetcfgOption;
    property priOption: longint Read FpriOption Write SetpriOption;
    property verOption: byte Read FverOption Write SetverOption;
    property ArchiveName: string Read FArchiveName Write SetArchiveName;
    property FileMasks: TStringList Read FFileMasks;
  end;

implementation

uses
  Math,
  Bee_Consts,
  Bee_Common;

constructor TCommandLine.Create;
begin
  inherited Create;
  FxOption := TStringList.Create;
  FFileMasks := TStringList.Create;
  Clear;
end;

procedure TCommandLine.Clear;
begin
  FCommand := ' ';
  FrOption := False;
  FuOption := False;
  FfOption := False;
  FeOption := '';
  FsOption := False;
  FaOption := '';
  FoOption := 'Y';
  FmOption := 1;
  FdOption := 2;
  FxOption.Clear;
  FtOption := False;
  FlOption := False;
  FyOption := '';
  FkOption := False;
  FvOption := False;
  FcdOption := '';
  FsoOption := False;
  FcfgOption := SelfPath + 'bee.ini';
  FpriOption := 1;
  FverOption := ver03;
  FArchiveName := '';
  FFileMasks.Clear;
end;

destructor TCommandLine.Destroy;
begin
  FxOption.Clear;
  FxOption.Destroy;
  FFileMasks.Clear;
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

procedure TCommandLine.Process(AParams: TStringList);
var
  I: longint;
  S: string;
begin
  // catch options, command, archive name and name of files
  for I := 0 to AParams.Count - 1 do
  begin
    S := AParams.Strings[I];
    if (not FsoOption) and (Length(S) > 1) and (S[1] = '-') then
    begin
      // options...
      case UpCase(S[2]) of
        '-': FsoOption := True;
        'R': ProcessOption(S, FrOption);
        'S': ProcessOption(S, FsOption);
        'U': ProcessOption(S, FuOption);
        'F': ProcessOption(S, FfOption);
        'T': ProcessOption(S, FtOption);
        'L': ProcessOption(S, FlOption);
        'K': ProcessOption(S, FkOption);
        'V': ProcessOption(S, FvOption);
        'Y':
        begin
          Delete(S, 1, 2);
          if DirectoryExists(ExcludeTrailingBackslash(S)) then
            FyOption := ExcludeTrailingBackslash(S);
        end;
        'A':
        begin
          Delete(S, 1, 2);
          if (S = '+') or (Length(S) = 0) then
            FaOption := 'bee.sfx'
          else
          if (S = '-') then
            FaOption := 'nul'
          else
            FaOption := S;
        end;
        'M':
        begin
          Delete(S, 1, 2);
          if (Length(S) = 1) and (S[1] in ['0'..'3']) then
            FmOption := StrToInt(S);
        end;
        'O':
        begin
          Delete(S, 1, 2);
          if (Length(S) = 1) and (UpCase(S[1]) in ['A', 'S', 'Q']) then
            FoOption := UpCase(S[1]);
        end;
        'D':
        begin
          Delete(S, 1, 2);
          if (Length(S) = 1) and (S[1] in ['0'..'9']) then
            FdOption := StrToInt(S);
        end;
        'E':
        begin
          Delete(S, 1, 2);
          if ExtractFileExt('.' + S) <> '.' then
            FeOption := ExtractFileExt('.' + S);
        end;
        'X':
        begin
          Delete(S, 1, 2);
          if Length(S) > 0 then
            FxOption.Add(S);
        end;
        else
          if Pos('-VER', UpperCase(S)) = 1 then
          begin
            Delete(S, 1, 4);
            if S = '02' then FverOption := ver02 else
              if S = '03' then FverOption := ver03 else
                if S = '04' then FverOption := ver04;
          end
          else
          if Pos('-PRI', UpperCase(S)) = 1 then
          begin
            Delete(S, 1, 4);
            if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
              FpriOption := StrToInt(S[1]);
          end
          else
          if Pos('-CD', UpperCase(S)) = 1 then
          begin
            Delete(S, 1, 3);
            if Length(S) > 0 then
              FcdOption := IncludeTrailingBackslash(FixDirName(S));
          end
          else
          if Pos('-CFG', UpperCase(S)) = 1 then
          begin
            Delete(S, 1, 4);
            FcfgOption := S;
          end;
      end; // end case
    end else
      if FCommand = ' ' then
      begin
        if Length(S) = 1 then
          FCommand := UpCase(S[1])
        else
          FCommand := '?';
      end else
        if FArchiveName = '' then
        begin
          FsoOption := True;
          FArchiveName := S;
          if ExtractFileExt(FArchiveName) = '' then
            FArchiveName := ChangeFileExt(FArchiveName, '.bee');
        end else
          FFileMasks.Add(S);// command or filenames...
  end; // end for loop

  // process file masks
  if FFileMasks.Count = 0 then
  begin
    case FCommand of
      {'a': nothing to do}
      {'D': nothing to do}
      'E': FFileMasks.Add('*');
      'L': FFileMasks.Add('*');
      {'R': nothing to do}
      'T': FFileMasks.Add('*');
      'X': FFileMasks.Add('*');
      {'?': nothing to do}
    end;
    FrOption := True;
  end;
end;

procedure TCommandLine.SetCommand(Value: char);
begin
  FCommand := UpCase(Value);
end;

procedure TCommandLine.SetrOption(Value: boolean);
begin
  FrOption := Value;
end;

procedure TCommandLine.SetuOption(Value: boolean);
begin
  FuOption := Value;
end;

procedure TCommandLine.SetfOption(Value: boolean);
begin
  FfOption := Value;
end;

procedure TCommandLine.SeteOption(Value: string);
begin
  FeOption := Value;
end;

procedure TCommandLine.SetsOption(Value: boolean);
begin
  FsOption := Value;
end;

procedure TCommandLine.SetaOption(Value: string);
begin
  FaOption := Value;
end;

procedure TCommandLine.SetoOption(Value: char);
begin
  FoOption := UpCase(Value);
end;

procedure TCommandLine.SetmOption(Value: longint);
begin
  FmOption := Value;
end;

procedure TCommandLine.SetdOption(Value: longint);
begin
  FdOption := Value;
end;

procedure TCommandLine.SettOption(Value: boolean);
begin
  FtOption := Value;
end;

procedure TCommandLine.SetlOption(Value: boolean);
begin
  FlOption := Value;
end;

procedure TCommandLine.SetyOption(Value: string);
begin
  FyOption := Value;
end;

procedure TCommandLine.SetkOption(Value: boolean);
begin
  FkOption := Value;
end;

procedure TCommandLine.SetvOption(Value: boolean);
begin
  FvOption := Value;
end;

procedure TCommandLine.SetcdOption(Value: string);
begin
  FcdOption := IncludeTrailingBackslash(FixDirName(Value));
end;

procedure TCommandLine.SetcfgOption(Value: string);
begin
  FcfgOption := Value;
end;

procedure TCommandLine.SetpriOption(Value: longint);
begin
  FpriOption := Value;
end;

procedure TCommandLine.SetverOption(Value: byte);
begin
  FverOption := Min(Max(ver02, Value), ver04);
end;

procedure TCommandLine.SetArchiveName(Value: string);
begin
  FArchiveName := Value;
end;

end.