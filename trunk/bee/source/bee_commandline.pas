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

  TCommandLine class.

  Modifyed:

  v0.7.9 build 0890 - 2008.10.18 by Melchiorre Caruso.
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
    FmOption:     integer;
    FdOption:     integer;
    FxOption:     TStringList;
    FtOption:     boolean;
    FlOption:     boolean;
    FyOption:     string;
    FkOption:     boolean;
    FcdOption:    string;
    FcfgOption:   string;
    FpriOption:   integer;
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
    procedure SetmOption(Value: integer);
    procedure SetdOption(Value: integer);
    procedure SettOption(Value: boolean);
    procedure SetlOption(Value: boolean);
    procedure SetyOption(Value: string);
    procedure SetkOption(Value: boolean);
    procedure SetcdOption(Value: string);
    procedure SetcfgOption(Value: string);
    procedure SetpriOption(Value: integer);
    procedure SetArchiveName(Value: string);
  protected
    procedure ProcessOption(var S: string; var Option: boolean);
  public
    constructor Create; dynamic;
    destructor Destroy; dynamic;
    procedure Process(Params: TStringList); dynamic;
    procedure Clear; dynamic;
  public
    property Command: char Read FCommand Write SetCommand;
    property rOption: boolean Read FrOption Write SetrOption;
    property uOption: boolean Read FuOption Write SetuOption;
    property fOption: boolean Read FfOption Write SetfOption;
    property eOption: string Read FeOption Write SeteOption;
    property sOption: boolean Read FsOption Write SetsOption;
    property aOption: string Read FaOption Write SetaOption;
    property oOption: char Read FoOption Write SetoOption;
    property mOption: integer Read FmOption Write SetmOption;
    property dOption: integer Read FdOption Write SetdOption;
    property xOption: TStringList Read FxOption;
    property tOption: boolean Read FtOption Write SettOption;
    property lOption: boolean Read FlOption Write SetlOption;
    property yOption: string Read FyOption Write SetyOption;
    property kOption: boolean Read FkOption Write SetkOption;
    property cdOption: string Read FcdOption Write SetcdOption;
    property cfgOption: string Read FcfgOption Write SetcfgOption;
    property priOption: integer Read FpriOption Write SetpriOption;
    property ArchiveName: string Read FArchiveName Write SetArchiveName;
    property FileMasks: TStringList Read FFileMasks;
  end;

implementation

uses
  Bee_Common;

constructor TCommandLine.Create;
begin
  inherited Create;
  FxOption   := TStringList.Create;
  FFileMasks := TStringList.Create;
  // ---
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
  FtOption     := False;
  FlOption     := False;
  FyOption     := '';
  FkOption     := False;
  FcdOption    := '';
  FcfgOption   := SelfPath + 'bee.ini';
  FpriOption   := 1;
  FArchiveName := '';
  FFileMasks.Clear;
end;

destructor TCommandLine.Destroy;
begin
  FxOption.Clear;
  FxOption.Free;
  FFileMasks.Clear;
  FFileMasks.Free;
  inherited Destroy;
end;

procedure TCommandLine.ProcessOption(var S: string; var Option: boolean);
begin
  if Length(S) > 1 then
  begin
    Delete(S, 1, 2);
    if (S = '') or (S = '+') then Option := True
    else
      if (S = '-') then Option := False;
  end;
end;

procedure TCommandLine.Process(Params: TStringList);
var
  I: integer;
  S: string;
begin
  // catch options, command, archive name and name of files
  for I := 0 to Params.Count - 1 do
  begin
    S := Params.Strings[I];
    if (FArchiveName = '') and (Length(S) > 1) and (S[1] = '-') then
    begin
      // options...
      case UpCase(S[2]) of
        'S': ProcessOption(S, FsOption);
        'U': ProcessOption(S, FuOption);
        'F': ProcessOption(S, FfOption);
        'T': ProcessOption(S, FtOption);
        'L': ProcessOption(S, FlOption);
        'K': ProcessOption(S, FkOption);
        'R': ProcessOption(S, FrOption);
        'Y':
        begin
          Delete(S, 1, 2);
          if DirectoryExists(ExcludeTrailingBackslash(S)) then
          begin
            FyOption := ExcludeTrailingBackslash(S);
          end;
        end;
        'A':
        begin
          Delete(S, 1, 2);
          if (S = '+') or (Length(S) = 0) then FaOption := 'bee.sfx'
          else
            if (S = '-') then FaOption := 'nul'
            else
              FaOption := S;
        end;
        'M':
        begin
          Delete(S, 1, 2);
          if (Length(S) = 1) and (S[1] in ['0'..'3']) then
          begin
            FmOption := StrToInt(S);
          end;
        end;
        'O':
        begin
          Delete(S, 1, 2);
          if (Length(S) = 1) and (UpCase(S[1]) in ['A', 'S', 'Q']) then
          begin
            FoOption := UpCase(S[1]);
          end;
        end;
        'D':
        begin
          Delete(S, 1, 2);
          if (Length(S) = 1) and (S[1] in ['0'..'9']) then
          begin
            FdOption := StrToInt(S);
          end;
        end;
        'E':
        begin
          Delete(S, 1, 2);
          if ExtractFileExt('.' + S) <> '.' then
          begin
            FeOption := ExtractFileExt('.' + S);
          end;
        end;
        'X':
        begin
          Delete(S, 1, 2);
          if Length(S) > 0 then
          begin
            FxOption.Add(S);
          end;
        end;
        else if Pos('-PRI', UpperCase(S)) = 1 then
          begin
            Delete(S, 1, 4);
            if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
            begin
              FpriOption := StrToInt(S[1]);
            end;
          end
          else begin
            if Pos('-CD', UpperCase(S)) = 1 then
            begin
              Delete(S, 1, 3);
              if Length(S) > 0 then
              begin
                FcdOption := IncludeTrailingBackslash(FixDirName(S));
              end;
            end
            else begin
              if Pos('-CFG', UpperCase(S)) = 1 then
              begin
                Delete(S, 1, 4);
                FcfgOption := S;
              end;
            end;
          end;
      end; // end case
    end
    else begin
      // command or filenames...
      if FCommand = ' ' then
      begin
        if Length(S) = 1 then FCommand := UpCase(S[1])
        else
          FCommand := '?';
      end
      else
        if FArchiveName = '' then
        begin
          FArchiveName := S;
          if ExtractFileExt(FArchiveName) = '' then
          begin
            FArchiveName := ChangeFileExt(FArchiveName, '.bee');
          end;
        end
        else
          FFileMasks.Add(S);
    end;
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
  FCommand := Value;
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
  FoOption := Value;
end;

procedure TCommandLine.SetmOption(Value: integer);
begin
  FmOption := Value;
end;

procedure TCommandLine.SetdOption(Value: integer);
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

procedure TCommandLine.SetcdOption(Value: string);
begin
  FcdOption := Value;
end;

procedure TCommandLine.SetcfgOption(Value: string);
begin
  FcfgOption := Value;
end;

procedure TCommandLine.SetpriOption(Value: integer);
begin
  FpriOption := Value;
end;

procedure TCommandLine.SetArchiveName(Value: string);
begin
  FArchiveName := Value;
end;

end.
