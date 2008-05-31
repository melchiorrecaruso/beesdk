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
  SysUtils; 

type
   
  { TCommandLine Application class }

  TCmdLine = class
  private
    FCommand: char;
    FrOption: boolean;
    FuOption: boolean;
    FfOption: boolean;
    FeOption: string;
    FsOption: boolean;
    FaOption: string;
    FoOption: char;
    FmOption: integer;
    FdOption: integer;
    FxOption: TStringList;
    FtOption: boolean;
    FlOption: boolean;
    FyOption: string;
    FkOption: boolean;
    FcdOption: string;
    FcfgOption: string;
    FpriOption: integer;
    FArcName:  string;
    FFileMasks: TStringList;
  private
    FRun: boolean;
    F0Option: string;
    F1Option: boolean;
    F2Option: boolean;
    FParams: TStringList;
  private
    procedure ProcessOptions;
    procedure ProcessParams;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Run: boolean read FRun;
    property Link: string read F0Option;
    property Log: boolean read F1Option;
    property Params: TStringList read FParams;
  public
    property Command: char read FCommand write FCommand;
    property rOption: boolean read FrOption write FrOption;
    property uOption: boolean read FuOption write FuOption;
    property fOption: boolean read FfOption write FfOption;
    property eOption: string read FeOption write FeOption;
    property sOption: boolean read FsOption write FsOption;
    property aOption: string read FaOption write FaOption;
    property oOption: char read FoOption write FoOption;
    property mOption: integer read FmOption write FmOption;
    property dOption: integer read FdOption write FdOption;
    property xOption: TStringList read FxOption;
    property tOption: boolean read FtOption write FtOption;
    property lOption: boolean read FlOption write FlOption;
    property yOption: string read FyOption write FyOption;
    property kOption: boolean read FkOption write FkOption;
    property cdOption: string read FcdOption write FcdOption;
    property cfgOption: string read FcfgOption write FcfgOption;
    property priOption: integer read FpriOption write FpriOption;
    property ArcName: string read FArcName write FArcName;
    property FileMasks: TStringList read FFileMasks;
  end;

implementation

uses
  Bee_Common,
  // ---
  BeeGui_Forms;

  constructor TCmdLine.Create;
  var
    i: integer;
    S: string;
  begin
    inherited Create;
    // ---
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
    FxOption := TStringList.Create;
    FtOption := False;
    FlOption := False;
    FyOption := '';
    FkOption := False;
    FcdOption := '';
    FcfgOption := '';
    FpriOption := 1;
    FArcName := '';
    FFileMasks := TStringList.Create;
    // ---
    FRun := False;
    F0Option := '';
    F1Option := False;
    F2Option := False;
    FParams := TStringList.Create;
    // ---
    ProcessOptions;
    ProcessParams;
  end;

  procedure TCmdLine.ProcessOptions;
  var
    S: string;
    i: integer;
  begin
    // catch options, command, archive name and name of files
    for i := 1 to ParamCount do
    begin
      S := ParamStr(i);
      if (Length(S) > 1) and (S[1] = '-') then
      begin
        // options...
        case UpCase(S[2]) of
          '0': begin
                 System.Delete(S, 1, 2);
                 F0Option := S;
               end;
          '1': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   F1Option := True
                 else
                   if (S = '-') then F1Option := False;
               end;
          '2': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   F2Option := True
                 else
                   if (S = '-') then F2Option := False;
               end;
          'R': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FrOption := True
                 else
                   if (S = '-') then FrOption := False;
               end;
          'U': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FuOption := True
                 else
                 if (S = '-') then FuOption := False;
               end;
          'F': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FfOption := True
                 else
                   if (S = '-') then FfOption := False;
               end;
          'E': begin
                 System.Delete(S, 1, 2);
                 if ExtractFileExt('.' + S) <> '.' then
                 begin
                   FeOption := ExtractFileExt('.' + S);
                 end;
               end;
          'S': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FsOption := True
                 else
                   if (S = '-') then FsOption := False;
               end;
          'A': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FaOption := 'beegui.sfx'
                 else
                   if (S = '-') then
                     FaOption := 'nul'
                   else
                     FaOption := S;
               end;
          'O': begin
                 System.Delete(S, 1, 2);
                 if (Length(S) = 1) and (UpCase(S[1]) in ['A', 'S', 'Q']) then
                 begin
                   FoOption := UpCase(S[1]);
                 end;
               end;
          'M': begin
                 System.Delete(S, 1, 2);
                 if (Length(S)= 1) and (S[1] in ['0'..'3']) then
                 begin
                   FmOption := StrToInt(S[1]);
                 end;
               end;
          'D': begin
                 System.Delete(S, 1, 2);
                 if (Length(S)= 1) and (S[1] in ['0'..'9']) then
                 begin
                   FdOption := StrToInt(S[1]);
                 end;
               end;
          'X': begin
                 System.Delete(S, 1, 2);
                 if Length(S) > 0 then
                 begin
                   FxOption.Add(S);
                 end;
               end;
          'T': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                 begin
                   FtOption := True;
                   F1Option := True;
                 end else
                   if (S = '-') then FtOption := False;
               end;
          'L': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FlOption := True
                 else
                   if (S = '-') then FlOption := False;
               end;
          'Y': begin
                 System.Delete(S, 1, 2);
                 if DirectoryExists(ExcludeTrailingBackslash(S)) then
                 begin
                   FyOption := ExcludeTrailingBackslash(S);
                 end;
               end;
          'K': begin
                 System.Delete(S, 1, 2);
                 if (S = '+') or (Length(S) = 0) then
                   FkOption := True
                 else
                   if (S = '-') then FkOption := False;
                end;
          else  if Pos('-CD', UpCase(S)) = 1 then
                begin
                  System.Delete(S, 1, 3);
                  if Length(S) > 0 then
                  begin
                    FcdOption := IncludeTrailingBackslash(FixDirName(S));
                  end;
                end else
                begin
                  if Pos('-CFG', UpCase(S)) = 1 then
                  begin
                    System.Delete(S, 1, 4);
                    if (Length(S) > 0) and FileExists(S) then
                    begin
                      FcfgOption := S;
                    end;
                  end else
                  begin
                    if Pos('-PRI', UpCase(S)) = 1 then
                    begin
                      System.Delete(S, 1, 4);
                      if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
                      begin
                        FpriOption := StrToInt(S[1]);
                      end;
                    end
                  end;
                end;
        end; // end case
      end else
      begin
        // command or filenames...
        if FCommand = ' ' then
        begin
          if Length(S) = 1 then
          begin
            FCommand := UpCase(S[1]);
            case FCommand of
             'T': F1Option := True;
            end;
          end else
            FCommand := '?';
        end else
          if FArcName = '' then
          begin
            FArcName := ExpandFileName(S);
            if ExtractFileExt(FArcName) = '' then
            begin
              FArcName := ChangeFileExt(FArcName, '.bee');
            end;
          end else
            FFileMasks.Add(S);
      end;
    end; // end for loop
  end;

  destructor TCmdLine.Destroy;
  begin
    FxOption.Free;
    FFileMasks.Free;
    // ---
    FParams.Free;
    inherited Destroy;
  end;

  procedure TCmdLine.ProcessParams;
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

      if FrOption then FParams.Add('-r+') else FParams.Add('-r-');
      if FuOption then FParams.Add('-u+') else FParams.Add('-u-');
      if FfOption then FParams.Add('-f+') else FParams.Add('-f-');

      if Length(FeOption) > 0 then
        FParams.Add('-e' + FeOption);

      if FsOption then FParams.Add('-s+') else FParams.Add('-s-');

      if Length(FaOption) > 0 then
        FParams.Add('-a' + FaOption);

      FParams.Add('-o'+ FoOption);
      FParams.Add('-m'+ IntToStr(FmOption));
      FParams.Add('-d'+ IntToStr(FdOption));

      for i := 0 to FxOption.Count - 1 do
        FParams.Add('-x' + FxOption.Strings[i]);

      if FtOption then FParams.Add('-t+') else FParams.Add('-t-');
      if FlOption then FParams.Add('-l+') else FParams.Add('-l-');

      if Length(FyOption) > 0 then
        FParams.Add('-y' + FyOption);

      if FkOption then FParams.Add('-k+') else FParams.Add('-k-');

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

end.

