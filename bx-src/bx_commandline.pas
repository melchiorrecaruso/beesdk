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

    Command line parser.

  Fist release:

    v1.0 build 0027 - 2013.11.09 by Melchiorre Caruso.

  Modifyed:

}

unit bx_CommandLine;

{$I bx_compiler.inc}

interface

uses
  Classes;

type
  // TCommand

  TCommand = (
    cmdA,  // cmdA  Add files
    cmdC,  // cmdC  Update comments
    cmdD,  // cmdD  Delete files
    cmdE,  // cmdE  Extract file
    cmdH,  // cmdH  Show Help
    cmdL,  // cmdL  List files
    cmdQ,  // cmdQ  Quick test files
    cmdR,  // cmdR  Rename files
    cmdT,  // cmdT  Test files
    cmdX); // cmdX  Extract files with full paths

  // TSwitch

  TSwitch = (
    swtACC, // swtACC  Set archive comment
    swtB,   // swtB    Set work in background
    swtC,   // swtC    Set compression parameters
    swtCC,  // swtCC   Set comment
    swtCD,  // swtCD   Set current archive directory
    swtE,   // swtE    Set encryption parameters
    swtH,   // swtH    Set hashing parameters
    swtP,   // swtP    Set password
    swtR,   // swtR    Recurse subdirectories
    swtRX,  // swtRX   Recurse subdirectories for "x" switch
    swtSFX, // swtSFX  Create self-extracting archive
    swtSL,  // swtSL   Sort list by filename - for "l" (list) command
    swtSS,  // swtSS   Stop switches parsing
    swtT,   // swtT    Test temporary archive after process
    swtU,   // swtU    Update files method
    swtV,   // swtV    Create volumes
    swtVB,  // swtVB   Verbose mode
    swtW,   // swtW    Set temporary work directory
    swtX,   // swtX    Exclude filenames
    swtY);  // swtY    Assume "yes" on all queries

  TSwitches = set of TSwitch;

  // TUpdateMethod

  TUpdateMethod = (
    umAdd,            // umAdd            Add only new files
    umUpdate,         // umUpdate         Update only existing files
    umReplace,        // umReplace        Replace only existing files
    umQuery,          // umQuery          Query always
    umAddUpdate,      // umAddUpdate      Add and update existing files
    umAddReplace,     // umAddReplace     Add and replace existing files
    umAddQuery,       // umAddQuery       Add and query if already exists
    umAddAutoRename); // umAddAutoRename  Add and rename if already exists

  // TCommandLineParser class

  TCommandLineParser = class
  protected
    FCommand: TCommand;
    FSwitches: TSwitches;
    FSwitchACC: string;
    FSwitchB: boolean;
    FSwitchC: string;
    FSwitchCC: string;
    FSwitchCD: string;
    FSwitchE: string;
    FSwitchH: string;
    FSwitchP: string;
    FSwitchR: string;
    FSwitchRX: string;
    FSwitchSFX: string;
    FSwitchSL: boolean;
    FSwitchSS: boolean;
    FSwitchT: boolean;
    FSwitchU: TUpdateMethod;
    FSwitchV: qword;
    FSwitchVB: boolean;
    FSwitchW: string;
    FSwitchX: TStringList;
    FSwitchY: boolean;
    FArchiveName: string;
    FFileMasks: TStringList;
    function GetSwitchR (Index: longint): boolean;
    function GetSwitchRX(Index: longint): boolean;
    procedure ProcessCommand(const S: string);
    procedure ProcessSwitchACC(var S: string);
    procedure ProcessSwitchB(var S: string);
    procedure ProcessSwitchC(var S: string);
    procedure ProcessSwitchCC(var S: string);
    procedure ProcessSwitchCD(var S: string);
    procedure ProcessSwitchE(var S: string);
    procedure ProcessSwitchH(var S: string);
    procedure ProcessSwitchI(var S: string);
    procedure ProcessSwitchP(var S: string);
    procedure ProcessSwitchR(var S: string);
    procedure ProcessSwitchRX(var S: string);
    procedure ProcessSwitchSFX(var S: string);
    procedure ProcessSwitchSL(var S: string);
    procedure ProcessSwitchSS(var S: string);
    procedure ProcessSwitchT(var S: string);
    procedure ProcessSwitchU(var S: string);
    procedure ProcessSwitchV(var S: string);
    procedure ProcessSwitchVB(var S: string);
    procedure ProcessSwitchW(var S: string);
    procedure ProcessSwitchX(var S: string);
    procedure ProcessSwitchY(var S: string);
    procedure ProcessArchiveName(var S: string);
    procedure ProcessFileMasks(const S: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property Command: TCommand read FCommand;
    property Options: TSwitches read FSwitches;
    property SwitchACC: string read FSwitchACC;
    property SwitchB: boolean read FSwitchB;
    property SwitchC: string read FSwitchC;
    property SwitchCC: string read FSwitchCC;
    property SwitchCD: string read FSwitchCD;
    property SwitchE: string read FSwitchE;
    property SwitchH: string read FSwitchH;
    property SwitchP: string read FSwitchP;
    property SwitchR [Index: longint]: boolean read GetSwitchR;
    property SwitchRX[Index: longint]: boolean read GetSwitchRX;
    property SwitchSFX: string read FSwitchSFX;
    property SwitchSL: boolean read FSwitchSL;
    property SwitchSS: boolean read FSwitchSS;
    property SwitchT: boolean read FSwitchT;
    property SwitchU: TUpdateMethod read FSwitchU;
    property SwitchV: qword read FSwitchV;
    property SwitchVB: boolean read FSwitchVB;
    property SwitchW: string read FSwitchW;
    property SwitchX: TStringList read FSwitchX;
    property SwitchY: boolean read FSwitchY;
    property ArchiveName: string read FArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

  function ExtractCompressionMethod(const Params: string): longword;
  function ExtractCompressionLevel(const Params: string): longword;
  function ExtractCompressionAuxLevel(Params: string): longword;
  function ExtractCompressionFilter(const Params: string): string;
  function ExtractCompressionAuxFilter(const Params: string): string;
  function ExtractCompressionBlock(Params: string): qword;
  function ExtractCompressionConfiguration(const Params: string): string;
  function ExtractEncryptionMethod(const Params: string): longword;
  function ExtractEncryptionPassword(const Params: string): string;
  function ExtractHashingMethod(Params: string): longword;
  function ExtractHashingAuxMethod(Params: string): longword;

implementation

uses
  DateUtils,
  SysUtils,
  // ---
  bx_Common,
  bx_Messages;

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

function ExtractStr(const Params: string; const K: string): string;
var
  I, J: longint;
  S: string;
begin
  Result := '';
  if Pos(K, UpCase(Params)) > 0 then
  begin
    for I := Pos(K, UpCase(Params)) + Length(K) to Length(Params) do
      if Params[I] <> char(0) then
        Result := Result + Params[I]
      else
        Break;
  end;
end;

function ExtractQWord(const Params: string; const K: string): qword;
var
  S: string;
begin
  S := ExtractStr(Params, K);
  if TryStrWithMultToQWord(S, Result) = FALSE then
  begin
    SetExitStatus(esCmdLineError);
  end;
end;

function ExtractDWord(const Params: string; const K: string): dword;
begin
  Result := ExtractQWord(Params, K);
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
  Result := 1; // Default value
  if Pos(char(0) + 'M', UpCase(Params)) > 0 then
  begin
    S := ExtractStr(Params, char(0) + 'M');
    if S = '0' then Result := 0 else
    if S = '1' then Result := 1 else
    if S = '2' then Result := 2 else SetExitStatus(esCmdLineCError);
  end;
end;

function ExtractCompressionLevel(const Params: string): longword;
begin
  case ExtractCompressionMethod(Params) of
    0:
    begin
      Result := 0;
    end;
    1:
    begin
      Result := 1; // Default value
      if Pos(char(0) + 'L', UpCase(Params)) > 0 then
        Result := ExtractQWord(Params, char(0) + 'L');

      if (3 < Result) or (Result < 1) then
        SetExitStatus(esCmdLineCError);
    end;
    2:
    begin
      Result := 6; // Default value
      if Pos(char(0) + 'L', UpCase(Params)) > 0 then
        Result := ExtractQWord(Params, char(0) + 'L');

      if (64 < Result) or (Result < 2) then
        SetExitStatus(esCmdLineCError);
    end;
    else SetExitStatus(esCmdLineCError);
  end;
end;

function ExtractCompressionAuxLevel(Params: string): longword;
begin
  case ExtractCompressionMethod(Params) of
    0:
    begin
      Result := 0;
    end;
    1:
    begin
      Result := 3; // Default value
      if Pos(char(0) + 'AL', UpCase(Params)) > 0 then
        Result := ExtractQWord(Params, char(0) + 'AL');

      if (9 < Result) or (Result < 0) then
        SetExitStatus(esCmdLineCError);
    end;
    2:
    begin
      Result := $500000; // Default value
      if Pos(char(0) + 'AL', UpCase(Params)) > 0 then
        Result := ExtractQWord(Params, char(0) + 'AL');

      if ($FFFFFFDB < Result) or (Result < $800) then
        SetExitStatus(esCmdLineCError);
    end;
    else SetExitStatus(esCmdLineCError);
  end;
end;

function ExtractCompressionFilter(const Params: string): string;
begin
  if Pos(char(0) + 'F', UpCase(Params)) > 0 then
    Result := ExtractStr(Params, char(0) + 'F')
  else
    Result := '';
end;

function ExtractCompressionAuxFilter(const Params: string): string;
begin
  if Pos(char(0) + 'AF', UpCase(Params)) > 0 then
    Result := ExtractStr(Params, char(0) + 'AF')
  else
    Result := '';
end;

function ExtractCompressionBlock(Params: string): qword;
begin
  if Pos(char(0) + 'B', UpCase(Params)) > 0 then
    Result := ExtractQWord(Params, char(0) + 'B')
  else
    Result := 0;
end;

function ExtractCompressionConfiguration(const Params: string): string;
begin
  if Pos(char(0) + 'C', UpCase(Params)) > 0 then
    Result := SelfPath + ExtractStr(Params, char(0) + 'C')
  else
    Result := SelfPath + DefaultConfigFileName;
end;

function ExtractEncryptionMethod(const Params: string): longword;
var
  S: string;
begin
  if Pos(char(0) + 'M', UpCase(Params)) > 0 then
  begin
    S := ExtractStr(Params, char(0) + 'M');
    if S = '0' then Result := 0 else
    if S = '1' then Result := 1 else
    if S = '2' then Result := 2 else SetExitStatus(esCmdLineEError);
  end else
  begin
    if Length(ExtractEncryptionPassword(Params)) = 0 then
      Result := 0
    else
      Result := 1;
  end;
end;

function ExtractEncryptionPassword(const Params: string): string;
begin
  Result := '';
  if Pos(char(0) + 'P', UpCase(Params)) > 0 then
  begin
    if Length(ExtractStr(Params, char(0) + 'P')) >= 4 then
      Result := ExtractStr(Params, char(0) + 'P')
    else
      SetExitStatus(esCmdLineEError);
  end;
end;

function ExtractHashingMethod(Params: string): longword;
var
  S: string;
begin
  Result := 1; // Default value
  if Pos(char(0) + 'M', UpCase(Params)) > 0 then
  begin
    S := ExtractStr(Params, char(0) + 'M');
    if S = '0' then Result := 0 else
    if S = '1' then Result := 1 else
    if S = '2' then Result := 2 else
    if S = '3' then Result := 3 else
    if S = '4' then Result := 4 else SetExitStatus(esCmdLineCIError);
  end;
end;

function ExtractHashingAuxMethod(Params: string): longword;
var
  S: string;
begin
  Result := 1; // Default value
  if Pos(char(0) + 'AM', UpCase(Params)) > 0 then
  begin
    S := ExtractStr(Params, char(0) + 'AM');
    if S = '0' then Result := 0 else
    if S = '1' then Result := 1 else
    if S = '2' then Result := 2 else
    if S = '3' then Result := 3 else
    if S = '4' then Result := 4 else SetExitStatus(esCmdLineCIError);
  end;
end;

// ---

constructor TCommandLineParser.Create;
begin
  inherited Create;
  FCommand     := cmdH;
  FSwitches    := [];
  FSwitchACC   := '';
  FSwitchB     := FALSE;
  FSwitchC     := char(0);
  FSwitchCC    := '';
  FSwitchCD    := '';

  FSwitchE     := char(0);
  FSwitchH     := char(0);
  FSwitchP     := '';
  FSwitchR     := '';
  FSwitchRX    := '';
  FSwitchSFX   := '';
  FSwitchSL    := FALSE;
  FSwitchSS    := FALSE;
  FSwitchT     := FALSE;
  FSwitchU     := umAddUpdate;
  FSwitchV     :=  0;
  FSwitchVB    := FALSE;
  FSwitchW     := '';
  FSwitchX     := TStringList.Create;
  FSwitchY     := FALSE;
  FArchiveName := '';
  FFileMasks   := TStringList.Create;
end;

destructor TCommandLineParser.Destroy;
begin
  FSwitchX.Destroy;
  FFileMasks.Destroy;
  inherited Destroy;
end;

procedure TCommandLineParser.ProcessCommand(const S: string);
begin
  if UpCase(S[1]) = 'A' then FCommand := cmdA else
  if UpCase(S[1]) = 'C' then FCommand := cmdC else
  if UpCase(S[1]) = 'D' then FCommand := cmdD else
  if UpCase(S[1]) = 'E' then FCommand := cmdE else
  if UpCase(S[1]) = 'H' then FCommand := cmdH else
  if UpCase(S[1]) = 'L' then FCommand := cmdL else
  if UpCase(S[1]) = 'Q' then FCommand := cmdQ else
  if UpCase(S[1]) = 'R' then FCommand := cmdR else
  if UpCase(S[1]) = 'T' then FCommand := cmdT else
  if UpCase(S[1]) = 'X' then FCommand := cmdX else SetExitStatus(esCmdLineError);
end;

procedure TCommandLineParser.ProcessSwitchACC(var S: string);
begin
  Delete(S, 1, 5);
  if Pos('!', S) = 1 then
  begin
    Delete(S, 1, 1);
    FSwitchACC := S;
  end else
    if Pos('@', S) = 1 then
    begin
      Delete(S, 1, 1);
      if FileExists(S) then
      begin
        FSwitchACC := ExtractStrFromFile(S);
      end else
        SetExitStatus(esCmdLineACCError);
    end else
      SetExitStatus(esCmdLineACCError);

  if Command in [cmdA, cmdD, cmdR] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtACC);
  end else
    SetExitStatus(esCmdLineACCError);
end;

procedure TCommandLineParser.ProcessSwitchB(var S: string);
begin
  Delete(S, 1, 2);
  if S = '' then
    FSwitchB := TRUE
  else
    SetExitStatus(esCmdLineBError);

  if FCommand in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtB);
  end else
    SetExitStatus(esCmdLineBError);
end;

procedure TCommandLineParser.ProcessSwitchC(var S: string);
begin
  Delete(S, 1, 3);
  FSwitchC := FSwitchC + S + char(0);
  // check ...
  ExtractCompressionMethod       (FSwitchC);
  ExtractCompressionLevel        (FSwitchC);
  ExtractCompressionAuxLevel     (FSwitchC);
  ExtractCompressionFilter       (FSwitchC);
  ExtractCompressionAuxFilter    (FSwitchC);
  ExtractCompressionBlock        (FSwitchC);
  ExtractCompressionConfiguration(FSwitchC);

  if Command in [cmdA] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtC);
  end else
    SetExitStatus(esCmdLineCError);
end;

procedure TCommandLineParser.ProcessSwitchCC(var S: string);
begin
  Delete(S, 1, 4);
  if Pos('!', S) = 1 then
  begin
    Delete(S, 1, 1);
    FSwitchCC := S;
  end else
    if Pos('@', S) = 1 then
    begin
      Delete(S, 1, 1);
      if FileExists(S) then
      begin
        FSwitchCC := ExtractStrFromFile(S);
      end else
        SetExitStatus(esCmdLineCCError);
    end else
      SetExitStatus(esCmdLineCCError);

  if Command in [cmdA, cmdD, cmdR] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtCC);
  end else
    SetExitStatus(esCmdLineCCError);
end;

procedure TCommandLineParser.ProcessSwitchCD(var S: string);
begin
  Delete(S, 1, 4);
  if Length(S) > 0 then
    FSwitchCD := IncludeTrailingBackSlash(S)
  else
    SetExitStatus(esCmdLineCDError);

  if Command in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtCD);
  end else
    SetExitStatus(esCmdLineCDError);
end;

procedure TCommandLineParser.ProcessSwitchH(var S: string);
begin
  Delete(S, 1, 4);
  FSwitchH := FSwitchH + S + char(0);
  // check ...
  ExtractHashingMethod(FSwitchH);
  ExtractHashingAuxMethod(FSwitchH);

  if Command in [cmdA] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtH);
  end else
    SetExitStatus(esCmdLineCIError);
end;

procedure TCommandLineParser.ProcessSwitchE(var S: string);
begin
  Delete(S, 1, 3);
  FSwitchE := FSwitchE + S + char(0);
  // check ...
  ExtractEncryptionMethod  (FSwitchE);
  ExtractEncryptionPassword(FSwitchE);

  if Command in [cmdA] then
  begin
    if ExitStatus = esNoError then
    begin
      Include(FSwitches, swtE);
      if ExtractEncryptionPassword(FSwitchE) <> '' then
      begin
        Include(FSwitches, swtP);
        FSwitchP := ExtractEncryptionPassword(FSwitchE);
      end;
    end;
  end else
    SetExitStatus(esCmdLineEError);
end;

procedure TCommandLineParser.ProcessSwitchI(var S: string);
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
        SetExitStatus(esCmdLineIError);
    end else
      SetExitStatus(esCmdLineIError);

  if FCommand in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    // nothing to do
  end else
    SetExitStatus(esCmdLineIError);
end;

procedure TCommandLineParser.ProcessSwitchP(var S: string);
begin
  Delete(S, 1, 3);
  if Length(S) >= 4 then
    FSwitchP := S
  else
    SetExitStatus(esCmdLinePError);

  if Command in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtP);
  end else
    SetExitStatus(esCmdLinePError);
end;

procedure TCommandLineParser.ProcessSwitchR(var S: string);
var
  I: longint;
begin
  if UpCase(S) = '-R' then
  begin
    if FSwitchR = '' then
      FSwitchR := 'A'
    else
      SetExitStatus(esCmdLineRError);
  end else
    if Pos('-R/', UpCase(S)) = 1 then
    begin
      Delete(S, 1, 3);
      FSwitchR := FSwitchR + UpCase(S);

      for I := 1 to Length(FSwitchR) do
        if (FSwitchR[I] in ['N', 'Y']) = FALSE then
          SetExitStatus(esCmdLineRError);
    end else
      SetExitStatus(esCmdLineRError);

  if FCommand in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtR);
  end else
    SetExitStatus(esCmdLineRError);
end;

procedure TCommandLineParser.ProcessSwitchRX(var S: string);
var
  I: longint;
begin
  if UpCase(S) = '-RX' then
  begin
    if FSwitchRX = '' then
      FSwitchRX := 'A'
    else
      SetExitStatus(esCmdLineRXError);
  end else
    if Pos('-RX/', UpCase(S)) = 1 then
    begin
      Delete(S, 1, 4);
      FSwitchRX := FSwitchRX + UpCase(S);

      for I := 1 to Length(FSwitchRX) do
        if (FSwitchRX[I] in ['N', 'Y']) = FALSE then
          SetExitStatus(esCmdLineRXError);
    end else
      SetExitStatus(esCmdLineRXError);

  if FCommand in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtRX);
  end else
    SetExitStatus(esCmdLineRXError);
end;

function  TCommandLineParser.GetSwitchR(Index: longint): boolean;
begin
  Result := FALSE;
  if FSwitchR = 'A' then
    Result := TRUE
  else
    if Index < Length(FSwitchR) then
      Result := FSwitchR[Index + 1] = 'Y';
end;

function  TCommandLineParser.GetSwitchRX(Index: longint): boolean;
begin
  Result := FALSE;
  if FSwitchRX = 'A' then
    Result := TRUE
  else
    if Index < Length(FSwitchRX) then
      Result := FSwitchRX[Index + 1] = 'Y';
end;

procedure TCommandLineParser.ProcessSwitchSFX(var S: string);
begin
  if UpCase(S) = '-SFX' then
  begin
    S := 'bx.sfx';
  end else
    if Pos('-SFX/', UpCase(S)) = 1 then
      Delete(S, 1, 5)
    else
      SetExitStatus(esCmdLineSFXError);

  FSwitchSFX := SelfPath + S;
  if FileExists(FSwitchSFX) = FALSE then
    SetExitStatus(esCmdLineSFXError);

  if FCommand in [cmdA, cmdD, cmdR] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtSFX);
  end else
    SetExitStatus(esCmdLineSFXError);
end;

procedure TCommandLineParser.ProcessSwitchSL(var S: string);
begin
  Delete(S, 1, 3);
  if S = '' then
    FSwitchSL := TRUE
  else
    SetExitStatus(esCmdLineSLError);

  if FCommand in [cmdL] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtSL);
  end else
    SetExitStatus(esCmdLineSLError);
end;

procedure TCommandLineParser.ProcessSwitchSS(var S: string);
begin
  Delete(S, 1, 3);
  if S = '' then
    FSwitchSS := TRUE
  else
    SetExitStatus(esCmdLineSSError);

  if FCommand in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtSS);
  end else
    SetExitStatus(esCmdLineSSError);
end;

procedure TCommandLineParser.ProcessSwitchT(var S: string);
begin
  Delete(S, 1, 2);
  if S = '' then
    FSwitchT := TRUE
  else
    SetExitStatus(esCmdLineTError);

  if Command in [cmdA, cmdD, cmdR] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtT);
  end else
    SetExitStatus(esCmdLineTError);
end;

procedure TCommandLineParser.ProcessSwitchU(var S: string);
var
  I: longint;
begin
  Delete(S, 1, 3);
  if TryStrToInt(S, I) then
  begin
    if I in [0..7] then
      FSwitchU := TUpdateMethod(I)
    else
      SetExitStatus(esCmdLineUError);
  end else
    SetExitStatus(esCmdLineUError);

  if FCommand in [cmdA, cmdE, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtU);
  end else
    SetExitStatus(esCmdLineUError);
end;

procedure TCommandLineParser.ProcessSwitchV(var S: string);
var
  I: longint;
begin
  Delete(S, 1, 3);
  for I := 1 to Length(S) do
    if S[I] in ['.', ','] then
      S[I] := DecimalSeparator;

  if TryStrWithMultToQWord(S, FSwitchV) = FALSE then
    SetExitStatus(esCmdLineVError);

  if FCommand in [cmdA, cmdD, cmdR] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtV);
  end else
    SetExitStatus(esCmdLineVError);
end;

procedure TCommandLineParser.ProcessSwitchVB(var S: string);
begin
  Delete(S, 1, 3);
  if S = '' then
    FSwitchVB := TRUE
  else
    SetExitStatus(esCmdLineVBError);

  if Command in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtVB);
  end else
    SetExitStatus(esCmdLineVBError);
end;

procedure TCommandLineParser.ProcessSwitchW(var S: string);
begin
  if UpCase(S) = '-W' then
  begin
    FSwitchW := ExcludeTrailingBackSlash(GetTempDir)
  end else
    if Pos('-W/', UpCase(S)) = 1 then
    begin
      Delete(S, 1, 3);
      FSwitchW := ExcludeTrailingBackSlash(S);
      if DirectoryExists(FSwitchW) = FALSE then
         SetExitStatus(esCmdLineWError);
    end else
      SetExitStatus(esCmdLineWError);

  if FCommand in [cmdA, cmdD, cmdR] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtW);
  end else
    SetExitStatus(esCmdLineWError);
end;

procedure TCommandLineParser.ProcessSwitchX(var S: string);
begin
  Delete(S, 1, 3);
  if Pos('!', S) = 1 then
  begin
    Delete(S, 1, 1);
    FSwitchX.Add(S);
  end else
    if Pos('@', S) = 1 then
    begin
      Delete(S,1, 1);
      if FileExists(S) then
        FSwitchX.LoadFromFile(S)
      else
        SetExitStatus(esCmdLineXError);
    end else
      SetExitStatus(esCmdLineXError);

  if FCommand in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtX);
  end else
    SetExitStatus(esCmdLineXError);
end;

procedure TCommandLineParser.ProcessSwitchY(var S: string);
begin
  Delete(S, 1, 2);
  if S = '' then
    FSwitchY := TRUE
  else
    SetExitStatus(esCmdLineYError);

  if Command in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    if ExitStatus = esNoError then
      Include(FSwitches, swtY);
  end else
    SetExitStatus(esCmdLineYError);
end;

procedure TCommandLineParser.ProcessArchiveName(var S: string);
begin
  FArchiveName := S;
  if FileExists(FArchiveName) = FALSE then
    if ExtractFileExt(FArchiveName) = '' then
      FArchiveName := ChangeFileExt(FArchiveName, '.bx');

  if FCommand in [cmdA, cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
  begin
    // check if archive exists
    if FCommand in [cmdD, cmdE, cmdL, cmdQ, cmdR, cmdT, cmdX] then
      if FileExists(FArchiveName) = FALSE then
        SetExitStatus(esCmdLineArcError);
  end else
    SetExitStatus(esCmdLineArcError);
end;

procedure TCommandLineParser.ProcessFileMasks(const S: string);
begin
  FFileMasks.Add(S);
  if FCommand in [cmdH] then
    SetExitStatus(esCmdLineMaskError);
end;

procedure TCommandLineParser.Execute;
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
      if (S[1] = '-') and (FSwitchSS = FALSE) then
      begin
        // options...
        if Pos('-ACC/',  UpCase(S)) = 1 then ProcessSwitchACC(S) else
        if Pos('-B',     UpCase(S)) = 1 then ProcessSwitchB  (S) else
        if Pos('-C/',    UpCase(S)) = 1 then ProcessSwitchC  (S) else
        if Pos('-CC/',   UpCase(S)) = 1 then ProcessSwitchCC (S) else
        if Pos('-CD/',   UpCase(S)) = 1 then ProcessSwitchCD (S) else
        if Pos('-E/',    UpCase(S)) = 1 then ProcessSwitchE  (S) else
        if Pos('-I/',    UpCase(S)) = 1 then ProcessSwitchI  (S) else
        if Pos('-H/',    UpCase(S)) = 1 then ProcessSwitchH  (S) else
        if Pos('-P/',    UpCase(S)) = 1 then ProcessSwitchP  (S) else
        if Pos('-RX/',   UpCase(S)) = 1 then ProcessSwitchRX (S) else
        if Pos('-RX',    UpCase(S)) = 1 then ProcessSwitchRX (S) else
        if Pos('-R/',    UpCase(S)) = 1 then ProcessSwitchR  (S) else
        if Pos('-R',     UpCase(S)) = 1 then ProcessSwitchR  (S) else
        if Pos('-SFX/',  UpCase(S)) = 1 then ProcessSwitchSFX(S) else
        if Pos('-SFX',   UpCase(S)) = 1 then ProcessSwitchSFX(S) else
        if Pos('-SL',    UpCase(S)) = 1 then ProcessSwitchSL (S) else
        if Pos('-SS',    UpCase(S)) = 1 then ProcessSwitchSS (S) else
        if Pos('-T',     UpCase(S)) = 1 then ProcessSwitchT  (S) else
        if Pos('-U/',    UpCase(S)) = 1 then ProcessSwitchU  (S) else
        if Pos('-V/',    UpCase(S)) = 1 then ProcessSwitchV  (S) else
        if Pos('-VB',    UpCase(S)) = 1 then ProcessSwitchVB (S) else
        if Pos('-W/',    UpCase(S)) = 1 then ProcessSwitchW  (S) else
        if Pos('-W',     UpCase(S)) = 1 then ProcessSwitchW  (S) else
        if Pos('-X/',    UpCase(S)) = 1 then ProcessSwitchX  (S) else
        if Pos('-Y',     UpCase(S)) = 1 then ProcessSwitchY  (S) else
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
    if FCommand in [cmdA, cmdD, cmdR] then
      SetExitStatus(esCmdLineMaskError)
    else
      if FCommand in [cmdE, cmdL, cmdQ, cmdT, cmdX] then
      begin
        Include(FSwitches, swtR);
        FSwitchR := 'A';
        FFileMasks.Add('*');
      end;
  end;

  {$IFDEF DEBUG}
  Writeln('  Command = ', FCommand);
  Writeln('    swACC = ', FSwitchACC);
  Writeln('      swB = ', FSwitchB);

  S := FSwitchC;
  for I := 1 to Length(S) do
    if S[I] = char(0) then S[I] := ' ';
  Writeln('      swC = ', S);

  Writeln('     swCC = ', FSwitchCC);
  Writeln('     swCD = ', FSwitchCD);

  S := FSwitchE;
  for I := 1 to Length(S) do
    if S[I] = char(0) then S[I] := ' ';
  Writeln('      swE = ', S);

  S := FSwitchH;
  for I := 1 to Length(S) do
    if S[I] = char(0) then S[I] := ' ';
  Writeln('      swH = ', S);

  Writeln('      swP = ', FSwitchP);
  Writeln('      swR = ', FSwitchR);
  Writeln('     swRX = ', FSwitchRX);
  Writeln('    swSFX = ', FSwitchSFX);
  Writeln('     swSL = ', FSwitchSL);
  Writeln('     swSS = ', FSwitchSS);
  Writeln('      swT = ', FSwitchT);
  Writeln('      swU = ', FSwitchU);
  Writeln('      swV = ', FSwitchV);
  Writeln('     swVB = ', FSwitchVB);
  Writeln('      swW = ', FSwitchW);

  for I := 0 to FSwitchX.Count -1 do
    Writeln('      swX= ', FSwitchX.Strings[I]);

  Writeln('      swY = ', FSwitchY);
  Writeln('  ArcName = ', FArchiveName);

  for I := 0 to FFileMasks.Count -1 do
    Writeln(' FileMask = ', FFileMasks.Strings[I]);
  Writeln;
  {$ENDIF}  
end;

end.
