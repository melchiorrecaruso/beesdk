{
  Copyright (c) 1999-2013 Andrew Filinsky and Melchiorre Caruso.

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

    Various helper routines.

  Modifyed:

    v0.7.8 build 0150 - 2005.06.27 by Melchiorre Caruso;
    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.8 build 0154 - 2005.07.23 by Melchiorre Caruso;
    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_Common;

{$I bee_compiler.inc}

interface

uses
  Classes,
  SysUtils,
  {$IFNDEF FPC} Math, {$ENDIF}
  {$IFDEF UNIX} BaseUnix; {$ENDIF}
  {$IFDEF MSWINDOWS} Windows; {$ENDIF}

const
  Cr = #13#10;

  { Default file names }

  DefaultCfgName      = 'bee.ini';
  DefaultSfxName      = 'bee.sfx';

  { Messages }

  cmCreating          = 'Creating   %s';
  cmOpening           = 'Opening    %s';
  cmScanning          = 'Scanning   %s';
  cmAdding            = 'Adding     %s';
  cmUpdating          = 'Updating   %s';
  cmCopying           = 'Copying    %s';
  cmSplitting         = 'Splitting  %s';
  cmEncoding          = 'Encoding   %s';
  cmExtracting        = 'Extracting %s';
  cmTesting           = 'Testing    %s';
  cmDecoding          = 'Decoding   %s';
  cmDeleting          = 'Deleting   %s';
  cmChecking          = 'Checking   %s';
  cmListing           = 'Listing    %s';
  cmSwapping          = 'Swapping   %s';
  cmRenaming          = 'Renaming   %s';
  cmLoading           = 'Loading    %s';

function SelfName: string;
function SelfPath: string;

function GetDriveFreeSpace(const FileName: string): int64;

{ filename handling routines }

function FileNameMatch(const FileName, Mask:  string; Recursive: boolean): boolean;
function FileNameHasWildcards(const FileName: string): boolean;
function FileNamePos(const FilePath, FileName: string): longint;
function FileNameIsValid(const FileName: string): boolean;

// 4x faster than dateutils version
function UNIXTimeToDateTimeFAST(UnixTime: LongWord): TDateTime;
begin
Result := (UnixTime / 86400) + 25569;
end;

// 10x faster than dateutils version
function DateTimeToUNIXTimeFAST(DelphiTime : TDateTime): LongWord;
begin
Result := Round((DelphiTime - 25569) * 86400);
end;



function GenerateFileName(const FilePath: string): string;
function GenerateAlternativeFileName(const FileName: string;
  var StartIndex: longint): string;


function DeleteFilePath(const FilePath, FileName: string): string;
function DeleteFileDrive(const FileName: string): string;


procedure ExpandFileMask(const Mask: string; Masks: TStringList; Recursive: boolean);

{  }


{ time handling routines }

function TimeDifference(X: double): string;
function TimeToStr(T: longint): string;
function DateTimeToString(X: TDateTime): string; overload;
function DateTimeToString(X: TDateTime; const Format: string): string; overload;
function FileTimeToString(X: longint): string; overload;
function FileTimeToString(X: longint; const Format: string): string; overload;
function SizeOfFile(const FileName: string): int64;

{ hex routines }

function Hex(const Data; Count: longint): string;
function HexToData(const S: string; var Data; Count: longint): boolean;

{ oem-ansi charset functions }

function ParamToOem(const Param: string): string;
function OemToParam(const Param: string): string;

{ system control }

function SetPriority(Priority: longint): boolean; { Priority is 0..3 }
procedure SetCtrlCHandler(CtrlHandler: pointer);


(*
;





















{ directory handling routines }





{ string, pchar routines }

function StringToPChar(const aValue: string): PChar;
function PCharToString(aValue: PChar): string;



function ReverseString(const Str: string): string;





{ low level functions }

function CreateText(var T: Text; const Name: string): boolean;
function AppendText(var T: Text; const Name: string): boolean;
function OpenText(var T: Text; const Name: string): boolean;
function WriteText(const FileName, S: string): boolean;

function SizeOfFile(const FileName: string): int64;




*)

implementation

const
  HexaDecimals: array [0..15] of char = '0123456789ABCDEF';
  HexValues: array ['0'..'F'] of byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15);

function SelfName: string;
begin
  Result := ExtractFileName(ParamStr(0));
end;

function SelfPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function GetDriveFreeSpace(const FileName: string): int64;
{$IFDEF MSWINDOWS}
var
  FreeAvailable, TotalSpace: int64;
begin
  if GetDiskFreeSpaceEx(PChar(ExtractFilePath(ExpandFileName(FileName))),
     FreeAvailable, TotalSpace, nil) then
    Result := FreeAvailable
  else
    Result := -1;
{$ENDIF}
{$IFDEF UNIX}
var
  FStats: {$IFDEF PosixAPI}_statvfs{$ELSE}TStatFs{$ENDIF};
begin
  Result := -1;
  {$IF DEFINED(LibcAPI)}
  if statfs(PAnsiChar(ExtractFilePath(FileName)), FStats) = 0 then
    Result := int64(FStats.f_bAvail) * int64(FStats.f_bsize);
  {$ELSEIF DEFINED(FPCUnixAPI)}
  if fpStatFS(PAnsiChar(ExtractFilePath(FileName)), @FStats) = 0 then
    Result := int64(FStats.bAvail) * int64(FStats.bsize);
  {$ELSEIF DEFINED(PosixAPI)}
  if statvfs(PAnsiChar(AbSysString(ExtractFilePath(FileName))), FStats) = 0 then
    Result := int64(FStats.f_bavail) * int64(FStats.f_bsize);
  {$IFEND}
{$ENDIF}
end;

function MatchPattern(Element, Pattern: PChar): boolean;
begin
  if 0 = StrComp(Pattern, '*') then
    Result := True
  else
    if (Element^ = Chr(0)) and (Pattern^ <> Chr(0)) then
      Result := False
    else
      if Element^ = Chr(0) then
        Result := True
      else
        case Pattern^ of
          '*': if MatchPattern(Element, @Pattern[1]) then
                 Result := True
               else
                 Result := MatchPattern(@Element[1], Pattern);

          '?': Result := MatchPattern(@Element[1], @Pattern[1]);

        else
          if Element^ = Pattern^ then
            Result := MatchPattern(@Element[1], @Pattern[1])
          else
            Result := False;
        end;
end;

function FileNameMatch(const FileName, Mask: string; Recursive: boolean): boolean;
var
  iFileDrive: string;
  iFilePath:  string;
  iFileName:  string;
  iMaskPath:  string;
  iMaskName:  string;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  iFilePath := ExtractFilePath(FileName);
  iFileName := ExtractFileName(FileName);
  iMaskPath := ExtractFilePath(Mask);
  iMaskName := ExtractFileName(Mask);
  {$ELSE}
  iFilePath := ExtractFilePath(UpperCase(FileName));
  iFileName := ExtractFileName(UpperCase(FileName));
  iMaskPath := ExtractFilePath(UpperCase(Mask));
  iMaskName := ExtractFileName(UpperCase(Mask));
  {$ENDIF}

  if ExtractFileDrive(iMaskPath) = '' then
  begin
    iFileDrive := ExtractFileDrive(iFilePath);
    if iFileDrive <> '' then
    begin
      iMaskPath := IncludeTrailingBackSlash(iFileDrive) + iMaskPath;
    end;
  end;

  if Recursive then
  begin
    iMaskPath := iMaskPath + '*';
  end;

  Result :=  MatchPattern(PChar(iFilePath), PChar(iMaskPath)) and
             MatchPattern(PChar(iFileName), PChar(iMaskName));
end;


function FileNameHasWildcards(const FileName: string): boolean;
begin
  if System.Pos('*', FileName) > 0 then
    Result := True
  else
    if System.Pos('?', FileName) > 0 then
      Result := True
    else
      Result := False;
end;

function FileNamePos(const FilePath, FileName: string): longint;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  Result := System.Pos(FilePath, FileName);
  {$ELSE}
  Result := System.Pos(UpperCase(FilePath), UpperCase(FileName));
  {$ENDIF}
end;

function FileNameIsValid(const FileName : string): boolean;
const
  {$IFDEF MSWINDOWS}
  InvalidCharacters: set of char = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
  {$ELSE}
  InvalidCharacters: set of char = [PathDelim];
  {$ENDIF}
var
  I: longint;
begin
  Result := FileName <> '';
  if Result then
    for I := 1 to Length(FileName) do
    begin
      Result := not (FileName[I] in InvalidCharacters) ;
      if not Result then Break;
    end;
end;


function GenerateFileName(const FilePath: string): string;
var
  I: longint;
begin
  repeat
    Result := '????????.$$$';
    for I := 1 to 8 do
      Result[I] := char(byte('A') + Random(byte('Z') - byte('A')));

    if FilePath <> '' then
      Result := IncludeTrailingBackSlash(FilePath) + Result;

  until FileAge(Result) = -1;
end;

function GenerateAlternativeFileName(const FileName: string;
  var StartIndex: longint): string;
begin
  Inc(StartIndex);
  Result := ChangeFileExt(FileName, '_' +
      IntToStr(StartIndex) + ExtractFileExt(FileName));
end;

function DeleteFilePath(const FilePath, FileName: string): string;
begin
  Result := FileName;
  if FileNamePos(FilePath, Result) = 1 then
  begin
    Delete(Result, 1, Length(FilePath));
  end;
end;

function DeleteFileDrive(const FileName: string): string;
var
  Drive: string;
begin
  Result := FileName;
  if Length(Result) > 0 then
  begin
    Drive := ExtractFileDrive(Result);
    System.Delete(Result, 1, Length(Drive));
  end;

  while Pos(PathDelim, Result) = 1 do
    Delete(Result, 1, 1);
end;

procedure ExpandFileMask(const Mask: string; Masks: TStringList; Recursive: boolean);
var
  I:     longint;
  Error: longint;
  Rec:   TSearchRec;
  Card:  boolean;
  LastSlash: longint;
  FirstSlash: longint;
  FolderName: string;
  FolderPath: string;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  Masks.CaseSensitive := True;
  {$ELSE}
  Masks.CaseSensitive := False;
  {$ENDIF}

  FirstSlash := 0;
  LastSlash  := 0;
  Card       := False;

  for I := 1 to Length(Mask) do
    if Card = False then
    begin
      if Mask[I] in ['*', '?'] then
        Card := True
      else
        if Mask[I] = PathDelim then
          FirstSlash := I;
    end else
    if Mask[I] = PathDelim then
    begin
      LastSlash := I;
      Break;
    end;

  if LastSlash > 0 then
  begin
    FolderPath := Copy(Mask, 1, FirstSlash);
    FolderName := Copy(Mask, FirstSlash + 1, LastSlash - (FirstSlash + 1));
    Error      := SysUtils.FindFirst(FolderPath + '*', faAnyFile, Rec);
    while Error = 0 do
    begin
      if ((Rec.Attr and faDirectory) = faDirectory) and
          (Rec.Name[1] <> '.') and (Rec.Name[1] <> '..') then
        if FileNameMatch(Rec.Name, FolderName, Recursive) then
          ExpandFileMask(FolderPath + Rec.Name + Copy(Mask, LastSlash,
            (Length(Mask) + 1) - LastSlash), Masks, Recursive);

      Error := SysUtils.FindNext(Rec);
    end;
    SysUtils.FindClose(Rec);
  end else
    if Masks.IndexOf(Mask) = -1 then Masks.Add(Mask);
end;

{ time handling routines }

function TimeDifference(X: double): string;
begin
  Result := Format('%0.2f', [(Now - X) * (24 * 60 * 60)]);
end;

function TimeToStr(T: longint): string;
var
  H, M, S:    string;
  ZH, ZM, ZS: longint;
begin
  ZH := T div 3600;
  ZM := T div 60 - ZH * 60;
  ZS := T - (ZH * 3600 + ZM * 60);

  if ZH < 10 then
    H := '0' + IntToStr(ZH)
  else
    H := IntToStr(ZH);

  if ZM < 10 then
    M := '0' + IntToStr(ZM)
  else
    M := IntToStr(ZM);

  if ZS < 10 then
    S := '0' + IntToStr(ZS)
  else
    S := IntToStr(ZS);

  Result := H + ':' + M + ':' + S;
end;

function DateTimeToString(X: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', X);
end;

function DateTimeToString(X: TDateTime; const Format: string): string;
begin
  Result := FormatDateTime(Format, X);
end;

function FileTimeToString(X: longint): string;
begin
  try
    Result := Bee_Common.DateTimeToString(SysUtils.FileDateToDateTime(X));
  except
    Result := '????-??-?? ??:??:??';
  end;
end;

function FileTimeToString(X: longint; const Format: string): string;
begin
  try
    Result := Bee_Common.DateTimeToString(SysUtils.FileDateToDateTime(X), Format);
  except
    Result := '????-??-?? ??:??:??';
  end;
end;

{ hex routines }

function Hex(const Data; Count: longint): string;
var
  I, J: longint;
  K:    longword;
begin
  SetLength(Result, Count shl 1);
  J := 1;
  for I := 0 to Count - 1 do
  begin
    K := TByteArray(Data) [I];
    Result[J] := HexaDecimals[K shr 4];
    Inc(J);
    Result[J] := HexaDecimals[K and $f];
    Inc(J);
  end;
end;

function HexToData(const S: string; var Data; Count: longint): boolean;
var
  I: longint;
begin
  Result := False;
  if Length(S) < Count * 2 then Exit;

  for I := 0 to Count - 1 do
  begin
    if (S[I * 2 + 1] in ['0'..'9', 'A'..'F']) and (S[I * 2 + 2] in ['0'..'9', 'A'..'F']) then
    begin
      TByteArray(Data)[I] := HexValues[S[I * 2 + 1]] shl 4 + HexValues[S[I * 2 + 2]]
    end else
      Exit;
  end;
  Result := True;
end;

function SizeOfFile(const FileName: string): int64;
var
  Err: longint;
  Rec: TSearchRec;
begin
  Err := SysUtils.FindFirst(FileName, faAnyFile, Rec);
  if (Err = 0) and ((Rec.Attr and faDirectory) = 0) then
    Result := Rec.Size
  else
    Result := -1;
  SysUtils.FindClose(Rec);
end;

{ oem-ansi charset functions }

function ParamToOem(const Param: string): string;
begin
  if Length(Param) <> 0 then
  begin
    {$IFDEF MSWINDOWS}
    SetLength(Result, Length(Param));
    CharToOem(PChar(Param), PChar(Result));
    {$ELSE}
    Result := Param;
    {$ENDIF}
  end else
    Result := '';
end;

function OemToParam(const Param: string): string;
begin
  if Length(Param) <> 0 then
  begin
    {$IFDEF MSWINDOWS}
    SetLength(Result, Length(Param));
    OemToChar(PChar(Param), PChar(Result));
    {$ELSE}
    Result := Param;
    {$ENDIF}
  end else
    Result := '';
end;

{ system control }


function SetPriority(Priority: longint): boolean;
{$IFDEF MSWINDOWS}
const
  PriorityValue: array [0..3] of longint = (IDLE_PRIORITY_CLASS,
    NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS);
begin
  Result := SetPriorityClass(GetCurrentProcess,
    PriorityValue[Max(0, Min(Priority, 3))]);
end;
{$ENDIF}
{$IFDEF UNIX}
begin
  // ...
end;
{$ENDIF}

procedure SetCtrlCHandler(CtrlHandler: pointer);
{$IFDEF UNIX}
var
  oa, na: SigActionRec;
{$ENDIF}
begin
{$IFDEF UNIX}
  na.sa_handler := SigActionHandler(CtrlHandler);
  FillChar(na.sa_mask, SizeOf(na.sa_mask), #0);
  na.sa_flags    := SA_ONESHOT;
  na.sa_restorer := nil;
  fpSigAction(SIGINT, @na, @oa);
{$ENDIF}

{$IFDEF MSWINDOWS}
  Windows.SetConsoleCtrlHandler(CtrlHandler, True);
{$ENDIF}
end;



(*




implementation



{ filename handling routines }



function FileNameLastPos(const Substr, Str: string): longint;
begin
  Result := FileNamePos(ReverseString(SubStr), ReverseString(Str));
  if (Result <> 0) then
  begin
    Result := Length(Str) - Length(SubStr) - Result + 2;
  end;
end;



function FileNameHasDrive(const FileName: string): boolean;
begin
  Result := Length(ExtractFileDrive(FileName)) > 0;
  if (Result = FALSE) and (Length(FileName) > 0) then
    Result := FileName[1] in AllowDirectorySeparators;
end;








{ directory handling routines }











{ low level functions }

function CreateText(var T: Text; const Name: string): boolean;
begin
  {$I-}
  Assign(T, Name);
  Rewrite(T);
  {$I+}
  Result := IOResult = 0;
end;

function AppendText(var T: Text; const Name: string): boolean;
begin
  {$I-}
  Assign(T, Name);
  Append(T);
  {$I+}
  Result := IOResult = 0;
end;

function OpenText(var T: Text; const Name: string): boolean;
begin
  {$I-}
  Assign(T, Name);
  Reset(T);
  {$I+}
  Result := IOResult = 0;
end;

function WriteText(const FileName, S: string): boolean;
var
  T: Text;
begin
  if AppendText(T, FileName) or CreateText(T, FileName) then
  begin
    Write(T, S);
    Close(T);
    Result := True;
  end else
    Result := False;
end;



     *)

end.
