{
  Copyright (c) 1999-2010 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1110 - 2010.01.23 by Melchiorre Caruso.
}

unit Bee_Common;

{$I compiler.inc}

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$IFNDEF FPC} Math, {$ENDIF}
  Bee_Types,
  Classes;

{ filename handling routines }

function FileNamePos(const Substr, Str: string): longint;
function FileNameLastPos(const Substr, Str: string): longint;
function FileNameUseWildcards(const FileName: string): boolean;
function FileNameMatch(const FileName, Mask: string; Recursive: TRecursiveMode): boolean; overload;
function FileNameMatch(const FileName: string; Masks: TStringList; Recursive: TRecursiveMode): boolean; overload;

function IncludeTrailingBackSpace(const DirName: string): string;
function ExcludeTrailingBackSpace(const DirName: string): string;
function IncludeTrailingBackSlash(const DirName: string): string;
function ExcludeTrailingBackSlash(const DirName: string): string;

function CompareFileName(const S1, S2: string): longint;
procedure ExpandFileMask(const Mask: string; Masks: TStringList; Recursive: TRecursiveMode);

function DeleteFilePath(const FilePath, FileName: string): string;
function DeleteFileDrive(const FileName: string): string;
function DoDirSeparators(const FileName: string): string;
function FixFileName(const FileName: string): string;
function FixDirName(const DirName: string): string;

function SelfName: string;
function SelfPath: string;
function GenerateFileName(const FilePath: string): string;
function GenerateAlternativeFileName(const FileName: string; Check: boolean): string;

{ directory handling routines }

function DirectoryExists(const DirName: string): boolean;
function ForceDirectories(const DirName: string): boolean;

{ oem-ansi charset functions }

function ParamToOem(const Param: string): string;
function OemToParam(const Param: string): string;

{ string, pchar routines }

function StringToPChar(const aValue: string): PChar;
function PCharToString(aValue: PChar): string;
procedure FreePChar(var aValue: PChar);

function RatioToStr(const PackedSize, Size: int64): string;
function SizeToStr(const Size: int64): string;
function AttrToStr(Attr: longint): string;

function ReverseString(const Str: string): string; inline;

{ time handling routines }

function TimeDifference(X: double): string;
function TimeToStr(T: longint): string;
function DateTimeToString(X: TDateTime): string; overload;
function DateTimeToString(X: TDateTime; const Format: string): string; overload;
function FileTimeToString(X: longint): string; overload;
function FileTimeToString(X: longint; const Format: string): string; overload;

{ hex routines }

function Hex(const Data; Count: longint): string;
function HexToData(const S: string; var Data; Count: longint): boolean;

{ low level functions }

function CreateText(var T: Text; const Name: string): boolean;
function AppendText(var T: Text; const Name: string): boolean;
function OpenText(var T: Text; const Name: string): boolean;
function WriteText(const FileName, S: string): boolean;

function SizeOfFile(const FileName: string): int64;

{ system control }

{$IFDEF MSWINDOWS}
function SetPriority(Priority: longint): boolean; { Priority is 0..3 }
{$ENDIF}

procedure SetCtrlCHandler(CtrlHandler: pointer);

implementation

uses
  SysUtils;

const
  HexaDecimals: array [0..15] of char = '0123456789ABCDEF';
  HexValues: array ['0'..'F'] of byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15);

{ filename handling routines }

function FileNamePos(const Substr, Str: string): longint; inline;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  Result := System.Pos(SubStr, Str);
  {$ELSE}
  Result := System.Pos(UpperCase(SubStr), UpperCase(Str));
  {$ENDIF}
end;

function FileNameLastPos(const Substr, Str: string): longint; inline;
begin
  Result := FileNamePos(ReverseString(SubStr), ReverseString(Str));
  if (Result <> 0) then
  begin
    Result := Length(Str) - Length(SubStr) - Result + 2;
  end;
end;

function FileNameUseWildcards(const FileName: string): boolean; inline;
begin
  if System.Pos('*', FileName) > 0 then
    Result := True
  else
    if System.Pos('?', FileName) > 0 then
      Result := True
    else
      Result := False;
end;

function MatchPattern(Element, Pattern: PChar): boolean; inline;
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

function CharCount(const S: string; C: char): longint; inline;
var
  I: longint;
begin
  Result := 0;
  for I := 1 to Length(S) do
  begin
    if CompareFileName(S[I], C) = 0 then
      Inc(Result);
  end;
end;

function FileNameMatch(const FileName, Mask: string; Recursive: TRecursiveMode): boolean; inline;
var
  iFileDrive: string;
  iFileName: string;
  iMaskPath: string;
  iMask: string;
  I: longint;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  iFileName := FileName;
  iMask     := Mask;
  {$ELSE}
  iFileName := UpperCase(FileName);
  iMask     := UpperCase(Mask);
  {$ENDIF}

  if ExtractFileDrive(iMask) = '' then
  begin
    iFileDrive := ExtractFileDrive(iFileName);
    if iFileDrive <> '' then
    begin
      iMask := IncludeTrailingBackSlash(iFileDrive) + iMask;
    end;
  end;

  if Recursive = rmWildCard then
  begin
    if FileNameUseWildCards(ExtractFileName(Mask)) then
      Recursive := rmFull
    else
      Recursive := rmNone;
  end;

  if Recursive = rmFull then
  begin
    iMaskPath := ExtractFilePath(iMask);
    for I := 1 to CharCount(iFileName, PathDelim) - CharCount(iMask, PathDelim) do
    begin
      iMaskPath := IncludeTrailingBackSlash(iMaskPath) + IncludeTrailingBackSlash('*');
    end;
    iMask := iMaskPath + ExtractFileName(iMask);
  end;

  if CharCount(iFileName, PathDelim) = CharCount(iMask, PathDelim) then
  begin
    if MatchPattern(PChar(ExtractFilePath(iFileName)), PChar(ExtractFilePath(iMask))) then
    begin
      Result := MatchPattern(PChar(ExtractFileName(iFileName)), PChar(ExtractFileName(iMask)))
    end else
    begin
      Result := False;
    end;
  end else
    Result := False;
end;

function FileNameMatch(const FileName: string; Masks: TStringList; Recursive: TRecursiveMode): boolean; inline;
var
  I: longint;
begin
  Result := False;
  for I := 0 to Masks.Count - 1 do
  begin
    if FileNameMatch(FileName, Masks[I], Recursive) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IncludeTrailingBackSpace(const DirName: string): string; inline;
var
  Len: longint;
begin
  Len := Length(DirName);
  if (Len > 0) and (not (DirName[Len] in [' '])) then
    Result := DirName + ' '
  else
    Result := DirName;
end;

function ExcludeTrailingBackSpace(const DirName: string): string; inline;
var
  Len: longint;
begin
  Len := Length(DirName);
  if (Len > 0) and (DirName[Len] in [' ']) then
    Result := Copy(DirName, 1, Len - 1)
  else
    Result := DirName;
end;

function IncludeTrailingBackSlash(const DirName: string): string; inline;
var
  Len: longint;
begin
  Len := Length(DirName);
  if (Len > 0) and (not (DirName[Len] in ['\', '/'])) then
    Result := DirName + PathDelim
  else
    Result := DirName;
end;

function ExcludeTrailingBackSlash(const DirName: string): string; inline;
var
  Len: longint;
begin
  Len := Length(DirName);
  if (Len > 0) and (DirName[Len] in ['\', '/']) then
    Result := Copy(DirName, 1, Len - 1)
  else
    Result := DirName;
end;

function CompareFileName(const S1, S2: string): longint; inline;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  Result := SysUtils.CompareStr(S1, S2);
  {$ELSE}
  Result := SysUtils.CompareText(S1, S2);
  {$ENDIF}
end;

procedure ExpandFileMask(const Mask: string; Masks: TStringList; Recursive: TRecursiveMode); inline;
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
  LastSlash := 0;
  Card := False;

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
    Error      := FindFirst(FolderPath + '*', faAnyFile, Rec);
    while Error = 0 do
    begin
      if ((Rec.Attr and faDirectory) = faDirectory) and
          (Rec.Name[1] <> '.') and (Rec.Name[1] <> '..') then
        if FileNameMatch(Rec.Name, FolderName, Recursive) then
          ExpandFileMask(FolderPath + Rec.Name + Copy(Mask, LastSlash,
            (Length(Mask) + 1) - LastSlash), Masks, Recursive);

      Error := FindNext(Rec);
    end;
    FindClose(Rec);
  end else
    if Masks.IndexOf(Mask) = -1 then Masks.Add(Mask);
end;

function DeleteFilePath(const FilePath, FileName: string): string; inline;
begin
  Result := FileName;
  if FileNamePos(FilePath, Result) = 1 then
  begin
    Delete(Result, 1, Length(FilePath));
  end;
end;

function DeleteFileDrive(const FileName: string): string; inline;
var
  Drive: string;
begin
  Result := FileName;
  if Length(Result) > 0 then
  begin
    Drive := ExtractFileDrive(Result);
    System.Delete(Result, 1, Length(Drive));
  end;
end;

function DoDirSeparators(const FileName: string): string; inline;
var
  I, Len: longint;
begin
  Result := FileName;
  Len := Length(Result);
  for I := 1 to Len do
  begin
    if Result[I] in ['\', '/'] then
      Result[I] := PathDelim;
  end;
end;

function FixFileName(const FileName: string): string; inline;
var
  I: longint;
begin
  Result := DoDirSeparators(FileName);
  Result := DeleteFileDrive(Result);

  I := System.Pos('*', Result);
  while I > 0 do
  begin
    Delete(Result, I, 1);
    I := System.Pos('*', Result);
  end;

  I := System.Pos('?', Result);
  while I > 0 do
  begin
    Delete(Result, I, 1);
    I := System.Pos('?', Result);
  end;

  I := System.Pos('"', Result);
  while I > 0 do
  begin
    Delete(Result, I, 1);
    I := System.Pos('"', Result);
  end;

  Result := ExcludeTrailingBackSlash(Result);
end;

function FixDirName(const DirName: string): string; inline;
var
  I: longint;
begin
  Result := DoDirSeparators(DirName);
  Result := DeleteFileDrive(Result);

  I := System.Pos('*', Result);
  while I > 0 do
  begin
    Delete(Result, I, 1);
    I := System.Pos('*', Result);
  end;

  I := System.Pos('?', Result);
  while I > 0 do
  begin
    Delete(Result, I, 1);
    I := System.Pos('?', Result);
  end;

  I := System.Pos('"', Result);
  while I > 0 do
  begin
    Delete(Result, I, 1);
    I := System.Pos('"', Result);
  end;
end;

function SelfName: string; inline;
begin
  Result := ExtractFileName(ParamStr(0));
end;

function SelfPath: string; inline;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function GenerateFileName(const FilePath: string): string; inline;
var
  I: longint;
begin
  repeat
    Result := '????????.$$$';
    for I := 1 to 8 do
    begin
      Result[I] := char(byte('A') + Random(byte('Z') - byte('A')));
    end;
    Result := IncludeTrailingBackSlash(FilePath) + Result;
  until FileAge(Result) = -1;
end;

function GenerateAlternativeFileName(const FileName: string; Check: boolean): string; inline;
var
  I: longint;
begin
  I := 0;
  repeat
    Inc(I);
    Result := ChangeFileExt(FileName, '.' +  IntToStr(I) + ExtractFileExt(FileName));
  until (Check = False) or (FileAge(Result) = -1);
end;

{ directory handling routines }

function DirectoryExists(const DirName: string): boolean; inline;
var
  Code: longint;
begin
  Code := FileGetAttr(DirName);
  Result := (Code <> -1) and (faDirectory and Code <> 0);
end;

function ForceDirectories(const DirName: string): boolean; inline;
var
  S: string;
begin
  if Length(DirName) <> 0 then
  begin
    S := ExcludeTrailingBackSlash(DirName);

    if (DirectoryExists(S) = False) and (ExtractFilePath(S) <> S) then
    begin
      if ForceDirectories(ExtractFilePath(S)) then
        Result := CreateDir(S)
      else
        Result := False;
    end else
      Result := True;

  end else
    Result := True;
end;

{ oem-ansi charset functions }

function ParamToOem(const Param: string): string; inline;
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

function OemToParam(const Param: string): string; inline;
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

{ string and pchar routines }

function StringToPChar(const aValue: string): PChar; inline;
begin
  Result := StrAlloc(Length(aValue) + 1);
  Result := StrPCopy(Result, aValue);
end;

function PCharToString(aValue: PChar): string; inline;
var
  I: longint;
begin
  if aValue <> nil then
  begin
    I := StrLen(aValue);
    if I <> 0 then
    begin
      SetLength(Result, I);
      Move(aValue^, Result[1], I);
    end;
  end else
    SetLength(Result, 0);
end;

procedure FreePChar(var aValue: PChar); inline;
begin
  StrDispose(aValue);
  aValue := nil;
end;

function RatioToStr(const PackedSize, Size: int64): string; inline;
begin
  if Size > 0 then
    Result := Format('%u%%', [Round((PackedSize / Size) * 100)])
  else
    Result := Format('%u%%', [100]);
end;

function SizeToStr(const Size: int64): string; inline;
begin
  Result := Format('%u', [Size]);
end;

function AttrToStr(Attr: longint): string; inline;
begin
  Result := 'RHSVDAL';
  if Attr and faReadOnly  = 0 then Result[1] := '.';
  if Attr and faHidden    = 0 then Result[2] := '.';
  if Attr and faSysFile   = 0 then Result[3] := '.';
  if Attr and faVolumeId  = 0 then Result[4] := '.';
  if Attr and faDirectory = 0 then Result[5] := '.';
  if Attr and faArchive   = 0 then Result[6] := '.';
  if Attr and faSymLink   = 0 then Result[7] := '.';
end;

function ReverseString(const Str: string): string; inline;
var
  I, Len: longint;
begin
  Len := Length(Str);
  SetLength(Result, Len);
  for I := 1 to Len do
  begin
    Result[I] := Str[(Len + 1) - I];
  end;
end;

function TimeDifference(X: double): string; inline;
begin
  Result := Format('%0.2f', [(Now - X) * (24 * 60 * 60)]);
end;

function TimeToStr(T: longint): string; inline;
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

function DateTimeToString(X: TDateTime): string; inline;
begin
  SysUtils.DateTimeToString(Result, 'dd/mm/yy hh:mm', X);
end;

function DateTimeToString(X: TDateTime; const Format: string): string; inline;
begin
  SysUtils.DateTimeToString(Result, Format, X);
end;

function FileTimeToString(X: longint): string;
begin
  try
    Result := DateTimeToString(FileDateToDateTime(X));
  except
    Result := '--/--/-- --:--';
  end;
end;

function FileTimeToString(X: longint; const Format: string): string;
begin
  try
    Result := DateTimeToString(FileDateToDateTime(X), Format);
  except
    Result := '--/--/-- --:--';
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

function SizeOfFile(const FileName: string): int64;
var
  Err: longint;
  Rec: TSearchRec;
begin
  Err := FindFirst(FileName, faAnyFile, Rec);
  if (Err = 0) and ((Rec.Attr and faDirectory) = 0) then
    Result := Rec.Size
  else
    Result := -1;
  FindClose(Rec);
end;

{ system control }

{$IFDEF MSWINDOWS}
function SetPriority(Priority: longint): boolean;
const
  PriorityValue: array [0..3] of longint = (IDLE_PRIORITY_CLASS,
    NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS);
begin
  Result := SetPriorityClass(GetCurrentProcess,
    PriorityValue[Max(0, Min(Priority, 3))]);
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
  FillChar(na.sa_mask, sizeof(na.sa_mask), #0);
  na.sa_flags    := SA_ONESHOT;
  na.sa_restorer := nil;
  fpSigAction(SIGINT, @na, @oa);
{$ENDIF}

{$IFDEF MSWINDOWS}
  Windows.SetConsoleCtrlHandler(CtrlHandler, True);
{$ENDIF}
end;

end.
