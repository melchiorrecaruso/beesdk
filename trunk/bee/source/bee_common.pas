{
  Copyright (c) 1999-2008 Andrew Filinsky and Melchiorre Caruso

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

  v0.7.9 build 0890 - 2008.10.18 by Melchiorre Caruso.
}

unit Bee_Common;

{$i compiler.inc}

interface

uses
  {$IFNDEF FPC}
  Math,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes,
  SysUtils;

const
  Cr = #13#10;

const
  msgUpdating   = 'Updating   ';
  msgFreshing   = 'Freshing   ';
  msgReplacing  = 'Replacing  ';
  msgExtracting = 'Extracting ';
  msgTesting    = 'Testing    ';
  msgSkipping   = 'Skipping   ';
  msgEncoding   = 'Encoding   ';
  msgDecoding   = 'Decoding   ';
  msgCopying    = 'Copying    ';
  msgMoving     = 'Moving     ';
  msgDeleting   = 'Deleting   ';
  msgScanning   = 'Scanning   ';
  msgOpening    = 'Opening    ';
  msgListing    = 'Listing    ';
  msgRenaming   = 'Renaming   ';
  msgRename     = 'Rename     ';
  msgAdding     = 'Adding     ';
  msgCRCERROR   = 'CRC-ERROR  ';
  msgFailed     = 'Failed     ';

// filename handling routines ...

function FileNamePos(const Substr, Str: string): integer;
function FileNameLastPos(const Substr, Str: string): integer;

function IncludeTrailingBackSpace(const DirName: string): string;
function ExcludeTrailingBackSpace(const DirName: string): string;

function IncludeTrailingBackSlash(const DirName: string): string;
function ExcludeTrailingBackSlash(const DirName: string): string;

function FileNameUseWildcards(const FileName: string): boolean;
function FileNameMatch(const FileName, Mask: string; Recursive: boolean): boolean; overload;
function FileNameMatch(const FileName: string; Masks: TStringList; Recursive: boolean): boolean; overload;

function CompareFileName(const S1, S2: string): integer;
function ExtractFileDrive(const FileName: string): string;

function DeleteFilePath(const FilePath, FileName: string): string;
function DeleteFileDrive(const FileName: string): string;
function DoDirSeparators(const FileName: string): string;
function FixFileName(const FileName: string): string;
function FixDirName(const DirName: string): string;

// directory handling routines ...

function DirectoryExists(const DirName: string): boolean;
function ForceDirectories(const Dir: string): boolean;

// oem-ansi charset functions

function ParamToOem(const Param: string): string;
function OemToParam(const Param: string): string;

// filename handling routines ...

function SelfName: string;
function SelfPath: string;
function GenerateFileName(const Path: string): string;

// string routines ...

function SizeToStr(const Size: Int64): string;
function RatioToStr(const PackedSize, Size: Int64): string;
function AttrToStr(Attr: integer): string;

// time handling routines ...

function TimeDifference(X: double): string;
function TimeToStr(T: integer): string;
function DateTimeToString(X: TDateTime): string; overload;
function DateTimeToString(X: TDateTime; const Format: string): string; overload;
function FileTimeToString(X: integer): string; overload;
function FileTimeToString(X: integer; const Format: string): string; overload;

// hex routines ...

function Hex(const Data; Count: integer): string;
function HexToData(const S: string; var Data; Count: integer): boolean;

// low level functions ...

function CreateText(var T: Text; const Name: string): boolean;
function AppendText(var T: Text; const Name: string): boolean;
function OpenText(var T: Text; const Name: string): boolean;
function WriteText(const FileName, S: string): boolean;

function SizeOfFile(const FileName: string): Int64;

// system control

{$IFDEF MSWINDOWS}
function SetPriority(Priority: integer): boolean; // Priority is 0..3
{$ENDIF}

implementation

uses
  Bee_Assembler;

const
  DoublePathDelim = PathDelim + PathDelim;

  HexaDecimals: array [0..15] of char = '0123456789ABCDEF';
  HexValues: array ['0'..'F'] of byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0,
    0, 0, 0, 0, 10, 11, 12, 13, 14, 15);

// string handling routines ...

function FileNamePos(const Substr, Str: string): integer;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  Result := System.Pos(SubStr, Str);
  {$ELSE}
  Result := System.Pos(UpperCase(SubStr), UpperCase(Str));
  {$ENDIF}
end;

function FileNameLastPos(const Substr, Str: string): integer;
begin
  Result := Length(Str);
  while (Result > 0) and (CompareFileName(Copy(Str, Result, Length(Substr)),
      Substr) <> 0) do
    Dec(Result);
end;

function CompareFileName(const S1, S2: string): integer;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  Result := SysUtils.CompareStr(S1, S2);
  {$ELSE}
  Result := SysUtils.CompareText(S1, S2);
  {$ENDIF}
end;

function ExtractFileDrive(const FileName: string): string;
var
  I, L: integer;
begin
  L := Length(FileName);
  I := Pos(':', FileName);
  while I < L do
    if FileName[I + 1] in ['\', '/'] then
      Inc(I)
    else
      Break;
  Result := Copy(FileName, 1, I);
end;

function DeleteFilePath(const FilePath, FileName: string): string;
begin
  Result := FileName;
  if FileNamePos(FilePath, Result) = 1 then
    Delete(Result, 1, Length(FilePath));
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
end;

function IncludeTrailingBackSlash(const DirName: string): string;
var
  L: integer;
begin
  L := Length(DirName);
  if (L > 0) and (not (DirName[L] in ['\', '/'])) then
    Result := DirName + PathDelim
  else
    Result := DirName;
end;

function ExcludeTrailingBackSlash(const DirName: string): string;
var
  L: integer;
begin
  L := Length(DirName);
  if (L > 0) and (DirName[L] in ['\', '/']) then
    Result := Copy(DirName, 1, L - 1)
  else
    Result := DirName;
end;

function IncludeTrailingBackSpace(const DirName: string): string;
var
  L: integer;
begin
  L := Length(DirName);
  if (L > 0) and (not (DirName[L] in [' '])) then
    Result := DirName + ' '
  else
    Result := DirName;
end;

function ExcludeTrailingBackSpace(const DirName: string): string;
var
  L: integer;
begin
  L := Length(DirName);
  if (L > 0) and (DirName[L] in [' ']) then
    Result := Copy(DirName, 1, L - 1)
  else
    Result := DirName;
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

    else if Element^ = Pattern^ then
        Result :=
          MatchPattern(@Element[1], @Pattern[1])
      else
        Result := False;
    end// end case
  ;
end;

function CharCount(const S: string; C: char): integer;
var
  I: integer;
  L: integer;
begin
  Result := 0;
  L      := Length(S);
  for I := 1 to L do
    if CompareFileName(S[I], C) = 0 then
      Inc(Result);
end;

function FileNameMatch(const FileName, Mask: string;
  Recursive: boolean): boolean;
var
  iFileDrive: string;
  iFileName: string;
  iMaskPath: string;
  iMask: string;
  I: integer;
begin
  {$IFDEF FILENAMECASESENSITIVE}
  iFileName := FileName;
  iMask := Mask;
  {$ELSE}
  iFileName := UpperCase(FileName);
  iMask     := UpperCase(Mask);
  {$ENDIF}

  if ExtractFileDrive(iMask) = '' then
  begin
    iFileDrive := ExtractFileDrive(iFileName);
    if iFileDrive <> '' then
      iMask := IncludeTrailingBackSlash(iFileDrive) + iMask;
  end;

  if Recursive then
  begin
    iMaskPath := ExtractFilePath(iMask);
    for I := 1 to CharCount(iFileName, PathDelim) -
      CharCount(iMask, PathDelim) do
      iMaskPath := IncludeTrailingBackSlash(iMaskPath) +
        IncludeTrailingBackSlash('*');
    iMask := iMaskPath + ExtractFileName(iMask);
  end;

  if CharCount(iFileName, PathDelim) = CharCount(iMask, PathDelim) then
  begin
    if MatchPattern(PChar(ExtractFilePath(iFileName)),
      PChar(ExtractFilePath(iMask))) then
      Result := MatchPattern(PChar(ExtractFileName(iFileName)),
        PChar(ExtractFileName(iMask)))
    else
    begin
      Result := False;
    end;
  end else
    Result := False;
end;

function FileNameMatch(const FileName: string; Masks: TStringList;
  Recursive: boolean): boolean;
var
  I: integer;
begin
  Result := False;
  if (Assigned(Masks)) and (Masks.Count > 0) then
    for I := 0 to Masks.Count - 1 do
      if FileNameMatch(FileName, Masks.Strings[I], Recursive) then
      begin
        Result := True;
        Break;
      end;
end;

function FileNameUseWildcards(const FileName: string): boolean;
begin
  if System.Pos('*', FileName) > 0 then
    Result := True
  else
  if System.Pos('?', FileName) > 0 then
    Result := True
  else
    Result := False;
end;

function DoDirSeparators(const FileName: string): string;
var
  I, L: longint;
begin
  Result := FileName;
  L      := Length(Result);
  for I := 1 to L do
    if Result[I] in ['\', '/'] then
      Result[I] := PathDelim;
end;

function FixFileName(const FileName: string): string;
var
  I: integer;
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

function FixDirName(const DirName: string): string;
var
  I: integer;
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

// oem-ansi charset functions

function ParamToOem(const Param: string): string;
begin
  if (Param = '') then
    Result := ''
  else
  begin
    {$IFDEF MSWINDOWS}
    SetLength(Result, Length(Param));
    CharToOem(PChar(Param), PChar(Result));
    {$ELSE}
    Result := Param;
    {$ENDIF}
  end;
end;

function OemToParam(const Param: string): string;
begin
  if (Param = '') then
    Result := ''
  else
  begin
    {$IFDEF MSWINDOWS}
    SetLength(Result, Length(Param));
    OemToChar(PChar(Param), PChar(Result));
    {$ELSE}
    Result := Param;
    {$ENDIF}
  end;
end;

function TimeDifference(X: double): string;
begin
  Result := Format('%0.2f', [(Now - X) * (24 * 60 * 60)]);
end;

function TimeToStr(T: integer): string;
var
  H, M, S:    string;
  ZH, ZM, ZS: integer;
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
  SysUtils.DateTimeToString(Result, 'dd/mm/yy hh:mm', X);
end;

function DateTimeToString(X: TDateTime; const Format: string): string;
begin
  SysUtils.DateTimeToString(Result, Format, X);
end;

function FileTimeToString(X: integer): string;
begin
  try
    Result := DateTimeToString(FileDateToDateTime(X));
  except
    Result := '--/--/-- --:--';
  end;
end;

function FileTimeToString(X: integer; const Format: string): string;
begin
  try
    Result := DateTimeToString(FileDateToDateTime(X), Format);
  except
    Result := '--/--/-- --:--';
  end;
end;

function DirectoryExists(const DirName: string): boolean;
var
  Code: integer;
begin
  Code   := FileGetAttr(DirName);
  Result := (Code <> -1) and (faDirectory and Code <> 0);
end;

function ForceDirectories(const Dir: string): boolean;
begin
  Result := True;
  if Dir = '' then
    Exit;

  if Dir[Length(Dir)] = PathDelim then
    Result := ForceDirectories(Copy(Dir, 1, Length(Dir) - 1))
  else
  begin
    if DirectoryExists(Dir) or (ExtractFilePath(Dir) = Dir) then
      Exit;

    if ForceDirectories(ExtractFilePath(Dir)) then
      Result := CreateDir(Dir)
    else
      Result := False;
  end;
end;

// filename handling routines ...

function SelfName: string;
begin
  Result := ExtractFileName(ParamStr(0));
end;

function SelfPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function GenerateFileName(const Path: string): string;
var
  I: integer;
begin
  repeat
    Result := '????????.$$$';
    for I := 1 to 8 do
      Result[I] := char(byte('A') + Random(byte('Z') - byte('A')));
    Result := IncludeTrailingBackSlash(Path) + Result;
  until FileAge(Result) = -1;
end;

// string routines

function SizeToStr(const Size: Int64): string;
begin
  Result := Format('%u', [Size]);
end;

function RatioToStr(const PackedSize, Size: Int64): string;
begin
  if Size > 0 then
    Result := Format('%u%%', [MulDiv(PackedSize, 100, Size)])
  else
    Result := Format('%u%%', [100]);
end;

function AttrToStr(Attr: integer): string;
begin
  Result := '..RHSA';
  if Attr and faReadOnly = 0 then
    Result[3] := '.';
  if Attr and faHidden = 0 then
    Result[4] := '.';
  if Attr and faSysFile = 0 then
    Result[5] := '.';
  if Attr and faArchive = 0 then
    Result[6] := '.';
end;

// hex routines ...

function Hex(const Data; Count: integer): string;
var
  I, J: integer;
  K:    cardinal;
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

function HexToData(const S: string; var Data; Count: integer): boolean;
var
  I: integer;
begin
  Result := False;
  if Length(S) < Count * 2 then
    Exit;

  for I := 0 to Count - 1 do
    if (S[I * 2 + 1] in ['0'..'9', 'A'..'F']) and
      (S[I * 2 + 2] in ['0'..'9', 'A'..'F']) then
      TByteArray(Data)[I] :=
        HexValues[S[I * 2 + 1]] shl 4 + HexValues[S[I * 2 + 2]]
    else
      Exit;
  Result := True;
end;

// low level functions...

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

function SizeOfFile(const FileName: string): Int64;
var
  Err: integer;
  Rec: TSearchRec;
begin
  Err := FindFirst(FileName, faAnyFile, Rec);
  if (Err = 0) and ((Rec.Attr and faDirectory) = 0) then
    Result := Rec.Size
  else
    Result := -1;
  FindClose(Rec);
end;

// system control

{$IFDEF MSWINDOWS}
function SetPriority(Priority: integer): boolean; // Priority is 0..3
const
  PriorityValue: array [0..3] of integer =
    (IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS,
    REALTIME_PRIORITY_CLASS);
begin
  Result := SetPriorityClass(GetCurrentProcess,
    PriorityValue[Max(0, Min(Priority, 3))]);
end;
{$ENDIF}

end.
