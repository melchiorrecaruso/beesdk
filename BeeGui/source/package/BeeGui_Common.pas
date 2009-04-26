{
    Copyright (c) 2005-2007 Andrew Filinsky and Melchiorre Caruso

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

{   Contains:

    Various helper routines.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 Melchiorre Caruso;
    v1.0.1 build 9154 - 2005.07.22 Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 Melchiorre Caruso;
    v1.0.2 build 0250 - 2005.11.13 Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 Melchiorre Caruso
}

unit BeeGui_Common;

{$R-,Q-,S-}

interface

const
  DOC_FOLDER           = 'docs';
  SKIN_FOLDER          = 'skins';
  LANGUAGE_FOLDER      = 'languages';
  CONFIGURATION_FOLDER = 'configuration';

  SKIN_FOLDER_MASK     = SKIN_FOLDER + '\*.bmp';
  LANGUAGE_FOLDER_MASK = LANGUAGE_FOLDER + '\*.ini';

  HLP_FILENAME         = DOC_FOLDER + '\help.htm';
  LIC_FILENAME         = DOC_FOLDER + '\license.htm';

  function  SizeToStr (Size: integer): string;
  function  RatioToStr (Ratio: integer): string;
  function  AttrToStr (Attr: integer): string;
  function  SizeOfFile (const FileName: string): integer;
  function  PosText (const SubStr: string; const S: string): Integer;

  function  IncludeDelimiter (const FileName: string): string;
  function  ForceDirectories (const Dir: string): boolean;
  function  DirectoryExists (const DirName: string): boolean;
  function  ReturnCheckOutDir: string;
  function  ReturnTempDir: string;

  procedure DeleteDir (const DirName: string);

  function HexToStr (const Data; Count: integer): string;
  function DateTimeToString(DateTime: TDateTime): string;

implementation

uses
  SysUtils,

  BeeGui_WinOS;

const
  KB_SIZE  = 1024;
  MB_SIZE  = KB_SIZE * 1024;
  GB_SIZE  = MB_SIZE * 1024;

const
  HexaDecimals: array [0..15] of char = '0123456789ABCDEF';
  HexValues: array ['0'..'F'] of byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15);

  function HexToStr (const Data; Count: integer): string;
  var
    I, J: integer;
    K: cardinal;
  begin
    SetLength (Result, Count shl 1);
    J := 1;
    for I := 0 to Count - 1 do
    begin
      K := TByteArray (Data) [I];
      Result [J] := HexaDecimals [K shr 4]; Inc (J);
      Result [J] := HexaDecimals [K and $f]; Inc (J);
    end;
  end;

  function DateTimeToString(DateTime: TDateTime): string;
  begin
    SysUtils.DateTimeToString(Result, 'dd/mm/yyyy hh:mm', DateTime);
  end;

  function SizeToStr;
  begin
    if Size < KB_SIZE then
      Result := Format ('%u B', [Size])
    else
      if Size < MB_SIZE then
        Result := Format ('%f KB', [Size / KB_SIZE])
      else
        if Size < GB_Size then
          Result := Format ('%f MB', [Size / MB_SIZE])
        else
          Result := Format ('%f GB', [Size / MB_SIZE]);    
  end;

  function RatioToStr;
  begin
    Result := Format ('%u%%', [Ratio]);
  end;

  function AttrToStr;
  begin
    Result := '';
    if (Attr and faDirectory) = faDirectory then Result := Result + 'D';
    if (Attr and faReadOnly)  = faReadOnly  then Result := Result + 'R';
    if (Attr and faHidden)    = faHidden    then Result := Result + 'H';
    if (Attr and faSysFile)   = faSysFile   then Result := Result + 'S';
    if (Attr and faArchive)   = faArchive   then Result := Result + 'A';
  end;

  function PosText;
  begin
    Result := Pos (LowerCase (Substr), LowerCase (S));
  end;

  procedure DeletePathScan (const Path: String);
  var
    T: TSearchRec;
  begin
    if FindFirst (IncludeTrailingBackslash (Path) + '*.*', faAnyFile - faDirectory, T) = 0 then
      repeat
        if (not (T.Name = '.')) and (not (T.Name = '..')) then
          DeleteFile (IncludeTrailingBackslash (Path) + T.Name);
      until not (FindNext (T) = 0);
    FindClose (T);

    if FindFirst (IncludeTrailingBackslash (Path) + '*.*', faDirectory, T) = 0 then
      repeat
        if (not (T.Name = '.')) and (not (T.Name = '..')) then
          DeletePathScan (IncludeTrailingBackslash (Path) + T.Name);
      until not (FindNext (T) = 0);
    FindClose (T);

    RemoveDir (Path);
  end;

  function GenerateRandomName: string;
  var
    I: integer;
  begin
    Result := '????????';
    for I := 1 to 8 do Result [I] := char (byte ('A') + Random (byte ('Z') - byte ('A')));
  end;

  function DirectoryExists (const DirName: string): boolean;
  var
    Code: integer;
  begin
    Code := FileGetAttr (DirName);
    Result := (Code <> -1) and (faDirectory and Code <> 0);
  end;
  
  function ReturnCheckOutDir;
  begin
    repeat
      //Result := IncludeTrailingBackslash (BeeGui_GetWin_UserTmpDir) + GenerateRandomName;
      Result := IncludeTrailingBackslash (BeeGui_GetWin_DesktopDir) + GenerateRandomName;
    until DirectoryExists (Result) = False;
  end;

  function ReturnTempDir;
  begin
    repeat
      Result := IncludeTrailingBackslash (BeeGui_GetWin_UserTmpDir) + GenerateRandomName;
    until DirectoryExists (Result) = False;
  end;

  procedure DeleteDir;
  begin
    if not SetCurrentDir (DirName) then Exit;
    if not SetCurrentDir (ExtractFilePath (DirName)) then Exit;
    DeletePathScan (DirName);
    RemoveDir (DirName);
  end;

  function SizeOfFile (const FileName: string): integer;
  var
    F: TSearchRec;
  begin
    if FindFirst (FileName, faAnyFile - faDirectory, F) = 0 then
      Result := F.Size
    else
      Result := -1;
    FindClose (F);
  end;

  function ForceDirectories (const Dir: string): boolean;
  begin
    Result := True;
    if Dir = '' then exit;
    if Dir [Length (Dir)] = '\' then
      Result := ForceDirectories (Copy (Dir, 1, Length (Dir) - 1))
    else
    begin
      if DirectoryExists (Dir) or (ExtractFilePath (Dir) = Dir) then Exit;
      if ForceDirectories (ExtractFilePath (Dir)) then
        Result := CreateDir (Dir)
      else
        Result := False;
    end;
  end;

  function IncludeDelimiter (const FileName: string): string;
  begin
    if Length (FileName) = 0 then
      Result := FileName
    else
      Result := IncludeTrailingBackslash (FileName);
  end;

end.
