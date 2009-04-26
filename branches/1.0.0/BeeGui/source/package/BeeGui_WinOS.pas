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

    TBGFileInfo class,
    Various helper system routines

    Modifyed:

    v1.0.1 build 9147 - 2005.07.11 Melchiorre Caruso;
    v1.0.1 build 9154 - 2005.07.24 Melchiorre Caruso;
    v1.0.1 build 9160 - 2005.08.03 Melchiorre Caruso;
    v1.0.2 build 0250 - 2005.11.13 Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 Melchiorre Caruso.
}

unit BeeGui_WinOS;

{$R-,Q-,S-}

interface

uses
  ShlObj,
  ActiveX,
  Dialogs,
  Windows,
  Classes,
  ShellApi,
  Controls,
  SysUtils,
  Registry;

type
  TBeeGui_FileInfo = record
    IconIndex: Integer;
    SelectIconIndex: Integer;
    TypeName: string;
  end;

procedure BeeGui_LoadListViewImages (Small, Large: TImageList);

function  BeeGui_DiskInDrive (Drive: Char): boolean;
procedure BeeGui_GetDrivers (var DriveList: TStringList);
function  BeeGui_GetFileImageIndex (const Str: string; const A: integer): integer;
function  BeeGui_GetFileOpenImageIndex (const Str: string; const A: integer): integer;
function  BeeGui_GetDirList (const Dir: string; var DirList: TStringList): boolean;
procedure BeeGui_GetFileInfo (const Str: string; A: integer; var FI: TBeeGui_FileInfo);
function  BeeGui_ShellExecute (const FileName: string; const PathName: string): integer;
procedure BeeGui_ShellExecuteAndWait (const FileName: string; const PathName: string; Wait: boolean);

function  BeeGui_GetCurrentUserName: string;
function  BeeGui_GetWin_AppDataDir:  string;
procedure BeeGui_RegisterFileType (prefix: string; exepfad: string);
procedure BeeGui_UnRegisterFileType (prefix: string; exepfad: string);
function  BeeGui_ShellFindExecutable (const FileName, DefaultDir: string): string;
function  BeeGui_CheckRegisterFileType (prefix: string; exepfad: string): boolean;

function  BeeGui_GetWin_UserTmpDir: string;
function  BeeGui_GetWin_StartMenuDir: string;
function  BeeGui_GetWin_StartMenuProgramsDir: string;
function  BeeGui_GetWin_DesktopDir: string;

function  BeeGui_DragQueryFile (Drop: integer; FileIndex: cardinal; FileName: PChar; cb: cardinal): integer;
procedure BeeGui_DragFinish (Drop: integer);

// File version routines

function  BeeGui_FileVersion (const FileName: string): string;
function  BeeGui_CompareVersion (const VerA, VerB: string): boolean;
procedure BeeGui_ExpandVersion (Version : string; out Major, Minor, Release, Build: Integer);

implementation

uses
  BeeGui_Common;

  function BeeGui_DiskInDrive;
  var
    ErrorMode: word;
  begin
    if Drive in ['a'..'z'] then Dec (Drive, $20);
    if (Drive in ['A'..'Z']) then
    begin
      ErrorMode := SetErrorMode (SEM_FailCriticalErrors);
      try
        if DiskSize (Ord (Drive) - $40) = -1 then
          Result:=False
        else
          Result:=True;
      finally
        SetErrorMode (ErrorMode);
      end;
    end
    else
      Result := False;
  end;

  function BeeGui_GetDirList;
  var
    Info: TSearchRec;
    err: Integer;
  begin
    Result := False;
    DirList.Clear;

    if not BeeGui_DiskInDrive (Dir [1]) then Exit;
    if not SetCurrentDir (Dir) then Exit;

    err := SysUtils.FindFirst ('*.*' ,faDirectory, Info);
    while err = 0 do
    begin
      if ((Info.Attr and faHidden) = 0) and
         ((Info.Attr and faDirectory) = faDirectory) and
         (Info.Name <> '..') and
         (Info.Name <> '.') then
        DirList.Add(Info.Name);

      err := SysUtils.FindNext (Info);
    end;
    SysUtils.FindClose (Info);

    Result := True;
  end;

  procedure BeeGui_GetDrivers;
  var
    i, PX: Integer;
    TPC: PChar;
    TMR, DM: string;
  begin
    DM := '';
    DriveList.Clear;
    GetMem (TPC, 100);
    PX := GetLogicalDriveStrings (100, TPC);
    for i := - 1 to PX do if (TPC + i) [1] <> #0 then DM := DM + (TPC + i) [1];
    FreeMem (TPC);
    TMR := '';
    for i := 1 to Length (DM) do
    begin
      TMR := TMR + DM [i];
      if (i mod 3) = 0 then begin
        DriveList.Add (TMR);
        TMR := '';
      end;
    end;
  end;

  function BeeGui_ShellExecute;
  begin
    Result := ShellExecute (0, 'open', PChar (FileName), nil, PChar (PathName), SW_SHOW);
  end;

  procedure BeeGui_ShellExecuteAndWait;
  var
    SEI: TShellExecuteInfo;
  begin
    FillChar (SEI, SizeOf (SEI), #0);
    SEI.cbSize := SizeOf (SEI);
    if Wait then SEI.fMask := SEE_MASK_NOCLOSEPROCESS;

    SEI.lpFile := PChar (FileName);
    SEI.lpDirectory := PChar (PathName);
    SEI.nShow := SW_NORMAL;

    if ShellExecuteEx (@SEI) then
    begin
      if Wait then WaitForSingleObject (SEI.hProcess, INFINITE);
      CloseHandle (SEI.hProcess);
    end;
  end;

  procedure BeeGui_GetFileInfo;
  var
    SFI: TSHFileInfo;
  begin
    SHGetFileInfo (PChar (Str), A, SFI, SizeOf (TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES);
    FI.IconIndex := SFI.iIcon;
    FI.TypeName := SFI.szTypeName;
  end;

  function BeeGui_GetFileImageIndex;
  var
    SFI: TSHFileInfo;
  begin
    SHGetFileInfo (PChar (Str), A, SFI, SizeOf (TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES);
    Result := SFI.iIcon;
  end;

  function BeeGui_GetFileOpenImageIndex;
  var
    SFI: TSHFileInfo;
  begin
    SHGetFileInfo (PChar (Str), A, SFI, SizeOf (TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES or SHGFI_OPENICON);
    Result := SFI.iIcon;
  end;

  procedure BeeGui_LoadListViewImages;
  var
    SysImageList: uint;
    SFI: TSHFileInfo;
  begin
    SysImageList := SHGetFileInfo ('', 0, SFI, SizeOf (TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    if SysImageList <> 0 then
    begin
      Small.Handle := SysImageList;
      Small.ShareImages := TRUE;
    end;

    SysImageList := SHGetFileInfo ('', 0, SFI, SizeOf (TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
    if SysImageList <> 0 then
    begin
      Large.Handle := SysImageList;
      Large.ShareImages := TRUE;
    end;
  end;

  function BeeGui_GetCurrentUserName;
  const
    cnMaxUserNameLen = 254;
  var
    sUserName: string;
    dwUserNameLen: DWORD;
  begin
    dwUserNameLen := cnMaxUserNameLen - 1;
    SetLength (sUserName, cnMaxUserNameLen);
    GetUserName (PChar (sUserName), dwUserNameLen);
    SetLength (sUserName, dwUserNameLen);
    Result := sUserName;
  end;

  procedure BeeGui_RegisterFileType;
  var
    reg: TRegistry;
  begin
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;

      reg.OpenKey ('.' + prefix, True);
      try
        reg.Writestring ('', prefix + 'file');
      finally
        reg.CloseKey;
      end;

      reg.CreateKey (prefix + 'file');
      reg.OpenKey (prefix + 'file\DefaultIcon', True);
      try
        reg.Writestring('', exepfad + ',0');
      finally
        reg.CloseKey;
      end;

      reg.OpenKey (prefix + 'file\shell\open\command', True);
      try
        reg.Writestring ('', exepfad+' "l" "%1" "*\*.*"');
      finally
        reg.CloseKey;
      end;

      //reg.CreateKey (prefix + 'file\shell\BeeGui');
      //reg.OpenKey (prefix + 'file\shell\BeeGui\Extract all...\command', True);
      //try
      //  reg.Writestring('', exepfad+' "x" "%1" "*\*.*"');
      //finally
      //  reg.CloseKey;
      //end;

    finally
      reg.Free;
    end;

    SHChangeNotify (SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  end;

  function BeeGui_CheckRegisterFileType;
  var
    reg: TRegistry;
  begin      
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;
      reg.OpenKey (prefix + 'file\shell\open\command', True);
      try
        Result := (PosText (exepfad, reg.ReadString ('')) = 1);
      finally
        reg.CloseKey;
      end;
    finally
      reg.Free;
    end;
  end;

  procedure BeeGui_UnRegisterFileType;
  var
    reg: TRegistry;
  begin
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;
      if (reg.openKey (prefix + 'file\shell\open\command', False) = True) and (PosText (exepfad,reg.readstring ('')) = 1) then
      begin
        reg.CloseKey;
        reg.DeleteKey ('.' + prefix);
        reg.DeleteKey (prefix + 'file');
      end;
    finally
      reg.Free;
    end;
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  end;

  procedure FreePidl(pidl: PItemIDList);
  var
    allocator: IMalloc;
  begin
    if Succeeded (SHGetMalloc (allocator)) then
    begin
      allocator.Free (pidl);
      {$IFDEF VER100}
      allocator.Release;
      {$ENDIF}
    end;
  end;

  function BeeGui_GetWin_StartMenuDir;
  var
    pidl: PItemIDList;
    buf: array [0..MAX_PATH] of Char;
  begin
    if Succeeded (SHGetSpecialFolderLocation (0, CSIDL_STARTMENU , pidl)) then
      SHGetPathFromIDList (pidl, buf);

    Result := StrPas (buf);
    // The calling application is responsible for freeing the PItemIDList-pointer
    // with the Shell's IMalloc interface
    FreePIDL (pidl);
  end;

  function BeeGui_GetWin_StartMenuProgramsDir;
  var
    pidl: PItemIDList;
    buf: array[0..MAX_PATH] of char;
  begin
    if Succeeded (SHGetSpecialFolderLocation (0, CSIDL_PROGRAMS, pidl)) then
      SHGetPathFromIDList(pidl, buf);

    Result := StrPas(buf);
    // The calling application is responsible for freeing the PItemIDList-pointer
    // with the Shell's IMalloc interface
    FreePIDL (pidl);
  end;

  function BeeGui_GetWin_DesktopDir;
  var
    pidl: PItemIDList;
    buf: array[0..MAX_PATH] of char;
  begin
    if Succeeded (SHGetSpecialFolderLocation (0, CSIDL_DESKTOP, pidl)) then
      SHGetPathFromIDList(pidl, buf);

    Result := StrPas (buf);
    // The calling application is responsible for freeing the PItemIDList-pointer
    // with the Shell's IMalloc interface
    FreePIDL (pidl);
  end;

  function BeeGui_GetWin_AppDataDir;
  var
    pidl: PItemIDList;
    buf: array[0..MAX_PATH] of char;
  begin
    if Succeeded (SHGetSpecialFolderLocation (0, CSIDL_APPDATA, pidl)) then
      SHGetPathFromIDList(pidl, buf);

    Result := StrPas (buf);
    // The calling application is responsible for freeing the PItemIDList-pointer
    // with the Shell's IMalloc interface
    FreePIDL (pidl);
  end;

  function BeeGui_GetWin_UserTmpDir;
  var
    tmppath: pchar;
  begin
    getmem (tmppath, max_path);
    GetTempPath (max_path, tmppath);
    result := IncludeTrailingBackslash (string (tmppath));
    freemem (tmppath, max_path);
  end;

  function BeeGui_ShellFindExecutable;
  var
    Res: HINST;
    Buffer: array[0..MAX_PATH] of char;
    P: PChar;
  begin
    FillChar (Buffer, SizeOf (Buffer), #0);
    if DefaultDir = '' then
      P := nil
    else
      P := PChar (DefaultDir);

    Res := FindExecutable (PChar (FileName), P, Buffer);
    if Res > 32 then
    begin
      P := Buffer;
      while PWord (P)^ <> 0 do
      begin
        if P^ = #0 then  P^ := #32; /// FindExecutable replaces #32 with #0
         
        Inc (P);
      end;
      Result := Buffer;
    end else
      Result := '';
  end;

  function  BeeGui_DragQueryFile;
  begin
    Result := DragQueryFile (Drop, FileIndex, FileName, cb);
  end;

  procedure BeeGui_DragFinish;
  begin
    DragFinish (Drop);
  end;

  // file version routines

  function BeeGui_FileVersion;
  var
    InfoSize, Wnd: DWORD;
    VerBuf: pointer;
    FI: PVSFixedFileInfo;
    VerSize: DWORD;
  begin
    Result := '0.0.0.0';
    InfoSize := GetFileVersionInfoSize (PChar(FileName), Wnd);
    if InfoSize <> 0 then
    begin
      GetMem (VerBuf, InfoSize);
      try
        if GetFileVersionInfo (PChar (FileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue (VerBuf, '\', Pointer (FI), VerSize) then

            Result := IntToStr(FI.dwFileVersionMS div $10000) + '.' +
                      IntToStr(FI.dwFileVersionMS mod $10000) + '.' +
                      IntToStr(FI.dwFileVersionLS div $10000) + '.' +
                      IntToStr(FI.dwFileVersionLS mod $10000);
      finally
        FreeMem (VerBuf);
      end;
    end;
  end;

  function BeeGui_CompareVersion;
  var
    AMajor, AMinor, ARelease, ABuild: integer;
    BMajor, BMinor, BRelease, BBuild: integer;
  begin
    BeeGui_ExpandVersion (VerA, AMajor, AMinor, ARelease, ABuild);
    BeeGui_ExpandVersion (VerB, BMajor, BMinor, BRelease, BBuild);
    if AMajor = BMajor then
    begin
      if AMinor = BMinor then
      begin
        if ARelease = BRelease then
          Result := ABuild < BBuild
        else
          Result := ARelease < BRelease
      end
      else
        Result := AMinor < BMinor;
    end else
      Result := AMajor < BMajor;
  end;

  procedure BeeGui_ExpandVersion;

    function ExtractNumber (var Version: string): integer;
    var
      p: integer;
    begin
      p := Pos ('.', Version);
      if p > 0 then
      begin
        Result := StrToIntDef (Copy (Version, 1, p - 1), 0);
        Delete (Version, 1, p);
      end else
        Result := StrToIntDef (Version, 0);
    end;

  begin
    Major   := ExtractNumber (Version);
    Minor   := ExtractNumber (Version);
    Release := ExtractNumber (Version);
    Build   := ExtractNumber (Version);
  end;


end.
