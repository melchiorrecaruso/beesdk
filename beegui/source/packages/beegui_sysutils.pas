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

    Various helper system routines.

  Modifyed:
}

unit BeeGui_SysUtils;

interface

uses
  {$IFDEF MSWINDOWS}
  Math,
  Windows,
  Registry,
  {$ENDIF}
  Dialogs,
  Classes,
  Process,
  SysUtils;
  
  { directories routines }
  
  {$IFDEF MSWINDOWS}
  function DiskInDrive (Drive: char): boolean;
  {$ENDIF}

  procedure GetDrivers(var Drives: TStringList);
  function GetDirectories(const PathName: string; var DirList: TStringList): boolean;
  
  procedure DeleteDirectory(const DirName: string);
  procedure ClearDirectory(const DirName: string);
  
  function GetApplicationCheckOutDir(const aApplicationName: string): string;
  function GetApplicationConfigDir(const aApplicationName: string): string;
  function GetApplicationTempDir(const aApplicationName: string): string;
  // ---
  function ExtractFirstFolder(const FullPath: string): string;

  { string routines }

  function AnsiCompareText (const Str1, Str2: string): integer;
  function AnsiPosText(const Substr, S: string): integer; inline;
  function AnsiIncludeTrailingBackSlash(const DirName: string): string;
  
  function SizeToStr(Size: integer): string;
  function RatioToStr(Ratio: integer): string;
  function AttrToStr(Attr: integer): string;
  
  { shell routines }
  
  function ShellExec(const FileName: string; ExecName: string): boolean;
  
  { files routines }
  
  function  SizeOfFile(const FileName: string): integer;
  
  function CopyFile(const FileName, NewFileName: string): boolean;
  function CopyFiles(SrcDir, DstDir: string): boolean;
  
  { register file type routines }
  
  procedure RegisterFileType (const prefix: string; const exepfad: string);
  procedure UnRegisterFileType (const prefix: string; const exepfad: string);
  function  CheckRegisterFileType (const prefix: string; const exepfad: string): boolean;
  
implementation

  const DoublePathDelim = PathDelim + PathDelim;

  { directories routines }

  {$IFDEF MSWINDOWS}
  function DiskInDrive (Drive: char): boolean;
  var
    ErrorMode: word;
  begin
    Result := False;
    if Drive in ['a'..'z'] then Dec (Drive, $20);
    if Drive in ['A'..'Z'] then
    begin
      ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
      try
        if DiskSize(Ord(Drive) - $40) <> -1 then
        begin
          Result := True;
        end;
      finally
        SetErrorMode(ErrorMode);
      end;
    end;
  end;
  {$ENDIF}

  procedure GetDrivers(var Drives: TStringList);
  {$IFDEF MSWINDOWS}
  var
    i, PX: Integer;
    TPC: PChar;
    TMR, DM: string;
  {$ENDIF}
  begin
    if Assigned(Drives) then
    begin
      Drives.Clear;
      {$IFDEF MSWINDOWS}
      DM := '';
      GetMem(TPC, 100);
      PX := GetLogicalDriveStrings(100, TPC);
      for i := - 1 to PX do
      begin
        if (TPC + i)[1] <> #0 then
          DM := DM + (TPC + i)[1];
      end;
      FreeMem(TPC);
      TMR := '';
      for i := 1 to Length(DM) do
      begin
        TMR := TMR + DM[i];
        if (i mod 3) = 0 then
        begin
          Drives.Add(TMR);
          TMR := '';
        end;
      end;
      {$ELSE}
      Drives.Add(PathDelim);
      {$ENDIF}
    end;
  end;
  
  function GetDirectories(const PathName: string; var DirList: TStringList): boolean;
  var
    Info: TSearchRec;
    error: Integer;
  begin
    Result := False;
    if Assigned(DirList) then
    begin
      DirList.Clear;
      {$IFDEF MSWINDOWS}
      if Length(PathName) = 0 then Exit;
      if not DiskInDrive(PathName[1]) then Exit;
      {$ENDIF}
      if not SetCurrentDir(PathName) then Exit;
      error := SysUtils.FindFirst ('*' ,faAnyFile, Info);
      while error = 0 do
      begin
        if ((Info.Attr and faHidden) = 0) and (Info.Name <> '..') and (Info.Name <> '.') and
           ((Info.Attr and faDirectory) = faDirectory)  then
        begin
          DirList.Add(Info.Name);
        end;
        error := SysUtils.FindNext(Info);
      end;
      SysUtils.FindClose(Info);
      Result := True;
    end;
  end;
  
  function GetApplicationCheckOutDir(const aApplicationName: string): string;
  begin
    Result := IncludeTrailingBackSlash(GetUserDir) + '.' +
      IncludeTrailingBackSlash(LowerCase(aApplicationName)) + ('checkout');
      
    if DirectoryExists(Result) = False then
      ForceDirectories(Result);
  end;
  
  function GetApplicationConfigDir(const aApplicationName: string): string;
  begin
    Result := IncludeTrailingBackSlash(GetUserDir) + '.' +
      IncludeTrailingBackSlash(LowerCase(aApplicationName)) + ('configuration');

    if DirectoryExists(Result) = False then
      ForceDirectories(Result);
  end;
  
  function GetApplicationTempDir(const aApplicationName: string): string;
  begin
    Result := IncludeTrailingBackSlash(GetUserDir) + '.' +
      IncludeTrailingBackSlash(LowerCase(aApplicationName)) + ('temp');

    if DirectoryExists(Result) = False then
      ForceDirectories(Result);
  end;
  
  procedure ClearDirectoryScan(const DirName: String);
  var
    T: TSearchRec;
  begin
    if FindFirst(IncludeTrailingBackslash(DirName) + '*', faAnyFile, T) = 0 then
    begin
      repeat
        if (T.Name <> '.') and (T.Name <> '..') and ((T.Attr and faDirectory) = 0) then
        begin
          DeleteFile(IncludeTrailingBackslash(DirName) + T.Name);
        end;
      until FindNext(T) <> 0;
    end;
    FindClose(T);
    // ---
    if FindFirst(IncludeTrailingBackslash(DirName) + '*', faAnyFile, T) = 0 then
    begin
      repeat
        if (T.Name <> '.') and (T.Name <> '..') and ((T.Attr and faDirectory) = faDirectory) then
        begin
          ClearDirectoryScan(IncludeTrailingBackslash(DirName) + T.Name);
        end;
      until FindNext(T) <> 0;
    end;
    FindClose(T);
    // ---
    RemoveDir(DirName);
  end;
  
  procedure DeleteDirectory(const DirName: string);
  begin
    if not SetCurrentDir(DirName) then Exit;
    if not SetCurrentDir(ExtractFilePath(DirName)) then Exit;
    ClearDirectoryScan(DirName);
    RemoveDir(DirName);
  end;

  procedure ClearDirectory(const DirName: string);
  begin
    if not SetCurrentDir(DirName) then Exit;
    if not SetCurrentDir(ExtractFilePath(DirName)) then Exit;

    ClearDirectoryScan(DirName);
  end;
  
  function ExtractFirstFolder(const FullPath: string): string;
  begin
    Result := FullPath;
    if Pos(PathDelim, Result) > -1 then SetLength(Result, Pos(PathDelim, Result) -1);
  end;

  { string routines }
  
  function AnsiCompareText(const Str1, Str2: string): integer; inline;
  begin
    {$IFDEF MSWINDOWS}
    Result := CompareText(Str1, Str2);
    {$ELSE}
    Result := CompareStr(Str1, Str2);
    {$ENDIF}
  end;
  
  function AnsiPosText(const Substr, S: string): integer; inline;
  begin
    {$IFDEF MSWINDOWS}
    Result := Pos(LowerCase(Substr), LowerCase(S));
    {$ELSE}
    Result := Pos(Substr, S);
    {$ENDIF}
  end;
  
  function AnsiIncludeTrailingBackSlash(const DirName: string): string;
  var i: integer;
  begin
    if DirName = '' then
    begin
      Result := DirName
    end else
    begin
      Result := IncludeTrailingBackSlash(DirName);
      i := Pos(DoublePathDelim, Result);
      while  i > 0 do
      begin
        delete(Result, i, 1);
        i := Pos(DoublePathDelim, Result);
      end;
    end;
  end;
  
  function SizeToStr(Size: integer): string;
  const
    KB_SIZE = 1024;
    MB_SIZE = KB_SIZE * 1024;
    GB_SIZE = MB_SIZE * 1024;
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

  function RatioToStr(Ratio: integer): string;
  begin
    Result := Format('%u%%', [Ratio])
  end;

  function AttrToStr(Attr: integer): string;
  begin
    Result := '.DRHSA';
    if Attr and faDirectory = 0 then Result[2] := '.';
    if Attr and faReadOnly  = 0 then Result[3] := '.';
    if Attr and faHidden    = 0 then Result[4] := '.';
    if Attr and faSysFile   = 0 then Result[5] := '.';
    if Attr and faArchive   = 0 then Result[6] := '.';
  end;
  
  { shell routines }
  
  function ShellExec(const FileName: string; ExecName: string): boolean;
  var
    aProcess: TProcess;
  begin
    ExecName := ExpandFileName(ExecName);
    if FileExists(ExecName) = False then
    begin
      {$IFDEF MSWINDOWS}
        Result := ShellExecute(0, 'open', PChar(FileName), nil, nil, SW_SHOW) > 32
      {$ELSE}
        Result := False;
      {$ENDIF}
    end else
    begin
      aProcess := TProcess.Create(nil);
      aProcess.CommandLine := Format(ExecName + ' %s', [FileName]);
      aProcess.Execute;
      if aProcess.ExitStatus = 0 then
        Result := True
      else
        Result := False;
      aProcess.Free;
    end;
  end;
  
  {  }
  
  function CopyFile(const FileName, NewFileName: string): boolean;
  var
    Src: TFileStream;
    Dst: TFileStream;
  begin
    Result := False;
    if FileExists(NewFileName) then Exit;
    if not FileExists(FileName)then Exit;
    {$IFDEF MSWINDOWS}
    Result := Windows.CopyFile(PChar(FileName), PChar(NewFileName), True);
    {$ELSE}
    try
      Src := TFileStream.Create(FileName, fmOpenRead);
    except
      Src := nil;
    end;
    try
      Dst := TFileStream.Create(NewFileName, fmCreate);
    except
      Dst := nil;
    end;
    if Assigned(Src) and Assigned(Dst) then
    begin
      try
        Result := (Dst.CopyFrom(Src, Src.Size) = Src.Size);
      finally
        Dst.Free;
        Src.Free;
      end;
    end;
    {$ENDIF}
    end;
    
    function CopyFiles(SrcDir, DstDir: string): boolean;
    var
      T: TSearchRec;
      Error: integer;
    begin
      Result := DirectoryExists(ExcludeTrailingBackSlash(SrcDir));
      if Result then Result := ForceDirectories(ExcludeTrailingBackSlash(DstDir));
      if Result then
      begin
        SrcDir := IncludeTrailingBackSlash(SrcDir);
        DstDir := IncludeTrailingBackSlash(DstDir);

        Error := SysUtils.FindFirst(SrcDir + '*', faAnyFile, T);
        while Error = 0 do
        begin
          if (T.Name <> '.') and (T.Name <> '..') then
          begin
            if (T.Attr and faDirectory = 0) then
              Result := CopyFile(SrcDir + T.Name, DstDir + T.Name)
            else
              Result := CopyFiles(SrcDir + T.Name, DstDir + T.Name);
          end;
          if not Result then Exit;
          Error := SysUtils.FindNext(T);
        end;
        SysUtils.FindClose(T);
      end;
    end;
    
    function SizeOfFile(const FileName: string): integer;
    var
      F: TSearchRec;
    begin
      if FindFirst(FileName, faAnyFile - faDirectory, F) = 0 then
        Result := F.Size
      else
        Result := -1;
      FindClose(F);
    end;
    
    { register file type routines }
    
    procedure RegisterFileType (const prefix: string; const exepfad: string);
    {$IFDEF MSWINDOWS}
    var
      reg: TRegistry;
    {$ENDIF}
    begin
      {$IFDEF MSWINDOWS}
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
          reg.Writestring('', exepfad + ',1');
        finally
          reg.CloseKey;
        end;
        reg.OpenKey (prefix + 'file\shell\open\command', True);
        try
          reg.Writestring ('', exepfad+' "l" "%1" "*\*"');
        finally
          reg.CloseKey;
        end;
        reg.CreateKey (prefix + 'file\shell\BeeGui');
        reg.OpenKey (prefix + 'file\shell\BeeGui\Extract all...\command', True);
        try
          reg.Writestring('', exepfad+' "x" "%1" "*\*"');
        finally
          reg.CloseKey;
        end;
      finally
        reg.Free;
      end;
      {$ENDIF}
    end;
    
    function CheckRegisterFileType (const prefix: string; const exepfad: string): boolean;
    {$IFDEF MSWINDOWS}
    var
      reg: TRegistry;
    {$ENDIF}
    begin
      {$IFDEF MSWINDOWS}
      reg := TRegistry.Create;
      try
        reg.RootKey := HKEY_CLASSES_ROOT;
        reg.OpenKey (prefix + 'file\shell\open\command', True);
        try
          Result := (AnsiPosText (exepfad, reg.ReadString ('')) = 1);
        finally
          reg.CloseKey;
        end;
      finally
        reg.Free;
      end;
      {$ENDIF}
    end;

    procedure UnRegisterFileType (const prefix: string; const exepfad: string);
    {$IFDEF MSWINDOWS}
    var
      reg: TRegistry;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      procedure DeleteRegKey (reg: TRegistry; const key: string);
      begin
        if reg.OpenKey (key, False) then
        begin
          if reg.ValueExists ('') then
             reg.DeleteValue ('');
          reg.CloseKey;
          reg.DeleteKey(key);
        end;
      end;
    {$ENDIF}
    begin
      {$IFDEF MSWINDOWS}
      if CheckRegisterFileType (prefix, exepfad) then
      begin
        reg := TRegistry.Create;
        try
          reg.RootKey := HKEY_CLASSES_ROOT;
          DeleteRegKey(reg, prefix + 'file\shell\BeeGui\Extract all...\command');
          DeleteRegKey(reg, prefix + 'file\shell\BeeGui\Extract all...');
          DeleteRegKey(reg, prefix + 'file\shell\BeeGui');
          DeleteRegKey(reg, prefix + 'file\shell\open\command');
          DeleteRegKey(reg, prefix + 'file\shell\open');
          DeleteRegKey(reg, prefix + 'file\shell');
          DeleteRegKey(reg, prefix + 'file\DefaultIcon');
          DeleteRegKey(reg, prefix + 'file');
          DeleteRegKey(reg, '.' + prefix);
        finally
          reg.Free;
        end;
      end;
     {$ENDIF}
    end;

end.

