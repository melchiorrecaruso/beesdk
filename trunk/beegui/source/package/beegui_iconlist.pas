{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

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

    TIconList class.

    Modifyed:

    xx.xx.xxxx
}

unit BeeGui_IconList;

{$I Compiler.inc}

interface

uses
  Classes,
  Controls,
  Graphics,
  SysUtils;

{ TFileInfo }

type
  TFileInfo = record
    FileIconIndex: integer;
    FileOpenIconIndex: integer;
    FileType: string;
  end;

  { TIconImageList }

  TIconList = class(TImageList)
  private
  public
    constructor Create(SmallIcon: boolean);
    destructor Destroy; override;
  public
    function GetFileInfo(FileExt: string; FileAttr: integer): TFileInfo;
  end;

implementation

uses
  Windows,
  ShellApi;

{ TIconImageList }

constructor TIconList.Create(SmallIcon: boolean);
var
  SysImageList: uint;
  SFI: TSHFileInfo;
begin
  inherited Create(nil);
  if SmallIcon then
  begin
    SysImageList := SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  end else
  begin
    SysImageList := SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  end;
  Handle := SysImageList;
  ShareImages := True;
end;

destructor TIconList.Destroy;
begin
  inherited Destroy;
end;

function TIconList.GetFileInfo(FileExt: string; FileAttr: integer): TFileInfo;
var
  SFI: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FileExt), FileAttr, SFI, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES);

  Result.FileIconIndex := SFI.iIcon;
  Result.FileType := SFI.szTypeName;

  SHGetFileInfo(PChar(FileExt), FileAttr, SFI, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES or SHGFI_OPENICON);

  Result.FileOpenIconIndex := SFI.iIcon;
end;

end.
