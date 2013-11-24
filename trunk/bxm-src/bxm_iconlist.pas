{
  Copyright (c) 2013 Melchiorre Caruso

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

    TIconList class.

  Modifyed:
    
}

unit bxm_IconList;

{$I bxm_compiler.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  ShellApi,
  {$ENDIF}
  Classes,
  Dialogs,
  Controls,
  Graphics,
  SysUtils,
  LResources;

type

  { TIconImageList }

  TIconList = class(TImageList)
  private
    FIconFolder: string;
    FExtentions: TStringList;
    FTypes:      TStringList;
    procedure SetIconFolder(Value: string);
    function GetSysFileIcon(const FileName: string; FileAttr: integer): integer;
    function GetFileIcon(const FileName: string; FileAttr: integer): integer;
  public
    function FileIcon(const FileName: string; FileAttr: integer): integer;
    function FileType(const FileName: string; FileAttr: integer): string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAll;
  published
    property IconFolder: string Read FIconFolder Write SetIconFolder;
  end;

{ Register }

procedure Register;

implementation

uses
  bx_Common;

constructor TIconList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtentions := TStringList.Create;
  FTypes      := TStringList.Create;
  FIconFolder := '';
end;

destructor TIconList.Destroy;
begin
  FTypes.Free;
  FExtentions.Free;
  inherited Destroy;
end;

procedure TIconList.ClearAll;
begin
  FExtentions.Clear;
  FTypes.Clear;
  Clear;
end;

function TIconList.FileIcon(const FileName: string; FileAttr: integer): integer;
var
  FileExt: string;
begin
  FileExt := ExtractFileExt(FileName);
  while Pos('.', FileExt) = 1 do
  begin
    System.Delete(FileExt, 1, 1);
  end;

  Result := FExtentions.IndexOf(FileExt);
  if Result = -1 then
  begin
    { TODO -oTIconList -cDebug : Disabilitare test per local icons }
    // Result := GetSysFileIcon(FileName, FileAttr);
    if Result = -1 then
    begin
      Result := GetFileIcon(FileName, FileAttr);
      if Result = -1 then
      begin
        Result := FExtentions.IndexOf('@unknow');
        if Result = -1 then
          Result := GetFileIcon('.@unknow', FileAttr);
      end;
    end;
  end;
end;

function TIconList.FileType(const FileName: string; FileAttr: integer): string;
var
  I: integer;
begin
  I := FileIcon(FileName, FileAttr);
  if I > -1 then
    Result := FTypes.Strings[I]
  else
    Result := '';
end;

procedure TIconList.SetIconFolder(Value: string);
begin
  if DirectoryExists(Value) then
  begin
    ClearAll;
    FIconFolder := IncludeTrailingBackSlash(Value);
  end;
end;

function TIconList.GetFileIcon(const FileName: string; FileAttr: integer): integer;
var
  Bmp:     TBitmap;
  Picture: TPicture;
  Error:   integer;
  FileExt: string;
  Rec:     TSearchRec;
begin
  Result := -1;
  if FIconFolder <> '' then
  begin
    FileExt := ExtractFileExt(FileName);
    while Pos('.', FileExt) = 1 do
    begin
      System.Delete(FileExt, 1, 1);
    end;

    Error := FindFirst(FIconFolder + FileExt + '.bmp', faAnyFile, Rec);
    if (Error = 0) and ((Rec.Attr and faDirectory) = 0) then
    begin
      Bmp     := nil;
      Picture := TPicture.Create;
      try
        Picture.LoadFromFile(FIconFolder + Rec.Name);

        Bmp := TBitmap.Create;
        Bmp.Assign(Picture.Graphic);
      finally
        Picture.Free;
      end;

      if Assigned(Bmp) then
      begin
        Result := Add(Bmp, nil);
        FreeAndNil(Bmp);
      end;

      FExtentions.Add(FileExt);
      if (Pos('@', FileExt) = 0) then
        FTypes.Add('File .' + FileExt)
      else
        FTypes.Add('');
    end;
    FindClose(Rec);
  end;
end;

function TIconList.GetSysFileIcon(const FileName: string; FileAttr: integer): integer;
  {$IFDEF MSWINDOWS}
var
  I:   cardinal;
  Bmp: TBitmap;
  BmpMask: TBitmap;
  IconInfo: TIconInfo;
  FI:  TSHFileInfo;
  FileExt: string;
  {$ENDIF}
begin
  Result := -1;
    {$IFDEF MSWINDOWS}
  case Height of
    16: I := SHGFI_ICON or SHGFI_SMALLICON or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES;
    32: I := SHGFI_ICON or SHGFI_LARGEICON or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES;
    else
      I := 0;
  end;

  if CompareFileName('.@folderopen', FileName) = 0 then
  begin
    I := I or SHGFI_OPENICON;
  end;

  if SHGetFileInfo(PChar(FileName), FileAttr, FI, SizeOf(FI), I) <> 0 then
  begin
    Bmp := TBitmap.Create;
    BmpMask := TBitmap.Create;
    try
      if (FI.hIcon <> 0) and GetIconInfo(FI.hIcon, IconInfo) then
      begin
        Bmp.Handle := IconInfo.hbmColor;
        BmpMask.Handle := IconInfo.hbmMask;
      end;
      Result := Add(Bmp, BmpMask);
    finally
      BmpMask.Free;
      Bmp.Free;
    end;

    FileExt := ExtractFileExt(FileName);
    while Pos('.', FileExt) = 1 do
    begin
      System.Delete(FileExt, 1, 1);
    end;
    FExtentions.Add(FileExt);
    FTypes.Add(FI.szTypeName);
  end;
    {$ENDIF}
end;

{ Register }

procedure Register;
begin
  RegisterComponents('BX Manager', [TIconList]);
end;

initialization

  {$I bxm_iconlist.lrs}

end.