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

    v1.0.3 build 0020 - 2006/11/25 by Melchiorre Caruso.
}

unit BeeGui_IconList;

interface

uses
  Classes,
  Controls,
  Graphics,
  SysUtils,
  LResources;

type
  { TIconImageList }
  
  TIconList = class(TImageList)
  private
    FExt: TStringList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(IconFolder: string);
  public
    function GetIconIndex(FileExt: string): integer;
  end;
  
  { Register }
  
  procedure Register;

implementation

  { TIconImageList }
  
  constructor TIconList.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    FExt:= TStringList.Create;
  end;
  
  destructor TIconList.Destroy;
  begin
    FExt.Free;
    inherited Destroy;
  end;
  
  function TIconList.GetIconIndex(FileExt: string): integer;
  begin
    while Pos('.', FileExt) = 1 do
    begin
      System.Delete(FileExt, 1, 1);
    end;
    Result := FExt.IndexOf(FileExt);
    if Result = -1 then
    begin
      Result := FExt.IndexOf('unknow');
    end;
  end;
  
  procedure TIconList.Initialize(IconFolder: string);
  var
    err: integer;
    Image: TBitmap;
    CurrExt: string;
    REC: TSearchRec;
  begin
    Clear;
    FExt.Clear;
    if DirectoryExists(IconFolder) then
    begin
      IconFolder := IncludeTrailingBackSlash(IconFolder);
      err := FindFirst(IconFolder + '*.bmp', faAnyFile, REC);
      while err = 0 do
      begin
        Image := TBitmap.Create;
        try
          Image.LoadFromFile(IconFolder + Rec.Name);
          CurrExt := REC.Name;
          SetLength(CurrExt, Length(CurrExt) - Length(ExtractFileExt(REC.Name)));
          FExt.Add(CurrExt);
          Add(Image, nil);
        finally
          Image.Free;
        end;
        err := FindNext(REC);
      end;
      FindClose(REC);
    end;
  end;

  { Register }

  procedure Register;
  begin
    RegisterComponents('BeeGui', [TIconList]);
  end;
  
initialization

  {$I beegui_iconlist.lrs} 

end.

