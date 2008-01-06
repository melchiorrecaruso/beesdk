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

    TIconList class.

  Modifyed:
    
}

unit BeeCore_IconList;

interface

uses
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
    procedure SetIconFolder(Value: string);
  public
    function FileIcon(const FileExt: string): integer;
    function FileType(const  FileExt: string): string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property IconFolder: string read FIconFolder write SetIconFolder;
  end;
  
  { Register }
  
  procedure Register;

implementation

  { TIconImageList }
  
  constructor TIconList.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FExtentions := TStringList.Create;
  end;

  destructor TIconList.Destroy;
  begin
    FExtentions.Free;
    inherited Destroy;
  end;
  
  function TIconList.FileIcon(const FileExt: string): integer;
  begin
    Result := FExtentions.IndexOf(FileExt);
    if Result = -1 then
    begin
      Result := FExtentions.IndexOf('unknow');
    end;
  end;
  
  function TIconList.FileType(const FileExt: string): string;
  begin
    Result := 'File ' + FileExt;
  end;
  
  procedure TIconList.SetIconFolder(Value: string);
  var
    Error: integer;
    Image: TBitmap;
    Rec: TSearchRec;
  begin
    if DirectoryExists(Value) then
    begin
      FIconFolder := Value;
      FExtentions.Clear; Clear;
      
      Error := FindFirst(IncludeTrailingBackSlash(FIconFolder) + '*.bmp', faAnyFile, Rec);
      while (Error = 0) and ((Rec.Attr and faDirectory) = 0) do
      begin
        Image := TBitmap.Create;
        try
          Image.LoadFromFile(IncludeTrailingBackSlash(FIconFolder) + Rec.Name);
          FExtentions.Add(Copy(Rec.Name, 1, Length(Rec.Name) - Length('.bmp')));
          Add(Image, nil);
        finally
          Image.Free;
        end;
        Error := FindNext(Rec);
      end;
      FindClose(Rec);
    end;
  end;

  { Register }

  procedure Register;
  begin
    RegisterComponents('BeeCore', [TIconList]);
  end;
  
initialization

  {$I beecore_iconlist.lrs} 

end.

