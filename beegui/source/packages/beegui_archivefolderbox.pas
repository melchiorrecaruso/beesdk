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

    TArchiveFolderBox class.

  Modifyed:

}

unit BeeGui_ArchiveFolderBox;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  StdCtrls,
  Graphics;
  
type

  { TArchiveFolderBox }

  TArchiveFolderBox = class(TComboBox)
  protected
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
 { Register }

  procedure Register;

implementation

  constructor TArchiveFolderBox.Create(AOwner: Tcomponent);
  begin
    inherited Create(AOwner);
  end;
  
  destructor TArchiveFolderBox.Destroy;
  var
    I: integer;
  begin
    for I := Items.Count -1 downto 0 do
      if Assigned(Items.Objects[I]) then
      begin
        TBitmap(Items.Objects[I]).FreeImage;
      end;

    inherited Destroy;
  end;
  
  procedure TArchiveFolderBox.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
  var
    Bitmap: TBitmap;
    Offset: Integer;
  begin
    inherited DrawItem(Index, ARect, State);
    Offset := 1;
    with Canvas do
    begin
      FillRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      Bitmap := TBitmap(Items.Objects[Index]);
      if Assigned(Bitmap) then
      begin
        Canvas.Draw(ARect.Left + OffSet, ARect.Top, Bitmap);
        Offset := Bitmap.Width + 3;
      end;
    end;
    Canvas.TextOut(ARect.Left + OffSet, ARect.Top, Items[Index]);
  end;
  
  { Register }

  procedure Register;
  begin
    RegisterComponents('BeePackage', [TArchiveFolderBox]);
  end;
  
end.

