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

    BeeGui OverWrite form.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;
  
    v1.0.3 build 0360 - 2006.12.28 by Melchiorre Caruso.
}

unit BeeGui_OverWriteFrm;

interface

uses
  Forms,
  Buttons,
  Dialogs,
  Classes,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  LResources,
  XMLPropStorage;

type

  { TOverwriteFrm }

  TOverwriteFrm = class(TForm)
    OverwriteFrm_Storage: TXMLPropStorage;
    OverWriteFrm_OldImage: TImage;
    OverWriteFrm_NewImage: TImage;
    OverWriteFrm_OldDate: TLabel;
    OverWriteFrm_NewDate: TLabel;
    OverWriteFrm_OldSize: TLabel;
    OverWriteFrm_NewSize: TLabel;
    OverWriteFrm_Would: TLabel;
    OverWriteFrm_Image: TImage;
    OverWriteFrm_With: TLabel;
    OverWriteFrm_The: TLabel;
    BtnYesAll: TBitBtn;
    BtnAbort: TBitBtn;
    BtnNoAll: TBitBtn;
    BtnYes: TBitBtn;
    BtnNo: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

implementation

uses
  BeeGui_SysUtils;

  { TOverwriteFrm class }

  procedure TOverwriteFrm.FormCreate(Sender: TObject);
  var
   CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      OverwriteFrm_Storage.FileName := CfgFolder + ('overwritefrm.xml');
    end;
    {$I beegui_overwritefrm.inc}
    OverwriteFrm_Storage.Restore;
  end;
  
  procedure TOverwriteFrm.FormShow(Sender: TObject);
  begin
    Width := OverWriteFrm_The.Left + OverWriteFrm_The.Width + 20;
  end;
    
initialization

  {$I beegui_overwritefrm.lrs}

end.