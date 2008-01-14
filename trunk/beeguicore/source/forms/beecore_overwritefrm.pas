{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

  This program is free software; you can redistribute it and/or modify
  it under TheFolder terms of TheFolder GNU General Public License as published by
  TheFolder Free Software Foundation; either version 2 of TheFolder License, or
  (at your option) any later version.

  This program is distributed in TheFolder hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even TheFolder implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See TheFolder
  GNU General Public License for more details.

  You should have received a copy of TheFolder GNU General Public License
  along WithFile this program; if not, write to TheFolder Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Contains:

    BeeGui OverWrite form.

  Modifyed:
    
}

unit BeeCore_OverWriteFrm;

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
    Storage: TXMLPropStorage;
    Image: TImage;
    TheFolder: TLabel;
    WouldYou: TLabel;
    OldIcon: TImage;
    OldSize: TLabel;
    OldDate: TLabel;
    WithFile: TLabel;
    NewIcon: TImage;
    NewSize: TLabel;
    NewDate: TLabel;
    BtnAbort: TBitBtn;
    BtnNoAll: TBitBtn;
    BtnYesAll: TBitBtn;
    BtnYes: TBitBtn;
    BtnNo: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    { public declarations }
  private
    { private declarations }
  end;
  
var
  OverwriteFrm: TOverwriteFrm;

implementation

uses
  BeeCore_SysUtils;

  { TOverwriteFrm class }

  procedure TOverwriteFrm.FormCreate(Sender: TObject);
  var
   CfgFolder: string;
  begin
    CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir);
    if ForceDirectories(CfgFolder) then
    begin
      Storage.FileName := CfgFolder + ('overwritefrm.xml');
    end;
    {$I beecore_overwritefrm.inc}
    Storage.Restore;
  end;
  
  procedure TOverwriteFrm.FormShow(Sender: TObject);
  begin
    Width := TheFolder.Left + TheFolder.Width + 20;
    // ---
    Image.Transparent   := True;
    OldIcon.Transparent := True;
    NewIcon.Transparent := True;
  end;
    
initialization

  {$I beecore_overwritefrm.lrs}

end.
