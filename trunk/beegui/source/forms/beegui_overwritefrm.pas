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

unit BeeGui_OverWriteFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Buttons,
  Dialogs,
  Classes,
  IniFiles,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  LResources,
  // ---
  BeeGui_IconList;

type

  { TOverwriteFrm }

  TOverwriteFrm = class(TForm)
    Bevel: TBevel;
    Images: TIconList;
    Image: TImage;
    TheFolder: TLabel;
    WouldYou: TLabel;
    OldIcon: TImage;
    OldSize: TLabel;
    OldTime: TLabel;
    WithFile: TLabel;
    NewIcon: TImage;
    NewSize: TLabel;
    NewTime: TLabel;
    BtnAbort: TBitBtn;
    BtnNoAll: TBitBtn;
    BtnYesAll: TBitBtn;
    BtnYes: TBitBtn;
    BtnNo: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    { public declarations }
    procedure SetFileName(const FileName: string);
    procedure SetOldFileTime(FileTime: integer);
    procedure SetOldFileSize(FileSize: integer);
    procedure SetNewFileTime(FileTime: integer);
    procedure SetNewFileSize(FileSize: integer);
  private
    { private declarations }
  end;

implementation

uses
  BeeGui_SysUtils;

  { TOverwriteFrm class }

  procedure TOverwriteFrm.FormCreate(Sender: TObject);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Images.IconFolder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('largeicons') ;
    // ---
    {$I beegui_overwritefrm_loadlanguage.inc}
    {$I beegui_overwritefrm_loadproperty.inc}
  end;

  procedure TOverwriteFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    {*$I beegui_overwritefrm_savelanguage.inc}
    {$I beegui_overwritefrm_saveproperty.inc}
  end;
  
  procedure TOverwriteFrm.FormShow(Sender: TObject);
  begin
    // Fix Laz Bug
    Image.Transparent   := True;
    OldIcon.Transparent := True;
    NewIcon.Transparent := True;
    // Fix Laz bug
    if Constraints.MaxWidth = 0 then
    begin
      Constraints.MaxWidth  := Width;
      Constraints.MaxHeight := Height;
      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;
    end;
  end;
  
  procedure TOverwriteFrm.SetFileName(const FileName: string);
  begin
    TheFolder.Caption := TheFolder.Caption + ' "' + FileName + '".';
    // ---
    Images.GetBitmap(Images.FileIcon(FileName, 0), OldIcon.Picture.Bitmap);
    Images.GetBitmap(Images.FileIcon(FileName, 0), NewIcon.Picture.Bitmap);
  end;
  
  procedure TOverwriteFrm.SetOldFileTime(FileTime: integer);
  begin
    OldTime.Caption := OldTime.Caption + '  ' + DateTimeToStr(FileDateToDateTime(FileTime));
  end;
  
  procedure TOverwriteFrm.SetOldFileSize(FileSize: integer);
  begin
    OldSize.Caption := OldSize.Caption + '  ' + SizeToStr(FileSize);
  end;
  
  procedure TOverwriteFrm.SetNewFileTime(FileTime: integer);
  begin
    NewTime.Caption := NewTime.Caption + '  ' + DateTimeToStr(FileDateToDateTime(FileTime));
  end;

  procedure TOverwriteFrm.SetNewFileSize(FileSize: integer);
  begin
    NewSize.Caption := NewSize.Caption + '  ' + SizeToStr(FileSize);
  end;
    
initialization

  {$I beegui_overwritefrm.lrs}

end.
