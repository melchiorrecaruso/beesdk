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
  public
    { public declarations }
    procedure SetFileName(const FileName: string);
    procedure SetOldFileTime(FileTime: integer);
    procedure SetOldFileSize(FileSize: integer);
    procedure SetNewFileTime(FileTime: integer);
    procedure SetNewFileSize(FileSize: integer);
  public
    { public declarations }
    procedure SaveProperty;
    procedure LoadProperty;
    procedure SaveLanguage;
    procedure LoadLanguage;
  private
    { private declarations }
  end;

var
  OverWriteFrm: TOverWriteFrm;

implementation

uses
  BeeGui_Consts,
  BeeGui_Messages,
  BeeGui_SysUtils;

  { TOverwriteFrm class }
  
  {$I beegui_overwritefrm_saveproperty.inc}
  {$I beegui_overwritefrm_loadproperty.inc}
  {$I beegui_overwritefrm_savelanguage.inc}
  {$I beegui_overwritefrm_loadlanguage.inc}

  procedure TOverwriteFrm.FormCreate(Sender: TObject);
  begin
    Images.IconFolder := ExtractFilePath(ParamStr(0)) + 'largeicons';
    // --- //
    LoadLanguage;
    LoadProperty;
  end;

  procedure TOverwriteFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    {$IFDEF SAVELANGUAGE}
    SaveLanguage;
    {$ENDIF}
    SaveProperty;
  end;
  
  procedure TOverwriteFrm.SetFileName(const FileName: string);
  begin
    TheFolder.Caption := Format(TheFolder.Caption, [FileName]);
    Images.GetBitmap(Images.FileIcon(FileName, 0), OldIcon.Picture.Bitmap);
    Images.GetBitmap(Images.FileIcon(FileName, 0), NewIcon.Picture.Bitmap);
  end;
  
  procedure TOverwriteFrm.SetOldFileTime(FileTime: integer);
  begin
    OldTime.Caption := Format(OldTime.Caption, [DateTimeToStr(FileDateToDateTime(FileTime))]);
  end;
  
  procedure TOverwriteFrm.SetOldFileSize(FileSize: integer);
  begin
    OldSize.Caption := Format(OldSize.Caption, [SizeToStr(FileSize)]);
  end;
  
  procedure TOverwriteFrm.SetNewFileTime(FileTime: integer);
  begin
    NewTime.Caption := Format(NewTime.Caption, [DateTimeToStr(FileDateToDateTime(FileTime))]);
  end;

  procedure TOverwriteFrm.SetNewFileSize(FileSize: integer);
  begin
    NewSize.Caption := Format(NewSize.Caption, [SizeToStr(FileSize)]);
  end;
    
initialization

  {$I beegui_overwritefrm.lrs}

end.
