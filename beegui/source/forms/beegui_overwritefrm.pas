{
    Copyright (c) 2005-2007 Andrew Filinsky and Melchiorre Caruso

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

unit BeeGui_OverWriteFrm;

{$I Compiler.inc}

interface

uses
  Forms,
  Buttons,
  Dialogs,
  Classes,
  Messages,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  // ---
  BeeGui_IconList,
  BeeGui_FormStorage;

type
  TOverWriteFrm = class(TForm)
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
    BtnNo:  TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Update(fName: string; fSize: integer; fTime: integer);
  private
    { Private declarations }
    FFormStorage: TFormStorage;
    FIconList: TIconList;
  public
    { Public declarations }
  end;

var
  OverWriteFrm: TOverWriteFrm;

implementation

{$R *.dfm}

uses
  BeeGui_OverWriteFrmRes,
  BeeGui_SysUtils;

// TOverwriteFrm class

procedure TOverWriteFrm.FormCreate(Sender: TObject);
begin
  // Create FormStorage Component
  FFormStorage := TFormStorage.Create(Self);
  // Load Setting
  FFormStorage.Properties := _SOverwriteFrm_PropertyFull;
  FFormStorage.Load(GetApplicationSettingFileName);
  {IFDEF DEBUG}
  // Save Language
  FFormStorage.Properties := _SOverwriteFrm_Language;
  FFormStorage.Save(GetApplicationLanguageFileName);
  {ENDIF}
  FIconList := TIconList.Create(False);
end;

procedure TOverWriteFrm.FormDestroy(Sender: TObject);
begin
  // Save Setting
  if WindowState = wsNormal then
    FFormStorage.Properties := _SOverwriteFrm_PropertyFull
  else
    FFormStorage.Properties := _SOverwriteFrm_Property;
  FFormStorage.Save(GetApplicationSettingFileName);
  FFormStorage.Free;
  // ---
  FIConList.Free;
end;

procedure TOverWriteFrm.Update(fName: string; fSize: integer; fTime: integer);
var
  FileInfo: TFileInfo;
begin
  // Load Language
  FFormStorage.Properties := _SOverwriteFrm_Language;
  FFormStorage.Load(GetApplicationLanguageFileName);
  try
    OverWriteFrm_The.Caption :=
      OverWriteFrm_The.Caption + ' "' + ExtractFileName(fName) + '".';

    OverWriteFrm_OldSize.Caption :=
      OverWriteFrm_OldSize.Caption + ' ' + SizeToStr(SizeOfFile(fName));

    OverWriteFrm_OldDate.Caption :=
      OverWriteFrm_OldDate.Caption + ': ' +
      DateTimeToStr(FileDateToDateTime(FileAge(fName)));

    OverWriteFrm_NewSize.Caption :=
      OverWriteFrm_NewSize.Caption + ' ' + SizeToStr(fSize);

    OverWriteFrm_NewDate.Caption :=
      OverWriteFrm_NewDate.Caption + ': ' + DateTimeToStr(FileDateToDateTime(fTime));

    FileInfo := FIconList.GetFileInfo(ExtractFileExt(fName), 0);
    FIconList.GetBitmap(FileInfo.FileIconIndex, OverWriteFrm_NewImage.Picture.Bitmap);
    FIconList.GetBitmap(FileInfo.FileIconIndex, OverWriteFrm_OldImage.Picture.Bitmap);

  finally
    inherited Update;
  end;
end;

end.
