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

{   Contains:

    Modifyed:
}

unit BeeGui_Consts;

{$I compiler.inc}

interface

uses
  SysUtils;

const
  cApplicationVersion   = '1.0.5 [build 0692]';
  cApplicationCopyright = '(C) 2003-2009 Andrew Filinsky and Melchiorre Caruso';

  cApplicationName    = 'BeeGui';
  cApplicationTitle   = 'Bee File Manager';
  cApplicationCaption = 'Bee File Manager';

  cApplicationViewerName    = 'BeeViewer';
  cApplicationViewerTitle   = 'Bee File Viewer';
  cApplicationViewerCaption = 'Bee File Viewer';

  cApplicationHomePage = 'http://www.beegui.org';

  cApplicationLicenseFile = 'license.htm';
  cApplicationHelpFile    = 'help.htm';
  cApplicationDocsFolder  = 'docs';

  cApplicationPluginsFolder    = 'plug-ins';
  cApplicationLanguagesFolder  = 'languages';
  cApplicationSmallIconsFolder = 'smallicons';
  cApplicationLargeIconsFolder = 'largeicons';

function GetApplicationPluginsDir: string;
function GetApplicationLanguageDir: string;
function GetApplicationSmallIconsDir: string;
function GetApplicationLargeIconsDir: string;

function GetApplicationCaption(const aApplicationCaption, aFileName: string): string;

implementation

uses
  GetText;

function GetApplicationCaption(const aApplicationCaption, aFileName: string): string;
begin
  if aFileName = '' then
    Result := aApplicationCaption
  else
    Result := aApplicationCaption + ' - ' + ExtractFileName(aFileName);
end;

function GetApplicationLanguageID: string;
var
  S: string;
begin
  GetLanguageIDs(S, Result);
  Result := LowerCase(Result);
end;

function GetApplicationLanguageDir: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + cApplicationLanguagesFolder;
  if GetApplicationLanguageID <> '' then
  begin
    Result := IncludeTrailingBackSlash(Result) + GetApplicationLanguageID;
  end;
end;

function GetApplicationPluginsDir: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + cApplicationPluginsFolder;
end;

function GetApplicationSmallIconsDir: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + cApplicationSmallIconsFolder;
end;

function GetApplicationLargeIconsDir: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + cApplicationLargeIconsFolder;
end;

end.
