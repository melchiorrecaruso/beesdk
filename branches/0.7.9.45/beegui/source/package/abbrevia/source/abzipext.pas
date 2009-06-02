(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

 {*********************************************************}
 {* ABBREVIA: AbZipExt.pas 3.05                           *}
 {*********************************************************}
 {* ABBREVIA: Zip file registration                       *}
 {*********************************************************}

{$I AbDefine.inc}

unit AbZipExt;

interface

uses
  SysUtils, Classes;

function AbExistingZipAssociation: boolean;
function AbGetZipAssociation(var App, ID, FileType: string): boolean;
function AbRegisterZipExtension(const App: string;
  ID, FileType: string;
  Replace: boolean): boolean;


implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
  Registry,
  ShellAPI,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  AbConst;

const
  ZipExt = '.zip';
  DefZipID = 'Zip';
  DefZipType = 'Zip File';
  OpenCommand = 'Shell\Open\Command';
  DefaultIcon = 'DefaultIcon';

var
  Reg: TRegistry;

{ -------------------------------------------------------------------------- }
function AbExistingZipAssociation: boolean;
var
  App, ID, FileType: string;
begin
  Result := False;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CLASSES_ROOT;
  Reg.OpenKey('', False);
  if Reg.OpenKey(ZipExt, False) then
  begin
    ID := Reg.ReadString('');
    if Reg.OpenKey('\' + ID, False) then
    begin
      FileType := Reg.ReadString('');
      if Reg.OpenKey(OpenCommand, False) then
      begin
        App := Reg.ReadString('');
        if (App <> '') then
          Result := True;
      end;
    end;
  end;
  Reg.Free;
end;

{ -------------------------------------------------------------------------- }
function AbGetZipAssociation(var App, ID, FileType: string): boolean;
begin
  Result := False;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CLASSES_ROOT;
  Reg.OpenKey('', False);
  if Reg.OpenKey(ZipExt, False) then
  begin
    ID := Reg.ReadString('');
    if Reg.OpenKey('\' + ID, False) then
    begin
      FileType := Reg.ReadString('');
      if Reg.OpenKey(OpenCommand, False) then
      begin
        App := Reg.ReadString('');
        Result := True;
      end;
    end;
  end;
  Reg.Free;
end;

{ -------------------------------------------------------------------------- }
function AbRegisterZipExtension(const App: string;
  ID, FileType: string;
  Replace: boolean): boolean;
begin
  Result := False;
  if AbExistingZipAssociation and not Replace then
    Exit;
  try
    if (ID = '') then
      ID := DefZipID;
    if (FileType = '') then
      FileType := DefZipType;
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('', False);
    Reg.OpenKey(ZipExt, True);
    Reg.WriteString('', ID);
    Reg.OpenKey('\' + ID, True);
    Reg.WriteString('', FileType);
    Reg.OpenKey(OpenCommand, True);
    Reg.WriteString('', App);
    Reg.OpenKey('\' + DefaultIcon, True);
    Reg.WriteString('', App + ',0');
    Result := True;
  finally
    Reg.Free;
  end;
end;

{ -------------------------------------------------------------------------- }

end.