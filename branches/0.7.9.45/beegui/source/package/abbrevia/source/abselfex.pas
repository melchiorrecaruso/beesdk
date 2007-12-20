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
 {* ABBREVIA: AbSelfEx.pas 3.05                           *}
 {*********************************************************}
 {* ABBREVIA: Component for building self-extracting zips *}
 {*********************************************************}

{$I AbDefine.inc}

unit AbSelfEx;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  Libc, 
{$ENDIF}
  AbArcTyp, AbBase, AbExcept, AbUtils, AbZipTyp;

type
  TAbGetFileEvent = procedure(Sender: TObject; var aFilename: string;
    var Abort: boolean) of object;

type
  TAbMakeSelfExe = class(TAbBaseComponent)
  protected {private}
    FStubExe: string;
    FZipFile: string;
    FSelfExe: string;
    FStubStream: TFileStream;
    FZipStream: TFileStream;
    FSelfStream: TFileStream;
    FOnGetStubExe: TAbGetFileEvent;
    FOnGetZipFile: TAbGetFileEvent;

    procedure DoGetStubExe(var Abort: boolean);
    procedure DoGetZipFile(var Abort: boolean);

  public
    function Execute: boolean;

  published
    property SelfExe: string Read FSelfExe Write FSelfExe;
    property StubExe: string Read FStubExe Write FStubExe;
    property ZipFile: string Read FZipFile Write FZipFile;
    property OnGetStubExe: TAbGetFileEvent Read FOnGetStubExe Write FOnGetStubExe;
    property OnGetZipFile: TAbGetFileEvent Read FOnGetZipFile Write FOnGetZipFile;
    property Version;
  end;


implementation

uses
  {$IFDEF TrialRun}
  AbTrial,
  {$ENDIF}
  AbConst;

{ -------------------------------------------------------------------------- }
function TAbMakeSelfExe.Execute: boolean;
var
  Abort: boolean;
begin
  Abort := False;
  if (FStubExe = '') then
    DoGetStubExe(Abort);
  if Abort then
    raise EAbUserAbort.Create;
  if not FileExists(FStubExe) then
    raise EAbFileNotFound.Create;
  if (FZipFile = '') then
    DoGetZipFile(Abort);
  if Abort then
    raise EAbUserAbort.Create;
  if not FileExists(FZipFile) then
    raise EAbFileNotFound.Create;

  FStubStream := TFileStream.Create(FStubExe, fmOpenRead or fmShareDenyWrite);
  FZipStream  := TFileStream.Create(FZipFile, fmOpenRead or fmShareDenyWrite);
  if (FSelfExe = '') then
    FSelfExe := ChangeFileExt(FZipFile, '.exe');
  FSelfStream := TFileStream.Create(FSelfExe, fmCreate or fmShareExclusive);
  try
    MakeSelfExtracting(FStubStream, FZipStream, FSelfStream);
    Result := True;
  finally
    FStubStream.Free;
    FZipStream.Free;
    FSelfStream.Free;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbMakeSelfExe.DoGetStubExe(var Abort: boolean);
begin
  if Assigned(FOnGetStubExe) then
    FOnGetStubExe(Self, FStubExe, Abort);
end;

{ -------------------------------------------------------------------------- }
procedure TAbMakeSelfExe.DoGetZipFile(var Abort: boolean);
begin
  if Assigned(FOnGetZipFile) then
    FOnGetZipFile(Self, FZipFile, Abort);
end;

{ -------------------------------------------------------------------------- }

end.
