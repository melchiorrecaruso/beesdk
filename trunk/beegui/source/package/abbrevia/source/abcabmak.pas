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
 {* ABBREVIA: AbCabMak.pas 3.05                           *}
 {*********************************************************}
 {* ABBREVIA: Cabinet builder component (VCL)             *}
 {*   See AbQCabMk.pas for the CLX header                 *}
 {*********************************************************}

unit AbCabMak;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes,
  AbCBrows,
  AbArcTyp, AbCabTyp;

type
  TAbCustomMakeCab = class(TAbCustomCabBrowser)
  protected {private}
    FFolderThreshold: longint;
    FCompressionType: TAbCabCompressionType;
    FOnSave: TAbArchiveEvent;

  protected {methods}
    procedure DoSave(Sender: TObject); virtual;
    procedure InitArchive; override;
    procedure SetFolderThreshold(Value: longint);
    procedure SetCompressionType(Value: TAbCabCompressionType);
    procedure SetFileName(const aFileName: string); override;

  protected {properties}
    property CompressionType: TAbCabCompressionType
      Read FCompressionType Write SetCompressionType;
    property FolderThreshold: longint Read FFolderThreshold Write SetFolderThreshold;

  protected {events}
    property OnSave: TAbArchiveEvent Read FOnSave Write FOnSave;

  public {methods}
    constructor Create(AOwner: TComponent); override;
    procedure AddFiles(const FileMask: string; SearchAttr: integer);
    procedure AddFilesEx(const FileMask: string;
      const ExclusionMask: string; SearchAttr: integer);
    procedure StartNewFolder;
    procedure StartNewCabinet;
  end;

type
  TAbMakeCab = class(TAbCustomMakeCab)
  published
    property ArchiveProgressMeter;
    property BaseDirectory;
    property CabSize;
    property CompressionType;
    property FolderThreshold;
    property ItemProgressMeter;
    property OnArchiveProgress;
    property OnArchiveItemProgress;
    property OnChange;
    property OnConfirmProcessItem;
    property OnLoad;
    property OnProcessItemFailure;
    property OnRequestImage;
    property OnSave;
    property SetID;
    property SpanningThreshold;
    property TempDirectory;
    property Version;
    property FileName; {must be after OnLoad}
  end;

{.Z+}


implementation

uses
  AbExcept,
  AbUtils;

{ TAbCustomMakeCab ========================================================= }
constructor TAbCustomMakeCab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompressionType := AbDefCompressionType;
  FSpanningThreshold := AbDefCabSpanningThreshold;
  FFolderThreshold := AbDefFolderThreshold;
  FSetID := 0;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.AddFiles(const FileMask: string; SearchAttr: integer);
{Add files to the cabinet where the disk filespec matches}
begin
  if Assigned(CabArchive) then
    CabArchive.AddFiles(FileMask, SearchAttr)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.AddFilesEx(const FileMask: string;
  const ExclusionMask: string;
  SearchAttr: integer);
{Add files that match Filemask except those matching ExclusionMask}
begin
  if Assigned(CabArchive) then
    CabArchive.AddFilesEx(FileMask, ExclusionMask, SearchAttr)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.DoSave(Sender: TObject);
begin
  if Assigned(FOnSave) then
    FOnSave(Self);
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.InitArchive;
begin
  inherited InitArchive;
  if Assigned(CabArchive) then
  begin
    {properties}
    CabArchive.FolderThreshold := FFolderThreshold;
    CabArchive.CompressionType := FCompressionType;
    CabArchive.SetID  := FSetID;
    {events}
    CabArchive.OnSave := DoSave;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.SetCompressionType(Value: TAbCabCompressionType);
{Set the type of compression to use}
begin
  FCompressionType := Value;
  if Assigned(CabArchive) then
    CabArchive.CompressionType := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.SetFileName(const aFileName: string);
{Create the specified cabinet file}
begin
  FFilename := aFileName;
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FArchive) then
  begin
    FArchive.Free;
    FArchive := nil;
  end;
  if (aFileName <> '') then
  begin
    FArchive := TAbCabArchive.Create(aFileName, fmOpenWrite);
    InitArchive;
    FArchive.Load;
    FArchiveType := atCab;
  end;
  DoChange;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.SetFolderThreshold(Value: longint);
{Set folder compression boundary}
begin
  FFolderThreshold := Value;
  if Assigned(CabArchive) then
    CabArchive.FolderThreshold := Value;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.StartNewCabinet;
{Flush current cabinet and start a new one}
begin
  if Assigned(CabArchive) then
    CabArchive.NewCabinet
  else
    raise EAbNoArchive.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbCustomMakeCab.StartNewFolder;
{Flush current folder and start a new one}
begin
  if Assigned(CabArchive) then
    CabArchive.NewFolder
  else
    raise EAbNoArchive.Create;
end;

{ -------------------------------------------------------------------------- }

end.
