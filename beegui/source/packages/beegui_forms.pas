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

    BeeGui forms routines.

  Modifyed:
}

unit BeeGui_Forms;

{$I compiler.inc}

interface

uses
  Forms,
  Dialogs,
  Classes,
  Controls,
  SysUtils,
  // ---
  BeeGui_CommandLine;

function ConfirmAdd(CommandLine: TCustomCommandLine): boolean;
function ConfirmExtract(CommandLine: TCustomCommandLine): boolean;

implementation

uses
  Bee_Types,
  Bee_Common,
  // ---
  BeeGui_AddFrm,
  BeeGui_ExtractFrm;

function ConfirmAdd(CommandLine: TCustomCommandLine): boolean;
var
  I: integer;
begin
  AddFrm := TAddFrm.Create(Application);
  AddFrm.rOption.Checked := CommandLine.rOption = rmFull;

  case CommandLine.uOption of
    umAdd:           AddFrm.uOption.ItemIndex := 0;
    umUpdate:        AddFrm.uOption.ItemIndex := 1;
    umReplace:       AddFrm.uOption.ItemIndex := 2;
    umAddUpdate:     AddFrm.uOption.ItemIndex := 3;
    umAddReplace:    AddFrm.uOption.ItemIndex := 4;
    umAddAutoRename: AddFrm.uOption.ItemIndex := 5;
  end;

  AddFrm.eOption.Text    := CommandLine.fOption;
  AddFrm.sOption.Checked := CommandLine.sOption;

  AddFrm.aOptionCheck.Checked := Length(CommandLine.sfxOption) > 0;
  if AddFrm.aOptionCheck.Checked then
  begin
    if CompareFileName(CommandLine.sfxOption, 'beegui.sfx') = 0 then
      AddFrm.aOption.ItemIndex := 0
    else
    if CompareFileName(CommandLine.sfxOption, 'bee.sfx') = 0 then
      AddFrm.aOption.ItemIndex := 1
    else
    if CommandLine.sfxOption = 'nul' then
      AddFrm.aOption.ItemIndex := 2;
  end;

  AddFrm.mOption.ItemIndex := Ord(CommandLine.mOption);
  AddFrm.dOption.ItemIndex := Ord(CommandLine.dOption);

  for i := 0 to CommandLine.xOptions.Count - 1 do
    AddFrm.FilesMgr.PlusMinus(AddFrm.FilesMgr.AddFile(
      ExpandFileName(CommandLine.xOptions.Strings[i])));

  AddFrm.tOption.Checked := CommandLine.tOption;

  if Length(CommandLine.wdOption) > 0 then
    AddFrm.yOption.Text := CommandLine.wdOption;

  AddFrm.kOption.Checked := Length(CommandLine.pOption) > 0;

  AddFrm.cdOptionCheck.Checked := Length(CommandLine.cdOption) > 0;
  if AddFrm.cdOptionCheck.Checked then
  begin
    AddFrm.cdOption.Text := CommandLine.cdOption;
  end;

  if Length(CommandLine.cfgOption) > 0 then
    AddFrm.cfgOption.Text := CommandLine.cfgOption;

  AddFrm.ArchiveName := CommandLine.ArchiveName;

  for I := 0 to CommandLine.FileMasks.Count - 1 do
  begin
    AddFrm.FilesMgr.AddFile(ExpandFileName(CommandLine.FileMasks.Strings[I]));
  end;
  AddFrm.FilesMgr.RootValue := GetCurrentDir;

  if AddFrm.ShowModal = mrOk then
  begin
    if AddFrm.rOption.Checked then
      CommandLine.rOption := rmWildCard
    else
      CommandLine.rOption := rmNone;

    CommandLine.uOption := TUpdateMode(AddFrm.uOption.ItemIndex);
    CommandLine.fOption := AddFrm.eOption.Text;
    CommandLine.sOption := AddFrm.sOption.Checked;

    if AddFrm.aOptionCheck.Checked then
    begin
      case AddFrm.aOption.ItemIndex of
        0: CommandLine.sfxOption := 'beegui.sfx';
        1: CommandLine.sfxOption := 'bee.sfx';
        2: CommandLine.sfxOption := 'nul';
      end;
    end;

    CommandLine.mOption := TmOption(AddFrm.mOption.ItemIndex);
    CommandLine.dOption := TdOption(AddFrm.dOption.ItemIndex);

    CommandLine.xOptions.Clear;
    for I := 0 to AddFrm.FilesMgr.Count - 1 do
      if AddFrm.FilesMgr.Excluded[I] = True then
        CommandLine.xOptions.Add(AddFrm.FilesMgr.Items[I]);

    CommandLine. tOption := AddFrm.tOption.Checked;
    CommandLine.wdOption := AddFrm.yOption.Text;
    // CommandLine. pOption := AddFrm.kOption.Checked;

    if AddFrm.cdOptionCheck.Checked then
      CommandLine.cdOption := AddFrm.cdOption.Text
    else
      CommandLine.cdOption := '';

    CommandLine.cfgOption := AddFrm.cfgOption.Text;

    CommandLine.ArchiveName := AddFrm.ArchiveName;

    CommandLine.FileMasks.Clear;
    for I := 0 to AddFrm.FilesMgr.Count - 1 do
      if AddFrm.FilesMgr.Excluded[I] = False then
        CommandLine.FileMasks.Add(AddFrm.FilesMgr.Items[I]);

    if (CommandLine.FileMasks.Count > 0) or (AddFrm.aOptionCheck.Checked) then
    begin
      if Length(AddFrm.FilesMgr.RootValue) > 0 then
        Result := SetCurrentDir(AddFrm.FilesMgr.RootValue)
      else
        Result := True;
    end else
      Result := False;
  end else
    Result := False;

  FreeAndNil(AddFrm);
end;

function ConfirmExtract(CommandLine: TCustomCommandLine): boolean;
var
  I: integer;
begin
  ExtractFrm := TExtractFrm.Create(Application);
  ExtractFrm.uOption.ItemIndex := Ord(CommandLine.uOption);
  ExtractFrm.xCommand.Checked  := CommandLine.Command = ccXextract;

  with ExtractFrm do
  begin
    cdOption.Text := CommandLine.cdOption;
    cdOptionCheck.Enabled := cdOption.Text <> '';
    cdOptionCheck.Checked := cdOption.Text <> '';
  end;

  Result := ExtractFrm.ShowModal = mrOk;
  if Result then
  begin
    Result := SetCurrentDir(ExtractFrm.Folder.Text);

    CommandLine.uOption := TUpdateMode(ExtractFrm.uOption.ItemIndex);
    if ExtractFrm.xCommand.Checked then
      CommandLine.Command := ccXextract
    else
      CommandLine.Command := ccExtract;

    if ExtractFrm.cdOptionCheck.Checked then
    begin
      CommandLine.cdOption := ExtractFrm.cdOption.Text;
    end else
    begin
      CommandLine.cdOption := '';
      with CommandLine.FileMasks do
        for I := 0 to Count -1 do
        begin
          Strings[I] := IncludeTrailingBackSlash(ExtractFrm.cdOption.Text) + Strings[I];
        end;
    end;
  end;
  FreeAndNil(ExtractFrm);
end;

end.
