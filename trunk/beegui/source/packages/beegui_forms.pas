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
  Bee_Common,
  // ---
  BeeGui_AddFrm,
  BeeGui_ExtractFrm;

function ConfirmAdd(CommandLine: TCustomCommandLine): boolean;
var
  I: integer;
begin
  AddFrm := TAddFrm.Create(Application);
  AddFrm.rOption.Checked := CommandLine.rOption;

  if (CommandLine.uOption xor CommandLine.fOption) then
  begin
    if CommandLine.uOption then
      AddFrm.ufOption.ItemIndex := 0
    else
      AddFrm.ufOption.ItemIndex := 1;
  end
  else
  begin
    AddFrm.ufOption.ItemIndex := 2;
  end;

  AddFrm.eOption.Text    := CommandLine.eOption;
  AddFrm.sOption.Checked := CommandLine.sOption;

  AddFrm.aOptionCheck.Checked := Length(CommandLine.aOption) > 0;
  if AddFrm.aOptionCheck.Checked then
  begin
    if CompareFileName(CommandLine.aOption, 'beegui.sfx') = 0 then
      AddFrm.aOption.ItemIndex := 0
    else
    if CompareFileName(CommandLine.aOption, 'bee.sfx') = 0 then
      AddFrm.aOption.ItemIndex := 1
    else
    if CommandLine.aOption = 'nul' then
      AddFrm.aOption.ItemIndex := 2;
  end;

  AddFrm.mOption.ItemIndex := CommandLine.mOption;
  AddFrm.dOption.ItemIndex := CommandLine.dOption;

  for i := 0 to CommandLine.xOption.Count - 1 do
    AddFrm.FilesMgr.PlusMinus(AddFrm.FilesMgr.AddFile(
      ExpandFileName(CommandLine.xOption.Strings[i])));

  AddFrm.tOption.Checked := CommandLine.tOption;

  if Length(CommandLine.yOption) > 0 then
    AddFrm.yOption.Text := CommandLine.yOption;

  AddFrm.kOption.Checked := CommandLine.kOption;

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
    CommandLine.rOption := AddFrm.rOption.Checked;

    case AddFrm.ufOption.ItemIndex of
      0: begin
           CommandLine.uOption := True;
           CommandLine.fOption := False;
         end;
      1: begin
           CommandLine.uOption := False;
           CommandLine.fOption := True;
         end;
      else
         begin
           CommandLine.uOption := True;
           CommandLine.fOption := True;
         end;
    end;

    CommandLine.eOption := AddFrm.eOption.Text;
    CommandLine.sOption := AddFrm.sOption.Checked;

    if AddFrm.aOptionCheck.Checked then
    begin
      case AddFrm.aOption.ItemIndex of
        0: CommandLine.aOption := 'beegui.sfx';
        1: CommandLine.aOption := 'bee.sfx';
        2: CommandLine.aOption := 'nul';
      end;
    end;

    CommandLine.mOption := AddFrm.mOption.ItemIndex;
    CommandLine.dOption := AddFrm.dOption.ItemIndex;

    CommandLine.xOption.Clear;
    for I := 0 to AddFrm.FilesMgr.Count - 1 do
      if AddFrm.FilesMgr.Excluded[I] = True then
        CommandLine.xOption.Add(AddFrm.FilesMgr.Items[I]);

    CommandLine.tOption := AddFrm.tOption.Checked;
    CommandLine.yOption := AddFrm.yOption.Text;
    CommandLine.kOption := AddFrm.kOption.Checked;

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

  ExtractFrm.xCommand.Checked := CommandLine.Command = 'X';

  { TODO : }

  // if (FuOption xor FfOption) then
  // begin
  //   if FuOption then
  //     ExtractFrm.ufOption.ItemIndex := 0
  //   else
  //     ExtractFrm.ufOption.ItemIndex := 1;
  // end else
  // begin
  //   ExtractFrm.ufOption.ItemIndex := 2;
  // end;

  case UpCase(CommandLine.oOption) of
    'Y': ExtractFrm.oOption.ItemIndex := 0;
    'A': ExtractFrm.oOption.ItemIndex := 1;
    'S': ExtractFrm.oOption.ItemIndex := 2;
    else
      ExtractFrm.oOption.ItemIndex := 0;
  end;

  with ExtractFrm do
  begin
    cdOption.Text := CommandLine.cdOption;
    cdOptionCheck.Enabled := cdOption.Text <> '';
    cdOptionCheck.Checked := cdOption.Text <> '';
  end;

  if ExtractFrm.ShowModal = mrOk then
  begin
    if ExtractFrm.xCommand.Checked then
      CommandLine.Command := 'X'
    else
      CommandLine.Command := 'E';

    { TODO : }

    case ExtractFrm.oOption.ItemIndex of
      0: CommandLine.oOption := 'Y';
      1: CommandLine.oOption := 'A';
      2: CommandLine.oOption := 'S';
      else
         CommandLine.oOption := 'Y';
    end;

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

    Result := SetCurrentDir(ExtractFrm.Folder.Text);
  end else
    Result := False;

  FreeAndNil(ExtractFrm);
end;

end.
