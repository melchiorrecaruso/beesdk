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
    F: TAddFrm;
  begin
    F := TAddFrm.Create(Application);
    F.rOption.Checked := CommandLine.rOption;

    if (CommandLine.uOption xor CommandLine.fOption) then
    begin
      if CommandLine.uOption then
        F.ufOption.ItemIndex := 0
      else
        F.ufOption.ItemIndex := 1;
    end else
    begin
      F.ufOption.ItemIndex := 2;
    end;

    F.eOption.Text := CommandLine.eOption;
    F.sOption.Checked := CommandLine.sOption;

    F.aOptionCheck.Checked := Length(CommandLine.aOption) > 0;
    if F.aOptionCheck.Checked then
    begin
      if CompareFileName(CommandLine.aOption, 'beegui.sfx') = 0 then
        F.aOption.ItemIndex := 0
      else
        if CompareFileName(CommandLine.aOption, 'bee.sfx') = 0 then
          F.aOption.ItemIndex := 1
        else
          if CommandLine.aOption = 'nul' then
            F.aOption.ItemIndex := 2;
    end;

    F.mOption.ItemIndex := CommandLine.mOption;
    F.dOption.ItemIndex := CommandLine.dOption;

    for i := 0 to CommandLine.xOption.Count -1  do
      F.FilesMgr.PlusMinus(F.FilesMgr.AddFile(
        ExpandFileName(CommandLine.xOption.Strings[i])));

    F.tOption.Checked := CommandLine.tOption;
    F.lOption.Checked := CommandLine.lOption;

    if Length(CommandLine.yOption) > 0 then
      F.yOption.Text := CommandLine.yOption;

    F.kOption.Checked := CommandLine.kOption;

    F.cdOptionCheck.Checked := Length(CommandLine.cdOption) > 0;
    if  F.cdOptionCheck.Checked then
    begin
      F.cdOption.Text := CommandLine.cdOption;
    end;
    
    if Length(CommandLine.cfgOption) > 0 then
      F.cfgOption.Text := CommandLine.cfgOption;

    F.ArchiveName := CommandLine.ArchiveName;

    for I := 0 to CommandLine.FileMasks.Count - 1 do
    begin
      F.FilesMgr.AddFile(ExpandFileName(CommandLine.FileMasks.Strings[I]));
    end;
    F.FilesMgr.RootValue := GetCurrentDir;

    if F.ShowModal = mrOk then
    begin
      CommandLine.rOption := F.rOption.Checked;

      case F.ufOption.ItemIndex of
        0:   begin
               CommandLine.uOption := True;
               CommandLine.fOption := False;
             end;
        1:   begin
               CommandLine.uOption := False;
               CommandLine.fOption := True;
             end;
        else begin
               CommandLine.uOption := True;
               CommandLine.fOption := True;
             end;
      end;

      CommandLine.eOption := F.eOption.Text;
      CommandLine.sOption := F.sOption.Checked;

      if F.aOptionCheck.Checked then
      begin
        case F.aOption.ItemIndex of
          0: CommandLine.aOption := 'beegui.sfx';
          1: CommandLine.aOption := 'bee.sfx';
          2: CommandLine.aOption := 'nul';
        end;
      end;

      CommandLine.mOption := F.mOption.ItemIndex;
      CommandLine.dOption := F.dOption.ItemIndex;

      CommandLine.xOption.Clear;
      for I := 0 to F.FilesMgr.Count - 1 do
        if F.FilesMgr.Excluded[I] = True then
          CommandLine.xOption.Add(F.FilesMgr.Items[I]);

      CommandLine.tOption := F.tOption.Checked;
      CommandLine.lOption := F.lOption.Checked;
      CommandLine.yOption := F.yOption.Text;
      CommandLine.kOption := F.kOption.Checked;

      if F.cdOptionCheck.Checked then
        CommandLine.cdOption := F.cdOption.Text
      else
        CommandLine.cdOption := '';

      CommandLine.cfgOption := F.cfgOption.Text;

      CommandLine.ArchiveName := F.ArchiveName;

      CommandLine.FileMasks.Clear;
      for I := 0 to F.FilesMgr.Count - 1 do
        if F.FilesMgr.Excluded[I] = False then
          CommandLine.FileMasks.Add(F.FilesMgr.Items[I]);

      if (CommandLine.FileMasks.Count > 0) or (F.aOptionCheck.Checked) then
      begin
        if Length(F.FilesMgr.RootValue) > 0 then
          Result := SetCurrentDir(F.FilesMgr.RootValue)
        else
          Result := True;
      end else
        Result := False;
    end else
      Result := False;

    F.Free;
  end;
  
function ConfirmExtract(CommandLine: TCustomCommandLine): boolean;
  var
    F: TExtractFrm;
  begin
    F := TExtractFrm.Create(Application);

    F.xCommand.Checked := CommandLine.Command = 'X';

    { TODO : Aggiungere nella overwriteBox altri valori }

    // if (FuOption xor FfOption) then
    // begin
    //   if FuOption then
    //     F.ufOption.ItemIndex := 0
    //   else
    //     F.ufOption.ItemIndex := 1;
    // end else
    // begin
    //   F.ufOption.ItemIndex := 2;
    // end;

    case UpCase(CommandLine.oOption) of
      'S': F.oOption.ItemIndex := 0;
      'A': F.oOption.ItemIndex := 1;
      'Q': F.oOption.ItemIndex := 2;
    end;

    F.cdOptionCheck.Checked := Length(CommandLine.cdOption) > 0;
    F.cdOption.Text := CommandLine.cdOption;

    if F.ShowModal = mrOk then
    begin
      if F.xCommand.Checked then
        CommandLine.Command := 'X'
      else
        CommandLine.Command := 'E';

      { TODO : sistemare con uOption ed fOption }

      case F.oOption.ItemIndex of
        0: CommandLine.oOption := 'S';
        1: CommandLine.oOption := 'A';
        2: CommandLine.oOption := 'Q';
      end;

      if F.cdOptionCheck .Enabled then
        CommandLine.cdOption := F.cdOption.Text
      else
        CommandLine.cdOption := '';

      Result := SetCurrentDir(F.Folder.Text);
    end else
      Result := False;

    F.Free;
  end;

end.

