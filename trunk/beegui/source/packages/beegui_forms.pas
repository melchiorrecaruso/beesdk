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
  BeeGui_CmdLine;
  
function ConfirmAdd(CmdLine: TCmdLine): boolean;
function ConfirmExtract(CmdLine: TCmdLine): boolean;

implementation

uses
  Bee_Common,
  // ---
  BeeGui_AddFrm,
  BeeGui_ExtractFrm;

  function ConfirmAdd(CmdLine: TCmdLine): boolean;
  var
    I: integer;
    F: TAddFrm;
  begin
    F := TAddFrm.Create(Application);
    F.rOption.Checked := CmdLine.rOption;

    if (CmdLine.uOption xor CmdLine.fOption) then
    begin
      if CmdLine.uOption then
        F.ufOption.ItemIndex := 0
      else
        F.ufOption.ItemIndex := 1;
    end else
    begin
      F.ufOption.ItemIndex := 2;
    end;

    F.eOption.Text := CmdLine.eOption;
    F.sOption.Checked := CmdLine.sOption;

    if Length(CmdLine.aOption) > 0 then
    begin
      F.aOptionCheck.Checked := True;
      if CompareFileName(CmdLine.aOption, 'beegui.sfx') = 0 then
        F.aOption.ItemIndex := 0
      else
        if CompareFileName(CmdLine.aOption, 'bee.sfx') = 0 then
          F.aOption.ItemIndex := 1
        else
          if CmdLine.aOption = 'nul' then
            F.aOption.ItemIndex := 2;

    end else
      F.aOptionCheck.Checked := False;

    F.mOption.ItemIndex := CmdLine.mOption;
    F.dOption.ItemIndex := CmdLine.dOption;

    for i := 0 to CmdLine.xOption.Count -1  do
      F.FilesMgr.PlusMinus(F.FilesMgr.AddFile(
        ExpandFileName(CmdLine.xOption.Strings[i])));

    F.tOption.Checked := CmdLine.tOption;
    F.lOption.Checked := CmdLine.lOption;

    if Length(CmdLine.yOption) > 0 then
      F.yOption.Text := CmdLine.yOption;

    F.kOption.Checked := CmdLine.kOption;

    if Length(CmdLine.cdOption) > 0 then
      F.cdOption.Text := CmdLine.cdOption;

    if Length(CmdLine.cfgOption) > 0 then
      F.cfgOption.Text := CmdLine.cfgOption;
      
    F.ArchiveName.Text := ExpandFileName(CmdLine.ArcName);
    
    for I := 0 to CmdLine.FileMasks.Count - 1 do
    begin
      F.FilesMgr.AddFile(ExpandFileName(CmdLine.FileMasks.Strings[I]));
    end;
    F.FilesMgr.RootValue := GetCurrentDir;

    if F.ShowModal = mrOk then
    begin
      CmdLine.rOption := F.rOption.Checked;

      case F.ufOption.ItemIndex of
        0:   begin
               CmdLine.uOption := True;
               CmdLine.fOption := False;
             end;
        1:   begin
               CmdLine.uOption := False;
               CmdLine.fOption := True;
             end;
        else begin
               CmdLine.uOption := True;
               CmdLine.fOption := True;
             end;
      end;

      CmdLine.eOption := F.eOption.Text;
      CmdLine.sOption := F.sOption.Checked;

      if F.aOptionCheck.Checked then
      begin
        case F.aOption.ItemIndex of
          0: CmdLine.aOption := 'beegui.sfx';
          1: CmdLine.aOption := 'bee.sfx';
          2: CmdLine.aOption := 'nul';
        end;
      end;

      CmdLine.mOption := F.mOption.ItemIndex;
      CmdLine.dOption := F.dOption.ItemIndex;

      CmdLine.xOption.Clear;
      for I := 0 to F.FilesMgr.Count - 1 do
        if F.FilesMgr.Excluded[I] = True then
          CmdLine.xOption.Add(F.FilesMgr.Items[I]);

      CmdLine.tOption := F.tOption.Checked;
      CmdLine.lOption := F.lOption.Checked;
      CmdLine.yOption := F.yOption.Text;
      CmdLine.kOption := F.kOption.Checked;

      CmdLine.cdOption := F.cdOption.Text;
      CmdLine.cfgOption := F.cfgOption.Text;

      CmdLine.ArcName := F.ArchiveName.Text;

      CmdLine.FileMasks.Clear;
      for I := 0 to F.FilesMgr.Count - 1 do
        if F.FilesMgr.Excluded[I] = False then
          CmdLine.FileMasks.Add(F.FilesMgr.Items[I]);

      if (CmdLine.FileMasks.Count > 0) or (F.aOptionCheck.Checked) then
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
  
function ConfirmExtract(CmdLine: TCmdLine): boolean;
  var
    F: TExtractFrm;
  begin
    F := TExtractFrm.Create(Application);

    F.xCommand.Checked := CmdLine.Command = 'X';

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

    case UpCase(CmdLine.oOption) of
      'S': F.oOption.ItemIndex := 0;
      'A': F.oOption.ItemIndex := 1;
      'Q': F.oOption.ItemIndex := 2;
    end;

    F.cdOptionCheck.Checked := Length(CmdLine.cdOption) > 0;
    F.cdOption.Text := CmdLine.cdOption;

    if F.ShowModal = mrOk then
    begin
      if F.xCommand.Checked then
        CmdLine.Command := 'X'
      else
        CmdLine.Command := 'E';

      { TODO : sistemare con uOption ed fOption }

      case F.oOption.ItemIndex of
        0: CmdLine.oOption := 'S';
        1: CmdLine.oOption := 'A';
        2: CmdLine.oOption := 'Q';
      end;

      if F.cdOptionCheck .Enabled then
        CmdLine.cdOption := F.cdOption.Text
      else
        CmdLine.cdOption := '';

      Result := SetCurrentDir(F.Folder.Text);
    end else
      Result := False;

    F.Free;
  end;

end.

