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

unit BeeGui_AddFrmRes;

{$I Compiler.inc}

interface

const
  _CAddFrm_SelFolderToAdd = 0;
  _CAddFrm_ModifyMask = 1;

resourcestring

  _SAddFrm_Property = 'AddFrm.WindowState;' +
    'AddFrm_FileList.Columns.Width;';
  _SAddFrm_PropertyFull = 'AddFrm.Left;' + 'AddFrm.Height;' +
    'AddFrm.Top;' + 'AddFrm.Width;' +
    'AddFrm.WindowState;' +
    'AddFrm_FileList.Columns.Width;';
  _SAddFrm_Language = 'AddFrm.Caption;' +
    'AddFrm_OpenDialog;' +
    'AddFrm_Pages_General.Caption;' +
    'AddFrm_Pages_Files.Caption;' +
    'AddFrm_ActionGB.Caption;' +
    'AddFrm_MethodGB.Caption;' +
    'AddFrm_ZipMethodGB.Caption;' +
    'AddFrm_DictionaryGB.Caption;' +
    'AddFrm_ZipCompressionLevelGB.Caption;' +
    'AddFrm_PriorityGB.Caption;' +
    'AddFrm_ForceExtGB.Caption;' +
    'AddFrm_OptionsGB.Caption;' +
    'AddFrm_Action.Items;' +
    'AddFrm_Method.Items;' +
    'AddFrm_ZipMethod.Items;' +
    'AddFrm_Dictionary.Items;' +
    'AddFrm_ZipCompressionLevel.Items;' +
    'AddFrm_Priority.Items;' +
    'AddFrm_Messages.Items;' +
    'AddFrm_rOption.Caption;' +
    'AddFrm_sOption.Caption;' +
    'AddFrm_tOption.Caption;' +
    'AddFrm_pOption.Caption;' +
    'AddFrm_cOption.Caption;' +
    'AddFrm_kOption.Caption;' +
    'AddFrm_aOption.Caption;' +
    'AddFrm_FileList.Columns.Caption;' +
    'AddFrm_FileListLabel.Caption;' +
    'AddFrm_RootLabel.Caption;' +
    'AddFrm_PMenu_AddDir.Caption;' +
    'AddFrm_PMenu_AddFiles.Caption;' +
    'AddFrm_PMenu_View.Caption;' +
    'AddFrm_PMenu_Type.Caption;' +
    'AddFrm_PMenu_Modify.Caption;' +
    'AddFrm_PMenu_Delete.Caption;' +
    'BtnDir.Caption;' +
    'BtnFile.Caption;' +
    'BtnView.Caption;' +
    'BtnType.Caption;' +
    'BtnModify.Caption;' +
    'BtnDelete.Caption;' +
    'BtnCancel.Caption;' +
    'BtnOk.Caption;';

implementation

end.
