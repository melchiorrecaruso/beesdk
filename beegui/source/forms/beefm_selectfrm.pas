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

{   Contains:

    BeeGui Select-UnSelect form.

    Modifyed:

    v1.0.1 build 9140 - 2005.07.08 by Melchiorre Caruso;
    v1.0.1 build 9158 - 2005.07.24 by Melchiorre Caruso;
    v1.0.2 build 0276 - 2006.01.05 by Melchiorre Caruso;

    v1.0.3 build 0312 - 2007.01.25 by Melchiorre Caruso.
}

unit BeeFm_SelectFrm;

{$I compiler.inc}

interface

uses
  Forms,
  Buttons,
  Classes,
  StdCtrls,
  Controls,
  Sysutils,
  ComCtrls,
  LResources,

  BeeGui_ArchiveListViewMgr;

type
  TSelectFrm = class (TForm)
    SelectFrm_Mask_: TLabel;
    SelectFrm_Mask: TEdit;
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    procedure FormShow(Sender: TObject);
  private    
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  BeeGui_Consts,
  BeeGui_Messages;

  procedure TSelectFrm.FormShow  (Sender: TObject);
  begin
    SelectFrm_Mask.SetFocus;
  end;

initialization

  {$i beefm_selectfrm.lrs}

end.
