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

    TAddTreeView class.

  Modifyed:

}

unit BeeGui_AddTreeViewMgr;

{$I compiler.inc}

interface

uses
  Math,
  Dialogs,
  Classes,
  SysUtils,
  StdCtrls,
  ComCtrls,
  LResources;
  
type

  { TAddTreeView }

  TAddTreeViewMgr = class (TComponent)
  private
    FRoot: TEdit;
    FRootValue: string;
    FSpin: integer;
    FTree: TTreeView;
    FFilePath: TStringList;
    FFileName: TStringList;
    FFileMask: TStringList;
    FFileBool: TStringList;
    procedure SetRoot(Value: TEdit);
    procedure SetSpin(Value: integer);
    procedure SetTree(Value: TTreeView);
    function GetCount: integer;
    function GetSel(Index: integer): boolean;
    function GetBool(Index: integer): boolean;
    function GetItem(Index: integer): string;
    function GetRootValue(SpinValue: integer): string;
    function And4Str(const Str1, Str2: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update;
  public
    function AddFile(const FileName: string): integer;
    function AddFolder(const FolderName: string): integer;
    procedure DeleteFile(FileIndex: integer);
    procedure DeleteFolder(FolderIndex: integer);
    procedure PlusMinus(Index: integer);
    procedure ChangeMask(Index: integer; const NewMask: string);
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: string read GetItem;
    property Selected[Index: integer]: boolean read GetSel;
    property Excluded[Index: integer]: boolean read GetBool;
  published
    property Tree: TTreeView read FTree write SetTree default nil;
    property Spin: integer read FSpin write SetSpin default 0;
    property Root: TEdit read FRoot write SetRoot default nil;
    property RootValue: string read FRootValue;
  end;

  { Register }

  procedure Register;

implementation

uses
  Bee_Common;

  { TAddTreeView }

  constructor TAddTreeViewMgr.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FRootValue := '';
    
    FFilePath := TStringList.Create;
    FFileName := TStringList.Create;
    FFileMask := TStringList.Create;
    FFileBool := TStringList.Create;
  end;
  
  destructor TAddTreeViewMgr.Destroy;
  begin
    FTree := nil;
    FRoot := nil;
    // ---
    FFilePath.Free;
    FFileName.Free;
    FFileMask.Free;
    FFileBool.Free;
    inherited Destroy;
  end;

  function TAddTreeViewMgr.And4Str(const Str1, Str2: string): string;
  var
    j : integer;
  begin
    Result := '';
    for j := 1 to Min(Length(Str1), Length(Str2)) do
      if CompareFileName(Str1[j], Str2[j]) = 0 then
      begin
        Result := Result + Str1[j]
      end else
      begin
        if CompareFileName(IncludeTrailingBackSlash(Result), Result) <> 0 then
        begin
          Result := ExtractFilePath(Result);
        end;
        Exit;
      end;
  end;
  
  function TAddTreeViewMgr.GetCount: integer;
  begin
    Result := FFilePath.Count;
  end;

  function TAddTreeViewMgr.GetRootValue(SpinValue: integer): string;
  var
    I: integer;
    S: TStringList;
  begin
    Result := '';
    S := TStringList.Create;
    for I := 0 to FFilePath.Count -1 do
    begin
      if FFileBool.Strings[I] = '+' then
      begin
        S.Add(FFilePath.Strings[I]);
      end;
    end;

    if S.Count > 0 then
    begin
      Result := S.Strings [0];
      for I := 1 to S.Count - 1 do
        Result := And4Str(Result, S.Strings[I]);
    end;

    if SpinValue > 0 then
      for I := 1 to SpinValue do
      begin
        Result := ExtractFilePath(ExcludeTrailingBackslash(Result));
        if Length(Result) > 0 then
          Result := IncludeTrailingBackslash(Result);
      end
    else
      if Length(Result) > 0 then
      begin
        Result := IncludeTrailingBackslash(Result);
      end;
    S.Free;
  end;
  
  function TAddTreeViewMgr.GetItem(Index: integer): string;
  var
    R: string;
  begin
    if (Index > -1) and (Index < FFilePath.Count) then
    begin
      Result :=
        FFilePath.Strings[Index] +
        FFileName.Strings[Index] +
        FFileMask.Strings[Index];

      R := IncludeTrailingBackSlash(FRootValue);
      if FileNamePos(R, Result) = 1 then
        Delete(Result, 1, Length(R));
    end else
      Result := '';
  end;

  procedure TAddTreeViewMgr.Update;
  var
    I, DirPos: integer;
    R, F: string;
  begin
    if FFilePath.Count = 0 then
    begin
      Spin := 0;
    end;
    
    R := GetRootValue(Spin);

    for I := FFilePath.Count - 1 downto 0 do
    begin
      F := FFilePath.Strings[I]
         + FFileName.Strings[I]
         + FFileMask.Strings[I];

      DirPos := Pos(R, F);
      if DirPos = 1  then
      begin
        System.Delete(F, DirPos, Length(R));
      end;
      if Assigned(FTree) then
      begin
        FTree.Items[I].Text := F;
        if FFileBool.Strings[I] = '+' then
        begin
          FTree.Items[I].SelectedIndex := 0;
          FTree.Items[I].ImageIndex := 0;
          FTree.Items[I].StateIndex := 0;
        end else
        begin
          FTree.Items[I].SelectedIndex := 1;
          FTree.Items[I].ImageIndex := 1;
          FTree.Items[I].StateIndex := 1;
        end;
      end;
    end;

    FRootValue := R;
    if Assigned (FRoot) then
    begin
      FRoot.Text := R;
    end;
  end;

  function TAddTreeViewMgr.AddFile(const FileName: string): integer;
  var
    Node: TTreeNode;
  begin
    Result := -1;
    if Assigned(FTree) then
    begin
      FTree.Items.BeginUpdate;

      FFilePath.Add(ExtractFilePath(FileName));
      FFileName.Add('');
      FFileMask.Add(ExtractFileName(FileName));
      FFileBool.Add('+');

      Node := FTree.Items.Add(FTree.Items.GetLastNode, 'nil');
      Result := Node.AbsoluteIndex;
      
      FTree.Items.EndUpdate;
      Update;
    end;
  end;
  
  function TAddTreeViewMgr.AddFolder(const FolderName: string): integer;
  var
    Node: TTreeNode;
  begin
    Result := -1;
    if Assigned(FTree) then
    begin
      FTree.Items.BeginUpdate;

      FFilePath.Add(ExtractFilePath(FolderName));
      if Length(ExtractFileName(FolderName)) = 0 then
        FFileName.Add('')
      else
        FFileName.Add(IncludeTrailingBackslash(ExtractFileName(FolderName)));
      FFileMask.Add('*');
      FFileBool.Add('+');

      Node := FTree.Items.Add(FTree.Items.GetLastNode, 'nil');
      Result := Node.AbsoluteIndex;

      FTree.Items.EndUpdate;
      Update;
    end;
  end;
  
  procedure TAddTreeViewMgr.DeleteFile(FileIndex: integer);
  begin
    if (FileIndex > -1) and (FileIndex < FFilePath.Count) then
    begin
      FFilePath.Delete(FileIndex);
      FFileName.Delete(FileIndex);
      FFileMask.Delete(FileIndex);
      FFileBool.Delete(FileIndex);
      if Assigned(FTree) then
      begin
        FTree.Items[FileIndex].Delete;
        Update;
      end;
    end;
  end;
  
  procedure TAddTreeViewMgr.DeleteFolder(FolderIndex: integer);
  begin
    if (FolderIndex > -1) and (FolderIndex < FFilePath.Count) then
    begin
      FFilePath.Delete(FolderIndex);
      FFileName.Delete(FolderIndex);
      FFileMask.Delete(FolderIndex);
      FFileBool.Delete(FolderIndex);
      if Assigned(FTree) then
      begin
        FTree.Items[FolderIndex].Delete;
        Update;
      end;
    end;
  end;
  
  procedure TAddTreeViewMgr.PlusMinus(Index: integer);
  begin
    if (Index > -1) and (Index < FFilePath.Count) then
    begin
      if FFileBool.Strings[Index] = '+' then
        FFileBool.Strings[Index] := '-'
      else
        FFileBool.Strings[Index] := '+';
      Update;
    end;
  end;
  
  procedure TAddTreeViewMgr.ChangeMask(Index: integer; const NewMask: string);
  begin
    if (Index > -1) and (Index < FFilePath.Count) then
    begin
      FFileMask.Strings[Index] := NewMask;
    end;
  end;
  
  function TAddTreeViewMgr.GetSel(Index: integer): boolean;
  begin
    if Assigned(FTree) and (Index > -1) and (Index < FFilePath.Count) then
    begin
      Result := FTree.Items[Index].Selected;
    end else
      Result := False;
  end;
  
  function TAddTreeViewMgr.GetBool(Index: integer): boolean;
  begin
    if (Index > -1) and (Index < FFilePath.Count) then
    begin
      if FFileBool.Strings[Index] = '+' then
        Result := True
      else
        Result := False;
    end else
      Result := False;
  end;
  
  procedure TAddTreeViewMgr.SetRoot(Value: TEdit);
  begin
    FRoot := Value;
  end;

  procedure TAddTreeViewMgr.SetSpin(Value: integer);
  begin
    if (Value > -1) and (CompareFileName(GetRootValue(Value), GetRootValue(FSpin)) <> 0) then
    begin
      FSpin := Value;
      Update;
    end;
  end;
  
  procedure TAddTreeViewMgr.SetTree(Value: TTreeView);
  begin
    FTree := Value;
  end;

  { Register }

  procedure Register;
  begin
    RegisterComponents ('BeePackage', [TAddTreeViewMgr]);
  end;
  
initialization

  {$I beegui_addtreeviewmgr.lrs }

end.
