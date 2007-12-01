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

unit BeeGui_AddListManager;

{$I Compiler.inc}

interface

uses
  Math,
  Dialogs,
  Classes,
  Controls,
  StdCtrls,
  SysUtils,
  ComCtrls;

type

  { TAddListItem }

  TAddListItem = class
    Path: string;
    Name: string;
    Mask: string;
    Icon: integer;
  end;

  { TAddListManager }

  TAddListManager = class
  private
    fRoot:  TEdit;
    fFiles: TList;
    fSpin:  integer;
    fList:  TListView;
    procedure UpdateList;
    function GetCount: integer;
    procedure SetRoot(Value: TEdit);
    procedure SetSpin(Value: integer);
    procedure SetList(Value: TListView);
    procedure SetPath(Index: integer; S: string);
    procedure SetName(Index: integer; S: string);
    procedure SetMask(Index: integer; S: string);
    procedure SetIcon(Index: integer; I: integer);
    function GetPath(Index: integer): string;
    function GetName(Index: integer): string;
    function GetMask(Index: integer): string;
    function GetIcon(Index: integer): integer;
    function GetSel(Index: integer): boolean;
    function GetRootText(Value: integer): string;
    function AndString(const S1, S2: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Name: string; const IsFolder: boolean);
    procedure Delete(Index: integer);
  public
    property Count: integer Read GetCount;
    property Root: TEdit Read fRoot Write SetRoot default nil;
    property Spin: integer Read fSpin Write SetSpin default 0;
    property List: TListView Read fList Write SetList default nil;
    property Paths[Index: integer]: string Read GetPath Write SetPath;
    property Names[Index: integer]: string Read GetName Write SetName;
    property Masks[Index: integer]: string Read GetMask Write SetMask;
    property Icons[Index: integer]: integer Read GetIcon Write SetIcon;
    property IsSel[Index: integer]: boolean Read GetSel;
  end;

implementation

uses
  BeeGui_SysUtils;

{ TAddListManager }

constructor TAddListManager.Create;
begin
  inherited Create;
  fFiles := TList.Create;
end;

destructor TAddListManager.Destroy;
var
  I: integer;
begin
  while fFiles.Count > 0 do
  begin
    TAddListItem(fFiles.Items[0]).Free;
    fFiles.Delete(0);
  end;
  fFiles.Free;
  inherited Destroy;
end;

function TAddListManager.AndString(const S1, S2: string): string;
var
  i, j: integer;
begin
  j := 0;
  for i := 1 to Min(Length(S1), Length(S2)) do
  begin
    if AnsiCompareFileName(S1[i], S2[i]) = 0 then
    begin
      Inc(j);
    end else
    begin
      Break;
    end;
  end;
  Result := ExtractFilePath(Copy(S1, 1, j));
end;

function TAddListManager.GetRootText(Value: integer): string;
var
  I: integer;
  S: TStringList;
begin
  S := TStringList.Create;
  for I := 0 to fFiles.Count - 1 do
  begin
    if TAddListItem(FFiles.Items[I]).Icon = 0 then
    begin
      S.Add(TAddListItem(fFiles.Items[I]).Path);
    end;
  end;
  // --
  if S.Count > 0 then
  begin
    Result := S.Strings[0];
    for i := 1 to S.Count - 1 do
    begin
      Result := AndString(Result, S.Strings[i]);
    end;
  end else
  begin
    Result := '';
  end;
  // --
  if Value > 0 then
  begin
    for I := 1 to Value do
    begin
      Result := ExtractFilePath(ExcludeTrailingBackslash(Result));
      if Length(Result) > 0 then
      begin
        Result := IncludeTrailingBackslash(Result);
      end;
    end;
  end else
  begin
    if Length(Result) > 0 then
    begin
      Result := IncludeTrailingBackslash(Result);
    end;
  end;
  // --
  S.Free;
end;

procedure TAddListManager.UpdateList;
var
  I: integer;
  DirPos: integer;
  RootText: string;
  ItemCaption: string;
begin
  if Assigned(fList) then
  begin
    fList.Items.BeginUpdate;
  end;
  if fFiles.Count = 0 then
  begin
    Spin := 0;
  end;
  RootText := GetRootText(Spin);
  for I := fFiles.Count - 1 downto 0 do
  begin
    ItemCaption := TAddListItem(fFiles.Items[I]).Path +
      TAddListItem(fFiles.Items[I]).Name +
      TAddListItem(fFiles.Items[I]).Mask;

    DirPos := Pos(RootText, ItemCaption);
    if DirPos = 1 then
    begin
      System.Delete(ItemCaption, DirPos, Length(RootText));
    end;
    if Assigned(fList) then
    begin
      fList.Items[I].Caption := ItemCaption;
      fList.Items[I].ImageIndex := TAddListItem(fFiles.Items[I]).Icon;
    end;
  end;
  if Assigned(fList) then
  begin
    fList.Items.EndUpdate;
  end;
  if Assigned(fRoot) then
  begin
    fRoot.Text := RootText;
  end;
end;

procedure TAddListManager.Add(const Name: string; const IsFolder: boolean);
var
  NewItem: TAddListItem;
begin
  NewItem := TAddListItem.Create;
  if IsFolder then
  begin
    NewItem.Path := ExtractFilePath(Name);
    NewItem.Name := AnsiIncludeTrailingBackslash(ExtractFileName(Name));
    NewItem.Mask := '*';
    NewItem.Icon := 0;
  end else
  begin
    NewItem.Path := ExtractFilePath(Name);
    NewItem.Name := '';
    NewItem.Mask := ExtractFileName(Name);
    NewItem.Icon := 0;
  end;
  fFiles.Add(NewItem);
  if Assigned(fList) then
  begin
    fList.Items.Add;
  end;
  UpdateList;
end;

procedure TAddListManager.Delete(Index: integer);
var
  i: integer;
begin
  fFiles.Delete(Index);
  if Assigned(fList) then
  begin
    fList.Items.Delete(Index);
  end;
  UpdateList;
end;

procedure TAddListManager.SetRoot(Value: TEdit);
begin
  if Assigned(Value) then
  begin
    fRoot := Value;
    UpdateList;
  end;
end;

procedure TAddListManager.SetSpin(Value: integer);
begin
  if AnsiCompareText(GetRootText(FSpin), GetRootText(Value)) <> 0 then
  begin
    fSpin := Value;
    UpdateList;
  end;
end;

procedure TAddListManager.SetList(Value: TListView);
begin
  if Assigned(Value) then
  begin
    fList := Value;
    UpdateList;
  end;
end;

function TAddListManager.GetPath(Index: integer): string;
begin
  Result := TAddListItem(fFiles.Items[Index]).Path;
end;

procedure TAddListManager.SetPath(Index: integer; S: string);
begin
  TAddListItem(fFiles.Items[Index]).Path := S;
  UpdateList;
end;

function TAddListManager.GetName(Index: integer): string;
begin
  Result := TAddListItem(fFiles.Items[Index]).Name;
end;

procedure TAddListManager.SetName(Index: integer; S: string);
begin
  TAddListItem(fFiles.Items[Index]).Name := S;
  UpdateList;
end;

function TAddListManager.GetMask(Index: integer): string;
begin
  Result := TAddListItem(fFiles.Items[Index]).Mask;
end;

procedure TAddListManager.SetMask(Index: integer; S: string);
begin
  TAddListItem(fFiles.Items[Index]).Mask := S;
  UpdateList;
end;

function TAddListManager.GetIcon(Index: integer): integer;
begin
  Result := TAddListItem(fFiles.Items[Index]).Icon;
end;

procedure TAddListManager.SetIcon(Index: integer; I: integer);
begin
  TAddListItem(fFiles.Items[Index]).Icon := I;
  UpdateList;
end;

function TAddListManager.GetSel(Index: integer): boolean;
begin
  if Assigned(fList) then
  begin
    Result := fList.Items[Index].Selected;
  end else
  begin
    Result := False;
  end;
end;

function TAddListManager.GetCount: integer;
begin
  Result := fFiles.Count;
end;

end.
