{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

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

    TAddTreeView class.

    Modifyed:

    v1.0.3 build 0020 - 2006.11.25 by Melchiorre Caruso.
}

unit BeeGui_AddTreeView;

interface

uses
  Math,
  Classes,
  StdCtrls,
  SysUtils,
  ComCtrls,
  LResources;
  
const
  ATV_SELECTED = 0;
  ATV_EXCLUDED = 1;
  
type
  { TAddTreeView }

  TAddTreeView = class(TTreeView)
  private
    FRoot: TEdit;
    FSpin: integer;
    FFilePath: TStringList;
    FFileName: TStringList;
    FFileMask: TStringList;
    procedure SetRoot(Value: TEdit);
    procedure SetSpin(Value: integer);
    function AndString(const str1, str2: string): string;
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateItems;
    function GetNewRoot(NewSpin: integer): string;
    procedure AddFileName(const FileName: string);
    procedure AddDirName(const DirName: string);
    procedure DeleteItem(Index: integer);
    function GetFullName(Index: integer): string;
  public
    property FilePath: TStringList read FFilePath write FFilePath;
    property FileName: TStringList read FFileName write FFileName;
    property FileMask: TStringList read FFileMask write FFileMask;
  published
    property Root: TEdit read FRoot write SetRoot default nil;
    property Spin: integer read FSpin write SetSpin default 0;
  end;

  { Register }

  procedure Register;

implementation

  { TAddTreeView }

  constructor TAddTreeView.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    {$IFDEF MSWINDOWS}
      BorderWidth := 0;
    {$ELSE}
      BorderWidth := 2;
    {$ENDIF}
    FFilePath := TStringList.Create;
    FFileName := TStringList.Create;
    FFileMask := TStringList.Create;
  end;
  
  destructor TAddTreeView.Destroy;
  begin
    FFilePath.Free;
    FFileName.Free;
    FFileMask.Free;
    inherited Destroy;
  end;

  function TAddTreeView.AndString(const str1, str2: string): string;
  var
    J : integer;
  begin
    Result := '';
    for J := 1 to Min(Length(str1), Length(str2)) do
      if AnsiCompareFileName (str1[J], str2[J]) = 0 then
        Result := Result + str1[J]
      else
      begin
        if not (AnsiCompareFileName (IncludeTrailingBackslash(Result), Result) = 0) then
          Result := ExtractFilePath(Result);
        Exit;
      end;
  end;

  function TAddTreeView.GetNewRoot(NewSpin: integer): string;
  var
    I: integer;
    S: TStringList;
  begin
    Result := '';
    S := TStringList.Create;
    
    for I := 0 to Items.Count - 1 do
    begin
      if Items[I].ImageIndex = ATV_SELECTED then
      begin
        S.Add(FFilePath.Strings[I]);
      end;
    end;

    if S.Count > 0 then
    begin
      Result := S.Strings [0];
      for I := 1 to S.Count - 1 do
        Result := ANDstring(Result, S.Strings[I]);
    end;

    if NewSpin > 0 then
      for I := 1 to NewSpin do
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
  
  function TAddTreeView.GetFullName(Index: integer): string;
  begin
    if Index < Items.Count then
      Result := FFilePath.Strings[Index] + FFileName.Strings[Index] + FFileMask.Strings[Index]
    else
      Result := '';
  end;

  procedure TAddTreeView.UpdateItems;
  var
    I, DirPos: integer;
    R, F: string;
  begin
    if Items.Count = 0 then Spin := 0;

    R := GetNewRoot(Spin);

    for I := Items.Count -1 downto 0 do
    begin
      F := FFilePath.Strings[I] + FFileName.Strings[I] + FFileMask.Strings[I];

      DirPos := Pos(R, F);

      if DirPos = 1  then
        System.Delete(F, DirPos, Length(R));

      Items[I].Text := F;
    end;

    if Assigned (FRoot) then FRoot.Text := R;
  end;

  procedure TAddTreeView.AddFileName(const FileName: string);
  var
    Node: TTreeNode;
  begin
    Items.BeginUpdate;

    FFilePath.Add(ExtractFilePath(FileName));
    FFileName.Add('');
    FFileMask.Add(ExtractFileName(FileName));

    Node := Items.Add(Items.GetLastNode, ExtractFileName(FileName));
    Node.SelectedIndex := ATV_SELECTED;
    Node.ImageIndex := ATV_SELECTED;
    Node.StateIndex := ATV_SELECTED;

    Items.EndUpdate;
    UpdateItems;
  end;
  
  procedure TAddTreeView.DeleteItem(Index: integer);
  begin
    Items [Index].Delete;
    FFilePath.Delete(Index);
    FFileName.Delete(Index);
    FFileMask.Delete(Index);
  end;

  procedure TAddTreeView.AddDirName(const DirName: string);
  var
    Node: TTreeNode;
    NodeText: string;
  begin
    Items.BeginUpdate;

    FFilePath.Add(ExtractFilePath(DirName));
    if Length (ExtractFileName(DirName)) = 0 then
    begin
      NodeText := '';
      FFileName.Add('')
    end else
    begin
      NodeText := IncludeTrailingBackslash(ExtractFileName(DirName));
      FFileName.Add(IncludeTrailingBackslash(ExtractFileName(DirName)));
    end;
    NodeText := NodeText + '*';
    FFileMask.Add('*');

    Node := Items.Add (Items.GetLastNode, NodeText);
    Node.SelectedIndex := ATV_SELECTED;
    Node.ImageIndex := ATV_SELECTED;
    Node.StateIndex := ATV_SELECTED;

    Items.EndUpdate;
    UpdateItems;
  end;
  
  procedure TAddTreeView.SetRoot(Value: TEdit);
  begin
    FRoot := Value;
  end;

  procedure TAddTreeView.SetSpin(Value: integer);
  begin
    FSpin := Value;
  end;

  { Register }

  procedure Register;
  begin
    RegisterComponents ('BeeGui', [TAddTreeView]);
  end;
  
initialization

  {$I beegui_addtreeview.lrs }

end.
