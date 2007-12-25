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

    TArchiveListView class.

    Modifyed:

    v1.0.3 build 0020 - 2007.11.25 by Melchiorre Caruso.
}

unit BeeGui_ArchiveListView;

interface

uses
  Math,
  Classes,
  Dialogs,
  Windows,
  ExtCtrls,
  StdCtrls,
  SysUtils,
  Controls,
  ComCtrls,
  LResources,
  // ---
  Bee_Common,
  BeeGui_IconList,
  BeeGui_ArchiveTreeView;

  { TArchiveListColumns }
  
type
  TArchiveListColumns = (csName, csSize, csPacked, csRatio, csType, csTime,
    csAttr, csMethod, csPassword, csCrc, csPath, csPosition);

  { TArchiveListManager }
  
type
  TArchiveListView = class(TListView)
  private
    FCurrFiles: TList;
    FCurrFolder: string;
    FFiles: TArchiveList;
    FFolders: TArchiveList;

    FListMode: boolean;
    FDirectionSort: boolean;
    FColumnSort: TArchiveListColumns;

    FFolderBox: TComboBox;
    FFolderBoxSign: string;

    FArchiveDetails: TArchiveListDetails;
  private
    procedure QuickSort(List: TList; L, R: integer);
    function CompareFn(L: TList; I1, I2: integer): integer;

    procedure UpdateArchiveDetails(Item: TArchiveListItem);
    function InitFolders: boolean;
    procedure InitColumns;
  private
    procedure SetFolder;
    procedure SetCurrFolder(Value: string);
    procedure SetFolderBox(Value: TComboBox);
    procedure SetFolderBoxSign(Value: string);
    procedure SetColumnSort(Value: TArchiveListColumns);
    procedure SetListMode(Value: boolean);
    procedure GetImageIndex(Sender: TObject; Item: TListItem);
    procedure Data(Sender: TObject; Item: TListItem);
  public
    constructor Create(Sender: TComponent);
    destructor Destroy; override;
    function Up: boolean;
    procedure Clear;
    procedure Init;

    property Files: TArchiveList Read FFiles Write FFiles default nil;
    property CurrFiles: TList Read FCurrFiles Write FCurrFiles default nil;
    property CurrFolder: string Read fCurrFolder Write SetCurrFolder;
    property Details: TArchiveListDetails Read FArchiveDetails;
  published
    property FolderBox: TComboBox Read FFolderBox Write SetFolderBox default nil;
    property FolderBoxSign: string Read fFolderBoxSign Write SetFolderBoxSign;
    property ColumnSort: TArchiveListColumns Read FColumnSort Write SetColumnSort default csName;
    property DirectionSort: boolean Read FDirectionSort Write FDirectionSort default True;
    property ListMode: boolean Read FListMode Write SetListMode default False;
  end;
  
 { Register }
  
  procedure Register;

implementation

uses
  Graphics,
  // ---
  BeeGui_SysUtils;

// TArchiveListView

constructor TArchiveListView.Create(Sender: TComponent);
var
  i: integer;
  Col: TListColumn;
begin
  inherited Create(Sender);

  TabOrder := 0;
  Enabled := False;
  MultiSelect := True;
  Color  := clInactiveBorder;
  // --
  OwnerDraw := False;
  OwnerData := True;
  // --
  SmallImages := TIconList.Create(Sender);
  LargeImages := TIconList.Create(Sender);
  // --
  InitColumns;


  FArchiveDetails := TArchiveListDetails.Create;
  // --    
  FFolders := TArchiveList.Create;
  FFiles := TArchiveList.Create;
  FCurrFiles := TList.Create;
  // --
  FDirectionSort := True;
  FColumnSort := csName;
  FCurrFolder := '';
  FListMode := False;
end;

destructor TArchiveListView.Destroy;
begin
  FArchiveDetails.Free;
  FCurrFiles.Free;
  FFiles.Free;

  FCurrFolder := '';
  FFolders.Free;

  SmallImages.Free;
  LargeImages.Free;

  inherited Destroy;
end;

function TArchiveListView.CompareFn(L: TList; I1, I2: integer): integer;
var
  bool1, bool2: boolean;
begin
  with TArchiveListItem(L.Items[I1]) do
    bool1 := not ((faDirectory and FileAttr) = faDirectory);
  with TArchiveListItem(L.Items[I2]) do
    bool2 := not ((faDirectory and FileAttr) = faDirectory);

  if bool1 xor bool2 then
  begin
    if bool1 then
      Result := 1
    else
      Result := -1;
  end else
  begin
    case FColumnSort of
      csName: Result :=
          AnsiCompareFileName(TArchiveListItem(L.Items[I1]).FileName,
            TArchiveListItem(L.Items[I2]).FileName);
      csPath: Result :=
          AnsiCompareFileName(TArchiveListItem(L.Items[I1]).FilePath,
            TArchiveListItem(L.Items[I2]).FilePath);
      csType: Result :=
          AnsiCompareFileName(TArchiveListItem(L.Items[I1]).FileType,
            TArchiveListItem(L.Items[I2]).FileType);
      csSize: Result :=
          (TArchiveListItem(L.Items[I1]).FileSize -
            TArchiveListItem(L.Items[I2]).FileSize);
      csPacked: Result :=
          (TArchiveListItem(L.Items[I1]).FilePacked -
            TArchiveListItem(L.Items[I2]).FilePacked);
      csRatio: Result :=
          (TArchiveListItem(L.Items[I1]).FileRatio -
            TArchiveListItem(L.Items[I2]).FileRatio);
      csAttr: Result :=
          (TArchiveListItem(L.Items[I1]).FileAttr -
            TArchiveListItem(L.Items[I2]).FileAttr);
      csTime: Result :=
          Round(TArchiveListItem(L.Items[I1]).FileTime -
            TArchiveListItem(L.Items[I2]).FileTime);
      csCRC: Result :=
          (TArchiveListItem(L.Items[I1]).FileCRC -
            TArchiveListItem(L.Items[I2]).FileCRC);
      csMethod: Result :=
          AnsiCompareText(TArchiveListItem(L.Items[I1]).FileMethod,
            TArchiveListItem(L.Items[I2]).FileMethod);
      csPassword: Result :=
          AnsiCompareText(TArchiveListItem(L.Items[I1]).FilePassword,
            TArchiveListItem(L.Items[I2]).FilePassword);
      csPosition: Result :=
          (TArchiveListItem(L.Items[I1]).FilePosition -
            TArchiveListItem(L.Items[I2]).FilePosition);
      else
        Result := I2 - I1;
    end;
  end;

  if not FDirectionSort then
    if Result > 0 then
      Result := -1
    else
    if Result < 0 then
      Result := 1;
end;

procedure TArchiveListView.QuickSort(List: TList; L, R: integer);
var
  I, J, Pivot: longint;
begin
  if (R > L) then
  begin
    repeat
      I := L;
      J := R;
      Pivot := (L + R) div 2;
      repeat
        while CompareFn(List, I, Pivot) < 0 do
          Inc(I);
        while CompareFn(List, J, Pivot) > 0 do
          Dec(J);

        if I <= J then
        begin
          List.Exchange(I, J);
          if Pivot = I then
            Pivot := J
          else
          if Pivot = J then
            Pivot := I;
          Inc(I);
          Dec(j);
        end;
      until I > J;

      if L < J then
        QuickSort(List, L, J);
      L := I;
    until I >= R;
  end;
end;

function TArchiveListView.InitFolders: boolean;
var
  D: string;
  I: integer;
  Node: TArchiveListItem;
begin
  FFolders.Clear;
  FArchiveDetails.Clear;
  for I := 0 to FFiles.Count - 1 do
  begin
    UpdateArchiveDetails(TArchiveListItem(FFiles.Items[I]));
    D := ExcludeTrailingBackslash(TArchiveListItem(FFiles.Items[I]).FilePath);
    while (Length(D) > 0) do
    begin
      if FFolders.IndexOf(ExtractFilePath(D), ExtractFileName(D)) = -1 then
      begin
        Node := TArchiveListItem.Create;
        Node.FileName := ExtractFileName(D);
        Node.FilePath := ExtractFilePath(D);
        Node.FilePacked := 0;
        Node.FileSize := 0;
        Node.FileRatio := 0;
        Node.FileTime := DateTimeToFileDate(Now);
        Node.FileAttr := faDirectory;
        Node.FileCRC := 0;
        Node.FilePosition := -1;

        FFolders.Add(Node);
        UpdateArchiveDetails(Node);
      end;
      D := ExtractFileDir(D);
    end;
  end;
  D := ExcludeTrailingBackSlash(fCurrFolder);
  Result := FFolders.IndexOf(ExtractFilePath(D), ExtractFileName(D)) <> -1;
end;

procedure TArchiveListView.InitColumns;

  procedure AddCol(const aCaption: string; aWidth: integer; aAlign: TAlignment);
  var
    Col: TListColumn;
  begin
    Col := Columns.Add;
    Col.Caption := aCaption;
    Col.AutoSize := False;
    Col.Alignment := aAlign;
    Col.Width := aWidth;
  end;

begin
  Columns.Clear;
  // ---
  AddCol('Name', 150, taLeftJustify);
  AddCol('Size', 70, taRightJustify);
  AddCol('Packed', 70, taRightJustify);
  AddCol('Ratio', 50, taRightJustify);
  AddCol('Type', 100, taLeftJustify);
  AddCol('Modified', 100, taLeftJustify);
  AddCol('Attribute', 55, taRightJustify);
  AddCol('Method', 50, taRightJustify);
  AddCol('Password', 60, taRightJustify);
  AddCol('Crc', 50, taRightJustify);
  AddCol('Path', 50, taLeftJustify);
  AddCol('Position', 50, taRightJustify);
end;

procedure TArchiveListView.UpdateArchiveDetails(Item: TArchiveListItem);
begin
  FArchiveDetails.Update(Item);
end;

procedure TArchiveListView.SetFolderBox(Value: TComboBox);
begin
  FFolderBox := Value;
end;

procedure TArchiveListView.SetFolderBoxSign(Value: string);
begin
  FFolderBoxSign := Value;
end;

procedure TArchiveListView.SetColumnSort(Value: TArchiveListColumns);
begin
  if Enabled then
  begin
    if Value = FColumnSort then
      FDirectionSort := not FDirectionSort
    else
      FDirectionSort := True;
  end;
  FColumnSort := Value;
  SetFolder;
end;

procedure TArchiveListView.SetListMode(Value: boolean);
begin
  FListMode := Value;
  SetFolder;
end;

procedure TArchiveListView.SetFolder;
var
  I, J: integer;
begin
  FCurrFiles.Clear;
  if FListMode then
  begin
    for I := 0 to FFiles.Count - 1 do
      FCurrFiles.Add(FFiles.Items[I]);
  end else
  begin
    for I := 0 to FFolders.Count - 1 do
      if AnsiCompareFileName(fCurrFolder, TArchiveListItem(
        FFolders.Items[I]).FilePath) = 0 then
      begin
        FCurrFiles.Add(FFolders.Items[I]);
      end;

    for I := 0 to FFiles.Count - 1 do
      if AnsiCompareFileName(fCurrFolder, TArchiveListItem(
        FFiles.Items[I]).FilePath) = 0 then
      begin
        FCurrFiles.Add(FFiles.Items[I]);
      end;

    if (not (Length(fFolderBoxSign) = 0)) and Assigned(FFolderBox) then
    begin
      J := -1;
      for I := 0 to FFolderBox.Items.Count - 1 do
        if AnsiCompareFileName(fFolderBoxSign + fCurrFolder,
          FFolderBox.Items[I]) = 0 then
        begin
          J := I;
          Break;
        end;

      if (J = -1) then
        FFolderBox.ItemIndex := FFolderBox.Items.Add(fFolderBoxSign + fCurrFolder)
      else
        FFolderBox.ItemIndex := J;
    end;
  end;
  QuickSort(FCurrFiles, 0, FCurrFiles.Count - 1);

  BeginUpdate;
  for I := 1 to FCurrFiles.Count do Items.Add;
  if (Items.Count > 0) then
  begin
    ItemFocused := Items[0];
  end;
  EndUpdate;
end;

procedure TArchiveListView.Init;
begin
  if InitFolders then
    SetCurrFolder(FCurrFolder)
  else
    SetCurrFolder('');

  if Enabled then
  begin
    SetFocus;
    OnClick(Self);
  end;

  if Assigned(FFolderBox) then
  begin
    // nothing to do!
  end;
end;

procedure TArchiveListView.Clear;
begin
  Items.Clear;
  Enabled := False;
  Color := clInactiveBorder;
  if Assigned(FFolderBox) then
  begin
    FFolderBox.Items.Clear;
    FFolderBox.Enabled := False;
    FFolderBox.Color := clInactiveBorder;
  end;
  SetLength(FCurrFolder, 0);
  FCurrFiles.Clear;
  FFolders.Clear;
  FFiles.Clear;
end;

procedure TArchiveListView.SetCurrFolder(Value: string);
begin
  if Pos(fFolderBoxSign, Value) = 1 then
  begin
    System.Delete(Value, 1, Length(fFolderBoxSign));
  end;
  fCurrFolder := Value;
  SetFolder;
end;

function TArchiveListView.Up: boolean;
var
  d: string;
begin
  Result := True;
  d := ExtractFilePath(ExcludeTrailingBackSlash(fCurrFolder));
  if not (CompareText(fCurrFolder, d) = 0) then
    SetCurrFolder(d)
  else
    Result := False;
end;

procedure TArchiveListView.GetImageIndex(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item) then
  begin
    Item.ImageIndex := TArchiveListItem(FCurrFiles.Items[Item.Index]).FileIconIndex;
  end;
end;

procedure TArchiveListView.Data(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item) then
  begin
    with TArchiveListItem(FCurrFiles.Items[Item.Index]) do
    begin
      Item.Caption := FileName;                                //  0 Name
      if (FileAttr and faDirectory) = 0 then
      begin
        Item.SubItems.Add(SizeToStr(FileSize));                //  1 Size
        Item.SubItems.Add(SizeToStr(FilePacked));              //  2 Packed
        Item.SubItems.Add(RatioToStr(FileRatio));              //  3 Ratio
        Item.SubItems.Add(FileType);                           //  4 Type

        Item.SubItems.Add(
          DateTimeToStr(FileDateToDateTime(FileTime)));     //  5 Time

        Item.SubItems.Add(AttrToStr(FileAttr));                //  6 Attr
        Item.SubItems.Add(FileMethod);                         //  7 Method
        Item.SubItems.Add(FilePassword);                       //  8 Password

        Item.SubItems.Add(IntToStr(FileCRC));
        // Item.SubItems.Add(HexToStr(FileCRC, SizeOf(FileCRC))); //  9 CRC


        Item.SubItems.Add(FilePath);                           // 10 Path
        Item.SubItems.Add(IntToStr(FilePosition));             // 11 Position
      end else
      begin
        Item.SubItems.Add('');                                 //  1 Size
        Item.SubItems.Add('');                                 //  2 Packed
        Item.SubItems.Add('');                                 //  3 Ratio
        Item.SubItems.Add(FileType);                           //  4 Type
        Item.SubItems.Add('');                                 //  5 Time
        Item.SubItems.Add(AttrToStr(FileAttr));                //  6 Attr
        Item.SubItems.Add('');                                 //  7 Method
        Item.SubItems.Add('');                                 //  8 Password
        Item.SubItems.Add('');                                 //  9 CRC
        Item.SubItems.Add(FilePath);                           // 10 Path
        Item.SubItems.Add('');                                 // 11 Position
      end;
    end;
  end;
end;

  { Register }

  procedure Register;
  begin
    RegisterComponents('BeeGui', [TArchiveListView]);
  end;

initialization

  {$I beegui_archivelistview.lrs }

end.
