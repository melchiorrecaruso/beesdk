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

    TArchiveListView class.

    Modifyed:

    v1.0.3 build 0020 - 2006/11/25 by Melchiorre Caruso.
}

unit BeeGui_ArchiveListView;

interface

uses
  Classes,
  StdCtrls,
  SysUtils,
  ComCtrls,
  // ---
  VirtualTrees,
  VirtualStringTree,
  // ---
  BeeGui_SysUtils,
  BeeGui_IconList,
  BeeGui_ArchiveTreeView;
  
type
  TArchiveListViewSort = (byName, byPath, byType,
    bySize, byPacked, byRatio, byAttr, byTime, byComm,
    byCrc, byMethod, byVersion, byPassword, byPosition, byIcon);

type
  TArchiveListView = class(TVirtualStringTree)
  private
    FCurrPath: string;
    FCurrFiles: TList;
    FListMode: boolean;
    FPathBox: TComboBox;
    FPathBoxSign: string;
    FDirectionSort: boolean;
    FArchiveFiles: TArchiveList;
    FArchiveFolders: TArchiveList;
    FColumnSort: TArchiveListViewSort;
    FArchiveFileDetails: TArchiveListDetails;
  private
    procedure SetPath;
    procedure SetPathBox(Value: TComboBox);
    procedure SetPathBoxSign(Value: string);
    procedure SetListMode(Value: boolean);
    procedure SetCurrPath(Value: string);
    // --
    procedure SetColumnSort(Value: TArchiveListViewSort);
    // ---
    function Compare(L: TList; Index1, Index2: integer): integer;
    procedure QuickSort(List: TList; L, R: Integer);
    function UpdateFArchiveFolders: boolean;
    // ---
    procedure Data(Sender: TObject; Item: TListItem);
    // ---
    procedure GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    // ---
    procedure GetText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
    var CellText: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // ---
    procedure Clear;
    function Up: boolean;
    procedure Initialize;
    property ArchiveFiles: TArchiveList read FArchiveFiles write FArchiveFiles default nil;
    property CurrFiles: TList read FCurrFiles write FCurrFiles default nil;
    property CurrDir: string read FCurrPath write SetCurrPath;
    property Info: TArchiveListDetails read FArchiveFileDetails;
  published
    property PathBox: TComboBox read FPathBox write SetPathBox default nil;
    property PathBoxSign: string read FPathBoxSign write SetPathBoxSign;
    // ---
    property ListMode: boolean read FListMode write SetListMode default False;
    // ---
    property ColumnSort: TArchiveListViewSort read FColumnSort write SetColumnSort default byName;
    property DirectionSort: boolean read FDirectionSort write FDirectionSort default True;
  end;
  
  { Register }

  procedure Register;

implementation

  constructor TArchiveListView.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    // ---
    FCurrPath := '';
    FListMode := True;
    FColumnSort := byName;
    FDirectionSort := True;
    FCurrFiles := TList.Create;
    FArchiveFiles := TArchiveList.Create;
    FArchiveFolders := TArchiveList.Create;
    FArchiveFileDetails := TArchiveListDetails.Create;
    // ---

    OnGetImageIndex := GetImageIndex;
    OnGetText := GetText;
  end;
  
  destructor TArchiveListView.Destroy;
  begin
    FCurrPath := '';
    FCurrFiles.Destroy;
    FArchiveFiles.Destroy;
    FArchiveFolders.Destroy;
    FArchiveFileDetails.Destroy;
    // ---
    inherited Destroy;
  end;
  
  function TArchiveListView.Compare(L: TList; Index1, Index2: integer): integer;
  var
    bool1, bool2 : boolean;
  begin
    with TArchiveListItem (L.Items [Index1]) do bool1 := not ((faDirectory and FileAttr) = faDirectory);
    with TArchiveListItem (L.Items [Index2]) do bool2 := not ((faDirectory and FileAttr) = faDirectory);

    if bool1 xor bool2 then
    begin
      if bool1 then
        Result := 1
      else
        Result := -1
    end else
    begin
      case FColumnSort of
        byName:     Result := AnsiCompareFileName (TArchiveListItem (L.Items [Index1]).FileName     , TArchiveListItem (L.Items [Index2]).FileName);
        byPath:     Result := AnsiCompareFileName (TArchiveListItem (L.Items [Index1]).FilePath     , TArchiveListItem (L.Items [Index2]).FilePath);
        byType:     Result := AnsiCompareText     (TArchiveListItem (L.Items [Index1]).FileType     , TArchiveListItem (L.Items [Index2]).FileType);
        bySize:     Result :=                      TArchiveListItem (L.Items [Index1]).FileSize     - TArchiveListItem (L.Items [Index2]).FileSize;
        byPacked:   Result :=                      TArchiveListItem (L.Items [Index1]).FilePacked   - TArchiveListItem (L.Items [Index2]).FilePacked;
        byRatio:    Result :=                      TArchiveListItem (L.Items [Index1]).FileRatio    - TArchiveListItem (L.Items [Index2]).FileRatio;
        byAttr:     Result :=                      TArchiveListItem (L.Items [Index1]).FileAttr     - TArchiveListItem (L.Items [Index2]).FileAttr;
        byTime:     Result := Round               (TArchiveListItem (L.Items [Index1]).FileTime     - TArchiveListItem (L.Items [Index2]).FileTime);
        byComm:     Result := AnsiCompareText     (TArchiveListItem (L.Items [Index1]).FileComm     , TArchiveListItem (L.Items [Index2]).FileComm);
        byCRC:      Result :=                      TArchiveListItem (L.Items [Index1]).FileCRC      - TArchiveListItem (L.Items [Index2]).FileCRC;
        byMethod:   Result := AnsiCompareText     (TArchiveListItem (L.Items [Index1]).FileMethod   , TArchiveListItem (L.Items [Index2]).FileMethod);
        byVersion:  Result := AnsiCompareText     (TArchiveListItem (L.Items [Index1]).FileVersion  , TArchiveListItem (L.Items [Index2]).FileVersion);
        byPassword: Result := AnsiCompareText     (TArchiveListItem (L.Items [Index1]).FilePassword , TArchiveListItem (L.Items [Index2]).FilePassword);
        byPosition: Result :=                      TArchiveListItem (L.Items [Index1]).FilePosition - TArchiveListItem (L.Items [Index2]).FilePosition;
       else
        Result := Index2 - Index1;
      end;
    end;

    if not FDirectionSort then
      if Result > 0 then
        Result := -1
      else
        if Result < 0 then
          Result := 1;
  end;
  
  procedure TArchiveListView.QuickSort;
  var
    I,J, Pivot : Longint;
  begin
    BeginUpdate;
    RootNodeCount := 0;
    if (R > L) then
    begin
      repeat
        I := L;
        J := R;
        Pivot := (L + R) div 2;
        repeat
          while Compare(List, I, Pivot) < 0 do Inc (I);
          while Compare(List, J, Pivot) > 0 do Dec (J);
          if I <= J then
          begin
            List.Exchange (I, J);
            if Pivot = I then
              Pivot := J
            else
              if Pivot = J then
                Pivot := I;
          Inc (I);
          Dec (j);
          end;
      until I > J;
      if L < J then QuickSort (List, L, J);
      L := I;
      until I >= R;
    end;

    RootNodeCount := FCurrFiles.Count;
    // if not (FCurrFiles.Count = 0) then ItemFocused := Items [0];
    EndUpdate;
  end;
  
  function TArchiveListView.UpdateFArchiveFolders: boolean;
  var
    I: integer;
    d: string;
    n: string;
    Node: TArchiveListItem;
  begin
    FArchiveFolders.Clear;
    FArchiveFileDetails.Clear;
    for I := 0 to FArchiveFiles.Count - 1 do
    begin
      n := TArchiveListItem (FArchiveFiles.Items [I]).FileName;
      d := TArchiveListItem (FArchiveFiles.Items [I]).FilePath;
      while not (Length (d) = 0) do
      begin
        if FArchiveFolders.IndexOf (d, n) = -1 then
        begin
          Node := TArchiveListItem.Create;
          Node.FileName := ExtractFileName (ExcludeTrailingBackslash (d));
          Node.FilePath := ExtractFilePath (ExcludeTrailingBackslash (d));

          Node.FilePacked := 0;
          Node.FileSize := 0;
          Node.FileRatio := 0;
          Node.FileTime := DateTimeToFileDate (Now);
          Node.FileAttr := faDirectory;
          Node.FileCRC := 0;
          Node.FilePosition := -1;

          FArchiveFolders.Add (Node);
        end;
        d := ExtractFilePath (ExtractFileDir (d));
      end;
    end;
    Result := True;
    // FArchiveFolders.IndexOfDir(FCurrPath) <> -1;
  end;
  
  procedure TArchiveListView.Data(Sender: TObject; Item: TListItem);
  begin
    if Item = nil then Exit;
    with TArchiveListItem(FCurrFiles.Items[Item.Index]) do
    begin
      Item.Caption := FileName;
      Item.SubItems.Add (FileType);
      if (FileAttr and faDirectory) = 0 then
      begin
        Item.SubItems.Add (SizeToStr (FilePacked));
        Item.SubItems.Add (SizeToStr (FileSize));
        Item.SubItems.Add (RatioToStr (FilePacked, FileSize));
        Item.SubItems.Add (FileMethod);
        Item.SubItems.Add (DateTimeToStr (FileDateToDateTime (FileTime)));
        Item.SubItems.Add (AttrToStr (FileAttr));
        Item.SubItems.Add (FilePassword);
        Item.SubItems.Add (IntToStr (FileCRC));
        Item.SubItems.Add (FilePath);
        Item.SubItems.Add (IntToStr (FilePosition));
      end else
      begin
        Item.SubItems.Add ('');
        Item.SubItems.Add ('');
        Item.SubItems.Add ('');
        Item.SubItems.Add ('');
        Item.SubItems.Add ('');
        Item.SubItems.Add (AttrToStr (FileAttr));
        Item.SubItems.Add ('');
        Item.SubItems.Add ('');
        Item.SubItems.Add (FilePath);
        Item.SubItems.Add ('');
      end;

    end;
  end;

  procedure TArchiveListView.SetPath;
  var
    I, J: integer;
  begin
    FCurrFiles.Clear;
    if fListMode then
    begin
      for I := 0 to FArchiveFiles.Count -1 do
        FCurrFiles.Add (FArchiveFiles.Items [I]);
    end else
    begin
      for I := 0 to FArchiveFolders.Count -1 do
        if AnsiCompareFileName(FCurrPath , TArchiveListItem(FArchiveFolders.Items [I]).FilePath) = 0 then
          FCurrFiles.Add (FArchiveFolders.Items [I]);

      for I := 0 to FArchiveFiles.Count -1 do
        if AnsiCompareFileName(FCurrPath , TArchiveListItem(FArchiveFiles.Items [I]).FilePath) = 0 then
          FCurrFiles.Add (FArchiveFiles.Items [I]);

      if (not (Length (FPathBoxSign)  = 0)) and Assigned (FPathBox) then
      begin
        J := -1;
        for I := 0 to FPathBox.Items.Count -1 do
          if CompareText (FPathBoxSign + FCurrPath, FPathBox.Items [I]) = 0 then
          begin
            J := I;
            Break;
          end;

        if (J = -1) then
          FPathBox.ItemIndex := FPathBox.Items.Add (FPathBoxSign + FCurrPath)
        else
          FPathBox.ItemIndex := J;
      end;
    end;

    QuickSort (FCurrFiles, 0, FCurrFiles.Count -1);
  end;
  
  procedure TArchiveListView.SetPathBox;
  begin
    FPathBox := Value;
  end;
  
  procedure TArchiveListView.SetPathBoxSign;
  begin
    FPathBoxSign := Value;
  end;
  
  procedure TArchiveListView.SetListMode;
  begin
    FListMode := Value;
    SetPath;
  end;
  
  procedure TArchiveListView.SetColumnSort;
  begin
    if Enabled then
    begin
      if Value = FColumnSort then
        FDirectionSort := not FDirectionSort
      else
        FDirectionSort := True;
    end;

    FColumnSort := Value;
    QuickSort (FCurrFiles, 0, FCurrFiles.Count -1);
  end;
  
  procedure TArchiveListView.SetCurrPath(Value: string);
  begin
    if Pos (FPathBoxSign, Value) = 1 then
    begin
      System.Delete (Value, 1, Length (FPathBoxSign));
    end;
    FCurrPath :=  (Value);
    SetPath;
  end;
  
  procedure TArchiveListView.Clear;
  begin
    FCurrPath := '';
    RootNodeCount := 0;
    FArchiveFiles.Clear;
    FArchiveFolders.Clear;
    FCurrFiles.Clear;
  end;
  
  function TArchiveListView.Up: boolean;
  var
    d: string;
  begin
    Result := True;
    d := ExtractFilePath (ExtractFileDir (FCurrPath));
    if not (CompareText (FCurrPath, d) = 0) then
      SetCurrPath (d)
    else
      Result := False;
  end;
  
  procedure TArchiveListView.Initialize;
  begin
    if UpdateFArchiveFolders  then
      SetCurrPath(FCurrPath)
    else
      SetCurrPath('');
  end;
  
  procedure TArchiveListView.GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Node = nil then Exit;

    with TArchiveListItem(FCurrFiles.Items[Node.Index]) do
    begin
      ImageIndex := TIconList(Images).GetIconIndex(ExtractFileExt(FileName));
    end;
  end;
  
  procedure TArchiveListView.GetText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
    var CellText: WideString);
  begin
    CellText := TArchiveListItem(FCurrFiles.Items[Node.Index]).FileName;
  end;

  { Register }

  procedure Register;
  begin
    RegisterComponents('BeeGui', [TArchiveListView]);
  end;

end.

