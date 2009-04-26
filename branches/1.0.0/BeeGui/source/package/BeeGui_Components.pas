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

    TBeeGui_List class
    TBeeGui_List_Info class
    TBeeGui_List_Node class

    TBeeGui_Label class
    TBeeGui_ListView class
    TBeeGui_TreeView class
    TBeeGui_ComboBox class
    TBeeGui_PathBox class

    TBeeGui_ListView_Add class

    Modifyed:

    v1.0.1 build 9154 - 2005.07.14 Melchiorre Caruso;
    v1.0.1 build 9160 - 2005.08.03 Melchiorre Caruso;
    v1.0.2 build 0250 - 2005.11.13 Melchiorre Caruso;

    v1.0.3 build 0310 - 2007.01.25 Melchiorre Caruso.
}

unit BeeGui_Components;

{$R-,Q-,S-}

interface

uses
  Math,
  Classes,
  Dialogs,
  StdCtrls,
  Messages,
  SysUtils,
  Graphics,
  Controls,
  ComCtrls,

  uDragFilesTrg,
  uDragFilesSrc;

const
  S_Index = 0;
  E_Index = 1;

type

  TBeeGui_ListView_Sort = (byName, byPath, byType, bySize, byPacked, byRatio, byAttr,
    byTime, byComm, byCrc, byMethod, byVersion, byPassword, byPosition, byIcon);

type

  TBeeGui_List_Node = class // see TAppListNode
  public
    FileName: string;
    FilePath: string;
    FileType: string;
    FileSize: integer;
    FilePacked: integer;
    FileRatio: integer;
    FileAttr: integer;
    FileTime: integer;
    FileComm: string;
    FileCrc: cardinal;
    FileMethod: string;
    FileVersion: string;
    FilePassword: string;
    FilePosition: integer;
    FileIcon: integer;
  end;

  TBeeGui_List = class (TList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IndexOfDir (const dir: string): integer;
  end;

  TBeeGui_List_Info = class
  public
    FilesCount: integer;
    FilesSize: integer;
    FilesPacked: integer;
    FilesCrypted: integer;
    DirectoriesCount: integer;
    Version: extended;
    LastTime: integer;
  public
    procedure Clear;
    procedure Update (Node: TBeeGui_List_Node);
  end;

  TBeeGui_ListView = class (TListView)
  private
    FCurrFiles: TList;
    FPathBox: TComboBox;
    FCurrPath: string;
    FFiles: TBeeGui_List;
    FDirs: TBeeGui_List;
    FColumnSort: TBeeGui_ListView_Sort;
    FListMode: boolean;
    FDirectionSort: boolean;
    FPathBoxSign: string;
    FInfo: TBeeGui_List_Info;
  private
    function CompareFN (L: TList; Index1, Index2: integer): integer;
    procedure QuickSort (List: TList; L, R: Integer);
    function LoadDirectories: boolean;
    procedure LoadIcon (Node: TBeeGui_List_Node);
    procedure SetPath;
  private
    procedure SetCurrPath (Value: string);
    procedure SetPathBox (Value: TComboBox);
    procedure SetPathBoxSign (Value: string);
    procedure SetColumnSort (Value: TBeeGui_ListView_Sort);
    procedure SetListMode (Value: boolean);
    // Event
    procedure GetImageIndex (Sender: TObject; Item: TListItem);
    procedure Data (Sender: TObject; Item: TListItem);
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Up: boolean;
    procedure Initialize;
    property Files: TBeeGui_List read FFiles write FFiles default nil;
    property CurrFiles: TList read FCurrFiles write FCurrFiles default nil;
    property CurrDir: string read FCurrPath write SetCurrPath;
    property Info: TBeeGui_List_Info read FInfo;
  published
    property PathBox: TComboBox read FPathBox write SetPathBox default nil;
    property PathBoxSign: string read FPathBoxSign write SetPathBoxSign;
    property ColumnSort: TBeeGui_ListView_Sort read FColumnSort write SetColumnSort default byName;
    property DirectionSort: boolean read FDirectionSort write FDirectionSort default True;
    property ListMode: boolean read FListMode write SetListMode default False;
  end;

  TBeeGui_TreeView = class (TTreeView)
  private
    FCurrPath:  string;
    FPathEdit:  TEdit;
  private
    function    SetPath: boolean;
    procedure   SetCurrPath (Value: string);
    procedure   SetEdit (Value: TEdit);
    //Event
    procedure   OnTreeExpanding (Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure   OnTreeDblClick (Sender: TObject);
    procedure   OnTreeClick (Sender: TObject);
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Initialize;
  published
    property    CurrDir: string read FCurrPath write SetCurrPath;
    property    PathEdit: TEdit read FPathEdit write SetEdit default nil;
  end;

  TBeeGui_ComboBox = class (TComboBox)
  private
    FMask:      string;
    procedure   SetMask (Value: string);
  public
    constructor Create (AOwner: TComponent); override;
  published
    property    Mask: string read FMask write SetMask;
  end;

  TBeeGui_PathBox = class (TComboBox)
    // empty
  end;

  TBeeGui_Label = class (TLabel)
    // empty
  end;

type
  TBeeGui_ListView_Add = class (TListView)
  private
    FRoot:    TEdit;
    FSpin:    integer;
    
    procedure SetRoot (Value: TEdit);
    procedure SetSpin (Value: integer);
    function  ANDstring (const str1, str2: string): string;
  public
    procedure UpdateItems;
    function  GetNewRoot (NewSpin: integer): string;
    procedure AddFile (const FileName: string);
    procedure AddDirectory (const DirName: string);
  published
    property  Root: TEdit read FRoot write SetRoot default nil;
    property  Spin: integer read FSpin write SetSpin default 0;
  end;

type
  TBeeGui_AppView = class (TThread)
  public
    cFileName:  string;
    cPathName:  string;
    cFileTime:  integer;
  public
    constructor Create (const FileName, PathName: string);
    procedure   Execute; override;
  end;

procedure Register;

implementation

uses
  BeeGui_WinOS,
  BeeGui_Common;

const
  MSG_SETDIR = 'Loading...';
  TreeConst = '\<Loading...>';

/// TBeeGui_List_Info

  procedure TBeeGui_List_Info.Clear;
  begin
    FilesCount := 0;
    FilesSize := 0;
    FilesPacked := 0;
    FilesCrypted := 0;
    DirectoriesCount := 0;
    Version := 0;
    LastTime := 0;
  end;

  procedure TBeeGui_List_Info.Update;
  begin
    if (Node.FileAttr and faDirectory) = 0 then
    begin
      Inc (FilesCount);
      Inc (FilesSize, Node.FileSize);
      Inc (FilesPacked, Node.FilePacked);

      if CompareText (Node.FilePassword, 'Yes') = 0 then Inc (FilesCrypted);

      LastTime := Max (LastTime, Node.FileTime);
      try
        Version := Max (Version, StrToCurr (Node.FileVersion));
      except
        Version := 0;
      end;
    end
    else
      Inc (DirectoriesCount)
  end;

/// TBeeGui_List 

  destructor TBeeGui_List.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;

  procedure TBeeGui_List.Clear;
  var
    I: integer;
  begin
    for I := Count - 1 downto 0 do
      TBeeGui_List_Node (Items [I]).Destroy;
    inherited Clear;
  end;

  function TBeeGui_List.IndexOfDir;
  var
    I: integer;
    Path, Name: string;
  begin
    Result := -1;

    Name := ExtractFileName (ExcludeTrailingBackslash (dir));
    Path := ExtractFilePath (ExcludeTrailingBackslash (dir));
    for I := 0 to Count - 1 do
      if CompareText (TBeeGui_List_Node (Items [I]).FilePath, Path) = 0 then
        if CompareText (TBeeGui_List_Node (Items [I]).FileName, Name) = 0 then
        begin
          Result := I;
          Break;
        end;
  end;

/// TBeeGui_ListView

  constructor TBeeGui_ListView.Create;
  begin
    inherited Create (AOwner);
    Enabled         := False;
    FCurrPath       := '';
    OwnerData       := True;
    OwnerDraw       := False;

    FCurrFiles      := TList.Create;
    FFiles          := TBeeGui_List.Create;
    FDirs           := TBeeGui_List.Create;
    FInfo           := TBeeGui_List_Info.Create;

    SmallImages     := TImageList.Create (nil);
    LargeImages     := TImageList.Create (nil);

    FColumnSort     := byName;
    FDirectionSort  := True;
    FListMode       := False;

    OnData          := Data;
    OnGetImageIndex := GetImageIndex;

    BeeGui_LoadListViewImages (TImageList (SmallImages), TImageList (LargeImages));
  end;

  destructor TBeeGui_ListView.Destroy;
  begin
    SetLength (FCurrPath, 0);
    FFiles.Free;
    FDirs.Free;
    FInfo.Free;

    FCurrFiles.Free;
    SmallImages.Free;
    LargeImages.Free;
    inherited Destroy;
  end;

  function TBeeGui_ListView.CompareFN;
  var
    bool1, bool2 : boolean;
  begin
    with TBeeGui_List_Node (L.Items [Index1]) do bool1 := not ((faDirectory and FileAttr) = faDirectory);
    with TBeeGui_List_Node (L.Items [Index2]) do bool2 := not ((faDirectory and FileAttr) = faDirectory);

    if bool1 xor bool2 then
    begin
      if bool1 then
        Result := 1
      else
        Result := -1
    end else
    begin
      case FColumnSort of
        byName:      Result := CompareText (TBeeGui_List_Node (L.Items [Index1]).FileName     , TBeeGui_List_Node (L.Items [Index2]).FileName);
        byPath:      Result := CompareText (TBeeGui_List_Node (L.Items [Index1]).FilePath     , TBeeGui_List_Node (L.Items [Index2]).FilePath);
        byType:      Result := CompareText (TBeeGui_List_Node (L.Items [Index1]).FileType     , TBeeGui_List_Node (L.Items [Index2]).FileType);
        bySize:      Result :=              TBeeGui_List_Node (L.Items [Index1]).FileSize     - TBeeGui_List_Node (L.Items [Index2]).FileSize;
        byPacked:    Result :=              TBeeGui_List_Node (L.Items [Index1]).FilePacked   - TBeeGui_List_Node (L.Items [Index2]).FilePacked;
        byRatio:     Result :=              TBeeGui_List_Node (L.Items [Index1]).FileRatio    - TBeeGui_List_Node (L.Items [Index2]).FileRatio;
        byAttr:      Result :=              TBeeGui_List_Node (L.Items [Index1]).FileAttr     - TBeeGui_List_Node (L.Items [Index2]).FileAttr;
        byTime:      Result := Round       (TBeeGui_List_Node (L.Items [Index1]).FileTime     - TBeeGui_List_Node (L.Items [Index2]).FileTime);
        byComm:      Result := CompareText (TBeeGui_List_Node (L.Items [Index1]).FileComm     , TBeeGui_List_Node (L.Items [Index2]).FileComm);
        byCRC:       Result :=              TBeeGui_List_Node (L.Items [Index1]).FileCRC      - TBeeGui_List_Node (L.Items [Index2]).FileCRC;
        byMethod:    Result := CompareText (TBeeGui_List_Node (L.Items [Index1]).FileMethod   , TBeeGui_List_Node (L.Items [Index2]).FileMethod);
        byVersion:   Result := CompareText (TBeeGui_List_Node (L.Items [Index1]).FileVersion  , TBeeGui_List_Node (L.Items [Index2]).FileVersion);
        byPassword:  Result := CompareText (TBeeGui_List_Node (L.Items [Index1]).FilePassword , TBeeGui_List_Node (L.Items [Index2]).FilePassword);
        byPosition:  Result :=              TBeeGui_List_Node (L.Items [Index1]).FilePosition - TBeeGui_List_Node (L.Items [Index2]).FilePosition;
        byIcon:      Result :=              TBeeGui_List_Node (L.Items [Index1]).FileIcon     - TBeeGui_List_Node (L.Items [Index2]).FileIcon;
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

  procedure TBeeGui_ListView.QuickSort;
  var
    I,J, Pivot : Longint;
  begin
    Items.BeginUpdate;
    Items.Count := 0;

    if (R > L) then
    begin
      repeat
        I := L;
        J := R;
        Pivot := (L + R) div 2;
        repeat
          while CompareFn (List, I, Pivot) < 0 do Inc (I);
          while CompareFn (List, J, Pivot) > 0 do Dec (J);
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

    Items.Count := FCurrFiles.Count;
    if not (FCurrFiles.Count = 0) then ItemFocused := Items [0];
    Items.EndUpdate;
  end;

  function TBeeGui_ListView.LoadDirectories;
  var
    I: integer;
    d: string;
    Node: TBeeGui_List_Node;
  begin
    FDirs.Clear;  FInfo.Clear;
    for I := 0 to FFiles.Count - 1 do
    begin
      LoadIcon (TBeeGui_List_Node (FFiles.Items [I]));
      d := TBeeGui_List_Node (FFiles.Items [I]).FilePath;
      while not (Length (d) = 0) do
      begin
        if FDirs.IndexOfDir (d) = -1 then
        begin
          Node := TBeeGui_List_Node.Create;
          Node.FileName := ExtractFileName (ExcludeTrailingBackslash (d));
          Node.FilePath := ExtractFilePath (ExcludeTrailingBackslash (d));

          Node.FilePacked := 0;
          Node.FileSize := 0;
          Node.FileRatio := 0;
          Node.FileTime := DateTimeToFileDate (Now);
          Node.FileAttr := faDirectory;
          Node.FileCRC := 0;
          Node.FilePosition := -1;

          FDirs.Add (Node);
          LoadIcon (Node);
        end;
        d := ExtractFilePath (ExtractFileDir (d));
      end;
    end;
    Result := not (FDirs.IndexOfDir (FCurrPath) = -1);
  end;

  procedure TBeeGui_ListView.LoadIcon;
  var
    Info: TBeeGui_FileInfo;
  begin
    BeeGui_GetFileInfo (Node.FileName, Node.FileAttr, Info);
    Node.FileType := Info.TypeName;
    Node.FileIcon := Info.IconIndex;
    FInfo.Update (Node);
  end;

  procedure TBeeGui_ListView.SetPathBox;
  begin
    FPathBox := Value;
  end;

  procedure TBeeGui_ListView.SetPathBoxSign;
  begin
    FPathBoxSign := Value;
  end;

  procedure TBeeGui_ListView.SetColumnSort;
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

  procedure TBeeGui_ListView.SetListMode;
  begin
    FListMode := Value;
    SetPath;
  end;

  procedure TBeeGui_ListView.SetPath;
  var
    I, J: integer;
  begin
    FCurrFiles.Clear;
    if fListMode then
    begin
      for I := 0 to FFiles.Count -1 do
        FCurrFiles.Add (FFiles.Items [I]);
    end else
    begin
      for I := 0 to FDirs.Count -1 do
        if CompareText (FCurrPath , TBeeGui_List_Node (FDirs.Items [I]).FilePath) = 0 then
          FCurrFiles.Add (FDirs.Items [I]);

      for I := 0 to FFiles.Count -1 do
        if CompareText (FCurrPath , TBeeGui_List_Node (FFiles.Items [I]).FilePath) = 0 then
          FCurrFiles.Add (FFiles.Items [I]);

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

  procedure TBeeGui_ListView.Initialize;
  begin
    if LoadDirectories  then
      SetCurrPath (FCurrPath)
    else
      SetCurrPath ('');
  end;

  procedure TBeeGui_ListView.Clear;
  begin
    SetLength (FCurrPath, 0);
    Items     .Clear;
    FFiles    .Clear;
    FDirs     .Clear;
    FCurrFiles.Clear;
  end;

  procedure TBeeGui_ListView.SetCurrPath;
  begin
    if Pos (FPathBoxSign, Value) = 1 then
      System.Delete (Value, 1, Length (FPathBoxSign));

    FCurrPath :=  (Value);
    SetPath;
  end;

  function TBeeGui_ListView.Up;
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

  procedure TBeeGui_ListView.GetImageIndex;
  begin
    if Item = nil then Exit;
    Item.ImageIndex := TBeeGui_List_Node (FCurrFiles.Items [Item.Index]).FileIcon;
  end;

  procedure TBeeGui_ListView.Data;
  begin
    if Item = nil then Exit;
    with TBeeGui_List_Node (FCurrFiles.Items [Item.Index]) do
    begin
      Item.Caption := FileName;                                               //  0 Name
      if (FileAttr and faDirectory) = 0 then
      begin
        Item.SubItems.Add (SizeToStr (FileSize));                             //  1 Size
        Item.SubItems.Add (SizeToStr (FilePacked));                           //  2 Packed
        Item.SubItems.Add (RatioToStr (FileRatio));                           //  3 Ratio
        Item.SubItems.Add (FileType);                                         //  4 Type

        Item.SubItems.Add (
             BeeGui_Common.DateTimeToString (FileDateToDateTime (FileTime))); //  5 Time

        Item.SubItems.Add (AttrToStr (FileAttr));                             //  6 Attr
        Item.SubItems.Add (FileMethod);                                       //  7 Method
        Item.SubItems.Add (FilePassword);                                     //  8 Password
        Item.SubItems.Add (HexToStr(FileCRC, SizeOf(FileCRC)));               //  9 CRC
        Item.SubItems.Add (FilePath);                                         // 10 Path
        Item.SubItems.Add (IntToStr (FilePosition));                          // 11 Position
      end else
      begin
        Item.SubItems.Add ('');                                               //  1 Size
        Item.SubItems.Add ('');                                               //  2 Packed
        Item.SubItems.Add ('');                                               //  3 Ratio
        Item.SubItems.Add (FileType);                                         //  4 Type
        Item.SubItems.Add ('');                                               //  5 Time
        Item.SubItems.Add (AttrToStr (FileAttr));                             //  6 Attr
        Item.SubItems.Add ('');                                               //  7 Method
        Item.SubItems.Add ('');                                               //  8 Password
        Item.SubItems.Add ('');                                               //  9 CRC
        Item.SubItems.Add (FilePath);                                         // 10 Path
        Item.SubItems.Add ('');                                               // 11 Position
      end;

    end;
  end;

/// TBeeGui_TreeView

  constructor TBeeGui_TreeView.Create;
  begin
    inherited Create (AOwner);
  end;

  destructor TBeeGui_TreeView.Destroy;
  begin
    SetLength (FCurrPath, 0);
    inherited Destroy;
  end;

  procedure TBeeGui_TreeView.Initialize;
  var
    I: integer;
    DriveList: TStringList;
    First, TMP: TTreeNode;
  begin
    FCurrPath := '';
    DriveList := TStringList.Create;
    Items.BeginUpdate;
    try
      Items.Clear;
      BeeGui_GetDrivers (DriveList);
      First := Items.GetFirstNode;
      for I := 0 to DriveList.Count -1 do
      begin
        TMP := Items.Add (First, DriveList.Strings [I]);
        TMP.ImageIndex := BeeGui_GetFileImageIndex (DriveList.Strings [I], 0);
        TMP.SelectedIndex := TMP.ImageIndex;

        TMP := Items.AddChild (TMP, treeconst);
        TMP.SelectedIndex := - 1;
        TMP.ImageIndex := - 1;
      end;
    finally
      Items.EndUpdate;
      DriveList.Free;
    end;

    OnExpanding := OnTreeExpanding;
    OnDblClick  := OnTreeDblClick;
    OnClick     := OnTreeClick;
  end;
  
  function TBeeGui_TreeView.SetPath;
  var
    I: integer;
    cDir, tDir: string;
    Start: TTreeNode;
  begin
    Result := False;
    SetCurrentDir (FCurrPath);
    cDir := IncludeTrailingBackslash (GetCurrentDir);

    I := 0;
    while I <  Items.Count do
    begin
      Start := Items [I];
      tDir  := IncludeTrailingBackslash (Start.Text);
      if Start.Level > 0 then
        repeat
          Start := Start.Parent;
          tDir  := IncludeTrailingBackslash (Start.Text) + tDir;
        until Start.Level = 0;

      if PosText (tDir, cDir) > 0 then
      begin
        Items [I].Expand (False);
        if CompareText (tDir, cDir) = 0 then
        begin
          Result := True;
          I := Items.Count
        end;
      end;
      Inc (I);
    end;
  end;

  procedure TBeeGui_TreeView.SetCurrPath;
  begin
    FCurrPath := Value;
    SetPath;
  end;

  procedure TBeeGui_TreeView.SetEdit;
  begin
    FPathEdit := Value;
  end;

  procedure TBeeGui_TreeView.OnTreeExpanding;
  var
    Start, TMP: TTreeNode;
    DirList: TStringList;
    Path: string;
    I: integer;
  begin
    Start := Node;
    Selected := Node;

    if not Start.HasChildren then Exit;
    if not Start.GetFirstChild.ImageIndex = -1 then Exit;

    Path := IncludeTrailingBackslash (Start.Text);
    if Start.Level > 0 then
      repeat
        Start := Start.Parent;
        Path := IncludeTrailingBackslash (Start.Text) + Path;
      until Start.Level = 0;


    DirList := TStringList.Create;
    Items.BeginUpdate;
    try
      if BeeGui_GetDirList (Path, DirList) then
        if Assigned (FPathEdit) then
          FPathEdit.Text := Path;

      Start := Node;
      while Start.HasChildren do Start.GetFirstChild.Delete;

      for I := 0 to DirList.Count - 1 do begin
        TMP := Items.AddChild (Start, DirList.Strings [I]);
        TMP.ImageIndex := BeeGui_GetFileImageIndex (DirList.Strings [I], faDirectory);
        TMP.SelectedIndex :=  BeeGui_GetFileOpenImageIndex (DirList.Strings [I], faDirectory);

        TMP := Items.AddChild (TMP, treeconst);
        TMP.SelectedIndex := - 1;
        TMP.ImageIndex := - 1;
      end;

    finally
      Items.EndUpdate;
      DirList.Free;
    end;
  end;

  procedure TBeeGui_TreeView.OnTreeDblClick;
  begin
    if not (Selected = nil) then Selected.Expand (False);
  end;

  procedure   TBeeGui_TreeView.OnTreeClick;
  var
    Start: TTreeNode;
    Path: string;
  begin
    if not (Selected = nil) then
    begin
      Start := Selected;
      Path := IncludeTrailingBackslash (Start.Text);
      if Start.Level > 0 then
        repeat
          Start := Start.Parent;
          Path := IncludeTrailingBackslash (Start.Text) + Path;
        until Start.Level = 0;

      if SetCurrentDir (Path) then
        if Assigned (FPathEdit) then
          FPathEdit.Text := Path;
    end;
  end;

// TBeeGui_ListView_Add

  function TBeeGui_ListView_Add.ANDstring;
  var
    J : integer;
  begin
    Result := '';
    for J := 1 to Min (Length (str1), Length (str2)) do
      if CompareText (str1 [J], str2 [J]) = 0 Then
        Result := Result + str1 [J]
      else
      begin
        if not (CompareText (IncludeTrailingBackslash (Result), Result) = 0) Then
          Result := ExtractFilePath (Result);
        Exit;
      end;
  end;

  function TBeeGui_ListView_Add.GetNewRoot;
  var
    I: integer;
    S: TStringList;
  begin
    Result := '';
    S := TStringList.Create;

    for I := 0 to Items.Count - 1 do
      if Items [I].ImageIndex = S_Index then
        S.Add (Items [I].SubItems [0]);

    if S.Count > 0 then
    begin
      Result := S.Strings [0];
      for I := 1 to S.Count - 1 do
        Result := ANDstring (Result, S.Strings [I]);
    end;

    if NewSpin > 0 then
      for I := 1 to NewSpin do
      begin
        Result := ExtractFilePath (ExcludeTrailingBackslash (Result));
        if Length (Result) > 0 then
          Result := IncludeTrailingBackslash (Result);
      end
    else
      if Length (Result) > 0 then
        Result := IncludeTrailingBackslash (Result);

    S.Free;
  end;

  procedure TBeeGui_ListView_Add.UpdateItems;
  var
    I, DirPos: integer;
    R, F: string;
  begin
    if Items.Count = 0 then Spin := 0;

    R := GetNewRoot (Spin);
    for I := Items.Count -1 downto 0 do
    begin
      F := Items [I].SubItems [0] + Items [I].SubItems [1] + Items [I].SubItems [2];

      DirPos := Pos (R, F);

      if DirPos = 1  then
        System.Delete (F, DirPos, Length (R));

      Items [I].Caption := F;
    end;

    if Assigned (FRoot) then FRoot.Text := R;
  end;

  procedure TBeeGui_ListView_Add.AddFile;
  var
    lvItem: TListItem;
  begin
    Items.BeginUpdate;
    lvItem := Items.Add;
    with lvItem do
    begin
      Caption := ExtractFileName (FileName);

      SubItems.Add (ExtractFilePath (FileName));
      SubItems.Add ('');
      SubItems.Add (Caption);

      ImageIndex := S_Index;
    end;
    Items.EndUpdate;
    UpdateItems;
  end;

  procedure TBeeGui_ListView_Add.AddDirectory;
  var
    lvItem: TListItem;
  begin
    Items.BeginUpdate;
    lvItem := Items.Add;
    with lvItem do
    begin
      Caption := '*.*';

      SubItems.Add (ExtractFilePath (DirName));
      if Length (ExtractFileName (DirName)) = 0 then
        SubItems.Add ('')
      else
        SubItems.Add (IncludeTrailingBackslash (ExtractFileName (DirName)));
      SubItems.Add (Caption);

      ImageIndex := S_Index;
    end;
    Items.EndUpdate;
    UpdateItems;
  end;

  procedure TBeeGui_ListView_Add.SetRoot (Value: TEdit);
  begin
    FRoot := Value;
  end;

  procedure TBeeGui_ListView_Add.SetSpin;
  begin
    FSpin := Value;
  end;

/// TBeeGui_AppView

  constructor TBeeGui_AppView.Create (const FileName, PathName: string);
  begin
    cFileName := FileName;
    cPathName := PathName;
    cFileTime := FileAge (FileName);

    inherited Create (True);
    FreeOnTerminate := True; 
  end;

  procedure TBeeGui_AppView.Execute;
  begin
    BeeGui_ShellExecuteAndWait (cFileName, cPathName, True);
  end;

// TBeeGui_ComboBox

  constructor TBeeGui_ComboBox.Create (AOwner: TComponent);
  begin
    inherited Create (AOwner);
    Style := csDropDownList;
  end;

  procedure TBeeGui_ComboBox.SetMask (Value: string);
  var
    T: TSearchRec;
  begin
    if Length (Value) > 0 then
      FMask := Value
    else
      FMask := '*.*';  

    Items.Clear; 
    if FindFirst (ExtractFilePath (ParamStr (0)) + FMask, faAnyFile - faDirectory, T) = 0 then
      repeat
        Items.Add (ChangeFileExt (T.Name, ''));
      until (not (FindNext (T) = 0));
    FindClose (T);
  end;

/// Register

  procedure Register;
  begin
    RegisterComponents ('BeeGui', [TBeeGui_ListView    ]);
    RegisterComponents ('BeeGui', [TBeeGui_ListView_Add]);
    RegisterComponents ('BeeGui', [TBeeGui_TreeView    ]);
    RegisterComponents ('BeeGui', [TBeeGui_ComboBox    ]);
    RegisterComponents ('BeeGui', [TBeeGui_PathBox     ]);
    RegisterComponents ('BeeGui', [TBeeGui_Label       ]);
    RegisterComponents ('BeeGui', [TDragFilesTrg       ]);
    RegisterComponents ('BeeGui', [TDragFilesSrc       ]);
  end;

end.
