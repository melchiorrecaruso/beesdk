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

{   Contains:

    TArcLisView class.

    Modifyed:
}

unit BeeGui_ArchiveListViewMgr;

{$I compiler.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Math,
  Classes,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  SysUtils,
  Controls,
  ComCtrls,
  LResources,
  // ---
  BeeGui_IconList,
  BeeGui_ArchiveFolderBox;

type

 { TArchiveItem }

  TArchiveItem = class
  public
    FileName: string;
    FilePath: string;
    FileType: string;
    FileIcon: integer;
    FileSize: integer;
    FilePacked: integer;
    FileRatio: integer;
    FileAttr: integer;
    FileTime: integer;
    FileComm: string;
    FileCrc:  cardinal;
    FileMethod: string;
    FileVersion: string;
    FilePassword: string;
    FilePosition: integer;
  end;
  
type

  { TArchiveList }

  TArchiveList = class(TList)
  public
    procedure Clear; override;
    destructor Destroy; override;
    function IndexOf(const FilePath: string; const FileName: string): integer;
  end;
  
type

  { TArchiveDetails }

  TArchiveDetails = class
  public
    FilesCount: integer;
    FilesSize: cardinal;
    FilesPacked: cardinal;
    FilesCrypted: integer;
    DirectoriesCount: integer;
    Version:  float;
    LastTime: integer;
  public
    constructor Create;
    procedure Clear;
    procedure Update(Item: TArchiveItem);
  end;

type

  { TArchiveListViewColumn }
  
  TArchiveListViewColumn = (alvcName, alvcPath, alvcType, alvcSize,
    alvcPacked, alvcRatio, alvcAttr, alvcTime, alvcComm, alvcCrc,
    alvcMethod, alvcVersion, alvcPassword, alvcPosition);
    
type

  { TCustomArchiveListView }

  TCustomArchiveListView = class(TCustomListView)
  private
    FFileName: string;
    FDetails: TArchiveDetails;
    // ---
    FFiles: TArchiveList;
    FFolders: TArchiveList;
    FFolderFiles: TList;
    FFolder: string;
    // ---
    FFolderBox: TArchiveFolderBox;
    FFolderBoxSign: string;
    // ---
    FSimpleList: boolean;
    FSortCol: TArchiveListViewColumn;
    FSortDir: boolean;
    FAutoLoad: boolean;
  private
    function CompareFn(L: TList; I1, I2: integer): integer;
    procedure QuickSort(List: TList; L, R: integer);
    function UpdateFolders: boolean;
    procedure UpdateFolder;
    // ---
    procedure SetFolder(Value: string);
    procedure SetAutoLoad(Value: boolean);
    procedure SetFolderBox(Value: TArchiveFolderBox);
    procedure SetSortDir(Value: boolean);
    procedure SetSortCol(Value: TArchiveListViewColumn);
    procedure SetSimpleList(Value: boolean);
    // ---
    procedure Data(Sender: TObject; Item: TListItem);
    procedure AddToFolderBox(const AFolder: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFolderBox;
    procedure Initialize;
    // ---
    function Open(const AArchiveName: string; AArchiveList: TList): boolean;
    procedure CloseArchive(Clean: boolean);
    function Up: boolean;
    // ---
    procedure ClearMasks;
    procedure InvertMasks;
    procedure GetMasks(FileMasks: TStringList);
    procedure SetMask(const Mask: string; Value: boolean);
  public
    property FileName: string read FFileName;
    property Details: TArchiveDetails read FDetails;
    property Files: TArchiveList read FFiles write FFiles default nil;
    property FolderFiles: TList read FFolderFiles write FFolderFiles default nil;
    property Folder: string read FFolder write SetFolder;
    property FolderBox: TArchiveFolderBox read FFolderBox write SetFolderBox default nil;
    property SortCol: TArchiveListViewColumn read FSortCol write SetSortCol default alvcName;
    property SimpleList: boolean read FSimpleList write SetSimpleList default False;
    property AutoLoadFolderBox: boolean read FAutoLoad write SetAutoLoad default False;
  end;
  
type

  { TArcListView }
  
  TArchiveListView = class(TCustomArchiveListView)
  public
    property Details;
    property Folder;
  published
    property FolderBox;
    property SimpleList;
    property SortCol;
    // ---
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
//    property Checkboxes;
//    property Color default clWindow;
    property Columns;
    property ColumnClick;
    property Constraints;
    property DragCursor;
    property DragMode;
//    property Enabled;
    property Font;
//    property HideSelection;
//    property Items;
    property LargeImages;
//    property MultiSelect;
    property PopupMenu;
//    property ReadOnly;
    property RowSelect;
    property ScrollBars;
//    property ShowColumnHeaders;
    property SmallImages;
//    property SortColumn;
//    property SortType;
//    property StateImages;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle;
//    property OnAdvancedCustomDraw;
//    property OnAdvancedCustomDrawItem;
//    property OnAdvancedCustomDrawSubItem;
//    property OnChange;
      property OnClick;
      property OnColumnClick;
//    property OnCompare;
//    property OnCustomDraw;
//    property OnCustomDrawItem;
//    property OnCustomDrawSubItem;
      property OnDblClick;
//    property OnDeletion;
//    property OnDragDrop;
//    property OnDragOver;
//    property OnEndDrag;
//    property OnKeyDown;
//    property OnKeyPress;
//    property OnKeyUp;
//    property OnMouseDown;
//    property OnMouseMove;
//    property OnMouseUp;
//    property OnResize;
      property OnSelectItem;
//    property OnStartDrag;
  end;
  
  
  { Register }

  procedure Register;

implementation

uses
  Graphics,
  // ---
  Bee_Common,
  BeeGui_SysUtils;

  { TArcList }

  destructor TArchiveList.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;

  procedure TArchiveList.Clear;
  var
    I: integer;
  begin
    for I := Count - 1 downto 0 do
    begin
      TArchiveItem(Items[I]).Free;
    end;
    inherited Clear;
  end;

  function TArchiveList.IndexOf(const FilePath: string; const FileName: string): integer;
  var
    I: integer;
  begin
    Result := -1;
    for I := 0 to Count - 1 do
    begin
      if CompareFileName(TArchiveItem(Items[I]).FileName, FileName) = 0 then
      begin
        if CompareFileName(TArchiveItem(Items[I]).FilePath, FilePath) = 0 then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;

  { TArchiveDetails }
  
  constructor TArchiveDetails.Create;
  begin
    inherited Create;
  end;

  procedure TArchiveDetails.Clear;
  begin
    FilesCount       := 0;
    FilesSize        := 0;
    FilesPacked      := 0;
    FilesCrypted     := 0;
    DirectoriesCount := 0;
    Version          := 0;
    LastTime         := 0;
  end;

  procedure TArchiveDetails.Update(Item: TArchiveItem);
  begin
    if (Item.FileAttr and faDirectory) = 0 then
    begin
      Inc(FilesCount);
      Inc(FilesSize, Item.FileSize);
      Inc(FilesPacked, Item.FilePacked);
      if CompareText(Item.FilePassword, 'Yes') = 0 then
      begin
        Inc(FilesCrypted);
      end;
      LastTime := Max(LastTime, Item.FileTime);
      try
        if StrToFloat(Item.FileVersion) > Version then
        begin
          Version := StrToFloat(Item.FileVersion);
        end;
      except
        Version := 0;
      end;
    end
    else
      Inc(DirectoriesCount);
  end;

  { TCustomArcListView }
  
  constructor TCustomArchiveListView.Create(AOwner: TComponent);
  var
    i: integer;
    Col: TListColumn;
  begin
    inherited Create(AOwner);
    FFileName := '';
    // --
    FFiles := TArchiveList.Create;
    FFolderFiles := TList.Create;
    FDetails := TArchiveDetails.Create;
    FFolders := TArchiveList.Create;
    FFolder := '';
    // --
    FSortDir := True;
    FAutoLoad := False;
    FSimpleList := False;
    // ---
    Color := clInactiveBorder;
    MultiSelect := True;
    Enabled := False;
  end;
  
  destructor TCustomArchiveListView.Destroy;
  begin
    FFileName := '';
    // ---
    FFiles.Free;
    FFolderFiles.Free;
    FDetails.Free;
    FFolders.Free;
    FFolder := '';
    inherited Destroy;
  end;

  function TCustomArchiveListView.CompareFn(L: TList; I1, I2: integer): integer;
  var
    bool1, bool2: boolean;
  begin
    with TArchiveItem(L.Items[I1]) do bool1 := not ((faDirectory and FileAttr) = faDirectory);
    with TArchiveItem(L.Items[I2]) do bool2 := not ((faDirectory and FileAttr) = faDirectory);

    if bool1 xor bool2 then
    begin
      if bool1 then
        Result := 1
      else
        Result := -1;
    end else
    begin
      case FSortCol of
        alvcName    : Result := CompareFileName(TArchiveItem(L.Items[I1]).FileName,       TArchiveItem(L.Items[I2]).FileName);
        alvcPath    : Result := CompareFileName(TArchiveItem(L.Items[I1]).FilePath,       TArchiveItem(L.Items[I2]).FilePath);
        alvcType    : Result := CompareFileName(TArchiveItem(L.Items[I1]).FileType,       TArchiveItem(L.Items[I2]).FileType);
        alvcSize    : Result :=                (TArchiveItem(L.Items[I1]).FileSize      - TArchiveItem(L.Items[I2]).FileSize);
        alvcPacked  : Result :=                (TArchiveItem(L.Items[I1]).FilePacked    - TArchiveItem(L.Items[I2]).FilePacked);
        alvcRatio   : Result :=                (TArchiveItem(L.Items[I1]).FileRatio     - TArchiveItem(L.Items[I2]).FileRatio);
        alvcAttr    : Result :=                (TArchiveItem(L.Items[I1]).FileAttr      - TArchiveItem(L.Items[I2]).FileAttr);
        alvcTime    : Result :=           Round(TArchiveItem(L.Items[I1]).FileTime      - TArchiveItem(L.Items[I2]).FileTime);
        alvcCRC     : Result :=                (TArchiveItem(L.Items[I1]).FileCRC       - TArchiveItem(L.Items[I2]).FileCRC);
        alvcMethod  : Result :=     CompareText(TArchiveItem(L.Items[I1]).FileMethod,     TArchiveItem(L.Items[I2]).FileMethod);
        alvcPassword: Result :=     CompareText(TArchiveItem(L.Items[I1]).FilePassword,   TArchiveItem(L.Items[I2]).FilePassword);
        alvcPosition: Result :=                (TArchiveItem(L.Items[I1]).FilePosition  - TArchiveItem(L.Items[I2]).FilePosition);
      else
        Result := I2 - I1;
      end;
    end;

    if FSortDir then
    begin
      if Result > 0 then
        Result := -1
      else
      if Result < 0 then
        Result := 1;
    end;
  end;

  procedure TCustomArchiveListView.QuickSort(List: TList; L, R: integer);
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

  procedure TCustomArchiveListView.SetFolderBox(Value: TArchiveFolderBox);
  begin
    FFolderBox := Value;
    if Assigned(FFolderBox) then
    begin
      FFolderBox.Enabled := False;
      FFolderBox.Color := clInactiveBorder;
    end;
  end;

  procedure TCustomArchiveListView.SetSimpleList(Value: boolean);
  begin
    FSimpleList := Value;
    UpdateFolder;
  end;
  
  procedure TCustomArchiveListView.SetSortDir(Value: boolean);
  begin
    FSortDir := Value;
    UpdateFolder;
  end;
  
  procedure TCustomArchiveListView.SetSortCol(Value: TArchiveListViewColumn);
  begin
    if Value = FSortCol then
      FSortDir := not FSortDir
    else
      FSortDir := True;
      
    FSortCol := Value;
    UpdateFolder;
  end;

  procedure TCustomArchiveListView.UpdateFolder;
  var
    I, J: integer;
    Item: TListItem;
    Image: TBitmap;
  begin
    FFolderFiles.Clear;
    if FSimpleList then
    begin
      for I := 0 to FFiles.Count -1 do
        FFolderFiles.Add(FFiles.Items[I]);
    end else
    begin
      for I := 0 to FFolders.Count -1 do
        if CompareFileName(FFolder, TArchiveItem(FFolders.Items[I]).FilePath) = 0 then
        begin
          FFolderFiles.Add(FFolders.Items[I]);
        end;

      for I := 0 to FFiles.Count - 1 do
        if CompareFileName(FFolder, TArchiveItem(FFiles.Items[I]).FilePath) = 0 then
        begin
          FFolderFiles.Add(FFiles.Items[I]);
        end;

      if (FFiles.Count > 0) and Assigned(FFolderBox) then
      begin
        if AutoLoadFolderBox then LoadFolderBox;

        J := -1;
        for I := 0 to FFolderBox.Items.Count -1 do
        begin
          if CompareFileName(FFolderBoxSign + FFolder, FFolderBox.Items[I]) = 0 then
          begin
            J := I;
            Break;
          end;
        end;

        if (J = -1) then
        begin
          if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
          begin
            if Length(FFolder) = 0 then
              I := TIconList(SmallImages).FileIcon('.bee' ,faArchive)
            else
              I := TIconList(SmallImages).FileIcon('.@folderclose' ,faDirectory);

            if I > -1 then
            begin
              Image := TBitmap.Create;
              TIconList(SmallImages).GetBitmap(I, Image);
            end else
              Image := nil;
          end;
          FFolderBox.ItemIndex := FFolderBox.Items.AddObject(FFolderBoxSign + FFolder, Image);
        end else
          FFolderBox.ItemIndex := J;
      end;
    end;
    QuickSort(FFolderFiles, 0, FFolderFiles.Count - 1);

    BeginUpdate; Clear;
    for I := 0 to  FFolderFiles.Count -1 do
    begin
      Data(Self, Items.Add);
    end;
    EndUpdate;
      
    if (Items.Count > 0) then
    begin
      ItemFocused := Items[0];
    end;
  end;

  procedure TCustomArchiveListView.Initialize;
  begin
    if UpdateFolders then
      SetFolder(FFolder)
    else
      SetFolder('');

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

  procedure TCustomArchiveListView.CloseArchive(Clean: boolean);
  var
    I: integer;
  begin
    Enabled := False;
    Color := clInactiveBorder;

    if Assigned(FFolderBox) then
    begin
      FFolderBox.Enabled := False;
      FFolderBox.Color := clInactiveBorder;

      for I := 0 to FFolderBox.Items.Count -1 do
      begin
        if Assigned(FFolderBox.Items.Objects[I]) then
          FFolderBox.Items.Objects[I].Free;
      end;
      FFolderBox.Clear;
    end;

    FFiles.Clear;
    FDetails.Clear;
    FFolders.Clear;
    FFolderFiles.Clear;

    if Clean then
    begin
      FFolderBoxSign := '';
      FFileName := '';
      FFolder:= '';
    end;

    BeginUpdate;
    Items.Clear;
    EndUpdate;
  end;

  procedure TCustomArchiveListView.SetFolder(Value: string);
  begin
    if FileNamePos(FFolderBoxSign, Value) = 1 then
    begin
      System.Delete(Value, 1, Length(FFolderBoxSign));
    end;
    FFolder := IncludeTrailingBackSlash(Value);
    UpdateFolder;
  end;

  function TCustomArchiveListView.Up: boolean;
  var
    D: string;
  begin
    Result := True;
    D := ExtractFilePath(ExcludeTrailingBackSlash(FFolder));
    if not (CompareFileName(FFolder, D) = 0) then
      SetFolder(D)
    else
      Result := False;
  end;
  
  procedure TCustomArchiveListView.GetMasks(FileMasks: TStringList);
  var
    S: string;
    I: integer;
    Node: TArchiveItem;
  begin
    for I := 0 to FFolderFiles.Count -1 do
    begin
      if Items[I].Selected then
      begin
        Node := TArchiveItem(FFolderFiles.Items[I]);
        if (Node.FileAttr and faDirectory) = faDirectory then
          S := Node.FilePath + IncludeTrailingBackSlash(Node.FileName) + '*'
        else
          S := Node.FilePath + Node.FileName;
        FileMasks.Add(S);
      end;
    end;
  end;
  
  procedure TCustomArchiveListView.ClearMasks;
  var
    I: integer;
  begin
    for I := 0 to FolderFiles.Count -1 do
    begin
      TListItem(Items[I]).Selected := False;
    end;
  end;
  
  procedure TCustomArchiveListView.InvertMasks;
  var
    I: integer;
  begin
    for I := 0 to FolderFiles.Count -1 do
    begin
      with TListItem(Items[I]) do
        Selected := not Selected;
    end;
  end;
  
  procedure TCustomArchiveListView.SetMask(const Mask: string; Value: boolean);
  var
    I: integer;
    Node: TArchiveItem;
  begin
    for I := 0 to FolderFiles.Count -1 do
    begin
      Node := TArchiveItem(FolderFiles[I]);
      if FileNameMatch(Node.FileName, Mask, False) then
      begin
        TListItem(Items[I]).Selected := Value;
      end;
    end;
  end;
  
  function TCustomArchiveListView.Open(const AArchiveName: string; AArchiveList: TList): boolean;
  var
    J, K: integer;
    Node: TArchiveItem;
  begin
    if CompareFileName(AArchiveName, FFileName) <> 0 then
    begin
      FFolder:= '';
    end;
    FFileName := AArchiveName;
    FFolderBoxSign := ExtractFileName(FFileName) + PathDelim;

    FFiles.Clear;
    FDetails.Clear;
    while AArchiveList.Count > 0 do
    begin
      Node := TArchiveItem(AArchiveList.Items[0]);

      if Assigned(LargeImages) and (LargeImages.ClassType = TIconList) then
        J := TIconList(LargeImages).FileIcon(Node.FileName, Node.FileAttr)
      else
        J := -1;

      if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
        K := TIconList(SmallImages).FileIcon(Node.FileName, Node.FileAttr)
      else
        K := -1;

      if J = K then
        Node.FileIcon := K
      else
        Node.FileIcon := -1;

      if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
        Node.FileType := TIconList(SmallImages).FileType(Node.FileName, Node.FileAttr);

      FFiles.Add(Node);
      FDetails.Update(Node);
      AArchiveList.Delete(0);
    end;
    UpdateFolders;
    UpdateFolder;
      
    if Assigned(FFolderBox) then
    begin
      FFolderBox.Color := clWindow;
      FFolderBox.Enabled := True;
    end;
    Color := clWindow;
    Enabled := True;
    Result := True;
  end;
  
  function TCustomArchiveListView.UpdateFolders: boolean;
  var
    D: string;
    I, J, K: integer;
    Node: TArchiveItem;
  begin
    for I := 0 to FFiles.Count - 1 do
    begin
      D := ExcludeTrailingBackslash(TArchiveItem(FFiles.Items[I]).FilePath);
      while (Length(D) > 0) do
      begin
        if FFolders.IndexOf(ExtractFilePath(D), ExtractFileName(D)) = -1 then
        begin
          Node := TArchiveItem.Create;
          Node.FileName := ExtractFileName(D);
          Node.FilePath := ExtractFilePath(D);
          Node.FilePacked := 0;
          Node.FileSize := 0;
          Node.FileRatio := 0;
          Node.FileTime := DateTimeToFileDate(Now);
          Node.FileAttr := faDirectory;
          Node.FileCRC := 0;
          Node.FilePosition := -1;
          
          if Assigned(LargeImages) and (LargeImages.ClassType = TIconList) then
            J := TIconList(LargeImages).FileIcon('.@folderclose', faDirectory)
          else
            J := -1;

          if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
            K := TIconList(SmallImages).FileIcon('.@folderclose', faDirectory)
          else
            K := -1;

          if J = K then
            Node.FileIcon := K
          else
            Node.FileIcon := -1;

          if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
            Node.FileType := TIconList(SmallImages).FileType('.@folderclose', faDirectory)
          else
            Node.FileType := '';

          FDetails.Update(Node);
          FFolders.Add(Node);
        end;
        D := ExtractFileDir(D);
      end;
    end;
    D := ExcludeTrailingBackSlash(FFolder);
    Result := FFolders.IndexOf(ExtractFilePath(D), ExtractFileName(D)) <> -1;
  end;

  procedure TCustomArchiveListView.SetAutoLoad(Value: boolean);
  begin
    FAutoLoad := Value;
    if FAutoLoad then LoadFolderBox;
  end;

  procedure TCustomArchiveListView.AddToFolderBox(const AFolder: string);
  var
    J, K, G: integer;
    Image: TBitmap;
  begin
    if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
      if Assigned(FFolderBox) then
      begin
        K := -1;
        for J := 0 to FFolderBox.Items.Count -1 do
        begin
          if CompareFileName(FFolderBoxSign + AFolder, FFolderBox.Items[J]) = 0 then
          begin
            K := J;
            Break;
          end;
        end;

        if K = -1 then
        begin
          if Length(AFolder) = 0 then
            G := TIconList(SmallImages).FileIcon('.bee' ,faArchive)
          else
            G := TIconList(SmallImages).FileIcon('.@folderclose' ,faDirectory);

          if G > -1 then
          begin
            Image := TBitmap.Create;
            TIconList(SmallImages).GetBitmap(G, Image);
          end else
            Image := nil;

          FFolderBox.Items.AddObject(FFolderBoxSign + AFolder, Image);
        end;
      end;
  end;

  procedure TCustomArchiveListView.LoadFolderBox;
  var
    I: integer;
  begin
    if Assigned(FFolderBox) then
    begin
      for I := 0 to FFolderBox.Items.Count -1 do
      begin
        if Assigned(FFolderBox.Items.Objects[I]) then
          FFolderBox.Items.Objects[I].Free;
      end;
      FFolderBox.Clear;

      AddToFolderBox('');
      for I := 0 to FFolders.Count -1 do
      begin
        with TArchiveItem(FFolders.Items[I]) do
          AddToFolderBox(IncludeTrailingBackSlash(FilePath + FileName));
      end;
      FFolderBox.Sorted := True;
    end;
  end;

  procedure TCustomArchiveListView.Data(Sender: TObject; Item: TListItem);
  begin
    if Assigned(Item) then
    begin
      with TArchiveItem(FFolderFiles.Items[Item.Index]) do
      begin
        Item.Caption := FileName;                                //  0 Name
        if (FileAttr and faDirectory) = 0 then
        begin
          Item.SubItems.Add(SizeToStr(FileSize));                //  1 Size
          Item.SubItems.Add(SizeToStr(FilePacked));              //  2 Packed
          Item.SubItems.Add(RatioToStr(FileRatio));              //  3 Ratio

          if FileType = '' then                                  //  4 Type
          begin
            FileType := 'File ' + ExtractFileExt(FileName);
          end;
          Item.SubItems.Add(FileType);

          try
          Item.SubItems.Add(
            DateTimeToString(FileDateToDateTime(FileTime)));     //  5 Time
          except
            Item.SubItems.Add('');
          end;
          Item.SubItems.Add(AttrToStr(FileAttr));                //  6 Attr
          Item.SubItems.Add(FileMethod);                         //  7 Method
          Item.SubItems.Add(FilePassword);                       //  8 Password
          Item.SubItems.Add(Hex(FileCRC, SizeOf(FileCRC)));      //  9 CRC
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
        Item.ImageIndex := FileIcon;
      end;
   end;
  end;

  { Register }

  procedure Register;
  begin
    RegisterComponents ('BeePackage', [TArchiveListView]);
  end;

initialization

  {$i beegui_archivelistviewmgr.lrs }

end.
