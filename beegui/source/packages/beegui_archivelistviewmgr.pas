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
  BeeGui_IconList;

type

 { TArcItem }

  TArcItem = class
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

  { TArcList }

  TArcList = class(TList)
  public
    procedure Clear; override;
    destructor Destroy; override;
    function IndexOf(const FilePath: string; const FileName: string): integer;
  end;
  
type

  { TArcDetails }

  TArcDetails = class
  public
    FilesCount: integer;
    FilesSize: integer;
    FilesPacked: integer;
    FilesCrypted: integer;
    DirectoriesCount: integer;
    Version:  float;
    LastTime: integer;
  public
    constructor Create;
    procedure Clear;
    procedure Update(Item: TArcItem);
  end;

type

  { TArcListViewColumn }
  
  TArcListViewColumn = (alvcName, alvcPath, alvcType, alvcSize,
    alvcPacked, alvcRatio, alvcAttr, alvcTime, alvcComm, alvcCrc,
    alvcMethod, alvcVersion, alvcPassword, alvcPosition);
    
type

  { TCustomArcListView }

  TCustomArcListView = class(TCustomListView)
  private
    FFileName: string;
    FFileLink: string;
    FDetails: TArcDetails;
    // ---
    FFiles: TArcList;
    FFolders: TArcList;
    FFolderFiles: TList;
    FFolder: string;
    // ---
    FFolderBox: TComboBox;
    FFolderBoxSign: string;
    // ---
    FSimpleList: boolean;
    FSortCol: TArcListViewColumn;
    FSortDir: boolean;
  private
    function CompareFn(L: TList; I1, I2: integer): integer;
    procedure QuickSort(List: TList; L, R: integer);
    function UpdateFolders: boolean;
    procedure UpdateFolder;
    // ---
    procedure SetFolder(Value: string);
    procedure SetFolderBox(Value: TComboBox);
    procedure SetSortDir(Value: boolean);
    procedure SetSortCol(Value: TArcListViewColumn);
    procedure SetSimpleList(Value: boolean);
    // ---
    procedure Data(Sender: TObject; Item: TListItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize;
    // ---
    function OpenArchive(const FileName, FileLink: string): boolean;
    procedure CloseArchive;
    function Up: boolean;
  protected
    property FileName: string read FFileName;
    property FileLink: string read FFileLink;
    property Details: TArcDetails read FDetails;
    property Files: TArcList read FFiles write FFiles default nil;
    property FolderFiles: TList read FFolderFiles write FFolderFiles default nil;
    property Folder: string read FFolder write SetFolder;
    property FolderBox: TComboBox read FFolderBox write SetFolderBox default nil;
    property SortCol: TArcListViewColumn read FSortCol write SetSortCol default alvcName;
    property SimpleList: boolean read FSimpleList write SetSimpleList default False;
  end;
  
type

  { TArcListView }
  
  TArcListView = class(TCustomArcListView)
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

  destructor TArcList.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;

  procedure TArcList.Clear;
  var
    I: integer;
  begin
    for I := Count - 1 downto 0 do
    begin
      TArcItem(Items[I]).Free;
    end;
    inherited Clear;
  end;

  function TArcList.IndexOf(const FilePath: string; const FileName: string): integer;
  var
    I: integer;
  begin
    Result := -1;
    for I := 0 to Count - 1 do
    begin
      if CompareFileName(TArcItem(Items[I]).FileName, FileName) = 0 then
      begin
        if CompareFileName(TArcItem(Items[I]).FilePath, FilePath) = 0 then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;

  { TArctDetails }
  
  constructor TArcDetails.Create;
  begin
    inherited Create;
  end;

  procedure TArcDetails.Clear;
  begin
    FilesCount       := 0;
    FilesSize        := 0;
    FilesPacked      := 0;
    FilesCrypted     := 0;
    DirectoriesCount := 0;
    Version          := 0;
    LastTime         := 0;
  end;

  procedure TArcDetails.Update(Item: TArcItem);
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
  
  constructor TCustomArcListView.Create(AOwner: TComponent);
  var
    i: integer;
    Col: TListColumn;
  begin
    inherited Create(AOwner);
    FFileName := '';
    FFileLink := '';
    // --
    FFiles := TArcList.Create;
    FFolderFiles := TList.Create;
    FDetails := TArcDetails.Create;
    FFolders := TArcList.Create;
    FFolder := '';
    // --
    FSortDir := True;
    FSimpleList := False;
    // ---
    Color := clInactiveBorder;
    Enabled := False;
  end;
  
  destructor TCustomArcListView.Destroy;
  begin
    FFileName := '';
    FFileLink := '';
    // ---
    FFiles.Free;
    FFolderFiles.Free;
    FDetails.Free;
    FFolders.Free;
    FFolder := '';
    inherited Destroy;
  end;

  function TCustomArcListView.CompareFn(L: TList; I1, I2: integer): integer;
  var
    bool1, bool2: boolean;
  begin
    with TArcItem(L.Items[I1]) do bool1 := not ((faDirectory and FileAttr) = faDirectory);
    with TArcItem(L.Items[I2]) do bool2 := not ((faDirectory and FileAttr) = faDirectory);

    if bool1 xor bool2 then
    begin
      if bool1 then
        Result := 1
      else
        Result := -1;
    end else
    begin
      case FSortCol of
        alvcName    : Result := CompareFileName(TArcItem(L.Items[I1]).FileName,       TArcItem(L.Items[I2]).FileName);
        alvcPath    : Result := CompareFileName(TArcItem(L.Items[I1]).FilePath,       TArcItem(L.Items[I2]).FilePath);
        alvcType    : Result := CompareFileName(TArcItem(L.Items[I1]).FileType,       TArcItem(L.Items[I2]).FileType);
        alvcSize    : Result :=                (TArcItem(L.Items[I1]).FileSize      - TArcItem(L.Items[I2]).FileSize);
        alvcPacked  : Result :=                (TArcItem(L.Items[I1]).FilePacked    - TArcItem(L.Items[I2]).FilePacked);
        alvcRatio   : Result :=                (TArcItem(L.Items[I1]).FileRatio     - TArcItem(L.Items[I2]).FileRatio);
        alvcAttr    : Result :=                (TArcItem(L.Items[I1]).FileAttr      - TArcItem(L.Items[I2]).FileAttr);
        alvcTime    : Result :=           Round(TArcItem(L.Items[I1]).FileTime      - TArcItem(L.Items[I2]).FileTime);
        alvcCRC     : Result :=                (TArcItem(L.Items[I1]).FileCRC       - TArcItem(L.Items[I2]).FileCRC);
        alvcMethod  : Result :=     CompareText(TArcItem(L.Items[I1]).FileMethod,     TArcItem(L.Items[I2]).FileMethod);
        alvcPassword: Result :=     CompareText(TArcItem(L.Items[I1]).FilePassword,   TArcItem(L.Items[I2]).FilePassword);
        alvcPosition: Result :=                (TArcItem(L.Items[I1]).FilePosition  - TArcItem(L.Items[I2]).FilePosition);
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

  procedure TCustomArcListView.QuickSort(List: TList; L, R: integer);
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

  procedure TCustomArcListView.SetFolderBox(Value: TComboBox);
  begin
    FFolderBox := Value;
    if Assigned(FFolderBox) then
    begin
      FFolderBox.Enabled := False;
      FFolderBox.Color := clInactiveBorder;
      FFolderBox.Style := csOwnerDrawVariable;
    end;
  end;

  procedure TCustomArcListView.SetSimpleList(Value: boolean);
  begin
    FSimpleList := Value;
    UpdateFolder;
  end;
  
  procedure TCustomArcListView.SetSortDir(Value: boolean);
  begin
    FSortDir := Value;
    UpdateFolder;
  end;
  
  procedure TCustomArcListView.SetSortCol(Value: TArcListViewColumn);
  begin
    if Value = FSortCol then
      FSortDir := not FSortDir
    else
      FSortDir := True;
      
    FSortCol := Value;
    UpdateFolder;
  end;

  procedure TCustomArcListView.UpdateFolder;
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
        if CompareFileName(FFolder, TArcItem(FFolders.Items[I]).FilePath) = 0 then
        begin
          FFolderFiles.Add(FFolders.Items[I]);
        end;

      for I := 0 to FFiles.Count - 1 do
        if CompareFileName(FFolder, TArcItem(FFiles.Items[I]).FilePath) = 0 then
        begin
          FFolderFiles.Add(FFiles.Items[I]);
        end;

      if (FFolderBoxSign <> '') and Assigned(FFolderBox) then
      begin
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
            if FFolderBox.Items.Count = 0 then
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
          FFolderBox.ItemIndex := FFolderBox.Items.AddObject
            (FFolderBoxSign + FFolder, Image);
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
    
    //if Assigned(FFolderBox) then
    //begin
    //end;
  end;

  procedure TCustomArcListView.Initialize;
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

  procedure TCustomArcListView.CloseArchive;
  begin
    FFileName := '';
    FFileLink := '';
    FFolderBoxSign := '';
    if Assigned(FFolderBox) then
    begin
      FFolderBox.Color := clInactiveBorder;
      FFolderBox.Enabled := False;
      FFolderBox.Clear;
    end;
    Color := clInactiveBorder;
    Enabled := False;
    // ---
    BeginUpdate;
    Items.Clear;
    EndUpdate;
    // ---
    FFiles.Clear;
    FFolderFiles.Clear;
    FDetails.Clear;
    FFolders.Clear;
    FFolder:= '';
  end;

  procedure TCustomArcListView.SetFolder(Value: string);
  begin
    if FileNamePos(FFolderBoxSign, Value) = 1 then
    begin
      System.Delete(Value, 1, Length(FFolderBoxSign));
    end;
    FFolder := IncludeTrailingBackSlash(Value);
    UpdateFolder;
  end;

  function TCustomArcListView.Up: boolean;
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
  
  function TCustomArcListView.OpenArchive(const FileName, FileLink: string): boolean;
  var
    FContents: TStringList;
    I, J, K: integer;
    Node: TArcItem;
  begin
    Result := False;
    CloseArchive;
    if FileExists(FileName) and FileExists(FileLink) then
    begin
      FFileLink := FileLink;
      FFileName := FileName;
      FFolderBoxSign := IncludeTrailingBackSlash(ExtractFileName(FFileName));
      // ---
      FContents := TStringList.Create;
      FContents.LoadFromFile(FFileLink);
      try
        I := 0;
        while I < FContents.Count do
        begin
          Node := TArcItem.Create;

          Node.FileName := (FContents.Strings[I +  0]);
          Node.FilePath := (FContents.Strings[I +  1]);

          TryStrToInt(FContents.Strings[I + 2], Node.FileSize);
          TryStrToInt(FContents.Strings[I + 3], Node.FilePacked);
          TryStrToInt(FContents.Strings[I + 4], Node.FileRatio);
          TryStrToInt(FContents.Strings[I + 5], Node.FileAttr);
          Node.FileAttr := Max(0, Node.FileAttr);
          TryStrToInt(FContents.Strings[I + 6], Node.FileTime);

          Node.FileComm :=           (FContents.Strings[I +  7]);
          Node.FileCrc  := StrToQWord(FContents.Strings[I +  8]);

          Node.FileMethod   := (FContents.Strings[I +  9]);
          Node.FileVersion  := (FContents.Strings[I + 10]);
          Node.FilePassword := (FContents.Strings[I + 11]);

          TryStrToInt(FContents.Strings[I + 12], Node.FilePosition);
          
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

          FDetails.Update(Node);
          FFiles.Add(Node);
          Inc(I, 13);
        end;
        Result := True;
      finally
        FContents.Free;
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
    end;
  end;
  
  function TCustomArcListView.UpdateFolders: boolean;
  var
    D: string;
    I, J, K: integer;
    Node: TArcItem;
  begin
    for I := 0 to FFiles.Count - 1 do
    begin
      D := ExcludeTrailingBackslash(TArcItem(FFiles.Items[I]).FilePath);
      while (Length(D) > 0) do
      begin
        if FFolders.IndexOf(ExtractFilePath(D), ExtractFileName(D)) = -1 then
        begin
          Node := TArcItem.Create;
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

  procedure TCustomArcListView.Data(Sender: TObject; Item: TListItem);
  begin
    if Assigned(Item) then
    begin
      with TArcItem(FFolderFiles.Items[Item.Index]) do
      begin
        Item.Caption := FileName;                                //  0 Name
        if (FileAttr and faDirectory) = 0 then
        begin
          Item.SubItems.Add(SizeToStr(FileSize));                //  1 Size
          Item.SubItems.Add(SizeToStr(FilePacked));              //  2 Packed
          Item.SubItems.Add(RatioToStr(FileRatio));              //  3 Ratio
          Item.SubItems.Add(FileType);                           //  4 Type
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
    RegisterComponents ('BeePackage', [TArcListView]);
  end;

initialization

  {$i beegui_archivelistviewmgr.lrs }

end.
