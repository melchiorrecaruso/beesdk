{
  Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

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

    TArcLisView class.

  Modifyed:

    v1.0.5 build 0840 - 2009.04.25 by Melchiorre Caruso.
}

unit BeeGui_ArchiveListViewMgr;

{$I compiler.inc}

interface

uses
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
    FileName:     string;
    FilePath:     string;
    FileType:     string;
    FileIcon:     integer;
    FileSize:     int64;
    FilePacked:   int64;
    FileRatio:    byte;
    FileAttr:     integer;
    FileTime:     integer;
    FileComm:     string;
    FileCrc:      cardinal;
    FileMethod:   string;
    FileVersion:  string;
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
    FilesCount:   integer;
    FilesSize:    cardinal;
    FilesPacked:  cardinal;
    FilesCrypted: integer;
    DirectoriesCount: integer;
    Version:      float;
    LastTime:     integer;
  public
    constructor Create;
    procedure Clear;
    procedure Update(Item: TArchiveItem);
  end;

type

  { TCustomArchiveListView }

  TCustomArchiveListView = class(TCustomListView)
  private
    FFileName:   string;
    FFiles:      TArchiveList;
    FFolders:    TArchiveList;
    FFolder:     string;
    FDetails:    TArchiveDetails;
    // ---
    FFolderBox:  TArchiveFolderBox;
    FFolderBoxSign: string;
    // ---
    FAutoLoad:   boolean;
    FSimpleList: boolean;
    FSortDirection: boolean;
    // ---
    FOnChangeFolder: TNotifyEvent;
  private
    function GetSelTime: integer;
    function GetSelSize: int64;
    function GetSelPackedSize: int64;

    procedure SetFolder(Value: string);
    procedure SetFolderBox(Value: TArchiveFolderBox);
    procedure SetAutoLoad(Value: boolean);
    procedure SetSimpleList(Value: boolean);
    procedure SetData(AItem: TListItem; AData: Pointer);

    procedure UpdateFolderBox(const AFolder: string);
    function UpdateFolders: boolean;
    procedure UpdateFolder;

    function GetFileIcon(const AFileName: string; AFileAttr: integer): integer;
    function GetFileType(const AFileName: string; AFileAttr: integer): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFolderBox;
    // ---
    function Open(const AArchiveName: string; AArchiveList: TList): boolean;
    procedure CloseArchive(Clean: boolean);
    procedure SortBy(ColumnIndex: integer);
    function Up: boolean;
    // ---
    procedure ClearMasks;
    procedure InvertMasks;
    procedure GetMasks(FileMasks: TStringList; Relative: boolean);
    procedure SetMask(const Mask: string; Value: boolean);

    function CompareFn(Item1, Item2: TListItem): integer;
  public
    property FileName: string read FFileName;
    property Folder: string read FFolder write SetFolder;
    property FolderBox: TArchiveFolderBox read FFolderBox write SetFolderBox default nil;
    property Details: TArchiveDetails read FDetails;

    property SelFilePackedSize: int64 read GetSelPackedSize;
    property SelFileSize: int64 read GetSelSize;
    property SelFileTime: integer read GetSelTime;
  published
    property SimpleList: boolean read FSimpleList write SetSimpleList default False;
    property AutoLoad: boolean read FAutoLoad write SetAutoLoad default False;
  published
    property OnChangeFolder: TNotifyEvent read FOnChangeFolder write FOnChangeFolder;
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
    // ---
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
    // property Checkboxes;
    // property Color default clWindow;
    property Columns;
    property ColumnClick;
    property Constraints;
    property DragCursor;
    property DragMode;
    // property Enabled;
    property Font;
    // property HideSelection;
    // property Items;
    property LargeImages;
    // property MultiSelect;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    // property ReadOnly;
    property RowSelect;
    property ScrollBars;
    // property ShowColumnHeaders;
    property SmallImages;
    property SortColumn;
    // property SortType;
    // property StateImages;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDrag;
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
  for I := 0 to Count -1 do
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
    if CompareFileName(TArchiveItem(Items[I]).FileName, FileName) = 0 then
    begin
      if CompareFileName(TArchiveItem(Items[I]).FilePath, FilePath) = 0 then
      begin
        Result := I;
        Break;
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
  FilesCount := 0;
  FilesSize := 0;
  FilesPacked := 0;
  FilesCrypted := 0;
  DirectoriesCount := 0;
  Version := 0;
  LastTime := 0;
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
        Version := StrToFloat(Item.FileVersion);
    except
      Version := -1;
    end;
  end
  else
    Inc(DirectoriesCount);
end;

{ TCustomArcListView }

constructor TCustomArchiveListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // ---
  FFolder  := '';
  FFileName := '';
  // ---
  FFiles   := TArchiveList.Create;
  FFolders := TArchiveList.Create;
  FDetails := TArchiveDetails.Create;
  // ---
  FAutoLoad := False;
  FSimpleList := False;
  FSortDirection := False;
  // ---
  Color    := clInactiveBorder;
  MultiSelect := True;
  SortType := stNone;
  ReadOnly := True;
  Enabled  := False;
end;

destructor TCustomArchiveListView.Destroy;
begin
  FFiles.Destroy;
  FFolders.Destroy;
  FDetails.Destroy;
  inherited Destroy;
end;

function TCustomArchiveListView.CompareFn(Item1, Item2: TListItem): integer;
var
  AI1, AI2:   TArchiveItem;
  AIB1, AIB2: boolean;
begin
  AI1 := TArchiveItem(Item1.Data);
  AI2 := TArchiveItem(Item2.Data);

  AIB1 := not ((AI1.FileAttr and faDirectory) = faDirectory);
  AIB2 := not ((AI2.FileAttr and faDirectory) = faDirectory);

  if AIB1 xor AIB2 then
  begin
    if AIB1 then
      Result := 1
    else
      Result := -1;
  end else
  begin
    case SortColumn of
      0: Result  := CompareFileName(AI1.FileName,     AI2.FileName);
      1: Result  :=                (AI1.FileSize    - AI2.FileSize);
      2: Result  :=                (AI1.FilePacked  - AI2.FilePacked);
      3: Result  :=                (AI1.FileRatio   - AI2.FileRatio);
      4: Result  := CompareFileName(AI1.FileType,     AI2.FileType);
      5: Result  := Round          (AI1.FileTime    - AI2.FileTime);
      6: Result  :=                (AI1.FileAttr    - AI2.FileAttr);
      7: Result  := CompareText    (AI1.FileMethod,   AI2.FileMethod);
      8: Result  := CompareText    (AI1.FilePassword, AI2.FilePassword);
      9:
      begin
        if AI1.FileCRC > AI2.FileCRC then
          Result := -1
        else
          if AI1.FileCRC < AI2.FileCRC then
            Result := 1
          else
            Result  := 0;
      end;
      10: Result := CompareFileName(AI1.FilePath,      AI2.FilePath);
      11: Result :=                (AI1.FilePosition - AI2.FilePosition);
    end;

    if FSortDirection then
    begin
      if Result > 0 then
        Result := -1
      else
      if Result < 0 then
        Result := 1;
    end;
  end;
end;

procedure TCustomArchiveListView.SortBy(ColumnIndex: integer);
var
  I: integer;
begin
  SortType := stNone;
  if (ColumnIndex > -1) and (ColumnIndex < Columns.Count) then
  begin
    if SortColumn = ColumnIndex then
      FSortDirection := not FSortDirection
    else
      FSortDirection := False;
    SortColumn := ColumnIndex;
  end;

  for I := 0 to Columns.Count - 1 do
  begin
    Columns[I].ImageIndex := -1;
  end;

  if FSortDirection then
    Columns[SortColumn].ImageIndex := GetFileIcon('.@sortdown', faDirectory)
  else
    Columns[SortColumn].ImageIndex := GetFileIcon('.@sortup', faDirectory);

  SortType := stData;
end;

procedure TCustomArchiveListView.SetFolderBox(Value: TArchiveFolderBox);
begin
  FFolderBox := Value;
  if Assigned(FFolderBox) then
  begin
    FFolderBox.Enabled := False;
    FFolderBox.Color   := clInactiveBorder;
  end;
end;

procedure TCustomArchiveListView.SetSimpleList(Value: boolean);
begin
  if Value <> FSimpleList then
  begin
    FSimpleList := Value;
    FFolder := '';
  end;
  UpdateFolder;
end;

procedure TCustomArchiveListView.UpdateFolder;
var
  I, J:  integer;
  Image: TBitmap;
begin
  Items.BeginUpdate;
  Items.Clear;

  if FSimpleList then
  begin
    for I := 0 to FFiles.Count - 1 do
      SetData(Items.Add, FFiles.Items[I]);
  end
  else
  begin
    for I := 0 to FFolders.Count - 1 do
      if CompareFileName(FFolder, TArchiveItem(FFolders.Items[I]).FilePath) = 0 then
      begin
        SetData(Items.Add, FFolders.Items[I]);
      end;

    for I := 0 to FFiles.Count - 1 do
      if CompareFileName(FFolder, TArchiveItem(FFiles.Items[I]).FilePath) = 0 then
      begin
        SetData(Items.Add, FFiles.Items[I]);
      end;

    if (Items.Count > 0) and Assigned(FFolderBox) then
    begin
      if FAutoLoad then
        LoadFolderBox;

      J := -1;
      for I := 0 to FFolderBox.Items.Count - 1 do
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
            I := GetFileIcon('.bee', faArchive)
          else
            I := GetFileIcon('.@folderclose', faDirectory);

          if I > -1 then
          begin
            Image := TBitmap.Create;
            TIconList(SmallImages).GetBitmap(I, Image);
          end
          else
            Image := nil;

          FFolderBox.ItemIndex :=
            FFolderBox.Items.AddObject(FFolderBoxSign + FFolder, Image);
        end;
      end
      else
        FFolderBox.ItemIndex := J;
    end;
  end;
  Items.EndUpdate;
  SortBy(-1);

  if Enabled and (Items.Count > 0) then
  begin
    if Assigned(FOnChangeFolder) then
    begin
      FOnChangeFolder(Self);
    end;
    ItemFocused := Items[0];
  end;
end;

procedure TCustomArchiveListView.CloseArchive(Clean: boolean);
begin
  if Assigned(FFolderBox) then
  begin
    FFolderBox.Color   := clInactiveBorder;
    FFolderBox.Enabled := False;
  end;
  Color   := clInactiveBorder;
  Enabled := False;

  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;

  FFiles.Clear;
  FDetails.Clear;
  FFolders.Clear;

  if Assigned(FFolderBox) then
  begin
    FFolderBox.Clear;
  end;

  if Clean then
  begin
    if Assigned(FOnChangeFolder) then
    begin
      FOnChangeFolder(Self);
    end;
    FFolderBoxSign := '';
    FFileName := '';
    FFolder := '';
  end;
end;

procedure TCustomArchiveListView.SetFolder(Value: string);
begin
  if FileNamePos(FFolderBoxSign, Value) = 1 then
  begin
    System.Delete(Value, 1, Length(FFolderBoxSign));
  end;
  Value := ExcludeTrailingBackSlash(Value);

  if Length(Value) > 0 then
  begin
    while FFolders.IndexOf(ExtractFilePath(Value), ExtractFileName(Value)) = -1 do
    begin
      Value := ExcludeTrailingBackSlash(ExtractFilePath(Value));
      if Length(Value) = 0 then
      begin
        Break;
      end;
    end;
  end;
  FFolder := IncludeTrailingBackSlash(Value);
  UpdateFolder;
end;

function TCustomArchiveListView.Up: boolean;
var
  D: string;
begin
  Result := True;
  D      := ExtractFilePath(ExcludeTrailingBackSlash(FFolder));
  if CompareFileName(FFolder, D) <> 0 then
    SetFolder(D)
  else
    Result := False;
end;

procedure TCustomArchiveListView.GetMasks(FileMasks: TStringList; Relative: boolean);
var
  S: string;
  I, J: integer;
  Node: TArchiveItem;
begin
  for I := 0 to Items.Count -1 do
  begin
    if Items[I].Selected then
    begin
      Node := TArchiveItem(Items[I].Data);
      if (Node.FileAttr and faDirectory) = faDirectory then
      begin

        if Relative then
        begin
          // Relative Path-Name
          S := IncludeTrailingBackSlash(Node.FilePath + Node.FileName);
          for J := 0 to FFiles.Count -1 do
            with TArchiveItem(FFiles.Items[J]) do
              if FileNamePos(S, FilePath) = 1 then
              begin
                FileMasks.Add(DeleteFilePath(Node.FilePath, FilePath + FileName));
              end;
        end else
        begin
          // Absolute Path-Name
          S := IncludeTrailingBackSlash(Node.FilePath + Node.FileName);
          for J := 0 to FFiles.Count -1 do
            with TArchiveItem(FFiles.Items[J]) do
              if FileNamePos(S, FilePath) = 1 then
              begin
                FileMasks.Add(FilePath + FileName);
              end;
        end;

      end else
      begin
        if Relative then
          // Relative Path-Name
          FileMasks.Add(Node.FileName)
        else
          // Absolute Path-Name
          FileMasks.Add(Node.FilePath + Node.FileName);
      end;
    end;
  end;
end;

procedure TCustomArchiveListView.ClearMasks;
var
  I: integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    Items[I].Selected := False;
  end;
end;

procedure TCustomArchiveListView.InvertMasks;
var
  I: integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    Items[I].Selected := not Items[I].Selected;
  end;
end;

procedure TCustomArchiveListView.SetMask(const Mask: string; Value: boolean);
var
  I:    integer;
  Node: TArchiveItem;
begin
  for I := 0 to Items.Count -1 do
  begin
    Node := TArchiveItem(Items[I].Data);
    if FileNameMatch(Node.FileName, Mask, False) then
    begin
      Items[I].Selected := Value;
    end;
  end;
end;

function TCustomArchiveListView.GetSelSize: int64;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if Items[I].Selected then
    begin
      Inc(Result, TArchiveItem(Items[I].Data).FileSize);
    end;
end;

function TCustomArchiveListView.GetSelPackedSize: int64;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if Items[I].Selected then
    begin
      Inc(Result, TArchiveItem(Items[I].Data).FilePacked);
    end;
end;

function TCustomArchiveListView.GetSelTime: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if Items[I].Selected then
    begin
      Result := Max(Result, TArchiveItem(Items[I].Data).FileTime);
    end;
end;

function TCustomArchiveListView.Open(const AArchiveName: string;
  AArchiveList: TList): boolean;
var
  Node: TArchiveItem;
begin
  if CompareFileName(AArchiveName, FFileName) <> 0 then
  begin
    FFolder := '';
  end;
  FFileName      := AArchiveName;
  FFolderBoxSign := ExtractFileName(FFileName) + PathDelim;

  FFiles.Clear;
  FDetails.Clear;
  while AArchiveList.Count > 0 do
  begin
    Node := TArchiveItem(AArchiveList.Items[0]);
    Node.FileIcon := GetFileIcon(Node.FileName, Node.FileAttr);
    Node.FileType := GetFileType(Node.FileName, Node.FileAttr);

    FFiles.Add(Node);
    FDetails.Update(Node);
    AArchiveList.Delete(0);
  end;
  UpdateFolders;

  if Assigned(FFolderBox) then
  begin
    FFolderBox.Color   := clWindow;
    FFolderBox.Enabled := True;
  end;
  Color   := clWindow;
  Enabled := True;
  Result  := True;
end;

function TCustomArchiveListView.UpdateFolders: boolean;
var
  D:    string;
  I, J: integer;
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

        begin
          Node.FileSize   := 0;
          Node.FilePacked := 0;
          Node.FileRatio  := 0;
          Node.FileTime   := 0;
          for J := 0 to FFiles.Count - 1 do
            with TArchiveItem(FFiles.Items[J]) do
              if FileNamePos(IncludeTrailingBackSlash(D), FilePath) = 1 then
              begin
                Inc(Node.FileSize, FileSize);
                Inc(Node.FilePacked, FilePacked);
                Node.FileTime := Max(Node.FileTime, FileTime);
              end;

          if Node.FileSize > 0 then
            Node.FileRatio := Round(100 * (Node.FilePacked / Node.FileSize))
          else
            Node.FileRatio := 0;
        end;

        Node.FileAttr     := faDirectory;
        Node.FileCRC      := cardinal(-1);
        Node.FilePosition := -1;

        Node.FileIcon := GetFileIcon('.@folderclose', faDirectory);
        Node.FileType := GetFileType('.@folderclose', faDirectory);

        FDetails.Update(Node);
        FFolders.Add(Node);
      end;
      D := ExtractFileDir(D);
    end;
  end;
  D      := ExcludeTrailingBackSlash(FFolder);
  Result := FFolders.IndexOf(ExtractFilePath(D), ExtractFileName(D)) <> -1;
end;

procedure TCustomArchiveListView.SetAutoLoad(Value: boolean);
begin
  FAutoLoad := Value;
  if FAutoLoad then
    LoadFolderBox;
end;

procedure TCustomArchiveListView.UpdateFolderBox(const AFolder: string);
var
  J, K, G: integer;
  Image:   TBitmap;
begin
  if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
    if Assigned(FFolderBox) then
    begin
      K := -1;
      for J := 0 to FFolderBox.Items.Count - 1 do
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
          G := GetFileIcon('.bee', faArchive)
        else
          G := GetFileIcon('.@folderclose', faDirectory);

        if G > -1 then
        begin
          Image := TBitmap.Create;
          TIconList(SmallImages).GetBitmap(G, Image);
        end
        else
        begin
          Image := nil;
        end;
        FFolderBox.Items.AddObject(FFolderBoxSign + AFolder, Image);
      end;
    end;
end;

procedure TCustomArchiveListView.LoadFolderBox;
var
  I: integer;
begin
  if Assigned(FFolderBox) and (FFileName <> '') then
  begin
    FFolderBox.Clear;

    UpdateFolderBox('');
    for I := 0 to FFolders.Count - 1 do
    begin
      with TArchiveItem(FFolders.Items[I]) do
        UpdateFolderBox(IncludeTrailingBackSlash(FilePath + FileName));
    end;
    FFolderBox.Sorted := True;
  end;
end;

procedure TCustomArchiveListView.SetData(AItem: TListItem; AData: Pointer);
begin
  AItem.Data := AData;
  with TArchiveItem(AItem.Data) do
  begin
    AItem.Caption := FileName;                               //  0 Name
    if (FileAttr and faDirectory) = 0 then
    begin
      AItem.SubItems.Add(SizeToStr(FileSize));               //  1 Size
      AItem.SubItems.Add(SizeToStr(FilePacked));             //  2 Packed
      AItem.SubItems.Add(RatioToStr(FileRatio));             //  3 Ratio
      if FileType = '' then                                  //  4 Type
      begin
        FileType := 'File ' + ExtractFileExt(FileName);
      end;
      AItem.SubItems.Add(FileType);
      try                                                    //  5 Time
        AItem.SubItems.Add(FileTimeToString(FileTime, 'dd/mm/yyyy hh:mm'));
      except
        AItem.SubItems.Add('');
      end;
      AItem.SubItems.Add(AttrToStr(FileAttr));               //  6 Attr
      AItem.SubItems.Add(FileMethod);                        //  7 Method
      AItem.SubItems.Add(FilePassword);                      //  8 Password
      AItem.SubItems.Add(Hex(FileCRC, SizeOf(FileCRC)));     //  9 CRC
      AItem.SubItems.Add(FilePath);                          // 10 Path
      AItem.SubItems.Add(IntToStr(FilePosition));            // 11 Position
    end
    else
    begin
      AItem.SubItems.Add(SizeToStr(FileSize));               //  1 Size
      AItem.SubItems.Add(SizeToStr(FilePacked));             //  2 Packed
      AItem.SubItems.Add(RatioToStr(FileRatio));             //  3 Ratio
      if FileType = '' then                                  //  4 Type
      begin
        FileType := 'Files folder';
      end;
      AItem.SubItems.Add(FileType);
      try                                                    //  5 Time
        AItem.SubItems.Add(FileTimeToString(FileTime, 'dd/mm/yyyy hh:mm'));
      except
        AItem.SubItems.Add('');
      end;
      AItem.SubItems.Add(AttrToStr(FileAttr));               //  6 Attr
      AItem.SubItems.Add('');                                //  7 Method
      AItem.SubItems.Add('');                                //  8 Password
      AItem.SubItems.Add('');                                //  9 CRC
      AItem.SubItems.Add(FilePath);                          // 10 Path
      AItem.SubItems.Add('');                                // 11 Position
    end;
    AItem.ImageIndex := FileIcon;
  end;
end;

function TCustomArchiveListView.GetFileIcon(const AFileName: string;
  AFileAttr: integer): integer;
var
  I, J: integer;
begin
  if Assigned(LargeImages) and (LargeImages.ClassType = TIconList) then
    I := TIconList(LargeImages).FileIcon(AFileName, AFileAttr)
  else
    I := -1;

  if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
    J := TIconList(SmallImages).FileIcon(AFileName, AFileAttr)
  else
    J := -1;

  if I = J then
    Result := I
  else
    Result := -1;
end;

function TCustomArchiveListView.GetFileType(const AFileName: string;
  AFileAttr: integer): string;
begin
  if Assigned(SmallImages) and (SmallImages.ClassType = TIconList) then
  begin
    Result := TIconList(SmallImages).FileType(AFileName, AFileAttr);
  end
  else
    Result := '';

  if Assigned(LargeImages) and (LargeImages.ClassType = TIconList) then
  begin
    Result := TIconList(LargeImages).FileType(AFileName, AFileAttr);
  end
  else
    Result := '';
end;

{ Register }

procedure Register;
begin
  RegisterComponents('BeePackage', [TArchiveListView]);
end;

initialization

  {$i beegui_archivelistviewmgr.lrs }

end.
