{
  Copyright (c) 1999-2007 Andrew Filinsky and Melchiorre Caruso

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

  Archive headers processing.

  Modifyed:

  v0.7.8 build 0150 - 2005.06.27 by Melchiorre Caruso;
  v0.7.8 build 0154 - 2005.07.23 by Melchiorre Caruso;
  v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;
  v0.7.9 build 0360 - 2006.06.02 by Melchiorre Caruso;

  v0.7.9 build 0828 - 2008.08.04 by Melchiorre Caruso.
}

unit Bee_Headers;

{$I compiler.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, // TSearchRec
  Classes,  // TList

  Bee_Configuration;

const

  // Id marker

  Marker: integer = 442852674;

type

  // Header flag

  THeaderFlag =
    (foVersion,  foMethod,   foDictionary, foTable,    foTear,     foMoved,
     foLast,     foPassword, fo09Unused,   fo10Unused, fo11Unused, fo12Unused,
     fo13Unused, fo14Unused, fo15Unused,   fo16Unused, fo17Unused, fo18Unused,
     fo19Unused, fo20Unused, fo21Unused,   fo22Unused, fo23Unused, fo24Unused,
     fo25Unused, fo26Unused, fo27Unused,   fo28Unused, fo29Unused, fo30Unused,
     fo31Unused, fo32Unused);

type

  // Header flags

  THeaderFlags = set of THeaderFlag;

type

  // Header action

  THeaderAction = (toUpdate, toFresh, toCopy,   toSwap,   toExtract, toTest,
                   toSkip,   toQuit,  toDelete, toRename, toList,    toNone);

type

  // Headers actions

  THeaderActions = set of THeaderAction;

type

  // Header structure, order of fields is significant

  THeader = class
  public
    // Start file header
    FileFlags: THeaderFlags;
    FileVersion: byte;
    FileMethod: byte;
    FileDictionary: byte;
    FileTable: TTableParameters;
    FileSize: integer;
    FileTime: integer;
    FileAttr: integer;
    FileCrc:  cardinal;
    FilePacked: integer;
    FileStartPos: integer;
    FileName: string;
    // End file header
  public
    Action: THeaderAction;
    FileLink: string;
  public
    constructor Create(const cdOption: string; const RecPath: string; const Rec: TSearchRec);
    procedure Fresh(const cdOption: string; const RecPath: string; const Rec: TSearchRec);
    constructor Read(Stream: TStream; aAction: THeaderAction);
    function SetTable(Config: TConfiguration): boolean;
    procedure Write(Stream: TStream);
    destructor Destroy; override;
  end;

type

  // Sorted headers list

  TSortedHeaders = class(TList)
  public
    function InsertItem(Item: pointer): integer;
    function SearchItem(const FileName: string): pointer;
  end;

type

  // Headers list

  THeaders = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItems(
      Masks: TStringList;
      const cdOption: string;
             fOption: boolean;
             rOption: boolean;
             uOption: boolean;
             xOption: TStringList;
        var Size: integer);

    function MarkItems(Masks: TStringList; MaskAct, aAction: THeaderAction; rOption: boolean): integer; overload;
    function MarkItems(const Mask: string; MaskAct, aAction: THeaderAction; rOption: boolean): integer; overload;
    procedure MarkItem(Index: integer; aAction: THeaderAction);
    procedure MarkAll(aAction: THeaderAction);

    procedure SortNews(Config: TConfiguration; sOption: boolean; kOption: boolean; const eOption: string);

    procedure ReadItemsB4b(Stream: TStream; aAction: THeaderAction);
    procedure ReadItems(Stream: TStream; aAction: THeaderAction);
    procedure WriteItems(Stream: TStream);

    function GetBack(Child: integer; aAction: THeaderAction): integer; overload;
    function GetNext(Child: integer; aAction: THeaderAction): integer; overload;
    function GetBack(Child: integer; Flag: THeaderFlag): integer; overload;
    function GetNext(Child: integer; Flag: THeaderFlag): integer; overload;
    function GetBack(Child: integer; aAction: THeaderAction; const aFileName: string): integer; overload;
    function GetNext(Child: integer; aAction: THeaderAction; const aFileName: string): integer; overload;
    function GetBack(Child: integer; aActions: THeaderActions; const aFileName: string): integer; overload;
    function GetNext(Child: integer; aActions: THeaderActions; const aFileName: string): integer; overload;

    function GetSize(aAction: THeaderAction): integer; overload;
    function GetSize(Actions: THeaderActions): integer; overload;
    function GetPackedSize(aAction: THeaderAction): integer; overload;
    function GetPackedSize(Actions: THeaderActions): integer; overload;

    function GetCount(Actions: THeaderActions): integer;

    function SetModule(const FileName: string): boolean;
    function GetModule: integer;
  private
    Module:  TStream;
  private
    procedure QuickSort(L, R: integer);
    procedure MarkAsLast(Action: THeaderAction);
    function  FindFirstMarker(Stream: TStream): integer;

    procedure ExpandMask(const Mask: string; Masks: TStringList; rOption: boolean);
    procedure ScanFileSystem(
      Mask: string;
      Sorted: TSortedHeaders;
      const cdOption: string;
             fOption: boolean;
             rOption: boolean;
             uOption: boolean;
             xOption: TStringList;
        var Size: integer);
  end;

implementation

uses
  Bee_Files,
  Bee_Common;

// Compare header function

function CompareFn(L: TList; Index1, Index2: integer): integer;
var
  Bool1, Bool2: boolean;
begin
  with THeader(L.Items[Index1]) do Bool1 := (Action = toUpdate);
  with THeader(L.Items[Index2]) do Bool2 := (Action = toUpdate);

  if (Bool1 and Bool2) then
  begin
    Result := CompareFileName(
      ExtractFileExt(THeader(L.Items[Index1]).FileName),
      ExtractFileExt(THeader(L.Items[Index2]).FileName));

    if Result = 0 then
      Result := CompareFileName(
        ExtractFileName(THeader(L.Items[Index1]).FileName),
        ExtractFileName(THeader(L.Items[Index2]).FileName));

    if Result = 0 then
      Result := CompareFileName(
        THeader(L.Items[Index1]).FileName,
        THeader(L.Items[Index2]).FileName);
  end else
    if Bool1 then
      Result := 1
    else
      if Bool2 then
        Result := -1
      else
        Result := Index1 - Index2;
end;

// THeader class

constructor THeader.Create(const cdOption: string; const RecPath: string; const Rec: TSearchRec);
begin
  Action := toUpdate;

  FileFlags := [foTear, foTable];
  FileVersion := 1; // Bee 0.3.x
  FileMethod := 1;
  FileDictionary := 2;
  FileTime := Rec.Time;
  FileCrc := cardinal(-1);
  FileName := cdOption + DeleteFileDrive(RecPath) + Rec.Name;
  // ---
  FileLink := RecPath + Rec.Name;
end;

procedure THeader.Fresh(const cdOption: string; const RecPath: string; const Rec: TSearchRec);
begin
  if Action = toCopy then
  begin
    Action := toFresh;
  end;

  FileTime := Rec.Time;
  FileName := cdOption + DeleteFileDrive(RecPath) + Rec.Name;
  // ---
  FileLink := RecPath + Rec.Name;
end;

constructor THeader.Read(Stream: TStream; aAction: THeaderAction);
var
  j: integer;
const
  sSecondPart = SizeOf(FileSize) + SizeOf(FileTime)   + SizeOf(FileAttr) +
                SizeOf(FileCrc)  + SizeOf(FilePacked) + SizeOf(FileStartPos);
begin
  Action := aAction;

  if Stream.Read(FileFlags, SizeOf(FileFlags)) <> SizeOf(FileFlags) then Fail;

  if foVersion in FileFlags then
    if Stream.Read(FileVersion, SizeOf(FileVersion)) <> SizeOf(FileVersion) then Fail;

  if foMethod in FileFlags then
    if Stream.Read(FileMethod, SizeOf(FileMethod)) <> SizeOf(FileMethod) then Fail;

  if foDictionary in FileFlags then
    if Stream.Read(FileDictionary, SizeOf(FileDictionary)) <> SizeOf(FileDictionary) then Fail;

  if foTable in FileFlags then
    if Stream.Read(FileTable, SizeOf(FileTable)) <> SizeOf(FileTable) then Fail;
      
  if Stream.Read(FileSize, sSecondPart) <> sSecondPart then Fail;

  if Stream.Read(j, SizeOf(j)) = SizeOf(j) then
    SetLength(FileName, j)
  else
    Fail;
  
  if j > 0 then
  begin
    if Stream.Read(FileName[1], j) = j then
    begin
      FileName := DoDirSeparators(FileName);
      FileLink := '';
    end else
      Fail;
  end;
end;

destructor THeader.Destroy;
begin
  FileName := '';
  FileLink := '';
  inherited Destroy;
end;

procedure THeader.Write(Stream: TStream);
var
  j: integer;
const
  sSecondPart = SizeOf(FileSize) + SizeOf(FileTime)   + SizeOf(FileAttr) +
                SizeOf(FileCrc)  + SizeOf(FilePacked) + SizeOf(FileStartPos);
begin
  Stream.Write(FileFlags, SizeOf(FileFlags));

  if foVersion in FileFlags then
    Stream.Write(FileVersion, SizeOf(FileVersion));

  if foMethod in FileFlags then
    Stream.Write(FileMethod, SizeOf(FileMethod));

  if foDictionary in FileFlags then
    Stream.Write(FileDictionary, SizeOf(FileDictionary));

  if foTable in FileFlags then
    Stream.Write(FileTable, SizeOf(FileTable));

  Stream.Write(FileSize, sSecondPart);

  j := Length(FileName);
  Stream.Write(j, SizeOf(j));

  if j > 0 then
  begin
    Stream.Write(FileName[1], j);
  end;
end;

function THeader.SetTable(Config: TConfiguration): boolean;
begin
  Result := Config.GetTable(ExtractFileExt(FileName), FileTable);
end;

/// Sorted header list class

function TSortedHeaders.SearchItem(const FileName: string): pointer;
var
  L, M, H: integer;
begin
  L := 0;
  M := -1;
  H := Count - 1;

  while H >= L do
  begin
    M := (L + H) div 2;
    if CompareFileName(FileName, THeader(Items[M]).FileName) > 0 then
      L := M + 1
    else
      if CompareFileName(FileName,THeader(Items[M]).FileName) < 0 then
        H := M - 1
      else
        H := -2;
  end;

  if H <> -2 then
    Result := nil
  else
    Result := Items[M];
end;

function TSortedHeaders.InsertItem(Item: pointer): integer;
var
  L, M, H: integer;
  FName: string;
begin
  L := 0;
  M := -1;
  H := Count - 1;

  FName := THeader(Item).FileName;
  while H >= L do
  begin
    M := (L + H) div 2;
    if  CompareFileName(FName, THeader(Items[M]).FileName) > 0 then
      L := M + 1
    else
      if CompareFileName(FName, THeader(Items[M]).FileName) < 0 then
        H := M - 1
      else
        H := -2;
  end;

  if M = -1 then
    Result := 0
  else
    if CompareFileName(FName, THeader(Items[M]).FileName) < 0 then
      Result := M
    else
      Result := M + 1;

  Insert(Result, Item);
end;

/// THeaders list class

constructor THeaders.Create;
begin
  inherited Create;
  Module := TMemoryStream.Create;
end;

destructor THeaders.Destroy;
var
  I: integer;
begin
  Module.Free;
  for I := Count - 1 downto 0 do
  begin
    THeader(Items[I]).Free;
  end;
  inherited Destroy;
end;


procedure THeaders.ExpandMask(const Mask: string; Masks: TStringList; rOption: boolean);
var
  I: integer;
  Error: integer;
  Rec: TSearchRec;
  Card: boolean;
  LastSlash: integer;
  FirstSlash: integer;
  FolderName: string;
  FolderPath: string;
begin
  Card := False;
  LastSlash := 0;
  FirstSlash := 0;
  for I := 1 to Length(Mask) do
  begin
    if Card = False then
    begin
      if Mask[I] in ['*', '?'] then
      begin
        Card := True;
      end;
      if Mask[I] = PathDelim then
      begin
        FirstSlash := I;
      end;
    end else
    begin
      if Mask[I] = PathDelim then
      begin
        LastSlash := I;
        Break;
      end;
    end;
  end;
  
  if LastSlash > 0 then
  begin
    FolderPath := Copy(Mask, 1, FirstSlash);
    FolderName := Copy(Mask, FirstSlash + 1, LastSlash - (FirstSlash + 1));
    Error := FindFirst(FolderPath + '*', faAnyFile, Rec);
    while Error = 0 do
    begin
      if ((Rec.Attr and faDirectory) = faDirectory) and (Rec.Name[1] <> '.') and (Rec.Name[1] <> '..') then
      begin
        if FileNameMatch(Rec.Name, FolderName, rOption) then
        begin
          ExpandMask(FolderPath + Rec.Name + Copy(Mask, LastSlash, (Length(Mask) + 1) - LastSlash), Masks, rOption);
        end;
      end;
      Error := FindNext(Rec);
    end;
    FindClose(Rec);
  end else
  begin
    Masks.Add(Mask);
  end;
end;

procedure THeaders.AddItems(
  Masks: TStringList;
  const cdOption: string;
         fOption: boolean;
         rOption: boolean;
         uOption: boolean;
         xOption: TStringList;
    var Size: integer);
var
  I, J: integer;
  CurrMasks: TStringList;
  Sorted: TSortedHeaders;
begin
  // Create sorted list item
  Sorted := TSortedHeaders.Create;
  for I := 0 to Count - 1 do
  begin
    Sorted.InsertItem(Items[I]);
  end;
  
  for I := 0 to Masks.Count - 1 do
  begin
    CurrMasks := TStringList.Create;
    ExpandMask(Masks.Strings[I], CurrMasks, rOption);
    for J := 0 to CurrMasks.Count - 1 do
    begin
      ScanFileSystem(
        CurrMasks.Strings[J],
        Sorted,
        cdOption,
         fOption,
         rOption,
         uOption,
         xOption,
        Size);
    end;
    CurrMasks.Free;
  end;
  Sorted.Free;
end;

function THeaders.MarkItems(Masks: TStringList; MaskAct: THeaderAction; aAction: THeaderAction; rOption: boolean): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    with THeader(Items[I]) do
    begin
      if (Action = MaskAct) and (FileNameMatch(FileName, Masks, rOption)) then
      begin
        Action := aAction;
        Inc(Result);
      end;
    end;
end;

function THeaders.MarkItems(const Mask: string; MaskAct: THeaderAction; aAction: THeaderAction; rOption: boolean): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    with THeader(Items[I]) do
    begin
      if (Action = MaskAct) and (FileNameMatch(FileName, Mask, rOption)) then
      begin
        Action := aAction;
        Inc(Result);
      end;
    end;
end;

procedure THeaders.MarkItem(Index: Integer; aAction: THeaderAction);
begin
  THeader(Items[Index]).Action := aAction;
end;

procedure THeaders.MarkAll(aAction: THeaderAction);
var
  I: integer;
begin
  for I := 0 to Count - 1 do
  begin
    THeader(Items[I]).Action := aAction;
  end;
end;

procedure THeaders.SortNews(Config: TConfiguration; sOption: Boolean; kOption: Boolean; const eOption: string);
var
  P: THeader;
  I, First, Method, Dictionary: integer;
  CurrentExt, PreviousExt: string;
begin
  QuickSort(0, Count - 1);

  Config.Selector('\main');
  Method := StrToInt(Config.CurrentSection.Values['Method']);
  Dictionary := StrToInt(Config.CurrentSection.Values['Dictionary']);
  Config.Selector('\m' + Config.CurrentSection.Values['Method']);

  First := GetNext(0, toUpdate);
  if First <> -1 then
  begin
    CurrentExt := '.';
    I := First;
    repeat
      P := Get(I);

      if I = First then
      begin
        P.FileFlags := P.FileFlags + [foVersion, foMethod, foDictionary];
      end;
      
      P.FileMethod := Method;
      P.FileDictionary := Dictionary;
      PreviousExt := CurrentExt;

      if Length(eOption) = 0 then
        CurrentExt := ExtractFileExt(P.FileName)
      else
        CurrentExt := eOption;

      if kOption then
        Include(P.FileFlags, foPassword);

      if (Method = 0) or (not Config.GetTable(CurrentExt, P.FileTable)) then
      begin
        Include(P.FileFlags, foMoved);
        Exclude(P.FileFlags, foTable);
      end else
        if CompareFileName(CurrentExt, PreviousExt) <> 0 then
        begin
          Include(P.FileFlags, foTable);
        end else
        begin
          Exclude(P.FileFlags, foTable);
          if sOption then
            Exclude(P.FileFlags, foTear);
        end;

      Inc(I);
    until I = Count;
  end;
end;

function THeaders.SetModule(const FileName: string): boolean;
var
  Strm: TStream;
begin
  Result := False;
  Module.Size := 0;
  if FileExists(FileName) then
  begin
    Strm := TFileReader.Create(FileName, fmOpenRead);
    try
      if Module.CopyFrom(Strm, Strm.Size) = Strm.Size then
        Result := True
      else
        Module.Size := 0;
    finally
      FreeAndNil(Strm);
    end;
  end;
end;

function THeaders.GetModule: integer;
begin
  if Assigned(Module) then
    Result := Module.Size
  else
    Result := -1;
end;

function THeaders.FindFirstMarker(Stream: TStream): integer;
var
  Id: integer;
  StrmPos: integer;
begin
  // archive type unknow
  Result := -1;

  StrmPos := Stream.Seek(0, 0);
  while Stream.Read(Id, SizeOf(integer)) = SizeOf(integer) do
  begin
    if Id = Marker then
    begin
      Result := StrmPos;
      Break;
    end;
    Inc(StrmPos, SizeOf(integer));
  end;

  // save sfx module
  if Result > 0 then
  begin
    Stream.Seek(0, 0);
    Module.Size := 0;
    Module.CopyFrom(Stream, Result);
  end;
end;

procedure THeaders.ReadItemsB4b(Stream: TStream; aAction: THeaderAction);
var
  P: THeader;
  Ptr: ^integer;
  Readed: byte;
  NextByte: integer;
  B4bMarker: array [0..3] of byte;
begin
  P := nil;
  Ptr := @B4bMarker;
  Ptr^ := Marker;

  NextByte := 0;
  Stream.Seek(0, 0);
  repeat
    if Stream.Read(Readed, 1) = 1 then
    begin
      if Readed = B4bMarker[NextByte] then
        Inc(NextByte)
      else
        NextByte := 0;

      if NextByte = SizeOf(integer) then
      begin
        NextByte := 0;
        try
          P := THeader.Read(Stream, aAction);
          Add(P);
        except
          P := nil;
        end;
      end;
    end else
      Break;

  until (P <> nil) and (foLast in P.FileFlags);

  if P <> nil then
  begin
    Exclude(P.FileFlags, foLast);
  end;
end;

procedure THeaders.ReadItems(Stream: TStream; aAction: THeaderAction);
var
  P:  THeader;
  Id: integer;
  OffSet: integer;
begin
  P := nil;
  OffSet := FindFirstMarker(Stream);

  if OffSet > -1 then
  begin
    Stream.Seek(OffSet, 0);
    repeat
      if (Stream.Read(Id, SizeOf(integer)) = SizeOf(integer)) and (Id = Marker) then
        try
          P := THeader.Read(Stream, aAction);
          Add(P);
        except
          P := nil;
        end
      else Break;
    until (P <> nil) and (foLast in P.FileFlags);

    if P <> nil then
    begin
      Exclude(P.FileFlags, foLast);
    end;
  end else
    ReadItemsB4b(Stream, aAction);
end;

procedure THeaders.WriteItems(Stream: TStream);
var
  I: integer;
begin
  if Stream.Seek(0, 1) = 0 then
  begin
    if Module.Size > 0 then
    begin
      Module.Seek(0, 0);
      Stream.CopyFrom(Module, Module.Size);
    end;
  end else
    Stream.Seek(Module.Size, 0);

  MarkAsLast(toDelete);
  for I := 0 to Count - 1 do
    if THeader(Items[I]).Action <> toDelete then
    begin
      Stream.Write(Marker, 4);
      THeader(Items[I]).Write(Stream);
    end;
end;

function THeaders.GetCount(Actions: THeaderActions): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if THeader(Items[I]).Action in Actions then
    begin
      Inc(Result);
    end;
end;

function THeaders.GetNext(Child: integer; aAction: THeaderAction): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child to Count - 1 do
    if THeader(Items[I]).Action = aAction then
    begin
      Result := I;
      Break;
    end;
end;

function THeaders.GetBack(Child: integer; aAction: THeaderAction): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child downto 0 do
    if THeader(Items[I]).Action = aAction then
    begin
      Result := I;
      Break;
    end;
end;

function THeaders.GetNext(Child: integer; Flag: THeaderFlag): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child to Count - 1 do
  begin
    if Flag in THeader(Items[I]).FileFlags then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function THeaders.GetBack(Child: integer; Flag: THeaderFlag): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child downto 0 do
  begin
    if Flag in THeader(Items[I]).FileFlags then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function THeaders.GetNext(Child: integer; aAction: THeaderAction; const aFileName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child to Count - 1 do
    with THeader(Items[I]) do
    begin
      if (Action = aAction) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
    end;
end;

function THeaders.GetBack(Child: integer; aActions: THeaderActions; const aFileName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child downto 0 do
    with THeader(Items[I]) do
    begin
      if (Action in aActions) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
    end;
end;

function THeaders.GetNext(Child: integer; aActions: THeaderActions; const aFileName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child to Count - 1 do
    with THeader(Items[I]) do
    begin
      if (Action in aActions) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
    end;
end;

function THeaders.GetBack(Child: integer; aAction: THeaderAction; const aFileName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child downto 0 do
    with THeader(Items[I]) do
    begin
      if (Action = aAction) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
    end;
end;

function THeaders.GetSize(aAction: THeaderAction): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if THeader(Items[I]).Action = aAction then
    begin
      Inc(Result, THeader(Items[I]).FileSize);
    end;
end;

function THeaders.GetPackedSize(aAction: THeaderAction): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if THeader(Items[I]).Action = aAction then
    begin
      Inc(Result, THeader(Items[I]).FilePacked);
    end;
end;

function THeaders.GetSize(Actions: THeaderActions): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if THeader(Items[I]).Action in Actions then
    begin
      Inc(Result, THeader(Items[I]).FileSize);
    end;
end;

function THeaders.GetPackedSize(Actions: THeaderActions): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if THeader(Items[I]).Action in Actions then
    begin
      Inc(Result, THeader(Items[I]).FilePacked);
    end;
end;

// Private procedure and function

procedure THeaders.QuickSort(L, R: integer);
var
  I, J, Pivot: integer;
begin
  if R < L then
    Exit;
  repeat
    I := L;
    J := R;

    Pivot := (L + R) div 2;
    repeat
      while CompareFn(Self, I, Pivot) < 0 do Inc(I);
      while CompareFn(Self, J, Pivot) > 0 do Dec(J);
      if I <= J then
      begin
        Exchange(I, J);

        if Pivot = I then
          Pivot := J
        else
          if Pivot = J then
            Pivot := I;

        Inc(I);
        Dec(j);
      end;
    until I > J;

    if L < J then QuickSort(L, J);

    L := I;
  until I >= R;
end;

procedure THeaders.MarkAsLast(Action: THeaderAction);
var
  I: integer;
begin
  for I := Count - 1 downto 0 do
    if THeader(Items[I]).Action <> Action then
    begin
      Include(THeader(Items[I]).FileFlags, foLast);
      Break;
    end;
end;

procedure THeaders.ScanFileSystem(
  Mask: string;
  Sorted: TSortedHeaders;
  const cdOption: string;
         fOption: boolean;
         rOption: boolean;
         uOption: boolean;
         xOption: TStringList;
    var Size: integer);
var
  P: THeader;
  J: pointer;
  Error: integer;
  Rec: TSearchRec;
  RecPath: string;
  RecName: string;
begin
  if (Length(Mask) > 0) and (Mask[Length(Mask)] = PathDelim) then
  begin
    Mask := IncludeTrailingBackSlash(Mask) + '*';
    rOption := True
  end else
    if DirectoryExists(Mask) then
    begin
      Mask := IncludeTrailingBackSlash(Mask) + '*';
      rOption := True;
    end;

  RecPath := ExtractFilePath(Mask);
  Error := FindFirst(RecPath + '*', faAnyFile, Rec);
  while Error = 0 do
  begin
    RecName := RecPath + Rec.Name;
    if (Rec.Attr and faDirectory) = 0 then
    begin
      if (FileNameMatch(RecName, Mask, rOption)) then
        if (FileNameMatch(RecName, xOption, rOption) = False) then
        begin
          J := Sorted.SearchItem(cdOption + DeleteFileDrive(RecName));
          if not (fOption xor uOption) then
          begin
            if (J = nil) then
            begin
              P := THeader.Create(cdOption, RecPath, Rec);
              Sorted.InsertItem(P);
              Add(P);
              Size := Size + Rec.Size;
            end else
            if (Rec.Time > THeader(J).FileTime) then
            begin
              THeader(J).Fresh(cdOption, RecPath, Rec);
              Size := Size + (Rec.Size - THeader(J).FileSize);
            end;
          end else
          begin
            if fOption then
            begin
              if (not (J = nil)) and (Rec.Time > THeader(J).FileTime) then
              begin
                THeader(J).Fresh(cdOption, RecPath, Rec);
                Size := Size + (Rec.Size - THeader(J).FileSize);
              end;
            end else
            begin
              if (J = nil) then
              begin
                P := THeader.Create(cdOption, RecPath, Rec);
                Sorted.InsertItem(P);
                Add(P);
                Size := Size + Rec.Size;
              end;
            end;
          end;
        end;
    end else
    begin
      if rOption and (Rec.Name <> '.') and (Rec.Name <> '..') then
      begin
        ScanFileSystem(
          IncludeTrailingBackSlash(RecName) + ExtractFileName(Mask),
          Sorted,
          cdOption,
           fOption,
           rOption,
           uOption,
           xOption,
          Size);
      end;
    end;
    Error := FindNext(Rec);
  end;
  FindClose(Rec);
end;

end.
