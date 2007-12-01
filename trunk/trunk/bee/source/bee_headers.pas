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

  v0.7.9 build 0497 - 2007.11.18 by Melchiorre Caruso;
}

unit Bee_Headers;

{$I compiler.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes,  // TList
  SysUtils, // TSearchRec

  Bee_Configuration;

// Id marker

const
  Marker: integer = 442852674;

// Header flags

type
  THeaderFlag =
    (foVersion, foMethod, foDictionary, foTable, foTear, foMoved, foLast,
    foPassword, fo09Unused, fo10Unused, fo11Unused, fo12Unused, fo13Unused, fo14Unused,
    fo15Unused, fo16Unused, fo17Unused, fo18Unused, fo19Unused, fo20Unused, fo21Unused,
    fo22Unused, fo23Unused, fo24Unused, fo25Unused, fo26Unused, fo27Unused, fo28Unused,
    fo29Unused, fo30Unused, fo31Unused, fo32Unused);

type
  THeaderFlags = set of THeaderFlag;

// Header action

type
  THeaderAction = (toUpdate, toFresh, toCopy, toSwap, toExtract,
    toTest, toSkip, toQuit, toDelete, toRename, toList, toNone);

// Headers actions

type
  THeaderActions = set of THeaderAction;

// Header structure, order of fields is significant

type
  THeader = class
  public
    // start file header
    Flags: THeaderFlags;
    Version: byte;
    Method: byte;
    Dictionary: byte;
    Table: TTableParameters;
    Size: integer;
    Time: integer;
    Attr: integer;
    Crc:  cardinal;
    PackedSize: integer;
    StartPos: integer;
    Name: string;
    // end file header
  public
    Action: THeaderAction;
    Option: string;
    RecSize: integer;
    constructor Create(const cdOption: string; const Rec: TSearchRec; const RecPath: string);
    function Fresh(const cdOption: string; const Rec: TSearchRec; const RecPath: string): integer;
    constructor Read(Stream: TStream; aAction: THeaderAction);
    function SetTable(Config: TConfiguration): boolean;
    procedure Write(Stream: TStream);
    destructor Destroy; override;
    function GetName: string;
  end;

// Sort headers list

type
  TSortHeaders = class(TList)
  public
    function InsertItem(Item: pointer): integer;
    function SearchItem(const FileName: string): pointer;
  end;

// Headers list

type
  THeaders = class(TList)
  public
    cdOption: string;
    fOption: boolean;
    uOption: boolean;
    xOption: TStringList;

    constructor Create;
    destructor Destroy; override;

    function AddItems(Masks: TStringList; rOption: boolean): integer;
    procedure MarkItems(Masks: TStringList; MaskAct, aAction: THeaderAction);
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
    function GetBack(Child: integer; aAction: THeaderAction; const aName: string): integer; overload;
    function GetNext(Child: integer; aAction: THeaderAction; const aName: string): integer; overload;
    function GetBack(Child: integer; aActions: THeaderActions; const aName: string): integer; overload;
    function GetNext(Child: integer; aActions: THeaderActions; const aName: string): integer; overload;

    function GetSize(aAction: THeaderAction): integer; overload;
    function GetSize(Actions: THeaderActions): integer; overload;
    function GetPackedSize(aAction: THeaderAction): integer; overload;
    function GetPackedSize(Actions: THeaderActions): integer; overload;

    function GetCount(Actions: THeaderActions): integer;

    function SetSFX(const FileName: string): boolean;
    function GetSFXsize: integer;
  private
    SfxStrm:  TStream;

    procedure MarkLast(Last: THeaderAction);
    procedure ExpandMask(const Mask: string; Masks: TStringList);
    
    procedure QuickSort(L, R: integer);
    function  FindFirstMarker(Stream: TStream): integer;
    procedure ScanFileSystem(SortHeaders: TSortHeaders; Mask: string; var Size: integer);
  end;

implementation

uses
  Bee_Files,
  Bee_Common;

// Compare header function

function CompareFn(L: TList; Index1, Index2: integer): integer;
var
  bool1, bool2: boolean;
begin
  with THeader(L.Items[Index1]) do bool1 := (Action = toUpdate);
  with THeader(L.Items[Index2]) do bool2 := (Action = toUpdate);

  if (bool1 and bool2) then
  begin
    Result := CompareFileName(
      ExtractFileExt(THeader(L.Items[Index1]).Name),
      ExtractFileExt(THeader(L.Items[Index2]).Name));

    if Result = 0 then
      Result := CompareFileName(
        ExtractFileName(THeader(L.Items[Index1]).Name),
        ExtractFileName(THeader(L.Items[Index2]).Name));

    if Result = 0 then
      Result := CompareFileName(
        THeader(L.Items[Index1]).Name,
        THeader(L.Items[Index2]).Name);
  end else
    if bool1 then
      Result := 1
    else
      if bool2 then
        Result := -1
      else
        Result := Index1 - Index2;
end;

// THeader

constructor THeader.Create(const cdOption: string; const Rec: TSearchRec; const RecPath: string);
begin
  Action := toUpdate;
  Option := cdOption;
  RecSize := Rec.Size;

  Flags := [foTear, foTable];
  Version := 1; // bee 0.3.x
  Method := 1;
  Dictionary := 2;
  Time := Rec.Time;
  Crc  := cardinal(-1);
  Name := RecPath + Rec.Name;
end;

function THeader.Fresh(const cdOption: string; const Rec: TSearchRec; const RecPath: string): integer;
begin
  Time    := Rec.Time;
  Name    := RecPath + Rec.Name;
  Result  := Rec.Size - RecSize;

  Option  := cdOption;
  RecSize := Rec.Size;

  if Action = toCopy then
    Action := toFresh;
end;

constructor THeader.Read(Stream: TStream; aAction: THeaderAction);
var
  J: integer;
const
  sSecondPart = SizeOf(Size) + SizeOf(Time) + SizeOf(Attr) +
    SizeOf(Crc) + SizeOf(PackedSize) + SizeOf(StartPos);
begin
  Action := aAction;
  Option := '';
  RecSize := 0;

  if Stream.Read(Flags, SizeOf(Flags)) <> SizeOf(Flags) then Fail;

  if foVersion in Flags then
    if Stream.Read(Version, SizeOf(Version)) <> SizeOf(Version) then Fail;

  if foMethod in Flags then
    if Stream.Read(Method, SizeOf(Method)) <> SizeOf(Method) then Fail;

  if foDictionary in Flags then
    if Stream.Read(Dictionary, SizeOf(Dictionary)) <> SizeOf(Dictionary) then Fail;

  if foTable in Flags then
    if Stream.Read(Table, SizeOf(Table)) <> SizeOf(Table) then Fail;
      
  if Stream.Read(Size, sSecondPart) <> sSecondPart then Fail;

  if Stream.Read(J, SizeOf(integer)) <> SizeOf(J) then Fail;

  SetLength(Name, J);
  if Stream.Read(Name[1], J) <> J then Fail;

  Bee_Common.DoDirSeparators(Name);
end;

destructor THeader.Destroy;
begin
  SetLength(Name, 0);
  SetLength(Option, 0);
  inherited Destroy;
end;

procedure THeader.Write(Stream: TStream);
var
  FName: string;
  LFName: integer;
const
  sSecondPart = SizeOf(Size) + SizeOf(Time) + SizeOf(Attr) +
    SizeOf(Crc) + SizeOf(PackedSize) + SizeOf(StartPos);
begin
  Stream.Write(Flags, SizeOf(Flags));

  if foVersion in Flags then
    Stream.Write(Version, SizeOf(Version));

  if foMethod in Flags then
    Stream.Write(Method, SizeOf(Method));

  if foDictionary in Flags then
    Stream.Write(Dictionary, SizeOf(Dictionary));

  if foTable in Flags then
    Stream.Write(Table, SizeOf(Table));

  Stream.Write(Size, sSecondPart);

  FName := GetName;
  LFName := Length(FName);
  Stream.Write(LFName, SizeOf(integer));
  Stream.Write(FName[1], LFName);
end;

function THeader.SetTable(Config: TConfiguration): boolean;
begin
  Result := Config.GetTable(ExtractFileExt(Name), Table);
end;

function THeader.GetName: string;
begin
  Result := Option + Bee_Common.DeleteFileDrive(Name);
end;

/// Sort header list

function TSortHeaders.SearchItem(const FileName: string): pointer;
var
  L, M, H: integer;
begin
  L := 0;
  M := -1;
  H := Count - 1;

  while H >= L do
  begin
    M := (L + H) div 2;
    if CompareFileName(FileName, THeader(Items[M]).GetName) > 0 then
      L := M + 1
    else
      if CompareFileName(FileName,THeader(Items[M]).GetName) < 0 then
        H := M - 1
      else
        H := -2;
  end;

  if H <> -2 then
    Result := nil
  else
    Result := Items[M];
end;

function TSortHeaders.InsertItem(Item: pointer): integer;
var
  L, M, H: integer;
  FName: string;
begin
  L := 0;
  M := -1;
  H := Count - 1;

  FName := THeader(Item).GetName;
  while H >= L do
  begin
    M := (L + H) div 2;
    if  CompareFileName(FName, THeader(Items[M]).GetName) > 0 then
      L := M + 1
    else
      if CompareFileName(FName, THeader(Items[M]).GetName) < 0 then
        H := M - 1
      else
        H := -2;
  end;

  if M = -1 then
    Result := 0
  else
    if CompareFileName(FName, THeader(Items[M]).GetName) < 0 then
      Result := M
    else
      Result := M + 1;

  Insert(Result, Item);
end;

/// THeaders list

constructor THeaders.Create;
begin
  inherited Create;
  cdOption := '';
  fOption := False;
  uOption := False;
  xOption := nil;

  SfxStrm := TMemoryStream.Create;
end;

destructor THeaders.Destroy;
var
  I: integer;
begin
  FreeAndNil(SfxStrm);
  for I := Count - 1 downto 0 do
  begin
    THeader(Items[I]).Destroy;
  end;
  inherited Destroy;
end;


procedure THeaders.ExpandMask(const Mask: string; Masks: TStringList);
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
      if ((Rec.Attr and faDirectory) > 0) and (Rec.Name[1] <> '.') then
      begin
        if FileNameMatch(Rec.Name, FolderName) then
        begin
          ExpandMask(FolderPath + Rec.Name +
            Copy(Mask, LastSlash, (Length(Mask) + 1) - LastSlash), Masks);
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

function THeaders.AddItems(Masks: TStringList; rOption: boolean): integer;
var
  I, J: integer;
  CurrMasks: TStringList;
  SortHeaders: TSortHeaders;
begin
  Result := 0;
  SortHeaders := TSortHeaders.Create;
  for I := 0 to Count - 1 do
  begin
    SortHeaders.InsertItem(Items[I]);
  end;
  
  for I := 0 to Masks.Count - 1 do
  begin
    CurrMasks := TStringList.Create;
    ExpandMask(Masks.Strings[I], CurrMasks);
    for J := 0 to CurrMasks.Count - 1 do
    begin
      ScanFileSystem(SortHeaders, CurrMasks.Strings[J], Result);
    end;
    FreeAndNil(CurrMasks);
  end;
  FreeAndNil(SortHeaders);
end;

procedure THeaders.MarkItems(Masks: TStringList; MaskAct: THeaderAction; aAction: THeaderAction);
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    with THeader(Items[I]) do
    begin
      if (Action = MaskAct) and (FileNameMatch(Name, Masks)) then
      begin
        Action := aAction;
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
  First, I, Method, Dictionary: integer;
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
        P.Flags := P.Flags + [foVersion, foMethod, foDictionary];
      end;
      
      P.Method := Method;
      P.Dictionary := Dictionary;
      PreviousExt := CurrentExt;

      if Length(eOption) = 0 then
        CurrentExt := ExtractFileExt(P.Name)
      else
        CurrentExt := eOption;

      if kOption then
        Include(P.Flags, foPassword);

      if (Method = 0) or (not Config.GetTable(CurrentExt, P.Table)) then
      begin
        Include(P.Flags, foMoved);
        Exclude(P.Flags, foTable);
      end else
        if CompareFileName(CurrentExt, PreviousExt) <> 0 then
        begin
          Include(P.Flags, foTable);
        end else
        begin
          Exclude(P.Flags, foTable);
          if sOption then
            Exclude(P.Flags, foTear);
        end;

      Inc(I);
    until I = Count;
  end;
end;

function THeaders.SetSFX(const FileName: string): boolean;
var
  Strm: TStream;
begin
  Result := False;
  SfxStrm.Size := 0;
  if FileExists(ExtractFilePath(ParamStr(0)) + FileName) then
  begin
    Strm := TFileReader.Create(ExtractFilePath(ParamStr(0)) + FileName, fmOpenRead);
    try
      SfxStrm.CopyFrom(Strm, Strm.Size);
      Result := True;
    finally
      FreeAndNil(Strm);
    end;
  end;
end;

function THeaders.GetSFXSize: integer;
begin
  if Assigned(SfxStrm) then
    Result := SfxStrm.Size
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
    SfxStrm.Size := 0;
    SfxStrm.CopyFrom(Stream, Result);
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

  until (P <> nil) and (foLast in P.Flags);

  if P <> nil then
  begin
    Exclude(P.Flags, foLast);
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
    until (P <> nil) and (foLast in P.Flags);

    if P <> nil then
    begin
      Exclude(P.Flags, foLast);
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
    if SfxStrm.Size > 0 then
    begin
      SfxStrm.Seek(0, 0);
      Stream.CopyFrom(SfxStrm, SfxStrm.Size);
    end;
  end else
    Stream.Seek(SfxStrm.Size, 0);

  MarkLast(toDelete);
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
      Exit;
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
      Exit;
    end;
end;

function THeaders.GetNext(Child: integer; Flag: THeaderFlag): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child to Count - 1 do
  begin
    if Flag in THeader(Items[I]).Flags then
    begin
      Result := I;
      Exit;
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
    if Flag in THeader(Items[I]).Flags then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function THeaders.GetNext(Child: integer; aAction: THeaderAction; const aName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child to Count - 1 do
    with THeader(Items[I]) do
    begin
      if (Action = aAction) and (CompareFileName(Name, aName) = 0) then
      begin
        Result := I;
        Exit;
      end;
    end;
end;

function THeaders.GetBack(Child: integer; aActions: THeaderActions; const aName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child downto 0 do
    with THeader(Items[I]) do
    begin
      if (Action in aActions) and (CompareFileName(Name, aName) = 0) then
      begin
        Result := I;
        Exit;
      end;
    end;
end;

function THeaders.GetNext(Child: integer; aActions: THeaderActions; const aName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child to Count - 1 do
    with THeader(Items[I]) do
    begin
      if (Action in aActions) and (CompareFileName(Name, aName) = 0) then
      begin
        Result := I;
        Exit;
      end;
    end;
end;

function THeaders.GetBack(Child: integer; aAction: THeaderAction; const aName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Child downto 0 do
    with THeader(Items[I]) do
    begin
      if (Action = aAction) and (CompareFileName(Name, aName) = 0) then
      begin
        Result := I;
        Exit;
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
      Inc(Result, THeader(Items[I]).Size);
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
      Inc(Result, THeader(Items[I]).PackedSize);
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
      Inc(Result, THeader(Items[I]).Size);
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
      Inc(Result, THeader(Items[I]).PackedSize);
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

procedure THeaders.MarkLast(Last: THeaderAction);
var
  I: integer;
begin
  for I := Count - 1 downto 0 do
    if THeader(Items[I]).Action <> Last then
    begin
      Include(THeader(Items[I]).Flags, foLast);
      Exit;
    end;
end;

procedure THeaders.ScanFileSystem(SortHeaders: TSortHeaders; Mask: string; var Size: integer);
var
  P: THeader;
  I: integer;
  J: pointer;
  Error: integer;
  Rec: TSearchRec;
  RecPath: string;
  RecName: string;
  Recursive: boolean;
begin
  I := System.Pos('!', Mask);
  if I > 0 then
  begin
    Recursive := True;
    repeat
      System.Delete(Mask, I, 1);
      I := System.Pos('!', Mask);
    until I = 0;
  end else
    Recursive := False;

  if (Length(Mask) > 0) and (Mask[Length(Mask)] = PathDelim) then
  begin
    Mask := Bee_Common.IncludeTrailingBackSlash(Mask) + '*';
    Recursive := True;
  end else
    if Bee_Common.DirectoryExists(Mask) then
    begin
      Mask := Bee_Common.IncludeTrailingBackSlash(Mask) + '*';
      Recursive := True;
    end;

  RecPath := ExtractFilePath(Mask);
  Error := FindFirst(RecPath + '*', faAnyFile, Rec);
  while Error = 0 do
  begin
    RecName := RecPath + Rec.Name;
    if (Rec.Attr and faDirectory) = 0 then
    begin
      if (FileNameMatch(RecName, Mask)) then
        if (FileNameMatch(RecName, xOption) = False) then
        begin
          J := SortHeaders.SearchItem(cdOption + Bee_Common.DeleteFileDrive(RecName));
          if not (fOption xor uOption) then
          begin
            if (J = nil) then
            begin
              P := THeader.Create(cdOption, Rec, RecPath);
              SortHeaders.InsertItem(P);
              Add(P);
              Inc(Size, Rec.Size);
            end else
            if (Rec.Time > THeader(J).Time) then
              Size := Size + THeader(J).Fresh(cdOption, Rec, RecPath);
          end else
          begin
            if fOption then
            begin
              if (not (J = nil)) and (Rec.Time > THeader(J).Time) then
                Size := Size + THeader(J).Fresh(cdOption, Rec, RecPath);
            end else
            begin
              if (J = nil) then
              begin
                P := THeader.Create(cdOption, Rec, RecPath);
                SortHeaders.InsertItem(P);
                Add(P);
                Inc(Size, Rec.Size);
              end;
            end;
          end;
        end;
    end else
    begin
      if Recursive and (Rec.Name <> '.') and (Rec.Name <> '..') then
      begin
        ScanFileSystem(SortHeaders, Bee_Common.IncludeTrailingBackSlash(RecName) +
          ExtractFileName(Mask) + '!', Size);
      end;
    end;
    Error := FindNext(Rec);
  end;
  FindClose(Rec);
end;

end.
