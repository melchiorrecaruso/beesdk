{
  Copyright (c) 1999-2010 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1120 - 2010.05.06 by Melchiorre Caruso.
}

unit Bee_Headers;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  Bee_Files,
  Bee_CommandLine,
  Bee_Configuration;

type
  { Header flags }

  THeaderFlag =
   (foVersion,  foMethod,   foDictionary, foTable,    foTear,     foMoved,
    foLast,     foPassword, fo09Unused,   fo10Unused, fo11Unused, fo12Unused,
    fo13Unused, fo14Unused, fo15Unused,   fo16Unused, fo17Unused, fo18Unused,
    fo19Unused, fo20Unused, fo21Unused,   fo22Unused, fo23Unused, fo24Unused,
    fo25Unused, fo26Unused, fo27Unused,   fo28Unused, fo29Unused, fo30Unused,
    fo31Unused, fo32Unused);

  THeaderFlags = set of THeaderFlag;

  // Header actions

  THeaderAction =
   (haAdd,  haUpdate, haCopy, haExtract, haDecode,
    haSkip, haDelete, haNone, haOther);

  THeaderActions = set of THeaderAction;

  // Header structure, order of fields is significant

  THeader = class
  private
    FileLink: string;
    FileAction: THeaderAction;
  public
    // - Start header data - //
    FileFlags: THeaderFlags;
    FileVersion: byte;
    FileMethod: byte;
    FileDictionary: byte;
    FileTable: TTableParameters;
    FileSize: int64;
    FileTime: longint;
    FileAttr: longint;
    FileCrc: longword;
    FilePacked: int64;
    FileStartPos: int64;
    FileName: string;
    // - End header data - //
  end;

  // Headers list                                           

  THeaders = class
  private
    FNews: longint;
    FModule: TStream;
    FPrimary: TList;
    FSecondary: TList;
    FCL: TCommandLine;
    procedure AddItem(P: THeader); overload;
    function Compare(P1, P2: THeader): longint;
    function CreateItem(const Rec: TCustomSearchRec): THeader; overload;
    function CreateItem(aStream: TStream; aAction: THeaderAction; var aVersion: byte): THeader; overload;
    function FindFirstMarker(aStream: TStream): int64;
    procedure MarkAsLast(aAction: THeaderAction);
    procedure ReadItemsB4b(aStream: TStream; aAction: THeaderAction);
    procedure WriteItem(aStream: TStream; P: THeader; var aVersion: byte);
  public
    constructor Create(aCommandLine: TCommandLine);
    destructor Destroy; override;
    function SearchItem(FileName: string): THeader;
    function AlreadyFileExists(const aIndex: longint; const aActions:
      THeaderActions; const aFileName: string): longint; overload;
    function AlreadyFileExists(const aFileName: string): longint; overload;

    function AddItem(const Rec: TCustomSearchRec): int64; overload;
    function UpdateItem(const Rec: TCustomSearchRec): int64;
    function ReplaceItem(const Rec: TCustomSearchRec): int64;
    function AddUpdateItem(const Rec: TCustomSearchRec): int64;
    function AddReplaceItem(const Rec: TCustomSearchRec): int64;
    function AddAutoRenameItem(const Rec: TCustomSearchRec): int64;

    procedure SortNews(aConfiguration: TConfiguration);

    procedure ReadItems(aStream: TStream; aAction: THeaderAction);
    procedure WriteItems(aStream: TStream);

    function  MarkItems(Masks: TStringList; MaskAct, aAction: THeaderAction): longint; overload;
    function  MarkItems(Mask: string; MaskAct, aAction: THeaderAction): longint; overload;
    procedure MarkItem(aIndex: longint; aAction: THeaderAction);
    procedure MarkAll(aAction: THeaderAction);

    function GetBack(aChild: longint; aAction: THeaderAction): longint; overload;
    function GetNext(aChild: longint; aAction: THeaderAction): longint; overload;
    function GetBack(aChild: longint; aFlag: THeaderFlag): longint; overload;
    function GetNext(aChild: longint; aFlag: THeaderFlag): longint; overload;
    function GetBack(aChild: longint; aAction: THeaderAction; const aFileName: string): longint; overload;
    function GetNext(aChild: longint; aAction: THeaderAction; const aFileName: string): longint; overload;
    function GetBack(aChild: longint; aActions: THeaderActions; const aFileName: string): longint; overload;
    function GetNext(aChild: longint; aActions: THeaderActions; const aFileName: string): longint; overload;

    function GetSize(aAction: THeaderAction): int64; overload;
    function GetSize(aActions: THeaderActions): int64; overload;
    function GetPackedSize(aAction: THeaderAction): int64; overload;
    function GetPackedSize(aActions: THeaderActions): int64; overload;

    function GetCount(Actions: THeaderActions): longint; overload;
    function GetCount: longint; overload;

    function GetItem(aIndex: longint): THeader;

    function SetModule(const aFileName: string): boolean;
    function GetModule: longint;
  end;

implementation

uses
  // Math,
  Bee_Types,
  Bee_Consts,
  Bee_Common;

// THeaders class

constructor THeaders.Create(aCommandLine: TCommandLine);
begin
  inherited Create;
  FCL := aCommandLine;
  FModule := TMemoryStream.Create;
  FSecondary := TList.Create;
  FPrimary := TList.Create;
  FNews := 0;
end;

destructor THeaders.Destroy;
var
  I: longint;
begin
  FNews := 0;
  FModule.Size := 0;
  FModule.Destroy;

  for I := 0 to FPrimary.Count - 1 do
    THeader(FPrimary.Items[I]).Free;

  FPrimary.Clear;
  FPrimary.Destroy;
  FSecondary.Clear;
  FSecondary.Destroy;
  FCL := nil;
  inherited Destroy;
end;

function THeaders.CreateItem(const Rec: TCustomSearchRec): THeader;
begin
  Result := THeader.Create;
  try
    // - Start header data - //
    Result.FileFlags := [foTear, foTable];
    { TODO : Verify Method - Dictionary }
    Result.FileVersion    := Ord(FCL.hvOption);
    Result.FileMethod     := Ord(moFast);
    Result.FileDictionary := Ord(do5MB);
    // Result.FileTable
    Result.FileSize     := Rec.FileSize;
    Result.FileTime     := Rec.FileTime;
    Result.FileAttr     := Rec.FileAttr;
    Result.FileCrc      := longword(-1);
    Result.FilePacked   := 0;
    Result.FileName     := FCL.cdOption + Rec.FileName;
    // - End header data - //
    Result.FileLink     := Rec.FileLink;
    Result.FileStartPos := 0;
    Result.FileAction   := haAdd;
  except
    FreeAndNil(Result);
  end;
end;

function THeaders.CreateItem(aStream: TStream; aAction: THeaderAction;
  var aVersion: byte): THeader;
var
  I: longint;
begin
  Result := THeader.Create;
  try
    with Result do
    begin
      aStream.Read(FileFlags, SizeOf(FileFlags));

      if foVersion in FileFlags then
      begin
        aStream.Read(FileVersion, SizeOf(FileVersion));
        aVersion := FileVersion;
      end;

      if foMethod in FileFlags then
        aStream.Read(FileMethod, SizeOf(FileMethod));

      if foDictionary in FileFlags then
        aStream.Read(FileDictionary, SizeOf(FileDictionary));

      if foTable in FileFlags then
        aStream.Read(FileTable, SizeOf(FileTable));

      if foVersion in FileFlags then
        aVersion := FileVersion;

      if aVersion < Ord(hv04) then
      begin                                            //  [ver03] | [ver04]
        aStream.Read(I, SizeOf(I));
        FileSize := I;                                 //  4 bytes | 8 bytes

        aStream.Read(FileTime, SizeOf(FileTime));      //  4 bytes | 4 bytes
        aStream.Read(FileAttr, SizeOf(FileAttr));      //  4 bytes | 4 bytes
        aStream.Read(FileCrc, SizeOf(FileCrc));        //  4 bytes | 4 bytes

        aStream.Read(I, SizeOf(I));
        FilePacked := I;                               //  4 bytes | 8 bytes

        aStream.Read(I, SizeOf(I));
        FileStartPos := I;                             //  4 bytes | 8 bytes
      end else
        aStream.Read(FileSize,
          SizeOf(FileSize) + SizeOf(FileTime) + SizeOf(FileAttr) +
          SizeOf(FileCrc)+ SizeOf(FilePacked) + SizeOf(FileStartPos));

      aStream.Read(I, SizeOf(I));
      SetLength(FileName, I);
      if I > 0 then
      begin
        aStream.Read(FileName[1], I);
        FileName := DoDirSeparators(FileName);
      end;
      
      SetLength(FileLink, 0);
      FileAction := aAction;
    end;
  except
    FreeAndNil(Result);
  end;
end;

function THeaders.Compare(P1, P2: THeader): longint;
begin
  Result := CompareFileName(ExtractFileExt(P1.FileName),
    ExtractFileExt(P2.FileName));

  if Result = 0 then
    Result := CompareFileName(ExtractFileName(P1.FileName),
      ExtractFileName(P2.FileName));

  if Result = 0 then
    Result := CompareFileName(P1.FileName, P2.FileName);
end;

procedure THeaders.AddItem(P: THeader);
var
  L, M, H, I: longint;
begin
  // Add item to secondary list
  L := 0;
  M := -2;
  H := FSecondary.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;

    I := CompareFileName(P.FileName, THeader(FSecondary.Items[M]).FileName);

    if I > 0 then
      L := M + 1
    else
    if I < 0 then
      H := M - 1
    else
      H := -2;
  end;

  if M = -2 then
    FSecondary.Add(P)
  else
  if H <> -2 then
  begin
    if I > 0 then
      FSecondary.Insert(M + 1, P)
    else
      FSecondary.Insert(M, P);
  end
  else
    FSecondary.Insert(M + 1, P);

  // Add item to primary list
  if P.FileAction = haAdd then
  begin

    L := FPrimary.Count - FNews;
    M := -2;

    if FNews <> 0 then
      H := FPrimary.Count - 1
    else
      H := -1;

    while H >= L do
    begin
      M := (L + H) div 2;

      I := Compare(P, FPrimary.Items[M]);

      if I > 0 then
        L := M + 1
      else
      if I < 0 then
        H := M - 1
      else
        H := -2;
    end;

    if M = -2 then
      FPrimary.Add(P)
    else
    if H <> -2 then
    begin
      if I > 0 then
        FPrimary.Insert(M + 1, P)
      else
        FPrimary.Insert(M, P);
    end
    else
      FPrimary.Insert(M + 1, P);

    Inc(FNews);
  end else
    FPrimary.Insert(FPrimary.Count - FNews, P);
end;

function THeaders.SearchItem(FileName: string): THeader;
var
  L, M, H, I: longint;
begin
  L := 0;
  H := FSecondary.Count - 1;

  FileName := FCL.cdOption + FileName;
  while H >= L do
  begin
    M := (L + H) div 2;

    I := CompareFileName(FileName, THeader(FSecondary.Items[M]).FileName);

    if I > 0 then
      L := M + 1
    else
    if I < 0 then
      H := M - 1
    else
      H := -2;
  end;

  if H = -2 then
    Result := FSecondary.Items[M]
  else
    Result := nil;
end;

function THeaders.AlreadyFileExists(const aIndex: longint; const aActions: THeaderActions; const aFileName: string): longint;
begin
  Result := GetBack(aIndex - 1, aActions, aFileName);
  if Result = -1 then
  begin
    Result := GetNext(aIndex + 1, aActions, aFileName);
  end;
end;

function THeaders.AlreadyFileExists(const aFileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := 0 to FPrimary.Count - 1 do
    if CompareFileName(aFileName, THeader(FPrimary.Items[I]).FileName) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

procedure THeaders.WriteItem(aStream: TStream; P: THeader; var aVersion: byte);
var
  I: longint;
begin
  aStream.Write(Marker, SizeOf(Marker));
  aStream.Write(P.FileFlags, SizeOf(P.FileFlags));
  with P do
  begin
    if foVersion in FileFlags then
    begin
      aStream.Write(FileVersion, SizeOf(FileVersion));
      aVersion := FileVersion;
    end;

    if foMethod in FileFlags then
      aStream.Write(FileMethod, SizeOf(FileMethod));

    if foDictionary in FileFlags then
      aStream.Write(FileDictionary, SizeOf(FileDictionary));
      
    if foTable in FileFlags then
      aStream.Write(FileTable, SizeOf(FileTable));

    if aVersion < Ord(hv04) then
    begin                                             //  [ver03] | [ver04]
      I := FileSize;
      aStream.Write(I, SizeOf(I));                    //  4 bytes | 8 bytes

      aStream.Write(FileTime, SizeOf(FileTime));      //  4 bytes | 4 bytes
      aStream.Write(FileAttr, SizeOf(FileAttr));      //  4 bytes | 4 bytes
      aStream.Write(FileCrc, SizeOf(FileCrc));        //  4 bytes | 4 bytes

      I := FilePacked;
      aStream.Write(I, SizeOf(I));                    //  4 bytes | 8 bytes

      I := FileStartPos;
      aStream.Write(I, SizeOf(I));                    //  4 bytes | 8 bytes
    end else
      aStream.Write(FileSize,
        SizeOf(FileSize) + SizeOf(FileTime) + SizeOf(FileAttr) +
        SizeOf(FileCrc) + SizeOf(FilePacked) + SizeOf(FileStartPos));

    I := Length(FileName);
    aStream.Write(I, SizeOf(FileName));
    if I > 0 then
    begin
      aStream.Write(FileName[1], I);
    end;
  end;
end;

procedure THeaders.WriteItems(aStream: TStream);
var
  I: longint;
  Version: byte;
begin
  if aStream.Seek(0, 1) = 0 then
  begin
    if FModule.Size > 0 then
    begin
      FModule.Seek(0, 0);
      aStream.CopyFrom(FModule, FModule.Size);
    end;
  end else
    aStream.Seek(FModule.Size, 0);

  Version := Ord(hv02);
  MarkAsLast(haDelete);
  for I := 0 to FPrimary.Count - 1 do
    if THeader(FPrimary.Items[I]).FileAction <> haDelete then
    begin
      WriteItem(aStream, THeader(FPrimary.Items[I]), Version);
    end;
end;

function THeaders.AddItem(const Rec: TCustomSearchRec): int64;
var
  Item: THeader;
begin
  Item := SearchItem(Rec.FileName);
  if Item = nil then
  begin
    Item := CreateItem(Rec);
    AddItem(Item);
    Result := Rec.FileSize;
  end else
    Result := 0;
end;

function THeaders.UpdateItem(const Rec: TCustomSearchRec): int64;
var
  Item: THeader;
begin
  Item := SearchItem(Rec.FileName);
  if (Item <> nil) and (Item.FileTime < Rec.FileTime) then
  begin
    if Item.FileAction = haCopy then
      Item.FileAction := haUpdate;

    Item.FileLink := Rec.FileLink;
    Item.FileTime := Rec.FileTime;
    Result := Rec.FileSize;
  end else
    Result := 0;
end;

function THeaders.ReplaceItem(const Rec: TCustomSearchRec): int64;
var
  Item: THeader;
begin
  Item := SearchItem(Rec.FileName);
  if Item <> nil then
  begin
    if Item.FileAction = haCopy then
      Item.FileAction := haUpdate;
    
    Item.FileLink := Rec.FileLink;
    Item.FileTime := Rec.FileTime;
    Result := Rec.FileSize;
  end else
    Result := 0;
end;

function THeaders.AddUpdateItem(const Rec: TCustomSearchRec): int64;
begin
  Result := AddItem(Rec);
  if Result = 0 then
  begin
    Result := UpdateItem(Rec);
  end;
end;

function Theaders.AddReplaceItem(const Rec: TCustomSearchRec): int64;
begin
  Result := AddItem(Rec);
  if Result = 0 then
  begin
    Result := ReplaceItem(Rec);
  end;
end;

function Theaders.AddAutoRenameItem(const Rec: TCustomSearchRec): int64;
var
  StartIndex: longint;
begin
  Result := AddItem(Rec);
  if Result = 0 then
  begin
    StartIndex := 1;
    while SearchItem(GenerateAlternativeFileName(Rec.FileName, StartIndex, False)) <> nil do
    begin
      Inc(StartIndex);
    end;
    Rec.FileName := GenerateAlternativeFileName(Rec.FileName, StartIndex, False);
    Result := AddItem(Rec);
  end;
end;

function THeaders.MarkItems(Masks: TStringList; MaskAct: THeaderAction; aAction: THeaderAction): longint;
var
  I: longint;
begin
  Result := 0;
  for I := 0 to Masks.Count - 1 do
  begin
    Inc(Result, MarkItems(Masks.Strings[I], MaskAct, aAction));
  end;
end;

function THeaders.MarkItems(Mask: string; MaskAct: THeaderAction; aAction: THeaderAction): longint;
var
  P: THeader;
  I: longint;
begin
  Result := 0;
  if FileNameUseWildcards(Mask) then
  begin
    Mask := FCL.cdOption + Mask;
    for  I := 0 to FPrimary.Count - 1 do
    begin
      P := THeader(FPrimary.Items[I]);
      if (P.FileAction = MaskAct) and (FileNameMatch(P.FileName, Mask, FCL.rOption)) then
      begin
        P.FileAction := aAction;
        Inc(Result);
      end;
    end;
  end else
  begin
    P := SearchItem(Mask);
    if (P <> nil) and (P.FileAction = MaskAct) then
    begin
      P.FileAction := aAction;
      Inc(Result);
    end;
  end;
end;

procedure THeaders.MarkItem(aIndex: longint; aAction: THeaderAction);
begin
  THeader(FPrimary.Items[aIndex]).FileAction := aAction;
end;

procedure THeaders.MarkAll(aAction: THeaderAction);
var
  I: longint;
begin
  for I := 0 to FPrimary.Count - 1 do
  begin
    THeader(FPrimary.Items[I]).FileAction := aAction;
  end;
end;

procedure THeaders.MarkAsLast(aAction: THeaderAction);
var
  I: longint;
begin
  for I := FPrimary.Count - 1 downto 0 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction <> aAction) then
      begin
        Include(FileFlags, foLast);
        Break;
      end;
end;

procedure THeaders.SortNews(aConfiguration: TConfiguration);
var
  P: THeader;
  I, First, Method, Dictionary: longint;
  CurrentExt, PreviousExt: string;
begin
  aConfiguration.Selector('\main');
  Method     := StrToInt(aConfiguration.CurrentSection.Values['Method']);
  Dictionary := StrToInt(aConfiguration.CurrentSection.Values['Dictionary']);
  aConfiguration.Selector('\m' + aConfiguration.CurrentSection.Values['Method']);

  if FNews <> 0 then
    First := FPrimary.Count - FNews
  else
    First := -1;

  if First <> -1 then
  begin
    CurrentExt := '.';
    I := First;
    repeat
      P := FPrimary.Items[I];
      if I = First then
      begin
        Include(P.FileFlags, foVersion);
        Include(P.FileFlags, foMethod);
        Include(P.FileFlags, foDictionary);
      end;
      P.FileMethod     := Method;
      P.FileDictionary := Dictionary;

      PreviousExt := CurrentExt;
      if Length(FCL.fOption) = 0 then
        CurrentExt := ExtractFileExt(P.FileName)
      else
        CurrentExt := FCL.fOption;

      if FCL.pOption then
        Include(P.FileFlags, foPassword);

      if (Method = 0) or (not aConfiguration.GetTable(CurrentExt, P.FileTable)) then
      begin
        Include(P.FileFlags, foMoved);
        Include(P.FileFlags, foTable);
      end
      else
      if CompareFileName(CurrentExt, PreviousExt) = 0 then
      begin
        Exclude(P.FileFlags, foTable);
        if FCL.sOption then
        begin
          Exclude(P.FileFlags, foTear);
        end;
      end
      else
        Include(P.FileFlags, foTable);

      Inc(I);
    until I = FPrimary.Count;
  end;
end;

function THeaders.FindFirstMarker(aStream: TStream): int64;
var
  Id:      longint;
  StrmPos: int64;
begin
  Result := -1;

  StrmPos := aStream.Seek(0, 0);
  while aStream.Read(Id, SizeOf(Id)) = SizeOf(Id) do
  begin
    if Id = Marker then
    begin
      Result := StrmPos;
      Break;
    end;
    Inc(StrmPos, SizeOf(Id));
  end;

  if Result > 0 then
  begin
    aStream.Seek(0, 0);
    FModule.Size := 0;
    FModule.CopyFrom(aStream, Result);
  end;
end;

procedure THeaders.ReadItemsB4b(aStream: TStream; aAction: THeaderAction);
var
  P:      THeader;
  Ptr:    ^longint;
  Symbol: byte;
  SymbolIndex: longint;
  B4bMarker: array [0..3] of byte;
  Version: byte;
begin
  P    := nil;
  Ptr  := @B4bMarker;
  Ptr^ := Marker;
  Version := Ord(hv02);

  SymbolIndex := 0;
  aStream.Seek(0, 0);
  repeat
    if aStream.Read(Symbol, 1) = 1 then
    begin
      if Symbol = B4bMarker[SymbolIndex] then
        Inc(SymbolIndex)
      else
        SymbolIndex := 0;

      if SymbolIndex = SizeOf(Marker) then
      begin
        P := CreateItem(aStream, aAction, Version);
        if P <> nil then
        begin
          AddItem(P);
        end;
        SymbolIndex := 0;
      end;

    end
    else
      Break;

  until (P <> nil) and (foLast in P.FileFlags);

  if P <> nil then
  begin
    Exclude(P.FileFlags, foLast);
  end;
end;

procedure THeaders.ReadItems(aStream: TStream; aAction: THeaderAction);
var
  P:      THeader;
  // I: longint;
  Id:     longint;
  OffSet: int64;
  Version: byte;
begin
  P := nil;
  Version := Ord(hv02);

  OffSet := FindFirstMarker(aStream);
  if OffSet > -1 then
  begin
    aStream.Seek(OffSet, 0);
    repeat
      if (aStream.Read(Id, SizeOf(Id)) = SizeOf(Id)) and (Id = Marker) then
      begin
        P := CreateItem(aStream, aAction, Version);
        if P <> nil then
        begin
          AddItem(P);
        end;
      end
      else
        Break;
    until (P <> nil) and (foLast in P.FileFlags);

    if P <> nil then
      Exclude(P.FileFlags, foLast);
  end else
    ReadItemsB4b(aStream, aAction);
  // OffSet := aStream.Seek(0, 1);
  // for I := 0 to FPrimary.Count -1 do
  //   with THeader(FPrimary.Items[I]) do
  //   begin
  //     FileStartPos := OffSet;
  //     Inc(OffSet, FilePacked);
  //   end;
end;

function THeaders.GetNext(aChild: longint; aAction: THeaderAction): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction = aAction) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetBack(aChild: longint; aAction: THeaderAction): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild downto 0 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction = aAction) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetNext(aChild: longint; aFlag: THeaderFlag): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if (aFlag in FileFlags) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetBack(aChild: longint; aFlag: THeaderFlag): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild downto 0 do
    with THeader(FPrimary.Items[I]) do
      if (aFlag in FileFlags) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetNext(aChild: longint; aAction: THeaderAction;
  const aFileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction = aAction) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetBack(aChild: longint; aActions: THeaderActions;
  const aFileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild downto 0 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction in aActions) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetNext(aChild: longint; aActions: THeaderActions;
  const aFileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction in aActions) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetBack(aChild: longint; aAction: THeaderAction;
  const aFileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild downto 0 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction = aAction) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetSize(aAction: THeaderAction): int64;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction = aAction) then
      begin
        Inc(Result, FileSize);
      end;
end;

function THeaders.GetPackedSize(aAction: THeaderAction): int64;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if FileAction = aAction then
      begin
        Inc(Result, FilePacked);
      end;
end;

function THeaders.GetSize(aActions: THeaderActions): int64;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction in aActions) then
      begin
        Inc(Result, FileSize);
      end;
end;

function THeaders.GetPackedSize(aActions: THeaderActions): int64;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction in aActions) then
      begin
        Inc(Result, FilePacked);
      end;
end;

function THeaders.GetCount(Actions: THeaderActions): longint;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to FPrimary.Count - 1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction in Actions) then
      begin
        Inc(Result);
      end;
end;

function THeaders.GetCount: longint;
begin
  Result := FPrimary.Count;
end;

function THeaders.GetItem(aIndex: longint): THeader;
begin
  Result := FPRimary.Items[aIndex];
end;

function THeaders.SetModule(const aFileName: string): boolean;
var
  Strm: TStream;
begin
  Result := False;
  FModule.Size := 0;
  if FileExists(aFileName) then
  begin
    Strm := TFileReader.Create(aFileName, fmOpenRead);
    try
      if FModule.CopyFrom(Strm, Strm.Size) = Strm.Size then
        Result := True
      else
        FModule.Size := 0;
    finally
      FreeAndNil(Strm);
    end;
  end;
end;

function THeaders.GetModule: longint;
begin
  if Assigned(FModule) then
    Result := FModule.Size
  else
    Result := 0;
end;

end.
