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
    foLast,     foPassword, foInt64,     fo10Unused, fo11Unused, fo12Unused,
    fo13Unused, fo14Unused, fo15Unused,   fo16Unused, fo17Unused, fo18Unused,
    fo19Unused, fo20Unused, fo21Unused,   fo22Unused, fo23Unused, fo24Unused,
    fo25Unused, fo26Unused, fo27Unused,   fo28Unused, fo29Unused, fo30Unused,
    fo31Unused, fo32Unused);

  THeaderFlags = set of THeaderFlag;

  { Header actions }

  THeaderAction = (haAdd, haUpdate, haCopy, haExtract,
     haDecode, haSkip, haDelete, haNone, haUnknow);

  THeaderActions = set of THeaderAction;

  { Header structure, order of fields is significant }

  THeader = class
  public
    Flags: THeaderFlags;
    Version: byte;
    Method: byte;
    Dictionary: byte;
    Table: TTableParameters;
    Size: int64;
    Time: longint;
    Attr: longint;
    Crc: longword;
    PackedSize: int64;
    StartPos: int64;
    Name: string;
  public
    Index: longint;        // reserved
    Action: THeaderAction; // reserved
    Link: string;          // reserved
  end;

  { Header list compare function}

  THeaderListCompare = function(Item1, Item2: THeader): longint;

  { Header list class }

   THeaderList = class
   private
     FItems: TList;
     FNames: TList;
     function GetSize(Actions: THeaderActions): int64;
     function GetPackedSize(Actions: THeaderActions): int64;
     function GetCount(Actions: THeaderActions): longint;
     function GetItem(Index: longint): THeader;
   protected
     FCL: TCommandLine;
     procedure Add(Item: THeader);
     procedure Insert(List: TList; Compare: THeaderListCompare; Item: THeader);
     procedure Clear;
   public
     constructor Create(CommandLine: TCommandLine);
     destructor Destroy; override;

     function Search(FileName: string): THeader;

     function SetAction(Masks: TStringList; MaskAct, Action: THeaderAction): longint; overload;
     function SetAction(Mask: string; MaskAct, Action: THeaderAction): longint; overload;
     function SetAction(Index: longint; Action: THeaderAction): longint; overload;
     function SetAction(Action: THeaderAction): longint; overload;

     function GetBack(Index: longint; Actions: THeaderActions): longint; overload;
     function GetNext(Index: longint; Actions: THeaderActions): longint; overload;
     function GetBack(Index: longint; Flag:    THeaderFlag): longint; overload;
     function GetNext(Index: longint; Flag:    THeaderFlag): longint; overload;

     property Size[Actions: THeaderActions]: int64 read GetSize;
     property PackedSize[Actions: THeaderActions]: int64 read GetPackedSize;
     property Count[Actions: THeaderActions]: longint read GetCount;
     property Items[Index: longint]: THeader read GetItem;
   end;

  { Headers class }

  THeaders = class(THeaderList)
  private
    FSfx: TStream;
    function GetSfxSize: longint;
    function GetFirst(Stream: TStream): int64;
    procedure SetLast(Action: THeaderAction);
    procedure ReadB4b(Stream: TStream);
    function CreateItem(const Rec: TCustomSearchRec): THeader; overload;
  public
    constructor Create(CommandLine: TCommandLine);
    destructor Destroy; override;

    procedure Read(Stream: TStream; Action: THeaderAction); virtual;
    procedure Write(Stream: TStream); virtual;
    procedure Configure(Configuration: TConfiguration);

    function AddItem(const Rec: TCustomSearchRec): int64;
    function UpdateItem(const Rec: TCustomSearchRec): int64;
    function ReplaceItem(const Rec: TCustomSearchRec): int64;
    function AddUpdateItem(const Rec: TCustomSearchRec): int64;
    function AddReplaceItem(const Rec: TCustomSearchRec): int64;
    function AddAutoRenameItem(const Rec: TCustomSearchRec): int64;

    function LoadSfx(const FileName: string): boolean;
    function SaveSfx(const FileName: string): boolean;
    property SfxSize: longint read GetSfxSize;
  end;


implementation

uses
  Bee_Types,
  Bee_Consts,
  Bee_Common;

{ Header list }

constructor THeaderList.Create(CommandLine: TCommandLine);
begin
  inherited Create;
  FItems := TList.Create;
  FNames := TList.Create;
  FCL    := CommandLine;
end;

destructor THeaderList.Destroy;
begin
  Clear;
  FItems.Free;
  FNames.Free;
  FCL := nil;
  inherited Destroy;
end;

procedure THeaderList.Clear;
var
  I: longint;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    THeader(FItems[I]).Free;
  end;
  FItems.Clear;
  FNames.Clear;
end;

function CompareItems(Item1, Item2: THeader): longint;
begin
  if (Item1.Index = -1) and (Item2.Index = -1) then
  begin
    Result := CompareFileName(ExtractFileExt(Item1.Name), ExtractFileExt(Item2.Name));

    if Result = 0 then
      Result := CompareFileName(ExtractFileName(Item1.Name), ExtractFileName(Item2.Name));

    if Result = 0 then
      Result := CompareFileName(Item1.Name, Item2.Name);

  end else
  begin
    if (Item1.Index <> -1) and (Item2.Index <> -1) then
    begin
      Result := CompareFileName(Item1.Name, Item2.Name);
    end else
      if (Item1.Index < Item2.Index) then
        Result := 1
      else
        if (Item1.Index > Item2.Index) then
          Result := -1
        else
          Result := 0;
  end;
end;

function CompareNames(Item1, Item2: THeader): longint;
begin
  Result := CompareFileName(THeader(Item1).Name, THeader(Item2).Name);
end;

procedure THeaderList.Insert(List: TList; Compare: THeaderListCompare; Item: THeader);
var
  L, M, H, I: longint;
begin
  L :=  0;
  M := -2;
  H := List.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := Compare(Item, THeader(List[M]));
    if I > 0 then
      L := M + 1
    else
      if I < 0 then
        H := M - 1
      else
        H := -2;
  end;

  if M = -2 then
    List.Add(Item)
  else
    if H <> -2 then
    begin
      if I > 0 then
        List.Insert(M + 1, Item)
      else
        List.Insert(M, Item);
    end else
      List.Insert(M + 1, Item);
end;

  function THeaderList.Search(FileName: string): THeader;
  var
    L, M, H, I: longint;
  begin
    FileName := FCL.cdOption + FileName;

    L := 0;
    H := FNames.Count - 1;
    while H >= L do
    begin
      M := (L + H) div 2;

      I := CompareFileName(FileName, THeader(FNames[M]).Name);

      if I > 0 then
        L := M + 1
      else
        if I < 0 then
          H := M - 1
        else
          H := -2;
    end;

    if H = -2 then
      Result := FNames[M]
    else
      Result := nil;
  end;

  procedure THeaderList.Add(Item: THeader);
  begin
    Insert(FItems, CompareItems, Item);
    Insert(FNames, CompareNames, Item);
  end;

  function THeaderList.SetAction(Masks: TStringList; MaskAct: THeaderAction; Action: THeaderAction): longint;
  var
    I: longint;
  begin
    Result := 0;
    for  I := 0 to Masks.Count - 1 do
    begin
      Inc(Result, SetAction(Masks[I], MaskAct, Action));
    end;
  end;

  function THeaderList.SetAction(Mask: string; MaskAct: THeaderAction; Action: THeaderAction): longint;
  var
    I: longint;
    P: THeader;
  begin
    Result := 0;
    if FileNameUseWildcards(Mask) then
    begin
      Mask := FCL.cdOption + Mask;
      for  I := 0 to FItems.Count - 1 do
      begin
        P := THeader(FItems[I]);
        if (P.Action = MaskAct) and (FileNameMatch(P.Name, Mask, FCL.rOption)) then
        begin
          P.Action := Action;
          Inc(Result);
        end;
      end;

    end else
    begin
      P := Search(Mask);
      if (P <> nil) and (P.Action = MaskAct) then
      begin
        P.Action := Action;
        Inc(Result);
      end;
    end;
  end;

  function THeaderList.SetAction(Index: longint; Action: THeaderAction): longint;
  begin
    Result := 1;
    THeader(FItems[Index]).Action := Action;
  end;

  function THeaderList.SetAction(Action: THeaderAction): longint;
  var
    I: longint;
  begin
    Result := FItems.Count;
    for I := 0 to Result - 1 do
    begin
      THeader(FItems[I]).Action := Action;
    end;
  end;

  function THeaderList.GetNext(Index: longint; Actions: THeaderActions): longint;
  var
    I: longint;
  begin
    Result := -1;
    for  I := Index to FItems.Count - 1 do
      if (THeader(FItems[I]).Action in Actions) then
      begin
        Result := I;
        Break;
      end;
  end;

  function THeaderList.GetBack(Index: longint; Actions: THeaderActions): longint;
  var
    I: longint;
  begin
    Result := -1;
    for  I := Index downto 0 do
      if (THeader(FItems[I]).Action in Actions) then
      begin
        Result := I;
        Break;
      end;
  end;

  function THeaderList.GetNext(Index: longint; Flag: THeaderFlag): longint;
  var
    I: longint;
  begin
    Result := -1;
    for  I := Index to FItems.Count - 1 do
      if (Flag in THeader(FItems[I]).Flags) then
      begin
        Result := I;
        Break;
      end;
  end;

  function THeaderList.GetBack(Index: longint; Flag: THeaderFlag): longint;
  var
    I: longint;
  begin
    Result := -1;
    for  I := Index downto 0 do
      if (Flag in THeader(FItems[I]).Flags) then
      begin
        Result := I;
        Break;
      end;
  end;

function THeaderList.GetSize(Actions: THeaderActions): int64;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to FItems.Count - 1 do
    if (THeader(FItems[I]).Action in Actions) then
    begin
      Inc(Result, THeader(FItems[I]).Size);
    end;
end;

function THeaderList.GetPackedSize(Actions: THeaderActions): int64;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to FItems.Count - 1 do
    if (THeader(FItems[I]).Action in Actions) then
    begin
      Inc(Result, THeader(FItems[I]).PackedSize);
    end;
end;

function THeaderList.GetCount(Actions: THeaderActions): longint;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to FItems.Count - 1 do
    if (THeader(FItems[I]).Action in Actions) then
    begin
      Inc(Result);
    end;
end;

function THeaderList.GetItem(Index: longint): THeader;
begin
  Result := FItems[Index];
end;

{ Headers class }

constructor THeaders.Create(CommandLine: TCommandLine);
begin
  inherited Create(CommandLine);
  FSfx := TMemoryStream.Create;
end;

destructor THeaders.Destroy;
begin
  FSfx.Free;
  inherited Destroy;
end;

function THeaders.CreateItem(const Rec: TCustomSearchRec): THeader;
begin
  Result := THeader.Create;
  try
    // - Start header data - //
    Result.Flags      := [foTear, foTable];
    { TODO : Verify Method - Dictionary }
    Result.Version    := Ord(FCL.hvOption);
    Result.Method     := Ord(moFast);
    Result.Dictionary := Ord(do5MB);
    // Result.FileTable
    Result.Size       := Rec.FileSize;
    Result.Time       := Rec.FileTime;
    Result.Attr       := Rec.FileAttr;
    Result.Crc        := longword(-1);
    Result.PackedSize := 0;
    Result.StartPos   := 0;
    Result.Name       := FCL.cdOption + Rec.FileName;
    // - End header data - //
    Result.Index      := -1;
    Result.Action     := haAdd;
    Result.Link       := Rec.FileLink;
  except
    FreeAndNil(Result);
  end;
end;

procedure ReadHv03(Stream: TStream; var Item: THeader);
var
  I: longint;
begin
  if foMethod in Item.Flags then
    Stream.Read(Item.Method, SizeOf(Item.Method))
  else
    Item.Method := -1;

  if foDictionary in Item.Flags then
    Stream.Read(Item.Dictionary, SizeOf(Item.Dictionary))
  else
    Item.Dictionary := -1;

  if foTable in Item.Flags then
    Stream.Read(Item.Table, SizeOf(Item.Table));

  Stream.Read(I, SizeOf(I));                 //  [ver03]
  Item.Size := I;                            //  4 bytes

  Stream.Read(Item.Time, SizeOf(Item.Time)); //  4 bytes
  Stream.Read(Item.Attr, SizeOf(Item.Attr)); //  4 bytes
  Stream.Read(Item.Crc,  SizeOf(Item.Crc));  //  4 bytes

  Stream.Read(I, SizeOf(I));                 //  4 bytes
  Item.PackedSize := I;

  Stream.Read(I, SizeOf(I));                 //  4 bytes
  Item.StartPos := I;

  Stream.Read(I, SizeOf(I));
  SetLength(Item.Name, I);
  if I > 0 then
  begin
    Stream.Read(Item.Name[1], I);
    Item.Name := DoDirSeparators(Item.Name);
  end;
  Item.Index  := -1;
  Item.Action := haCopy;
  Item.Link   := '';
end;

procedure ReadHv04(Stream: TStream; var Item: THeader);
var
  I: longint;
begin


  // if foVersion in Result.Flags then
  //   Stream.Read(Result.Version, SizeOf(Result.Version))
  // else
  //   Result.Version := -1;

  if foMethod in Item.Flags then
    Stream.Read(Item.Method, SizeOf(Item.Method))
  else
    Item.Method := -1;

  if foDictionary in Item.Flags then
    Stream.Read(Item.Dictionary, SizeOf(Item.Dictionary))
  else
    Item.Dictionary := -1;

  if foTable in Item.Flags then
    Stream.Read(Item.Table, SizeOf(Item.Table));
                                                         //  [ver04]
  Stream.Read(Item.Size, SizeOf(Item.Size));             //  8 bytes
  Stream.Read(Item.Time, SizeOf(Item.Time));             //  4 bytes
  Stream.Read(Item.Attr, SizeOf(Item.Attr));             //  4 bytes
  Stream.Read(Item.Crc,  SizeOf(Item.Crc));              //  4 bytes
  Stream.Read(Item.PackedSize, SizeOf(Item.PackedSize)); //  8 bytes
  Stream.Read(Item.StartPos, SizeOf(Item.StartPos));     //  8 bytes

  Stream.Read(I, SizeOf(I));
  SetLength(Item.Name, I);
  if I > 0 then
  begin
    Stream.Read(Item.Name[1], I);
    Item.Name := DoDirSeparators(Item.Name);
  end;
  Item.Index  := -1;
  Item.Action := haCopy;
  Item.Link   := '';
end;

function THeaders.GetFirst(Stream: TStream): int64;
var
  Id: longint;
  StrmPos: int64;
begin
  Result := -1;
  StrmPos := Stream.Seek(0, 0);
  while Stream.Read(Id, SizeOf(Id)) = SizeOf(Id) do
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
    Stream.Seek(0, 0);
    FSfx.Size := 0;
    FSfx.CopyFrom(Stream, Result);
  end;
end;

procedure THeaders.SetLast(Action: THeaderAction);
var
  I: longint;
begin
  for I := FItems.Count - 1 downto 0 do
    if (THeader(FItems[I]).Action <> Action) then
    begin
      Include(THeader(FItems[I]).Flags, foLast);
      Break;
    end;
end;

procedure THeaders.ReadB4b(Stream: TStream);
var
  P: THeader;
  Ptr: ^longint;
  Symbol: byte;
  SymbolIndex: longint;
  B4bMarker: array [0..3] of byte;
begin
  P    := nil;
  Ptr  := @B4bMarker;
  Ptr^ := Marker;

  SymbolIndex := 0;
  Stream.Seek(0, 0);
  repeat
    if Stream.Read(Symbol, 1) = 1 then
    begin
      if Symbol = B4bMarker[SymbolIndex] then
        Inc(SymbolIndex)
      else
        SymbolIndex := 0;

      if SymbolIndex = SizeOf(Marker) then
      begin
        P := THeader.Create;
        try
          Stream.Read(P.Flags, SizeOf(P.Flags));
          if foVersion in P.Flags then
            Stream.Read(P.Version, SizeOf(P.Version))
          else
            P.Version := -1;

          case P.Version of
            Ord(hv02): ReadHv03(Stream, P);
            Ord(hv03): ReadHv03(Stream, P);
            Ord(hv04): ReadHv04(Stream, P);
          end;
        except
          P := nil;
        end;






        ReadItem(Stream, aAction, Version);
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




























procedure THeaders.Read(aStream: TStream);
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
