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
  Bee_Types,
  Bee_Files,
  Bee_CommandLine,
  Bee_Configuration;

type

  { Header flags }

  THeaderFlag =
   (foVersion,  foMethod,   foDictionary, foTable,    foTear,     foMoved,
    foLast,     foPassword, fo9Unused,    fo10Unused, fo11Unused, fo12Unused,
    fo13Unused, fo14Unused, fo15Unused,   fo16Unused, fo17Unused, fo18Unused,
    fo19Unused, fo20Unused, fo21Unused,   fo22Unused, fo23Unused, fo24Unused,
    fo25Unused, fo26Unused, fo27Unused,   fo28Unused, fo29Unused, fo30Unused,
    fo31Unused, fo32Unused);

  THeaderFlags = set of THeaderFlag;

  { Header actions }

  THeaderAction = (haNew, haUpdate, haNone, haExtract, haDecode, haDelete);

  THeaderActions = set of THeaderAction;

  { Header structure, order of fields is significant }

  THeaderRec= class
  public
    Name: string;
    Size: int64;
    Time: longint;
    Attr: longint;
  end;

  THeader = class(THeaderRec)
  public
    Flags: THeaderFlags;
    Version: byte;
    Method: byte;
    Dictionary: byte;
    Table: TTableParameters;
    Crc: longword;
    PackedSize: int64;
    StartPos: int64;
    Comment: string;
  public
    Action: THeaderAction; { reserved }
    Link: string;          { reserved }
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
     function GetActionCount(Actions: THeaderActions): longint;
     function GetItem(Index: longint): THeader;
     function GetCount: longint;
   protected
     FCL: TCommandLine;
     procedure Insert(Item: THeader);
     procedure Clear;
   public
     constructor Create(CommandLine: TCommandLine);
     destructor Destroy; override;

     function Search(FileName: string): THeader; virtual; overload;
     function Search(Item: THeader): longint; virtual; overload;

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
     property Items[Index: longint]: THeader read GetItem;
     property ActionCount[Actions: THeaderActions]: longint read GetActionCount;
     property Count: longint read GetCount;
   end;

  { Headers class }

  THeaders = class(THeaderList)
  private
    FSfx: TStream;
    FNews: longint;
    procedure SetLast;
    function GetSfxSize: longint;
    function GetFirst(Stream: TStream): int64;
    procedure ReadB4b(Stream: TStream);
    function New(const Rec: TCustomSearchRec): THeader; overload;
  public
    constructor Create(CommandLine: TCommandLine);
    destructor Destroy; override;

    procedure Read(Stream: TStream); virtual;
    procedure Write(Stream: TStream); virtual;
    procedure Configure(Configuration: TConfiguration);

    function Add(const Rec: TCustomSearchRec): int64;
    function Update(const Rec: TCustomSearchRec): int64;
    function Replace(const Rec: TCustomSearchRec): int64;
    function AddUpdate(const Rec: TCustomSearchRec): int64;
    function AddReplace(const Rec: TCustomSearchRec): int64;
    function AddAutoRename(const Rec: TCustomSearchRec): int64;

    procedure ClearSfx;
    function LoadSfx(const FileName: string): boolean;
    property SfxSize: longint read GetSfxSize;
  end;

implementation

uses
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
  if (Item1.Action = haNew) and (Item2.Action = haNew) then
  begin
    Result := CompareFileName(ExtractFileExt(Item1.Name), ExtractFileExt(Item2.Name));

    if Result = 0 then
      Result := CompareFileName(ExtractFileName(Item1.Name), ExtractFileName(Item2.Name));

    if Result = 0 then
      Result := CompareFileName(Item1.Name, Item2.Name);

  end else
  begin
    if (Item1.Action <> haNew) and (Item2.Action <> haNew) then
    begin
      Result := CompareFileName(Item1.Name, Item2.Name);

    end else
      if (Item1.Action = haNew) then
        Result := -1
      else
        Result :=  1;
  end;
end;

function CompareNames(Item1, Item2: THeader): longint;
begin
  Result := CompareFileName(THeader(Item1).Name, THeader(Item2).Name);
end;

procedure BinaryInsert(List: TList; Compare: THeaderListCompare; Item: THeader);
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

procedure THeaderList.Insert(Item: THeader);
begin
  BinaryInsert(FItems, CompareItems, Item);
  BinaryInsert(FNames, CompareNames, Item);
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

function THeaderList.Search(Item: THeader): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := 0 to FItems.Count - 1 do
    if (THeader(FItems[I]) = Item) then
    begin
      Result := I;
      Break;
    end;
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

function THeaderList.GetActionCount(Actions: THeaderActions): longint;
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

function THeaderList.GetCount: longint;
begin
  Result := FItems.Count;
end;

function THeaderList.GetItem(Index: longint): THeader;
begin
  Result := nil;
  if (Index > -1) and (Index < FItems.Count) then
  begin
    Result := FItems[Index];
  end;
end;


{ Headers class }

constructor THeaders.Create(CommandLine: TCommandLine);
begin
  inherited Create(CommandLine);
  FSfx  := TMemoryStream.Create;
  FNews := 0;
end;

destructor THeaders.Destroy;
begin
  FSfx.Free;
  inherited Destroy;
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

procedure THeaders.SetLast;
var
  I: longint;
begin
  for I := FItems.Count - 1 downto 0 do
    if (THeader(FItems[I]).Action <> haDelete) then
    begin
      Include(THeader(FItems[I]).Flags, foLast);
      Break;
    end;
end;

function THeaders.New(const Rec: TCustomSearchRec): THeader;
begin
  Result := THeader.Create;
  // - Start header data - //
  Result.Flags      := [foTear, foTable];
  { TODO : Verify Method - Dictionary }
  Result.Version    := Ord(FCL.hvOption);
  Result.Method     := Ord(moFast);
  Result.Dictionary := Ord(do5MB);
  // Result.FileTable
  Result.Size       := Rec.Size;
  Result.Time       := Rec.Time;
  Result.Attr       := Rec.Attr;
  Result.Crc        := longword(-1);
  Result.PackedSize := 0;
  Result.StartPos   := 0;
  Result.Name       := FCL.cdOption + Rec.Name;
  // - End header data - //
  Result.Action     := haNew;
  Result.Link       := Rec.Link;
end;

procedure ReadHv03(Stream: TStream; var Item: THeader);
var
  I: longint;
begin
  if foMethod in Item.Flags then
    Stream.Read(Item.Method, SizeOf(Item.Method))
  else
    Item.Method := 0;

  if foDictionary in Item.Flags then
    Stream.Read(Item.Dictionary, SizeOf(Item.Dictionary))
  else
    Item.Dictionary := 0;

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
  Item.Action := haNone;
  Item.Link   := '';
end;

procedure ReadHv04(Stream: TStream; var Item: THeader);
var
  I: longint;
begin

  if foMethod in Item.Flags then
    Stream.Read(Item.Method, SizeOf(Item.Method))
  else
    Item.Method := 0;

  if foDictionary in Item.Flags then
    Stream.Read(Item.Dictionary, SizeOf(Item.Dictionary))
  else
    Item.Dictionary := 0;

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
  Item.Action := haNone;
  Item.Link   := '';
end;

function ReadHxx(Stream: TStream; var Version: byte): THeader;
begin
  Result := THeader.Create;
  try
    Stream.Read(Result.Flags, SizeOf(Result.Flags));
    if foVersion in Result.Flags then
    begin
      Stream.Read(Version, SizeOf(Version));
    end;
    Result.Version := Version;

    case Result.Version of
      Ord(hv02): ReadHv03(Stream, Result);
      Ord(hv03): ReadHv03(Stream, Result);
      Ord(hv04): ReadHv04(Stream, Result);
      else FreeAndNil(Result);
    end;
  except
    FreeAndNil(Result);
  end;
end;

procedure THeaders.ReadB4b(Stream: TStream);
var
  P: THeader;
  Ptr: ^longint;
  Symbol: byte;
  SymbolIndex: longint;
  B4bMarker: array [0..3] of byte;
  Ver: byte;
begin
  P    := nil;
  Ptr  := @B4bMarker;
  Ptr^ := Marker;
  Ver  := Ord(hv02);

  Stream.Seek(0, 0);
  SymbolIndex := 0;
  repeat
    if Stream.Read(Symbol, 1) = 1 then
    begin
      if Symbol = B4bMarker[SymbolIndex] then
        Inc(SymbolIndex)
      else
        SymbolIndex := 0;

      if SymbolIndex = SizeOf(Marker) then
      begin
        P := ReadHxx(Stream, Ver);
        if P <> nil then
        begin
          Insert(P);
        end;
        SymbolIndex := 0;
      end;

    end else Break;

  until (P <> nil) and (foLast in P.Flags);

  if P <> nil then
  begin
    Exclude(P.Flags, foLast);
  end;
end;

procedure THeaders.Read(Stream: TStream);
var
  P: THeader;
  // I: longint;
  Id: longint;
  OffSet: int64;
  Ver: byte;
begin
  P   := nil;
  Ver := Ord(hv02);

  OffSet := GetFirst(Stream);
  if OffSet > -1 then
  begin
    Stream.Seek(OffSet, 0);
    repeat
      if (Stream.Read(Id, SizeOf(Id)) = SizeOf(Id)) and (Id = Marker) then
      begin
        P := ReadHxx(Stream, Ver);
        if P <> nil then
        begin
          Insert(P);
        end;
      end else Break;

    until (P <> nil) and (foLast in P.Flags);

    if P <> nil then
    begin
      Exclude(P.Flags, foLast);
    end;

  end else
    ReadB4b(Stream);

  // if RecalStartPos then
  // begin
  //   OffSet := Stream.Seek(0, 1);
  //   for I := 0 to FItems.Count - 1 do
  //   begin
  //     THeader(FItems[I]).FileStartPos := OffSet;
  //     Inc(OffSet, THeader(FItems[I]).FilePacked);
  //   end;
  // end;
end;

procedure WriteHv03(Stream: TStream; const Item: THeader);
var
  I: longint;
begin
  Stream.Write(Marker, SizeOf(Marker));
  Stream.Write(Item.Flags, SizeOf(Item.Flags));

  if foVersion in Item.Flags then
    Stream.Write(Item.Version, SizeOf(Item.Version));

  if foMethod in Item.Flags then
    Stream.Write(Item.Method, SizeOf(Item.Method));

  if foDictionary in Item.Flags then
    Stream.Write(Item.Dictionary, SizeOf(Item.Dictionary));

  if foTable in Item.Flags then
    Stream.Write(Item.Table, SizeOf(Item.Table));

  I := Item.Size;                                  //  [ver03]
  Stream.Write(I, SizeOf(I));                      //  4 bytes

  Stream.Write(Item.Time, SizeOf(Item.Time));      //  4 bytes
  Stream.Write(Item.Attr, SizeOf(Item.Attr));      //  4 bytes
  Stream.Write(Item.Crc,  SizeOf(Item.Crc));       //  4 bytes

  I := Item.PackedSize;
  Stream.Write(I, SizeOf(I));                      //  4 bytes

  I := Item.StartPos;
  Stream.Write(I, SizeOf(I));                      //  4 bytes

  I := Length(Item.Name);
  Stream.Write(I, SizeOf(I));
  if I > 0 then
  begin
    Stream.Write(Item.Name[1], I);
  end;
end;

procedure WriteHv04(Stream: TStream; const Item: THeader);
var
  I: longint;
begin
  Stream.Write(Marker, SizeOf(Marker));
  Stream.Write(Item.Flags, SizeOf(Item.Flags));

  if foVersion in Item.Flags then
    Stream.Write(Item.Version, SizeOf(Item.Version));

  if foMethod in Item.Flags then
    Stream.Write(Item.Method, SizeOf(Item.Method));

  if foDictionary in Item.Flags then
    Stream.Write(Item.Dictionary, SizeOf(Item.Dictionary));

  if foTable in Item.Flags then
    Stream.Write(Item.Table, SizeOf(Item.Table));
                                                          //  [ver04]
  Stream.Write(Item.Size,       SizeOf(Item.Size));       //  8 bytes
  Stream.Write(Item.Time,       SizeOf(Item.Time));       //  4 bytes
  Stream.Write(Item.Attr,       SizeOf(Item.Attr));       //  4 bytes
  Stream.Write(Item.Crc,        SizeOf(Item.Crc));        //  4 bytes
  Stream.Write(Item.PackedSize, SizeOf(Item.PackedSize)); //  8 bytes
  Stream.Write(Item.StartPos,   SizeOf(Item.StartPos));   //  8 bytes

  I := Length(Item.Name);
  Stream.Write(I, SizeOf(I));
  if I > 0 then
  begin
    Stream.Write(Item.Name[1], I);
  end;
end;

procedure THeaders.Write(Stream: TStream);
var
  I: longint;
  P: THeader;
  Ver: byte;
begin
  if Stream.Seek(0, 1) = 0 then
  begin
    if FSfx.Size > 0 then
    begin
      FSfx.Seek(0, 0);
      Stream.CopyFrom(FSfx, FSfx.Size);
    end;
  end else
    Stream.Seek(FSfx.Size, 0);

  SetLast;
  Ver := Ord(hv02);
  for I := 0 to FItems.Count - 1 do
  begin
    P := THeader(FItems[I]);
    if P.Action <> haDelete then
    begin
      if foVersion in P.Flags then
      begin
        Ver := P.Version;
      end;

      case Ver of
        Ord(hv02): WriteHv03(Stream, THeader(FItems[I]));
        Ord(hv03): WriteHv03(Stream, THeader(FItems[I]));
        Ord(hv04): WriteHv04(Stream, THeader(FItems[I]));
      end;
    end;
  end;
end;

procedure THeaders.Configure(Configuration: TConfiguration);
var
  P: THeader;
  I, Method, Dictionary: longint;
  CurrentExt, PreviousExt: string;
begin
  CurrentExt := '.';
  Configuration.Selector('\main');
  Method     := StrToInt(Configuration.CurrentSection.Values['Method']);
  Dictionary := StrToInt(Configuration.CurrentSection.Values['Dictionary']);
  Configuration.Selector('\m' + Configuration.CurrentSection.Values['Method']);

  if FNews > 0 then
  begin
    I := FItems.Count - FNews;

    P := FItems[I];
    Include(P.Flags, foVersion);
    Include(P.Flags, foMethod);
    Include(P.Flags, foDictionary);
    repeat
      P            := FItems[I];
      P.Method     := Method;
      P.Dictionary := Dictionary;

      PreviousExt := CurrentExt;
      if Length(FCL.fOption) = 0 then
        CurrentExt := ExtractFileExt(P.Name)
      else
        CurrentExt := FCL.fOption;

      if Length(FCL.pOption) > 0 then
      begin
        Include(P.Flags, foPassword);
      end;

      if (Method = 0) or (not Configuration.GetTable(CurrentExt, P.Table)) then
      begin
        Include(P.Flags, foMoved);
        Include(P.Flags, foTable);
      end else
        if CompareFileName(CurrentExt, PreviousExt) = 0 then
        begin
          Exclude(P.Flags, foTable);
          if FCL.sOption then
          begin
            Exclude(P.Flags, foTear);
          end;
        end else
          Include(P.Flags, foTable);

      Inc(I);
    until I = FItems.Count;

  end;
end;

function THeaders.Add(const Rec: TCustomSearchRec): int64;
begin
  if Search(Rec.Name) = nil then
  begin
    Inc(FNews);
    Insert(New(Rec));
    Result := Rec.Size;
  end else
    Result := 0;
end;

function THeaders.Update(const Rec: TCustomSearchRec): int64;
var
  Item: THeader;
begin
  Item := Search(Rec.Name);
  if (Item <> nil) and (Item.Time < Rec.Time) then
  begin
    if Item.Action = haNone then
      Item.Action := haUpdate;

    Item.Time := Rec.Time;
    Item.Link := Rec.Link;
    Result    := Rec.Size;
  end else
    Result := 0;
end;

function THeaders.Replace(const Rec: TCustomSearchRec): int64;
var
  Item: THeader;
begin
  Item := Search(Rec.Name);
  if Item <> nil then
  begin
    if Item.Action = haNone then
      Item.Action := haUpdate;

    Item.Time := Rec.Time;
    Item.Link := Rec.Link;
    Result    := Rec.Size;
  end else
    Result := 0;
end;

function THeaders.AddUpdate(const Rec: TCustomSearchRec): int64;
begin
  Result := Add(Rec);
  if Result = 0 then
  begin
    Result := Update(Rec);
  end;
end;

function Theaders.AddReplace(const Rec: TCustomSearchRec): int64;
begin
  Result := Add(Rec);
  if Result = 0 then
  begin
    Result := Replace(Rec);
  end;
end;

function Theaders.AddAutoRename(const Rec: TCustomSearchRec): int64;
var
  I: longint;
begin
  Result := Add(Rec);
  if Result = 0 then
  begin
    I := 0;
    repeat
       Inc(I);
    until Search(GenerateAlternativeFileName(Rec.Name, I, False)) = nil;

    Rec.Name := GenerateAlternativeFileName(Rec.Name, I, False);
    Result   := Add(Rec);
  end;
end;

function THeaders.LoadSfx(const FileName: string): boolean;
var
  Stream: TStream;
begin
  ClearSfx;
  Result := False;
  if FileExists(FileName) then
  begin
    Stream := CreateTFileReader(FileName, fmOpenRead);
    if Stream <> nil then
      try
        Result    := FSfx.CopyFrom(Stream, Stream.Size) = Stream.Size;
      finally
        Stream.Free;
      end;
  end;
end;

procedure THeaders.ClearSfx;
begin
  FSfx.Size := 0;
end;

function THeaders.GetSfxSize: longint;
begin
  Result := FSfx.Size;
end;

end.
