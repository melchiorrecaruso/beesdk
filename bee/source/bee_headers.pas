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

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_Headers;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  Bee_Files,
  Bee_Types,
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

  THeaderAction = (haNone, haUpdate, haDecode, haDecodeAndUpdate);

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
    Comment: string;
  public
    Action: THeaderAction; { reserved }
    ExtName: string;       { reserved }
    ExtSize: int64;        { reserved }
    Position: longint;     { reserved }
  end;

  { Header list compare function}

  THeaderListCompare = function(Item1, Item2: THeader): longint;

  { Header list class }

   THeaderList = class
   private
     FItems: TList;
     FNames: TList;
     function IndexOfItem(Item: THeader): longint;
     function IndexOfName(Item: THeader): longint;
     function GetItem(Index: longint): THeader;
     function GetName(Index: longint): THeader;
     function GetCount: longint;
   protected
     FCL: TCommandLine;
     procedure Insert(Item: THeader);
     procedure Clear;
   public
     constructor Create(CommandLine: TCommandLine);
     destructor Destroy; override;

     function Search(FileName: string): THeader; virtual;

     function SetAction(Masks: TStringList; MaskAct, Action: THeaderAction): longint; overload;
     function SetAction(Mask: string; MaskAct, Action: THeaderAction): longint; overload;
     function SetAction(Index: longint; Action: THeaderAction): longint; overload;
     function SetAction(Action: THeaderAction): longint; overload;

     function GetBack(Index: longint; Action: THeaderAction): longint; overload;
     function GetNext(Index: longint; Action: THeaderAction): longint; overload;
     function GetBack(Index: longint; Flag:    THeaderFlag): longint; overload;
     function GetNext(Index: longint; Flag:    THeaderFlag): longint; overload;

     property Items[Index: longint]: THeader read GetItem;
     property Names[Index: longint]: THeader read GetName;
     property Count: longint read GetCount;
   end;

  { Headers class }

  THeaders = class(THeaderList)
  private
    FNews: longint;
    FModule: TStream;
    function GetModuleSize: longint;
    function GetFirst(Stream: TStream): int64;
    procedure ReadB4b(Stream: TStream);
    function New(const Rec: TCustomSearchRec): THeader; overload;
  public
    constructor Create(CommandLine: TCommandLine);
    destructor Destroy; override;

    procedure Read(Stream: TStream); virtual;
    procedure Write(Stream: TStream); virtual;
    procedure Configure(Configuration: TConfiguration);

    procedure Delete(Index: longint);
    function Add(const Rec: TCustomSearchRec): boolean;
    function Update(const Rec: TCustomSearchRec): boolean;
    function Replace(const Rec: TCustomSearchRec): boolean;
    function AddUpdate(const Rec: TCustomSearchRec): boolean;
    function AddReplace(const Rec: TCustomSearchRec): boolean;
    function AddAutoRename(const Rec: TCustomSearchRec): boolean;

    procedure ClearModule;
    function LoadModule(const FileName: string): boolean;
  public
    property ModuleSize: longint read GetModuleSize;
  end;

  function MethodToStr(const Item: THeader): string;
  function VersionToStr(const Ver: byte): string;

implementation

uses
  Bee_Consts,
  Bee_Common,
  Bee_BlowFish;

function MethodToStr(const Item: THeader): string;
begin
  Result := 'm0a';
  if not (foTear in Item.Flags) then
  begin
    Result[1] := 's';
  end;

  if not (foMoved in Item.Flags) then
  begin
    if Item.Method in [1..3] then
      Result[2] := char(byte('0') + Item.Method)
    else
      Result[2] := '?';
  end;

  if Item.Dictionary in [0..9] then
    Result[3] := char(byte('a') + Item.Dictionary)
  else
    Result[3] := '?';
end;

function VersionToStr(const Ver: byte): string;
begin
  case Ver of
    Ord(hv02): Result := '0' + DecimalSeparator + '2';
    Ord(hv03): Result := '0' + DecimalSeparator + '3';
    Ord(hv04): Result := '0' + DecimalSeparator + '4';
    else       Result := '?' + DecimalSeparator + '?';
  end;
end;

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
  if (Item1.Position = -1) and (Item2.Position = -1) then
  begin
    Result := CompareFileName(ExtractFileExt(Item1.Name), ExtractFileExt(Item2.Name));

    if Result = 0 then
      Result := CompareFileName(ExtractFileName(Item1.Name), ExtractFileName(Item2.Name));

    if Result = 0 then
      Result := CompareFileName(Item1.Name, Item2.Name);

  end else
  begin
    if (Item1.Position <> -1) and (Item2.Position <> -1) then
    begin
      Result := -1;
    end else
      if (Item1.Position <> -1) then
        Result := -1
      else
        Result := 1;
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
    I := Compare(THeader(List[M]), Item);
    if I < 0 then
      L := M + 1
    else
      if I > 0 then
        H := M - 1
      else
        H := -2;
  end;

  if M = -2 then
    List.Add(Item)
  else
    if H = -2 then
    begin
      List.Insert(M + 1, Item);
    end else
    begin
      if I < 0 then
        List.Insert(M + 1, Item)
      else
        List.Insert(M, Item);
    end;
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

function THeaderList.IndexOfItem(Item: THeader): longint;
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

function THeaderList.IndexOfName(Item: THeader): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := 0 to FNames.Count - 1 do
    if (THeader(FNames[I]) = Item) then
    begin
      Result := I;
      Break;
    end;
end;

function THeaderList.SetAction(Masks: TStringList; MaskAct, Action: THeaderAction): longint;
var
  I: longint;
begin
  Result := 0;
  for  I := 0 to Masks.Count - 1 do
  begin
    Inc(Result, SetAction(Masks[I], MaskAct, Action));
  end;
end;

function THeaderList.SetAction(Mask: string; MaskAct, Action: THeaderAction): longint;
var
  I: longint;
  P: THeader;
begin
  Mask := FCL.cdOption + Mask;

  Result := 0;
  for I  := 0 to FItems.Count - 1 do
  begin
    P := THeader(FItems[I]);
    if (P.Action = MaskAct) and (FileNameMatch(P.Name, Mask, FCL.rOption)) then
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

function THeaderList.GetNext(Index: longint; Action: THeaderAction): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index to FItems.Count - 1 do
    if (Action = THeader(FItems[I]).Action) then
    begin
      Result := I;
      Break;
    end;
end;

function THeaderList.GetBack(Index: longint; Action: THeaderAction): longint;
var
  I: longint;
begin
  Result := -1;
  for  I := Index downto 0 do
    if (Action = THeader(FItems[I]).Action) then
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

function THeaderList.GetCount: longint;
begin
  Result := FItems.Count;
end;

function THeaderList.GetItem(Index: longint): THeader;
begin
  Result := FItems[Index];
end;

function THeaderList.GetName(Index: longint): THeader;
begin
  Result := FNames[Index];
end;

{ Headers class }

constructor THeaders.Create(CommandLine: TCommandLine);
begin
  inherited Create(CommandLine);
  FModule := TMemoryStream.Create;
  FNews   := 0;
end;

destructor THeaders.Destroy;
begin
  FModule.Free;
  inherited Destroy;
end;

function THeaders.GetFirst(Stream: TStream): int64;
var
  Id: longint;
  StrmPos: int64;
begin
  Result  := -1;
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
    FModule.Size := 0;
    FModule.CopyFrom(Stream, Result);
  end;
end;

function THeaders.New(const Rec: TCustomSearchRec): THeader;
begin
  Result := THeader.Create;
  // - Start header data - //
  Result.Flags      := [];
  Result.Version    := Ord(FCL.hvOption);
  Result.Method     := Ord(moFast);
  Result.Dictionary := Ord(do5MB);
  // Result.FileTable
  Result.Size       := -1;
  Result.Time       := Rec.Time;
  Result.Attr       := Rec.Attr;
  Result.Crc        := longword(-1);
  Result.PackedSize := -1;
  Result.StartPos   := -1;
  Result.Name       := FCL.cdOption + DeleteFileDrive(Rec.Name);
  // - End header data - //
  Result.Action     := haUpdate;
  Result.ExtName    := Rec.Name;
  Result.ExtSize    := Rec.Size;
  Result.Position   := -1;
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
  Item.Action   := haNone;
  Item.ExtName  := '';
  Item.ExtSize  := -1;
  Item.Position := -1;
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
  Item.Action   := haNone;
  Item.ExtName  := '';
  Item.ExtSize  := -1;
  Item.Position := -1;
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
          P.Position := FItems.Count;
          Insert(P);
        end;
        SymbolIndex := 0;
      end;

    end else Break;

  until (P <> nil) and (foLast in P.Flags);

  if P <> nil then Exclude(P.Flags, foLast);
end;

procedure THeaders.Read(Stream: TStream);
var
  P: THeader;
  Id: longint;
  I: longint;
  OffSet: int64;
  Ver: byte;
begin
  P     := nil;
  Ver   := Ord(hv02);

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
          P.Position := FItems.Count;
          Insert(P);
        end;
      end else Break;

    until (P <> nil) and (foLast in P.Flags);

    if P <> nil then Exclude(P.Flags, foLast);
  end else
  begin
    ReadB4b(Stream);
  end;

  //OffSet := Stream.Seek(0, 1);
  //FCheck := True;
  //I := 0;
  //while (I < FItems.Count) and FCheck do
  //begin
  //  FCheck := THeader(FItems[I]).StartPos = OffSet;
  //  Inc(OffSet, THeader(FItems[I]).PackedSize);
  //  Inc(I);
  //end;
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
    if FModule.Size > 0 then
    begin
      FModule.Seek(0, soBeginning);
      Stream.CopyFrom(FModule, FModule.Size);
    end;
  end else
    Stream.Seek(FModule.Size, 0);

  if FItems.Count > 0 then
  begin
    Include(Items[FItems.Count - 1].Flags, foLast);

    Ver := Ord(hv02);
    for I := 0 to FItems.Count - 1 do
    begin
      P := THeader(FItems[I]);
      if foVersion in P.Flags then
      begin
        Ver := P.Version;
      end;

      case Ver of
        Ord(hv02): WriteHv03(Stream, Items[I]);
        Ord(hv03): WriteHv03(Stream, Items[I]);
        Ord(hv04): WriteHv04(Stream, Items[I]);
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

      Include(P.Flags, foTear);
      Include(P.Flags, foTable);

      PreviousExt := CurrentExt;
      if Length(FCL.fOption) = 0 then
        CurrentExt := ExtractFileExt(P.Name)
      else
        CurrentExt := FCL.fOption;

      if Length(FCL.pOption) >= MinBlowFishKeyLength then
      begin
        Include(P.Flags, foPassword);
      end;

      if (Method = 0) or (not Configuration.GetTable(CurrentExt, P.Table)) then
      begin
        Include(P.Flags, foMoved);
        Exclude(P.Flags, foTable);
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

procedure THeaders.Delete(Index: longint);
 var
   I: longint;
   Item, Next: THeader;
begin
  Item := Items[Index];
  if Index < FItems.Count - 1 then
  begin
    Next := Items[Index + 1];
    if (foVersion in Item.Flags) and (not(foVersion in Next.Flags)) then
    begin
      Next.Version := Item.Version;
      Include(Next.Flags, foVersion);
    end;

    if (foMethod in Item.Flags) and (not(foMethod in Next.Flags)) then
    begin
      Next.Method := Item.Method;
      Include(Next.Flags, foMethod);
    end;

    if (foDictionary in Item.Flags) and (not(foDictionary in Next.Flags)) then
    begin
      Next.Dictionary := Item.Dictionary;
      Include(Next.Flags, foDictionary);
    end;

    if (foTable in Item.Flags) and (not(foTable in Next.Flags)) then
    begin
      Next.Table := Item.Table;
      Include(Next.Flags, foTable);
    end;

    if (foTear in Item.Flags) and (not(foTear in Next.Flags)) then
    begin
      Include(Next.Flags, foTear);
    end;
  end;

  FNames.Delete(IndexOfName(Item));
  FItems.Delete(Index);
  Item.Destroy;
end;

function THeaders.Add(const Rec: TCustomSearchRec): boolean;
begin
  Result := Search(DeleteFileDrive(Rec.Name)) = nil;
  if Result then
  begin
    Inc(FNews);
    Insert(New(Rec));
  end;
end;

function THeaders.Update(const Rec: TCustomSearchRec): boolean;
var
  Item: THeader;
begin
  Item := Search(DeleteFileDrive(Rec.Name));
  Result := (Item <> nil) and (Item.Time < Rec.Time);

  if Result then
  begin
    Item.Action   := haUpdate;
    Item.ExtName  := Rec.Name;
    Item.ExtSize  := Rec.Size;
    Item.Time     := Rec.Time;
    Item.Attr     := Rec.Attr;
  end;
end;

function THeaders.Replace(const Rec: TCustomSearchRec): boolean;
var
  Item: THeader;
begin
  Item := Search(DeleteFileDrive(Rec.Name));
  Result := Item <> nil;

  if Result then
  begin
    Item.Action   := haUpdate;
    Item.ExtName  := Rec.Name;
    Item.ExtSize  := Rec.Size;
    Item.Time     := Rec.Time;
    Item.Attr     := Rec.Attr;
  end;
end;

function THeaders.AddUpdate(const Rec: TCustomSearchRec): boolean;
begin
  Result := Add(Rec);
  if not Result then
  begin
    Result := Update(Rec);
  end;
end;

function Theaders.AddReplace(const Rec: TCustomSearchRec): boolean;
begin
  Result := Add(Rec);
  if not Result then
  begin
    Result := Replace(Rec);
  end;
end;

function Theaders.AddAutoRename(const Rec: TCustomSearchRec): boolean;
var
  I: longint;
begin
  Result := Add(Rec);
  if not Result then
  begin
    I := 0;
    repeat
       Inc(I);
    until Search(GenerateAlternativeFileName(Rec.Name, I, False)) = nil;
    Rec.Name := GenerateAlternativeFileName(Rec.Name, I, False);
    Result   := Add(Rec);
  end;
end;

function THeaders.LoadModule(const FileName: string): boolean;
var
  Stream: TStream;
begin
  ClearModule;
  Result := False;
  if FileExists(FileName) then
  begin
    Stream := CreateTFileReader(FileName, fmOpenRead);
    if Stream <> nil then
      try
        Result    := FModule.CopyFrom(Stream, Stream.Size) = Stream.Size;
      finally
        Stream.Free;
      end;
  end;
end;

procedure THeaders.ClearModule;
begin
  FModule.Size := 0;
end;

function THeaders.GetModuleSize: longint;
begin
  Result := FModule.Size;
end;

end.
