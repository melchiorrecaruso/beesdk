{
  Copyright (c) 1999-2009 Andrew Filinsky and Melchiorre Caruso

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

    v0.8.0 build 1024 - 2009.04.17 by Melchiorre Caruso.
}

unit Bee_Headers;

{$I compiler.inc}

interface

uses
  Classes,           // TList
  SysUtils,          // TSearchRec
  Bee_CommandLine,   // TCommandLine
  Bee_Configuration; // TConfiguration

type
  // Header flags

  THeaderFlag =
    (foVersion,  foMethod,   foDictionary, foTable,    foTear,     foMoved,
     foLast,     foPassword, fo09Unused,   fo10Unused, fo11Unused, fo12Unused,
     fo13Unused, fo14Unused, fo15Unused,   fo16Unused, fo17Unused, fo18Unused,
     fo19Unused, fo20Unused, fo21Unused,   fo22Unused, fo23Unused, fo24Unused,
     fo25Unused, fo26Unused, fo27Unused,   fo28Unused, fo29Unused, fo30Unused,
     fo31Unused, fo32Unused);

  THeaderFlags = set of THeaderFlag;

type
  // Header actions

  THeaderAction = (toUpdate, toFresh, toCopy,   toSwap,   toExtract, toTest,
                   toSkip,   toQuit,  toDelete, toRename, toList,    toNone);

  THeaderActions = set of THeaderAction;

type
  // Header structure, order of fields is significant

  THeader = class
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
    FileName: string;
    // - End header data - //
    FileLink: string;
    FileStartPos: int64;
    FileAction: THeaderAction;
  end;

type
  // Headers list

  THeaders = class
  public
    constructor Create(aCommandLine: TCommandLine);
    destructor Destroy; override;
    procedure Clear;
  public
    function  AddItems  (aConfiguration: TConfiguration): int64;
    procedure ReadItems (aStream: TStream; aAction: THeaderAction);
    procedure WriteItems(aStream: TStream);

    function  MarkItems(Masks: TStringList; MaskAct, aAction: THeaderAction): longint; overload;
    function  MarkItems(Mask: string; MaskAct, aAction: THeaderAction): longint; overload;
    procedure MarkItem(aIndex: longint; aAction: THeaderAction);
    procedure MarkAll(aAction: THeaderAction);

    function GetBack(aChild: longint; aAction:  THeaderAction): longint; overload;
    function GetNext(aChild: longint; aAction:  THeaderAction): longint; overload;
    function GetBack(aChild: longint;   aFlag:  THeaderFlag):   longint; overload;
    function GetNext(aChild: longint;   aFlag:  THeaderFlag):   longint; overload;
    function GetBack(aChild: longint; aAction:  THeaderAction;  const aFileName: string): longint; overload;
    function GetNext(aChild: longint; aAction:  THeaderAction;  const aFileName: string): longint; overload;
    function GetBack(aChild: longint; aActions: THeaderActions; const aFileName: string): longint; overload;
    function GetNext(aChild: longint; aActions: THeaderActions; const aFileName: string): longint; overload;

    function GetSize (aAction: THeaderAction): int64; overload;
    function GetSize (aActions: THeaderActions): int64; overload;
    function GetPackedSize(aAction: THeaderAction): int64; overload;
    function GetPackedSize(aActions: THeaderActions): int64; overload;

    function GetCount(Actions: THeaderActions): longint; overload;
    function GetCount: longint; overload;

    function GetItem(aIndex: longint): THeader;

    function SetModule(const aFileName: string): boolean;
    function GetModule: longint;
  private
    FNews: longint;
    FModule: TStream;
    FPrimary: TList;
    FSecondary: TList;
    FCommandLine: TCommandLine;
  private
    procedure AddItem(P: THeader);
    function Compare(P1, P2: THeader): longint;
    function CreatePHeader(const RecPath: string; const Rec: TSearchRec): THeader; overload;
    function CreatePHeader(aStream: TStream; aAction: THeaderAction; var aVersion: byte): THeader; overload;
    function FindFirstMarker(aStream: TStream): int64;
    procedure MarkAsLast(aAction: THeaderAction);
    procedure ReadItemsB4b(aStream: TStream; aAction: THeaderAction);
    function SearchItem(FileName: string): THeader;
    procedure ScanFileSystem(Mask: string; var Size: int64);
    procedure SortNews(aConfiguration: TConfiguration);
    procedure WriteItem(aStream: TStream; P: THeader; var aVersion: byte);
  end;

implementation

uses
  Math,
  Bee_Files,
  Bee_Consts,
  Bee_Common;

// THeaders class

constructor THeaders.Create;
begin
  inherited Create;
  FCommandLine := aCommandLine;
  FModule      := TMemoryStream.Create;
  FSecondary   := TList.Create;
  FPrimary     := TList.Create;
  FNews        := 0;
end;

destructor THeaders.Destroy;
begin
  Clear;
  FPrimary.Free;
  FSecondary.Free;
  FModule.Free;
  FCommandLine := nil;
  inherited Destroy;
end;

procedure THeaders.Clear;
var
  I: longint;
begin
  FNews := 0;
  FModule.Size := 0;
  for I := 0 to FPrimary.Count -1 do
  begin
    THeader(FPrimary.Items[I]).Free;
  end;
  FPrimary.Clear;
  FSecondary.Clear;
end;

function THeaders.CreatePHeader(const RecPath: string; const Rec: TSearchRec): THeader;
begin
  Result := THeader.Create;
  try
    // - Start header data - //
    Result.FileFlags := [foTear, foTable];
    Result.FileVersion := Min(Max(ver02, FCommandLine.vOption), ver04);
    Result.FileMethod := 1;
    Result.FileDictionary := 2;
    // Result.FileTable
    Result.FileSize := Rec.Size;
    Result.FileTime := Rec.Time;
    Result.FileAttr := Rec.Attr;
    Result.FileCrc := longword(-1);
    Result.FilePacked := 0;
    Result.FileName := FCommandLine.cdOption + DeleteFileDrive(RecPath) + Rec.Name;
    // - End header data - //
    Result.FileLink := RecPath + Rec.Name;
    Result.FileStartPos := 0;
    Result.FileAction := toUpdate;
  except
    FreeAndNil(Result);
  end;
end;

function THeaders.CreatePHeader(aStream: TStream; aAction: THeaderAction; var aVersion: byte): THeader;
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

      if foMethod     in FileFlags then aStream.Read(FileMethod,     SizeOf(FileMethod));
      if foDictionary in FileFlags then aStream.Read(FileDictionary, SizeOf(FileDictionary));
      if foTable      in FileFlags then aStream.Read(FileTable,      SizeOf(FileTable));

      if foVersion    in FileFlags then aVersion := FileVersion;

      if aVersion < ver04 then
      begin                                            //  [ver03] | [ver04]
        aStream.Read(I, SizeOf(I)); FileSize := I;     //  4 bytes | 8 bytes

        aStream.Read(FileTime, SizeOf(FileTime));      //  4 bytes | 4 bytes
        aStream.Read(FileAttr, SizeOf(FileAttr));      //  4 bytes | 4 bytes
        aStream.Read(FileCrc,  SizeOf(FileCrc));       //  4 bytes | 4 bytes

        aStream.Read(I, SizeOf(I)); FilePacked   := I; //  4 bytes | 8 bytes
        aStream.Read(I, SizeOf(I)); FileStartPos := I; //  4 bytes |    ---
      end else
        aStream.Read(FileSize,
          SizeOf(FileSize) +
          SizeOf(FileTime) +
          SizeOf(FileAttr) +
          SizeOf(FileCrc)  +
          SizeOf(FilePacked));

      aStream.Read(I, SizeOf(I));
      if I > 0 then
      begin
        SetLength(FileName, I);
        aStream.Read(FileName[1], I);
        FileName := DoDirSeparators(FileName);
      end else
        SetLength(FileName, 0);

      SetLength(FileLink, 0);
      FileAction := aAction;
    end;
  except
    FreeAndNil(Result);
  end;
end;

function THeaders.Compare(P1, P2: THeader): longint;
begin
  Result := CompareFileName(
    ExtractFileExt(P1.FileName),
    ExtractFileExt(P2.FileName));

  if Result = 0 then
    Result := CompareFileName(
      ExtractFileName(P1.FileName),
      ExtractFileName(P2.FileName));

  if Result = 0 then
    Result := CompareFileName(
      P1.FileName,
      P2.FileName);
end;

procedure THeaders.AddItem(P: THeader);
var
  L, M, H, I: longint;
begin
  // Add item to secondary list
  L :=  0;
  M := -2;
  H := FSecondary.Count -1;
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
    end else
      FSecondary.Insert(M + 1, P);

  // Add item to primary list
  if P.FileAction = toUpdate then
  begin

    L := FPrimary.Count - FNews;
    M := -2;

    if FNews <> 0 then
      H := FPrimary.Count -1
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
      end else
        FPrimary.Insert(M + 1, P);

    Inc(FNews);
  end else
  begin
    FPrimary.Insert(FPrimary.Count - FNews, P)
  end;
end;

function THeaders.SearchItem(FileName: string): THeader;
var
  L, M, H, I: longint;
  S: string;
begin
  L :=  0;
  H := FSecondary.Count -1;
  FileName := FCommandLine.cdOption + FileName;
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
    Result := nil
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

    if foMethod     in FileFlags then aStream.Write(FileMethod,     SizeOf(FileMethod));
    if foDictionary in FileFlags then aStream.Write(FileDictionary, SizeOf(FileDictionary));
    if foTable      in FileFlags then aStream.Write(FileTable,      SizeOf(FileTable));

    if aVersion < ver04 then
    begin                                             //  [ver03] | [ver04]
      I := FileSize;     aStream.Write(I, SizeOf(I)); //  4 bytes | 8 bytes

      aStream.Write(FileTime, SizeOf(FileTime));      //  4 bytes | 4 bytes
      aStream.Write(FileAttr, SizeOf(FileAttr));      //  4 bytes | 4 bytes
      aStream.Write(FileCrc,  SizeOf(FileCrc));       //  4 bytes | 4 bytes

      I := FilePacked;   aStream.Write(I, SizeOf(I)); //  4 bytes | 8 bytes
      I := FileStartPos; aStream.Write(I, SizeOf(I)); //  4 bytes |    ---
    end else
      aStream.Write(FileSize,
        SizeOf(FileSize) +
        SizeOf(FileTime) +
        SizeOf(FileAttr) +
        SizeOf(FileCrc)  +
        SizeOf(FilePacked));

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

  Version := ver02;
  MarkAsLast(toDelete);
  for I := 0 to FPrimary.Count -1 do
    if THeader(FPrimary.Items[I]).FileAction <> toDelete then
    begin
      WriteItem(aStream, THeader(FPrimary.Items[I]), Version);
    end;
end;

function THeaders.AddItems(aConfiguration: TConfiguration): int64;
var
  I, J: longint;
  S: TStringList;
begin
  Result := 0;
  with FCommandLine do
  begin
    for I := 0 to FileMasks.Count -1 do
    begin
      S := TStringList.Create;
      ExpandMask(FileMasks.Strings[I], S, rOption);
      for J := 0 to S.Count -1 do
      begin
        ScanFileSystem(S.Strings[J], Result);
      end;
      S.Free;
    end;
  end;
  SortNews(aConfiguration);
end;

function THeaders.MarkItems(Masks: TStringList; MaskAct: THeaderAction; aAction: THeaderAction): longint;
var
  I, J: longint;
begin
  Result := 0;
  for  I := 0 to Masks.Count -1 do
  begin
    Inc(Result, MarkItems(Masks.Strings[I], MaskAct, aAction));
  end;
end;

function THeaders.MarkItems(Mask: string; MaskAct: THeaderAction; aAction: THeaderAction): longint;
var
  I: longint;
begin
  Mask := FCommandLine.cdOption + Mask;

  Result := 0;
  for  I := 0 to FPrimary.Count -1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction = MaskAct) and (FileNameMatch(FileName, Mask, FCommandLine.rOption)) then
      begin
        FileAction := aAction;
        Inc(Result);
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
  for I := 0 to FPrimary.Count -1 do
  begin
    THeader(FPrimary.Items[I]).FileAction := aAction;
  end;
end;

procedure THeaders.MarkAsLast(aAction: THeaderAction);
var
  I: longint;
begin
  for I := FPrimary.Count -1 downto 0 do
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
      if Length(FCommandLine.eOption) = 0 then
        CurrentExt := ExtractFileExt(P.FileName)
      else
        CurrentExt := FCommandLine.eOption;

      if FCommandLine.kOption then Include(P.FileFlags, foPassword);

      if (Method = 0) or (not aConfiguration.GetTable(CurrentExt, P.FileTable)) then
      begin
        Include(P.FileFlags, foMoved);
        Include(P.FileFlags, foTable);
      end else
        if CompareFileName(CurrentExt, PreviousExt) = 0 then
        begin
          Exclude(P.FileFlags, foTable);
          if FCommandLine.sOption then
          begin
            Exclude(P.FileFlags, foTear);
          end;
        end else
          Include(P.FileFlags, foTable);

      Inc(I);
    until I = FPrimary.Count;
  end;
end;

function THeaders.FindFirstMarker(aStream: TStream): int64;
var
  Id: longint;
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
  P: THeader;
  Ptr: ^longint;
  Symbol: byte;
  SymbolIndex: longint;
  B4bMarker: array [0..3] of byte;
  Version: byte;
begin
  P       := nil;
  Ptr     := @B4bMarker;
  Ptr^    := Marker;
  Version := ver02;

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
        P := CreatePHeader(aStream, aAction, Version);
        if P <> nil then
        begin
          AddItem(P);
        end;
        SymbolIndex := 0;
      end;

    end else Break;

  until (P <> nil) and (foLast in P.FileFlags);

  if P <> nil then
  begin
    Exclude(P.FileFlags, foLast);
  end;
end;

procedure THeaders.ReadItems(aStream: TStream; aAction: THeaderAction);
var
  P: THeader;
  I: longint;
  Id: longint;
  OffSet: int64;
  Version: byte;
begin
  P := nil;
  Version := ver02;

  OffSet  := FindFirstMarker(aStream);
  if OffSet > -1 then
  begin
    aStream.Seek(OffSet, 0);
    repeat
      if (aStream.Read(Id, SizeOf(Id)) = SizeOf(Id)) and (Id = Marker) then
      begin
        P := CreatePHeader(aStream, aAction, Version);
        if P <> nil then
        begin
          AddItem(P);
        end;
      end else
        Break;
    until (P <> nil) and (foLast in P.FileFlags);

    if P <> nil then
      Exclude(P.FileFlags, foLast);
  end else
    ReadItemsB4b(aStream, aAction);

  //OffSet := aStream.Seek(0, 1);
  //for I := 0 to FPrimary.Count -1 do
  //  with THeader(FPrimary.Items[I]) do
  //  begin
  //    FileStartPos := OffSet;
  //    Inc(OffSet, FilePacked);
  //  end;
end;

function THeaders.GetNext(aChild: longint; aAction: THeaderAction): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild to FPrimary.Count -1 do
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
  for I := aChild to FPrimary.Count -1 do
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

function THeaders.GetNext(aChild: longint; aAction: THeaderAction; const aFileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild to FPrimary.Count -1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction = aAction) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetBack(aChild: longint; aActions: THeaderActions; const aFileName: string): longint;
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

function THeaders.GetNext(aChild: longint; aActions: THeaderActions; const aFileName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := aChild to FPrimary.Count -1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction in aActions) and (CompareFileName(FileName, aFileName) = 0) then
      begin
        Result := I;
        Break;
      end;
end;

function THeaders.GetBack(aChild: longint; aAction: THeaderAction; const aFileName: string): longint;
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
  for  I := 0 to FPrimary.Count -1 do
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
  for  I := 0 to FPrimary.Count -1 do
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
  for  I := 0 to FPrimary.Count -1 do
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
  for  I := 0 to FPrimary.Count -1 do
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
  for  I := 0 to FPrimary.Count -1 do
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
        Result      := True
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

procedure THeaders.ScanFileSystem(Mask: string; var Size: int64);
var
  P: THeader;
  Error: longint;
  Rec: TSearchRec;
  RecPath: string;
  RecName: string;
  Recursive: boolean;
begin
  Recursive := FCommandLine.rOption;
  Mask      := ExcludeTrailingBackSlash(Mask);
  if DirectoryExists(Mask) then
  begin
    Recursive := True;
    Mask      := IncludeTrailingBackSlash(Mask) + '*';
  end;
  RecPath := ExtractFilePath(Mask);

  Error   := FindFirst(RecPath + '*', faAnyFile, Rec);
  while Error = 0 do
  begin
    RecName := RecPath + Rec.Name;
    if (Rec.Attr and faDirectory) = 0 then
    begin

      if (FileNameMatch(RecName, Mask, Recursive)) then
        if (FileNameMatch(RecName, FCommandLine.xOption, Recursive) = False) then
        begin
          P := SearchItem(DeleteFileDrive(RecName));

          if (FCommandLine.fOption xor FCommandLine.uOption) = False then
          begin
            if (P = nil) then
            begin
              P := CreatePHeader(RecPath, Rec);
              AddItem(P);
              Size := Size + Rec.Size;
            end else
              if (Rec.Time > P.FileTime) then
              begin
                if P.FileAction = toCopy then
                begin
                  P.FileAction := toFresh;
                end;
                P.FileLink   := RecName;
                P.FileTime   := Rec.Time;
                Size := Size + (Rec.Size - P.FileSize);
              end;
          end else
            if FCommandLine.fOption then
            begin
              if (P <> nil) and (Rec.Time > P.FileTime) then
              begin
                if P.FileAction = toCopy then
                begin
                  P.FileAction := toFresh;
                end;
                P.FileLink   := RecName;
                P.FileTime   := Rec.Time;
                Size := Size + (Rec.Size - P.FileSize);
              end;
            end else
              if (P = nil) then
              begin
                P := CreatePHeader(RecPath, Rec);
                AddItem(P);
                Size := Size + Rec.Size;
              end;
        end;

    end else
      if Recursive and (Rec.Name <> '.') and (Rec.Name <> '..') then
      begin
        ScanFileSystem(IncludeTrailingBackSlash(RecName) + ExtractFileName(Mask), Size);
      end;

    Error := FindNext(Rec);
  end;
  FindClose(Rec);
end;

end.
