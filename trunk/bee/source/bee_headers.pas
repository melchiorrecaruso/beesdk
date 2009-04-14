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

    v0.7.9 build 0994 - 2009.04.14 by Melchiorre Caruso.
}

unit Bee_Headers;

{$I compiler.inc}

interface

uses
  Classes,           // TList
  SysUtils,          // TSearchRec
  Bee_CommandLine,   // TCommandLine
  Bee_Configuration; // TConfiguration

const
  // Id marker

  Marker: integer = 442852674;

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
    // Start header data
    FileFlags: THeaderFlags;
    FileVersion: byte;
    FileMethod: byte;
    FileDictionary: byte;
    FileTable: TTableParameters;
    FileSize: cardinal;
    FileTime: integer;
    FileAttr: integer;
    FileCrc: cardinal;
    FilePacked: cardinal;
    FileStartPos: cardinal;
    FileName: string;
    // End header data
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

    function  MarkItems(Masks: TStringList; MaskAct, aAction: THeaderAction): integer; overload;
    function  MarkItems(Mask: string; MaskAct, aAction: THeaderAction): integer; overload;
    procedure MarkItem(aIndex: integer; aAction: THeaderAction);
    procedure MarkAll(aAction: THeaderAction);

    function GetBack(aChild: integer; aAction:  THeaderAction): integer; overload;
    function GetNext(aChild: integer; aAction:  THeaderAction): integer; overload;
    function GetBack(aChild: integer;   aFlag:  THeaderFlag):   integer; overload;
    function GetNext(aChild: integer;   aFlag:  THeaderFlag):   integer; overload;
    function GetBack(aChild: integer; aAction:  THeaderAction;  const aFileName: string): integer; overload;
    function GetNext(aChild: integer; aAction:  THeaderAction;  const aFileName: string): integer; overload;
    function GetBack(aChild: integer; aActions: THeaderActions; const aFileName: string): integer; overload;
    function GetNext(aChild: integer; aActions: THeaderActions; const aFileName: string): integer; overload;

    function GetSize (aAction: THeaderAction): int64; overload;
    function GetSize (aActions: THeaderActions): int64; overload;
    function GetPackedSize(aAction: THeaderAction): int64; overload;
    function GetPackedSize(aActions: THeaderActions): int64; overload;

    function GetCount(Actions: THeaderActions): integer; overload;
    function GetCount: integer; overload;

    function GetItem(aIndex: integer): THeader;

    function SetModule(const aFileName: string): boolean;
    function GetModule: integer;
  private
    FModule:  TStream;
    FPrimary:   TList;
    FSecondary: TList;
    FCommandLine: TCommandLine;
  private
    procedure AddItem(P: THeader);
    function Compare(P1, P2: THeader): integer;
    function CreatePHeader(const RecPath: string; const Rec: TSearchRec): THeader; overload;
    function CreatePHeader(Stream: TStream; aAction: THeaderAction): THeader; overload;
    function FindFirstMarker(aStream: TStream): int64;
    procedure FreePHeader(P: THeader);
    procedure MarkAsLast(aAction: THeaderAction);
    procedure ReadItemsB4b(aStream: TStream; aAction: THeaderAction);
    function SearchItem(FileName: string): THeader;
    procedure ScanFileSystem(Mask: string; var Size: int64);
    procedure SortNews(aConfiguration: TConfiguration);
    procedure WriteItem (aStream: TStream; P: THeader);
  end;

implementation

uses
  Math,
  Bee_Files,
  Bee_Common;

// THeaders class

constructor THeaders.Create;
begin
  inherited Create;
  FCommandLine := aCommandLine;
  FModule      := TMemoryStream.Create;
  FSecondary   := TList.Create;
  FPrimary     := TList.Create;
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
  I: integer;
begin
  FModule.Size := 0;
  for I := 0 to FPrimary.Count -1 do
  begin
    FreePHeader(THeader(FPrimary.Items[I]));
  end;
  FPrimary.Clear;
  FSecondary.Clear;
end;

function THeaders.CreatePHeader(const RecPath: string; const Rec: TSearchRec): THeader;
begin
  Result := THeader.Create;
  try
    Result.FileFlags := [foTear, foTable];
    Result.FileVersion := 1; // Bee 0.3.x
    Result.FileMethod := 1;
    Result.FileDictionary := 2;
    // Result.FileTable
     Result.FileSize := 0;
    Result.FileTime := Rec.Time;
    // Result.FileAttr
    Result.FileCrc := cardinal(-1);
    // Result.FilePacked
    // Result.FileStartPos
    Result.FileName := RecPath + Rec.Name;
    // ---
    Result.FileAction := toUpdate;
  except
    FreePHeader(Result);
    Result := nil;
  end;
end;

function THeaders.CreatePHeader(Stream: TStream; aAction: THeaderAction): THeader;
var
  J: integer;
begin
  Result := THeader.Create;
  try
    with Result do
    begin
      Stream.Read(FileFlags, SizeOf(FileFlags));

      if foVersion    in FileFlags then Stream.Read(FileVersion,    SizeOf(FileVersion));
      if foMethod     in FileFlags then Stream.Read(FileMethod,     SizeOf(FileMethod));
      if foDictionary in FileFlags then Stream.Read(FileDictionary, SizeOf(FileDictionary));
      if foTable      in FileFlags then Stream.Read(FileTable,      SizeOf(FileTable));

      Stream.Read(FileSize,
        SizeOf(FileSize) + SizeOf(FileTime)   + SizeOf(FileAttr) +
        SizeOf(FileCrc)  + SizeOf(FilePacked) + SizeOf(FileStartPos));

      Stream.Read(J, SizeOf(J));
      if J > 0 then
      begin
        SetLength(FileName, J);
        Stream.Read(FileName[1], J);
        FileName := DoDirSeparators(FileName);
      end else
        SetLength(FileName, 0);

      FileAction := aAction;
    end;
  except
    FreePHeader(Result);
    Result := nil;
  end;
end;

procedure THeaders.FreePHeader(P: THeader);
begin
  if Assigned(P) then
  begin
    SetLength(P.FileName, 0);
  end;
  FreeMem(P);
end;

function THeaders.Compare(P1, P2: THeader): integer;
var
  B1, B2: boolean;
begin
  with P1 do B1 := (FileAction = toUpdate);
  with P2 do B2 := (FileAction = toUpdate);

  if (B1 and B2) then
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

  end else
    if B1 then
      Result := 1
    else
      if B2 then
        Result := -1
      else
        Result := 0;
end;

procedure THeaders.AddItem(P: THeader);
var
  L, M, H, I: integer;
begin
  // Add item to secondary list
  L := 0;
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

  if H = -1 then
    FSecondary.Add(P)
  else
    if H <> -2 then
    begin
      if I > 0 then
        FSecondary.Insert(M + 1, P)
      else
        FSecondary.Insert(M, P);
    end;


  // Add item to primary list
  if P.FileAction = toUpdate then
  begin
    L := Max(0, GetNext(0, toUpdate));
    H := GetBack(FPrimary.Count -1, toUpdate);
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

    if H = -1 then
      FPrimary.Add(P)
    else
      if H <> -2 then
      begin
        if I > 0 then
          FPrimary.Insert(M + 1, P)
        else
          FPrimary.Insert(M, P);
      end;

  end else
  begin
    I := GetNext(0, toUpdate);
    if I > -1 then
      FPrimary.Insert(I, P)
    else
      FPrimary.Add(P);
  end;
end;


function THeaders.SearchItem(FileName: string): THeader;
var
  L, M, H, I: integer;
  S: string;
begin
  L := 0;
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

procedure THeaders.WriteItem(aStream: TStream; P: THeader);
var
  J: integer;
  S: string;
begin
  aStream.Write(Marker, SizeOf(Marker));
  aStream.Write(P.FileFlags, SizeOf(P.FileFlags));
  with P do
  begin
    if foVersion    in FileFlags then aStream.Write(FileVersion,    SizeOf(FileVersion));
    if foMethod     in FileFlags then aStream.Write(FileMethod,     SizeOf(FileMethod));
    if foDictionary in FileFlags then aStream.Write(FileDictionary, SizeOf(FileDictionary));
    if foTable      in FileFlags then aStream.Write(FileTable,      SizeOf(FileTable));

    aStream.Write(FileSize,
      SizeOf(FileSize) + SizeOf(FileTime)   + SizeOf(FileAttr) +
      SizeOf(FileCrc)  + SizeOf(FilePacked) + SizeOf(FileStartPos));
  end;

  with FCommandLine do
  begin
    case P.FileAction of
      toUpdate: S := cdOption + DeleteFileDrive(P.FileName);
      toFresh:  S := cdOption + DeleteFileDrive(P.FileName);
      else      S := P.FileName;
    end;
  end;
  J := Length(S);
  aStream.Write(J, SizeOf(J));
  if J <> 0 then
  begin
    aStream.Write(S[1], J);
  end;
end;

procedure THeaders.WriteItems(aStream: TStream);
var
  I: integer;
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

  MarkAsLast(toDelete);
  for I := 0 to FPrimary.Count -1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction <> toDelete) then
      begin
        WriteItem(aStream, FPrimary.Items[I]);
      end;
end;

function THeaders.AddItems(aConfiguration: TConfiguration): int64;
var
  I, J: integer;
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

function THeaders.MarkItems(Masks: TStringList; MaskAct: THeaderAction; aAction: THeaderAction): integer;
var
  I, J: integer;
begin
  Result := 0;
  for  I := 0 to Masks.Count -1 do
  begin
    Inc(Result, MarkItems(Masks.Strings[I], MaskAct, aAction));
  end;
end;

function THeaders.MarkItems(Mask: string; MaskAct: THeaderAction; aAction: THeaderAction): integer;
var
  I: integer;
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

procedure THeaders.MarkItem(aIndex: integer; aAction: THeaderAction);
begin
  THeader(FPrimary.Items[aIndex]).FileAction := aAction;
end;

procedure THeaders.MarkAll(aAction: THeaderAction);
var
  I: integer;
begin
  for I := 0 to FPrimary.Count -1 do
  begin
    THeader(FPrimary.Items[I]).FileAction := aAction;
  end;
end;

procedure THeaders.MarkAsLast(aAction: THeaderAction);
var
  I: integer;
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
  I, First, Method, Dictionary: integer;
  CurrentExt, PreviousExt: string;
begin
  aConfiguration.Selector('\main');
  Method     := StrToInt(aConfiguration.CurrentSection.Values['Method']);
  Dictionary := StrToInt(aConfiguration.CurrentSection.Values['Dictionary']);
  aConfiguration.Selector('\m' + aConfiguration.CurrentSection.Values['Method']);

  First := GetNext(0, toUpdate);
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
  Id: integer;
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
  Ptr: ^integer;
  Symbol: byte;
  SymbolIndex: integer;
  B4bMarker: array [0..3] of byte;
begin
  P    := nil;
  Ptr  := @B4bMarker;
  Ptr^ := Marker;

  SymbolIndex  := 0;
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
        P := CreatePHeader(aStream, aAction);
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
  Id: integer;
  OffSet: int64;
begin
  P      := nil;
  OffSet := FindFirstMarker(aStream);

  if OffSet > -1 then
  begin
    aStream.Seek(OffSet, 0);
    repeat
      if (aStream.Read(Id, SizeOf(Id)) = SizeOf(Id)) and (Id = Marker) then
      begin
        P := CreatePHeader(aStream, aAction);
        if P <> nil then
        begin
          AddItem(P);
        end;

      end else Break;

    until (P <> nil) and (foLast in P.FileFlags);

    if P <> nil then
    begin
      Exclude(P.FileFlags, foLast);
    end;

  end else
    ReadItemsB4b(aStream, aAction);
end;

function THeaders.GetNext(aChild: integer; aAction: THeaderAction): integer;
var
  I: integer;
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

function THeaders.GetBack(aChild: integer; aAction: THeaderAction): integer;
var
  I: integer;
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

function THeaders.GetNext(aChild: integer; aFlag: THeaderFlag): integer;
var
  I: integer;
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

function THeaders.GetBack(aChild: integer; aFlag: THeaderFlag): integer;
var
  I: integer;
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

function THeaders.GetNext(aChild: integer; aAction: THeaderAction; const aFileName: string): integer;
var
  I: integer;
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

function THeaders.GetBack(aChild: integer; aActions: THeaderActions; const aFileName: string): integer;
var
  I: integer;
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

function THeaders.GetNext(aChild: integer; aActions: THeaderActions; const aFileName: string): integer;
var
  I: integer;
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

function THeaders.GetBack(aChild: integer; aAction: THeaderAction; const aFileName: string): integer;
var
  I: integer;
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
  I: integer;
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
  I: integer;
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
  I: integer;
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
  I: integer;
begin
  Result := 0;
  for  I := 0 to FPrimary.Count -1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction in aActions) then
      begin
        Inc(Result, FilePacked);
      end;
end;

function THeaders.GetCount(Actions: THeaderActions): integer;
var
  I: integer;
begin
  Result := 0;
  for  I := 0 to FPrimary.Count -1 do
    with THeader(FPrimary.Items[I]) do
      if (FileAction in Actions) then
      begin
        Inc(Result);
      end;
end;

function THeaders.GetCount: integer;
begin
  Result := FPrimary.Count;
end;

function THeaders.GetItem(aIndex: integer): THeader;
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

function THeaders.GetModule: integer;
begin
  if Assigned(FModule) then
    Result := FModule.Size
  else
    Result := 0;
end;

procedure THeaders.ScanFileSystem(Mask: string; var Size: int64);
var
  P: THeader;
  Error: integer;
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
                P.FileAction := toFresh;
                P.FileName   := RecName;
                P.FileTime   := Rec.Time;
                Size := Size + (Rec.Size - P.FileSize);
              end;
          end else
            if FCommandLine.fOption then
            begin
              if (P <> nil) and (Rec.Time > P.FileTime) then
              begin
                P.FileAction := toFresh;
                P.FileName   := RecName;
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
