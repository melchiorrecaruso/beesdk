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

    TStreamCoder class, stream encoder/decoder;

  Modifyed:

   v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  // ---
  Bee_Headers,
  BeeLib_Interface,
  {$IFDEF cppDLL}
    Bee_LibLink;
  {$ELSE}
    {$IFDEF fpcDLL}
      Bee_LibLink;
    {$ELSE}
      BeeLib_StreamCoder;
    {$ENDIF}
  {$ENDIF}

type
  { THeaderCoder class }

  THeaderCoder = class
  private
    FCoder: pointer;
    FOwner: pointer;
    FStream: TStream;
    FPassword: string;
    FOnTickEvent: TTickEvent;
    FOnFillEvent: TFillEvent;
    FOnFlushEvent: TFlushEvent;
  public
    constructor Create(
      Stream: TStream;
      OnFill: TFillEvent;
      OnFlush: TFlushEvent;
      Ticker: pointer;
      OnTick: TTickEvent);
    destructor Destroy; override;
    procedure Initialize(Item: THeader);
    property Password: string read FPassword write FPassword;
    property OnTickEvent: TTickEvent read FOnTickEvent write FOnTickEvent;
    property OnFillEvent: TFillEvent read FOnFillEvent write FOnFillEvent;
    property OnFlushEvent: TFlushEvent read FOnFlushEvent write FOnFlushEvent;
  end;

  { THeaderEncoder class }

  THeaderEncoder = class(THeaderCoder)
  private
    function CopySilent(Strm: TStream; const Size: int64): int64;
    function Copy(Strm: TStream; const Size: int64): int64; overload;
    function Copy(Strm: TStream; const Size: int64; var CRC: longword): int64; overload;
    function Write(Item: THeader; Strm: TStream; const Size: int64): boolean;
  public
    constructor Create(
      Stream: TStream;
      OnFill: TFillEvent;
      OnFlush: TFlushEvent;
      Ticker: pointer;
      OnTick: TTickEvent);
    destructor Destroy; override;
    function WriteFromArch(Item: THeader; Strm: TStream): boolean;
    function WriteFromSwap(Item: THeader; Strm: TStream): boolean;
    function WriteFromFile(Item: THeader): boolean;
  end;

  { THeaderDecoder class }

  THeaderDecoder = class(THeaderCoder)
  private
    function Copy(Strm: TStream; const Size: int64; var CRC: longword): int64;
  public
    constructor Create(
      Stream: TStream;
      OnFill: TFillEvent;
      OnFlush: TFlushEvent;
      Ticker: pointer;
      OnTick: TTickEvent);
    destructor Destroy; override;
    function ReadToSwap(Item: THeader; Strm: TStream): boolean;
    function ReadToNul(Item: THeader): boolean;
    function ReadToFile(Item: THeader): boolean; overload;
  end;

implementation

uses
  Bee_Files,
  BeeLib_Crc;

{ THeaderCoder class }

  constructor THeaderCoder.Create(
    Stream: TStream;
    OnFill: TFillEvent;
    OnFlush: TFlushEvent;
    Ticker: pointer;
    OnTick: TTickEvent);
  begin
    FCoder        := nil;
    FPassword     := '';
    FStream       := Stream;
    FOnFillEvent  := OnFill;
    FOnFlushEvent := OnFlush;
    FOwner        := Ticker;
    FOnTickevent  := OnTick;
  end;

  destructor THeaderCoder.Destroy;
  begin
    FCoder        := nil;
    FStream       := nil;
    FOnFillEvent  := nil;
    FOnFlushEvent := nil;
    FOwner        := nil;
    FOnTickevent  := nil;
  end;

  procedure THeaderCoder.Initialize(Item: THeader);
  begin
    if foDictionary in Item.Flags then
      SetDictionaryLevel(FCoder, Item.Dictionary);

    if foTable in Item.Flags then
      SetTableParameters(FCoder, Item.Table);

    if foTear in Item.Flags then
      FreshFlexible(FCoder)
    else
      FreshSolid(FCoder);
  end;

  { THeaderEncoder class }

  constructor THeaderEncoder.Create(
    Stream: TStream;
    OnFill: TFillEvent;
    OnFlush: TFlushEvent;
    Ticker: pointer;
    OnTick: TTickEvent);
  begin
    inherited Create(Stream, OnFill, OnFlush, Ticker, OnTick);
    FCoder := CreateEncoder(Stream, OnFill, OnFlush, Ticker, OnTick);
  end;

  destructor THeaderEncoder.Destroy;
  begin
    DestroyCoder(FCoder);
    inherited Destroy;
  end;

  function THeaderEncoder.CopySilent(Strm: TStream; const Size: int64): int64;
  var
    Symbol: byte;
  begin
    Result := 0;
    while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
    begin
      FStream.Write(Symbol, 1);
      Inc(Result);
    end;
  end;

  function THeaderEncoder.Copy(Strm: TStream; const Size: int64): int64;
   var
     Symbol: byte;
   begin
     Result := 0;
     while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
     begin
       FStream.Write(Symbol, 1);
       Inc(Result);

       if (Result and DefaultTickStepSize = 0)
         and Assigned(FOnTickEvent)
         and FOnTickEvent(FOwner) then Break;
     end;
   end;

  function THeaderEncoder.Copy(Strm: TStream; const Size: int64; var CRC: longword): int64;
  var
    Symbol: byte;
  begin
    Result := 0;
    CRC    := longword(-1);
    while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
    begin
      FStream.Write(Symbol, 1);
      UpdCrc32(CRC, Symbol);
      Inc(Result);

      if (Result and DefaultTickStepSize = 0)
        and Assigned(FOnTickEvent)
        and FOnTickEvent(FOwner) then Break;
    end;
  end;

  function THeaderEncoder.Write(Item: THeader; Strm: TStream; const Size: int64): boolean;
  var
    StartPos: int64;
  begin
         StartPos :=  Strm.Seek(0, soCurrent);
    Item.StartPos := FStream.Seek(0, soCurrent);
    case foMoved in Item.Flags of
      True:  Item.Size := Copy (Strm, Size, Item.Crc);
      False: Item.Size := Encode(Fcoder, Strm, Size, Item.Crc);
    end;
    Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;

    // optimize compression ...
    if Item.PackedSize > Item.Size then
    begin
      Strm.Seek(StartPos, soBeginning);
      FStream.Size := Item.StartPos;

      Include(Item.Flags, foMoved);
      Include(Item.Flags, foTear);
      Initialize(Item);

      Item.PackedSize := CopySilent(Strm, Size);
    end;
    Result := Item.Size = Size;
  end;

  function THeaderEncoder.WriteFromSwap(Item: THeader; Strm: TStream): boolean;
  begin
    if foPassword in Item.Flags then
    begin
      if FStream is TFileWriter then TFileWriter(FStream).StartEncode(FPassword);
      if  Strm is TFileReader then TFileReader( Strm).StartDecode(FPassword);
    end;

    Strm.Seek(Item.StartPos, soBeginning);
    Result := Write(Item, Strm, Item.Size);

    if FStream is TFileWriter then TFileWriter(FStream).FinishEncode;
    if  Strm is TFileReader then TFileReader( Strm).FinishDecode;
  end;

  function THeaderEncoder.WriteFromArch(Item: THeader; Strm: TStream): boolean;
  var
   Symbol: byte;
   begin
     Strm.Seek(Item.StartPos, soBeginning);
     Item.StartPos := FStream.Seek(0, soCurrent);
     Result := Copy(Strm, Item.PackedSize) = Item.PackedSize;
   end;

  function THeaderEncoder.WriteFromFile(Item: THeader): boolean;
  var
    Strm: TFileReader;
  begin
    Result := False;
    Strm   := CreateTFileReader(Item.ExtName, fmOpenRead);
    if Assigned(Strm) then
    begin
      if foPassword in Item.Flags then
        if FStream is TFileWriter then
          TFileWriter(FStream).StartEncode(FPassword);

      Result := Write(Item, Strm, Strm.Size);
      if FStream is TFileWriter then
        TFileWriter(FStream).FinishEncode;

      Strm.Destroy;
    end;
  end;

  { TheaderDecoder class }

  constructor THeaderDecoder.Create(
    Stream: TStream;
    OnFill: TFillEvent;
    OnFlush: TFlushEvent;
    Ticker: pointer;
    OnTick: TTickEvent);
  begin
    inherited Create(Stream, OnFill, OnFlush, Ticker, OnTick);
    FCoder := CreateDecoder(Stream, OnFill, OnFlush, Ticker, OnTick);
  end;

  destructor THeaderDecoder.Destroy;
  begin
    DestroyCoder(FCoder);
    inherited Destroy;
  end;

  function THeaderDecoder.Copy(Strm: TStream; const Size: int64; var CRC: longword): int64;
  var
    Symbol: byte;
  begin
    Result := 0;
    CRC    := longword(-1);
    while (Result < Size) and (FStream.Read(Symbol, 1) = 1) do
    begin
      Strm.Write(Symbol, 1);
      UpdCrc32(CRC, Symbol);
      Inc(Result);

      if (Result and DefaultTickStepSize = 0)
        and Assigned(FOnTickEvent)
        and FOnTickEvent(FOwner) then Break;
    end;
  end;

  function THeaderDecoder.ReadToSwap(Item: THeader; Strm: TStream): boolean;
  var
    CRC: longword;
  begin
    if foPassword in Item.Flags then
    begin
      if FStream is TFileReader then TFileReader(FStream).StartDecode(FPassword);
      if  Strm is TFileWriter then TFileWriter( Strm).StartEncode(FPassword);
    end;

    FStream.Seek(Item.StartPos, soBeginning);
    Item.StartPos := Strm.Seek(0, soCurrent);
    case foMoved in Item.Flags of
      True:  Result := Copy          (Strm, Item.Size, CRC) = Item.Size;
      False: Result := Decode(FCoder, Strm, Item.Size, CRC) = Item.Size;
    end;
    if Result then Result := Item.Crc = CRC;

    if FStream is TFileReader then TFileReader(FStream).FinishDecode;
    if  Strm is TFileWriter then TFileWriter( Strm).FinishEncode;
  end;

  function THeaderDecoder.ReadToNul(Item: THeader): boolean;
  var
    CRC: longword;
    Strm: TNulWriter;
  begin
    if foPassword in Item.Flags then
      if FStream is TFileReader then
        TFileReader(FStream).StartDecode(FPassword);

    Strm := TNulWriter.Create;
    FStream.Seek(Item.StartPos, soBeginning);
    case foMoved in Item.Flags of
      True:  Result := Copy          (Strm, Item.Size, CRC) = Item.Size;
      False: Result := Decode(FCoder, Strm, Item.Size, CRC) = Item.Size;
    end;
    Strm.Free;

    if FStream is TFileReader then
      TFileReader(FStream).FinishDecode;

    Result := Result and (Item.Crc = CRC);
  end;

  function THeaderDecoder.ReadToFile(Item: THeader): boolean;
  var
    CRC: longword;
    Strm: TFileWriter;
  begin
    Result := False;
    Strm   := CreateTFileWriter(Item.ExtName, fmCreate);
    if Assigned(Strm) then
    begin
      if foPassword in Item.Flags then
         if FStream is TFileReader then
           TFileReader(FStream).StartDecode(FPassword);

      FStream.Seek(Item.StartPos, soBeginning);
      case foMoved in Item.Flags of
        True:  Result := Copy(Strm, Item.Size, CRC) = Item.Size;
        False: Result := Decode(FCoder, Strm, Item.Size, CRC) = Item.Size;
      end;
      Strm.Free;

      if FStream is TFileReader then
        TFileReader(FStream).FinishDecode;

      Result := Result and (Item.Crc = CRC);

      if Result then
      begin
        FileSetAttr(Item.ExtName, Item.Attr);
        FileSetDate(Item.ExtName, Item.Time);
      end;
    end;
  end;

end.
