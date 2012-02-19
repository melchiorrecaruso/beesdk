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
  BeeLib_Configuration,
  {$IFDEF cppDLL}
    Bee_LibLink;
  {$ELSE}
    {$IFDEF fpcDLL}
      Bee_LibLink;
    {$ELSE}
      BeeLib_StreamCoder;
    {$ENDIF}
  {$ENDIF}

// const
//  DEFAULT_BUFFER_SIZE = 64 * 1024;

type
  TTickEvent = function(Value: longint): boolean of object;

type
  { THeaderCoder class }

  THeaderCoder = class
  private
    FTick: TTickEvent;
    FPassword: string;
    FStream: TStream;
    FCoder: pointer;
    FModeller: pointer;
  public
    constructor Create(Stream: TStream; Tick: TTickEvent);
    destructor Destroy; override;
    procedure Initialize(Item: THeader);
    property Password: string read FPassword write FPassword;
  end;

  { THeaderEncoder class }

  THeaderEncoder = class(THeaderCoder)
  private
    function Copy                (Stream: TStream; const Size: int64;  Silent:  boolean): int64; overload;
    function Copy                (Stream: TStream; const Size: int64; var CRC: longword): int64; overload;
    function Encode              (Stream: TStream; const Size: int64; var CRC: longword): int64;
    function Write(Item: THeader; Stream: TStream; const Size: int64): boolean;
  public
    constructor Create(Stream: TStream; Tick: TTickEvent);
    destructor Destroy; override;

    function WriteFromArch(Item: THeader; Stream: TStream): boolean;
    function WriteFromSwap(Item: THeader; Stream: TStream): boolean;
    function WriteFromFile(Item: THeader): boolean;
  end;

  { THeaderDecoder class }

  THeaderDecoder = class(THeaderCoder)
  private
    function Copy  (Stream: TStream; const Size: int64; var CRC: longword): int64;
    function Decode(Stream: TStream; const Size: int64; var CRC: longword): int64;
  public
    constructor Create(Stream: TStream; Tick: TTickEvent);
    destructor Destroy; override;

    function ReadToSwap(Item: THeader; Stream: TStream): boolean;
    function ReadToNul (Item: THeader): boolean;
    function ReadToFile(Item: THeader): boolean; overload;
  end;

implementation

uses
  Bee_Files,
  BeeLib_Crc;

  { THeaderCoder class }

  constructor THeaderCoder.Create(Stream: TStream; Tick: TTickEvent);
  begin
    inherited Create;
    FTick     := Tick;
    FPassword := '';
    FStream   := Stream;
  end;

  destructor THeaderCoder.Destroy;
  begin
    inherited Destroy;
  end;

  procedure THeaderCoder.Initialize(Item: THeader);
  begin
    if foDictionary in Item.Flags then
      BaseCoder_SetDictionary(FModeller, Item.Dictionary);

    if foTable in Item.Flags then
      BaseCoder_SetTable(FModeller, @Item.Table);

    if foTear in Item.Flags then
      BaseCoder_FreshFlexible(FModeller)
    else
      BaseCoder_FreshSolid(FModeller);
  end;

  { THeaderEncoder class }

  constructor THeaderEncoder.Create(Stream: TStream; Tick: TTickEvent);
  begin
    inherited Create(Stream, Tick);
    FCoder    := RangeEncoder_Create(FStream, @DoFlush);
    FModeller := BaseCoder_Create(FCoder);
  end;

  destructor THeaderEncoder.Destroy;
  begin
    BaseCoder_Destroy(FModeller);
    RangeEncoder_Destroy(FCoder);
    inherited Destroy;
  end;

  function THeaderEncoder.Copy(Stream: TStream; const Size: int64; Silent: boolean): int64;
  var
    Count:  int64;
    Readed: longint;
    Writed: longint;
    Buffer: array[0..$FFFF] of byte;
  begin
    Result := 0;
    Count  := Size div SizeOf(Buffer);
    while Count <> 0 do
    begin
      Readed :=  Stream.Read (Buffer, SizeOf(Buffer));
      Writed := FStream.Write(Buffer, Readed);
      Inc(Result, Writed);
      Dec(Count);

      if (not Silent) and FTick(Writed) then Exit;
    end;
    Readed :=  Stream.Read (Buffer, Size mod SizeOf(Buffer));
    Writed := FStream.Write(Buffer, Readed);
    Inc(Result, Writed);

    if (not Silent) then FTick(Writed);
  end;

  function THeaderEncoder.Copy(Stream: TStream; const Size: int64; var CRC: longword): int64;
  var
    Count:  int64;
    Readed: longint;
    Writed: longint;
    Buffer: array[0..$FFFF] of byte;
  begin
    Result := 0;
    CRC    := longword(-1);
    Count  := Size div SizeOf(Buffer);
    while Count <> 0 do
    begin
      Readed :=  Stream.Read (Buffer, SizeOf(Buffer));
      Writed := FStream.Write(Buffer, Readed);
      UpdateCrc32(CRC, Buffer, Writed);
      Inc(Result, Writed);
      Dec(Count);

      if FTick(Writed) then Exit;
    end;
    Readed :=  Stream.Read (Buffer, Size mod SizeOf(Buffer));
    Writed := FStream.Write(Buffer, Readed);
    UpdateCRC32(CRC, Buffer, Writed);
    Inc(Result, Writed);

    FTick(Writed);
  end;

  function THeaderEncoder.Encode(Stream: TStream; const Size: int64; var CRC: longword): int64;
  var
    Count:  int64;
    Readed: longint;
    Buffer: array[0..$FFFF] of byte;
  begin

    Writeln('ENCODE: RangeEncoder_StartEncode');
    RangeEncoder_StartEncode(FCoder);


    Result := 0;
    CRC    := longword(-1);
    Count  := Size div SizeOf(Buffer);
    while Count <> 0 do
    begin
      Readed := Stream.Read(Buffer, SizeOf(Buffer));

      Writeln('ENCODE: BaseCoder_Encode');
      BaseCoder_Encode(FModeller, @Buffer, Readed);

      Writeln('ENCODE: UpdateCrc32');
      UpdateCrc32(CRC, Buffer, Readed);
      Inc(Result, Readed);
      Dec(Count);

      if FTick(Readed) then Exit;
    end;
    Readed := Stream.Read(Buffer, Size mod SizeOf(Buffer));
    BaseCoder_Encode(FModeller, @Buffer, Readed);
    UpdateCRC32(CRC, Buffer, Readed);
    Inc(Result, Readed);
    FTick(Readed);

    RangeEncoder_FinishEncode(FCoder);
  end;

  function THeaderEncoder.Write(Item: THeader; Stream: TStream; const Size: int64): boolean;
  var
    StartPos: int64;
    CRC: longword;
  begin
         StartPos :=  Stream.Seek(0, soCurrent); // flush buffer
    Item.StartPos := FStream.Seek(0, soCurrent); // flush buffer
    case foMoved in Item.Flags of
      True:  Item.Size := Copy  (Stream, Size, CRC);
      False: Item.Size := Encode(Stream, Size, CRC);
    end;
    Item.PackedSize := FStream.Seek(0, soCurrent) - Item.StartPos;
    Item.Crc        := CRC;

    // optimize compression ...
    if Item.PackedSize > Item.Size then
    begin
       Stream.Seek(StartPos, soBeginning);
      FStream.Size := Item.StartPos;

      Include(Item.Flags, foMoved);
      Include(Item.Flags, foTear);
      Initialize(Item);

      Item.PackedSize := Copy(Stream, Size, TRUE);
    end;
    Result := Item.Size = Size;
  end;

  function THeaderEncoder.WriteFromSwap(Item: THeader; Stream: TStream): boolean;
  begin
    if foPassword in Item.Flags then
    begin
      if FStream is TFileWriter then TFileWriter(FStream).StartEncode(FPassword);
      if  Stream is TFileReader then TFileReader( Stream).StartDecode(FPassword);
    end;

    Stream.Seek(Item.StartPos, soBeginning);
    Result := Write(Item, Stream, Item.Size);

    if FStream is TFileWriter then TFileWriter(FStream).FinishEncode;
    if  Stream is TFileReader then TFileReader( Stream).FinishDecode;
  end;

  function THeaderEncoder.WriteFromArch(Item: THeader; Stream: TStream): boolean;
  var
   Symbol: byte;
   begin
     Stream.Seek(Item.StartPos, soBeginning);
     Item.StartPos := FStream.Seek(0, soCurrent);
     Result := Copy(Stream, Item.PackedSize, FALSE) = Item.PackedSize;
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

  constructor THeaderDecoder.Create(Stream: TStream; Tick: TTickEvent);
  begin
    inherited Create(Stream, Tick);
    FCoder    := RangeDecoder_Create(FStream, @DoFill);
    FModeller := BaseCoder_Create(FCoder);
  end;

  destructor THeaderDecoder.Destroy;
  begin
    BaseCoder_Destroy(FModeller);
    RangeDecoder_Destroy(FCoder);
    inherited Destroy;
  end;

  function THeaderDecoder.Copy(Stream: TStream; const Size: int64; var CRC: longword): int64;
  var
    Count:  int64;
    Readed: longint;
    Writed: longint;
    Buffer: array[0..$1FFFE] of byte;
  begin
    Result := 0;
    CRC    := longword(-1);
    Count  := Size div SizeOf(Buffer);
    while Count <> 0 do
    begin
      Readed := FStream.Read(Buffer, SizeOf(Buffer));
      Writed := Stream.Write(Buffer, Readed);
      UpdateCrc32(CRC, Buffer, Writed);
      Inc(Result, Writed);
      Dec(Count);

      if FTick(Writed) then Break;
    end;
    Readed := FStream.Read(Buffer, Size mod SizeOf(Buffer));
    Writed := Stream.Write(Buffer, Readed);
    UpdateCRC32(CRC, Buffer, Writed);
    Inc(Result, Writed);

    FTick(Writed);
  end;

  function THeaderDecoder.Decode(Stream: TStream; const Size: int64; var CRC: longword): int64;
  begin

  end;

  function THeaderDecoder.ReadToSwap(Item: THeader; Stream: TStream): boolean;
  var
    CRC: longword;
  begin
    if foPassword in Item.Flags then
    begin
      if FStream is TFileReader then TFileReader(FStream).StartDecode(FPassword);
      if  Stream is TFileWriter then TFileWriter( Stream).StartEncode(FPassword);
    end;

    FStream.Seek(Item.StartPos, soBeginning);
    Item.StartPos := Stream.Seek(0, soCurrent);
    case foMoved in Item.Flags of
      True:  Result := Copy  (Stream, Item.Size, CRC) = Item.Size;
      False: Result := Decode(Stream, Item.Size, CRC) = Item.Size;
    end;
    if Result then Result := Item.Crc = CRC;

    if FStream is TFileReader then TFileReader(FStream).FinishDecode;
    if  Stream is TFileWriter then TFileWriter( Stream).FinishEncode;
  end;

  function THeaderDecoder.ReadToNul(Item: THeader): boolean;
  var
    CRC: longword;
    Strm: TNulWriter;
  begin
    if foPassword in Item.Flags then
    begin
      if FStream is TFileReader then TFileReader(FStream).StartDecode(FPassword);
    end;

    Strm := TNulWriter.Create;
    FStream.Seek(Item.StartPos, soBeginning);
    case foMoved in Item.Flags of
      True:  Result := Copy  (Strm, Item.Size, CRC) = Item.Size;
      False: Result := Decode(Strm, Item.Size, CRC) = Item.Size;
    end;
    Result := Result and (Item.Crc = CRC);
    Strm.Free;

    if FStream is TFileReader then TFileReader(FStream).FinishDecode;
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
        True:  Result := Copy  (Strm, Item.Size, CRC) = Item.Size;
        False: Result := Decode(strm, Item.Size, CRC) = Item.Size;
      end;
      Strm.Free;

      if FStream is TFileReader then TFileReader(FStream).FinishDecode;

      Result := Result and (Item.Crc = CRC);
      if Result then
      begin
        FileSetAttr(Item.ExtName, Item.Attr);
        FileSetDate(Item.ExtName, Item.Time);
      end;
    end;
  end;

end.
