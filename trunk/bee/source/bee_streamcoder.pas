{
  Copyright (c) 1999-2011 Andrew Filinsky and Melchiorre Caruso

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

    Stream coder/decoder classes

  Modifyed:

    v0.8.0 build 1280 - 2011.02.15 by Melchiorre Caruso.
}

unit Bee_StreamCoder;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  Bee_Codec,
  Bee_Modeller,
  Bee_Configuration;

const
  DefaultUserAbortEventStepSize: longint  = $FFFF;
  DefaultDictionaryLevel:        longword = $0002;
  DefaultTableParameters: TTableParameters =
    (  3, 163, 157,  65,  93, 117, 135, 109, 126, 252, 172, 252, 152, 227, 249,
     249, 253, 196,  27,  82,  93,  74, 182, 245,  40,  67,  77, 143, 133, 135,
     128, 155, 207, 177, 225, 251, 253, 248,  73,  35,  15, 107, 143);

type
  { TUserAbortEvent event }

  TUserAbortEvent = function: boolean of object;

  { TStreamCoder abstract class }

  TStreamCoder = class
  private
    FDictionaryLevel: longword;
    FTableParameters: TTableParameters;
    procedure SetDictionaryLevel(Value: longword);
    procedure SetTableParameters(const Value: TTableParameters);
  protected
    FStrm: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
    FOnUserAbortEvent: TUserAbortEvent;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure FreshSolid;
    procedure FreshFlexible;
    function Read(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual; abstract;
    function Write(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual; abstract;
    function Write2(Strm: TStream; const Size: int64): int64; virtual; abstract;
    property DictionaryLevel: longword read FDictionaryLevel write SetDictionaryLevel;
    property TableParameters: TTableParameters read FTableParameters write SetTableParameters;
    property OnUserAbortEvent: TUserAbortEvent read FOnUserAbortEvent write FOnUserAbortEvent;
  end;

  { TStreamEncoder class }

  TStreamEncoder = class(TStreamCoder)
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Read (Strm: TStream; const Size: int64; var CRC: longword): int64; override;
    function Write(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
    function Write2(Strm: TStream; const Size: int64): int64; override;
  end;

  { TStreamDecoder class }

  TStreamDecoder = class(TStreamCoder)
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Read (Strm: TStream; const Size: int64; var CRC: longword): int64; override;
    function Write(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
  end;

  EStreamEncoderError = class(Exception);
  EStreamDecoderError = class(Exception);

implementation

uses
  Bee_Crc;

{ TStreamCoder class }

constructor TStreamCoder.Create(Stream: TStream);
begin
  inherited Create;
  FStrm := Stream;
  FOnUserAbortEvent := nil;
end;

destructor TStreamCoder.Destroy;
begin
  FStrm := nil;
  FOnUserAbortEvent := nil;
  inherited Destroy;
end;

procedure TStreamCoder.SetDictionaryLevel(Value: longword);
begin
  FDictionaryLevel := FPPM.SetDictionary(Value);
end;

procedure TStreamCoder.SetTableParameters(const Value: TTableParameters);
var
  I: longint;
begin
  // -- DEBUG --
  // for I := Low(Value) to High(Value) do
  //   System.Writeln(Value[I],', ');
  // -- END --
  FTableParameters := Value;
  FPPM.SetTable(Value);
end;

procedure TStreamCoder.FreshFlexible;
begin
  FPPM.FreshFlexible;
end;

procedure TStreamCoder.FreshSolid;
begin
  FPPM.FreshSolid;
end;

{ TStreamEncoder class }

constructor TStreamEncoder.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FSecondaryCodec := TSecondaryEncoder.Create(FStrm);
  FPPM := TBaseCoder.Create(FSecondaryCodec);
  FPPM.SetDictionary(DefaultDictionaryLevel);
  // SetDefaultTableParameters
end;

destructor TStreamEncoder.Destroy;
begin
  FPPM.Free;
  FSecondaryCodec.Free;
  inherited Destroy;
end;

function TStreamEncoder.Write(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC    := longword(-1);
  FSecondaryCodec.Start;
  while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
  begin
    FPPM.UpdateModel(Symbol);
    UpdCrc32(CRC, Symbol);
    Inc(Result);
    if (Result and DefaultUserAbortEventStepSize = 0)
      and Assigned(FOnUserAbortEvent)
        and FOnUserAbortEvent then Break;
  end;
  FSecondaryCodec.Flush;
end;

function TStreamEncoder.Write2(Strm: TStream; const Size: int64): int64;
var
  Symbol: byte;
begin
  Result := 0;
  FSecondaryCodec.Start;
  while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
  begin
    FPPM.UpdateModel(Symbol);
    Inc(Result);
    if (Result and DefaultUserAbortEventStepSize = 0)
      and Assigned(FOnUserAbortEvent)
        and FOnUserAbortEvent then Break;
  end;
  FSecondaryCodec.Flush;
end;

function TStreamEncoder.Read(Strm: TStream; const Size: int64; var CRC: longword): int64;
begin
  raise EStreamEncoderError.Create('Invalid stream operation.');
end;

{ TStreamDecoder class }

constructor TStreamDecoder.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FSecondaryCodec := TSecondaryDecoder.Create(FStrm);
  FPPM := TBaseCoder.Create(FSecondaryCodec);
  FPPM.SetDictionary(DefaultDictionaryLevel);
  // SetDefaultTableParameters
end;

destructor TStreamDecoder.Destroy;
begin
  FPPM.Free;
  FSecondaryCodec.Free;
  inherited Destroy;
end;

function TStreamDecoder.Read(Strm: TStream; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
begin
  Result := 0;
  CRC    := longword(-1);
  FSecondaryCodec.Start;
  while (Result < Size) do
  begin
    Symbol := FPPM.UpdateModel(0);
    Strm.Write(Symbol, 1);
    UpdCrc32(CRC, Symbol);
    Inc(Result);
    if (Result and DefaultUserAbortEventStepSize = 0)
      and Assigned(FOnUserAbortEvent)
        and FOnUserAbortEvent then Break;
  end;
  FSecondaryCodec.Flush;
end;

function TStreamDecoder.Write(Strm: TStream; const Size: int64; var CRC: longword): int64;
begin
  raise EStreamDecoderError.Create('Invalid stream operation.');
end;

end.

