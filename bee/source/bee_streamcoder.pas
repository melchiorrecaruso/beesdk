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

    Stream coder/decoder abstract classes

  Modifyed:

    v0.8.0 build 1157 - 2011.02.04 by Melchiorre Caruso.
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

type
  { TUserAbortEvent event }

  TUserAbortEvent = function: boolean of object;

  { TStreamCoder class }

  TStreamCoder = class
  protected
    FStream: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
    FOnUserAbortEvent: TUserAbortEvent;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure FreshSolid;
    procedure FreshFlexible;
    procedure SetDictionary(Value: byte);
    procedure SetTable(const Value: TTableParameters);
    function Read(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual; abstract;
    function Write(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual; abstract;
    property OnUserAbortEvent: TUserAbortEvent read FOnUserAbortEvent write FOnUserAbortEvent;
  end;

  { TStreamEncoder class }

  TStreamEncoder = class(TStreamCoder)
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Read(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
    function Write(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
  end;

  { TStreamDecoder class }

  TStreamDecoder = class(TStreamCoder)
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Read(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
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
  FStream := Stream;
  FOnUserAbortEvent := nil;
end;

destructor TStreamCoder.Destroy;
begin
  FStream := nil;
  FOnUserAbortEvent := nil;
  inherited Destroy;
end;

procedure TStreamCoder.SetDictionary(Value: byte);
begin
  FPPM.SetDictionary(Value);
end;

procedure TStreamCoder.SetTable(const Value: TTableParameters);
begin
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
  FSecondaryCodec := TSecondaryEncoder.Create(FStream);
  FPPM := TBaseCoder.Create(FSecondaryCodec);
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
    if (Result and $FFFF = 0)
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
  FSecondaryCodec := TSecondaryDecoder.Create(FStream);
  FPPM := TBaseCoder.Create(FSecondaryCodec);
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
    if (Result and $FFFF = 0)
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

