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
  Bee_Codec,
  Bee_Modeller,
  Bee_Configuration;

type
  TUserAbortEvent = function: boolean of object;

  { TStreamCoder class }

  TStreamCoder = class
  protected
    FStream: TStream;
  private
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
    FOnUserAbortEvent: TUserAbortEvent;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure SetTable(const Value: TTableParameters);
    procedure SetDictionary(Value: byte);
    procedure FreshFlexible;
    procedure FreshSolid;
    function Copy(Strm: TStream; const Size: int64): int64; overload; virtual; abstract;
    function Copy(Strm: TStream; const Size: int64; var CRC: longword): int64; overload; virtual; abstract;
    function Write(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual; abstract;
    function Read(Strm: TStream; const Size: int64; var CRC: longword): int64; virtual; abstract;
    property OnUserAbortEvent: TUserAbortEvent read FOnUserAbortEvent write FOnUserAbortEvent;
  end;

  { TStreamEncoder class }

  TStreamEncoder = class(TStreamCoder)
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Copy(Strm: TStream; const Size: int64): int64; override;
    function Copy(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
    function Write(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
  end;

  { TStreamDecoder class }

  TStreamDecoder = class(TStreamCoder)
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Copy(Strm: TStream; const Size: int64): int64; override;
    function Copy(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
    function Read(Strm: TStream; const Size: int64; var CRC: longword): int64; override;
  end;

  { TFileStreamEncoder class }

  TFileStreamEncoder = class(TStreamEncoder)
  public
    function Copy(const FileName: string; var CRC: longword): int64; overload;
    function Write(const FileName: string; var CRC: longword): int64; overload;
  end;

  { TFileStreamDecoder class }

  TFileStreamDecoder = class(TStreamDecoder)
  public
    function Copy(const FileName: string; const Size: int64; var CRC: longword): int64; overload;
    function Write(const FileName: string; const Size: int64; var CRC: longword): int64; overload;
  end;

implementation

uses
  Bee_Crc,
  Bee_Files;

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

function TStreamEncoder.Copy(Strm: TStream; const Size: int64): int64;
var
  Symbol: byte;
begin
  Result := 0;
  while (Result < Size) and (Strm.Read(Symbol, 1) = 1) do
  begin
    FStream.Write(Symbol, 1);
    Inc(Result);
    if (Result and $FFFF = 0)
      and Assigned(FOnUserAbortEvent)
        and FOnUserAbortEvent then Break;
  end;
end;

function TStreamEncoder.Copy(Strm: TStream; const Size: int64; var CRC: longword): int64;
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
    if (Result and $FFFF = 0)
      and Assigned(FOnUserAbortEvent)
        and FOnUserAbortEvent then Break;
  end;
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

function TStreamDecoder.Copy(Strm: TStream; const Size: int64): int64;
var
  Symbol: byte;
begin
  Result := 0;
  while (Result < Size) and (FStream.Read(Symbol, 1) = 1) do
  begin
    Strm.Write(Symbol, 1);
    Inc(Result);
    if (Result and $FFFF = 0)
      and Assigned(FOnUserAbortEvent)
        and FOnUserAbortEvent then Break;
  end;
end;

function TStreamDecoder.Copy(Strm: TStream; const Size: int64; var CRC: longword): int64;
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
    if (Result and $FFFF = 0)
      and Assigned(FOnUserAbortEvent)
        and FOnUserAbortEvent then Break;
  end;
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

{ TFileStreamEncoder class }

function TFileStreamEncoder.Copy(const FileName: string; var CRC: longword): int64;
var
  Strm: TFileReader;
begin
  Strm := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := Copy(Strm, Strm.Size, CRC);
    Strm.Free;
  end else
    Result := 0;
end;

function TFileStreamEncoder.Write(const FileName: string; var CRC: longword): int64;
var
  Strm: TFileReader;
begin
  Strm := CreateTFileReader(FileName, fmOpenRead);
  if Assigned(Strm) then
  begin
    Result := Write(Strm, Strm.Size, CRC);
    Strm.Free;
  end else
    Result := 0;
end;

{ TFileStreamDecoder class }

function TFileStreamDecoder.Copy(const FileName: string; const Size: int64; var CRC: longword): int64;
var
  Strm: TFileWriter;
begin
  Strm := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := Copy(Strm, Size, CRC);
    Strm.Free;
  end else
    Result := 0;
end;

function TFileStreamDecoder.Write(const FileName: string; const Size: int64; var CRC: longword): int64;
var
  Strm: TFileWriter;
begin
  Strm := CreateTFileWriter(FileName, fmCreate);
  if Assigned(Strm) then
  begin
    Result := Write(Strm, Size, CRC);
    Strm.Free;
  end else
    Result := 0;
end;

end.

