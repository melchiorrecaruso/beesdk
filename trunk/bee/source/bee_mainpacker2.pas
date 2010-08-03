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

}

unit Bee_MainPacker2;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Codec,
  Bee_Modeller,
  Bee_Configuration,
  Bee_Headers;

type

  { TStreamCoder class }

  TStreamCoder = class
  private
    FStream: TStream;
    FPPM: TBaseCoder;
    FSecondaryCodec: TSecondaryCodec;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Copy(SrcStrm: TStream; Size: int64): longword;
    function Encode(SrcStrm: TStream; Size: int64): longword;
    function Decode(DstStrm: TStream; Size: int64): longword;
    procedure SetTable(const Value: TTableParameters);
    procedure SetDictionary(Value: byte);
    procedure FreshFlexible;
    procedure FreshSolid;
  end;

implementation

uses
  Bee_Crc;

{ TStreamCoder class }

constructor TStreamCoder.Create(Stream: TStream);
begin
  FStream := Stream;
  FSecondaryCodec := TSecondaryEncoder.Create(FStream);
  FPPM := TBaseCoder.Create(FSecondaryCodec);
end;

destructor TStreamCoder.Destroy;
begin
  FPPM.Free;
  FSecondaryCodec.Free;
  FStream := nil;
end;

function TStreamCoder.Copy(SrcStrm: TStream; Size: int64): longword;
var
  Symbol: byte;
begin
  Result := longword(-1);
  while Size > 0 do
  begin
    SrcStrm.Read(Symbol, 1);
    FStream.Write(Symbol, 1);
    UpdCrc32(Result, Symbol);
    Dec(Size);
  end;
end;

function TStreamCoder.Encode(SrcStrm: TStream; Size: int64): longword;
var
  Symbol: byte;
begin
  Result := longword(-1);
  FSecondaryCodec.Start;
  while Size > 0 do
  begin
    SrcStrm.Read(Symbol, 1);
    FPPM.UpdateModel(Symbol);
    UpdCrc32(Result, Symbol);
    Dec(Size);
  end;
  FSecondaryCodec.Flush;
end;

function TStreamCoder.Decode(DstStrm: TStream; Size: int64): longword;
var
  Symbol: byte;
begin
  Result := longword(-1);
  FSecondaryCodec.Start;
  while Size > 0 do
  begin
    Symbol := FPPM.UpdateModel(0);
    DstStrm.Write(Symbol, 1);
    UpdCrc32(Result, Symbol);
    Dec(Size);
  end;
  FSecondaryCodec.Flush;
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

end.
