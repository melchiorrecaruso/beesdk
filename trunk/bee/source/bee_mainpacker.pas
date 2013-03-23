{
  Copyright (c) 2013 Melchiorre Caruso.

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

   v0.8.0 build 1864 - 2013.02.16 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  // ---
  Bee_BufStream,
  Bee_Files,
  Bee_Interface,
  Bee_Configuration,
  {$IFDEF cLib}
    Bee_LibLink;
  {$ELSE}
    Bee_Modeller;
  {$ENDIF}

type
  TCoderProgressEvent = procedure(Value: longint) of object;

type
  { TCoders classes }

  TBaseCoder = class
  private
    FStream: TBufStream;
    FOnProgressEvent: TCoderProgressEvent;
    procedure DoProgress(Value: longint);
  public
    constructor Create(Stream: TBufStream);
    procedure SetCompressionLevel(Value: longint); virtual abstract;
    procedure SetCompressionLevelAux(Value: longint); virtual abstract;
    procedure SetCompressionFilter(const Filter: string); virtual abstract;
    procedure SetCompressionFilterAux(const Filter: string); virtual abstract;
    function Encode(Stream: TFileReader; const Size: int64): int64; virtual abstract;
    function Decode(Stream: TFileWriter; const Size: int64): int64; virtual abstract;
    function Copy  (Stream: TFileReader; const Size: int64): int64; virtual overload;
    function Copy  (Stream: TFileWriter; const Size: int64): int64; virtual overload;
  public
    property OnProgress: TCoderProgressEvent read FOnProgressEvent write FOnProgressEvent;
  end;

  TBeeCoder = class(TBaseCoder)
  private
    FCoder: pointer;
    FModeller: pointer;
  public
    procedure SetCompressionLevelAux(Value: longint); override;
    procedure SetCompressionFilter(const Filter: string); override;
    procedure FreshFlexible;
    procedure FreshSolid;
  end;

  TBeeEncoder = class(TBeeCoder)
  public
    constructor Create(Stream: TBufStream);
    destructor Destroy; override;
    function Encode(Stream: TFileReader; const Size: int64): int64; override;
  end;

  TBeeDecoder = class(TBeeCoder)
  public
    constructor Create(Stream: TBufStream);
    destructor Destroy; override;
    function Decode(Stream: TFileWriter; const Size: int64): int64; override;
  end;

implementation

uses
  Bee_Common;

/// TBaseCoder class

constructor TBaseCoder.Create(Stream: TBufStream);
begin
  inherited Create;
  FStream := Stream;
end;

procedure TBaseCoder.DoProgress(Value: longint); inline;
begin
  if Assigned(FOnProgressEvent) then
    FOnProgressEvent(Value);
end;

function TBaseCoder.Copy(Stream: TFileReader; const Size: int64): int64;
var
  Count:  int64;
  Buffer: TBuffer;
  Readed: longint;
begin
  Result := 0;
  if Size > 0 then
  begin
    Count := Size div SizeOf(TBuffer);
    while (Count <> 0) and (ExitStatus = esNoError) do
    begin
      Readed :=  Stream.Read (@Buffer[0], SizeOf(TBuffer));
      Readed := FStream.Write(@Buffer[0], Readed);
      Inc(Result, Readed);
      DoProgress(Readed);
      Dec(Count);
    end;
    Readed :=  Stream.Read (@Buffer[0], Size mod SizeOf(TBuffer));
    Readed := FStream.Write(@Buffer[0], Readed);
    Inc(Result, Readed);
    DoProgress(Readed);
  end;
end;

function TBaseCoder.Copy(Stream: TFileWriter; const Size: int64): int64;
var
  Count:  int64;
  Buffer: TBuffer;
  Readed: longint;
begin
  Result := 0;
  if Size > 0 then
  begin
    Count := Size div SizeOf(TBuffer);
    while (Count <> 0) and (ExitStatus = esNoError) do
    begin
      Readed := FStream.Read (@Buffer[0], SizeOf(TBuffer));
      Readed :=  Stream.Write(@Buffer[0], Readed);
      Inc(Result, Readed);
      DoProgress(Readed);
      Dec(Count);
    end;
    Readed := FStream.Read (@Buffer[0], Size mod SizeOf(TBuffer));
    Readed :=  Stream.Write(@Buffer[0], Readed);
    Inc(Result, Readed);
    DoProgress(Readed);
  end;
end;

/// TBeeCoder class

procedure TBeeCoder.SetCompressionLevelAux(Value: longint);
begin
  BaseCoder_SetDictionary(FModeller, Value);
end;

procedure TBeeCoder.SetCompressionFilter(const Filter: string);
begin
  BaseCoder_SetTable(FModeller, @Filter[1]);
end;

procedure TBeeCoder.FreshFlexible;
begin
  BaseCoder_FreshFlexible(FModeller)
end;

procedure TBeeCoder.FreshSolid;
begin
  BaseCoder_FreshSolid(FModeller);
end;

/// TBeeEncoder class

constructor TBeeEncoder.Create(Stream: TBufStream);
begin
  inherited Create(Stream);
  FCoder    := RangeEncoder_Create(FStream, @DoFlush);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor TBeeEncoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeEncoder_Destroy(FCoder);
  inherited Destroy;
end;

function TBeeEncoder.Encode(Stream: TFileReader; const Size: int64): int64;
var
  Count:  int64;
  Buffer: TBuffer;
  Readed: longint;
begin
  Result := 0;
  if Size > 0 then
  begin
    RangeEncoder_StartEncode(FCoder);
    Count := Size div SizeOf(TBuffer);
    while (Count <> 0) and (ExitStatus = esNoError) do
    begin
      Readed := Stream.Read      (@Buffer[0], SizeOf(TBuffer));
      BaseCoder_Encode(FModeller, @Buffer[0], Readed);
      Inc(Result, Readed);
      DoProgress(Readed);
      Dec(Count);
    end;
    Readed := Stream.Read      (@Buffer[0], Size mod SizeOf(TBuffer));
    BaseCoder_Encode(FModeller, @Buffer[0], Readed);
    Inc(Result, Readed);
    DoProgress(Readed);
    RangeEncoder_FinishEncode(FCoder);
  end;
end;

/// TBeeDecoder class

constructor TBeeDecoder.Create(Stream: TBufStream);
begin
  inherited Create(Stream);
  FCoder    := RangeDecoder_Create(FStream, @DoFill);
  FModeller := BaseCoder_Create(FCoder);
end;

destructor TBeeDecoder.Destroy;
begin
  BaseCoder_Destroy(FModeller);
  RangeDecoder_Destroy(FCoder);
  inherited Destroy;
end;

function TBeeDecoder.Decode(Stream: TFileWriter; const Size: int64): int64;
var
  Count:  int64;
  Buffer: TBuffer;
  Writed: longint;
begin
  Result := 0;
  if Size > 0 then
  begin
    RangeDecoder_StartDecode(FCoder);
    Count := Size div SizeOf(TBuffer);
    while (Count <> 0) and (ExitStatus = esNoError) do
    begin
      BaseCoder_Decode(FModeller, @Buffer[0], SizeOf(TBuffer));
      Writed := Stream.Write(     @Buffer[0], SizeOf(TBuffer));
      Inc(Result, Writed);
      DoProgress(Writed);
      Dec(Count);
    end;
    BaseCoder_Decode(FModeller, @Buffer[0], Size mod SizeOf(TBuffer));
    Writed := Stream.Write(     @Buffer[0], Size mod SizeOf(TBuffer));
    Inc(Result, Writed);
    DoProgress(Writed);
    RangeDecoder_FinishDecode(FCoder);
  end;
end;

end.
