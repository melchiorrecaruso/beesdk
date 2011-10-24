{
  Copyright (c) 2010-2011 Melchiorre Caruso

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

  Modifyed:
}

unit BeeLib_StreamCoder;

interface

uses
  BeeLib_Interface,
  BeeLib_Configuration;

  function  CreateEncoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv: TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer;
  function  CreateDecoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv: TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer;

  procedure DestroyCoder(Handle: pointer);

  procedure SetDictionaryLevel(Handle: pointer; Value: longword);
  procedure SetTableParameters(Handle: pointer; const Value: TTableParameters);

  procedure FreshFlexible(Handle: pointer);
  procedure FreshSolid(Handle: pointer);

  function  Encode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64;
  function  Decode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64;

implementation

uses
  BeeLib_Crc,
  BeeLib_Codec,
  BeeLib_Stream,
  BeeLib_Modeller;

type
  { TStreamCoder packed record }

  TStreamCoder = packed record
    Tick: pointer;
    OnTick: TTickEvent;

    Stream: TStream;
    OnFill: TFillEvent;
    OnFlush: TFlushEvent;

    SecondaryCodec: TSecondaryCodec;
    PPM: TBaseCoder;
  end;

{ TStreamCoder routines ... }

procedure SetDictionaryLevel(Handle: pointer; Value: longword);
begin
  with TStreamCoder(Handle^) do
  begin
    PPM.SetDictionary(Value);
  end;
end;

procedure SetTableParameters(Handle: pointer; const Value: TTableParameters);
begin
  with TStreamCoder(Handle^) do
  begin
    PPM.SetTable(Value);
  end;
end;

procedure FreshFlexible(Handle: pointer);
begin
  with TStreamCoder(Handle^) do
  begin
    PPM.FreshFlexible;
  end;
end;

procedure FreshSolid(Handle: pointer);
begin
  with TStreamCoder(Handle^) do
  begin
    PPM.FreshSolid;
  end;
end;

function CreateEncoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv: TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer;
begin
  GetMem(Result, SizeOf(TStreamCoder));
  with TStreamCoder(Result^) do
  begin
    Tick    := TickPtr;
    OnTick  := OnTickEv;

    Stream  := TWriteStream.Create(StrmPtr, OnFlushEv);
    OnFill  := OnFillEv;
    OnFlush := OnFlushEv;

    SecondaryCodec := TSecondaryEncoder.Create(Stream);
    PPM := TBaseCoder.Create(SecondaryCodec);
    PPM.SetDictionary(DefaultDictionaryLevel);
    PPM.SetTable(DefaultTableParameters);
  end;
end;

procedure DestroyCoder(Handle: pointer);
begin
  with TStreamCoder(Handle^) do
  begin
    PPM.Free;
    SecondaryCodec.Free;
    Stream.Free;
  end;
  FreeMem(Handle);
end;

function Encode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
  Source: TReadStream;
begin
  Result := 0;
  CRC    := longword(-1);

  with TStreamCoder(Handle^) do
  begin
    Source := TReadStream.Create(StrmPtr, OnFill);
    SecondaryCodec.Start;
    while Result < Size do
    begin
      Symbol := Source.Read;    // eof bug
      PPM.UpdateModel(Symbol);
      UpdCrc32(CRC, Symbol);
      Inc(Result);

      if (Result and DefaultTickStepSize = 0)
        and Assigned(OnTick)
        and OnTick(Tick) then Break;
    end;
    SecondaryCodec.Flush;
    Source.Free;
  end;
end;

{ TStreamDecoder class }

function CreateDecoder(StrmPtr: pointer; OnFillEv: TFillEvent; OnFlushEv: TFlushEvent; TickPtr: pointer; OnTickEv: TTickEvent): pointer;
begin
  GetMem(Result, SizeOf(TStreamCoder));
  with TStreamCoder(Result^) do
  begin
    Tick    := TickPtr;
    OnTick  := OnTickEv;

    Stream  := TReadStream.Create(StrmPtr, OnFillEv);
    OnFill  := OnFillEv;
    OnFlush := OnFlushEv;

    SecondaryCodec := TSecondaryDecoder.Create(Stream);
    PPM := TBaseCoder.Create(SecondaryCodec);
    PPM.SetDictionary(DefaultDictionaryLevel);
    PPM.SetTable(DefaultTableParameters);
  end;
end;

function Decode(Handle: pointer; StrmPtr: pointer; const Size: int64; var CRC: longword): int64;
var
  Symbol: byte;
  Dest: TWriteStream;
begin
  Result := 0;
  CRC    := longword(-1);

  with TStreamCoder(Handle^) do
  begin
    Stream.FlushBuffer; // Clear stream

    Dest := TWriteStream.Create(StrmPtr, OnFlush);
    SecondaryCodec.Start;
    while Result < Size do
    begin
      Symbol := PPM.UpdateModel(0);
      Dest.Write(Symbol);
      UpdCrc32(CRC, Symbol);
      Inc(Result);

      if (Result and DefaultTickStepSize = 0)
        and Assigned(OnTick)
        and OnTick(Tick) then Break;
    end;
    SecondaryCodec.Flush;
    Dest.Free;
  end;
end;

end.
