{
  Copyright (c) 1999-2007 Andrew Filinsky and Melchiorre Caruso

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

  TEncoder class, file encoder;
  TDecoder class, file decoder;

  Modifyed:

  v0.7.8 build 0148 - 2005.06.23 by Melchiorre Caruso;
  v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
  v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

  v0.7.9 build 0505 - 2007.11.25 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,      // TStream

  Bee_App,
  Bee_Codec,    // TSecondaryEncoder, TSecondaryDecoder...
  Bee_Headers,
  Bee_Modeller; // TBaseCoder...

// Extracting Modes:
//   pmNorm  Extract files
//   pmSkip  Extract files, but skip current
//   pmTest  Test files (Extract to nul)
//   pmQuit  Cancel extracting

type
  TExtractingMode = (pmNorm, pmSkip, pmTest, pmQuit);

// Encoding Modes:
//   emNorm  Encode files
//   emOpt   Encode files to nul, with no messages

type
  TEncodingMode = (emNorm, emOpt);

// Encoder ...

type
  TEncoder = class
  public
    constructor Create(aStream: TStream; aApp: TBeeApp);
    destructor Destroy; override;
    function EncodeFile(Header: THeader; Mode: TEncodingMode): boolean;
    function EncodeStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TStream): boolean;
    function CopyStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TStream): boolean;
  private
    function GetKey(Header: THeader): string;
  private
    App: TBeeApp;
    Stream: TStream;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
  end;

// Decoder ...

type
  TDecoder = class
  public
    constructor Create(aStream: TStream; aApp: TBeeApp);
    destructor Destroy; override;
    function DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
    function DecodeStrm(Header: THeader; Mode: TExtractingMode; DstStrm: TStream): boolean;
  private
    function GetKey(Header: THeader): string;
  private
    App: TBeeApp;
    Stream: TStream;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
  end;

implementation

uses
  SysUtils,     // FileSetAttr...

  Bee_Crc,      // UpdCrc32...
  Bee_Files,    // TBufferizedReader
  Bee_Common,   // Diag...
  Bee_BlowFish;

/// TEncoder

constructor TEncoder.Create(aStream: TStream; aApp: TBeeApp);
begin
  App := aApp;
  Stream := aStream;
  SecondaryCodec := TSecondaryEncoder.Create(Stream);
  PPM := TBaseCoder.Create(SecondaryCodec);
end;

destructor TEncoder.Destroy;
begin
  PPM.Free;
  SecondaryCodec.Free;
end;

function TEncoder.GetKey(Header: THeader): string;
begin
  App.AppInterface.cFileName := Header.Name;
  App.AppInterface.cFileSize := Header.Size;
  App.AppInterface.cFileTime := Header.Time;

  App.Syn(App.AppInterface.OnKey);
  Result := App.AppInterface.cMsg;

  if Length(App.AppInterface.cMsg) < MinKeyLength then
  begin
    Exclude(Header.Flags, foPassword);
  end;
end;

function TEncoder.EncodeFile(Header: THeader; Mode: TEncodingMode): boolean;
var
  SrcFile: TFileReader;
  Symbol: byte;
  I: integer;
begin
  if foDictionary in Header.Flags then
    PPM.SetDictionary(Header.Dictionary);

  if foTable in Header.Flags then
    PPM.SetTable(Header.Table);

  if foTear in Header.Flags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  Header.StartPos := Stream.Seek(0, 1); // stream flush
  Header.Crc := cardinal(-1);

  try
    SrcFile := TFileReader.Create(Header.Name, fmOpenRead + fmShareDenyWrite);
  except
    SrcFile := nil;
  end;

  if (SrcFile <> nil) then
  begin
  
    if Mode = emNorm then
    begin
      App.AppInterface.cMsg := msgUpdating + Header.GetName;
      App.Syn(App.AppInterface.OnDisplay);
    end;

    Header.Size := SrcFile.Size;
    Header.Attr := FileGetAttr(Header.Name);

    if foPassword in Header.Flags then
    begin
      TFileWriter(Stream).BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.Flags then
    begin
      for I := 1 to Header.Size do
      begin
        if App.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.RemainSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Crc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.Size do
      begin
        if App.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.RemainSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Crc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    SrcFile.Free;

    Header.PackedSize := Stream.Seek(0, 1) - Header.StartPos; // last stream flush
    TFileWriter(Stream).BlowFish.Finish; // finish after last stream flush

    App.Syn(App.AppInterface.OnClear);
  end else
  begin
    App.AppInterface.cMsg := ('Error: can''t open file ' + Header.Name);
    App.Syn(App.AppInterface.OnError);
  end;

  if (not (foMoved in Header.Flags)) and (Header.PackedSize >= Header.Size) then
  begin
    Include(Header.Flags, foTear);
    Include(Header.Flags, foMoved);
    Stream.Size := Header.StartPos;

    Dec(App.RemainSize, Header.Size);
    Result := EncodeFile(Header, emOpt);
  end else
    Result := True;
end;

function TEncoder.EncodeStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TStream): boolean;
var
  SrcFile: TStream;
  SrcPosition: integer;
  Symbol: byte;
  I: integer;
begin
  if foDictionary in Header.Flags then
    PPM.SetDictionary(Header.Dictionary);

  if foTable in Header.Flags then
    PPM.SetTable(Header.Table);

  if foTear in Header.Flags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  SrcPosition := 0;
  SrcFile := SrcStrm;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      App.AppInterface.cMsg := msgEncoding + Header.Name;
      App.Syn(App.AppInterface.OnDisplay);
    end;

    SrcPosition := Header.StartPos;
    SrcFile.Seek(Header.StartPos, 0);

    Header.Crc := cardinal(-1);
    Header.StartPos := Stream.Seek(0, 1); // stream flush

    if foPassword in Header.Flags then
    begin
      TFileReader(SrcFile).BlowFish.Start(GetKey(Header));
      TFileWriter(Stream).BlowFish.Start(App.AppInterface.cMsg);
    end;

    if foMoved in Header.Flags then
    begin
      for I := 1 to Header.Size do
      begin
        if App.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.RemainSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Crc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.Size do
      begin
        if App.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.RemainSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Crc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    TFileReader(SrcFile).BlowFish.Finish;

    Header.PackedSize := Stream.Seek(0, 1) - Header.StartPos; // last stream flush
    TFileWriter(Stream).BlowFish.Finish; // finish after last stream flush

    App.Syn(App.AppInterface.OnClear);
  end else
  begin
    App.AppInterface.cMsg := ('Error: stream  not found');
    App.Syn(App.AppInterface.OnError);
  end;

  if (not (foMoved in Header.Flags)) and (Header.PackedSize >= Header.Size) then
  begin
    Include(Header.Flags, foTear);
    Include(Header.Flags, foMoved);
    Stream.Size := Header.StartPos;
    Header.StartPos := SrcPosition;

    Dec(App.RemainSize, Header.Size);
    Result := EncodeStrm(Header, emOpt, SrcStrm);
  end else
    Result := True;
end;

function TEncoder.CopyStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TStream): boolean;
var
  SrcFile: TStream;
  Symbol: byte;
  I: integer;
begin
  if foDictionary in Header.Flags then
    PPM.SetDictionary(Header.Dictionary);

  if foTable in Header.Flags then
    PPM.SetTable(Header.Table);

  if foTear in Header.Flags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  SrcFile := SrcStrm;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      App.AppInterface.cMsg := msgCopying + Header.Name;
      App.Syn(App.AppInterface.OnDisplay);
    end;

    SrcFile.Seek(Header.StartPos, 0);
    Header.StartPos := Stream.Seek(0, 1);

    for I := 1 to Header.PackedSize do
    begin
      if App.RemainSize and $FFFF = 0 then
      begin
        if App.Tick then Break;
      end;
      Inc(App.RemainSize);
      SrcFile.Read(Symbol, 1);
      Stream.Write(Symbol, 1);
    end;

    App.Syn(App.AppInterface.OnClear);
  end else
  begin
    App.AppInterface.cMsg := ('Error: stream  not found');
    App.Syn(App.AppInterface.OnError);
  end;

  Result := True;
end;

/// TDecoder

constructor TDecoder.Create(aStream: TStream; aApp: TBeeApp);
begin
  App := aApp;
  Stream := aStream;
  SecondaryCodec := TSecondaryDecoder.Create(Stream);
  PPM := TBaseCoder.Create(SecondaryCodec);
end;

destructor TDecoder.Destroy;
begin
  PPM.Free;
  SecondaryCodec.Free;
end;

function TDecoder.GetKey(Header: THeader): string;
begin
  App.AppInterface.cFileName := Header.Name;
  App.AppInterface.cFileSize := Header.Size;
  App.AppInterface.cFileTime := Header.Time;

  App.Syn(App.AppInterface.OnKey);
  Result := App.AppInterface.cMsg;
end;

function TDecoder.DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
var
  DstFile: TStream;
  I, Crc: cardinal;
  Symbol: byte;
begin
  if foDictionary in Header.Flags then
    PPM.SetDictionary(Header.Dictionary);
    
  if foTable in Header.Flags then
    PPM.SetTable(Header.Table);

  if foTear in Header.Flags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: App.AppInterface.cMsg := msgSkipping   + Header.Name;
    pmTest: App.AppInterface.cMsg := msgTesting    + Header.Name;
    pmNorm: App.AppInterface.cMsg := msgExtracting + Header.Name;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  App.Syn(App.AppInterface.OnDisplay);

  Stream.Seek(Header.StartPos, 0); // stream flush
  Crc := cardinal(-1);

  if Mode = pmNorm then
    try
      DstFile := TFileWriter.Create(Header.Name, fmCreate)
    except
      DstFile := nil;
    end
  else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin

    if foPassword in Header.Flags then
    begin
      TFileReader(Stream).BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.Flags then
    begin
      for I := 1 to Header.Size do
      begin
        if App.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.RemainSize);
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.Size do
      begin
        if App.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.RemainSize);
        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
      SecondaryCodec.Flush;
    end;
    TFileReader(Stream).BlowFish.Finish;

    if Mode = pmNorm then
    begin
      TFileWriter(DstFile).Flush;
      FileSetDate(TFileWriter(DstFile).Handle, Header.Time);
    end;
    DstFile.Free;

    if Mode = pmNorm then
      FileSetAttr(Header.Name, Header.Attr);

    App.Syn(App.AppInterface.OnClear);
  end;

  Result := Header.Crc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      App.AppInterface.cMsg := ('Error: can''t open file ' + Header.Name)
    else
      App.AppInterface.cMsg := msgCRCERROR + Header.Name;
    App.Syn(App.AppInterface.OnError);
  end;
end;

function TDecoder.DecodeStrm(Header: THeader; Mode: TExtractingMode; DstStrm: TStream): boolean;
var
  DstFile: TStream;
  I, Crc: cardinal;
  Symbol: byte;
begin
  if foDictionary in Header.Flags then
    PPM.SetDictionary(Header.Dictionary);
    
  if foTable in Header.Flags then
    PPM.SetTable(Header.Table);
    
  if foTear in Header.Flags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: App.AppInterface.cMsg := msgSkipping + Header.Name;
    pmTest: App.AppInterface.cMsg := msgTesting  + Header.Name;
    pmNorm: App.AppInterface.cMsg := msgDecoding + Header.Name;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  App.Syn(App.AppInterface.OnDisplay);

  Stream.Seek(Header.StartPos, 0);
  Crc := cardinal(-1);

  if Mode = pmNorm then
    try
      DstFile := DstStrm;
      Header.StartPos := DstFile.Seek(0, 1);
    except
      DstFile := nil;
    end
  else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin

    if foPassword in Header.Flags then
    begin
      TFileReader(Stream).BlowFish.Start(GetKey(Header));
      TFileWriter(DstFile).BlowFish.Start(App.AppInterface.cMsg);
    end;

    if foMoved in Header.Flags then
    begin
      for I := 1 to Header.Size do
      begin
        if App.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.RemainSize);
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.Size do
      begin
        if App.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.RemainSize);
        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
      SecondaryCodec.Flush;
    end;
    TFileReader(Stream).BlowFish.Finish;

    if Mode = pmNorm then
    begin
      TFileWriter(DstFile).Flush; // last stream flush
    end;
    TFileWriter(DstFile).BlowFish.Finish; // finish after last stream flush

    App.Syn(App.AppInterface.OnClear);
  end;

  Result := Header.Crc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      App.AppInterface.cMsg := ('Error: stream not found')
    else
      App.AppInterface.cMsg := msgCRCERROR + Header.Name;
    App.Syn(App.AppInterface.OnError);
  end;
end;

end.
