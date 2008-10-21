{
  Copyright (c) 1999-2008 Andrew Filinsky and Melchiorre Caruso

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

  v0.7.9 build 0890 - 2008.10.18 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,       // TStream
  // ---
  Bee_Files,     // TFileReader, TFileWriter...
  Bee_Codec,     // TSecondaryEncoder, TSecondaryDecoder...
  Bee_Headers,
  Bee_Modeller,  // TBaseCoder...
  Bee_Interface;

type

  // Extracting Modes:
  //   pmNorm  Extract files
  //   pmSkip  Extract files, but skip current
  //   pmTest  Test files (Extract to nul)
  //   pmQuit  Cancel extracting

  TExtractingMode = (pmNorm, pmSkip, pmTest, pmQuit);

type

  // Encoding Modes:
  //   emNorm  Encode files
  //   emOpt   Encode files to nul, with no messages

  TEncodingMode = (emNorm, emOpt);

type

  // Encoder ...

  TEncoder = class
  public
    constructor Create(aStream: TFileWriter; aInterfaces: TInterfaces;
      aSync: TSynchronizer);
    destructor Destroy; override;
    function EncodeFile(Header: THeader; Mode: TEncodingMode): boolean;
    function EncodeStrm(Header: THeader; Mode: TEncodingMode;
      SrcStrm: TFileReader): boolean;
    function CopyStrm(Header: THeader; Mode: TEncodingMode;
      SrcStrm: TFileReader): boolean;
  private
    function GetKey(Header: THeader): string;
    procedure Tick;
  private
    Stream: TFileWriter;
    PPM:    TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Interfaces: TInterfaces;
    Sync:   TSynchronizer;
  end;

type

  // Decoder ...

  TDecoder = class
  public
    constructor Create(aStream: TFileReader; aInterfaces: TInterfaces;
      aSync: TSynchronizer);
    destructor Destroy; override;
    function DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
    function DecodeStrm(Header: THeader; Mode: TExtractingMode;
      DstStrm: TFileWriter): boolean;
  private
    function GetKey(Header: THeader): string;
    procedure Tick;
  private
    Stream: TFileReader;
    PPM:    TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Interfaces: TInterfaces;
    Sync:   TSynchronizer;
  end;

implementation

uses
  SysUtils,      // FileSetAttr...
  // ---
  Bee_Crc,       // UpdCrc32...
  Bee_Common,    // ForceDirecotires, ...
  Bee_BlowFish,  // TBlowFish...
  Bee_Assembler;

/// TEncoder

constructor TEncoder.Create(aStream: TFileWriter; aInterfaces: TInterfaces;
  aSync: TSynchronizer);
begin
  Stream := aStream;
  SecondaryCodec := TSecondaryEncoder.Create(Stream);
  PPM    := TBaseCoder.Create(SecondaryCodec);
  Interfaces := aInterfaces;
  Sync   := aSync;
end;

destructor TEncoder.Destroy;
begin
  PPM.Free;
  SecondaryCodec.Free;
  Interfaces := nil;
  Sync := nil;
end;

function TEncoder.GetKey(Header: THeader): string;
begin
  Interfaces.OnKey.Data.FileName := ExtractFileName(Header.Data.FileName);
  Interfaces.OnKey.Data.FilePath := ExtractFilePath(Header.Data.FileName);
  Interfaces.OnKey.Data.FileSize := Header.Data.FileSize;
  Interfaces.OnKey.Data.FileTime := Header.Data.FileTime;

  Sync(Interfaces.OnKey.Method);
  Result := Interfaces.OnKey.Answer;

  if Length(Result) < MinKeyLength then
  begin
    Exclude(Header.Data.FileFlags, foPassword);
  end;
end;

procedure TEncoder.Tick;
begin
  while Interfaces.Suspend do Sleep(250);
  with Interfaces.OnTick.Data do
  begin
    Percentage := MulDiv(ProcessedSize, 100, TotalSize);
  end;
  Sync(Interfaces.OnTick.Method);
end;

function TEncoder.EncodeFile(Header: THeader; Mode: TEncodingMode): boolean;
var
  SrcFile: TFileReader;
  Symbol: byte;
  I: integer;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then PPM.FreshFlexible
  else
    PPM.FreshSolid;

  Header.Data.FileStartPos := Stream.Seek(0, 1); // stream flush
  Header.Data.FileCrc      := cardinal(-1);

  try
    SrcFile := TFileReader.Create(Header.FileLink, fmOpenRead +
      fmShareDenyWrite);
  except
    SrcFile := nil;
  end;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      Interfaces.OnDisplay.Data.Msg := msgUpdating + Header.Data.FileName;
      Sync(Interfaces.OnDisplay.Method);
    end;

    Header.Data.FileSize := SrcFile.Size;
    Header.Data.FileAttr := FileGetAttr(Header.Data.FileName);

    if foPassword in Header.Data.FileFlags then
    begin
      Stream.BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.Data.FileFlags then
    begin
      for I := 1 to Header.Data.FileSize do
      begin
        if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if Interfaces.Stop = False then Tick
          else
            Break;
        end;
        Inc(Interfaces.OnTick.Data.ProcessedSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Data.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end
    else begin
      SecondaryCodec.Start;
      for I := 1 to Header.Data.FileSize do
      begin
        if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if Interfaces.Stop = False then Tick
          else
            Break;
        end;
        Inc(Interfaces.OnTick.Data.ProcessedSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Data.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    SrcFile.Free;

    Header.Data.FilePacked := Stream.Seek(0, 1) - Header.Data.FileStartPos;
    // last stream flush
    Stream.BlowFish.Finish; // finish after last stream flush

    Sync(Interfaces.OnClear.Method);
  end
  else begin
    Interfaces.OnError.Data.Msg :=
      ('Error: can''t open file ' + Header.Data.FileName);
    Sync(Interfaces.OnError.Method);
  end;

  if (not (foMoved in Header.Data.FileFlags)) and
    (Header.Data.FilePacked >= Header.Data.FileSize) then
  begin
    Include(Header.Data.FileFlags, foTear);
    Include(Header.Data.FileFlags, foMoved);
    Stream.Size := Header.Data.FileStartPos;

    Dec(Interfaces.OnTick.Data.ProcessedSize, Header.Data.FileSize);
    Result := EncodeFile(Header, emOpt);
  end
  else
    Result := True;
end;

function TEncoder.EncodeStrm(Header: THeader; Mode: TEncodingMode;
  SrcStrm: TFileReader): boolean;
var
  SrcFile: TFileReader;
  SrcPosition: integer;
  Symbol: byte;
  I: integer;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then PPM.FreshFlexible
  else
    PPM.FreshSolid;

  SrcFile     := SrcStrm;
  SrcPosition := 0;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      Interfaces.OnDisplay.Data.Msg := msgEncoding + Header.Data.FileName;
      Sync(Interfaces.OnDisplay.Method);
    end;

    SrcPosition := Header.Data.FileStartPos;
    SrcFile.Seek(Header.Data.FileStartPos, 0);

    Header.Data.FileCrc      := cardinal(-1);
    Header.Data.FileStartPos := Stream.Seek(0, 1); // stream flush

    if foPassword in Header.Data.FileFlags then
    begin
      Stream.BlowFish.Start(GetKey(Header));
      if Header.Action = toSwap then
      begin
        SrcFile.BlowFish.Start(Interfaces.OnKey.Answer);
      end;
    end;

    if foMoved in Header.Data.FileFlags then
    begin
      for I := 1 to Header.Data.FileSize do
      begin
        if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if Interfaces.Stop = False then Tick
          else
            Break;
        end;
        Inc(Interfaces.OnTick.Data.ProcessedSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Data.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end
    else begin
      SecondaryCodec.Start;
      for I := 1 to Header.Data.FileSize do
      begin
        if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if Interfaces.Stop = False then Tick
          else
            Break;
        end;
        Inc(Interfaces.OnTick.Data.ProcessedSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Data.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    SrcFile.BlowFish.Finish;

    Header.Data.FilePacked := Stream.Seek(0, 1) - Header.Data.FileStartPos;
    // last stream flush
    Stream.BlowFish.Finish; // finish after last stream flush

    Sync(Interfaces.OnClear.Method);
  end
  else begin
    Interfaces.OnError.Data.Msg := ('Error: stream  not found');
    Sync(Interfaces.OnError.Method);
  end;

  if (not (foMoved in Header.Data.FileFlags)) and
    (Header.Data.FilePacked >= Header.Data.FileSize) then
  begin
    Include(Header.Data.FileFlags, foTear);
    Include(Header.Data.FileFlags, foMoved);
    Stream.Size := Header.Data.FileStartPos;
    Header.Data.FileStartPos := SrcPosition;

    Dec(Interfaces.OnTick.Data.ProcessedSize, Header.Data.FileSize);
    Result := EncodeStrm(Header, emOpt, SrcStrm);
  end
  else
    Result := True;
end;

function TEncoder.CopyStrm(Header: THeader; Mode: TEncodingMode;
  SrcStrm: TFileReader): boolean;
var
  SrcFile: TFileReader;
  Symbol: byte;
  I: integer;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then PPM.FreshFlexible
  else
    PPM.FreshSolid;

  SrcFile := SrcStrm;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      Interfaces.OnDisplay.Data.Msg := msgCopying + Header.Data.FileName;
      Sync(Interfaces.OnDisplay.Method);
    end;

    SrcFile.Seek(Header.Data.FileStartPos, 0);
    Header.Data.FileStartPos := Stream.Seek(0, 1);

    for I := 1 to Header.Data.FilePacked do
    begin
      if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
      begin
        if Interfaces.Stop = False then Tick
        else
          Break;
      end;
      Inc(Interfaces.OnTick.Data.ProcessedSize);
      SrcFile.Read(Symbol, 1);
      Stream.Write(Symbol, 1);
    end;

    Sync(Interfaces.OnClear.Method);
  end
  else begin
    Interfaces.OnError.Data.Msg := ('Error: stream  not found');
    Sync(Interfaces.OnError.Method);
  end;

  Result := True;
end;

/// TDecoder

constructor TDecoder.Create(aStream: TFileReader; aInterfaces: TInterfaces;
  aSync: TSynchronizer);
begin
  Stream := aStream;
  SecondaryCodec := TSecondaryDecoder.Create(Stream);
  PPM    := TBaseCoder.Create(SecondaryCodec);
  Interfaces := aInterfaces;
  Sync   := aSync;
end;

destructor TDecoder.Destroy;
begin
  PPM.Free;
  SecondaryCodec.Free;
  Interfaces := nil;
  Sync := nil;
end;

function TDecoder.GetKey(Header: THeader): string;
begin
  Interfaces.OnKey.Data.FileName := ExtractFileName(Header.Data.FileName);
  Interfaces.OnKey.Data.FilePath := ExtractFilePath(Header.Data.FileName);
  Interfaces.OnKey.Data.FileSize := Header.Data.FileSize;
  Interfaces.OnKey.Data.FileTime := Header.Data.FileTime;

  Sync(Interfaces.OnKey.Method);
  Result := Interfaces.OnKey.Answer;
end;

procedure TDecoder.Tick;
begin
  while Interfaces.Suspend do Sleep(250);
  with Interfaces.OnTick.Data do
  begin
    Percentage := MulDiv(ProcessedSize, 100, TotalSize);
  end;
  Sync(Interfaces.OnTick.Method);
end;

function TDecoder.DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
var
  DstFile: TFileWriter;
  I, Crc:  cardinal;
  Symbol:  byte;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: Interfaces.OnDisplay.Data.Msg :=
        msgSkipping + Header.Data.FileName;
    pmTest: Interfaces.OnDisplay.Data.Msg := msgTesting + Header.Data.FileName;
    pmNorm: Interfaces.OnDisplay.Data.Msg :=
        msgExtracting + Header.Data.FileName;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  Sync(Interfaces.OnDisplay.Method);

  Stream.Seek(Header.Data.FileStartPos, 0); // stream flush
  Crc := cardinal(-1);

  if Mode = pmNorm then try
      DstFile := TFileWriter.Create(Header.Data.FileName, fmCreate)
    except
      DstFile := nil;
    end
  else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin

    if foPassword in Header.Data.FileFlags then
    begin
      Stream.BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.Data.FileFlags then
    begin
      for I := 1 to Header.Data.FileSize do
      begin
        if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if Interfaces.Stop = False then Tick
          else
            Break;
        end;
        Inc(Interfaces.OnTick.Data.ProcessedSize);
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end
    else begin
      SecondaryCodec.Start;
      for I := 1 to Header.Data.FileSize do
      begin
        if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if Interfaces.Stop = False then Tick
          else
            Break;
        end;
        Inc(Interfaces.OnTick.Data.ProcessedSize);
        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
      SecondaryCodec.Flush;
    end;
    Stream.BlowFish.Finish;

    if Mode = pmNorm then
    begin
      DstFile.Flush;
      FileSetDate(DstFile.Handle, Header.Data.FileTime);
    end;
    DstFile.Free;

    if Mode = pmNorm then
      FileSetAttr(Header.Data.FileName, Header.Data.FileAttr);

    Sync(Interfaces.OnClear.Method);
  end;

  Result := Header.Data.FileCrc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then Interfaces.OnError.Data.Msg :=
        ('Error: can''t open file ' + Header.Data.FileName)
    else
      Interfaces.OnError.Data.Msg := msgCRCERROR + Header.Data.FileName;
    Sync(Interfaces.OnError.Method);
  end;
end;

function TDecoder.DecodeStrm(Header: THeader; Mode: TExtractingMode;
  DstStrm: TFileWriter): boolean;
var
  DstFile: TFileWriter;
  I, Crc:  cardinal;
  Symbol:  byte;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: Interfaces.OnDisplay.Data.Msg :=
        msgSkipping + Header.Data.FileName;
    pmTest: Interfaces.OnDisplay.Data.Msg := msgTesting + Header.Data.FileName;
    pmNorm: Interfaces.OnDisplay.Data.Msg :=
        msgDecoding + Header.Data.FileName;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  Sync(Interfaces.OnDisplay.Method);

  Stream.Seek(Header.Data.FileStartPos, 0);
  Crc := cardinal(-1);

  if Mode = pmNorm then try
      DstFile := DstStrm;
      Header.Data.FileStartPos := DstFile.Seek(0, 1);
    except
      DstFile := nil;
    end
  else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin

    if foPassword in Header.Data.FileFlags then
    begin
      Stream.BlowFish.Start(GetKey(Header));
      if Header.Action = toSwap then
      begin
        DstFile.BlowFish.Start(Interfaces.OnKey.Answer);
      end;
    end;

    if foMoved in Header.Data.FileFlags then
    begin
      for I := 1 to Header.Data.FileSize do
      begin
        if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if Interfaces.Stop = False then Tick
          else
            Break;
        end;
        Inc(Interfaces.OnTick.Data.ProcessedSize);
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end
    else begin
      SecondaryCodec.Start;
      for I := 1 to Header.Data.FileSize do
      begin
        if Interfaces.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if Interfaces.Stop = False then Tick
          else
            Break;
        end;
        Inc(Interfaces.OnTick.Data.ProcessedSize);
        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
      SecondaryCodec.Flush;
    end;
    Stream.BlowFish.Finish;

    if Mode = pmNorm then
    begin
      DstFile.Flush; // last stream flush
    end;
    DstFile.BlowFish.Finish; // finish after last stream flush

    Sync(Interfaces.OnClear.Method);
  end;

  Result := Header.Data.FileCrc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then Interfaces.OnError.Data.Msg :=
        ('Error: stream not found')
    else
      Interfaces.OnError.Data.Msg := msgCRCERROR + Header.Data.FileName;
    Sync(Interfaces.OnError.Method);
  end;
end;

end.
