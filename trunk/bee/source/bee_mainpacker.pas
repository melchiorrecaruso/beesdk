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

  v0.7.9 build 0593 - 2000.01.12 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,      // TStream

  Bee_App,
  Bee_Codec,    // TSecondaryEncoder, TSecondaryDecoder...
  Bee_Headers,
  Bee_Modeller, // TBaseCoder...
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

type

  // Decoder ...

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
  // ---
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
  App.AppInterface.OnKey.Data.FileName := ExtractFileName(Header.FileName);
  App.AppInterface.OnKey.Data.FilePath := ExtractFilePath(Header.FileName);
  App.AppInterface.OnKey.Data.FileSize := Header.FileSize;
  App.AppInterface.OnKey.Data.FileTime := Header.FileTime;

  App.Sync(App.AppInterface.OnKey.Method);
  Result := App.AppInterface.OnKey.Answer;
  
  if Length(Result) < MinKeyLength then
  begin
    Exclude(Header.FileFlags, foPassword);
  end;
end;

function TEncoder.EncodeFile(Header: THeader; Mode: TEncodingMode): boolean;
var
  SrcFile: TFileReader;
  Symbol: byte;
  I: integer;
begin
  if foDictionary in Header.FileFlags then
    PPM.SetDictionary(Header.FileDictionary);

  if foTable in Header.FileFlags then
    PPM.SetTable(Header.FileTable);

  if foTear in Header.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  Header.FileStartPos := Stream.Seek(0, 1); // stream flush
  Header.FileCrc := cardinal(-1);

  try
    SrcFile := TFileReader.Create(Header.FileLink, fmOpenRead + fmShareDenyWrite);
  except
    SrcFile := nil;
  end;

  if (SrcFile <> nil) then
  begin
  
    if Mode = emNorm then
    begin
      App.AppInterface.OnDisplay.Data.Msg := msgUpdating + Header.FileName;
      App.Sync(App.AppInterface.OnDisplay.Method);
    end;

    Header.FileSize := SrcFile.Size;
    Header.FileAttr := FileGetAttr(Header.FileName);

    if foPassword in Header.FileFlags then
    begin
      TFileWriter(Stream).BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.FileFlags then
    begin
      for I := 1 to Header.FileSize do
      begin
        if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.AppInterface.OnTick.Data.RemainSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.FileSize do
      begin
        if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.AppInterface.OnTick.Data.RemainSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    SrcFile.Free;

    Header.FilePacked := Stream.Seek(0, 1) - Header.FileStartPos; // last stream flush
    TFileWriter(Stream).BlowFish.Finish; // finish after last stream flush

    App.Sync(App.AppInterface.OnClear.Method);
  end else
  begin
    App.AppInterface.OnError.Data.Msg := ('Error: can''t open file ' + Header.FileName);
    App.Sync(App.AppInterface.OnError.Method);
  end;

  if (not (foMoved in Header.FileFlags)) and (Header.FilePacked >= Header.FileSize) then
  begin
    Include(Header.FileFlags, foTear);
    Include(Header.FileFlags, foMoved);
    Stream.Size := Header.FileStartPos;

    Dec(App.AppInterface.OnTick.Data.RemainSize, Header.FileSize);
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
  if foDictionary in Header.FileFlags then
    PPM.SetDictionary(Header.FileDictionary);

  if foTable in Header.FileFlags then
    PPM.SetTable(Header.FileTable);

  if foTear in Header.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  SrcPosition := 0;
  SrcFile := SrcStrm;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      App.AppInterface.OnDisplay.Data.Msg := msgEncoding + Header.FileName;
      App.Sync(App.AppInterface.OnDisplay.Method);
    end;

    SrcPosition := Header.FileStartPos;
    SrcFile.Seek(Header.FileStartPos, 0);

    Header.FileCrc := cardinal(-1);
    Header.FileStartPos := Stream.Seek(0, 1); // stream flush

    if foPassword in Header.FileFlags then
    begin
      TFileReader(SrcFile).BlowFish.Start(GetKey(Header));
      TFileWriter(Stream).BlowFish.Start(App.AppInterface.OnKey.Answer);
    end;

    if foMoved in Header.FileFlags then
    begin
      for I := 1 to Header.FileSize do
      begin
        if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.AppInterface.OnTick.Data.RemainSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.FileSize do
      begin
        if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.AppInterface.OnTick.Data.RemainSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    TFileReader(SrcFile).BlowFish.Finish;

    Header.FilePacked := Stream.Seek(0, 1) - Header.FileStartPos; // last stream flush
    TFileWriter(Stream).BlowFish.Finish; // finish after last stream flush

    App.Sync(App.AppInterface.OnClear.Method);
  end else
  begin
    App.AppInterface.OnError.Data.Msg := ('Error: stream  not found');
    App.Sync(App.AppInterface.OnError.Method);
  end;

  if (not (foMoved in Header.FileFlags)) and (Header.FilePacked >= Header.FileSize) then
  begin
    Include(Header.FileFlags, foTear);
    Include(Header.FileFlags, foMoved);
    Stream.Size := Header.FileStartPos;
    Header.FileStartPos := SrcPosition;

    Dec(App.AppInterface.OnTick.Data.RemainSize, Header.FileSize);
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
  if foDictionary in Header.FileFlags then
    PPM.SetDictionary(Header.FileDictionary);

  if foTable in Header.FileFlags then
    PPM.SetTable(Header.FileTable);

  if foTear in Header.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  SrcFile := SrcStrm;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      App.AppInterface.OnDisplay.Data.Msg := msgCopying + Header.FileName;
      App.Sync(App.AppInterface.OnDisplay.Method);
    end;

    SrcFile.Seek(Header.FileStartPos, 0);
    Header.FileStartPos := Stream.Seek(0, 1);

    for I := 1 to Header.FilePacked do
    begin
      if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
      begin
        if App.Tick then Break;
      end;
      Inc(App.AppInterface.OnTick.Data.RemainSize);
      SrcFile.Read(Symbol, 1);
      Stream.Write(Symbol, 1);
    end;

    App.Sync(App.AppInterface.OnClear.Method);
  end else
  begin
    App.AppInterface.OnError.Data.Msg := ('Error: stream  not found');
    App.Sync(App.AppInterface.OnError.Method);
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
  App.AppInterface.OnKey.Data.FileName := ExtractFileName(Header.FileName);
  App.AppInterface.OnKey.Data.FilePath := ExtractFilePath(Header.FileName);
  App.AppInterface.OnKey.Data.FileSize := Header.FileSize;
  App.AppInterface.OnKey.Data.FileTime := Header.FileTime;

  App.Sync(App.AppInterface.OnKey.Method);
  Result := App.AppInterface.OnKey.Answer;
end;

function TDecoder.DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
var
  DstFile: TStream;
  I, Crc: cardinal;
  Symbol: byte;
begin
  if foDictionary in Header.FileFlags then
    PPM.SetDictionary(Header.FileDictionary);
    
  if foTable in Header.FileFlags then
    PPM.SetTable(Header.FileTable);

  if foTear in Header.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: App.AppInterface.OnDisplay.Data.Msg := msgSkipping   + Header.FileName;
    pmTest: App.AppInterface.OnDisplay.Data.Msg := msgTesting    + Header.FileName;
    pmNorm: App.AppInterface.OnDisplay.Data.Msg := msgExtracting + Header.FileName;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  App.Sync(App.AppInterface.OnDisplay.Method);

  Stream.Seek(Header.FileStartPos, 0); // stream flush
  Crc := cardinal(-1);

  if Mode = pmNorm then
    try
      DstFile := TFileWriter.Create(Header.FileName, fmCreate)
    except
      DstFile := nil;
    end
  else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin

    if foPassword in Header.FileFlags then
    begin
      TFileReader(Stream).BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.FileFlags then
    begin
      for I := 1 to Header.FileSize do
      begin
        if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.AppInterface.OnTick.Data.RemainSize);
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.FileSize do
      begin
        if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.AppInterface.OnTick.Data.RemainSize);
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
      FileSetDate(TFileWriter(DstFile).Handle, Header.FileTime);
    end;
    DstFile.Free;

    if Mode = pmNorm then
      FileSetAttr(Header.FileName, Header.FileAttr);

    App.Sync(App.AppInterface.OnClear.Method);
  end;

  Result := Header.FileCrc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      App.AppInterface.OnError.Data.Msg := ('Error: can''t open file ' + Header.FileName)
    else
      App.AppInterface.OnError.Data.Msg := msgCRCERROR + Header.FileName;
    App.Sync(App.AppInterface.OnError.Method);
  end;
end;

function TDecoder.DecodeStrm(Header: THeader; Mode: TExtractingMode; DstStrm: TStream): boolean;
var
  DstFile: TStream;
  I, Crc: cardinal;
  Symbol: byte;
begin
  if foDictionary in Header.FileFlags then
    PPM.SetDictionary(Header.FileDictionary);
    
  if foTable in Header.FileFlags then
    PPM.SetTable(Header.FileTable);
    
  if foTear in Header.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: App.AppInterface.OnDisplay.Data.Msg := msgSkipping + Header.FileName;
    pmTest: App.AppInterface.OnDisplay.Data.Msg := msgTesting  + Header.FileName;
    pmNorm: App.AppInterface.OnDisplay.Data.Msg := msgDecoding + Header.FileName;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  App.Sync(App.AppInterface.OnDisplay.Method);

  Stream.Seek(Header.FileStartPos, 0);
  Crc := cardinal(-1);

  if Mode = pmNorm then
    try
      DstFile := DstStrm;
      Header.FileStartPos := DstFile.Seek(0, 1);
    except
      DstFile := nil;
    end
  else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin

    if foPassword in Header.FileFlags then
    begin
      TFileReader(Stream).BlowFish.Start(GetKey(Header));
      TFileWriter(DstFile).BlowFish.Start(App.AppInterface.OnKey.Answer);
    end;

    if foMoved in Header.FileFlags then
    begin
      for I := 1 to Header.FileSize do
      begin
        if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.AppInterface.OnTick.Data.RemainSize);
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.FileSize do
      begin
        if App.AppInterface.OnTick.Data.RemainSize and $FFFF = 0 then
        begin
          if App.Tick then Break;
        end;
        Inc(App.AppInterface.OnTick.Data.RemainSize);
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

    App.Sync(App.AppInterface.OnClear.Method);
  end;

  Result := Header.FileCrc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      App.AppInterface.OnError.Data.Msg := ('Error: stream not found')
    else
      App.AppInterface.OnError.Data.Msg := msgCRCERROR + Header.FileName;
    App.Sync(App.AppInterface.OnError.Method);
  end;
end;

end.
