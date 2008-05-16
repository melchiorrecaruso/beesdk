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

  v0.7.9 build 0693 - 2000.03.31 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,      // TStream
  
  Bee_Files,    // TFileReader, TFileWriter...
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
    constructor Create(aStream: TFileWriter; const aAppInterface: TAppInterface);
    destructor Destroy; override;
    function EncodeFile(Header: THeader; Mode: TEncodingMode): boolean;
    function EncodeStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TFileReader): boolean;
    function CopyStrm  (Header: THeader; Mode: TEncodingMode; SrcStrm: TFileReader): boolean;
  private
    function GetKey(Header: THeader): string;
  private
    Stream: TFileWriter;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    AppInterface: TAppInterface;
  end;

type

  // Decoder ...

  TDecoder = class
  public
    constructor Create(aStream: TFileReader; const aAppInterface: TAppInterface);
    destructor Destroy; override;
    function DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
    function DecodeStrm(Header: THeader; Mode: TExtractingMode; DstStrm: TFileWriter): boolean;
  private
    function GetKey(Header: THeader): string;
  private
    Stream: TFileReader;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    AppInterface: TAppInterface;
  end;

implementation

uses
  SysUtils,     // FileSetAttr...
  // ---
  Bee_Crc,      // UpdCrc32...
  Bee_Common,   // ForceDirecotires, ...
  Bee_BlowFish; // TBlowFish...

/// TEncoder

constructor TEncoder.Create(aStream: TFileWriter; const aAppInterface: TAppInterface);
begin
  Stream := aStream;
  SecondaryCodec := TSecondaryEncoder.Create(Stream);
  PPM := TBaseCoder.Create(SecondaryCodec);
  AppInterface := aAppInterface;
end;

destructor TEncoder.Destroy;
begin
  PPM.Free;
  SecondaryCodec.Free;
  AppInterface := nil;
end;

function TEncoder.GetKey(Header: THeader): string;
begin
  AppInterface.OnKey.Data.FileName := ExtractFileName(Header.FileName);
  AppInterface.OnKey.Data.FilePath := ExtractFilePath(Header.FileName);
  AppInterface.OnKey.Data.FileSize := Header.FileSize;
  AppInterface.OnKey.Data.FileTime := Header.FileTime;

  AppInterface.Methods.Synchronize(AppInterface.OnKey.Method);
  Result := AppInterface.OnKey.Answer;
  
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
      AppInterface.OnDisplay.Data.Msg := msgUpdating + Header.FileName;
      AppInterface.Methods.Synchronize(AppInterface.OnDisplay.Method);
    end;

    Header.FileSize := SrcFile.Size;
    Header.FileAttr := FileGetAttr(Header.FileName);

    if foPassword in Header.FileFlags then
    begin
      Stream.BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.FileFlags then
    begin
      for I := 1 to Header.FileSize do
      begin
        if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if AppInterface.Methods.Tick then Break;
        end;
        Inc(AppInterface.OnTick.Data.ProcessedSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.FileSize do
      begin
        if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if AppInterface.Methods.Tick then Break;
        end;
        Inc(AppInterface.OnTick.Data.ProcessedSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    SrcFile.Free;

    Header.FilePacked := Stream.Seek(0, 1) - Header.FileStartPos; // last stream flush
    Stream.BlowFish.Finish; // finish after last stream flush

    AppInterface.Methods.Synchronize(AppInterface.OnClear.Method);
  end else
  begin
    AppInterface.OnError.Data.Msg := ('Error: can''t open file ' + Header.FileName);
    AppInterface.Methods.Synchronize(AppInterface.OnError.Method);
  end;

  if (not (foMoved in Header.FileFlags)) and (Header.FilePacked >= Header.FileSize) then
  begin
    Include(Header.FileFlags, foTear);
    Include(Header.FileFlags, foMoved);
    Stream.Size := Header.FileStartPos;

    Dec(AppInterface.OnTick.Data.ProcessedSize, Header.FileSize);
    Result := EncodeFile(Header, emOpt);
  end else
    Result := True;
end;

function TEncoder.EncodeStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TFileReader): boolean;
var
  SrcFile: TFileReader;
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

  SrcFile := SrcStrm;
  SrcPosition := 0;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      AppInterface.OnDisplay.Data.Msg := msgEncoding + Header.FileName;
      AppInterface.Methods.Synchronize(AppInterface.OnDisplay.Method);
    end;

    SrcPosition := Header.FileStartPos;
    SrcFile.Seek(Header.FileStartPos, 0);

    Header.FileCrc := cardinal(-1);
    Header.FileStartPos := Stream.Seek(0, 1); // stream flush

    if foPassword in Header.FileFlags then
    begin
      Stream.BlowFish.Start(GetKey(Header));
      if Header.Action = toSwap then
      begin
        SrcFile.BlowFish.Start(AppInterface.OnKey.Answer);
      end;
    end;

    if foMoved in Header.FileFlags then
    begin
      for I := 1 to Header.FileSize do
      begin
        if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if AppInterface.Methods.Tick then Break;
        end;
        Inc(AppInterface.OnTick.Data.ProcessedSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.FileSize do
      begin
        if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if AppInterface.Methods.Tick then Break;
        end;
        Inc(AppInterface.OnTick.Data.ProcessedSize);
        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    SrcFile.BlowFish.Finish;

    Header.FilePacked := Stream.Seek(0, 1) - Header.FileStartPos; // last stream flush
    Stream.BlowFish.Finish; // finish after last stream flush

    AppInterface.Methods.Synchronize(AppInterface.OnClear.Method);
  end else
  begin
    AppInterface.OnError.Data.Msg := ('Error: stream  not found');
    AppInterface.Methods.Synchronize(AppInterface.OnError.Method);
  end;

  if (not (foMoved in Header.FileFlags)) and (Header.FilePacked >= Header.FileSize) then
  begin
    Include(Header.FileFlags, foTear);
    Include(Header.FileFlags, foMoved);
    Stream.Size := Header.FileStartPos;
    Header.FileStartPos := SrcPosition;

    Dec(AppInterface.OnTick.Data.ProcessedSize, Header.FileSize);
    Result := EncodeStrm(Header, emOpt, SrcStrm);
  end else
    Result := True;
end;

function TEncoder.CopyStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TFileReader): boolean;
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

  SrcFile := SrcStrm;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      AppInterface.OnDisplay.Data.Msg := msgCopying + Header.FileName;
      AppInterface.Methods.Synchronize(AppInterface.OnDisplay.Method);
    end;

    SrcFile.Seek(Header.FileStartPos, 0);
    Header.FileStartPos := Stream.Seek(0, 1);

    for I := 1 to Header.FilePacked do
    begin
      if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
      begin
        if AppInterface.Methods.Tick then Break;
      end;
      Inc(AppInterface.OnTick.Data.ProcessedSize);
      SrcFile.Read(Symbol, 1);
      Stream.Write(Symbol, 1);
    end;

    AppInterface.Methods.Synchronize(AppInterface.OnClear.Method);
  end else
  begin
    AppInterface.OnError.Data.Msg := ('Error: stream  not found');
    AppInterface.Methods.Synchronize(AppInterface.OnError.Method);
  end;

  Result := True;
end;

/// TDecoder

constructor TDecoder.Create(aStream: TFileReader; const aAppInterface: TAppInterface);
begin
  Stream := aStream;
  SecondaryCodec := TSecondaryDecoder.Create(Stream);
  PPM := TBaseCoder.Create(SecondaryCodec);
  AppInterface := aAppInterface;
end;

destructor TDecoder.Destroy;
begin
  PPM.Free;
  SecondaryCodec.Free;
  AppInterface := nil;
end;

function TDecoder.GetKey(Header: THeader): string;
begin
  AppInterface.OnKey.Data.FileName := ExtractFileName(Header.FileName);
  AppInterface.OnKey.Data.FilePath := ExtractFilePath(Header.FileName);
  AppInterface.OnKey.Data.FileSize := Header.FileSize;
  AppInterface.OnKey.Data.FileTime := Header.FileTime;

  AppInterface.Methods.Synchronize(AppInterface.OnKey.Method);
  Result := AppInterface.OnKey.Answer;
end;

function TDecoder.DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
var
  DstFile: TFileWriter;
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
    pmSkip: AppInterface.OnDisplay.Data.Msg := msgSkipping   + Header.FileName;
    pmTest: AppInterface.OnDisplay.Data.Msg := msgTesting    + Header.FileName;
    pmNorm: AppInterface.OnDisplay.Data.Msg := msgExtracting + Header.FileName;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  AppInterface.Methods.Synchronize(AppInterface.OnDisplay.Method);

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
      Stream.BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.FileFlags then
    begin
      for I := 1 to Header.FileSize do
      begin
        if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if AppInterface.Methods.Tick then Break;
        end;
        Inc(AppInterface.OnTick.Data.ProcessedSize);
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.FileSize do
      begin
        if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if AppInterface.Methods.Tick then Break;
        end;
        Inc(AppInterface.OnTick.Data.ProcessedSize);
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
      FileSetDate(DstFile.Handle, Header.FileTime);
    end;
    DstFile.Free;

    if Mode = pmNorm then
      FileSetAttr(Header.FileName, Header.FileAttr);

    AppInterface.Methods.Synchronize(AppInterface.OnClear.Method);
  end;

  Result := Header.FileCrc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      AppInterface.OnError.Data.Msg := ('Error: can''t open file ' + Header.FileName)
    else
      AppInterface.OnError.Data.Msg := msgCRCERROR + Header.FileName;
    AppInterface.Methods.Synchronize(AppInterface.OnError.Method);
  end;
end;

function TDecoder.DecodeStrm(Header: THeader; Mode: TExtractingMode; DstStrm: TFileWriter): boolean;
var
  DstFile: TFileWriter;
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
    pmSkip: AppInterface.OnDisplay.Data.Msg := msgSkipping + Header.FileName;
    pmTest: AppInterface.OnDisplay.Data.Msg := msgTesting  + Header.FileName;
    pmNorm: AppInterface.OnDisplay.Data.Msg := msgDecoding + Header.FileName;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  AppInterface.Methods.Synchronize(AppInterface.OnDisplay.Method);

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
      Stream.BlowFish.Start(GetKey(Header));
      if Header.Action = toSwap then
      begin
        DstFile.BlowFish.Start(AppInterface.OnKey.Answer);
      end;
    end;

    if foMoved in Header.FileFlags then
    begin
      for I := 1 to Header.FileSize do
      begin
        if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if AppInterface.Methods.Tick then Break;
        end;
        Inc(AppInterface.OnTick.Data.ProcessedSize);
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.FileSize do
      begin
        if AppInterface.OnTick.Data.ProcessedSize and $FFFF = 0 then
        begin
          if AppInterface.Methods.Tick then Break;
        end;
        Inc(AppInterface.OnTick.Data.ProcessedSize);
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

    AppInterface.Methods.Synchronize(AppInterface.OnClear.Method);
  end;

  Result := Header.FileCrc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      AppInterface.OnError.Data.Msg := ('Error: stream not found')
    else
      AppInterface.OnError.Data.Msg := msgCRCERROR + Header.FileName;
    AppInterface.Methods.Synchronize(AppInterface.OnError.Method);
  end;
end;

end.
