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

  v0.7.9 build 0980 - 2009.03.11 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,       // TStream...
  DateUtils,

  Bee_Files,     // TFileReader, TFileWriter...
  Bee_Codec,     // TSecondaryEncoder, TSecondaryDecoder...
  Bee_Headers,
  Bee_Modeller,  // TBaseCoder...
  Bee_Interface; // TApp...

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
    constructor Create(aStream: TFileWriter; aApp: TApp);
    destructor Destroy; override;
    function EncodeFile(Header: THeader; Mode: TEncodingMode): boolean;
    function EncodeStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TFileReader): boolean;
    function CopyStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TFileReader): boolean;
  private
    function GetKey(Header: THeader): string;
    procedure Tick;
  private
    App: TApp;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Stream: TFileWriter;
  end;

type

  // Decoder ...

  TDecoder = class
  public
    constructor Create(aStream: TFileReader; aApp: TApp);
    destructor Destroy; override;
    function DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
    function DecodeStrm(Header: THeader; Mode: TExtractingMode; DstStrm: TFileWriter): boolean;
  private
    function GetKey(Header: THeader): string;
    procedure Tick;
  private
    App: TApp;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Stream: TFileReader;
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

constructor TEncoder.Create(aStream: TFileWriter; aApp: TApp);
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
  App := nil;
end;

function TEncoder.GetKey(Header: THeader): string;
var
  FileInfo: TFileInfoRec;
begin
  with FileInfo do
  begin
    FileName := ExtractFileName(Header.Data.FileName);
    FilePath := ExtractFilePath(Header.Data.FileName);
    FileSize := Header.Data.FileSize;
    FileTime := Header.Data.FileTime;
    FileAttr := Header.Data.FileAttr;
  end;
  Result := App.ProcessKey(FileInfo, '');

  if Length(Result) < MinKeyLength then
  begin
    Exclude(Header.Data.FileFlags, foPassword);
  end;
end;

procedure TEncoder.Tick;
begin
  while App.Suspended do
  begin
    Sleep(250);
  end;
  App.ProcessTick;
end;

function TEncoder.EncodeFile(Header: THeader; Mode: TEncodingMode): boolean;
var
  SrcFile: TFileReader;
  Symbol: byte;
  I: cardinal;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then
    PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  Header.Data.FileStartPos := Stream.Seek(0, 1); // stream flush
  Header.Data.FileCrc      := cardinal(-1);

  SrcFile := CreateTFileReader(Header.FileLink, fmOpenRead + fmShareDenyWrite);
  if (SrcFile <> nil) then
  begin
    if Mode = emNorm then
    begin
      App.ProcessMessage(msgUpdating + Header.Data.FileName);
    end;

    Header.Data.FileSize := SrcFile.Size;
    Header.Data.FileAttr := FileGetAttr(Header.FileLink);

    if foPassword in Header.Data.FileFlags then
    begin
      Stream.BlowFish.Start(GetKey(Header));
    end;

    if foMoved in Header.Data.FileFlags then
    begin
      for I := 1 to Header.Data.FileSize do
      begin
        if App.ProcessedSize and $FFFF = 0 then
        begin
          if not App.Terminated then
            Tick
          else
            Break;
        end;
        App.IncProcessedSize;

        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Data.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.Data.FileSize do
      begin
        if App.ProcessedSize and $FFFF = 0 then
        begin
          if not App.Terminated then
            Tick
          else
            Break;
        end;
        App.IncProcessedSize;

        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Data.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    App.ProcessClear;

    SrcFile.Free;
    // last stream flush
    Header.Data.FilePacked := Stream.Seek(0, 1) - Header.Data.FileStartPos;
    // finish after last stream flush
    Stream.BlowFish.Finish;
  end else
    App.ProcessError('Error: can''t open file ' + Header.Data.FileName, 1);

  if (not (foMoved in Header.Data.FileFlags)) and (Header.Data.FilePacked >= Header.Data.FileSize) then
  begin
    Include(Header.Data.FileFlags, foTear);
    Include(Header.Data.FileFlags, foMoved);
    Stream.Size := Header.Data.FileStartPos;

    App.DecProcessedSize(Header.Data.FileSize);
    Result := EncodeFile(Header, emOpt);
  end else
    Result := True;
end;

function TEncoder.EncodeStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TFileReader): boolean;
var
  SrcFile: TFileReader;
  SrcPosition: cardinal;
  Symbol: byte;
  I: cardinal;
  Key: string;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then
    PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  SrcPosition := 0;
  SrcFile := SrcStrm;
  if (SrcFile <> nil) then
  begin
    if Mode = emNorm then
    begin
      App.ProcessMessage(msgEncoding + Header.Data.FileName);
    end;

    SrcPosition := Header.Data.FileStartPos;
    SrcFile.Seek(Header.Data.FileStartPos, 0);

    Header.Data.FileStartPos := Stream.Seek(0, 1); // stream flush
    Header.Data.FileCrc      := cardinal(-1);

    if foPassword in Header.Data.FileFlags then
    begin
      Key := GetKey(Header);
      Stream.BlowFish.Start(Key);
      if Header.Action = toSwap then
      begin
        SrcFile.BlowFish.Start(Key);
      end;
    end;

    if foMoved in Header.Data.FileFlags then
    begin
      for I := 1 to Header.Data.FileSize do
      begin
        if App.ProcessedSize and $FFFF = 0 then
        begin
          if not App.Terminated then
            Tick
          else
            Break;
        end;
        App.IncProcessedSize;

        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Data.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.Data.FileSize do
      begin
        if App.ProcessedSize and $FFFF = 0 then
        begin
          if not App.Terminated then
            Tick
          else
            Break;
        end;
        App.IncProcessedSize;

        SrcFile.Read(Symbol, 1);
        UpdCrc32(Header.Data.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
      end;
      SecondaryCodec.Flush;
    end;
    App.ProcessClear;

    SrcFile.BlowFish.Finish;
    // last stream flush
    Header.Data.FilePacked := Stream.Seek(0, 1) - Header.Data.FileStartPos;
    // finish after last stream flush
    Stream.BlowFish.Finish;
  end else
    App.ProcessError('Error: stream  not found', 1);

  if (not (foMoved in Header.Data.FileFlags)) and (Header.Data.FilePacked >= Header.Data.FileSize) then
  begin
    Include(Header.Data.FileFlags, foTear);
    Include(Header.Data.FileFlags, foMoved);
    Stream.Size := Header.Data.FileStartPos;
    Header.Data.FileStartPos := SrcPosition;

    App.DecProcessedSize(Header.Data.FileSize);
    Result := EncodeStrm(Header, emOpt, SrcStrm);
  end else
    Result := True;
end;

function TEncoder.CopyStrm(Header: THeader; Mode: TEncodingMode; SrcStrm: TFileReader): boolean;
var
  SrcFile: TFileReader;
  Symbol: byte;
  I: cardinal;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then
    PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  SrcFile := SrcStrm;
  if (SrcFile <> nil) then
  begin
    if Mode = emNorm then
    begin
      App.ProcessMessage(msgCopying + Header.Data.FileName);
    end;

    SrcFile.Seek(Header.Data.FileStartPos, 0);
    Header.Data.FileStartPos := Stream.Seek(0, 1);

    for I := 1 to Header.Data.FilePacked do
    begin
      if App.ProcessedSize and $FFFF = 0 then
      begin
        if not App.Terminated then
          Tick
        else
          Break;
      end;
      App.IncProcessedSize;

      SrcFile.Read(Symbol, 1);
      Stream.Write(Symbol, 1);
    end;
    App.ProcessClear;

  end else
    App.ProcessError('Error: stream  not found', 1);

  Result := True;
end;

/// TDecoder

constructor TDecoder.Create(aStream: TFileReader; aApp: TApp);
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
  App := nil;
end;

function TDecoder.GetKey(Header: THeader): string;
var
  FileInfo: TFileInfoRec;
begin
  with FileInfo do
  begin
    FileName := ExtractFileName(Header.Data.FileName);
    FilePath := ExtractFilePath(Header.Data.FileName);
    FileSize := Header.Data.FileSize;
    FileTime := Header.Data.FileTime;
    FileAttr := Header.Data.FileAttr;
  end;
  Result := App.ProcessKey(FileInfo, '');
end;

procedure TDecoder.Tick;
begin
  while App.Suspended do
  begin
    Sleep(250);
  end;
  App.ProcessTick;
end;

function TDecoder.DecodeFile(Header: THeader; Mode: TExtractingMode): boolean;
var
  DstFile: TFileWriter;
  I, Crc:  cardinal;
  Symbol:  byte;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then
    PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: App.ProcessMessage(msgSkipping   + Header.Data.FileName);
    pmTest: App.ProcessMessage(msgTesting    + Header.Data.FileName);
    pmNorm: App.ProcessMessage(msgExtracting + Header.Data.FileName);
    pmQuit:
    begin
      Result := True; Exit;
    end;
  end;

  Stream.Seek(Header.Data.FileStartPos, 0); // stream flush
  Crc := cardinal(-1);

  if Mode = pmNorm then
    DstFile := CreateTFileWriter(Header.Data.FileName, fmCreate)
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
        if App.ProcessedSize and $FFFF = 0 then
        begin
          if not App.Terminated then
            Tick
          else
            Break;
        end;
        App.IncProcessedSize;

        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.Data.FileSize do
      begin
        if App.ProcessedSize and $FFFF = 0 then
        begin
          if not App.Terminated then
            Tick
          else
            Break;
        end;
        App.IncProcessedSize;

        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
      SecondaryCodec.Flush;
    end;
    App.ProcessClear;

    Stream.BlowFish.Finish;
    if Mode = pmNorm then
    begin
      DstFile.Flush;  FileSetDate(DstFile.Handle, Header.Data.FileTime);
    end;
    DstFile.Free;
    if Mode = pmNorm then
    begin
      FileSetAttr(Header.Data.FileName, Header.Data.FileAttr);
    end;
  end;

  Result := Header.Data.FileCrc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      App.ProcessError('Error: can''t open file ' + Header.Data.FileName, 1)
    else
      App.ProcessError(msgCRCERROR + Header.Data.FileName, 1);
  end;
end;

function TDecoder.DecodeStrm(Header: THeader; Mode: TExtractingMode; DstStrm: TFileWriter): boolean;
var
  DstFile: TFileWriter;
  I, Crc:  cardinal;
  Symbol:  byte;
  Key: string;
begin
  if foDictionary in Header.Data.FileFlags then
    PPM.SetDictionary(Header.Data.FileDictionary);

  if foTable in Header.Data.FileFlags then
    PPM.SetTable(Header.Data.FileTable);

  if foTear in Header.Data.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: App.ProcessMessage(msgSkipping + Header.Data.FileName);
    pmTest: App.ProcessMessage(msgTesting  + Header.Data.FileName);
    pmNorm: App.ProcessMessage(msgDecoding + Header.Data.FileName);
    pmQuit:
    begin
      Result := True; Exit;
    end;
  end;

  Stream.Seek(Header.Data.FileStartPos, 0);
  Crc := cardinal(-1);

  if Mode = pmNorm then
  begin
    DstFile := DstStrm;  Header.Data.FileStartPos := DstFile.Seek(0, 1);
  end else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin
    if foPassword in Header.Data.FileFlags then
    begin
      Key := GetKey(Header);
      Stream.BlowFish.Start(Key);
      if Header.Action = toSwap then
      begin
        DstFile.BlowFish.Start(Key);
      end;
    end;

    if foMoved in Header.Data.FileFlags then
    begin
      for I := 1 to Header.Data.FileSize do
      begin
        if App.ProcessedSize and $FFFF = 0 then
        begin
          if not App.Terminated then
            Tick
          else
            Break;
        end;
        App.IncProcessedSize;

        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
    end else
    begin
      SecondaryCodec.Start;
      for I := 1 to Header.Data.FileSize do
      begin
        if App.ProcessedSize and $FFFF = 0 then
        begin
          if not App.Terminated then
            Tick
          else
            Break;
        end;
        App.IncProcessedSize;

        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
      end;
      SecondaryCodec.Flush;
    end;
    App.ProcessClear;

    Stream.BlowFish.Finish;
    // last stream flush
    if Mode = pmNorm then DstFile.Flush;
    // finish after last stream flush
    DstFile.BlowFish.Finish;
  end;

  Result := Header.Data.FileCrc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      App.ProcessError('Error: stream not found', 1)
    else
      App.ProcessError(msgCRCERROR + Header.Data.FileName, 1);
  end;
end;

end.
