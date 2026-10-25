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

  v0.7.9 build 0553 - 2007.12.15 by Melchiorre Caruso.
  v0.7.9 build 0602 - 2026.10.25 by Melchiorre Caruso.
}

unit BeeMainPacker;

{$MODE OBJFPC}{$H+}
{$R-}{$Q-}{$S-}{$I-}

interface

uses
  Classes,      // TStream

  BeeApp,
  BeeCodec,     // TSecondaryEncoder, TSecondaryDecoder...
  BeeHeaders,
  BeeModeller,  // TBaseCoder...
  BeeMixer,     // TBeeMixer
  BeeInterface;

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
    procedure EncodeData(SrcFile: TStream; Header: THeader);
    procedure EncodeByte(Value: byte); inline;
    procedure SyncMixer(Header: THeader);
  private
    App: TBeeApp;
    Stream: TStream;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Mixer: TBeeMixer;     // created with the first member coded by it
    UseMixer: boolean;    // the current member is coded by the mixer
    Dictionary: cardinal; // dictionary level of the model
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
    procedure DecodeData(Header: THeader; DstFile: TStream; var Crc: cardinal);
    function DecodeByte: byte; inline;
    procedure SyncMixer(Header: THeader);
  private
    App: TBeeApp;
    Stream: TStream;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Mixer: TBeeMixer;     // created with the first member coded by it
    UseMixer: boolean;    // the current member is coded by the mixer
    Dictionary: cardinal; // dictionary level of the model
  end;

implementation

uses
  SysUtils,     // FileSetAttr...

  BeeCrc,       // UpdCrc32...
  BeeFiles,     // TBufferizedReader
  BeeCommon,    // Diag...
  BeeBlowFish,
  BeeExeFilter;

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
  Mixer.Free;
  PPM.Free;
  SecondaryCodec.Free;
end;

// Keeps the mixer in step with the PPM model: the same dictionary level and
// a restart wherever the model restarts. Encoder and decoder call it at the
// same members, so their mixers stay identical.

procedure TEncoder.SyncMixer(Header: THeader);
begin
  if foDictionary in Header.Flags then
    Dictionary := Header.Dictionary;
  if Mixer <> nil then
    if foDictionary in Header.Flags then
      Mixer.SetDictionaryLevel(Dictionary)
    else
      if foTear in Header.Flags then Mixer.Reset;
end;

procedure TEncoder.EncodeByte(Value: byte);
begin
  if UseMixer then
    Mixer.EncodeByte(Value, SecondaryCodec)
  else
    PPM.UpdateModel(Value);
end;

function TEncoder.GetKey(Header: THeader): string;
begin
  App.AppInterface^.OnKey.Data.FileName := ExtractFileName(Header.Name);
  App.AppInterface^.OnKey.Data.FilePath := ExtractFilePath(Header.Name);
  App.AppInterface^.OnKey.Data.FileSize := Header.Size;
  App.AppInterface^.OnKey.Data.FileTime := Header.Time;

  App.Sync(App.AppInterface^.OnKey.Method);
  Result := App.AppInterface^.OnKey.Answer;
  
  if Length(Result) < MinKeyLength then
  begin
    Exclude(Header.Flags, foPassword);
  end;
end;

// Encodes Header.Size bytes of SrcFile. With the exe filter enabled an
// executable is coded as its filtered image, preceded by its length; with
// the mixer enabled every byte is coded by the mixer instead of the plain
// PPM model.

procedure TEncoder.EncodeData(SrcFile: TStream; Header: THeader);
var
  Buffer: TBytes;
  Load: TBeeExeLoad;
  Len: cardinal;
  Symbol: byte;
  I: integer;
  Stopped: boolean;
begin
  Load := elStream;
  Buffer := nil;
  if App.ExeFilter then
    Load := BeeExeLoad(SrcFile, Header.Size, Header.Crc, Buffer);

  UseMixer := App.Mixer;
  if UseMixer then
  begin
    Include(Header.Flags, foMixer);
    if Mixer = nil then
    begin
      Mixer := TBeeMixer.Create(PPM);
      Mixer.SetDictionaryLevel(Dictionary);
    end;
  end;

  SecondaryCodec.Start;
  if Load = elFiltered then
  begin
    Include(Header.Flags, foFilter);
    Len := Length(Buffer);
    for I := 0 to 3 do EncodeByte(byte(Len shr (I * 8)));
  end;

  Stopped := False;
  for I := 0 to High(Buffer) do
  begin
    if App.RemainSize and $FFFF = 0 then
    begin
      Stopped := App.Tick;
      if Stopped then Break;
    end;
    if I < Header.Size then Inc(App.RemainSize);
    EncodeByte(Buffer[I]);
  end;

  if (Load = elStream) and (not Stopped) then
    for I := Length(Buffer) + 1 to Header.Size do
    begin
      if App.RemainSize and $FFFF = 0 then
      begin
        if App.Tick then Break;
      end;
      Inc(App.RemainSize);
      SrcFile.Read(Symbol, 1);
      UpdCrc32(Header.Crc, Symbol);
      EncodeByte(Symbol);
    end;
  SecondaryCodec.Flush;
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
  SyncMixer(Header);
  Exclude(Header.Flags, foFilter);
  Exclude(Header.Flags, foMixer);

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
      App.AppInterface^.OnDisplay.Data.Msg := msgUpdating + Header.GetName;
      App.Sync(App.AppInterface^.OnDisplay.Method);
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
      EncodeData(SrcFile, Header);
    SrcFile.Free;

    Header.Pack := Stream.Seek(0, 1) - Header.StartPos; // last stream flush
    TFileWriter(Stream).BlowFish.Finish; // finish after last stream flush

    App.Sync(App.AppInterface^.OnClear.Method);
  end else
  begin
    App.AppInterface^.OnError.Data.Msg := ('Error: can''t open file ' + Header.Name);
    App.Sync(App.AppInterface^.OnError.Method);
  end;

  if (not (foMoved in Header.Flags)) and (Header.Pack >= Header.Size) then
  begin
    Include(Header.Flags, foTear);
    Include(Header.Flags, foMoved);
    Exclude(Header.Flags, foFilter);
    Exclude(Header.Flags, foMixer);
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
  SyncMixer(Header);
  Exclude(Header.Flags, foFilter);
  Exclude(Header.Flags, foMixer);

  SrcPosition := 0;
  SrcFile := SrcStrm;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      App.AppInterface^.OnDisplay.Data.Msg := msgEncoding + Header.Name;
      App.Sync(App.AppInterface^.OnDisplay.Method);
    end;

    SrcPosition := Header.StartPos;
    SrcFile.Seek(Header.StartPos, 0);

    Header.Crc := cardinal(-1);
    Header.StartPos := Stream.Seek(0, 1); // stream flush

    if foPassword in Header.Flags then
    begin
      TFileReader(SrcFile).BlowFish.Start(GetKey(Header));
      TFileWriter(Stream).BlowFish.Start(App.AppInterface^.OnKey.Answer);
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
      EncodeData(SrcFile, Header);
    TFileReader(SrcFile).BlowFish.Finish;

    Header.Pack := Stream.Seek(0, 1) - Header.StartPos; // last stream flush
    TFileWriter(Stream).BlowFish.Finish; // finish after last stream flush

    App.Sync(App.AppInterface^.OnClear.Method);
  end else
  begin
    App.AppInterface^.OnError.Data.Msg := ('Error: stream  not found');
    App.Sync(App.AppInterface^.OnError.Method);
  end;

  if (not (foMoved in Header.Flags)) and (Header.Pack >= Header.Size) then
  begin
    Include(Header.Flags, foTear);
    Include(Header.Flags, foMoved);
    Exclude(Header.Flags, foFilter);
    Exclude(Header.Flags, foMixer);
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
  SyncMixer(Header);

  SrcFile := SrcStrm;

  if (SrcFile <> nil) then
  begin

    if Mode = emNorm then
    begin
      App.AppInterface^.OnDisplay.Data.Msg := msgCopying + Header.Name;
      App.Sync(App.AppInterface^.OnDisplay.Method);
    end;

    SrcFile.Seek(Header.StartPos, 0);
    Header.StartPos := Stream.Seek(0, 1);

    for I := 1 to Header.Pack do
    begin
      if App.RemainSize and $FFFF = 0 then
      begin
        if App.Tick then Break;
      end;
      Inc(App.RemainSize);
      SrcFile.Read(Symbol, 1);
      Stream.Write(Symbol, 1);
    end;

    App.Sync(App.AppInterface^.OnClear.Method);
  end else
  begin
    App.AppInterface^.OnError.Data.Msg := ('Error: stream  not found');
    App.Sync(App.AppInterface^.OnError.Method);
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
  Mixer.Free;
  PPM.Free;
  SecondaryCodec.Free;
end;

// Keeps the mixer in step with the PPM model: the same dictionary level and
// a restart wherever the model restarts. Encoder and decoder call it at the
// same members, so their mixers stay identical.

procedure TDecoder.SyncMixer(Header: THeader);
begin
  if foDictionary in Header.Flags then
    Dictionary := Header.Dictionary;
  if Mixer <> nil then
    if foDictionary in Header.Flags then
      Mixer.SetDictionaryLevel(Dictionary)
    else
      if foTear in Header.Flags then Mixer.Reset;
end;

function TDecoder.DecodeByte: byte;
begin
  if UseMixer then
    Result := Mixer.DecodeByte(SecondaryCodec)
  else
    Result := PPM.UpdateModel(0);
end;

function TDecoder.GetKey(Header: THeader): string;
begin
  App.AppInterface^.OnKey.Data.FileName := ExtractFileName(Header.Name);
  App.AppInterface^.OnKey.Data.FilePath := ExtractFilePath(Header.Name);
  App.AppInterface^.OnKey.Data.FileSize := Header.Size;
  App.AppInterface^.OnKey.Data.FileTime := Header.Time;

  App.Sync(App.AppInterface^.OnKey.Method);
  Result := App.AppInterface^.OnKey.Answer;
end;

// Decodes a member into DstFile, updating Crc. A filtered member that does
// not restore to exactly Header.Size bytes is reported as a CRC error.

procedure TDecoder.DecodeData(Header: THeader; DstFile: TStream; var Crc: cardinal);
var
  Buffer, Data: TBytes;
  Len: cardinal;
  Symbol: byte;
  I: integer;
  Stopped: boolean;
begin
  UseMixer := foMixer in Header.Flags;
  if UseMixer and (Mixer = nil) then
  begin
    Mixer := TBeeMixer.Create(PPM);
    Mixer.SetDictionaryLevel(Dictionary);
  end;

  if not (foFilter in Header.Flags) then
  begin
    for I := 1 to Header.Size do
    begin
      if App.RemainSize and $FFFF = 0 then
      begin
        if App.Tick then Break;
      end;
      Inc(App.RemainSize);
      Symbol := DecodeByte;
      UpdCrc32(Crc, Symbol);
      DstFile.Write(Symbol, 1);
    end;
    Exit;
  end;

  Len := 0;
  for I := 0 to 3 do Len := Len or (cardinal(DecodeByte) shl (I * 8));
  if (Header.Size <= 0) or (Len > BeeExeFilteredSizeLimit(Header.Size)) then
  begin
    Crc := not Header.Crc;
    Exit;
  end;

  SetLength(Buffer, Len);
  Stopped := False;
  for I := 0 to integer(Len) - 1 do
  begin
    if App.RemainSize and $FFFF = 0 then
    begin
      Stopped := App.Tick;
      if Stopped then Break;
    end;
    if I < Header.Size then Inc(App.RemainSize);
    Buffer[I] := DecodeByte;
  end;
  if Stopped then Exit;

  if BeeExeDecode(Buffer, Data) and (Length(Data) = Header.Size) then
  begin
    Buffer := nil;
    for I := 0 to High(Data) do UpdCrc32(Crc, Data[I]);
    DstFile.Write(Data[0], Length(Data));
  end else
    Crc := not Header.Crc;
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
  SyncMixer(Header);

  case Mode of
    pmSkip: App.AppInterface^.OnDisplay.Data.Msg := msgSkipping   + Header.Name;
    pmTest: App.AppInterface^.OnDisplay.Data.Msg := msgTesting    + Header.Name;
    pmNorm: App.AppInterface^.OnDisplay.Data.Msg := msgExtracting + Header.Name;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  App.Sync(App.AppInterface^.OnDisplay.Method);

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
      DecodeData(Header, DstFile, Crc);
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

    App.Sync(App.AppInterface^.OnClear.Method);
  end;

  Result := Header.Crc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      App.AppInterface^.OnError.Data.Msg := ('Error: can''t open file ' + Header.Name)
    else
      App.AppInterface^.OnError.Data.Msg := msgCRCERROR + Header.Name;
    App.Sync(App.AppInterface^.OnError.Method);
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
  SyncMixer(Header);

  case Mode of
    pmSkip: App.AppInterface^.OnDisplay.Data.Msg := msgSkipping + Header.Name;
    pmTest: App.AppInterface^.OnDisplay.Data.Msg := msgTesting  + Header.Name;
    pmNorm: App.AppInterface^.OnDisplay.Data.Msg := msgDecoding + Header.Name;
    pmQuit:
    begin
      Result := True;
      Exit;
    end;
  end;
  App.Sync(App.AppInterface^.OnDisplay.Method);

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
      TFileWriter(DstFile).BlowFish.Start(App.AppInterface^.OnKey.Answer);
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
      DecodeData(Header, DstFile, Crc);
      SecondaryCodec.Flush;
    end;
    TFileReader(Stream).BlowFish.Finish;

    if Mode = pmNorm then
    begin
      TFileWriter(DstFile).Flush; // last stream flush
    end;
    TFileWriter(DstFile).BlowFish.Finish; // finish after last stream flush

    App.Sync(App.AppInterface^.OnClear.Method);
  end;

  Result := Header.Crc = Crc;
  if Result = False then
  begin
    if Crc = cardinal(-1) then
      App.AppInterface^.OnError.Data.Msg := ('Error: stream not found')
    else
      App.AppInterface^.OnError.Data.Msg := msgCRCERROR + Header.Name;
    App.Sync(App.AppInterface^.OnError.Method);
  end;
end;

end.
