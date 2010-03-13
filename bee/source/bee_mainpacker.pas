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

    TEncoder class, file encoder;
    TDecoder class, file decoder;

  Modifyed:

    v0.7.8 build 0148 - 2005.06.23 by Melchiorre Caruso;
    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;

    v0.8.0 build 1100 - 2010.01.23 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Classes,
  Bee_Types,
  Bee_Files,
  Bee_Codec,
  Bee_Consts,
  Bee_Headers,
  Bee_Modeller,
  Bee_Interface;

type
  { Extracting Modes:                                   }
  {   pmNorm  Extract files                             }
  {   pmSkip  Extract files, but skip current           }
  {   pmTest  Test files (Extract to nul)               }
  {   pmQuit  Cancel extracting                         }

  TExtractingMode = (pmNorm, pmSkip, pmTest, pmQuit);

  { Encoding Modes:                                     }
  {   emNorm  Encode files                              }
  {   emOpt   Encode files to nul, with no messages     }

  TEncodingMode = (emNorm, emOpt);

  { Encoder class }

  TEncoder = class
  private
    App: TApp;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Stream: TFileWriter;
    procedure Progress;
    function GetPassword(P: THeader): string;
  public
    constructor Create(aStream: TFileWriter; aApp: TApp);
    destructor Destroy; override;
    procedure EncodeFile(P: THeader; Mode: TEncodingMode);
    procedure EncodeStrm(P: THeader; Mode: TEncodingMode; SrcStrm: TFileReader;
      const SrcSize: int64; SrcEncoded: boolean);
    procedure CopyStrm(P: THeader; Mode: TEncodingMode; SrcStrm: TFileReader;
      const SrcStartPos: int64; const SrcSize: int64; SrcEncoded: boolean);
  end;

  { Decoder class }

  TDecoder = class
  private
    App: TApp;
    PPM: TBaseCoder;
    SecondaryCodec: TSecondaryCodec;
    Stream: TFileReader;
    procedure Progress;
    function GetPassword(P: THeader): string;
  public
    constructor Create(aStream: TFileReader; aApp: TApp);
    destructor Destroy; override;
    procedure DecodeFile(P: THeader; Mode: TExtractingMode);
    procedure DecodeStrm(P: THeader; Mode: TExtractingMode; DstStrm: TFileWriter;
      const DstSize: int64; DstEncoded: boolean);
  end;

implementation

uses
  SysUtils,
  Bee_Crc,
  Bee_Common,
  Bee_BlowFish;

{ TEncoder class }

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
  Stream := nil;
  App := nil;
end;

function TEncoder.GetPassword(P: THeader): string;
var
  FI: TFileInfo;
begin
  FI.FileName := StringToPChar(ExtractFileName(P.FileName));
  FI.FilePath := StringToPChar(ExtractFilePath(P.FileName));

  FI.FileSize := P.FileSize;
  FI.FileTime := P.FileTime;
  FI.FileAttr := P.FileAttr;

  Result := App.DoPassword(FI, '');

  FreePChar(FI.FileName);
  FreePChar(FI.FileName);

  if Length(Result) < MinKeyLength then
    Exclude(P.FileFlags, foPassword);
end;

procedure TEncoder.Progress;
begin
  while App.Suspended do Sleep(250);
  App.DoProgress;
end;

procedure TEncoder.EncodeFile(P: THeader; Mode: TEncodingMode);
var
  SrcFile: TFileReader;
  Symbol:  byte;
begin
  if foDictionary in P.FileFlags then
    PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then
    PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    emNorm: App.DoMessage(msgUpdating + P.FileName);
  end;

  P.FileStartPos := Stream.Seek(0, 1);
  P.FileCrc      := longword(-1);

  SrcFile := CreateTFileReader(P.FileLink, fmOpenRead + fmShareDenyWrite);
  if (SrcFile <> nil) then
  begin
    if foPassword in P.FileFlags then
      Stream.BlowFish.Start(GetPassword(P));

    P.FileSize := SrcFile.Size;
    P.FileAttr := FileGetAttr(P.FileLink);

    if foMoved in P.FileFlags then
    begin
      while SrcFile.Read(Symbol, 1) = 1 do
      begin
        UpdCrc32(P.FileCrc, Symbol);
        Stream.Write(Symbol, 1);

        if App.Size and $FFFF = 0 then
        begin
          if App.Terminated = False then
            Progress
          else
            Break;
        end;
        App.IncSize;
      end;
    end
    else
    begin
      SecondaryCodec.Start;
      while SrcFile.Read(Symbol, 1) = 1 do
      begin
        UpdCrc32(P.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);

        if App.Size and $FFFF = 0 then
        begin
          if App.Terminated = False then
            Progress
          else
            Break;
        end;
        App.IncSize;
      end;
      SecondaryCodec.Flush;
    end;
    App.DoClearLine;
    SrcFile.Free;

    P.FilePacked := Stream.Seek(0, 1) - P.FileStartPos; // stream flush
    Stream.BlowFish.Finish;                // finish after stream flush

    if (not (foMoved in P.FileFlags)) and (P.FilePacked > P.FileSize) then
    begin
      Include(P.FileFlags, foTear);
      Include(P.FileFlags, foMoved);

      Stream.Size := P.FileStartPos;
      App.DecSize(P.FileSize);

      EncodeFile(P, emOpt);
    end;

  end else
    App.DoError('Error: can''t open file "' + P.FileLink + '"', ccError);
end;

procedure TEncoder.EncodeStrm(P: THeader; Mode: TEncodingMode;
  SrcStrm: TFileReader; const SrcSize: int64; SrcEncoded: boolean);
var
  Symbol: byte;
  Password: string;
  SrcPosition: int64;
  I: int64;
begin
  if foDictionary in P.FileFlags then
    PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then
    PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    emNorm: App.DoMessage(msgEncoding + P.FileName);
  end;

  P.FileStartPos := Stream.Seek(0, 1);
  P.FileCrc      := longword(-1);

  if (SrcStrm <> nil) then
  begin
    if foPassword in P.FileFlags then
    begin
      Password := GetPassword(P);
      Stream.BlowFish.Start(Password);
      if SrcEncoded then
        SrcStrm.BlowFish.Start(Password);
    end;

    SrcPosition := SrcStrm.Seek(0, 1);

    I := 0;
    if foMoved in P.FileFlags then
    begin
      while I < SrcSize do
      begin
        SrcStrm.Read(Symbol, 1);
        UpdCrc32(P.FileCrc, Symbol);
        Stream.Write(Symbol, 1);

        if App.Size and $FFFF = 0 then
        begin
          if App.Terminated = False then
            Progress
          else
            Break;
        end;
        App.IncSize;
        Inc(I);
      end;
    end
    else
    begin
      SecondaryCodec.Start;
      while I < SrcSize do
      begin
        SrcStrm.Read(Symbol, 1);
        UpdCrc32(P.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);

        if App.Size and $FFFF = 0 then
        begin
          if App.Terminated = False then
            Progress
          else
            Break;
        end;
        App.IncSize;
        Inc(I);
      end;
      SecondaryCodec.Flush;
    end;
    App.DoClearLine;
    SrcStrm.BlowFish.Finish;

    P.FilePacked := Stream.Seek(0, 1) - P.FileStartPos; // stream flush
    Stream.BlowFish.Finish;                // finish after stream flush

    if (not (foMoved in P.FileFlags)) and (P.FilePacked > P.FileSize) then
    begin
      Include(P.FileFlags, foTear);
      Include(P.FileFlags, foMoved);

      Stream.Size := P.FileStartPos;
      SrcStrm.Seek(SrcPosition, 0);
      App.DecSize(P.FileSize);

      EncodeStrm(P, emOpt, SrcStrm, SrcSize, SrcEncoded);
    end;

  end else
    App.DoError('Error: stream  not found', ccError);
end;

procedure TEncoder.CopyStrm(P: THeader; Mode: TEncodingMode; SrcStrm: TFileReader;
  const SrcStartPos: int64; const SrcSize: int64; SrcEncoded: boolean);
var
  Symbol: byte;
  I:      int64;
begin
  if foDictionary in P.FileFlags then
    PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then
    PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    emNorm: App.DoMessage(msgCopying + P.FileName);
  end;

  P.FileStartPos := Stream.Seek(0, 1);

  if (SrcStrm <> nil) then
  begin
    if SrcEncoded then
      SrcStrm.BlowFish.Start(GetPassword(P));

    SrcStrm.Seek(SrcStartPos, 0);

    I := 0;
    while I < SrcSize do
    begin
      SrcStrm.Read(Symbol, 1);
      Stream.Write(Symbol, 1);

      if App.Size and $FFFF = 0 then
      begin
        if App.Terminated = False then
          Progress
        else
          Break;
      end;
      App.IncSize;
      Inc(I);
    end;
    App.DoClearLine;
    SrcStrm.BlowFish.Finish;

  end else
    App.DoError('Error: stream  not found', ccError);
end;

{ TDecoder class }

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
  Stream := nil;
  App := nil;
end;

function TDecoder.GetPassword(P: THeader): string;
var
  FI: TFileInfo;
begin
  FI.FileName := StringToPChar(ExtractFileName(P.FileName));
  FI.FilePath := StringToPChar(ExtractFilePath(P.FileName));

  FI.FileSize := P.FileSize;
  FI.FileTime := P.FileTime;
  FI.FileAttr := P.FileAttr;

  Result := App.DoPassword(FI, '');

  FreePChar(FI.FileName);
  FreePChar(FI.FileName);
end;

procedure TDecoder.Progress;
begin
  while App.Suspended do
    Sleep(250);
  App.DoProgress;
end;

procedure TDecoder.DecodeFile(P: THeader; Mode: TExtractingMode);
var
  DstFile: TFileWriter;
  Symbol: byte;
  Crc: longword;
  I: int64;
begin
  if foDictionary in P.FileFlags then
    PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then
    PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: App.DoMessage(msgSkipping + P.FileName);
    pmTest: App.DoMessage(msgTesting + P.FileName);
    pmNorm: App.DoMessage(msgExtracting + P.FileName);
    pmQuit: Exit;
  end;

  Stream.Seek(P.FileStartPos, 0);
  Crc := longword(-1);

  if Mode = pmNorm then
    DstFile := CreateTFileWriter(P.FileName, fmCreate)
  else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin
    if foPassword in P.FileFlags then
      Stream.BlowFish.Start(GetPassword(P));

    I := 0;
    if foMoved in P.FileFlags then
    begin
      while I < P.FileSize do
      begin
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);

        if App.Size and $FFFF = 0 then
        begin
          if App.Terminated = False then
            Progress
          else
            Break;
        end;
        App.IncSize;
        Inc(I);
      end;
    end
    else
    begin
      SecondaryCodec.Start;
      while I < P.FileSize do
      begin
        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);

        if App.Size and $FFFF = 0 then
        begin
          if App.Terminated = False then
            Progress
          else
            Break;
        end;
        App.IncSize;
        Inc(I);
      end;
      SecondaryCodec.Flush;
    end;
    App.DoClearLine;
    Stream.BlowFish.Finish;

    if Mode = pmNorm then
    begin
      DstFile.Flush;
      FileSetDate(DstFile.Handle, P.FileTime);
    end;
    DstFile.Free;
    if Mode = pmNorm then
      FileSetAttr(P.FileName, P.FileAttr);
  end;

  if Crc = longword(-1) then
    App.DoError('Error: can''t open file ' + P.FileName, ccError)
  else
    if Crc <> P.FileCrc then
      App.DoError(msgCRCERROR + P.FileName, ccError);
end;

procedure TDecoder.DecodeStrm(P: THeader; Mode: TExtractingMode;
  DstStrm: TFileWriter; const DstSize: int64; DstEncoded: boolean);
var
  DstFile: TFileWriter;
  Password: string;
  Symbol: byte;
  Crc: longword;
  I: int64;
begin
  if foDictionary in P.FileFlags then
    PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then
    PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmSkip: App.DoMessage(msgSkipping + P.FileName);
    pmTest: App.DoMessage(msgTesting + P.FileName);
    pmNorm: App.DoMessage(msgDecoding + P.FileName);
    pmQuit: Exit;
  end;

  Stream.Seek(P.FileStartPos, 0);
  Crc := longword(-1);

  if Mode = pmNorm then
    DstFile := DstStrm
  else
    DstFile := TNulWriter.Create;

  if (DstFile <> nil) then
  begin
    if foPassword in P.FileFlags then
    begin
      Password := GetPassword(P);
      Stream.BlowFish.Start(Password);
      if DstEncoded then
        DstFile.BlowFish.Start(Password);
    end;

    I := 0;
    if foMoved in P.FileFlags then
    begin
      while I < DstSize do
      begin
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);

        if App.Size and $FFFF = 0 then
        begin
          if App.Terminated = False then
            Progress
          else
            Break;
        end;
        App.IncSize;
        Inc(I);
      end;
    end
    else
    begin
      SecondaryCodec.Start;
      while I < DstSize do
      begin
        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);

        if App.Size and $FFFF = 0 then
        begin
          if not App.Terminated then
            Progress
          else
            Break;
        end;
        App.IncSize;
        Inc(I);
      end;
      SecondaryCodec.Flush;
    end;
    App.DoClearLine;
    Stream.BlowFish.Finish;

    if Mode = pmNorm then
      DstFile.Flush;  // stream flush
    DstFile.BlowFish.Finish; // finish after stream flush
  end;

  if Crc = longword(-1) then
    App.DoError('Error: stream not found', 1)
  else
    if Crc <> P.FileCrc then
      App.DoError(msgCRCERROR + P.FileName, 1);
end;

end.
