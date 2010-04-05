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

    v0.8.0 build 1100 - 2010.03.21 by Melchiorre Caruso.
}

unit Bee_MainPacker;

{$I compiler.inc}

interface

uses
  Bee_Types,
  Bee_Files,
  Bee_Codec,
  Bee_Consts,
  Bee_Headers,
  Bee_Modeller,
  Bee_Interface;

type
  { Extracting Modes:                        }
  {   pmNorm    Extract files                }
  {   pmNul     Extract files to nul stream  }
  {   pmSkip    Skip files                   }

  TExtractingMode = (pmNorm, pmNul, pmSkip);

  { Encoding Modes:                          }
  {   emNorm  Encode files                   }
  {   emOpt   Encode files, with no messages }

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
  Classes,
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
  begin
    Exclude(P.FileFlags, foPassword);
  end;
end;

procedure TEncoder.Progress; {$IFDEF FPC} inline; {$ENDIF}
begin
  if App.Size and $FFFF = 0 then
  begin
    while App.Suspended do Sleep(250);
    App.DoProgress;
  end;
  App.IncSize;
end;

procedure TEncoder.EncodeFile(P: THeader; Mode: TEncodingMode); {$IFDEF FPC} inline; {$ENDIF}
var
  SrcFile: TFileReader;
  Symbol: byte;
begin
  if foDictionary in P.FileFlags then PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    emNorm: App.DoMessage(Format(msgUpdating, [P.FileName]));
  end;

  P.FileStartPos := Stream.Seek(0, soFromCurrent);
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
      while (App.Code < ccError) and (SrcFile.Read(Symbol, 1) = 1) do
      begin
        UpdCrc32(P.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
        Progress;
      end;
    end else
    begin
      SecondaryCodec.Start;
      while (App.Code < ccError) and (SrcFile.Read(Symbol, 1) = 1) do
      begin
        UpdCrc32(P.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
        Progress;
      end;
      SecondaryCodec.Flush;
    end;
    App.DoClearLine;
    SrcFile.Free;

    P.FilePacked := Stream.Seek(0, soFromCurrent) - P.FileStartPos;
    Stream.BlowFish.Finish;

    if (not (foMoved in P.FileFlags)) and (P.FilePacked > P.FileSize) then
    begin
      Include(P.FileFlags, foTear);
      Include(P.FileFlags, foMoved);
      Stream.Size := P.FileStartPos;
      App.DecSize(P.FileSize);
      EncodeFile(P, emOpt);
    end;

  end else
    App.DoMessage(Format(cmFileOpenError, [P.FileLink]), ccError);
end;

procedure TEncoder.EncodeStrm(P: THeader; Mode: TEncodingMode;
  SrcStrm: TFileReader; const SrcSize: int64; SrcEncoded: boolean); {$IFDEF FPC} inline; {$ENDIF}
var
  Symbol: byte;
  Password: string;
  SrcPosition: int64;
  I: int64;
begin
  if foDictionary in P.FileFlags then PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    emNorm: App.DoMessage(Format(msgEncoding, [P.FileName]));
  end;

  P.FileStartPos := Stream.Seek(0, soFromCurrent);
  P.FileCrc      := longword(-1);

  if (SrcStrm <> nil) then
  begin
    if foPassword in P.FileFlags then
    begin
      Password := GetPassword(P);
      Stream.BlowFish.Start(Password);
      if SrcEncoded then
      begin
        SrcStrm.BlowFish.Start(Password);
      end;
    end;

    SrcPosition := SrcStrm.Seek(0, soFromCurrent);

    I := 0;
    if foMoved in P.FileFlags then
    begin
      while (App.Code < ccError) and (I < SrcSize) do
      begin
        SrcStrm.Read(Symbol, 1);
        UpdCrc32(P.FileCrc, Symbol);
        Stream.Write(Symbol, 1);
        Progress;
        Inc(I);
      end;
    end else
    begin
      SecondaryCodec.Start;
      while I < SrcSize do
      begin
        SrcStrm.Read(Symbol, 1);
        UpdCrc32(P.FileCrc, Symbol);
        PPM.UpdateModel(Symbol);
        Progress;
        Inc(I);
      end;
      SecondaryCodec.Flush;
    end;
    App.DoClearLine;
    SrcStrm.BlowFish.Finish;

    P.FilePacked := Stream.Seek(0, soFromCurrent) - P.FileStartPos; // stream flush
    Stream.BlowFish.Finish;                                         // finish after stream flush

    if (not (foMoved in P.FileFlags)) and (P.FilePacked > P.FileSize) then
    begin
      Include(P.FileFlags, foTear);
      Include(P.FileFlags, foMoved);
      Stream.Size := P.FileStartPos;
      SrcStrm.Seek(SrcPosition, soFromBeginning);
      App.DecSize(P.FileSize);
      EncodeStrm(P, emOpt, SrcStrm, SrcSize, SrcEncoded);
    end;

  end else
    App.DoMessage(cmStrmReadError, ccError);
end;

procedure TEncoder.CopyStrm(P: THeader; Mode: TEncodingMode; SrcStrm: TFileReader;
  const SrcStartPos: int64; const SrcSize: int64; SrcEncoded: boolean); {$IFDEF FPC} inline; {$ENDIF}
var
  Symbol: byte;
  I:      int64;
begin
  if foDictionary in P.FileFlags then PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    emNorm: App.DoMessage(Format(msgCopying, [P.FileName]));
  end;

  P.FileStartPos := Stream.Seek(0, soFromCurrent);

  if (SrcStrm <> nil) then
  begin
    if SrcEncoded then
      SrcStrm.BlowFish.Start(GetPassword(P));

    SrcStrm.Seek(SrcStartPos, soFromBeginning);

    I := 0;
    while (App.Code < ccError) and (I < SrcSize) do
    begin
      SrcStrm.Read(Symbol, 1);
      Stream.Write(Symbol, 1);
      Progress;
      Inc(I);
    end;
    App.DoClearLine;
    SrcStrm.BlowFish.Finish;

  end else
    App.DoMessage(cmStrmReadError, ccError);
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

procedure TDecoder.Progress; {$IFDEF FPC} inline; {$ENDIF}
begin
  if App.Size and $FFFF = 0 then
  begin
    while App.Suspended do Sleep(250);
    App.DoProgress;
  end;
  App.IncSize;
end;

procedure TDecoder.DecodeFile(P: THeader; Mode: TExtractingMode); {$IFDEF FPC} inline; {$ENDIF}
var
  DstFile: TFileWriter;
  Symbol: byte;
  Crc: longword;
  I: int64;
begin
  if foDictionary in P.FileFlags then PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmNorm: App.DoMessage(Format(msgExtracting, [P.FileName]));
    pmNul:  App.DoMessage(Format(msgDecoding,   [P.FileName]));
    pmSkip: App.DoMessage(Format(msgSkipping,   [P.FileName]));
  end;
  if Mode = pmSkip then Exit;

  Stream.Seek(P.FileStartPos, soFromBeginning);
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
      while (App.Code < ccError) and (I < P.FileSize) do
      begin
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
        Progress;
        Inc(I);
      end;
    end else
    begin
      SecondaryCodec.Start;
      while (App.Code < ccError) and (I < P.FileSize) do
      begin
        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
        Progress;
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

    if Mode = pmNorm then FileSetAttr(P.FileName, P.FileAttr);
  end else
    App.DoMessage(Format(cmFileOpenError, [P.FileName]), ccError);

  if Crc <> P.FileCrc then
    App.DoMessage(Format(msgCRCERROR, [P.FileName]), ccError);
end;

procedure TDecoder.DecodeStrm(P: THeader; Mode: TExtractingMode;
  DstStrm: TFileWriter; const DstSize: int64; DstEncoded: boolean); {$IFDEF FPC} inline; {$ENDIF}
var
  DstFile: TFileWriter;
  Password: string;
  Symbol: byte;
  Crc: longword;
  I: int64;
begin
  if foDictionary in P.FileFlags then PPM.SetDictionary(P.FileDictionary);
  if foTable in P.FileFlags then PPM.SetTable(P.FileTable);
  if foTear in P.FileFlags then
    PPM.FreshFlexible
  else
    PPM.FreshSolid;

  case Mode of
    pmNorm: App.DoMessage(Format(msgDecoding, [P.FileName]));
    pmNul:  App.DoMessage(Format(msgDecoding, [P.FileName]));
    pmSkip: App.DoMessage(Format(msgSkipping, [P.FileName]));
  end;
  if Mode = pmSkip then Exit;

  Stream.Seek(P.FileStartPos, soFromBeginning);
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
      while (App.Code < ccError) and (I < DstSize) do
      begin
        Stream.Read(Symbol, 1);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
        Progress;
        Inc(I);
      end;
    end else
    begin
      SecondaryCodec.Start;
      while (App.Code < ccError) and (I < DstSize) do
      begin
        Symbol := PPM.UpdateModel(0);
        UpdCrc32(Crc, Symbol);
        DstFile.Write(Symbol, 1);
        Progress;
        Inc(I);
      end;
      SecondaryCodec.Flush;
    end;
    App.DoClearLine;
    Stream.BlowFish.Finish;

    if Mode = pmNorm then DstFile.Flush; // stream flush
    DstFile.BlowFish.Finish;             // finish after stream flush
  end else
    App.DoMessage(cmStreamError, ccError);

  if Crc <> P.FileCrc then
    App.DoMessage(Format(msgCRCERROR, [P.FileName]), ccError);
end;

end.
