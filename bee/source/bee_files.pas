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

  TFileReader class, bufferized TStream-similar input stream;
  TFileWriter class, bufferized TStream-similar output stream;
  TNulWriter  class, TStream-similar output stream, but works with 'nul' file.

  Modifyed:

  v0.7.8 build 0148 - 2005.06.23 by Andrew Filinsky;
  v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;
  
  v0.7.9 build 0955 - 2009.02.25 by Melchiorre Caruso.
}

unit Bee_Files;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,
  // ---
  Bee_BlowFish;

type
  TFileReader = class(TFileStream)
  public
    constructor Create(const FileName: string; Mode: word);
    destructor Destroy; override;
    function Read(var Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  public
    BlowFish: TBlowFish;
  private
    BufferSize: cardinal;
    BufferReaded: cardinal;
    Buffer: array [0..$1FFFF] of byte;
  end;

type
  TFileWriter = class(TFileStream)
  public
    constructor Create(const FileName: string; Mode: word);
    destructor Destroy; override;
    procedure Flush;
    function Write(const Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  public
    BlowFish: TBlowFish;
  private
    BufferSize: cardinal;
    Buffer:     array [0..$1FFFF] of byte;
  end;

type
  TNulWriter = class(TFileWriter)
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  protected
    procedure SetSize(NewSize: longint); override;
    procedure SetSize(const NewSize: int64); override;
  end;

function CreateTFileReader(const FileName: string; Mode: word): TFileReader;
function CreateTFileWriter(const FileName: string; Mode: word): TFileWriter;

implementation

uses
  Bee_Common,
  Bee_Assembler; // Low-level routines ...

// class TFileReader...

constructor TFileReader.Create(const FileName: string; Mode: word);
begin
  if Mode = fmCreate then
  begin
    ForceDirectories(ExtractFilePath(FileName));
  end;
  BufferSize   := 0;
  BufferReaded := 0;
  BlowFish     := TBlowFish.Create;
  inherited Create(FileName, Mode);
end;

destructor TFileReader.Destroy;
begin
  BlowFish.Free;
  inherited Destroy;
end;

function TFileReader.Read(var Data; Count: longint): longint;
var
  Bytes: array [0..$FFFFFFF] of byte absolute Data;
  S:     longint;
begin
  if (Count = 1) and (BufferReaded < BufferSize) then
  begin
    byte(Data) := Buffer[BufferReaded];
    Inc(BufferReaded);
    Result := Count;
  end
  else
  begin
    Result := 0;
    repeat
      if BufferReaded = BufferSize then
      begin
        BufferReaded := 0;
        BufferSize   := inherited Read(Buffer, SizeOf(Buffer));

        if BufferSize = 0 then
          Exit; // This causes Result < Count

        if BlowFish.Started then
          BlowFish.Decode(Buffer, BufferSize);
      end;
      S := Count - Result;

      if S > BufferSize - BufferReaded then
        S := BufferSize - BufferReaded;

      CopyBytes(Buffer[BufferReaded], Bytes[Result], S);
      Inc(Result, S);
      Inc(BufferReaded, S);
    until Result = Count;
  end;
end;

function TFileReader.Seek(Offset: longint; Origin: word): longint;
begin
  BufferSize := 0;
  BufferReaded := 0;
  Result := inherited Seek(Offset, Origin);
end;

function TFileReader.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  BufferSize := 0;
  BufferReaded := 0;
  Result := inherited Seek(Offset, Origin);
end;

function CreateTFileReader(const FileName: string; Mode: word): TFileReader;
begin
  try
    Result := TFileReader.Create(FileName, Mode);
  except
    Result := nil;
  end;
end;

// class TFileWriter...

constructor TFileWriter.Create(const FileName: string; Mode: word);
begin
  if Mode = fmCreate then
  begin
    ForceDirectories(ExtractFilePath(FileName));
  end;
  BufferSize := 0;
  BlowFish   := TBlowFish.Create;
  inherited Create(FileName, Mode);
end;

procedure TFileWriter.Flush;
begin
  if BlowFish.Started then
    BufferSize := BlowFish.Encode(Buffer, BufferSize);

  inherited Write(Buffer, BufferSize);
  BufferSize := 0;
end;

function TFileWriter.Write(const Data; Count: longint): longint;
var
  Bytes: array [0..$FFFFFFF] of byte absolute Data;
  S:     longint;
begin
  if Count > (SizeOf(Buffer) - BufferSize) then
  begin
    Result := 0;
    repeat
      S := SizeOf(Buffer) - BufferSize;
      CopyBytes(Bytes[Result], Buffer[BufferSize], S);
      Inc(Result, S);
      Inc(BufferSize, S);
      Flush;
    until ((Count - Result) <= SizeOf(Buffer));

    CopyBytes(Bytes[Result], Buffer[BufferSize], Count - Result);
    Inc(BufferSize, Count - Result);
    Inc(Result, Count - Result);
  end
  else
  if Count > 1 then
  begin
    CopyBytes(Data, Buffer[BufferSize], Count);
    Inc(BufferSize, Count);
    Result := Count;
  end
  else
  begin
    Buffer[BufferSize] := byte(Data);
    Inc(BufferSize);
    Result := Count;
  end;
end;

function TFileWriter.Seek(Offset: longint; Origin: word): longint;
begin
  if BufferSize > 0 then
    Flush;
  Result := inherited Seek(Offset, Origin);
end;

function TFileWriter.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if BufferSize > 0 then
    Flush;
  Result := inherited Seek(Offset, Origin);
end;

destructor TFileWriter.Destroy;
begin
  if BufferSize > 0 then
    Flush;

  BlowFish.Free;
  inherited Destroy;
end;

function CreateTFileWriter(const FileName: string; Mode: word): TFileWriter;
begin
  try
    Result := TFileWriter.Create(FileName, Mode);
  except
    Result := nil;
  end;
end;

/// class TNulWriter...

constructor TNulWriter.Create;
begin
  // inherited Create;
  BlowFish := TBlowFish.Create;
end;

destructor TNulWriter.Destroy;
begin
  BlowFish.Free;
  // inherited Destroy;
end;

function TNulWriter.Read(var Buffer; Count: longint): longint;
begin
  Result := 0;
end;

function TNulWriter.Write(const Buffer; Count: longint): longint;
begin
  Result := 0;
end;

function TNulWriter.Seek(Offset: longint; Origin: word): longint;
begin
  Result := 0;
end;

function TNulWriter.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  Result := 0;
end;

procedure TNulWriter.SetSize(NewSize: longint);
begin
  // nothing to do
end;

procedure TNulWriter.SetSize(const NewSize: int64);
begin
  // nothing to do
end;

end.
