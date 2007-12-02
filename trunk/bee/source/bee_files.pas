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

  TFileReader class, bufferized TStream-similar input stream;
  TFileWriter class, bufferized TStream-similar output stream;
  TNulWriter  class, TStream-similar output stream, but works with 'nul' file.

  Modifyed:

  v0.7.8 build 0148 - 2005.06.23 by Andrew Filinsky;
  v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;
  
  v0.7.9 build 0511 - 2007.12.01 by Melchiorre Caruso.
}

unit Bee_Files;

{$I compiler.inc}

interface

uses
  Classes,
  SysUtils,

  Bee_BlowFish;

type
  TFileReader = class(TFileStream)
  public
    constructor Create(const FileName: string; Mode: word);
    destructor Destroy; override;
    function Read(var Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
  public
    BlowFish: TBlowFish;
  private
    Size, Readed: longint;
    LocalBuffer:  array [0..$FFFF] of byte;
  end;

type
  TFileWriter = class(TFileStream)
  public
    constructor Create(const FileName: string; Mode: word);
    destructor Destroy; override;
    procedure Flush;
    function Write(const Data; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
  public
    BlowFish: TBlowFish;
  private
    function WriteBlock(const aData; aCount: longint): longint;
  private
    Size: longint;
    LocalBuffer: array [0..$FFFF] of byte;
  end;

type
  TNulWriter = class(TStream)
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
  public
    BlowFish: TBlowFish;
  protected
    procedure SetSize(NewSize: longint); override;
  private
    Current, Longest: longint;
  end;

implementation

uses
  Bee_Common,
  Bee_Assembler; // Low-level routines ...

// class TFileReader...

constructor TFileReader.Create(const FileName: string; Mode: word);
begin
  if Mode = fmCreate then
  begin
    Bee_Common.ForceDirectories(ExtractFilePath(FileName));
  end;
  BlowFish := TBlowFish.Create;
  Readed := 0;
  Size := 0;

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
  S: longint;
begin
  if (Count = 1) and (Readed < Size) then
  begin
    byte(Data) := LocalBuffer[Readed];
    Inc(Readed);
    Result := Count;
  end else
  begin
    Result := 0;
    repeat
      if Readed = Size then
      begin
        Readed := 0;
        Size := inherited Read(LocalBuffer, SizeOf(LocalBuffer));

        if Size = 0 then
          Exit; // This causes Result < Count

        if BlowFish.Started then
          BlowFish.Decode(LocalBuffer, Size);
      end;
      S := Count - Result;

      if S > Size - Readed then
        S := Size - Readed;

      CopyBytes(LocalBuffer[Readed], Bytes[Result], S);
      Inc(Result, S);
      Inc(Readed, S);
    until Result = Count;
  end;
end;

function TFileReader.Seek(Offset: longint; Origin: word): longint;
begin
  Size := 0;
  Readed := 0;
  Result := inherited Seek(Offset, Origin);
end;

// class TFileWriter...

constructor TFileWriter.Create(const FileName: string; Mode: word);
begin
  if Mode = fmCreate then
  begin
    Bee_Common.ForceDirectories(ExtractFilePath(FileName));
  end;
  BlowFish := TBlowFish.Create;
  Size := 0;

  inherited Create(FileName, Mode);
end;

procedure TFileWriter.Flush;
begin
  if BlowFish.Started then
    Size := BlowFish.Encode(LocalBuffer, Size);

  if inherited Write(LocalBuffer, Size) <> Size then
    raise EWriteError.Create('SWriteError');

  Size := 0;
end;

function TFileWriter.Write(const Data; Count: longint): longint;
begin
  if Count > SizeOf(LocalBuffer) - Size then
    Result := WriteBlock(Data, Count)
  else
    if Count > 1 then
    begin
      CopyBytes(Data, LocalBuffer[Size], Count);
      Inc(Size, Count);
      Result := Count;
    end else
    begin
      LocalBuffer[Size] := byte(Data);
      Inc(Size);
      Result := Count;
    end;
end;

function TFileWriter.WriteBlock(const aData; aCount: longint): longint;
var
  Data: array [0..MaxInt - 1] of byte absolute aData;
  S: longint;
begin
  Result := 0;
  repeat
    S := SizeOf(LocalBuffer) - Size;
    CopyBytes(Data[Result], LocalBuffer[Size], S);
    Inc(Result, S);
    Inc(Size, S);
    Flush;
  until not (aCount - Result > SizeOf(LocalBuffer));

  CopyBytes(Data[Result], LocalBuffer[Size], aCount - Result);
  Inc(Size, aCount - Result);
  Inc(Result, aCount - Result);
end;

function TFileWriter.Seek(Offset: longint; Origin: word): longint;
begin
  if Size > 0 then Flush;
  Result := inherited Seek(Offset, Origin);
end;

destructor TFileWriter.Destroy;
begin
  if Size > 0 then
  begin
    Flush;
  end;
  BlowFish.Free;
  inherited Destroy;
end;

/// class TNulWriter...

constructor TNulWriter.Create;
begin
  inherited Create;
  BlowFish := TBlowFish.Create;
  Current  := 0;
  Longest  := 0;
end;

destructor TNulWriter.Destroy;
begin
  BlowFish.Free;
  inherited Destroy;
end;

function TNulWriter.Read(var Buffer; Count: longint): longint;
begin
  Result := 0;
end;

function TNulWriter.Write(const Buffer; Count: longint): longint;
begin
  Inc(Current, Count);
  Result := Count;
end;

function TNulWriter.Seek(Offset: longint; Origin: word): longint;
begin
  if Current > Longest then
    Longest := Current;

  case Origin of
    soFromCurrent: Inc(Offset, Current);
    soFromEnd: Inc(Offset, Longest);
  end;

  Current := Offset;
  Result  := Offset;
end;

procedure TNulWriter.SetSize(NewSize: longint);
begin
  Current := NewSize;
end;

end.
