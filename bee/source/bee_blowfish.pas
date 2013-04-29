{
  Copyright (c) 2006-2013 Melchiorre Caruso.

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

  Modifyed:

    v0.8.0 build 1864 - 2013.02.16 by Melchiorre Caruso.
}

unit Bee_BlowFish;

{$I bee_compiler.inc}

interface

uses
  Idea, BlowFish;

const
  DefaultBufferSize = 4096;

type
  TBuffer = array [0.. DefaultBufferSize -1] of byte;

  { TBaseCipher class }

  TBaseCipher = class(TObject)
  public
    function Encrypt(var Data: TBuffer; Count: longint): longint; virtual; abstract;
    function Decrypt(var Data: TBuffer; Count: longint): longint; virtual; abstract;
  end;

  { TBlowFishCipher class }

  TBlowFishCipher = class(TBaseCipher)
  private
    FBlowFish: TBlowFish;
  public
    constructor Create(const Key: string);
    destructor Destroy; override;
    function Encrypt(var Data: TBuffer; Count: longint): longint; override;
    function Decrypt(var Data: TBuffer; Count: longint): longint; override;
  end;

  { TIdeaCipher class }

  TIdeaCipher = class(TBaseCipher)
  private
    FKey: TIDEAKey;
  public
    constructor CreateEn(const Key: string);
    constructor CreateDe(const Key: string);
    function Encrypt(var Data: TBuffer; Count: longint): longint; override;
    function Decrypt(var Data: TBuffer; Count: longint): longint; override;
  end;

  TCipherAlgorithm = (caNul, caBlowFish, caIdea);

implementation

uses
  Math;

/// TBlowFishCipher class

constructor TBlowFishCipher.Create(const Key: string);
var
  I: longint;
  K: TBlowFishKey;
  KLen: longint;
begin
  inherited Create;
  KLen := Min(Length(Key), Length(K));
  for I := 1 to KLen do
    K[I - 1] := byte(Key[I]);
  FBlowFish := TBlowFish.Create(K, KLen);
end;

destructor TBlowFishCipher.Destroy;
begin
  FBlowFish.Destroy;
  inherited Destroy;
end;

function TBlowFishCipher.Encrypt(var Data: TBuffer; Count: longint): longint;
var
  Block: ^TBFBlock;
begin
  Result := 0;
  while Result < Count do
  begin
    Block := @Data[Result];
    FBlowFish.Encrypt(Block^);
    Inc(Result,  SizeOf(TBFBlock));
  end;
end;

function TBlowFishCipher.Decrypt(var Data: TBuffer; Count: longint): longint;
var
  Block: ^TBFBlock;
begin
  Result := 0;
  while Result < Count do
  begin
    Block := @Data[Result];
    FBlowFish.Decrypt(Block^);
    Inc(Result, SizeOf(TBFBlock));
  end;
end;

/// TIdeaCipher class

constructor TIdeaCipher.CreateEn(const Key: string);
var
  K: TIdeaCryptKey;
  KLen: Integer;
begin
  KLen := Length(Key);
  if KLen > SizeOf(K) then
    KLen := SizeOf(K);
  FillChar(K, SizeOf(K),0);
  Move(Key[1], K, KLen);

  EnKeyIdea(K, FKey);
end;

constructor TIdeaCipher.CreateDe(const Key: string);
var
  K: TIdeaCryptKey;
  KLen: Integer;
  Z: TIDEAKey;
begin
  KLen := Length(Key);
  if KLen > SizeOf(K) then
    KLen := SizeOf(K);
  FillChar(K, SizeOf(K),0);
  Move(Key[1], K, KLen);

  EnKeyIdea(K, Z);
  DeKeyIdea(Z, FKey);
end;

function TIdeaCipher.Encrypt(var Data: TBuffer; Count: longint): longint;
var
  Cache: TBuffer;
  InPtr:  ^TIDEACryptData;
  OutPtr: ^TIDEACryptData;
begin
  Move(Data[0], Cache[0], Count);

  Result := 0;
  while Result < Count do
  begin
    InPtr  := @Cache[Result];
    OutPtr := @Data [Result];
    CipherIdea(InPtr^, OutPtr^, FKey);
    Inc(Result, SizeOf(TIDEACryptData));
  end;
end;

function TIdeaCipher.Decrypt(var Data: TBuffer; Count: longint): longint;
var
  Cache: TBuffer;
  InPtr:  ^TIDEACryptData;
  OutPtr: ^TIDEACryptData;
begin
  Move(Data[0], Cache[0], Count);

  Result := 0;
  while Result < Count do
  begin
    InPtr  := @Cache[Result];
    OutPtr := @Data [Result];
    CipherIdea(InPtr^, OutPtr^, FKey);
    Inc(Result, SizeOf(TIDEACryptData));
  end;
end;

end.
