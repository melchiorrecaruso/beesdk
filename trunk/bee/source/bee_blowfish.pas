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

{$I compiler.inc}

interface

uses
  BlowFish;

const
  DefaultBufferSize = 4096;

type
  TBuffer = array [0.. DefaultBufferSize -1] of byte;

  { TBaseCipher class }

  TBaseCipher = class(TObject)
  public
    function Encrypt(const Data: TBuffer; Count: longint): longint; virtual; abstract;
    function Decrypt(const Data: TBuffer; Count: longint): longint; virtual; abstract;
  end;

  { NulCipher class }

  TNulCipher = class(TBaseCipher)
  public
    function Encrypt(const Data: TBuffer; Count: longint): longint; override;
    function Decrypt(const Data: TBuffer; Count: longint): longint; override;
  end;

  { TBlowFishCipher class }

  TBlowFishCipher = class(TBaseCipher)
  private
    FBlowFish: TBlowFish;
  public
    constructor Create(const Key: string);
    destructor Destroy; override;
    function Encrypt(const Data: TBuffer; Count: longint): longint; override;
    function Decrypt(const Data: TBuffer; Count: longint): longint; override;
  end;

  TCipherAlgorithm = (caNul, caBlowFish);

implementation

uses
  Math;

/// TNulCipher class

function TNulCipher.Encrypt(const Data: TBuffer; Count: longint): longint;
begin
  Result := Count;
end;

function TNulCipher.Decrypt(const Data: TBuffer; Count: longint): longint;
begin
  Result := Count;
end;

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
  begin
    K[I -1] := byte(Key[I]);
  end;
  FBlowFish := TBlowFish.Create(K, KLen);
end;

destructor TBlowFishCipher.Destroy;
begin
  FBlowFish.Destroy;
  inherited Destroy;
end;

function TBlowFishCipher.Encrypt(const Data: TBuffer; Count: longint): longint;
var
  Block: ^TBFBlock;
begin
  Result := 0;
  while Result < Count do
  begin
    Block := @Data[Result];
    FBlowFish.Encrypt(Block^);
    Inc(Result, 8);
  end;
end;

function TBlowFishCipher.Decrypt(const Data: TBuffer; Count: longint): longint;
var
  Block: ^TBFBlock;
begin
  Result := 0;
  while Result < Count do
  begin
    Block := @Data[Result];
    FBlowFish.Decrypt(Block^);
    Inc(Result, 8);
  end;
end;

end.
