{
  Copyright (c) 2013 Melchiorre Caruso.

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

{
  Contains:

    TCipher classes.

  Fist release:

    v1.0 build 2153 - 2013.12.15 by Melchiorre Caruso.

  Modifyed:

}

unit bx_Cipher;

{$I bx_compiler.inc}

interface

uses
  BlowFish,
  Idea;

const
  DefaultBufferSize = 4096;

type
  TBuffer = array [0.. DefaultBufferSize - 1] of byte;

  { TCipher class }

  TCipher = class(TObject)
  public
    function Encrypt(var Data: TBuffer; Count: longint): longint; virtual; abstract;
    function Decrypt(var Data: TBuffer; Count: longint): longint; virtual; abstract;
  end;

  { TBlowFishCipher class }

  TBlowFishCipher = class(TCipher)
  private
    FBlowFish: TBlowFish;
  public
    constructor Create(const Key: string);
    destructor Destroy; override;
    function Encrypt(var Data: TBuffer; Count: longint): longint; override;
    function Decrypt(var Data: TBuffer; Count: longint): longint; override;
  end;

  { TIdeaCipher class }

  TIdeaCipher = class(TCipher)
  private
    FKey: TIDEAKey;
  public
    constructor CreateEnc(const Key: string);
    constructor CreateDec(const Key: string);
    function Encrypt(var Data: TBuffer; Count: longint): longint; override;
    function Decrypt(var Data: TBuffer; Count: longint): longint; override;
  end;

implementation

uses
  Math;

/// TBlowFishCipher class

constructor TBlowFishCipher.Create(const Key: string);
var
  K: TBlowFishKey;
  KLen: longint;
begin
  inherited Create;
  FillByte(K[0], SizeOf(K), 0);
  Move(Key[1], K[0], Min(Length(Key), Length(K)));

  FBlowFish := TBlowFish.Create(K, Length(K));
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
    Inc(Result, SizeOf(TBFBlock));
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

constructor TIdeaCipher.CreateEnc(const Key: string);
var
  K: TIdeaCryptKey;
begin
  inherited Create;
  FillByte(K[0], SizeOf(K), 0);
  Move(Key[1], K[0], Min(Length(Key), Length(K)));

  EnKeyIdea(K, FKey);
end;

constructor TIdeaCipher.CreateDec(const Key: string);
var
  K: TIdeaCryptKey;
  Z: TIDEAKey;
begin
  FillChar(K[0], SizeOf(K),0);
  Move(Key[1], K[0], Min(Length(Key), Length(K)));

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
