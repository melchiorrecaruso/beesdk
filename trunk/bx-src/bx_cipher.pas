{
  Copyright (c) 2012-2014 Melchiorre Caruso.

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

    Cipher class.

  Modifyed:

    v1.0.0 build 2210 - 2014.01.15 by Melchiorre Caruso.

}

unit bx_cipher;

{$I bx_compiler.inc}

interface

uses
  BlowFish, Idea;

const
  DEFAULTBUFFERSIZE = 4096;

type
  TBuffer = array [0.. DEFAULTBUFFERSIZE - 1] of byte;

  { TCipher abstract class }

  TCipher = class(TObject)
  public
    procedure Start(const Key: string); virtual;
    procedure Finish virtual;
    function Update(var Data: TBuffer; Count: longint): longint; virtual;
  end;

  { TBlowFishCipher class }

  TBlowFishEnCipher = class(TCipher)
  private
    FBlowFish: TBlowFish;
  public
    procedure Start(const Key: string); override;
    procedure Finish; override;
    function Update(var Data: TBuffer; Count: longint): longint; override;
  end;

  TBlowFishDeCipher = class(TCipher)
  private
    FBlowFish: TBlowFish;
  public
    procedure Start(const Key: string); override;
    procedure Finish; override;
    function Update(var Data: TBuffer; Count: longint): longint; override;
  end;

  { TIdeaCipher class }

  TIdeaEnCipher = class(TCipher)
  private
    FKey: TIDEAKey;
  public
    procedure Start(const Key: string); override;
    procedure Finish; override;
    function Update(var Data: TBuffer; Count: longint): longint; override;
  end;

  TIdeaDeCipher = class(TCipher)
  private
    FKey: TIDEAKey;
  public
    procedure Start(const Key: string); override;
    procedure Finish; override;
    function Update(var Data: TBuffer; Count: longint): longint; override;
  end;

implementation

uses
  Math;

/// TCipher class

procedure TCipher.Start(const Key: string);
begin
  // nothing to do
end;

procedure TCipher.Finish;
begin
  // noting do to
end;

function TCipher.Update(var Data: TBuffer; Count: longint): longint;
begin
  Result := Count;
end;

/// TBlowFishEnCipher class

procedure TBlowFishEnCipher.Start(const Key: string);
var
  K: TBlowFishKey;
  KLen: longint;
begin
  FillByte(K[0], SizeOf(K), 0);
  Move(Key[1], K[0], Min(Length(Key), Length(K)));

  FBlowFish := TBlowFish.Create(K, Length(K));
end;

procedure TBlowFishEnCipher.Finish;
begin
  FBlowFish.Destroy;
end;

function TBlowFishEnCipher.Update(var Data: TBuffer; Count: longint): longint;
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

/// TBlowFishDeCipher class

procedure TBlowFishDeCipher.Start(const Key: string);
var
  K: TBlowFishKey;
  KLen: longint;
begin
  FillByte(K[0], SizeOf(K), 0);
  Move(Key[1], K[0], Min(Length(Key), Length(K)));

  FBlowFish := TBlowFish.Create(K, Length(K));
end;

procedure TBlowFishDeCipher.Finish;
begin
  FBlowFish.Destroy;
end;

function TBlowFishDeCipher.Update(var Data: TBuffer; Count: longint): longint;
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

/// TIdeaEnCipher class

procedure TIdeaEnCipher.Start(const Key: string);
var
  K: TIdeaCryptKey;
begin
  FillByte(K[0], SizeOf(K), 0);
  Move(Key[1], K[0], Min(Length(Key), Length(K)));

  EnKeyIdea(K, FKey);
end;

procedure TIdeaEnCipher.Finish;
begin
  // nothing to do
end;

function TIdeaEnCipher.Update(var Data: TBuffer; Count: longint): longint;
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

/// TIdeaDeCipher class

procedure TIdeaDeCipher.Start(const Key: string);
var
  K: TIdeaCryptKey;
  Z: TIDEAKey;
begin
  FillByte(K[0], SizeOf(K), 0);
  Move(Key[1], K[0], Min(Length(Key), Length(K)));

  EnKeyIdea(K, Z);
  DeKeyIdea(Z, FKey);
end;

procedure TIdeaDeCipher.Finish;
begin
  // nothing to do
end;

function TIdeaDeCipher.Update(var Data: TBuffer; Count: longint): longint;
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