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

    v0.8.0 build 1864 - 2013.02.15 by Melchiorre Caruso.
}

unit Bee_Crc;

{$I compiler.inc}

interface

uses
  Crc,
  Sha1;

type
  { TBaseHash class }

  TBaseHash = class(TObject)
  public
    procedure Initialize; virtual; abstract;
    procedure Update(Data: PByte; Count: longint); virtual; abstract;
    function Finalize: string; virtual; abstract;
  end;

  { TCRC32Hash class }

  TCRC32Hash = class(TBaseHash)
  private
    FCRC: longword;
  public
    procedure Initialize; override;
    procedure Update(Data: PByte; Count: longint); override;
    function Finalize: string; override;
  end;

  { TCRC64Hash class }

  TCRC64Hash = class(TBaseHash)
  private
    FCRC: qword;
  public
    procedure Initialize; override;
    procedure Update(Data: PByte; Count: longint); override;
    function Finalize: string; override;
  end;

  { TSHA1Hash class }

  TSHA1Hash = class(TBaseHash)
  private
    FCTX: TSHA1Context;
    FDigest: TSHA1Digest;
  public
    procedure Initialize; override;
    procedure Update(Data: PByte; Count: longint); override;
    function Finalize: string; override;
  end;

  THashAlgorithm = (haNone, haCRC32, haCRC64, haSHA1);

implementation

uses
  Bee_Common;

/// TCRC32Hash class

procedure TCRC32Hash.Initialize;
begin
  FCRC := crc32(0, nil, 0);
end;

procedure TCRC32Hash.Update(Data: PByte; Count: longint);
begin
  FCRC := crc32(FCRC, Data, Count);
end;

function TCRC32Hash.Finalize: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

/// TCRC64Hash class

procedure TCRC64Hash.Initialize;
begin
  FCRC := crc64(0, nil, 0);
end;

procedure TCRC64Hash.Update(Data: PByte; Count: longint);
begin
  FCRC := crc64(FCRC, Data, Count);
end;

function TCRC64Hash.Finalize: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

/// TSHA1Hash class

procedure TSHA1Hash.Initialize;
begin
  SHA1Init(FCTX);
end;

procedure TSHA1Hash.Update(Data: PByte; Count: longint);
begin
  SHA1Update(FCTX, Data, Count);
end;

function TSHA1Hash.Finalize: string;
var
  Digest: TSHA1Digest;
begin
  SHA1Final(FCTX, Digest);
  Result := SHA1Print(Digest);
end;

end.
