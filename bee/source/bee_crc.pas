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

{$I bee_compiler.inc}

interface

uses
  Crc,
  Md5,
  Sha1;

type
  { TBaseHash class }

  TBaseHash = class(TObject)
  private
    FDigest: string;
  public
    procedure Start;  virtual; abstract;
    function  Finish: string; virtual; abstract;
    procedure Update(Data: PByte; Count: longint); virtual; abstract;
  end;

  { TCRC32Hash class }

  TCRC32Hash = class(TBaseHash)
  private
    FCRC: longword;
  public
    procedure Start;  override;
    function  Finish: string; override;
    procedure Update(Data: PByte; Count: longint); override;
  end;

  { TCRC64Hash class }

  TCRC64Hash = class(TBaseHash)
  private
    FCRC: qword;
  public
    procedure Start;  override;
   function  Finish: string; override;
    procedure Update(Data: PByte; Count: longint); override;
  end;

  { TSHA1Hash class }

  TSHA1Hash = class(TBaseHash)
  private
    FCTX: TSHA1Context;
  public
    procedure Start;  override;
   function  Finish: string; override;
    procedure Update(Data: PByte; Count: longint); override;
  end;

  { TMD5Hash class }

  TMD5Hash = class(TBaseHash)
  private
    FCTX: TMD5Context;
  public
    procedure Start;  override;
    function  Finish: string; override;
    procedure Update(Data: PByte; Count: longint); override;
  end;

implementation

uses
  Bee_Common;

/// TCRC32Hash class

procedure TCRC32Hash.Start;
begin
  FCRC := crc32(0, nil, 0);
end;

function TCRC32Hash.Finish: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

procedure TCRC32Hash.Update(Data: PByte; Count: longint);
begin
  FCRC := crc32(FCRC, Data, Count);
end;

/// TCRC64Hash class

procedure TCRC64Hash.Start;
begin
  FCRC := crc64(0, nil, 0);
end;

function TCRC64Hash.Finish: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

procedure TCRC64Hash.Update(Data: PByte; Count: longint);
begin
  FCRC := crc64(FCRC, Data, Count);
end;

/// TSHA1Hash class

procedure TSHA1Hash.Start;
begin
  SHA1Init(FCTX);
end;

function TSHA1Hash.Finish: string;
var
  Digest: TSHA1Digest;
begin
  SHA1Final(FCTX, Digest);
  Result := UpCase(SHA1Print(Digest));
end;

procedure TSHA1Hash.Update(Data: PByte; Count: longint);
begin
  SHA1Update(FCTX, Data[0], Count);
end;

/// TMD5Hash class

procedure TMD5Hash.Start;
begin
  MD5Init(FCTX);
end;

function TMD5Hash.Finish: string;
var
  Digest: TMD5Digest;
begin
  MD5Final(FCTX, Digest);
  Result := UpCase(MD5Print(Digest));
end;

procedure TMD5Hash.Update(Data: PByte; Count: longint);
begin
  MD5Update(FCTX, Data[0], Count);
end;

end.