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

    THashGen classes.

  Fist release:

    v1.0 build 2153 - 2013.12.15 by Melchiorre Caruso.

  Modifyed:

}

unit bx_HashGen;

{$I bx_compiler.inc}

interface

uses
  Crc,
  Md5,
  Sha1;

type
  { THashGen class }

  THashGen = class(TObject)
  private
    FDigest: string;
  public
    procedure Start;  virtual; abstract;
    function  Finish: string; virtual; abstract;
    procedure Update(Data: PByte; Count: longint); virtual; abstract;
  end;

  { TCRC32HashGen class }

  TCRC32HashGen = class(THashGen)
  private
    FCRC: longword;
  public
    procedure Start;  override;
    function  Finish: string; override;
    procedure Update(Data: PByte; Count: longint); override;
  end;

  { TCRC64HashGen class }

  TCRC64HashGen = class(THashGen)
  private
    FCRC: qword;
  public
    procedure Start;  override;
    function  Finish: string; override;
    procedure Update(Data: PByte; Count: longint); override;
  end;

  { TSHA1HashGen class }

  TSHA1HashGen = class(THashGen)
  private
    FCTX: TSHA1Context;
  public
    procedure Start;  override;
    function  Finish: string; override;
    procedure Update(Data: PByte; Count: longint); override;
  end;

  { TMD5HashGen class }

  TMD5HashGen = class(THashGen)
  private
    FCTX: TMD5Context;
  public
    procedure Start;  override;
    function  Finish: string; override;
    procedure Update(Data: PByte; Count: longint); override;
  end;

implementation

uses
  bx_Common;

/// TCRC32HashGen class

procedure TCRC32HashGen.Start;
begin
  FCRC := crc32(0, nil, 0);
end;

function TCRC32HashGen.Finish: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

procedure TCRC32HashGen.Update(Data: PByte; Count: longint);
begin
  FCRC := crc32(FCRC, Data, Count);
end;

/// TCRC64HashGen class

procedure TCRC64HashGen.Start;
begin
  FCRC := crc64(0, nil, 0);
end;

function TCRC64HashGen.Finish: string;
begin
  Result := Hex(FCRC, SizeOf(FCRC));
end;

procedure TCRC64HashGen.Update(Data: PByte; Count: longint);
begin
  FCRC := crc64(FCRC, Data, Count);
end;

/// TSHA1HashGen class

procedure TSHA1HashGen.Start;
begin
  SHA1Init(FCTX);
end;

function TSHA1HashGen.Finish: string;
var
  Digest: TSHA1Digest;
begin
  SHA1Final(FCTX, Digest);
  Result := UpCase(SHA1Print(Digest));
end;

procedure TSHA1HashGen.Update(Data: PByte; Count: longint);
begin
  SHA1Update(FCTX, Data[0], Count);
end;

/// TMD5HashGen class

procedure TMD5HashGen.Start;
begin
  MD5Init(FCTX);
end;

function TMD5HashGen.Finish: string;
var
  Digest: TMD5Digest;
begin
  MD5Final(FCTX, Digest);
  Result := UpCase(MD5Print(Digest));
end;

procedure TMD5HashGen.Update(Data: PByte; Count: longint);
begin
  MD5Update(FCTX, Data[0], Count);
end;

end.
