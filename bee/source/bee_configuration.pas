{
  Copyright (c) 2003-2013 Andrew Filinsky.

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

    Bee algorithm configuration support.

  Modifyed:

    v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
    v0.7.9 build 0312 - 2007.02.16 by Andrew Filinsky;

    v0.8.0 build 2060 - 2013.09.08 by Melchiorre Caruso.
}

unit bx_Configuration;

{$I bx_compiler.inc}

interface

uses
  Classes,
  SysUtils;

// -------------------------------------------------------------------------- //
//  Configuration tables type                                                 //
// -------------------------------------------------------------------------- //

const
  TableSize = 20;
  TableCols = 2;

type
  TTableCol = array [0..TableSize] of longword;

  TTable = packed record
    Level: longword;
    T: array [0..TableCols - 1] of TTableCol;
  end;

  TTableParameters = array [1..SizeOf(TTable) div 4] of byte;

// -------------------------------------------------------------------------- //
//  Default table parameters                                                  //
// -------------------------------------------------------------------------- //

const
  DefaultDictionaryLevel: longword = $0002;
  DefaultTableParameters: TTableParameters =
    (  3, 163, 157,  65,  93, 117, 135, 109, 126, 252, 172, 252, 152, 227, 249,
     249, 253, 196,  27,  82,  93,  74, 182, 245,  40,  67,  77, 143, 133, 135,
     128, 155, 207, 177, 225, 251, 253, 248,  73,  35,  15, 107, 143);

  EmptyTableParameters: TTableParameters =
    (  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0);

// -------------------------------------------------------------------------- //
//  Configuration class                                                       //
// -------------------------------------------------------------------------- //

type
  TConfigSection = class(TStringList)
  public
    function  GetTable(const Ext: string; var T: TTableParameters): boolean;
    procedure PutData (const Name: string; var Data; aCount: longint);
    function  GetData (const Name: string; var Data; aCount: longint): boolean;
  end;

  TConfiguration = class(TStringList)
  public
    CurrentSection: TConfigSection;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    function  GetTable(const Ext: string; var T: TTableParameters): boolean;
    procedure PutData (const Name: string; var Data; aCount: longint);
    function  GetData (const Name: string; var Data; aCount: longint): boolean;
    procedure Selector(const Name: string);
    function  Split (const S: string; var Name, Value: string): boolean;
  end;

implementation

uses
  bx_Common;

{ Class:  TConfiguration | methods }
{ Domain: public                   }

constructor TConfiguration.Create;
begin
  inherited Create;
  Selector('\main');
end;

destructor TConfiguration.Destroy;
var
  I: longint;
begin
  for I := 0 to Count - 1 do
  begin
    Objects[I].Free;
  end;
  inherited Destroy;
end;

procedure TConfiguration.LoadFromFile(const FileName: string);
var
  S, aName, aValue: string;
  List: TStringList;
  I:    longint;
begin
  List := TStringList.Create;
  List.LoadFromFile(FileName);
  Selector('\main');
  for I := 0 to List.Count - 1 do
  begin
    S := List[I];
    if (S > '') and (S[1] = '\') then
      Selector(S)
    else
      if (S = '') or (S[1] = ';') or not Split(S, aName, aValue) then
        CurrentSection.Add(S)
      else
        CurrentSection.Values[aName] := aValue;
  end;
  Selector('\main');
  List.Free;
end;

procedure TConfiguration.SaveToFile(const FileName: string);
var
  List: TStringList;
  I:    longint;
begin
  List := TStringList.Create;
  for I := 0 to Count - 1 do
  begin
    List.Add(Names[I]);
    List.AddStrings(TConfigSection(Objects[I]));
  end;
  List.SaveToFile(FileName);
  List.Free;
end;

function TConfiguration.GetTable(const Ext: string; var T: TTableParameters): boolean;
var
  OldSection: TConfigSection;
begin
  OldSection := CurrentSection;
  Selector('\main');
  Selector('\m' + CurrentSection.Values['Method']);
  Result := CurrentSection.GetTable(Ext, T);
  CurrentSection := OldSection;
end;

procedure TConfiguration.PutData(const Name: string; var Data; aCount: longint);
begin
  CurrentSection.PutData(Name, Data, aCount);
end;

function TConfiguration.GetData(const Name: string; var Data; aCount: longint): boolean;
begin
  Result := CurrentSection.GetData(Name, Data, aCount);
end;

procedure TConfiguration.Selector(const Name: string);
var
  Index: longint;
begin
  Index := IndexOfName(Name);
  if Index < 0 then
  begin
    CurrentSection := TConfigSection.Create;
    Objects[Add(Name + '=yes')] := CurrentSection;
  end else
    CurrentSection := TConfigSection(Objects[Index]);
end;

function TConfiguration.Split(const S: string; var Name, Value: string): boolean;
var
  Index: longint;
begin
  Index  := System.Pos('=', S);
  Result := Index > 0;
  if Result then
  begin
    Name  := Trim(Copy(S, 1, Index - 1));
    Value := Trim(Copy(S, Index + 1, MaxInt));
  end;
end;

{ Class:  TConfigSection | methods }
{ Domain: public                   }

function TConfigSection.GetTable(const Ext: string; var T: TTableParameters): boolean;
var
  S: string;
begin
  S      := Values[Ext];
  Result := GetData(Ext, T, SizeOf(T));
  if not Result then Result := (S = '') and (CompareText(Ext, '.Default') <> 0) and GetTable('.Default', T);
  if not Result then Result := (S > '') and (IndexOfName(S) >= 0) and (IndexOfName(S) < IndexOfName(Ext)) and GetTable(S, T);
end;

procedure TConfigSection.PutData(const Name: string; var Data; aCount: longint);
begin
  Values[Name] := bx_Common.Hex(Data, aCount);
end;

function TConfigSection.GetData(const Name: string; var Data; aCount: longint): boolean;
begin
  FillChar(Data, aCount, 0);
  Result := bx_Common.HexToData(Values[Name], Data, aCount);
end;

end.