{
  Copyright (c) 2003-2008 Andrew Filinsky

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

  Bee configuration support.

  Modifyed:

  v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
  
  v0.7.9 build 0312 - 2007.02.16 by Andrew Filinsky.
}

unit Bee_Configuration;

{$I compiler.inc}

interface

uses
  SysUtils, // Trim
  Classes;  // TStringList

const
  TableSize = 20;
  TableCols = 2;

type
  TTableCol = array [0..TableSize] of cardinal;

  TTable = packed record
    Level: cardinal;
    T:     array [0..TableCols - 1] of TTableCol;
  end;

  TTableParameters = array [1..SizeOf(TTable) div 4] of byte;

  TConfigSection = class(TStringList)
  public
    function GetTable(const Ext: string; var T: TTableParameters): boolean;
    procedure PutData(const Name: string; var Data; aCount: integer);
    function GetData(const Name: string; var Data; aCount: integer): boolean;
  end;

  TConfiguration = class(TStringList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    function GetTable(const Ext: string; var T: TTableParameters): boolean;
    procedure PutData(const Name: string; var Data; aCount: integer);
    function GetData(const Name: string; var Data; aCount: integer): boolean;
    procedure Selector(const Name: string);
    function Split(const S: string; var Name, Value: string): boolean;
  public
    CurrentSection: TConfigSection;
  end;

implementation

uses
  Bee_Common;

 (***************************************************************************)
 (* Class:  TConfiguration | methods                                        *)
 (* Domain: public                                                          *)
 (***************************************************************************)

constructor TConfiguration.Create;
begin
  inherited;
  Selector('\main');
end;

destructor TConfiguration.Destroy;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited;
end;

procedure TConfiguration.LoadFromFile(const FileName: string);
var
  List: TStringList;
  I:    integer;
  S, aName, aValue: string;
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
  I:    integer;
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

procedure TConfiguration.PutData(const Name: string; var Data; aCount: integer);
begin
  CurrentSection.PutData(Name, Data, aCount);
end;

function TConfiguration.GetData(const Name: string; var Data; aCount: integer): boolean;
begin
  Result := CurrentSection.GetData(Name, Data, aCount);
end;

procedure TConfiguration.Selector(const Name: string);
var
  Index: integer;
begin
  Index := IndexOfName(Name);
  if Index < 0 then
  begin
    CurrentSection := TConfigSection.Create;
    Objects[Add(Name + '=yes')] := CurrentSection;
  end
  else
    CurrentSection := TConfigSection(Objects[Index]);
end;

function TConfiguration.Split(const S: string; var Name, Value: string): boolean;
var
  Index: integer;
begin
  Index  := System.Pos('=', S);
  Result := Index > 0;
  if Result then
  begin
    Name  := Trim(Copy(S, 1, Index - 1));
    Value := Trim(Copy(S, Index + 1, MaxInt));
  end;
end;

 (***************************************************************************)
 (* Class:  TConfigSection | methods                                        *)
 (* Domain: public                                                          *)
 (***************************************************************************)

function TConfigSection.GetTable(const Ext: string; var T: TTableParameters): boolean;
var
  S: string;
begin
  S      := Values[Ext];
  Result := GetData(Ext, T, SizeOf(T));
  if not Result then
    Result := (S = '') and (CompareText(Ext, '.Default') <> 0) and
      GetTable('.Default', T);

  if not Result then
    Result := (S > '') and (IndexOfName(S) >= 0) and
      (IndexOfName(S) < IndexOfName(Ext)) and GetTable(S, T);
end;

procedure TConfigSection.PutData(const Name: string; var Data; aCount: integer);
begin
  Values[Name] := Bee_Common.Hex(Data, aCount);
end;

function TConfigSection.GetData(const Name: string; var Data; aCount: integer): boolean;
begin
  FillChar(Data, aCount, 0);
  Result := Bee_Common.HexToData(Values[Name], Data, aCount);
end;

end.
