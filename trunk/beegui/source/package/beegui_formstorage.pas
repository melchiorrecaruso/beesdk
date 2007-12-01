 {
    Copyright (c) 2005-2007 Andrew Filinsky and Melchiorre Caruso

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

unit BeeGui_FormStorage;

{$I Compiler.inc}

interface

uses
  Classes,
  Dialogs,
  TypInfo,
  Graphics,
  ComCtrls,
  StdCtrls,
  IniFiles,
  SysUtils;

type
  TFormStorage = class(TComponent)
  private
    FProperties: TStringList;
    function GetProperties: string;
    procedure Split(const S1: string; var S2, S3: string);
    function Merge(AComp: TComponent; AProp: PPropInfo): string;
    procedure PutProperties(const Value: string);
    procedure SaveProperty(AFile: TIniFile; AComp: TComponent; const AProp: string);
    procedure LoadProperty(AFile: TIniFile; AComp: TComponent; const AProp: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Load(const FileName: string);
    procedure Save(const FileName: string);
    destructor Destroy; override;
  public
    property Properties: string Read GetProperties Write PutProperties;
  end;

implementation

{ TFormStorage }

constructor TFormStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProperties := TStringList.Create;
end;

destructor TFormStorage.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

procedure TFormStorage.PutProperties(const Value: string);
var
  I: integer;
  S1, S2: string;
begin
  FProperties.Clear;
  S1 := Value + ';';
  I  := Pos(';', S1);
  while I > 0 do
  begin
    S2 := Copy(S1, 1, I - 1);
    if I > 1 then
    begin
      FProperties.Add(S2);
    end;
    Delete(S1, 1, I);
    I := Pos(';', S1);
  end;
end;

function TFormStorage.GetProperties: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FProperties.Count - 1 do
  begin
    Result := Result + FProperties.Strings[i] + ';';
  end;
end;

procedure TFormStorage.Split(const S1: string; var S2, S3: string);
var
  I: integer;
begin
  I := Pos('.', S1);
  if I = 0 then
  begin
    S2 := S1;
    S3 := '';
  end else
  begin
    S2 := Copy(S1, 1, I - 1);
    S3 := Copy(S1, I + 1, Length(S1) - I);
  end;
end;

function TFormStorage.Merge(AComp: TComponent; AProp: PPropInfo): string;
begin
  Result := AComp.GetNamePath + '.' + AProp.Name;
end;

procedure TFormStorage.Load(const FileName: string);
var
  sComp, sProp: string;
  IniFile: TIniFile;
  I: integer;
begin
  IniFile := TIniFile.Create(FileName);
  try
    for I := 0 to FProperties.Count - 1 do
    begin
      Split(FProperties.Strings[I], sComp, sProp);
      if AnsiCompareText(sComp, Owner.Name) = 0 then
      begin
        LoadProperty(IniFile, Owner, sProp);
      end else
      begin
        LoadProperty(IniFile, Owner.FindComponent(sComp), sProp);
      end;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TFormStorage.Save(const FileName: string);
var
  sComp, sProp: string;
  IniFile: TIniFile;
  I: integer;
begin
  IniFile := TIniFile.Create(FileName);
  try
    for I := 0 to FProperties.Count - 1 do
    begin
      Split(FProperties.Strings[I], sComp, sProp);
      if AnsiCompareText(sComp, Owner.Name) = 0 then
      begin
        SaveProperty(IniFile, Owner, sProp);
      end else
      begin
        SaveProperty(IniFile, Owner.FindComponent(sComp), sProp);
      end;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TFormStorage.LoadProperty(AFile: TIniFile; AComp: TComponent;
  const AProp: string);
var
  S: string;
  Obj: TObject;
  I, J: integer;
  Prop: PPropInfo;
  sProp, sProps: string;
begin
  if Assigned(AComp) then
  begin
    Split(AProp, sProp, sProps);
    Prop := GetPropInfo(AComp.ClassInfo, sProp);
    if Prop <> nil then
    begin
      if Prop^.PropType^.Kind in [tkString, tkLString, tkWString] then
      begin
        S := GetStrProp(AComp, Prop);
        SetStrProp(AComp, Prop, AFile.ReadString(AComp.ClassName,
          Merge(AComp, Prop), S));
      end else
      if Prop^.PropType^.Kind in [tkInteger] then
      begin
        I := GetOrdProp(AComp, Prop);
        SetOrdProp(AComp, Prop, AFile.ReadInteger(AComp.ClassName,
          Merge(AComp, Prop), I));
      end else
      if Prop^.PropType^.Kind in [tkEnumeration] then
      begin
        S := GetEnumProp(AComp, Prop);
        SetEnumProp(AComp, Prop, AFile.ReadString(AComp.ClassName,
          Merge(AComp, Prop), S));
      end else
      if Prop^.PropType^.Kind in [tkClass] then
      begin
        Obj := GetObjectProp(AComp, Prop);
        if (Obj <> nil) then
        begin
          if (Obj.ClassNameIs('TListColumns')) then
          begin
            for i := 0 to TListColumns(Obj).Count - 1 do
            begin
              LoadProperty(AFile,
                TComponent(TListColumns(Obj).Items[I]), sProps);
            end;
          end else
          if (Obj.ClassNameIs('TComboBoxStrings')) then
          begin
            for I := 0 to TComboBox(AComp).Items.Count - 1 do
            begin
              J := TComboBox(AComp).ItemIndex;
              S := TComboBox(AComp).Items[I];
              TComboBox(AComp).Items[I] :=
                AFile.ReadString(AComp.ClassName, Merge(AComp, Prop) +
                '[' + IntToStr(I) + ']', S);
              TComboBox(AComp).ItemIndex := J;
            end;
          end else
          if (Obj.ClassNameIs('TFont')) then
          begin
            TFont(Obj).Color :=
              AFile.ReadInteger(AComp.ClassName, Merge(AComp, Prop) +
              '.Color', TFont(Obj).Color);
            TFont(Obj).Size  :=
              AFile.ReadInteger(AComp.ClassName, Merge(AComp, Prop) +
              '.Size', TFont(Obj).Size);
            TFont(Obj).Charset :=
              AFile.ReadInteger(AComp.ClassName, Merge(AComp, Prop) +
              '.Charset', TFont(Obj).Charset);
            TFont(Obj).Name  :=
              AFile.ReadString(AComp.ClassName, Merge(AComp, Prop) +
              '.Name', TFont(Obj).Name);
            // ---
            with TFont(Obj) do
            begin
              Style := [];
              S :=
                AFile.ReadString(AComp.ClassName, Merge(AComp, Prop) +
                '.Style', '0000');
              if S[1] = 'B' then
                Style := Style + [fsBold];
              if S[2] = 'I' then
                Style := Style + [fsItalic];
              if S[3] = 'U' then
                Style := Style + [fsUnderline];
              if S[4] = 'S' then
                Style := Style + [fsStrikeOut];
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFormStorage.SaveProperty(AFile: TIniFile; AComp: TComponent;
  const AProp: string);
var
  i: integer;
  S: string;
  Obj: TObject;
  Prop: PPropInfo;
  sProp, sProps: string;
begin
  if Assigned(AComp) then
  begin
    Split(AProp, sProp, sProps);
    Prop := GetPropInfo(AComp.ClassInfo, sProp);
    if Prop <> nil then
    begin
      if Prop^.PropType^.Kind in [tkString, tkLString, tkWString] then
      begin
        AFile.WriteString(AComp.ClassName, Merge(AComp, Prop),
          GetStrProp(AComp, Prop));
      end else
      if Prop^.PropType^.Kind in [tkInteger] then
      begin
        AFile.WriteInteger(AComp.ClassName, Merge(AComp, Prop),
          GetOrdProp(AComp, Prop));
      end else
      if Prop^.PropType^.Kind in [tkEnumeration] then
      begin
        AFile.WriteString(AComp.ClassName, Merge(AComp, Prop),
          GetEnumProp(AComp, Prop));
      end else
      if Prop^.PropType^.Kind in [tkClass] then
      begin
        Obj := GetObjectProp(AComp, Prop);
        if (Obj <> nil) then
        begin
          if (Obj.ClassNameIs('TListColumns')) then
          begin
            for i := 0 to TListColumns(Obj).Count - 1 do
            begin
              SaveProperty(AFile,
                TComponent(TListColumns(Obj).Items[I]), sProps);
            end;
          end else
          if (Obj.ClassNameIs('TComboBoxStrings')) then
          begin
            for i := 0 to TComboBox(AComp).Items.Count - 1 do
            begin
              AFile.WriteString(AComp.ClassName, Merge(AComp, Prop) +
                '[' + IntToStr(I) + ']', TComboBox(AComp).Items[I]);
            end;
          end else
          if (Obj.ClassNameIs('TFont')) then
          begin
            AFile.WriteInteger(AComp.ClassName, Merge(AComp, Prop) +
              '.Color', TFont(Obj).Color);
            AFile.WriteInteger(AComp.ClassName, Merge(AComp, Prop) +
              '.Size', TFont(Obj).Size);
            AFile.WriteInteger(AComp.ClassName, Merge(AComp, Prop) +
              '.Charset', TFont(Obj).Charset);
            AFile.WriteString(AComp.ClassName, Merge(AComp, Prop) +
              '.Name', TFont(Obj).Name);
            // ---
            with TFont(Obj) do
            begin
              S := '0000';
              if fsBold in Style then
                S[1] := 'B';
              if fsItalic in Style then
                S[2] := 'I';
              if fsUnderline in Style then
                S[3] := 'U';
              if fsStrikeOut in Style then
                S[4] := 'S';
              AFile.WriteString(AComp.ClassName,
                Merge(AComp, Prop) + '.Style', S);
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
