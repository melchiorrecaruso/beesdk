{
  Copyright (c) 2013 Melchiorre Caruso

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

    TParser class.

  Modifyed:

    v0.1.0 build 0642 - 2011.11.25 by Melchiorre Caruso.
}

unit bxm_plugins;

{$I bxm_compiler.inc}

interface

uses
  Classes,
  Process,
  SysUtils;

type
  { TCommand }

  TCommand = (cNone, cAdd, cDelete, cExtract, cList, cTest, cxExtract);

  { TCompressionMode }

  TCompressionMode = (cmStore, cmFastest, cmFast, cmNormal, cmMaximum, cmUltra);

  { TParserCommandLine }

  TParserCommandLine = class
  private
    FCommand: TCommand;
    FCompressionMode: TCompressionMode;
    FPassword: string;
    FArchiveName: string;
    FFileMasks: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetCommandLine: string;
  public
    property Command: TCommand read FCommand write FCommand;
    property CompressionMode: TCompressionMode read FCompressionMode write FCompressionMode;
    property Password: string read FPassword write FPassword;
    property ArchiveName: string read FArchiveName write FArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

  { TParser }

  TParser = class(TThread)
  private
    FMessages: TStringList;
    FOnTerminate: TNotifyEvent;
    FPCL: TParserCommandLine;
    FProcess: TProcess;
    function GetCount: longint;
    function GetMessage(index: longint): string;
  public
    constructor Create(PCL: TParserCommandLine);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property Count: longint read GetCount;
    property Messages[index: longint]: string read GetMessage;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

  TParserItem = class(TObject)
  public
    ItemName:   string;
    ItemPath:   string;
    ItemType:   string;
    ItemSize:   int64;
    ItemPacked: int64;
    ItemAttr:   longint;
    ItemTime:   int64;
    ItemComm:   string;
    ItemHash:   string;
    ItemCypher: string;
    ItemMethod: string;
  end;

  { TParserList }

  TParserList = class
  private
    FList: TList;
    FParser: TParser;
    function GetCount: longint;
    function GetItem(Index: longint): TParserItem;
  public
    constructor Create(Parser: TParser);
    destructor Destroy; override;
    procedure Execute;
    procedure Clear;
  public
    property Count: longint read GetCount;
    property Items[Index: longint]: TParserItem read GetItem;
  end;

implementation

uses
  bx_Common,
  Dialogs;

/// TParserCommandLine class ...

constructor TParserCommandLine.Create;
begin
  inherited Create;
  FCommand         := cNone;
  FCompressionMode := cmNormal;
  FPassword        := '';
  FArchiveName     := '';
  FFileMasks       := TStringList.Create;
end;

procedure TParserCommandLine.Clear;
begin
  FCommand         := cNone;
  FCompressionMode := cmNormal;
  FPassword        := '';
  FArchiveName     := '';
  FFileMasks.Clear;
end;

destructor TParserCommandLine.Destroy;
begin
  FFileMasks.Destroy;
  inherited Destroy;
end;

function TParserCommandLine.GetCommandLine: string;
var
  i: longint;
begin
  Result := '';
  if FCommand <> cNone then
  begin
    Result := '7z';
    case FCommand of
      cAdd:      Result := Result + ' a -y';
      cDelete:   Result := Result + ' d -y';
      cExtract:  Result := Result + ' e -y';
      cList:     Result := Result + ' l -y -slt';
      cTest:     Result := Result + ' t -y';
      cxExtract: Result := Result + ' x -y';
    end;

    if FCommand in [cAdd] then
      case FCompressionMode of
        cmStore:   Result := Result + ' -mx0';
        cmFastest: Result := Result + ' -mx1';
        cmFast:    Result := Result + ' -mx3';
        cmNormal:  Result := Result + ' -mx5';
        cmMaximum: Result := Result + ' -mx7';
        cmUltra:   Result := Result + ' -mx9';
      end;

    if FPassword <> '' then
      Result := Result + ' "-p' + FPassword + '"';

    Result := Result + ' "' + FArchiveName + '"';

    for i := 0 to FFileMasks.Count - 1 do
      Result := Result + ' "' + FFileMasks[i] + '"';
  end;
end;

/// TParser class ...

constructor TParser.Create(PCL: TParserCommandLine);
begin
  inherited Create(TRUE);
  FMessages    := TStringList.Create;
  FOnTerminate := nil;
  FPCL         := PCL;
  FProcess     := TProcess.Create(nil);
end;

destructor TParser.Destroy;
begin
  FMessages.Destroy;
  FOnTerminate := nil;
  FPCL         := nil;
  FProcess.Destroy;
  inherited Destroy;
end;

procedure TParser.Execute;
var
  Count: longint;
  FMem: TMemoryStream;
  FProcess: TProcess;
  Readed: int64;
begin

  FMem := TMemoryStream.Create;
  FProcess := TProcess.Create(nil);
  try
    FMessages.Clear;
    ShowMessage(FPCL.GetCommandLine);

    FProcess.CommandLine := FPCL.GetCommandLine;
    FProcess.Options := [poNoConsole, poUsePipes];

    Readed := 0;
    FProcess.Execute;
    while FProcess.Running do
    begin
      FMem.SetSize(Readed + 2048);
      Count := FProcess.Output.Read((FMem.Memory + Readed)^, 2048);

      if Count > 0 then
        Inc(Readed, Count)
      else
        Sleep(100);
    end;

    repeat
      FMem.SetSize(Readed + 2048);
      Count := FProcess.Output.Read((FMem.Memory + Readed)^, 2048);

      if Count > 0 then
        Inc(Readed, Count);
    until Count <= 0;
    FMem.SetSize(Readed);

    FMessages.LoadFromStream(FMem);
  finally
    FProcess.Free;
    FMem.Free;
  end;

  if Assigned(FOnTerminate) then
    FOnTerminate(Self);
end;

function TParser.GetMessage(index: longint): string;
begin
  Result := FMessages[index];
end;

function TParser.GetCount: longint;
begin
  Result := FMessages.Count;
end;

{ TParserList }

constructor TParserList.Create(Parser: TParser);
begin
  inherited Create;
  FList   := TList.Create;
  FParser := Parser;
end;

destructor TParserList.Destroy;
begin
  Clear;
  FList.Destroy;
end;

procedure TParserList.Clear;
var
  i: longint;
begin
  for i := 0 to FList.Count -1 do
    TParserItem(FList[i]).Destroy;
  FList.Clear;
end;

function TParserList.GetCount: longint;
begin
  Result := FList.Count;
end;

function TParserList.GetItem(Index: longint): TParserItem;
begin
  Result := TParserItem(FList[Index]);
end;

procedure TParserList.Execute;
var
  I, K: longint;
  Item: TParserItem;
  S: string;
begin
  K := 0;
  I := 0;
  while (I < FParser.Count) and (K < 7) do
  begin
    S := FParser.Messages[I];
    if FileNamePos('7-Zip '           , S) = 1 then Inc(K);
    if FileNamePos('Listing archive: ', S) = 1 then Inc(K);
    if FileNamePos('--'           ,     S) = 1 then Inc(K);
    if FileNamePos('Path = ',           S) = 1 then Inc(K);
    if FileNamePos('Type = ',           S) = 1 then Inc(K);
    if FileNamePos('Physical Size = ' , S) = 1 then Inc(K);
    if FileNamePos('----------' ,       S) = 1 then Inc(K);
    Inc(I);
  end;

  while I < FParser.Count do
  begin
    S := FParser.Messages[I];
    // Search "error" message ...
    if FileNamePos('Error: ', S) = 1 then
    begin
      ShowMessage(S);
    end else

    if FileNamePos('Path = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('Path = '));

      Item := TParserItem.Create;
      Item.ItemPath := ExtractFilePath(S);
      Item.ItemName := ExtractFileName(S);
      FList.Add(Item);
    end else

    if FileNamePos('Size = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('Size = '));
      try
        if Length(S) > 0 then
          Item.ItemSize := StrToInt(S)
        else
          Item.ItemSize := 0;
      except
        ShowMessage('ERROR');
        Item.ItemSize := -1;
      end;
    end else

    if FileNamePos('Packed Size = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('Packed Size = '));
      try
        if Length(S) > 0 then
          Item.ItemPacked := StrToInt(S)
        else
          Item.ItemPacked := 0;
      except
        Item.ItemPacked := -1;
      end;
    end;

    if FileNamePos('Modified = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('Modified = '));
      try
        Item.ItemTime := DateTimeToFileDate(StrToDateTime(S));
      except
         Item.ItemTime := 0;
      end;
    end;

    if FileNamePos('Attributes = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('Attributes = '));
      Item.ItemAttr := 0;
      if Pos('D', S) > 0 then
        Item.ItemAttr := Item.ItemAttr or faDirectory;
      if Pos('R', S) > 0 then
        Item.ItemAttr := Item.ItemAttr or faReadOnly;
      if Pos('H', S) > 0 then
        Item.ItemAttr := Item.ItemAttr or faHidden;
      if Pos('S', S) > 0 then
        Item.ItemAttr := Item.ItemAttr or faSysFile;
      if Pos('A', S) > 0 then
        Item.ItemAttr := Item.ItemAttr or faArchive;
      end else

    if FileNamePos('Encrypted = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('Encrypted = '));
      if S = '-' then
        Item.ItemCypher := 'No'
      else
        if S = '+' then
          Item.ItemCypher := 'Yes'
        else
          Item.ItemCypher := '?';
    end else

    if FileNamePos('CRC = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('CRC = '));
      Item.ItemHash := S;
    end else

    if FileNamePos('Method = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('Method = '));
      Item.ItemMethod := S;
    end else

    if FileNamePos('Comment = ', S) = 1 then
    begin
      Writeln(S);
      Delete(S, 1, Length('Comment = '));
      Item.ItemComm := S;
    end;
    Inc(I);
  end;
end;

end.