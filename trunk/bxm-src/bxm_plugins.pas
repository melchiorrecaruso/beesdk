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
    FExec: string;
    FCommand: TCommand;
    FCompressionMode: TCompressionMode;
    FPassword: string;
    FRecursive: boolean;
    FArchiveName: string;
    FFileMasks: TStringList;
    FExcludeMasks: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetCommandLine: string;
  public
    property Exec: string read FExec write FExec;
    property Command: TCommand read FCommand write FCommand;
    property CompressionMode: TCompressionMode read FCompressionMode write FCompressionMode;
    property Password: string read FPassword write FPassword;
    property Recursive: boolean read FRecursive write FRecursive;
    property ArchiveName: string read FArchiveName write FArchiveName;
    property FileMasks: TStringList read FFileMasks;
    property ExcludeMasks: TStringList read FExcludeMasks;
  end;

  { TParser }

  TParser = class(TThread)
  private
    FMessages: TStringList;
    FPCL: TParserCommandLine;
    FProcess: TProcess;
    FTerminated: boolean;
    function GetCount: longint;
    function GetMessage(index: longint): string;
  public
    constructor Create(PCL: TParserCommandLine);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Terminate;
  public
    property Count: longint read GetCount;
    property Messages[index: longint]: string read GetMessage;
    property Terminated: boolean read FTerminated;
  end;

  TParserItem = class(TObject)
  public
    ItemName:   string;
    ItemPath:   string;
    ItemType:   string;
    ItemSize:   string;
    ItemPacked: string;
    ItemAttr:   string;
    ItemTime:   string;
    ItemComm:   string;
    ItemHash:   string;
    ItemCypher: string;
    ItemMethod: string;
    ItemIndex: longint;
  end;

  { TParserList }

  TParserList = class
  private
    FList: TList;
    function GetCount: longint;
    function GetItem(Index: longint): TParserItem;
    procedure UpdateItem(Item: TParserItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(Parser: TParser);
    procedure Clear;
  public
    property Count: longint read GetCount;
    property Items[Index: longint]: TParserItem read GetItem;
  end;

implementation

uses
  bx_Common,
  Dialogs,
  IniFiles;

/// TParserCommandLine class ...

constructor TParserCommandLine.Create;
begin
  inherited Create;
  FFileMasks    := TStringList.Create;
  FExcludeMasks := TStringList.Create;
  begin
    Clear;
  end;
end;

destructor TParserCommandLine.Destroy;
begin
  FExcludeMasks.Destroy;
  FFileMasks.Destroy;
  inherited Destroy;
end;

procedure TParserCommandLine.Clear;
begin
  FExec            := '';
  FCommand         := cNone;
  FCompressionMode := cmNormal;
  FPassword        := '';
  FRecursive       := FALSE;
  FArchiveName     := '';
  FFileMasks.Clear;
  FExcludeMasks.Clear;
end;

function TParserCommandLine.GetCommandLine: string;
var
  I: longint;
  ParserIni: TIniFile;
begin
  ParserIni := TIniFile.Create('bxm.ini');

  Result := '';
  if FCommand <> cNone then
  begin
    Result := ParserIni.ReadString(FExec, 'exec', '');
    case FCommand of
      cAdd:      Result := Result + ' ' + ParserIni.ReadString(FExec, 'add',      '');
      cDelete:   Result := Result + ' ' + ParserIni.ReadString(FExec, 'delete',   '');
      cExtract:  Result := Result + ' ' + ParserIni.ReadString(FExec, 'extract',  '');
      cList:     Result := Result + ' ' + ParserIni.ReadString(FExec, 'list',     '');
      cTest:     Result := Result + ' ' + ParserIni.ReadString(FExec, 'test',     '');
      cxExtract: Result := Result + ' ' + ParserIni.ReadString(FExec, 'xextract', '');
    end;

    if FCommand in [cAdd] then
      case FCompressionMode of
        cmStore:   Result := Result + ' ' + ParserIni.ReadString(FExec, 'store',   '');
        cmFastest: Result := Result + ' ' + ParserIni.ReadString(FExec, 'fastest', '');
        cmFast:    Result := Result + ' ' + ParserIni.ReadString(FExec, 'fast',    '');
        cmNormal:  Result := Result + ' ' + ParserIni.ReadString(FExec, 'normal',  '');
        cmMaximum: Result := Result + ' ' + ParserIni.ReadString(FExec, 'maximum', '');
        cmUltra:   Result := Result + ' ' + ParserIni.ReadString(FExec, 'ultra',   '');
      end;

    if FPassword <> '' then
      Result := Result + ' ' + ParserIni.ReadString(FExec, 'password',   '')  + ' ' + FPassword;

    if FRecursive = TRUE then
      Result := Result + ' ' + ParserIni.ReadString(FExec, 'recursive',   '');

    for I := 0 to FExcludeMasks.Count - 1 do
      Result := Result + ' ' + ParserIni.ReadString(FExec, 'exclude',   '') + FExcludeMasks[I];

    Result := Result + ' ''' + FArchiveName + '''';

    for I := 0 to FFileMasks.Count - 1 do
      Result := Result + ' ' + FFileMasks[I];
  end;
  ParserIni.Destroy;
end;

/// TParser class ...

constructor TParser.Create(PCL: TParserCommandLine);
begin
  inherited Create(TRUE);
  FMessages    := TStringList.Create;
  FPCL         := PCL;
  FProcess     := nil;
  FTerminated  := FALSE;
end;

destructor TParser.Destroy;
begin
  FMessages.Destroy;
  FPCL := nil;
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
    FreeAndNil(FProcess);
    FMem.Free;
  end;

  FTerminated := TRUE;
end;

procedure TParser.Terminate;
begin
  if Assigned(FProcess) then
    FProcess.Terminate(255);
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

constructor TParserList.Create;
begin
  inherited Create;
  FList := TList.Create;
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

procedure TParserList.UpdateItem(Item: TParserItem);
begin
  if Pos('D', UpperCase(Item.ItemAttr)) > 0 then
  begin
    Item.ItemType   := '.folderclose';
    Item.ItemPacked := '';
    Item.ItemSize   := '';
  end;
end;

procedure TParserList.Execute(Parser: TParser);
var
  I, K: longint;
  Item: TParserItem;
  S: string;
begin
  K := 0;
  I := 0;
  while (I < Parser.Count) and (K < 7) do
  begin
    S := Parser.Messages[I];
    if FileNamePos('7-Zip '           , S) = 1 then Inc(K);
    if FileNamePos('Listing archive: ', S) = 1 then Inc(K);
    if FileNamePos('--'           ,     S) = 1 then Inc(K);
    if FileNamePos('Path = ',           S) = 1 then Inc(K);
    if FileNamePos('Type = ',           S) = 1 then Inc(K);
    if FileNamePos('Physical Size = ' , S) = 1 then Inc(K);
    if FileNamePos('----------' ,       S) = 1 then Inc(K);
    Inc(I);
  end;

  while I < Parser.Count do
  begin
    S := Parser.Messages[I];
    // Search "error" message ...
    if FileNamePos('Error: ', S) = 1 then
    begin
      ShowMessage(S);
    end else

    if FileNamePos('Path = ', S) = 1 then
    begin
      Delete(S, 1, Length('Path = '));
      Item := TParserItem.Create;
      Item.ItemPath := ExtractFilePath(S);
      Item.ItemName := ExtractFileName(S);
      Item.ItemType := ExtractFileExt(S);
      FList.Add(Item);
    end else

    if FileNamePos('Size = ', S) = 1 then
    begin
      Delete(S, 1, Length('Size = '));
      Item.ItemSize := S;
    end else

    if FileNamePos('Packed Size = ', S) = 1 then
    begin
      Delete(S, 1, Length('Packed Size = '));
      Item.ItemPacked := S;
    end;

    if FileNamePos('Modified = ', S) = 1 then
    begin
      Delete(S, 1, Length('Modified = '));
      Item.ItemTime := S;
    end;

    if FileNamePos('Attributes = ', S) = 1 then
    begin
      Delete(S, 1, Length('Attributes = '));
      Item.ItemAttr := S;
    end;

    if FileNamePos('Encrypted = ', S) = 1 then
    begin
      Delete(S, 1, Length('Encrypted = '));
      Item.ItemCypher := S;
    end else

    if FileNamePos('CRC = ', S) = 1 then
    begin
      Delete(S, 1, Length('CRC = '));
      Item.ItemHash := S;
    end else

    if FileNamePos('Method = ', S) = 1 then
    begin
      Delete(S, 1, Length('Method = '));
      Item.ItemMethod := S;
    end else

    if FileNamePos('Comment = ', S) = 1 then
    begin
      Delete(S, 1, Length('Comment = '));
      Item.ItemComm := S;
    end;
    Inc(I);
  end;

  for I := 0 to GetCount - 1 do UpdateItem(GetItem(I));
end;

end.