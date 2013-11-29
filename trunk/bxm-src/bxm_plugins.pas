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
    function GetCommandLine: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  public
    property CommandLine: string read GetCommandLine;
    property Command: TCommand read FCommand write FCommand;
    property CompressionMode: TCompressionMode read FCompressionMode write FCompressionMode;
    property Password: string read FPassword write FPassword;
    property ArchiveName: string read FArchiveName write FArchiveName;
    property FileMasks: TStringList read FFileMasks;
  end;

  { TParser }

  TParser = class(TThread)
  private
    FCommandLine: string;
    FMessages: TStringList;
    function GetCount: longint;
    function GetMessage(index: longint): string;
  public
    constructor Create(const CommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property Count: longint read GetCount;
    property Messages[index: longint]: string read GetMessage;
  end;

implementation

uses
  Process;

/// TParserCommandLine class ...

constructor TParserCommandLine.Create;
begin
  inherited Create;
  FFileMasks := TStringList.Create;
end;

procedure TParserCommandLine.Clear;
begin
  FCommand := cNone;
  FCompressionMode := cmNormal;
  FPassword := '';
  FArchiveName := '';
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
      cList:     Result := Result + ' l -y -stl';
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

constructor TParser.Create(const CommandLine: string);
begin
  inherited Create(FALSE);
  FCommandLine := CommandLine;
  FMessages := TStringList.Create;
end;

destructor TParser.Destroy;
begin
  FMessages.Destroy;
  inherited Destroy;
end;

procedure TParser.Execute;
var
  Count: longint;
  FMem: TMemoryStream;
  FProcess: TProcess;
  Readed: int64;
begin
  FMessages.Clear;
  FMem := TMemoryStream.Create;
  FProcess := TProcess.Create(nil);
  FProcess.CommandLine := FCommandLine;
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
  FProcess.Free;
  FMem.Free;
end;

function TParser.GetMessage(index: longint): string;
begin
  Result := FMessages[index];
end;

function TParser.GetCount: longint;
begin
  Result := FMessages.Count;
end;

(*

procedure TSevenZipApp.ProcessListOutput(FOutput: TStringList);
var
  I:  integer;
  ItemStr: string;
  ItemStrSwap: string;
  FI: TFileInfoExtra;
begin
  I := 0;
  while I < FOutput.Count do
  begin
    ItemStr := FOutput.Strings[I];
    // Error
    if FileNamePos(SevenZipErrorMark, ItemStr) = 1 then
    begin
      Delete(ItemStr, 1, Length(SevenZipErrorMark));
      ProcessFatalError(ItemStr, 255);
    end
    else
    // List archive
    if FileNamePos(SevenZipListMark, ItemStr) = 1 then
    begin
      ProcessMessage(msgOpening + FCommandLine.ArchiveName);
      ProcessMessage(msgScanning + '...');
    end
    else
    // File Path - New
    if FileNamePos(SevenZipPathMark, ItemStr) = 1 then
    begin
      Delete(ItemStr, 1, Length(SevenZipPathMark));

      FI.FileName     := StringToPChar(ExtractFileName(ItemStr));
      FI.FilePath     := StringToPChar(ExtractFilePath(ItemStr));
      FI.FileSize     := 0;
      FI.FilePacked   := 0;
      FI.FileRatio    := 0;
      FI.FileAttr     := 0;
      FI.FileTime     := 0;
      FI.FileComm     := StringToPChar('');
      FI.FileCrc      := 0;
      FI.FileMethod   := StringToPChar('');
      FI.FileVersion  := StringToPChar('');
      FI.FilePassword := StringToPChar('');
      FI.FilePosition := -1;

      Inc(I);
      while I < FOutput.Count do
      begin
        ItemStr := FOutput.Strings[I];
        // File Size
        if FileNamePos(SevenZipSizeMark, ItemStr) = 1 then
        begin
          Delete(ItemStr, 1, Length(SevenZipSizeMark));
          try
            if Length(ItemStr) > 0 then
              FI.FileSize := StrToInt(ItemStr)
            else
              FI.FileSize := 0;

            if FI.FileSize > 0 then
              if FI.FilePacked > 0 then
              begin
                FI.FileRatio := Round(100 * (FI.FilePacked / FI.FileSize));
              end;
          except
            ProcessFatalError('Error: reading file size', 255);
            FI.FileSize := -1;
          end;
        end
        else
        // File Packed Size
        if FileNamePos(SevenZipPackedMark, ItemStr) = 1 then
        begin
          Delete(ItemStr, 1, Length(SevenZipPackedMark));
          try
            if Length(ItemStr) > 0 then
              FI.FilePacked := StrToInt(ItemStr)
            else
              FI.FilePacked := 0;

            if FI.FileSize > 0 then
              if FI.FilePacked > 0 then
              begin
                FI.FileRatio := Round(100 * (FI.FilePacked / FI.FileSize));
              end;
          except
            ProcessFatalError('Error: reading file packed-size', 255);
            FI.FilePacked := -1;
          end;
        end
        else
        // File Time
        if FileNamePos(SevenZipTimeMark, ItemStr) = 1 then
        begin
          Delete(ItemStr, 1, Length(SevenZipTimeMark));
          SetLength(ItemStr, 16);
          ItemStrSwap := ItemStr;
          ItemStr[1]  := ItemStrSwap[9];
          ItemStr[2]  := ItemStrSwap[10];
          ItemStr[3]  := '/';
          ItemStr[4]  := ItemStrSwap[6];
          ItemStr[5]  := ItemStrSwap[7];
          ItemStr[6]  := '/';
          ItemStr[7]  := ItemStrSwap[1];
          ItemStr[8]  := ItemStrSwap[2];
          ItemStr[9]  := ItemStrSwap[3];
          ItemStr[10] := ItemStrSwap[4];
          ItemStr[14] := '.';
          try
            FI.FileTime := DateTimeToFileDate(StrToDateTime(ItemStr));
          except
            ProcessFatalError('Error: reading file date-time', 255);
            FI.FileTime := -1;
          end;
        end
        else
        // File Attributes
        if FileNamePos(SevenZipAttrMark, ItemStr) = 1 then
        begin
          Delete(ItemStr, 1, Length(SevenZipAttrMark));

          if Pos('D', ItemStr) > 0 then
            FI.FileAttr := FI.FileAttr or faDirectory;
          if Pos('R', ItemStr) > 0 then
            FI.FileAttr := FI.FileAttr or faReadOnly;
          if Pos('H', ItemStr) > 0 then
            FI.FileAttr := FI.FileAttr or faHidden;
          if Pos('S', ItemStr) > 0 then
            FI.FileAttr := FI.FileAttr or faSysFile;
          if Pos('A', ItemStr) > 0 then
            FI.FileAttr := FI.FileAttr or faArchive;

        end
        else
        // File Password
        if FileNamePos(SevenZipPasswordMark, ItemStr) = 1 then
        begin
          Delete(ItemStr, 1, Length(SevenZipPasswordMark));
          if ItemStr = '-' then
            FI.FilePassword := 'No'
          else
          if ItemStr = '+' then
            FI.FilePassword := 'Yes'
          else
            FI.FilePassword := '?';
        end
        else
        // File Comment
        if FileNamePos(SevenZipCommentMark, ItemStr) = 1 then
        begin
          Delete(ItemStr, 1, Length(SevenZipCommentMark));
          FI.FileComm := StringToPChar(ItemStr);
        end
        else
        // File Method
        if FileNamePos(SevenZipMethodMark, ItemStr) = 1 then
        begin
          Delete(ItemStr, 1, Length(SevenZipMethodMark));
          FI.FileMethod := StringToPChar(ItemStr);
        end
        else
        // Error
        if FileNamePos(SevenZipErrorMark, ItemStr) = 1 then
        begin
          ProcessFatalError(ItemStr, 255);
          Break;
        end
        else
        // Next -->
        if FileNamePos(SevenZipPathMark, ItemStr) = 1 then
        begin
          Dec(I);
          Break;
        end;
        Inc(I);
      end;
      // ---
      if (FI.FileAttr and faDirectory) = 0 then
      begin
        ProcessList(FI, FCommandLine.vOption);
      end;
    end;
    Inc(I);
  end;
end;

procedure TSevenZipApp.ProcessTestOutput(FOutput: TStringList);
begin
  ProcessMessage(FOutput.Text);
end;

*)


end.