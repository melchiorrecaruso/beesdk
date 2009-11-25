{
  Copyright (c) 2008-2009 Andrew Filinsky and Melchiorre Caruso

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

    TSevenZipApp class.

  Modifyed:

    v0.1.40 build 0642 - 2009.02.16 by Melchiorre Caruso.
}

unit BeeLib_Plugins;

{$I Compiler.inc}

interface

uses
  Classes,
  Process,
  SysUtils,
  // ---
  Bee_Types,
  Bee_Common,
  Bee_Interface,
  Bee_CommandLine;

const
  SevenZipPluginVer = '7Zip Plugin ver 0.1.40';

  SevenZipPluginSupport =
    '|7z|zip|gz|gzip|tgz|bz2|bzip2|tbz2|tbz|tar|lzma' +
    '|rar|cab|arj|z|taz|cpio|rpm|deb|lzh|lha|chm|chw' +
    '|hxs|iso|msi|doc|xls|ppt|wim|swm|dmg|xar|hfs|';

type
  TProcessOutput = procedure(FOutput: TStringList) of object;

type
  TSevenZipApp = class(TApp)
  private
    FCommandLine:   TCommandLine;
    FMemOutputProc: TProcessOutput;
  private
    procedure ProcessListOutput(FOutput: TStringList);
    procedure ProcessTestOutput(FOutput: TStringList);

    function CheckCommandLine: string;
    procedure CheckOverwrite;
  public
    constructor Create(aParams: TStringList);
    destructor Destroy; override;
    procedure Execute; override;
  end;

{ plugins routines }

function SevenZipPlugin: string; overload;
function SevenZipPlugin(const aArchiveName: string): boolean; overload;

implementation

uses
  Bee_Consts;

const
  SevenZipPathMark     = 'Path = ';
  SevenZipSizeMark     = 'Size = ';
  SevenZipPackedMark   = 'Packed Size = ';
  SevenZipTimeMark     = 'Modified = ';
  SevenZipAttrMark     = 'Attributes = ';
  SevenZipCRCMarck     = 'CRC = ';
  SevenZipPasswordMark = 'Encrypted = ';
  SevenZipMethodMark   = 'Method = ';
  SevenZipBlockMark    = 'Block = ';

  SevenZipListMark    = 'Listing archive: ';
  SevenZipCommentMark = 'Comment = ';
  SevenZipErrorMark   = 'Error: ';

/// SevenZipPlungin function

function SevenZipPlugin: string;
begin
  {$IFDEF UNIX}
  // Result := IncludeTrailingBackSlash(GetApplicationPluginsDir) + '7za';
  {$ELSE}
  {$IFDEF MSWINDOWS}
  // Result := IncludeTrailingBackSlash(GetApplicationPluginsDir) + '7z.exe';
  {$ELSE}
  Result := '';
  {$ENDIF}
  {$ENDIF}
  if FileExists(Result) then
  begin
    Result := ExtractFileName(Result);
  end;
end;

function SevenZipPlugin(const aArchiveName: string): boolean; overload;
var
  FExtention: string;
begin
  Result     := False;
  FExtention := ExtractFileExt(aArchiveName);
  if Length(FExtention) > 0 then
  begin
    FExtention    := LowerCase(FExtention) + '|';
    FExtention[1] := '|';
    if Pos(FExtention, SevenZipPluginSupport) > 0 then
    begin
      Result := True;
    end;
  end;
end;

/// TSevenZipApp class ...

constructor TSevenZipApp.Create(aParams: TStringList);
begin
  inherited Create(aParams);
  FCommandLine := TCommandLine.Create;
  FCommandLine.Process(FParams);
end;

destructor TSevenZipApp.Destroy;
begin
  FCommandLine.Destroy;
  inherited Destroy;
end;

function TSevenZipApp.CheckCommandLine: string;
var
  I: integer;
begin
  ProcessMessage(SevenZipPluginVer);
  case FCommandLine.Command of
    'L':
    begin
      FMemOutputProc := ProcessListOutput;
      Result := SevenZipPlugin + ' l -slt';
    end;
    'T':
    begin
      FMemOutputProc := ProcessTestOutput;
      Result := SevenZipPlugin + ' t';
    end;
    'E':
    begin
      FMemOutputProc := ProcessTestOutput;
      Result := SevenZipPlugin + ' e';
    end;
    'X':
    begin
      FMemOutputProc := ProcessTestOutput;
      Result := SevenZipPlugin + ' x';
    end;
    else
    begin
      ProcessFatalError(SevenZipPluginVer + ' - error : command line unsupported', 255);
      FMemOutputProc := nil;
    end;
  end;

  if Assigned(FMemOutputProc) then
  begin
    if FCommandLine.rOption then
      Result := Result + ' -r';

    Result := Result + ' "' + FCommandLine.ArchiveName + '"';
    for I := 0 to FCommandLine.FileMasks.Count - 1 do
    begin
      Result := Result + ' "' + FCommandLine.FileMasks[I] + '"';
    end;
  end
  else
    Result := '';
end;

procedure TSevenZipApp.Execute;
var
  Count:      integer;
  BytesRead:  int64;
  FProcess:   TProcess;
  FCommandLine: string;
  FMemOutput: TMemoryStream;
  FMemOutputStrings: TStringList;
begin
  inherited Execute;
  FCommandLine := CheckCommandLine;
  if Assigned(FMemOutputProc) then
  begin
    FMemOutput := TMemoryStream.Create;
    FProcess   := TProcess.Create(nil);
    FProcess.CommandLine := FCommandLine;
    FProcess.Options := [poNoConsole, poUsePipes];

    BytesRead := 0;
    FProcess.Execute;
    while FProcess.Running do
    begin
      FMemOutput.SetSize(BytesRead + 2048);
      Count := FProcess.Output.Read((FMemOutput.Memory + BytesRead)^, 2048);

      if Count > 0 then
        Inc(BytesRead, Count)
      else
        Sleep(100);

      if FTerminated then
      begin
        FProcess.Terminate(255);
      end;
    end;

    repeat
      FMemOutput.SetSize(BytesRead + 2048);
      Count := FProcess.Output.Read((FMemOutput.Memory + BytesRead)^, 2048);
      if Count > 0 then
      begin
        Inc(BytesRead, Count);
      end;
    until Count <= 0;
    FMemOutput.SetSize(BytesRead);

    if not FTerminated then
    begin
      FMemOutputStrings := TStringList.Create;
      FMemOutputStrings.LoadFromStream(FMemOutput);
      FMemOutputProc(FMemOutputStrings);
      FMemOutputStrings.Free;
    end;
    FProcess.Free;
    FMemOutput.Free;
  end;

  if FCode = ccSuccesful then
    ProcessMessage(Cr + 'Everything went ok - ' + TimeDifference(FStartTime) +
      ' seconds')
  else
    ProcessMessage(Cr + 'Process aborted - ' + TimeDifference(FStartTime) + ' seconds');

  FTerminated := True;
end;

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

procedure TSevenZipApp.CheckOverwrite;
begin

end;

end.
