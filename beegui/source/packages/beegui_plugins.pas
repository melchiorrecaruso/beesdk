{
    Copyright (c) 2008 Andrew Filinsky and Melchiorre Caruso

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

{   Contains:

      T7zApp class.

    Modifyed:

      v1.0.5 build 0026 - 2009.02.16 by Melchiorre Caruso.
}

unit BeeGui_Plugins;

{$I Compiler.inc}

interface

uses
  Dialogs,
  Classes,
  Process,
  SysUtils,
  // --
  Bee_Interface,
  Bee_CommandLine,
  // --
  BeeGui_SysUtils;

const
  SevenZipPluginVer = '7Zip Plugin ver 0.1.40';
  
type
  TProcessOutput = procedure(FOutput: TStringList) of object;
  
type
  TSevenZipApp = class (TApp)
  private
    FCommandLine: TCommandLine;
    FMemOutputProc: TProcessOutput;
  private
    procedure ProcessListOutput(FOutput: TStringList);
    procedure ProcessTestOutput(FOutput: TStringList);

    function  CheckCommandLine: string;
    procedure CheckOverwrite;
  public
    constructor Create(aInterface: TInterfaces; aParams: TParams);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { plugins routines }

  function SevenZipPlugin: string;

implementation

uses
  BeeGui_Consts;

  /// SevenZipPlungin function
  
  function SevenZipPlugin: string;
  begin
    {$IFDEF UNIX}
    Result := IncludeTrailingBackSlash(GetApplicationPluginsDir) + '7za';
    {$ELSE}
      {$IFDEF MSWINDOWS}
      Result := IncludeTrailingBackSlash(GetApplicationPluginsDir) + '7za.exe'
      {$ELSE}
      Result := '';
      {$ENDIF}
    {$ENDIF}
  end;
  
  /// TSevenZipApp class ...

  constructor TSevenZipApp.Create(aInterface: TInterfaces; aParams: TParams);
  begin
    inherited Create(aInterface);
    FCommandLine := TCommandLine.Create;
    FCommandLine.Process(aParams);
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
    FMemOutputProc := nil;
    case FCommandLine.Command of
    'L': begin
           FMemOutputProc := ProcessListOutput;
           Result := SevenZipPlugin + ' l -slt ';
         end;
    'T': begin
           FMemOutputProc := ProcessTestOutput;
           Result := SevenZipPlugin + ' t ';
         end;
    'E': begin
           FMemOutputProc := ProcessTestOutput;
           Result := SevenZipPlugin + ' e ';
         end;
    'X': begin
           FMemOutputProc := ProcessTestOutput;
           Result := SevenZipPlugin + ' x ';
         end;
    else begin
           Interfaces.OnError.Data.Msg := SevenZipPluginVer + ' - error : command line unsupported';
           Synchronize(Interfaces.OnError.Method);
           Result := '';
         end;
    end;

    if FCommandLine.rOption then Result := Result + ' -r ';

    FCommandLine.ArchiveName := '"' + FCommandLine.ArchiveName + '"';

    for I := 0 to FCommandLine.FileMasks.Count -1 do
    begin
      FCommandLine.FileMasks[I] := '"' + FCommandLine.FileMasks[I] + '"';
    end;
    ShowMessage(Result);
  end;

  procedure TSevenZipApp.Execute;
  var
    Count: integer;
    BytesReaded: integer;
    // --
    FProcess: TProcess;
    FCommandLine: string;
    FMemOutput: TMemoryStream;
    FMemOutputStrings: TStringList;
  begin
    FCommandLine := CheckCommandLine;
    if (Assigned(FMemOutputProc)) then
    begin
      BytesReaded := 0;
      // --
      Interfaces.OnDisplay.Data.Msg := '7z plugin running...';
      Synchronize(Interfaces.OnDisplay.Method);
      // --
      FMemOutput:= TMemoryStream.Create;
      // --
      FProcess := TProcess.Create(nil);
      FProcess.Options := [poUsePipes, poNoConsole];
      FProcess.CommandLine := FCommandLine;
      FProcess.Execute;
      while FProcess.Running do
      begin
        FMemOutput.SetSize(BytesReaded + 2048);
        Count := FProcess.Output.Read((FMemOutput.Memory + BytesReaded)^, 2048);
        if Count > 0 then
        begin
          Inc(BytesReaded, Count);
        end else
        begin
          Sleep(100);
        end;
      end;
      // --
      repeat
        FMemOutput.SetSize(BytesReaded + 2048);
        Count := FProcess.Output.Read((FMemOutput.Memory + BytesReaded)^, 2048);
        if Count > 0 then
        begin
          Inc(BytesReaded, Count);
        end;
      until Count <= 0;
      FMemOutput.SetSize(BytesReaded);
      // --
      FMemOutputStrings := TStringList.Create;
      FMemOutputStrings.LoadFromStream(FMemOutput);
      FMemOutputProc(FMemOutputStrings);
      // --
      FMemOutputStrings.Free;
      FMemOutput.Free;
      FProcess.Free;
      // --
      Interfaces.OnDisplay.Data.Msg := '';
      Synchronize(Interfaces.OnDisplay.Method);
    end;
  end;
  
  procedure TSevenZipApp.ProcessListOutput(FOutput: TStringList);
  var
    i: integer;
    ItemStr: string;
    ItemStrSwap: string;
    Item: TAppListItem;
  begin
    Synchronize(AppInterface.OnList);
    AppInterface.cList.Clear;
    // --
    i := 0;
    while i < FOutput.Count do
    begin
      AppInterface.cPercentage := (100 * i) div FOutput.Count;
      Synchronize(AppInterface.OnTick);
    
      ItemStr := FOutput.Strings[i];
      if AnsiPosText('Error: ', ItemStr) = 1 then
      begin
        AppInterface.cPercentage := 0;
        Synchronize(AppInterface.OnError);
      end else
      if AnsiPosText('Listing archive: ', ItemStr) = 1 then
      begin
        AppInterface.cMsg :=ItemStr;
        Synchronize(AppInterface.OnDisplay);
      end else
      if AnsiPosText('Path = ', ItemStr) = 1 then
      begin
        Delete(ItemStr, 1, Length('Path = '));
        Item := TAppListItem.Create;
        Item.FileName := ExtractFileName(ItemStr);
        Item.FilePath := ExtractFilePath(ItemStr);
        Item.FileVersion := 'unknow';
        Item.FileAttr := 0;

        Inc(i);
        while i < FOutput.Count do
        begin
          ItemStr := FOutput.Strings[i];
          if AnsiPosText('Size = ', ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length('Size = '));
            try
              Item.FileSize := StrToInt(ItemStr);
            except
              Item.FileSize := -1;
              AppInterface.cMsg := 'Error: reading file size';
              Synchronize(AppInterface.OnError);
            end;
          end else
          if AnsiPosText('Packed Size = ', ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length('Packed Size = '));
            try
              Item.FilePacked := StrToInt(ItemStr);
            except
              Item.FilePacked := -1;
              AppInterface.cMsg := 'Error: reading file packed-size';
              Synchronize(AppInterface.OnError);
            end;
          end else
          if AnsiPosText('Modified = ', ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length('Modified = '));
            SetLength(ItemStr, 16);
            ItemStrSwap  := ItemStr;
            ItemStr [ 1] := ItemStrSwap [ 9];
            ItemStr [ 2] := ItemStrSwap [10];
            ItemStr [ 3] := '/';
            ItemStr [ 4] := ItemStrSwap [ 6];
            ItemStr [ 5] := ItemStrSwap [ 7];
            ItemStr [ 6] := '/';
            ItemStr [ 7] := ItemStrSwap [ 1];
            ItemStr [ 8] := ItemStrSwap [ 2];
            ItemStr [ 9] := ItemStrSwap [ 3];
            ItemStr [10] := ItemStrSwap [ 4];
            ItemStr [14] := '.';
            try
              Item.FileTime := DateTimeToFileDate(StrToDateTime(ItemStr));
            except
              Item.FileTime := -1;
              AppInterface.cMsg := 'Error: reading file date-time';
              Synchronize(AppInterface.OnError);
            end;
          end else
          if AnsiPosText('Attributes = ', ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length('Attributes = '));
            if Pos('D', ItemStr) > 0 then Item.FileAttr := Item.FileAttr or faDirectory;
            if Pos('R', ItemStr) > 0 then Item.FileAttr := Item.FileAttr or faReadOnly;
            if Pos('H', ItemStr) > 0 then Item.FileAttr := Item.FileAttr or faHidden;
            if Pos('S', ItemStr) > 0 then Item.FileAttr := Item.FileAttr or faSysFile;
            if Pos('A', ItemStr) > 0 then Item.FileAttr := Item.FileAttr or faArchive;
          end else
          if AnsiPosText('Encrypted = ', ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length('Encrypted = '));
            if ItemStr = '0' then
              Item.FilePassword := 'No'
            else
              if ItemStr = '1' then
                Item.FilePassword := 'Yes'
              else
                Item.FilePassword := '?';
          end else
          if AnsiPosText('Comment = ', ItemStr) = 1 then
          begin
            Delete (ItemStr, 1, Length('Comment = '));
            Item.FileComm := ItemStr;
          end else
          if AnsiPosText('Method = ', ItemStr) = 1 then
          begin
            Delete (ItemStr, 1, Length('Method = '));
            Item.FileMethod := ItemStr;
          end else
          if AnsiPosText('Error: ', ItemStr) = 1 then
          begin
            AppInterface.cMsg := ItemStr;
            Synchronize(AppInterface.OnError);
            // ---
            Break;
          end else
          if AnsiPosText('Path = ', ItemStr) = 1 then
          begin
            Dec(i);
            Break;
          end;
          Inc(i);
        end;

        if ((Item.FileAttr and faDirectory) = 0) then
        begin
          AppInterface.cList.Add(Item);
        end else
        begin
          Item.Free;
        end;
      end;
      Inc(i);
    end;
  end;
  
  procedure TSevenZipApp.ProcessTestOutput(FOutput: TStringList);
  begin
    AppInterface.cMsg := FOutput.Text;
    Synchronize(AppInterface.OnDisplay);
  end;
  
  procedure TSevenZipApp.CheckOverwrite;
  begin
  
  end;

end.

