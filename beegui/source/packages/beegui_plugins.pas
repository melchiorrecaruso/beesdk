{
    Copyright (c) 2006 Andrew Filinsky and Melchiorre Caruso

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

    v1.0.3 build 0026 - 2006/12/13 by Melchiorre Caruso.
}

unit BeeGui_Plugins;

{$I Compiler.inc}

interface

uses
  Classes,
  Process,
  SysUtils,
  // --
  Bee_Interface,
  // --
  BeeGui_SysUtils;

const
  SevenZipPluginVer = '7zip Plugin ver 0.1.40';
  
type
  TProcessOutput = procedure(FOutput: TStringList) of object;
  
type
  T7zApp = class (TApp)
  private
    FMemOutputProc: TProcessOutput;
  private
    procedure ProcessListOutput(FOutput: TStringList);
    procedure ProcessTestOutput(FOutput: TStringList);
    function  TranslateCommandLine: string;
    procedure CheckOverwrite;
  public
    constructor Create (aAppInterface: TAppInterface; aAppParams: TStringList);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { directories routines }
  
  function GetApplicationPluginsDir: string;

  { plugins routines }

  function _7zPlugin: string;

implementation

uses
  BeeGui_Consts;

/// directories routines

  function GetApplicationPluginsDir: string;
  begin
    Result := ExtractFilePath(ParamStr(0)) + cApplicationPluginsFolder;
  end;
  
/// 7zPlungin function
  
  function _7zPlugin: string;
  begin
    Result := '';
    {$IFDEF UNIX}
      // Result := '7z';
      Result := AnsiIncludeTrailingBackSlash(GetApplicationPluginsDir) +
        '7za' + PathDelim + 'linux' + PathDelim + 'bin' + PathDelim + '7za'
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      Result := AnsiIncludeTrailingBackSlash(GetApplicationPluginsDir) +
        '7za' + PathDelim + 'mswindows' + PathDelim + '7za.exe'
    {$ENDIF}
  end;
  
/// T7zApp class ...

  constructor T7zApp.Create (aAppInterface: TAppInterface; aAppParams: TStringList);
  begin
    inherited Create(aAppInterface, aAppParams);
  end;

  destructor T7zApp.Destroy;
  begin
    inherited Destroy;
  end;

  function T7zApp.TranslateCommandLine: string;
  var
    i: integer;
    Command: string;
    Options: string;
    ArcName: string;
    FileNames: string;
  begin
    Result := '';
    Command := '';
    Options := ' -y ';
    ArcName := '';
    FileNames := '';
    FMemOutputProc := nil;
    // --
    for i := 0 to AppParams.Count -1 do
    begin
      if i = 0 then
      begin
        if Length(AppParams.Strings[i]) <> 1 then Exit;
        // --
        case System.UpCase(AppParams.Strings[i][1]) of
         'L': begin
                Command := _7zPlugin + ' l -slt ';
                FMemOutputProc := ProcessListOutput;
              end;
         'T': begin
                Command := _7zPlugin + ' t ';
                FMemOutputProc := ProcessTestOutput;
              end;
         'E': begin
                Command := _7zPlugin + ' e ';
                FMemOutputProc := ProcessTestOutput;
              end;
         'X': begin
                Command := _7zPlugin + ' x ';
                FMemOutputProc := ProcessTestOutput;
              end;
         else begin
                AppInterface.cMsg := _7zPluginVer + ' - error : command line unsupported';
                Synchronize(AppInterface.OnError);
              end;
        end;
      end else
        if (Length(AppParams.Strings[i]) > 1) and (AppParams.Strings[i][1] = '-') then
        begin
          case System.UpCase(AppParams.Strings[i][1]) of
           'R': Options := Options + ' -r ';
          end;
        end else
          if ArcName = '' then
          begin
            ArcName := ' "' + AppParams.Strings[i] + '" ';
          end else
          begin
            if AppParams.Strings[i] = ('*' + PathDelim + '*') then
              FileNames := FileNames + ' * '
            else
              FileNames := FileNames + ' "' + AppParams.Strings[i] + '" ';
          end;
    end;
    Result := Command + Options + ArcName + FileNames;
  end;

  procedure T7zApp.Execute;
  var
    Count: integer;
    BytesReaded: integer;
    // --
    FProcess: TProcess;
    FCommandLine: String;
    FMemOutput: TMemoryStream;
    FMemOutputStrings: TStringList;
  begin
    FCommandLine := TranslateCommandLine;
    if (Assigned(FMemOutputProc)) then
    begin
      BytesReaded := 0;
      // --
      AppInterface.cMsg := '7z plugin running...';
      Synchronize(AppInterface.OnDisplay);
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
      AppInterface.cMsg := '';
      Synchronize(AppInterface.OnDisplay);
    end;
  end;
  
  procedure T7zApp.ProcessListOutput(FOutput: TStringList);
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
  
  procedure T7zApp.ProcessTestOutput(FOutput: TStringList);
  begin
    AppInterface.cMsg := FOutput.Text;
    Synchronize(AppInterface.OnDisplay);
  end;
  
  procedure T7zApp.CheckOverwrite;
  begin
  
  end;

end.

