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

      TSevenZipApp class.

    Modifyed:

      v1.0.5 build 0642 - 2009.02.16 by Melchiorre Caruso.
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
  TSevenZipApp = class(TApp)
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

  function SevenZipPlugin: string; overload;
  function SevenZipPlugin(const aArchiveName: string): boolean; overload;

implementation

uses
  Bee_Common,
  BeeGui_Consts;

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

  SevenZipListMark     = 'Listing archive: ';
  SevenZipCommentMark  = 'Comment = ';
  SevenZipErrorMark    = 'Error: ';






  /// SevenZipPlungin function
  
  function SevenZipPlugin: string;
  begin
    {$IFDEF UNIX}
    Result := IncludeTrailingBackSlash(GetApplicationPluginsDir) + '7z/7za';
    {$ELSE}
      {$IFDEF MSWINDOWS}
      Result := IncludeTrailingBackSlash(GetApplicationPluginsDir) + '7z\7za.exe';
      {$ELSE}
      Result := '';
      {$ENDIF}
    {$ENDIF}
    if (Result <> '') and (FileExists(Result) = False) then
    begin
      Result := '';
    end;
  end;

  function SevenZipPlugin(const aArchiveName: string): boolean; overload;
  var
    FExt: string;
  begin
    FExt := ExtractFileExt(aArchiveName);

    if CompareFileName(FExt, '.zip'  ) then Result := True else
    if CompareFileName(FExt, '.gz'   ) then Result := True else
    if CompareFileName(FExt, '.gzip' ) then Result := True else
    if CompareFileName(FExt, '.tgz'  ) then Result := True else
    if CompareFileName(FExt, '.bz2'  ) then Result := True else
    if CompareFileName(FExt, '.bzip2') then Result := True else
    if CompareFileName(FExt, '.tbz2' ) then Result := True else
    if CompareFileName(FExt, '.tbz'  ) then Result := True else
    if CompareFileName(FExt, '.tar'  ) then Result := True else
    if CompareFileName(FExt, '.lzma' ) then Result := True else
    if CompareFileName(FExt, '.rar'  ) then Result := True else
    if CompareFileName(FExt, '.cab'  ) then Result := True else
    if CompareFileName(FExt, '.arj'  ) then Result := True else
    if CompareFileName(FExt, '.z'    ) then Result := True else
    if CompareFileName(FExt, '.taz'  ) then Result := True else
    if CompareFileName(FExt, '.cpio' ) then Result := True else
    if CompareFileName(FExt, '.rpm'  ) then Result := True else




 'deb',
 'lzh',
 'lha',

 chm chw hxs
 iso
 iso
 msi doc xls ppt
 wim swm
 dmg
 xar
 hfs
 exe


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
    case FCommandLine.Command of
    'L': begin
           FMemOutputProc := ProcessListOutput;
           Result := SevenZipPlugin + ' l -slt';
         end;
    'T': begin
           FMemOutputProc := ProcessTestOutput;
           Result := SevenZipPlugin + ' t';
         end;
    'E': begin
           FMemOutputProc := ProcessTestOutput;
           Result := SevenZipPlugin + ' e';
         end;
    'X': begin
           FMemOutputProc := ProcessTestOutput;
           Result := SevenZipPlugin + ' x';
         end;
    else begin
           Interfaces.OnError.Data.Msg := SevenZipPluginVer + ' - error : command line unsupported';
           Synchronize(Interfaces.OnError.Method);
           FMemOutputProc := nil;
           SetExitCode(255);
         end;
    end;

    if Assigned(FMemOutputProc) then
    begin
      if FCommandLine.rOption then Result := Result + ' -r';

      Result := Result + ' "' + FCommandLine.ArchiveName + '"';
      for I := 0 to FCommandLine.FileMasks.Count -1 do
      begin
        Result := Result +  ' "' + FCommandLine.FileMasks[I] + '"';
      end;
    end else
      Result := '';
  end;

  procedure TSevenZipApp.Execute;
  var
    Count: integer;
    BytesReaded: integer;
    // ---
    FProcess: TProcess;
    FMemOutput: TMemoryStream;
    FMemOutputStrings: TStringList;
  begin
    if Assigned(FMemOutputProc) then
    begin
      Interfaces.OnError.Data.Msg := '7Zip plugin running...';
      Synchronize(Interfaces.OnError.Method);

      FMemOutput:= TMemoryStream.Create;
      FProcess := TProcess.Create(nil);
      FProcess.Options := [poUsePipes, poNoConsole];
      FProcess.CommandLine := CheckCommandLine;


      Interfaces.OnError.Data.Msg := FProcess.CommandLine;
      Synchronize(Interfaces.OnError.Method);




      FProcess.Execute;

      BytesReaded := 0;
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
      Interfaces.OnError.Data.Msg := FMemOutputStrings.Text;
      Synchronize(Interfaces.OnError.Method);
    end;
  end;
  
  procedure TSevenZipApp.ProcessListOutput(FOutput: TStringList);
  var
    I: integer;
    ItemStr: string;
    ItemStrSwap: string;
  begin
    I := 0;
    while I < FOutput.Count do
    begin
      ItemStr := FOutput.Strings[I];
      // Error
      if AnsiPosText(SevenZipErrorMArk, ItemStr) = 1 then
      begin
        Delete(ItemStr, 1, Length(SevenZipErrorMArk));
        Interfaces.OnError.Data.Msg := ItemStr;
        Synchronize(Interfaces.OnError.Method);
        SetExitCode(255);
      end else
      // List archive
      if AnsiPosText(SevenZipListMark, ItemStr) = 1 then
      begin
        Interfaces.OnDisplay.Data.Msg := 'Listing archive...';
        Synchronize(Interfaces.OnDisplay.Method);
      end else
      // File Path - New
      if AnsiPosText(SevenZipPathMark, ItemStr) = 1 then
      begin
        Delete(ItemStr, 1, Length(SevenZipPathMark));
        Interfaces.OnList.Data.FileName := ExtractFileName(ItemStr);
        Interfaces.OnList.Data.FilePath := ExtractFilePath(ItemStr);

        Inc(I);
        while I < FOutput.Count do
        begin
          ItemStr := FOutput.Strings[I];
          // File Size
          if AnsiPosText(SevenZipSizeMark, ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length(SevenZipSizeMark));
            try
              Interfaces.OnList.Data.FileSize := StrToInt(ItemStr);
            except
              Interfaces.OnList.Data.FileSize := -1;
              Interfaces.OnError.Data.Msg := 'Error: reading file size';
              Synchronize(Interfaces.OnError.Method);
            end;
          end else
          // File Packed Size
          if AnsiPosText(SevenZipPackedMark, ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length(SevenZipPackedMark));
            try
              Interfaces.OnList.Data.FilePacked := StrToInt(ItemStr);
            except
              Interfaces.OnList.Data.FilePacked := -1;
              Interfaces.OnError.Data.Msg := 'Error: reading file packed-size';
              Synchronize(Interfaces.OnError.Method);
            end;
          end else
          // File Time
          if AnsiPosText(SevenZipTimeMark, ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length(SevenZipTimeMark));
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
              Interfaces.OnList.Data.FileTime := DateTimeToFileDate(StrToDateTime(ItemStr));
            except
              Interfaces.OnList.Data.FileTime := -1;
              Interfaces.OnError.Data.Msg := 'Error: reading file date-time';
              Synchronize(Interfaces.OnError.Method);
            end;
          end else
          // File Attributes
          if AnsiPosText(SevenZipAttrMark, ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length(SevenZipAttrMark));
            Interfaces.OnList.Data.FileAttr := 0;
            with Interfaces.OnList.Data do
            begin
              if Pos('D', ItemStr) > 0 then FileAttr := FileAttr or faDirectory;
              if Pos('R', ItemStr) > 0 then FileAttr := FileAttr or faReadOnly;
              if Pos('H', ItemStr) > 0 then FileAttr := FileAttr or faHidden;
              if Pos('S', ItemStr) > 0 then FileAttr := FileAttr or faSysFile;
              if Pos('A', ItemStr) > 0 then FileAttr := FileAttr or faArchive;
            end;
          end else
          // File Password
          if AnsiPosText(SevenZipPasswordMark, ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length(SevenZipPasswordMark));
            if ItemStr = '-' then
              Interfaces.OnList.Data.FilePassword := 'No'
            else
              if ItemStr = '+' then
                Interfaces.OnList.Data.FilePassword := 'Yes'
              else
                Interfaces.OnList.Data.FilePassword := '?';
          end else
          // File Comment
          if AnsiPosText(SevenZipCommentMark, ItemStr) = 1 then
          begin
            Delete(ItemStr, 1, Length(SevenZipCommentMark));
            Interfaces.OnList.Data.FileComm := ItemStr;
          end else
          // File Method
          if AnsiPosText(SevenZipMethodMark, ItemStr) = 1 then
          begin
            Delete (ItemStr, 1, Length(SevenZipMethodMark));
            Interfaces.OnList.Data.FileMethod := ItemStr;
          end else
          // Error
          if AnsiPosText(SevenZipErrorMark, ItemStr) = 1 then
          begin
            Interfaces.OnError.Data.Msg := ItemStr;
            Synchronize(Interfaces.OnError.Method);
            SetExitCode(255);
            Break;
          end else
          // Next -->
          if AnsiPosText(SevenZipPathMark, ItemStr) = 1 then
          begin
            Dec(I);
            Break;
          end;
          Inc(I);
        end;

        //
        if (Interfaces.OnList.Data.FileAttr and faDirectory) = 0 then
        begin
          Synchronize(Interfaces.OnList.Method);
        end;
      end;
      Inc(I);
    end;
  end;
  
  procedure TSevenZipApp.ProcessTestOutput(FOutput: TStringList);
  begin
    Interfaces.OnDisplay.Data.Msg := FOutput.Text;
    Synchronize(Interfaces.OnDisplay.Method);
  end;
  
  procedure TSevenZipApp.CheckOverwrite;
  begin
  
  end;

end.

