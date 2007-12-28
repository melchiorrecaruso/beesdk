{
  Copyright (c) 2003-2007 Andrew Filinsky and Melchiorre Caruso

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

  Bee archiver shell.

  Modifyed:

  v0.7.8 build 0150 - 2005.06.27 by Melchiorre Caruso;
  v0.7.8 build 0153 - 2005.07.08 by Andrew Filinsky;
  v0.7.8 build 0154 - 2005.07.23 by Melchiorre Caruso;
  v0.7.9 build 0298 - 2006.01.05 by Melchiorre Caruso;
  v0.7.9 build 0301 - 2007.01.23 by Andrew Filinsky;
  v0.7.9 build 0316 - 2007.02.16 by Andrew Filinsky;

  v0.7.9 build 0561 - 2007.12.24 by Melchiorre Caruso.
}

unit Bee_App;

{$I compiler.inc}

interface

uses
  Classes, // TStringList, ...
  //
  Bee_Headers,
  Bee_Interface,
  Bee_Configuration; // TConfiguration, TTable;

type
  TBeeApp = class(TApp)
  public
    constructor Create(aAppInterface: TAppInterface; aAppParams: TStringList);
    destructor Destroy; override;
    procedure Execute; override;
    function Tick: boolean;
  public
    GeneralSize: integer;
    RemainSize:  integer;
  private
    function OpenArchive(Headers: THeaders; aAction: THeaderAction): boolean;

    procedure ProcessOptions;
    procedure ProcessMasks;
    // decode solid sequences using a swapfile
    function ProcessFilesToSwap(Headers: THeaders): boolean;
    // find and prepare sequences
    procedure ProcessFilesToFresh(Headers: THeaders);
    procedure ProcessFilesToDelete(Headers: THeaders);
    procedure ProcessFilesToDecode(Headers: THeaders; aAction: THeaderAction);
    procedure ProcessFilesToExtract(Headers: THeaders);
    procedure ProcessFilesToOverWrite(Headers: THeaders);

    procedure ProcessFilesDeleted(Headers: THeaders);
    // overwrite sub-routines
    procedure ProcessFilesToOverWriteDefault(Headers: THeaders);
    procedure ProcessFilesToOverWriteAdvanced(Headers: THeaders);
    procedure ProcessFileToOverWrite(Headers: THeaders; FileIndex: integer);
    //
    function AlreadyFileExists(Headers: THeaders; FileIndex: integer;
      FileActions: THeaderActions; const FileName: string): integer;
    //
    procedure ProcesstOption;
    procedure ProcesslOption;
    //
    procedure DisplayUsage;
    procedure EncodeShell;
    procedure DecodeShell(Action: THeaderAction);
    procedure RenameShell;
    procedure DeleteShell;
    procedure ListShell;
    //
    procedure SetPriority(aPriority: integer);
  protected
    // string routines
    function VersionToStr(VersionId: cardinal): string;
    function MethodToStr(P: THeader; Method, Dictionary: integer): string;

  private
    SelfName: string;

    ArcName:  string;  // archive file name
    ArcFile:  TStream; // archive file stream
    SwapName: string;  // swap file name
    SwapFile: TStream; // swap file stream

    CfgName: string;
    Cfg: TConfiguration;

    Command: char;     // command
    aOption: string;
    cdOption: string;
    eOption: string;   // forced file extension
    fOption: boolean;
    kOption: boolean;
    lOption: boolean;
    oOption: char;
    pOption: boolean;
    rOption: boolean;
    sOption: boolean;
    tOption: boolean;
    uOption: boolean;
    xOption: TStringList;
    yOption: string;

    FileMasks: TStringList; // file masks
  end;

implementation

uses
  Math,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, // faReadOnly, ...

  Bee_Files,
  Bee_Common, // Various helper routines
  Bee_Assembler,
  Bee_MainPacker; // TEncoder...

// TBeeApp ...

constructor TBeeApp.Create(aAppInterface: TAppInterface; aAppParams: TStringList);
begin
  inherited Create(aAppInterface, aAppParams);
  Randomize; // randomize, uses for unique filename generation...

  SelfName := 'The Bee 0.7.9 build 0565 archiver utility, freeware version, Dec 2007.'
    + Cr + '(C) 1999-2007 Andrew Filinsky and Melchiorre Caruso.';

  ArcName  := '';
  ArcFile  := nil;
  SwapName := '';
  SwapFile := nil;

  GeneralSize := 0;
  RemainSize  := 0;

  CfgName := Bee_Common.SelfPath + 'bee.ini';
  Cfg := TConfiguration.Create;

  Command := ' ';
  aOption := '';
  cdOption := '';
  eOption := ''; // forced file extension
  fOption := False;
  kOption := False;
  lOption := False;
  oOption := 'Y';
  pOption := False;
  rOption := False;
  sOption := False;
  tOption := False;
  uOption := False;
  xOption := TStringList.Create;
  yOption := '';

  FileMasks := TStringList.Create;

  ProcessOptions; // process options
  ProcessMasks; // porcess masks
end;

destructor TBeeApp.Destroy;
begin
  Cfg.Free;
  xOption.Free;
  FileMasks.Free;
  inherited Destroy;
end;

procedure TBeeApp.DisplayUsage;
begin
                                       AppInterface.OnDisplay.Data.Msg := (Cr + '  Usage: Bee <Command> -<Option 1> -<Option N> <ArchiveName> <FileNames...>');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := (Cr + '  Commands:' + Cr);
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    a   Add files to archive');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    d   Delete files from archive');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    e   Extract files from archive');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    x   eXtract files from archive with path name');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    l   List archive');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    t   Test archive files');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    r   Rename files in archive');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := (Cr + '  Options:' + Cr);
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    r       Recurse subdirectories');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    u       Update files');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    f       Freshen files');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    e       force file Extention');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    s       create Solid archive');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    a       add self-extrActor module');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    o<mode> set overwrite file Mode (Q-Query (default), A-All, S-Skip all)');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    m<0..3> set compression Method (0-store...1-default...3-maximal)');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    d<0..9> set Dictionary size (d1 uses < 5M, d2 (default) < 10M, d3 < 20M...)' + Cr);
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    x       eXclude filenames');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    t       Test archive after process');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    l       List archive after process');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    y       set temporany directory');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    k       use blowfish crypter/decrypter (min key-length 4 bytes)');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    cd<dir> set current archive directory' + Cr);
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    cfg<filename> use specified configuration file');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := ('    pri<priority> set process Priority (0-Idle, 1-Normal, 2-High, 3-RealTime)');
  Sync(AppInterface.OnDisplay.Method); AppInterface.OnDisplay.Data.Msg := (Cr + '  Use BeeOpt to make most optimal parameters.');
  Sync(AppInterface.OnDisplay.Method);
end;

procedure TBeeApp.Execute;
const
  SetOfCommands = ['A', 'D', 'E', 'L', 'R', 'T', 'X'];
begin
  AppInterface.OnDisplay.Data.Msg := SelfName;
  Sync(AppInterface.OnDisplay.Method);
  /// process command
  if ((Command in SetOfCommands) and (ArcName > '')) or (Command = '?') then
    case Command of
      'A': EncodeShell;
      'D': DeleteShell;
      'E': DecodeShell(toExtract);
      'L': ListShell;
      'R': RenameShell;
      'T': DecodeShell(toTest);
      'X': DecodeShell(toExtract);
      '?': DisplayUsage;
    end
  else
    DisplayUsage;
end;

function TBeeApp.Tick: boolean;
begin
  AppInterface.OnTick.Data.Percentage := MulDiv(RemainSize, 100, GeneralSize);
  Sync(AppInterface.OnTick.Method);
  Result := Terminated;
end;

procedure TBeeApp.SetPriority(aPriority: integer); // Priority is 0..3
begin
  {$IFDEF CONSOLEAPPLICATION}
    {$IFDEF MSWINDOWS}
    Bee_Common.SetPriority(aPriority);
    {$ELSE}
    case aPriority of
      0: Priority := tpIdle;
      1: Priority := tpNormal;
      2: Priority := tpHigher;
      3: Priority := tpTimeCritical;
      else
        Priority := tpNormal;
    end;
    {$ENDIF}
  {$ELSE}
  case aPriority of
    0: Priority := tpIdle;
    1: Priority := tpNormal;
    2: Priority := tpHigher;
    3: Priority := tpTimeCritical;
    else
      Priority := tpNormal;
  end;
  {$ENDIF}
end;

function TBeeApp.OpenArchive(Headers: THeaders; aAction: THeaderAction): boolean;
begin
  Result := True;
  if FileExists(ArcName) then
  begin
    try
      ArcFile := TFileReader.Create(ArcName, fmOpenRead + fmShareDenyWrite);
      Headers.ReadItems(ArcFile, aAction);
      if (Headers.Count = 0) and (ArcFile.Size <> 0) then
      begin
        AppInterface.OnFatalError.Data.Msg := ('Error: can''t open archive');
        Sync(AppInterface.OnFatalError.Method);
        Result := False;
      end;
    except
      AppInterface.OnFatalError.Data.Msg := ('Error: can''t open archive');
      Sync(AppInterface.OnFatalError.Method);
      Result := False;
    end;
  end;
end;

// Options processing

procedure TBeeApp.ProcessOptions;
var
  I: integer;
  S: string;
begin
  // catch -cfg option
  for I := 0 to AppParams.Count - 1 do
  begin
    S := AppParams.Strings[I];
    if FileNamePos('-cfg', S) = 1 then
    begin
      Delete(S, 1, 4);
      CfgName := S;
    end;
  end;

  // default configuration
  Cfg.Selector('\main');
  Cfg.CurrentSection.Values['Method'] := '1';
  Cfg.CurrentSection.Values['Dictionary'] := '2';

  // process configuration
  if not FileExists(CfgName) then
  begin
    AppInterface.OnWarning.Data.Msg := (Cr + 'Configuration file ' + CfgName + ' not found, using default settings' + Cr);
    Sync(AppInterface.OnWarning.Method);
  end else
    Cfg.LoadFromFile(CfgName);

  // catch options, command, archive name and name of files
  for I := 0 to AppParams.Count - 1 do
  begin
    S := AppParams.Strings[I];
    if (Length(S) > 1) and (S[1] = '-') then
    begin
      // options...
      case UpCase(S[2]) of
        'S': sOption := True;
        'U': uOption := True;
        'F': fOption := True;
        'T': tOption := True;
        'L': lOption := True;
        'K': kOption := True;
        'R': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 rOption := True
               else
                 if (S = '-') then rOption := False;
             end;
        'Y': begin
               Delete(S, 1, 2);
               if Bee_Common.DirectoryExists(Bee_Common.ExcludeTrailingBackslash(S)) then
               begin
                 yOption := Bee_Common.ExcludeTrailingBackslash(S);
               end;
             end;
        'A': begin
               Delete(S, 1, 2);
               if (S = '+') or (Length(S) = 0) then
                 aOption := 'beesfx.bin'
               else
                 if (S = '-') then
                   aOption := 'beesfx.empty'
                 else
                   aOption := S;
             end;
        'M': begin
               Delete(S, 1, 2);
               if (Length(S)= 1) and (S[1] in ['0'..'3']) then
               begin
                 Cfg.Selector('\main');
                 Cfg.CurrentSection.Values['Method'] := S;
               end;
             end;
        'O': begin
               Delete(S, 1, 2);
               if (Length(S) = 1) and (UpCase(S[1]) in ['A', 'S', 'Q']) then
               begin
                 oOption := UpCase(S[1]);
               end;
             end;
        'D': begin
               Delete(S, 1, 2);
               if (Length(S)= 1) and (S[1] in ['0'..'9']) then
               begin
                 Cfg.Selector('\main');
                 Cfg.CurrentSection.Values['Dictionary'] := S;
               end;
             end;
        'E': begin
               Delete(S, 1, 2);
               if ExtractFileExt('.' + S) <> '.' then
               begin
                 eOption := ExtractFileExt('.' + S);
               end;
             end;
        'X': begin
               Delete(S, 1, 2);
               xOption.Add(S);
             end;
        else if FileNamePos('-pri', S) = 1 then
             begin
               Delete(S, 1, 4);
               if (Length(S) = 1) and (S[1] in ['0'.. '3']) then
               begin
                 SetPriority(StrToInt(S[1]));
               end;
             end else
             begin
               if FileNamePos('-cd', S) = 1 then
               begin
                 Delete(S, 1, 3);
                 cdOption := Bee_Common.IncludeTrailingBackslash(Bee_Common.FixDirName(S));
               end;
             end;
        end; // end case
    end else
    begin
      // command or filenames...
      if Command = ' ' then
      begin
        if Length(S) = 1 then
          Command := UpCase(S[1])
        else
          Command := '?';
      end else
        if ArcName = '' then
        begin
          ArcName := S;
          if ExtractFileExt(ArcName) = '' then
          begin
            ArcName := ChangeFileExt(ArcName, '.bee');
          end;
        end else
          FileMasks.Add(Bee_Common.DoDirSeparators(S));
    end;
  end; // end for loop
end;

procedure TBeeApp.ProcessMasks;
var
  I: integer;
begin
  if rOption then
  begin
    for I := 0 to FileMasks.Count - 1 do
    begin
      if System.Pos('!', FileMasks.Strings[I]) = 0 then
      begin
        FileMasks.Strings[I] := FileMasks.Strings[I] + '!';
      end;
    end;

    for I := 0 to xOption.Count - 1 do
    begin
      if System.Pos('!', xOption.Strings[I]) = 0 then
      begin
        xOption.Strings[I] := xOption.Strings[I] + '!';
      end;
    end;
  end;
  if FileMasks.Count = 0 then
  begin
    case Command of
      'A': FileMasks.Add('*!');
     {'D': nothing to do}
      'E': FileMasks.Add('*!');
      'L': FileMasks.Add('*!');
     {'R': nothing to do}
      'T': FileMasks.Add('*!');
      'X': FileMasks.Add('*!');
     {'?': nothing to do}
    end;
  end;
end;

procedure TBeeApp.ProcessFilesToExtract;
var
  I: integer;
begin
  if Command = 'E' then
  begin
    for I := 0 to Headers.Count - 1 do
      with THeader(Headers.Items[I]) do
        Name := ExtractFileName(Name);
  end else
    if Length(cdOption) > 0 then
    begin
      for I := 0 to Headers.Count - 1 do
        with THeader(Headers.Items[I]) do
          Name := Bee_Common.DeleteFilePath(cdOption, Name);
    end;
end;

// OvewWrite file processing

procedure TBeeApp.ProcessFilesToOverWrite;
begin
  if (uOption = False) and (fOption = False) then
    ProcessFilesToOverWriteDefault(Headers)
  else
    ProcessFilesToOverWriteAdvanced(Headers);
end;

// ProcessOverWrite sub-routines

procedure TBeeApp.ProcessFilesToOverWriteDefault(Headers: THeaders);
var
  I, J: integer;
  NewFileName: string;
begin
  I := 0;
  while I < Headers.Count do
  begin
    if (THeader(Headers.Items[I]).Action = toExtract) and
       (FileExists(THeader(Headers.Items[I]).Name) = True) then
    begin
      if (oOption in ['A', 'Q', 'S']) = False then
      begin
        repeat
          AppInterface.OnOverWrite.Data.FileName := ExtractFileName(THeader(Headers.Items[I]).Name);
          AppInterface.OnOverWrite.Data.FilePath := ExtractFilePath(THeader(Headers.Items[I]).Name);
          AppInterface.OnOverWrite.Data.FileSize := THeader(Headers.Items[I]).Size;
          AppInterface.OnOverWrite.Data.FileTime := THeader(Headers.Items[I]).Time;

          AppInterface.OnOverWrite.Answer := 'A';
          Sync(AppInterface.OnOverWrite.Method);
        until UpCase(AppInterface.OnOverWrite.Answer) in ['A', 'N', 'R', 'S', 'Q', 'Y'];

        oOption := UpCase(AppInterface.OnOverWrite.Answer);
      end;

      case UpCase(oOption) of
        'A': Break;
        'N': THeader(Headers.Items[I]).Action := toNone;
        'R': begin
               while True do
               begin
                 AppInterface.OnRename.Data.FileName := ExtractFileName(THeader(Headers.Items[I]).Name);
                 AppInterface.OnRename.Data.FilePath := ExtractFilePath(THeader(Headers.Items[I]).Name);
                 AppInterface.OnRename.Data.FileSize := THeader(Headers.Items[I]).Size;
                 AppInterface.OnRename.Data.FileTime := THeader(Headers.Items[I]).Time;

                 AppInterface.OnRename.Answer := '';
                 Sync(AppInterface.OnRename.Method);
                 
                 NewFileName := Bee_Common.FixFileName(AppInterface.OnRename.Answer);
                 if (FileExists(NewFileName) = True) or (AlreadyFileExists(Headers, I, [toExtract], NewFileName) <> -1) then
                 begin
                   AppInterface.OnWarning.Data.Msg := ('File "' + NewFileName + '" already exists!');
                   Sync(AppInterface.OnWarning.Method);
                 end else
                   Break;
               end;

               if Length(NewFileName) = 0 then
                 THeader(Headers.Items[I]).Action := toNone
               else
                 THeader(Headers.Items[I]).Name := NewFileName;
             end;
        'S': for J := I to Headers.Count - 1 do
             begin
               THeader(Headers.Items[J]).Action := toNone;
               I := J;
             end;
        'Q': for J := 0 to Headers.Count - 1 do
             begin
               THeader(Headers.Items[J]).Action := toNone;
               I := J;
             end;
      end;
    end;
    Inc(I);
  end;

  for I := 0 to Headers.Count - 1 do
  begin
    if (THeader(Headers.Items[I]).Action = toExtract) then
    begin
      J := Headers.GetBack(I - 1, toExtract, THeader(Headers.Items[I]).Name);

      if J > -1 then
      begin
        THeader(Headers.Items[J]).Action := toNone;
      end;
    end;
  end;
end;

procedure TBeeApp.ProcessFilesToOverWriteAdvanced(Headers: THeaders);
var
  I: integer;
begin
  if (uOption xor fOption) then
  begin

    if uOption then
    begin
      for I := 0 to Headers.Count - 1 do
        if (THeader(Headers.Items[I]).Action = toExtract) then
        begin
          if (FileExists(THeader(Headers.Items[I]).Name) = True) then
            THeader(Headers.Items[I]).Action := toNone
          else
            ProcessFileToOverWrite(Headers, I);
        end;
    end else
      for I := 0 to Headers.Count - 1 do
        if (THeader(Headers.Items[I]).Action = toExtract) then
        begin
          if (FileExists(THeader(Headers.Items[I]).Name) = False) then
            THeader(Headers.Items[I]).Action := toNone
          else
          begin
            if FileAge(THeader(Headers.Items[I]).Name) >=
              THeader(Headers.Items[I]).Time then
              THeader(Headers.Items[I]).Action := toNone
            else
              ProcessFileToOverWrite(Headers, I);
          end;
        end;
  end else

  if (uOption and fOption) then
  begin
    for I := 0 to Headers.Count - 1 do
      if FileExists(THeader(Headers.Items[I]).Name) = True then
      begin
        if FileAge(THeader(Headers.Items[I]).Name) >=
          THeader(Headers.Items[I]).Time then
          THeader(Headers.Items[I]).Action := toNone
        else
          ProcessFileToOverWrite(Headers, I);
      end else
        ProcessFileToOverWrite(Headers, I);
  end;

end;

function TBeeApp.AlreadyFileExists(Headers: THeaders; FileIndex: integer;
  FileActions: THeaderActions; const FileName: string): integer;
begin
  if Length(FileName) > 0 then
  begin
    Result := Headers.GetBack(FileIndex - 1, FileActions, FileName);
    if Result = -1 then
    begin
      Result := Headers.GetNext(FileIndex + 1, FileActions, FileName);
    end;
  end else
    Result := -1
end;

procedure TBeeApp.ProcessFileToOverWrite(Headers: THeaders; FileIndex: integer);
var
  J: integer;
begin
  J := Headers.GetBack(FileIndex - 1, toExtract, THeader(Headers.Items[FileIndex]).Name);

  if J > -1 then
  begin
    if (THeader(Headers.Items[FileIndex]).Time > THeader(Headers.Items[J]).Time) then
      THeader(Headers.Items[J]).Action := toNone
    else
      THeader(Headers.Items[FileIndex]).Action := toNone;
  end;
end;

// Sequences processing

procedure TBeeApp.ProcessFilesToFresh;
var
  I, J, BackTear, NextTear: integer;
begin
  I := Headers.GetBack(Headers.Count - 1, toFresh);
  // find sequences and mark as toSwap files that not toFresh
  while I > -1 do
  begin
    BackTear := Headers.GetBack(I, foTear);
    NextTear := Headers.GetNext(I + 1, foTear);

    if NextTear = -1 then
      NextTear := Headers.Count;

    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := Headers.GetBack(NextTear - 1, toCopy);
      for J := BackTear to NextTear do
      begin
        case THeader(Headers.Items[J]).Action of
           toCopy: begin
                    THeader(Headers.Items[J]).Action := toSwap;
                    Inc(GeneralSize, THeader(Headers.Items[J]).Size * 2); // decoding  and Encoding size
                   end;
          toFresh: Inc(GeneralSize, THeader(Headers.Items[J]).Size); // decoding size
        end;
      end;
      I := BackTear;
    end;
    I := Headers.GetBack(I - 1, toFresh);
  end;
  Inc(GeneralSize, Headers.GetPackedSize(toCopy));
end;

procedure TBeeApp.ProcessFilesToDelete;
var
  I, J, BackTear, NextTear: integer;
begin
  I := Headers.GetBack(Headers.Count - 1, toDelete);
  // find sequences and ...
  while I > -1 do
  begin
    BackTear := Headers.GetBack(I, foTear);
    NextTear := Headers.GetNext(I + 1, foTear);

    if NextTear = -1 then
      NextTear := Headers.Count;

    // if is solid header
    if ((NextTear - BackTear) > 1) then
    begin
      NextTear := Headers.GetBack(NextTear - 1, toCopy);
      // if exists an header toDelete
      if Headers.GetBack(NextTear, toDelete) > (BackTear - 1) then
      begin
        for J := BackTear to NextTear do
          case THeader(Headers.Items[J]).Action of
            toCopy:   begin
                        THeader(Headers.Items[J]).Action := toSwap;
                        Inc(GeneralSize, THeader(Headers.Items[J]).Size * 2);
                      end;
            toDelete: Inc(GeneralSize, THeader(Headers.Items[J]).Size);
          end;
      end;
      I := BackTear;
    end;
    I := Headers.GetBack(I - 1, toDelete);
  end;
  Inc(GeneralSize, Headers.GetPackedSize(toCopy));
end;

function TBeeApp.ProcessFilesToSwap(Headers: THeaders): boolean;
var
  I, J: integer;
  Decoder: TDecoder;
  iDictionary, iTable, iTear: integer;
  CurrDictionary, CurrTable: integer;
begin
  Result := True;
  I := Headers.GetBack(Headers.Count - 1, toSwap);
  if (I > -1) and (not Terminated) then
  begin
    SwapName := Bee_Common.GenerateFileName(yOption);
    SwapFile := TFileWriter.Create(SwapName, fmCreate);

    CurrDictionary := Headers.Count;
    CurrTable := Headers.Count;

    Decoder := TDecoder.Create(ArcFile, Self); // get GeneralSize
    while (I > -1) and (not Terminated) do
    begin
      iDictionary := Headers.GetBack(I, foDictionary); // find dictionary info
      iTable := Headers.GetBack(I, foTable); // find table info
      iTear  := Headers.GetBack(I, foTear); // find tear info

      if (iDictionary > -1) and (iDictionary <> CurrDictionary) and (iDictionary <> iTear) then
      begin
        CurrDictionary := iDictionary;
        Decoder.DecodeStrm(THeader(Headers.Items[iDictionary]), pmQuit, SwapFile);
      end;

      if (iTable > -1) and (iTable <> CurrTable) and (iTable <> iTear) then
      begin
        CurrTable := iTable;
        Decoder.DecodeStrm(THeader(Headers.Items[iTable]), pmQuit, SwapFile);
      end;

      for J := iTear to I do
      begin
        if not Terminated then
        begin
          if THeader(Headers.Items[J]).Action = toSwap then
            Result := Decoder.DecodeStrm(Headers.Items[J], pmNorm, SwapFile)
          else
            Result := Decoder.DecodeStrm(Headers.Items[J], pmSkip, SwapFile);
        end else
          Result := True;

        if Result = False then Break;
      end;
      if Result = False then Break;

      I := Headers.GetBack(iTear - 1, toSwap);
    end;
    Decoder.Destroy;
    FreeAndNil(SwapFile);
  end;
end;

procedure TBeeApp.ProcessFilesDeleted;
var
  I: integer;
begin
  // rescue header informatios
  with Headers do
    for I := 0 to Count - 2 do
    begin
      if THeader(Items[I]).Action = toDelete then
      begin
        if (foVersion in THeader(Items[I]).Flags) and (not (foVersion in THeader(Items[I + 1]).Flags)) then
        begin
          Include(THeader(Items[I + 1]).Flags, foVersion);
          THeader(Items[I + 1]).Version := THeader(Items[I]).Version;
        end;

        if (foMethod in THeader(Items[I]).Flags) and (not (foMethod in THeader(Items[I + 1]).Flags)) then
        begin
          Include(THeader(Items[I + 1]).Flags, foMethod);
          THeader(Items[I + 1]).Method := THeader(Items[I]).Method;
        end;

        if (foDictionary in THeader(Items[I]).Flags) and (not (foDictionary in THeader(Items[I + 1]).Flags)) then
        begin
          Include(THeader(Items[I + 1]).Flags, foDictionary);
          THeader(Items[I + 1]).Dictionary := THeader(Items[I]).Dictionary;
        end;

        if (foTable in THeader(Items[I]).Flags) and (not (foTable in THeader(Items[I + 1]).Flags)) then
        begin
          Include(THeader(Items[I + 1]).Flags, foTable);
          THeader(Items[I + 1]).Table := THeader(Items[I]).Table;
        end;

        if (foTear in THeader(Items[I]).Flags) and (not (foTear in THeader(Items[I + 1]).Flags)) then
        begin
          Include(THeader(Items[I + 1]).Flags, foTear);
        end;
      end;
    end;
end;

procedure TBeeApp.ProcessFilesToDecode;
var
  I, J: integer;
  iDictionary, iTable, iTear: integer;
begin
  I := Headers.GetBack(Headers.Count - 1, aAction); // last header
  while I > -1 do
  begin
    iDictionary := Headers.GetBack(I, foDictionary); // find dictionary info
    iTable := Headers.GetBack(I, foTable); // find table info
    iTear  := Headers.GetBack(I, foTear); // find tear info

    for J := iTear to (I - 1) do
      if THeader(Headers.Items[J]).Action in [toNone, toQuit] then
      begin
        THeader(Headers.Items[J]).Action := toSkip;
        Inc(GeneralSize, THeader(Headers.Items[J]).Size);
      end;

    if (iDictionary > -1) and (THeader(Headers.Items[iDictionary]).Action = toNone) then
      THeader(Headers.Items[iDictionary]).Action := toQuit;

    if (iTable > -1) and (THeader(Headers.Items[iTable]).Action = toNone) then
      THeader(Headers.Items[iTable]).Action := toQuit;

    I := Headers.GetBack(iTear - 1, aAction);
  end;
end;

/// Option processing

procedure TBeeApp.ProcesstOption;
begin
  if tOption then
  begin
    GeneralSize := 0;
    RemainSize  := 0;

    xOption.Clear; // clear xOption
    FileMasks.Clear; // clear FileMasks
    FileMasks.Add('*!');
    DecodeShell(toTest);
  end;
end;

procedure TBeeApp.ProcesslOption;
begin
  if lOption then
  begin
    GeneralSize := 0;
    RemainSize  := 0;

    xOption.Clear; // clear xOption
    FileMasks.Clear; // clear FileMasks
    FileMasks.Add('*!');
    ListShell;
  end;
end;

/// Shell procedures

procedure TBeeApp.EncodeShell;
var
  I: integer;
  Encoder: TEncoder;
  TmpFileName: string;
  TmpFile: TFileWriter;
  Headers: THeaders;
  Time: double;
begin
  AppInterface.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + ArcName);
  Sync(AppInterface.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    Headers.cdOption := cdOption;
    Headers.fOption := fOption;
    Headers.uOption := uOption;
    Headers.xOption := xOption;

    AppInterface.OnDisplay.Data.Msg := (msgScanning + '...');
    Sync(AppInterface.OnDisplay.Method);

    // process FileMasks and xFileMasks
    Inc(GeneralSize, Headers.AddItems(FileMasks, rOption));

    if (Headers.GetCount([toUpdate, toFresh]) > 0) or
      ((Length(aOption) > 0) and (Headers.GetNext(0, toCopy) > -1)) then
    begin
      Time := Now;
      TmpFileName := Bee_Common.GenerateFileName(yOption);
      TmpFile := TFileWriter.Create(TmpFileName, fmCreate);

      // find sequences and...
      ProcessFilesToFresh(Headers);
      // decode solid header modified in a swap file
      if ProcessFilesToSwap(Headers) then
      begin
        // sort headers (only toUpdate headers)
        Headers.SortNews(Cfg, sOption, kOption, eOption);

        // if exists a modified solid sequence open Swap file
        if Length(SwapName) > 0 then
          SwapFile := TFileReader.Create(SwapName, fmOpenRead + fmShareDenyWrite);

        // set sfx module
        if Length(aOption) > 0 then Headers.SetSFX(aOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Self);
        for I := 0 to Headers.Count - 1 do
          if not Terminated then
          begin
            case THeader(Headers.Items[I]).Action of
              toCopy:   Encoder.CopyStrm  (Headers.Items[I], emNorm, ArcFile);
              toSwap:   Encoder.EncodeStrm(Headers.Items[I], emNorm, SwapFile);
              toFresh:  Encoder.EncodeFile(Headers.Items[I], emNorm);
              toUpdate: Encoder.EncodeFile(Headers.Items[I], emNorm);
            end;
          end;
        Encoder.Destroy;
        // rewrite Headers
        Headers.WriteItems(TmpFile);

        if not Terminated then
        begin
          AppInterface.OnDisplay.Data.Msg := (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + Bee_Common.TimeDifference(Time) + ' seconds');
          Sync(AppInterface.OnDisplay.Method);
        end else
        begin
          AppInterface.OnError.Data.Msg := (Cr + 'Process aborted - ' + Bee_Common.TimeDifference(Time) + ' seconds');
          Sync(AppInterface.OnError.Method);
        end;

        if Assigned(SwapFile) then FreeAndNil(SwapFile);
        if Assigned(ArcFile)  then FreeAndNil(ArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        DeleteFile(SwapName);
        if not Terminated then
        begin
          SysUtils.DeleteFile(ArcName);
          if not RenameFile(TmpFileName, ArcName) then
          begin
            AppInterface.OnError.Data.Msg := ('Error: can''t rename TempFile to ' + ArcName);
            Sync(AppInterface.OnError.Method);
          end else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption
          end;
        end else
          DeleteFile(TmpFileName);

      end else // if ProcessFilesToSwap
      begin
        if Assigned(SwapFile) then FreeAndNil(SwapFile);
        if Assigned(ArcFile)  then FreeAndNil(ArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(SwapName);
        SysUtils.DeleteFile(TmpFileName);

        AppInterface.OnError.Data.Msg := ('Error: can''t decode solid sequences');
        Sync(AppInterface.OnError.Method);
      end;

    end else // if Headers.GetCount
    begin
      AppInterface.OnWarning.Data.Msg := ('Warning: no files to process');
      Sync(AppInterface.OnWarning.Method);
    end;
  end;
  Headers.Free;

  if Assigned(ArcFile) then FreeAndNil(ArcFile);
end;

procedure TBeeApp.DecodeShell(Action: THeaderAction);
var
  Decoder: TDecoder;
  Headers: THeaders;
  Return: boolean;
  Time: double;
  I: integer;
begin
  AppInterface.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + ArcName);
  Sync(AppInterface.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toNone) then
  begin
    AppInterface.OnDisplay.Data.Msg := (msgScanning + '...');
    Sync(AppInterface.OnDisplay.Method);

    Headers.MarkItems(FileMasks, toNone, Action);
    Headers.MarkItems(xOption, Action, toNone);

    if (Action = toExtract) then
    begin
      ProcessFilesToExtract(Headers);
      ProcessFilesToOverWrite(Headers);
    end;

    GeneralSize := Headers.GetSize(Action);
    if (Headers.GetNext(0, Action) > -1) then // action = toTest or toExtract
    begin
      Time := Now;
      ProcessFilesToDecode(Headers, Action);

      Return  := True;
      Decoder := TDecoder.Create(ArcFile, Self);
      for I := 0 to Headers.Count - 1 do
        if Terminated = False then
        begin
          case THeader(Headers.Items[I]).Action of
            toExtract: Return := Decoder.DecodeFile(Headers.Items[I], pmNorm);
            toTest:    Return := Decoder.DecodeFile(Headers.Items[I], pmTest);
            toSkip:    Return := Decoder.DecodeFile(Headers.Items[I], pmSkip);
            toQuit:    Return := Decoder.Decodefile(Headers.Items[I], pmQuit);
          end;
          if Return = False then Break;
        end;
      Decoder.Destroy;

      if Terminated = False then
      begin
        if Return = True then
        begin
          AppInterface.OnDisplay.Data.Msg := (Cr + 'Everything went ok - ' + TimeDifference(Time) + ' seconds');
          Sync(AppInterface.OnDisplay.Method);
        end else
        begin
          AppInterface.OnError.Data.Msg := (Cr + 'Process aborted, a fatal error occourred - ' + TimeDifference(Time) + ' seconds');
          Sync(AppInterface.OnError.Method);
        end
      end else
      begin
        AppInterface.OnError.Data.Msg := (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Sync(AppInterface.OnError.Method);
      end;
      
    end else // if Headers.GetNext
    begin
      AppInterface.OnWarning.Data.Msg := ('Warning: no files to decode');
      Sync(AppInterface.OnWarning.Method);
    end;
  end;
  Headers.Free;

  if Assigned(ArcFile) then FreeAndNil(ArcFile);
end;

procedure TBeeApp.DeleteShell;
var
  TmpFileName: string;
  TmpFile: TFileWriter;
  I: integer;
  Time: double;
  Headers: THeaders;
  Encoder: TEncoder;
begin
  AppInterface.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + ArcName);
  Sync(AppInterface.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    AppInterface.OnDisplay.Data.Msg := (msgScanning + '...');
    Sync(AppInterface.OnDisplay.Method);

    Headers.MarkItems(FileMasks, toCopy, toDelete);
    Headers.MarkItems(xOption, toDelete, toCopy);

    if (Headers.GetNext(0, toDelete) > -1) or ((Length(aOption) > 0) and (Headers.GetNext(0, toCopy) > -1)) then
    begin
      Time := Now;
      TmpFileName := GenerateFileName(yOption);
      TmpFile := TFileWriter.Create(TmpFileName, fmCreate);

      // find sequences
      ProcessFilesToDelete(Headers);
      if ProcessFilesToSwap(Headers) then
      begin
        // rescue headers information
        ProcessFilesDeleted(Headers);

        // if SwapSequences has found a modified sequence open Swap file
        if Length(SwapName) > 0 then
          SwapFile := TFileReader.Create(SwapName, fmOpenRead + fmShareDenyWrite);

        // set sfx module
        if Length(aOption) > 0 then Headers.SetSFX(aOption);

        // write Headers
        Headers.WriteItems(TmpFile);
        Encoder := TEncoder.Create(TmpFile, Self);
        for I := 0 to Headers.Count - 1 do
          if not Terminated then
          begin
            case THeader(Headers.Items[I]).Action of
              toCopy:   Encoder.CopyStrm(Headers.Items[I], emNorm, ArcFile);
              toSwap:   Encoder.EncodeStrm(Headers.Items[I], emNorm, SwapFile);
              toDelete: begin
                          AppInterface.OnDisplay.Data.Msg := (msgDeleting + THeader(Headers.Items[I]).Name);
                          Sync(AppInterface.OnDisplay.Method);
                        end;
            end;
          end;
        Encoder.Destroy;
        Headers.WriteItems(TmpFile);

        if not Terminated then
        begin
          AppInterface.OnDisplay.Data.Msg := (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(Time) + ' seconds');
          Sync(AppInterface.OnDisplay.Method);
        end else
        begin
          AppInterface.OnError.Data.Msg := (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
          Sync(AppInterface.OnError.Method);
        end;

        if Assigned(SwapFile) then FreeAndNil(SwapFile);
        if Assigned(ArcFile)  then FreeAndNil(ArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(SwapName);
        if not Terminated then
        begin
          SysUtils.DeleteFile(ArcName);
          if not RenameFile(TmpFileName, ArcName) then
          begin
            AppInterface.OnError.Data.Msg := ('Error: can''t rename TempFile to ' + ArcName);
            Sync(AppInterface.OnError.Method);
          end else
          begin
            ProcesstOption; // process tOption
            ProcesslOption; // process lOption  
          end;
        end else
          SysUtils.DeleteFile(TmpFileName);

      end else // if ProcessFilesToSwap
      begin
        if Assigned(SwapFile) then FreeAndNil(SwapFile);
        if Assigned(ArcFile)  then FreeAndNil(ArcFile);
        if Assigned(TmpFile)  then FreeAndNil(TmpFile);

        SysUtils.DeleteFile(SwapName);
        SysUtils.DeleteFile(TmpFileName);

        AppInterface.OnError.Data.Msg := ('Error: can''t decode solid sequences');
        Sync(AppInterface.OnError.Method);
      end;

    end else // if Headers.GetNext
    begin
      AppInterface.OnWarning.Data.Msg := ('Warning: no files to delete');
      Sync(AppInterface.OnWarning.Method);
    end;
  end;
  Headers.Free;

  if Assigned(ArcFile) then FreeAndNil(ArcFile);
end;

procedure TBeeApp.RenameShell;
var
  TmpFile: TFileWriter;
  TmpFileName: string;
  NewFileName: string;
  Headers: THeaders;
  Encoder: TEncoder;
  Time: double;
  I: integer;
begin
  AppInterface.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + ArcName);
  Sync(AppInterface.OnDisplay.Method);

  Headers := THeaders.Create;
  if OpenArchive(Headers, toCopy) then
  begin
    AppInterface.OnDisplay.Data.Msg := (msgScanning + '...');
    Sync(AppInterface.OnDisplay.Method);

    Headers.MarkItems(FileMasks, toCopy, toRename);
    Headers.MarkItems(xOption, toRename, toCopy);

    GeneralSize := Headers.GetPackedSize([toCopy, toRename]);

    if (Headers.GetNext(0, toRename) > -1) or ((Length(aOption) > 0) and (Headers.GetNext(0, toCopy) > -1)) then
    begin
      Time := Now;
      TmpFileName := GenerateFileName(yOption);
      TmpFile := TFileWriter.Create(TmpFileName, fmCreate);

      for I := 0 to Headers.Count - 1 do
      begin
        if (THeader(Headers.Items[I]).Action = toRename) then
        begin
          while True do
          begin
            AppInterface.OnRename.Data.FileName := ExtractFileName(THeader(Headers.Items[I]).Name);
            AppInterface.OnRename.Data.FilePath := ExtractFilePath(THeader(Headers.Items[I]).Name);
            AppInterface.OnRename.Data.FileSize := THeader(Headers.Items[I]).Size;
            AppInterface.OnRename.Data.FileTime := THeader(Headers.Items[I]).Time;

            SetLength(AppInterface.OnRename.Answer, 0);
            Sync(AppInterface.OnRename.Method);

            NewFileName := Bee_Common.FixFileName(AppInterface.OnRename.Answer);
            if (AlreadyFileExists(Headers, I, [toCopy, toRename], NewFileName) <> -1) then
            begin
              AppInterface.OnWarning.Data.Msg := ('File "' + NewFileName + '" already existing in archive!');
              Sync(AppInterface.OnWarning.Method);
            end else
              Break;
          end;
          if Length(NewFileName) > 0 then
          begin
            THeader(Headers.Items[I]).Name := NewFileName;
          end;
        end;
      end;

      // set sfx module
      if Length(aOption) > 0 then Headers.SetSFX(aOption);

      Headers.WriteItems(TmpFile);
      Encoder := TEncoder.Create(TmpFile, Self);
      for I := 0 to Headers.Count - 1 do
        if not Terminated then
        begin
          Encoder.CopyStrm(Headers.Items[I], emNorm, ArcFile);
        end;
      Encoder.Destroy;
      Headers.WriteItems(TmpFile);

      if not Terminated then
      begin
        AppInterface.OnDisplay.Data.Msg := (Cr + 'Archive size ' + SizeToStr(TmpFile.Size) + ' bytes - ' + TimeDifference(Time) + ' seconds');
        Sync(AppInterface.OnDisplay.Method);
      end else
      begin
        AppInterface.OnError.Data.Msg := (Cr + 'Process aborted - ' + TimeDifference(Time) + ' seconds');
        Sync(AppInterface.OnError.Method);
      end;

      if Assigned(ArcFile) then FreeAndNil(ArcFile);
      if Assigned(TmpFile) then FreeAndNil(TmpFile);

      if not Terminated then
      begin
        SysUtils.DeleteFile(ArcName);
        if not RenameFile(TmpFileName, ArcName) then
        begin
          AppInterface.OnError.Data.Msg := ('Error: can''t rename TempFile to ' + ArcName);
          Sync(AppInterface.OnError.Method);
        end else
        begin
          ProcesstOption; // process tOption
          ProcesslOption; // process lOption
        end;
      end else
        SysUtils.DeleteFile(TmpFileName);

    end else // if Headers.GetNext
    begin
      AppInterface.OnWarning.Data.Msg := ('Warning: no files to rename');
      Sync(AppInterface.OnWarning.Method);
    end;
  end;
  Headers.Free;

  if Assigned(ArcFile) then FreeAndNil(ArcFile);
end;

{$IFDEF CONSOLEAPPLICATION}

procedure TBeeApp.ListShell;
var
  P: THeader;
  I: integer;
  Info: THeaders;  
  Version, Method, Dictionary: integer;
  TotalPack, TotalSize: integer;
  CountFiles: integer;
begin
  AppInterface.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + ArcName);
  Sync(AppInterface.OnDisplay.Method);

  Info := THeaders.Create;
  if OpenArchive(Info, toNone) then
  begin
    AppInterface.OnDisplay.Data.Msg := (msgScanning + '...');
    Sync(AppInterface.OnDisplay.Method);

    TotalSize  := 0;
    TotalPack  := 0;
    CountFiles := 0;

    Info.MarkItems(FileMasks, toNone, toList);
    Info.MarkItems(xOption, toList, toNone);

    if (Info.GetNext(0, toList) > -1) then
    begin
      AppInterface.OnDisplay.Data.Msg := (Cr + 'Name' + StringOfChar(' ', 18) + 'Size     Packed Ratio     Date  Time   Attr      CRC Meth');
      Sync(AppInterface.OnDisplay.Method);

      AppInterface.OnDisplay.Data.Msg := StringOfChar('-', 79);
      Sync(AppInterface.OnDisplay.Method);

      for I := 0 to Info.Count - 1 do
        if THeader(Info.Items[I]).Action = toList then
        begin
          P := Info.Items[I];

          Version := Info.GetBack(I, foVersion);
          if (Version > -1) and (Version < Info.Count) then
            Version := THeader(Info.Items[Version]).Version;

          Method := Info.GetBack(I, foMethod);
          if (Method > -1) and (Method < Info.Count) then
            Method := THeader(Info.Items[Method]).Method;

          Dictionary := Info.GetBack(I, foDictionary);
          if (Dictionary > -1) and (Dictionary < Info.Count) then
            Dictionary := THeader(Info.Items[Dictionary]).Dictionary;

          AppInterface.OnDisplay.Data.Msg := (P.Name);
          Sync(AppInterface.OnDisplay.Method);

          AppInterface.OnDisplay.Data.Msg := (StringOfChar(' ', 15)
            + Format(' %10s %10s %5s %14s %6s %8.8x %4s',
            [SizeToStr(P.Size),
             SizeToStr(P.Pack),
             RatioToStr(P.Pack, P.Size),
             Bee_Common.DateTimeToString(FileDateToDateTime(P.Time)),
             AttrToStr(P.Attr),
             P.Crc,
             MethodToStr(P, Method, Dictionary)]));

          Sync(AppInterface.OnDisplay.Method);

          Inc(TotalSize, P.Size);
          Inc(TotalPack, P.Pack);
          Inc(CountFiles);
        end;

      AppInterface.OnDisplay.Data.Msg := StringOfChar('-', 79);
      Sync(AppInterface.OnDisplay.Method);

      AppInterface.OnDisplay.Data.Msg := (Format('%d files', [CountFiles]))
        + StringOfChar(' ', 15 - Length((Format('%d files', [CountFiles]))))
        + (Format(' %10s %10s %5s' + Cr, [SizeToStr(TotalSize), SizeToStr(TotalPack), RatioToStr(TotalPack, TotalSize)]));
      Sync(AppInterface.OnDisplay.Method);

      // self-extractor module size
      if Info.GetSFXsize > 0 then
      begin
        AppInterface.OnDisplay.Data.Msg := ('Note: Bee Self-Extractor module founded' + Cr);
        Sync(AppInterface.OnDisplay.Method);
      end;

    end else
    begin
      AppInterface.OnWarning.Data.Msg := ('Warning: no files to list');
      Sync(AppInterface.OnWarning.Method);
    end;

  end;
  Info.Free;

  if Assigned(ArcFile) then FreeAndNil(ArcFile);
end;

{$ELSE}

procedure TBeeApp.ListShell;
var
  P: THeader;
  I: integer;
  Info: THeaders;
  Dictionary: integer;
  Version: integer;
  Method: integer;
begin
  AppInterface.OnDisplay.Data.Msg := (Cr + msgOpening + 'archive ' + ArcName);
  Sync(AppInterface.OnDisplay.Method);

  Info := THeaders.Create;
  if OpenArchive(Info, toNone) then
  begin
    AppInterface.OnDisplay.Data.Msg := (msgScanning + '...');
    Sync(AppInterface.OnDisplay.Method);

    Version    := -1;
    Method     := -1;
    Dictionary := -1;

    for I := 0 to Info.Count - 1 do
    begin
      AppInterface.OnTick.Data.Percentage := MulDiv(I, 100, Info.Count);
      Sync(AppInterface.OnTick.Method);

      P := Info.Items[I];

      if foVersion in P.Flags then
        Version := P.Version;

      if foMethod in P.Flags then
        Method := P.Method;

      if foDictionary in P.Flags then
        Dictionary := P.Dictionary;

      AppInterface.OnList.Data.FileName := ExtractFileName(P.Name);
      AppInterface.OnList.Data.FilePath := ExtractFilePath(P.Name);
      AppInterface.OnList.Data.FileSize := P.Size;
      AppInterface.OnList.Data.FilePack := P.Pack;
      AppInterface.OnList.Data.FileAttr := P.Attr;
      AppInterface.OnList.Data.FileTime := P.Time;
      AppInterface.OnList.Data.FileComm := '';
      AppInterface.OnList.Data.FileCrc := P.Crc;
      AppInterface.OnList.Data.FileMethod := MethodToStr(P, Method, Dictionary);
      AppInterface.OnList.Data.FileVersion := VersionToStr(Version);

      if foPassword in P.Flags then
        AppInterface.OnList.Data.FilePassword := 'Yes'
      else
        AppInterface.OnList.Data.FilePassword := 'No';

      AppInterface.OnList.Data.FilePosition := I;
      Sync(AppInterface.OnList.Method);
    end;
    Info.Free;

    if Assigned(ArcFile) then FreeAndNil(ArcFile);
  end;
end;

{$ENDIF}

// string routines

function TBeeApp.MethodToStr(P: THeader; Method, Dictionary: integer): string;
begin
  Result := 'm0a';

  if not (foTear in P.Flags) then
    Result[1] := 's';

  if not (foMoved in P.Flags) then
  begin
    if Method in [1..3] then
      Result[2] := char(byte('0') + Method)
    else
      Result[2] := '?';
  end;

  if Dictionary in [0..9] then
    Result[3] := char(byte('a') + Dictionary)
  else
    Result[3] := '?';
end;

function TBeeApp.VersionToStr(VersionId: cardinal): string;
begin
  case VersionId of
    0: Result := ' 0' + DecimalSeparator + '2';
    1: Result := ' 0' + DecimalSeparator + '3';
  else Result := ' 0' + DecimalSeparator + '0';
  end;
end;

end.
