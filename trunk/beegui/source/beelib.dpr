{
  Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

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

    Bee library.

  Modifyed:
}

library BeeLib;

{$I compiler.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF UNIX}
  cThreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Bee_App,
  Bee_Types,
  Bee_Common,
  Bee_Interface;

type
  TCore = class(TThread)
  private
    FApp:       TApp;
    FKey:       string;
    FStatus:    integer;
    FParams:    TStringList;
    FMessages:  TStringList;
    FMessage:   string;
    FFileInfo:  TFileInfoA;
    FResult:    string;
    FContents:  TList;
  private
    procedure ProcessFatalError(const aMessage: string);
    procedure ProcessError(const aMessage: string);
    procedure ProcessWarning(const aMessage: string);
    procedure ProcessMessage(const aMessage: string);
    procedure ProcessOverwrite(const aFileInfo: TFileInfoA; var Result: char);
    procedure ProcessRename(const aFileInfo: TFileInfoA; var Result: string);
    procedure ProcessList(const aFileInfo: TFileInfoB);
    procedure ProcessKey(const aFileInfo: TFileInfoA; var Result: string);
    procedure ProcessRequest(const aMessage: string);
    procedure ProcessTick;
    procedure ProcessClear;
  public
    constructor Create(const aCommandLine: string);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  // -------------------------------------------------------------------------- //
  //                                                                            //
  //  Library TCore class implementation                                        //
  //                                                                            //
  // -------------------------------------------------------------------------- //

  constructor TCore.Create(const ACommandLine: string);
  begin
    inherited Create(True);
    FreeOnTerminate := False;

    FKey      := '';
    FMessage  := '';
    FStatus   := csReady;
    FMessages := TStringList.Create;
    FParams   := TStringList.Create;
    FContents := TList.Create;

    FParams.Text := aCommandLine;

    FApp := TBeeApp.Create(FParams);
    FApp.OnFatalError := ProcessFatalError;
    FApp.OnError      := ProcessError;
    FApp.OnWarning    := ProcessWarning;
    FApp.OnMessage    := ProcessMessage;
    FApp.OnOverwrite  := ProcessOverwrite;
    FApp.OnRename     := ProcessRename;
    FApp.OnList       := ProcessList;
    FApp.OnKey        := ProcessKey;
    FApp.OnRequest    := ProcessRequest;
    FApp.OnTick       := ProcessTick;
    FApp.OnClear      := ProcessClear;
  end;

  destructor TCore.Destroy;
  var
    I: integer;
  begin
    FMessages.Free;
    FParams.Free;
    with FContents do
    begin
      for I := 0 to Count -1 do
      begin
        FreeMem(Items[I]);
      end;
      Clear;
    end;
    FContents.Free;
    FApp.Free;
    inherited Destroy;
  end;

  procedure TCore.Execute;
  begin
    FStatus := csExecuting;
    begin
      FApp.Execute;
    end;
    FStatus := csTerminated;
  end;

  procedure TCore.ProcessFatalError(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCore.ProcessError(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCore.ProcessWarning(const aMessage: string);
  begin
    FMessages.Add(aMessage);
  end;

  procedure TCore.ProcessMessage(const aMessage: string);
  begin
    FMessage := aMessage;
    FMessages.Add(FMessage);
  end;

  procedure TCore.ProcessOverwrite(const aFileInfo: TFileInfoA; var Result: char);
  begin
    FResult   := Result;
    FFileInfo := aFileInfo;
    FStatus   := csWaitingOverwrite;
    while FStatus = csWaitingOverwrite do
    begin
      Sleep(250);
    end;
    Result := FResult[1];
  end;

  procedure TCore.ProcessRename(const aFileInfo: TFileInfoA; var Result: string);
  begin
    FResult   := Result;
    FFileInfo := aFileInfo;
    FStatus   := csWaitingRename;
    while FStatus = csWaitingRename do
    begin
      Sleep(250);
    end;
    Result := FResult;
  end;

  procedure TCore.ProcessList(const aFileInfo: TFileInfoB);
  var
   P: ^TFileInfoB;
  begin
    GetMem(P, SizeOf(TFileInfoB));
    P^ := aFileInfo;
    FContents.Add(P);

    // MessageBox(0, PChar(aFileInfo.FileName), '', 0);

  end;

  procedure TCore.ProcessKey(const aFileInfo: TFileInfoA; var Result: string);
  begin
    FResult   := Result;
    FFileInfo := aFileInfo;
    FStatus   := csWaitingKey;
    while FStatus = csWaitingKey do
    begin
      Sleep(250);
    end;
    Result := FResult;
  end;

  procedure TCore.ProcessRequest(const aMessage: string);
  begin
    FMessage := aMessage;
    FStatus  := csWaitingRequest;
    while FStatus = csWaitingRequest do
    begin
      Sleep(250);
    end;
  end;

  procedure TCore.ProcessTick;
  begin

  end;

  procedure TCore.ProcessClear;
  begin

  end;

  // -------------------------------------------------------------------------- //
  //                                                                            //
  //  Library core routines                                                     //
  //                                                                            //
  // -------------------------------------------------------------------------- //

var
  Core: TCore = nil;

  function CoreCreate(const ACommandLine: PChar): boolean;
  begin
    Result := not Assigned(Core);
    if Result then
    begin
      Core := TCore.Create(string(ACommandLine));
    end;
  end;

  function CoreExecute: boolean;
  begin
    Result := Assigned(Core) and (Core.FStatus = csReady);
    if Result then
    begin
      Core.Resume;
    end;
  end;

  function CoreDestroy: boolean;
  begin
    Result := Assigned(Core);
    if Assigned(Core) then
    begin
      FreeAndNil(Core);
    end;
  end;

  function CoreSuspended(AValue: boolean): boolean;
  begin
    Result := Assigned(Core);
    if Result then
    begin
      Core.FApp.Suspended := AValue;
    end;
  end;

  function CoreTerminate: boolean;
  begin
    Result := Assigned(Core);
    if Result then
    begin
      Core.FApp.Terminated := True;
      if Core.FStatus = csReady then
      begin
        Core.FStatus := csTerminated;
      end;
    end;
  end;

  function CoreGetExitCode: integer;
  begin
    if Assigned(Core) then
      Result := Core.FApp.ExitCode
    else
      Result := esUnknow;
  end;

  function CoreGetStatus: integer;
  begin
    if Assigned(Core) then
      Result := Core.FStatus
    else
      Result := csUnknow;
  end;

  function CoreGetMessage: PChar;
  begin
    if Assigned(Core) then
    begin
      Result := StrAlloc(Length(Core.FMessage) + 1);
      strpcopy(Result, Core.FMessage);
    end else
      Result := nil;
  end;

  function CoreGetMessages: PChar;
  begin
    if Assigned(Core) then
    begin
      Result := StrAlloc(Length(Core.FMessages.Text) + 1);
      strpcopy(Result, Core.FMessages.Text);
    end else
      Result := nil;
  end;

  function CoreGetElapsedTime: integer;
  begin
    if Assigned(Core) then
      Result := Core.FApp.ElapsedTime
    else
      Result := -1;
  end;

  function CoreGetRemainingTime: integer;
  begin
    if Assigned(Core) then
      Result := Core.FApp.RemainingTime
    else
      Result := -1;
  end;

  function CoreGetPercentes: integer;
  begin
    if Assigned(Core) then
      Result := Core.FApp.Percentes
    else
      Result := -1;
  end;

  function CoreGetSpeed: integer;
  begin
    if Assigned(Core) then
      Result := Core.FApp.Speed
    else
      Result := -1;
  end;

  function CoreGetTotalSize: int64;
  begin
    if Assigned(Core) then
      Result := Core.FApp.TotalSize
    else
      Result := -1;
  end;

  function CoreGetProcessedSize: int64;
  begin
    if Assigned(Core) then
      Result := Core.FApp.ProcessedSize
    else
      Result := -1;
  end;

  function CoreSetPriority(const AValue: TThreadPriority): boolean;
  begin
    Result := Assigned(Core);
    if Assigned(Core) then
    begin
      Core.Priority := AValue;
    end;
  end;

  function CoreGetPriority(var AValue: TThreadPriority): boolean;
  begin
    Result := Assigned(Core);
    if Result then
    begin
      AValue := Core.Priority
    end;
  end;

  function CoreGetFileInfo: PPCharFileInfoA;
  begin
    if Assigned(Core) then
    begin
      GetMem(Result, SizeOf(PCharFileInfoA));

      Result.FileName := stralloc(Length(Core.FFileInfo.FileName) + 1);
      strpcopy(Result.FileName, Core.FFileInfo.FileName);

      Result.FilePath := stralloc(Length(Core.FFileInfo.FilePath) + 1);
      strpcopy(Result.FilePath, Core.FFileInfo.FilePath);

      Result.FileSize := Core.FFileInfo.FileSize;
      Result.FileTime := Core.FFileInfo.FileTime;
      Result.FileAttr := Core.FFileInfo.FileAttr;
    end else
      Result := nil;
  end;

  function CoreSetOverwriteFileInfo(const AValue: char): boolean;
  begin
    Result := Assigned(Core);
    if Result then
    begin
      Core.FResult := AValue;
      Core.FStatus := csExecuting;
    end;
  end;

  function CoreSetRenameFileInfo(const AValue: PChar): boolean;
  begin
    Result := Assigned(Core);
    if Result then
    begin
      Core.FResult := string(AValue);
      Core.FStatus := csExecuting;
    end;
  end;

  function CoreSetPasswordFileInfo(const AValue: PChar): boolean;
  begin
    Result := Assigned(Core);
    if Result then
    begin
      Core.FResult := string(AValue);
      Core.FStatus := csExecuting;
    end;
  end;

  function CoreGetRequestMessage: PChar;
  begin
    if Assigned(Core) then
    begin
      Result := StrAlloc(Length(Core.FMessage) + 1);
      strpcopy(Result, Core.FMessage);
    end else
      Result := nil;
  end;

  function CoreSetRequestMessage(const AValue: PChar): boolean;
  begin
    Result := Assigned(Core);
    if Result then
    begin
      Core.FStatus := csExecuting;
    end;
  end;

  function CoreGetItemsCount: integer;
  begin
    if Assigned(Core) then
      Result := Core.FContents.Count
    else
      Result := 0;
  end;

  function CoreGetItems(const AIndex: integer): PPCharFileInfoB;
  begin
    if Assigned(Core) then
    with TFileInfoB(Core.FContents.Items[AIndex]^) do
    begin
      GetMem(Result, SizeOf(PCharFileInfoB));

      Result.FileName := stralloc(Length(FileName) + 1);
      strpcopy(Result.FileName, FileName);

      Result.FilePath := stralloc(Length(FilePath) + 1);
      strpcopy(Result.FilePath, FilePath);

      Result.FileSize   := FileSize;
      Result.FileTime   := FileTime;
      Result.FileAttr   := FileAttr;
      Result.FilePacked := FilePacked;
      Result.FileRatio  := FileRatio;

      Result.FileComm := stralloc(Length(FileComm) + 1);
      strpcopy(Result.FileComm, FileComm);

      Result.FileCrc  := FileCrc;

      Result.FileMethod   := stralloc(Length(FileMethod) + 1);
      strpcopy(Result.FileMethod, FileMethod);

      Result.FileVersion  := stralloc(Length(FileVersion) + 1);
      strpcopy(Result.FileVersion, FileVersion);

      Result.FilePassword := stralloc(Length(FilePassword) + 1);
      strpcopy(Result.FilePassword, FilePassword);

      Result.FilePosition := FilePosition;
    end else
      Result := nil;
  end;

  procedure FreePChar(P: PChar);
  begin
    strdispose(P); P := nil;
  end;

  procedure FreePPCharFileInfoA(P: PPCharFileInfoA);
  begin
    strdispose(P.FileName); P.FileName := nil;
    strdispose(P.FilePath); P.FilePath := nil;

    FreeMem(P); P := nil;
  end;

  procedure FreePPCharFileInfoB(P: PPCharFileInfoB);
  begin
    strdispose(P.FileName);     P.FileName := nil;
    strdispose(P.FilePath);     P.FilePath := nil;
    strdispose(P.FileComm);     P.FileComm := nil;
    strdispose(P.FileMethod);   P.FileMethod   := nil;
    strdispose(P.FileVersion);  P.FileVersion  := nil;
    strdispose(P.FilePassword); P.FilePassword := nil;

    FreeMem(P); P := nil;
  end;

  // -------------------------------------------------------------------------- //
  //                                                                            //
  //  Library core routines exported                                            //
  //                                                                            //
  // -------------------------------------------------------------------------- //

exports
  CoreCreate,
  CoreExecute,
  CoreDestroy,
  CoreSuspended,
  CoreTerminate,
  // ---
  CoreGetExitCode,
  CoreGetStatus,
  CoreGetMessage,
  CoreGetMessages,
  CoreGetPercentes,
  CoreGetSpeed,
  CoreGetTotalSize,
  CoreGetProcessedSize,
  CoreGetElapsedTime,
  CoreGetRemainingTime,
  // ---
  CoreGetPriority,
  CoreSetPriority,
  // ---
  CoreGetFileInfo,
  CoreSetRenameFileInfo,
  CoreSetPasswordFileInfo,
  CoreSetOverwriteFileInfo,
  // ---
  CoreGetRequestMessage,
  CoreSetRequestMessage,
  // ---
  CoreGetItemsCount,
  CoreGetItems,
  // ---
  FreePChar,
  FreePPCharFileInfoA,
  FreePPCharFileInfoB;

begin

end.
