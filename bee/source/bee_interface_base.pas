{
  Copyright (c) 2005-2008 Andrew Filinsky and Melchiorre Caruso

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

  TThreadEx, abstract class of Thread.

  Modifyed:

  v0.7.9 build 0890 - 2008.10.18 by Melchiorre Caruso;
}

unit Bee_Interface_Base;

{$I compiler.inc}

interface

uses
  Windows,
  Classes;

type
  TThreadSynchronizer = class
  private
    FMethod: TThreadMethod;
    FSynchronizeException: TObject;
    FSyncBaseThreadID: longword;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Synchronize(Method: TThreadMethod);
    property SyncBaseThreadID: longword Read FSyncBaseThreadID;
  end;

  TThreadEx = class(TThread)
  private
    FSynchronizer: TThreadSynchronizer;
    procedure HandleTerminate;
  protected
    procedure DoTerminate; override;
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    procedure Wait;
    property Synchronizer: TThreadSynchronizer Read FSynchronizer;
  end;

implementation

const
  CM_EXECPROC      = $8FFD;
  CM_DESTROYWINDOW = $8FFC;

type
  TSyncInfo = class
    FSyncBaseThreadID: longword;
    FThreadWindow:     HWND;
    FThreadCount:      integer;
  end;

  TSynchronizerManager = class
  private
    FThreadLock: TRTLCriticalSection;
    FList: TList;
    procedure FreeSyncInfo(AInfo: TSyncInfo);
    procedure DoDestroyWindow(AInfo: TSyncInfo);
    function InfoBySync(ASyncBaseThreadID: longword): TSyncInfo;
    function FindSyncInfo(ASyncBaseThreadID: longword): TSyncInfo;
  public
    class function Instance: TSynchronizerManager;
    constructor Create();
    destructor Destroy; override;
    procedure AddThread(ASynchronizer: TThreadSynchronizer);
    procedure RemoveThread(ASynchronizer: TThreadSynchronizer);
    procedure Synchronize(ASynchronizer: TThreadSynchronizer);
  end;

var
  SynchronizerManager: TSynchronizerManager;

function ThreadWndProc(Window: HWND; Message, wParam, lParam: longint): longint; stdcall;
begin
  case Message of
    CM_EXECPROC:
      with TThreadSynchronizer(lParam) do
      begin
        Result := 0;
        try
          FSynchronizeException := nil;
          FMethod();
        except
          FSynchronizeException := AcquireExceptionObject();
        end;
      end;
    CM_DESTROYWINDOW:
    begin
      TSynchronizerManager.Instance().DoDestroyWindow(TSyncInfo(lParam));
      Result := 0;
    end;
    else
      Result := DefWindowProc(Window, Message, wParam, lParam);
  end;
end;

var
  ThreadWindowClass: TWndClass = (style: 0;
  lpfnWndProc: @ThreadWndProc;
  cbClsExtra: 0;
  cbWndExtra: 0;
  hInstance: 0;
  hIcon: 0;
  hCursor: 0;
  hbrBackground: 0;
  lpszMenuName: nil;
  lpszClassName: 'TThreadSynchronizerWindow');

{ TSynchronizerManager }

constructor TSynchronizerManager.Create;
begin
  inherited Create();
  InitializeCriticalSection(FThreadLock);
  FList := TList.Create();
end;

destructor TSynchronizerManager.Destroy;
var
  i: integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    FreeSyncInfo(TSyncInfo(FList[i]));
  end;
  FList.Free();
  DeleteCriticalSection(FThreadLock);
  inherited Destroy();
end;

class function TSynchronizerManager.Instance: TSynchronizerManager;
begin
  if (SynchronizerManager = nil) then
  begin
    SynchronizerManager := TSynchronizerManager.Create();
  end;
  Result := SynchronizerManager;
end;

procedure TSynchronizerManager.AddThread(ASynchronizer: TThreadSynchronizer);

  function AllocateWindow: HWND;
  var
    TempClass: TWndClass;
    ClassRegistered: boolean;
  begin
    ThreadWindowClass.hInstance := HInstance;
    ClassRegistered := GetClassInfo(HInstance, ThreadWindowClass.lpszClassName,
      TempClass);
    if not ClassRegistered or (@TempClass.lpfnWndProc <> @ThreadWndProc) then
    begin
      if ClassRegistered then
        Windows.UnregisterClass(ThreadWindowClass.lpszClassName, HInstance);
      Windows.RegisterClass(ThreadWindowClass);
    end;

    Result := CreateWindow(ThreadWindowClass.lpszClassName, '', 0,
      0, 0, 0, 0, 0, 0, HInstance, nil);
  end;

var
  info: TSyncInfo;
begin
  EnterCriticalSection(FThreadLock);
  try
    info := FindSyncInfo(ASynchronizer.SyncBaseThreadID);
    if (info = nil) then
    begin
      info := TSyncInfo.Create();
      info.FSyncBaseThreadID := ASynchronizer.SyncBaseThreadID;
      FList.Add(info);
    end;
    if (info.FThreadCount = 0) then
    begin
      info.FThreadWindow := AllocateWindow();
    end;
    Inc(info.FThreadCount);
  finally
    LeaveCriticalSection(FThreadLock);
  end;
end;

procedure TSynchronizerManager.RemoveThread(ASynchronizer: TThreadSynchronizer);
var
  info: TSyncInfo;
begin
  EnterCriticalSection(FThreadLock);
  try
    info := InfoBySync(ASynchronizer.SyncBaseThreadID);
    PostMessage(info.FThreadWindow, CM_DESTROYWINDOW, 0, longint(info));
  finally
    LeaveCriticalSection(FThreadLock);
  end;
end;

procedure TSynchronizerManager.DoDestroyWindow(AInfo: TSyncInfo);
begin
  EnterCriticalSection(FThreadLock);
  try
    Dec(AInfo.FThreadCount);
    if AInfo.FThreadCount = 0 then
    begin
      FreeSyncInfo(AInfo);
    end;
  finally
    LeaveCriticalSection(FThreadLock);
  end;
end;

procedure TSynchronizerManager.FreeSyncInfo(AInfo: TSyncInfo);
begin
  if AInfo.FThreadWindow <> 0 then
  begin
    DestroyWindow(AInfo.FThreadWindow);
    AInfo.Free();
    FList.Remove(AInfo);
  end;
end;

procedure TSynchronizerManager.Synchronize(ASynchronizer: TThreadSynchronizer);
begin
  SendMessage(InfoBySync(ASynchronizer.SyncBaseThreadID).FThreadWindow,
    CM_EXECPROC, 0, longint(ASynchronizer));
end;

function TSynchronizerManager.FindSyncInfo(ASyncBaseThreadID: longword): TSyncInfo;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Result := TSyncInfo(FList[i]);
    if (Result.FSyncBaseThreadID = ASyncBaseThreadID) then
      Exit;
  end;
  Result := nil;
end;

function TSynchronizerManager.InfoBySync(ASyncBaseThreadID: longword): TSyncInfo;
begin
  Result := FindSyncInfo(ASyncBaseThreadID);
  Assert(Result <> nil, 'Cannot find SyncInfo for the specified thread synchronizer');
end;

{ TThreadSynchronizer }

constructor TThreadSynchronizer.Create;
begin
  inherited Create();
  FSyncBaseThreadID := GetCurrentThreadId();
  TSynchronizerManager.Instance().AddThread(Self);
end;

destructor TThreadSynchronizer.Destroy;
begin
  TSynchronizerManager.Instance().RemoveThread(Self);
  inherited Destroy();
end;

procedure TThreadSynchronizer.Synchronize(Method: TThreadMethod);
begin
  FSynchronizeException := nil;
  FMethod := Method;
  TSynchronizerManager.Instance().Synchronize(Self);
  if Assigned(FSynchronizeException) then
    raise FSynchronizeException;
end;

{ TThreadEx }

constructor TThreadEx.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FSynchronizer := TThreadSynchronizer.Create();
end;

destructor TThreadEx.Destroy;
begin
  FSynchronizer.Free();
  inherited Destroy();
end;

procedure TThreadEx.DoTerminate;
begin
  if Assigned(OnTerminate) then
    Synchronizer.Synchronize(HandleTerminate);
end;

procedure TThreadEx.HandleTerminate;
begin
  if Assigned(OnTerminate) then
    OnTerminate(Self);
end;

procedure TThreadEx.Wait;
var
  Msg: TMsg;
  H:   THandle;
begin
  DuplicateHandle(GetCurrentProcess(), Handle, GetCurrentProcess(),
    @H, 0, False, DUPLICATE_SAME_ACCESS);
  try
    if GetCurrentThreadID = Synchronizer.SyncBaseThreadID then
    begin
      while MsgWaitForMultipleObjects(1, H, False, INFINITE, QS_SENDMESSAGE) =
        WAIT_OBJECT_0 + 1 do
      begin
        while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
        begin
          DispatchMessage(Msg);
        end;
      end;
    end
    else
    begin
      WaitForSingleObject(H, INFINITE);
    end;
  finally
    CloseHandle(H);
  end;
end;

initialization
  SynchronizerManager := nil;

finalization
  SynchronizerManager.Free();
  SynchronizerManager := nil;

end.
