unit BeeGui_LibLink;

{$I compiler.inc}

{$IFDEF STATIC-LINKING}

interface

uses
  Classes,
  // ---
  Bee_Types;

const
  {$IFDEF MSWINDOWS} cApplicationLib = 'beelib.dll'; {$ENDIF}
  {$IFDEF UNIX} cApplicationLib = 'beelib.so'; {$ENDIF}

  function CoreLibVersion: integer; external cApplicationLib;

  // ---

  procedure CoreFreePChar(P: PChar);                   external cApplicationLib;
  procedure CoreFreePFileInfo(P: PFileInfo);           external cApplicationLib;
  procedure CoreFreePFileInfoExtra(P: PFileInfoExtra); external cApplicationLib;

  // ---

  function CoreCreate(aCommandLine: PChar): boolean; external cApplicationLib;
  function CoreDestroy: boolean;                     external cApplicationLib;
  function CoreExecute: boolean;                     external cApplicationLib;
  function CoreSuspended(aValue: boolean): boolean;  external cApplicationLib;
  function CoreTerminate: boolean;                   external cApplicationLib;

  // ---

  function CoreGetPriority: integer;                  external cApplicationLib;
  function CoreSetPriority(aValue: integer): boolean; external cApplicationLib;

  // ---

  function CoreGetSpeed: integer;     external cApplicationLib;
  function CoreGetMessage: PChar;     external cApplicationLib;
  function CoreGetMessages: PChar;    external cApplicationLib;
  function CoreGetPercentes: integer; external cApplicationLib;
  function CoreGetTotalTime: integer; external cApplicationLib;
  function CoreGetTotalSize: int64;   external cApplicationLib;
  function CoreGetTime: integer;      external cApplicationLib;
  function CoreGetSize: int64;        external cApplicationLib;
  function CoreGetCode: integer;      external cApplicationLib;
  function CoreGetStatus: integer;    external cApplicationLib;

  // ---

  function CoreGetRequest: PChar;                  external cApplicationLib;
  function CoreGetRequestItem: PFileInfo;          external cApplicationLib;
  function CoreSetRequest(aValue: PChar): boolean; external cApplicationLib;

  // ---

  function CoreGetItemsCount: integer;                    external cApplicationLib;
  function CoreGetItems(aIndex: integer): PFileInfoExtra; external cApplicationLib;

implementation

end;

{$ELSE}

interface

uses
  Classes,
  DynLibs,
  // ---
  Bee_Types;

const
  {$IFDEF MSWINDOWS} cApplicationLib = 'beelib.dll'; {$ENDIF}
  {$IFDEF UNIX} cApplicationLib = 'beelib.so'; {$ENDIF}

type
  TCoreLibVersion = function: integer;

  // ---

  TCoreFreePChar = procedure(P: PChar);
  TCoreFreePFileInfo = procedure(P: PFileInfo);
  TCoreFreePFileInfoExtra = procedure(P: PFileInfoExtra);

  // ---

  TCoreCreate = function(aCommandLine: PChar): boolean;
  TCoreDestroy = function: boolean;
  TCoreExecute = function: boolean;
  TCoreSuspended = function(aValue: boolean): boolean;
  TCoreTerminate = function: boolean;

  // ---

  TCoreGetPriority = function: integer;
  TCoreSetPriority = function(aValue: integer): boolean;

  // ---

  TCoreGetSpeed = function: integer;
  TCoreGetMessage = function: PChar;
  TCoreGetMessages = function: PChar;
  TCoreGetPercentes = function: integer;
  TCoreGetTotalTime = function: integer;
  TCoreGetTotalSize = function: int64;
  TCoreGetTime = function: integer;
  TCoreGetSize = function: int64;
  TCoreGetCode = function: integer;
  TCoreGetStatus = function: integer;

  // ---

  TCoreGetRequest = function: PChar;
  TCoreGetRequestItem = function: PFileInfo;
  TCoreSetRequest = function(aValue: PChar): boolean;

  // ---

  TCoreGetItemsCount = function: integer;
  TCoreGetItems = function(aIndex: integer): PFileInfoExtra;

var
  CoreLibVersion: TCoreLibVersion;

  // ---

  CoreFreePChar: TCoreFreePChar;
  CoreFreePFileInfo: TCoreFreePFileInfo;
  CoreFreePFileInfoExtra: TCoreFreePFileInfoExtra;

  // ---

  CoreCreate: TCoreCreate;
  CoreDestroy: TCoreDestroy;
  CoreExecute: TCoreExecute;
  CoreSuspended: TCoreSuspended;
  CoreTerminate: TCoreTerminate;

  // ---

  CoreGetPriority: TCoreGetPriority;
  CoreSetPriority: TCoreSetPriority;

  // ---

  CoreGetSpeed: TCoreGetSpeed;
  CoreGetMessage: TCoreGetMessage;
  CoreGetMessages: TCoreGetMessages;
  CoreGetPercentes: TCoreGetPercentes;
  CoreGetTotalTime: TCoreGetTotalTime;
  CoreGetTotalSize: TCoreGetTotalSize;
  CoreGetTime: TCoreGetTime;
  CoreGetSize: TCoreGetSize;
  CoreGetCode: TCoreGetCode;
  CoreGetStatus: TCoreGetStatus;

  // ---

  CoreGetRequest: TCoreGetRequest;
  CoreGetRequestItem: TCoreGetRequestItem;
  CoreSetRequest: TCoreSetRequest;

  // ---

  CoreGetItemsCount: TCoreGetItemsCount;
  CoreGetItems: TCoreGetItems ;

  LibHandle: THandle;

implementation

  procedure LoadApplicationLib;
  begin
    LibHandle := LoadLibrary(cApplicationLib);

    CoreLibVersion := GetProcedureAddress(LibHandle, 'CoreLibVersion');

    // ---

    CoreFreePChar := GetProcedureAddress(LibHandle, 'CoreFreePChar');
    CoreFreePFileInfo := GetProcedureAddress(LibHandle, 'CoreFreePFileInfo');
    CoreFreePFileInfoExtra := GetProcedureAddress(LibHandle, 'CoreFreePFileInfoExtra');

    // ---

    CoreCreate := GetProcedureAddress(LibHandle, 'CoreCreate');
    CoreDestroy := GetProcedureAddress(LibHandle, 'CoreDestroy');
    CoreExecute := GetProcedureAddress(LibHandle, 'CoreExecute');
    CoreSuspended := GetProcedureAddress(LibHandle, 'CoreSuspended');
    CoreTerminate := GetProcedureAddress(LibHandle, 'CoreTerminate');

    // ---

    CoreGetPriority := GetProcedureAddress(LibHandle, 'CoreGetPriority');
    CoreSetPriority := GetProcedureAddress(LibHandle, 'CoreSetPriority');

    // ---

    CoreGetSpeed := GetProcedureAddress(LibHandle, 'CoreGetSpeed');
    CoreGetMessage := GetProcedureAddress(LibHandle, 'CoreGetMessage');
    CoreGetMessages := GetProcedureAddress(LibHandle, 'CoreGetMessages');
    CoreGetPercentes := GetProcedureAddress(LibHandle, 'CoreGetPercentes');
    CoreGetTotalTime := GetProcedureAddress(LibHandle, 'CoreGetTotalTime');
    CoreGetTotalSize := GetProcedureAddress(LibHandle, 'CoreGetTotalSize');
    CoreGetTime := GetProcedureAddress(LibHandle, 'CoreGetTime');
    CoreGetSize := GetProcedureAddress(LibHandle, 'CoreGetSize');
    CoreGetCode := GetProcedureAddress(LibHandle, 'CoreGetCode');
    CoreGetStatus := GetProcedureAddress(LibHandle, 'CoreGetStatus');

    // ---

    CoreGetRequest:= GetProcedureAddress(LibHandle, 'CoreGetRequest');
    CoreGetRequestItem := GetProcedureAddress(LibHandle, 'CoreGetRequestItem');
    CoreSetRequest := GetProcedureAddress(LibHandle, 'CoreSetRequest');

    // ---

    CoreGetItemsCount := GetProcedureAddress(LibHandle, 'CoreGetItemsCount');
    CoreGetItems := GetProcedureAddress(LibHandle, 'CoreGetItems');
  end;

  procedure UnLoadApplicationLib;
  begin
    UnloadLibrary(LibHandle);
  end;

initialization
  LoadApplicationLib;

finalization
  UnLoadApplicationLib;

end.

{$ENDIF}



