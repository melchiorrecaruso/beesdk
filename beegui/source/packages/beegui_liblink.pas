unit BeeGui_LibLink;

{$I compiler.inc}

interface

uses
  Classes,
  // ---
  Bee_Types;

const
  cApplicationLib = '.\beelib.dll';

  function CoreCreate(const aCommandLine: PChar): boolean; external cApplicationLib;
  function CoreExecute:   boolean;                         external cApplicationLib;
  function CoreDestroy:   boolean;                         external cApplicationLib;
  function CoreTerminate: boolean;                         external cApplicationLib;

  function CoreSuspended(AValue: boolean): boolean; external cApplicationLib;

  function CoreGetExitCode: integer; external cApplicationLib;
  function CoreGetStatus:   integer; external cApplicationLib;

  function CoreGetElapsedTime:   integer; external cApplicationLib;
  function CoreGetRemainingTime: integer; external cApplicationLib;
  function CoreGetPercentes:     integer; external cApplicationLib;
  function CoreGetSpeed:         integer; external cApplicationLib;

  function CoreGetTotalSize:     int64;  external cApplicationLib;
  function CoreGetProcessedSize: int64;  external cApplicationLib;

  function CoreSetPriority(const AValue: TThreadPriority): boolean; external cApplicationLib;
  function CoreGetPriority(var   AValue: TThreadPriority): boolean; external cApplicationLib;

  function CoreGetMessage:  PChar; external cApplicationLib;
  function CoreGetMessages: PChar; external cApplicationLib;
  function CoreGetRequest:  PChar; external cApplicationLib;

  function CoreSetOverwrite(const AValue: char): boolean;  external cApplicationLib;
  function CoreSetRename   (const AValue: PChar): boolean; external cApplicationLib;
  function CoreSetPassword (const AValue: PChar): boolean; external cApplicationLib;
  function CoreSetRequest  (const AValue: PChar): boolean; external cApplicationLib;

  function CoreGetItemsCount: integer;                          external cApplicationLib;
  function CoreGetItems(const AIndex: integer): PFileInfoExtra; external cApplicationLib;
  function CoreGetItem: PFileInfo;                              external cApplicationLib;

  procedure FreePChar(P: PChar); external cApplicationLib;

implementation

end.

