unit BeeGui_LibLink;

{$I compiler.inc}

interface

uses
  Classes,
  // ---
  Bee_Types;

const
  cApplicationLib = '.\beelib.dll';

  // ---

  function LibVersion: integer;  external cApplicationLib;

  // ---

  procedure FreePChar(P: PChar); external cApplicationLib;

  // ---

  function CoreCreate(const aCommandLine: PChar): boolean; external cApplicationLib;
  function CoreDestroy: boolean;                           external cApplicationLib;
  function CoreExecute: boolean;                           external cApplicationLib;
  function CoreSuspended(aValue: boolean): boolean;        external cApplicationLib;
  function CoreTerminate: boolean;                         external cApplicationLib;

  // ---

  function CoreGetPriority(var   aValue: TThreadPriority): boolean; external cApplicationLib;
  function CoreSetPriority(const aValue: TThreadPriority): boolean; external cApplicationLib;

  // ---

  function CoreGetSpeed: integer;     external cApplicationLib;
  function CoreGetRequest: PChar;     external cApplicationLib;
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

  function CoreSetRequest  (const aValue: PChar): boolean; external cApplicationLib;
  function CoreSetRename   (const aValue: PChar): boolean; external cApplicationLib;
  function CoreSetPassword (const aValue: PChar): boolean; external cApplicationLib;
  function CoreSetOverwrite(const aValue: char): boolean;  external cApplicationLib;

  // ---

  function CoreGetItemsCount: integer;                          external cApplicationLib;
  function CoreGetItems(const aIndex: integer): PFileInfoExtra; external cApplicationLib;
  function CoreGetItem: PFileInfo;                              external cApplicationLib;

implementation

end.

