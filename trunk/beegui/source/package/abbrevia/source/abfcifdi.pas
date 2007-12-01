(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

 {*********************************************************}
 {* ABBREVIA: AbFciFdi.pas 3.05                           *}
 {*********************************************************}
 {* ABBREVIA: Cabinet DLL wrapper                         *}
 {* Based on info from the FCI/FDI Library Description,   *}
 {* included in the Microsoft Cabinet SDK                 *}
 {*********************************************************}

unit AbFciFdi;

interface

uses
  Windows, SysUtils, Classes;

const
  CabinetDLL = 'cabinet.dll';
  cpuUnknown = -1;
  cpu80286 = 0;
  cpu80386 = 1;
  cpuDefault = cpuUnknown;


type
  {FDI errors}
  FDIError =
    (FDIError_None, FDIError_Cabinet_Not_Found,
    FDIError_Not_A_Cabinet, FDIError_Unknown_Cabinet_Version,
    FDIError_Corrupt_Cabinet, FDIError_Alloc_Fail,
    FDIError_Bad_Compr_Type, FDIError_MDI_Fail,
    FDIError_Target_File, FDIError_Reserve_Mismatch,
    FDIError_Wrong_Cabinet, FDIError_User_Abort);

  {FCI errors}
  FCIError =
    (FCIError_NONE, FCIError_Open_SRC,
    FCIError_Read_SRC, FCIError_Alloc_Fail,
    FCIError_Temp_File, FCIError_Bad_Compr_Type,
    FCIError_Cab_File, FCIError_User_Abort,
    FCIERRor_MCI_Fail);

  {FDI notifications}
  FDINotificationType =
    (FDINT_Cabinet_Info, FDINT_Partial_File,
    FDINT_Copy_File, FDINT_Close_File_Info,
    FDINT_Next_Cabinet, FDINT_Enumerate);

  {FDI/FCI error structure}
  PCabErrorRecord = ^CabErrorRecord;

  CabErrorRecord = record
    ErrorCode: integer;
    ErrorType: integer;
    ErrorPresent: boolean;
  end;

  {FDI cabinet information structure}
  PFDICabInfo = ^FDICabInfo;

  FDICabInfo = record
    cbCabinet: longint;
    cFolders: word;
    cFiles: word;
    setID:  word;
    iCabinet: word;
    fReserve: BOOL;
    hasprev: BOOL;
    hasnext: BOOL;
  end;

  {FCI cabinet information structure}
  PFCICabInfo = ^FCICabInfo;

  FCICabInfo = record
    cb: longint;
    cbFolderThresh: longint;
    cbReserveCFHeader: integer;
    cbReserveCFFolder: integer;
    cbReserveCFData: integer;
    iCab: integer;
    iDisk: integer;
    fFailOnIncompressible: integer;
    setID: word;
    szDisk: array[0..255] of char;
    szCab: array[0..255] of char;
    szCabPath: array[0..255] of char;
  end;

  {FDI notification structure}
  PFDINotification = ^FDINotification;

  FDINotification = record
    cb: longint;
    psz1: PChar;
    psz2: PChar;
    psz3: PChar;
    pv: Pointer;
    hf: integer;
    date: word;
    time: word;
    attribs: word;
    setID: word;
    iCabinet: word;
    iFolder: word;
    fdie: FDIERROR;
  end;

  {misc defines}
  HFDI = Pointer;
  HFCI = Pointer;
  FARPROC = Pointer;


{== Cabinet DLL routine prototypes ==========================================}
type
  TFDICreate =
    function(pfnalloc, pfnfree, pfnopen, pfnread, pfnwrite, pfnclose,
    pfnseek: FARPROC; cpuType: integer; pError: PCabErrorRecord): HFDI;
    cdecl;
  {----------------------------------------------------------------------------}
  TFDIIsCabinet =
    function(hfdi: HFDI; hf: integer; pfdici: PFDICabInfo): boolean;
    cdecl;
  {----------------------------------------------------------------------------}
  TFDICopy =
    function(hfdi: HFDI; pszCabinet, pszCabPath: PChar; flags: integer;
    pfnfdin, pfnfdid: FARPROC; Archive: Pointer): boolean;
    cdecl;
  {----------------------------------------------------------------------------}
  TFDIDestroy =
    function(hfdi: HFDI): boolean;
    cdecl;
  {----------------------------------------------------------------------------}
  TFCICreate =
    function(pError: PCabErrorRecord;
    pfnfcifp, pfnalloc, pfnfree, pfnopen, pfnread, pfnwrite, pfnclose,
    pfnseek, pfndelete, pfnfcigtf: FARPROC; pccab: PFCICabInfo;
    Archive: Pointer): HFCI;
    cdecl;
  {----------------------------------------------------------------------------}
  TFCIAddFile =
    function(hfci: HFCI; pszFilePath, pszFileName: PChar;
    fExecute: boolean; pfnfcignc, pfnfcis, pfnfcigoi: FARPROC;
    typeCompress: word): boolean;
    cdecl;
  {----------------------------------------------------------------------------}
  TFCIFlushCabinet =
    function(hfci: HFCI; fGetNextCab: boolean;
    pfnfcignc, pfnfcis: FARPROC): boolean;
    cdecl;
  {----------------------------------------------------------------------------}
  TFCIFlushFolder =
    function(hfci: HFCI; pfnfcignc, pfnfcis: FARPROC): boolean;
    cdecl;
  {----------------------------------------------------------------------------}
  TFCIDestroy =
    function(hfci: HFCI): boolean;
    cdecl;


{== DLL routine wrappers ====================================================}
function FDICreate(pfnalloc, pfnfree, pfnopen, pfnread, pfnwrite,
  pfnclose, pfnseek: FARPROC; cpuType: integer; pError: PCabErrorRecord): HFDI;
 {returns an FDI context for opening an existing cabinet}
 {  pfnalloc - heap allocation callback function }
 {  pfnfree  -  heap deallocation callback function }
 {  pfnopen  -  open file callback function }
 {  pfnwrite - write file callback function }
 {  pfnclose - close file callback function }
 {  pfnseek  - reposition file pointer callback function }
 {  cpuType  -  -1: unknown, 0: 80286, 1: 80386 }
 {  pError   -  pointer to error record }
 {----------------------------------------------------------------------------}
function FDIIsCabinet(hfdi: HFDI; hf: integer; pfdici: PFDICabInfo): boolean;
 {checks cabinet file for validity}
 {  hfdi   - FDI context }
 {  hf     - cabinet file handle }
 {  pfdici - pointer to FDI cabinet info structure }
 {----------------------------------------------------------------------------}
function FDICopy(hfdi: HFDI; pszCabinet, pszCabPath: PChar;
  flags: integer; pfnfdin, pfnfdid: FARPROC; Archive: Pointer): boolean;
  {enumerates every file in the cabinet.  The callback function
  {should indicate whether or not to extract a given file}
 {  hfdi       - FDI context }
 {  pszCabinet - cabinet file name }
 {  pszCabPath - cabinet file path }
 {  flags      - currently not used }
 {  pfnfdin    - FDI notifaction callback function }
 {  pfnfdid    - decryption callback (currently not used)}
 {  Archive    - the calling TAbCabArchive instance }
 {----------------------------------------------------------------------------}
function FDIDestroy(hfdi: HFDI): boolean;
 {releases FDI context and frees resources}
 {  hfdi - FDI context }
 {----------------------------------------------------------------------------}
function FCICreate(pError: PCabErrorRecord;
  pfnfcifp, pfnalloc, pfnfree, pfnopen, pfnread, pfnwrite, pfnclose,
  pfnseek, pfndelete, pfnfcigtf: FARPROC; pccab: PFCICabInfo;
  Archive: Pointer): HFCI;
 {creates a new cabinet file and returns the FCI context}
 {  pError    - pointer to error record }
 {  pfnfcifp  - callback notification when file has been placed in cabinet }
 {  pfnalloc  - callback function to allocate memory }
 {  pfnfree   - callback function to free memory }
 {  pfnopen   - callback function to open a file }
 {  pfnwrite  - callback function to write to a file }
 {  pfnclose  - callback function to close a file }
 {  pfnseek   - callback function to reposition file pointer }
 {  pfndelete - callback function to delete a file }
 {  pfnfcigtf - callback function to obtain temp filename }
 {  pccab     - pointer to FCI cabinet infor structure }
 {  Archive   - the calling TAbCabArchive instance }
 {----------------------------------------------------------------------------}
function FCIAddFile(hfci: HFCI; pszFilePath, pszFileName: PChar;
  fExecute: boolean; pfnfcignc, pfnfcis, pfnfcigoi: FARPROC;
  typeCompress: word): boolean;
 {adds a file to the cabinet}
 {  hfci         - FCI context }
 {  pszFilePath  - full pathname of file being added }
 {  pszFileName  - just the file name }
 {  fExecute     - flag to indicate if file is executable }
 {  pfnfcignc    - callback function to obtain next cabinet info }
 {  pfnfcis      - callback function to relay progress }
 {  pfnfcigoi    - callback function to open file and get attributes }
 {  typeCompress - compression type to use }
 {----------------------------------------------------------------------------}
function FCIFlushCabinet(hfci: HFCI; fGetNextCab: boolean;
  pfnfcignc, pfnfcis: FARPROC): boolean;
 {writes current cabinet file out to disk and optionally starts a new one}
 {  hfci        - FCI context }
 {  fGetNextCab - flag indicating whether to start a new cabinet }
 {  pfnfcignc   - callback function to obtain next cabinet info }
 {  pfnfcis     - callback function to relay progress }
 {----------------------------------------------------------------------------}
function FCIFlushFolder(hfci: HFCI; pfnfcignc, pfnfcis: FARPROC): boolean;
 {close current compression block and start a new one}
 {  hfci      - FCI context }
 {  pfnfcignc - callback function to obtain next cabinet info }
 {  pfnfcis   - callback function to relay progress }
 {----------------------------------------------------------------------------}
function FCIDestroy(hfci: HFCI): boolean;
 {releases FCI context and frees resources}
 {  hfdi - FDI context }
 {----------------------------------------------------------------------------}


implementation

uses
  AbExcept,
  AbConst;

var
  CabDLLLoaded: boolean;
  CabDLLHandle: THandle;
  FDICreateProc: TFDICreate;
  FDIIsCabinetProc: TFDIIsCabinet;
  FDICopyProc: TFDICopy;
  FDIDestroyProc: TFDIDestroy;
  FCICreateProc: TFCICreate;
  FCIAddFileProc: TFCIAddFile;
  FCIFlushCabinetProc: TFCIFlushCabinet;
  FCIFlushFolderProc: TFCIFlushFolder;
  FCIDestroyProc: TFCIDestroy;


{============================================================================}
procedure LoadCabinetDLL;
begin
  if CabDllLoaded then
    Exit;
  CabDllHandle := LoadLibrary(CabinetDLL);
  if (CabDllHandle = 0) then
    raise EAbNoCabinetDLL.Create;
  @FDICreateProc := GetProcAddress(CabDllHandle, 'FDICreate');
  @FDIIsCabinetProc := GetProcAddress(CabDllHandle, 'FDIIsCabinet');
  @FDICopyProc := GetProcAddress(CabDllHandle, 'FDICopy');
  @FDIDestroyProc := GetProcAddress(CabDllHandle, 'FDIDestroy');
  @FCICreateProc := GetProcAddress(CabDllHandle, 'FCICreate');
  @FCIAddFileProc := GetProcAddress(CabDllHandle, 'FCIAddFile');
  @FCIFlushCabinetProc := GetProcAddress(CabDllHandle, 'FCIFlushCabinet');
  @FCIFlushFolderProc := GetProcAddress(CabDllHandle, 'FCIFlushFolder');
  @FCIDestroyProc := GetProcAddress(CabDllHandle, 'FCIDestroy');
  CabDllLoaded := True;
end;

{----------------------------------------------------------------------------}
function FDICreate(pfnalloc, pfnfree, pfnopen, pfnread, pfnwrite,
  pfnclose, pfnseek: FARPROC; cpuType: integer; pError: PCabErrorRecord): HFDI;
begin
  LoadCabinetDLL;
  if Assigned(FDICreateProc) then
    Result := FDICreateProc(pfnalloc, pfnfree, pfnopen, pfnread,
      pfnwrite, pfnclose, pfnseek, cpuType, pError)
  else
    Result := nil;
end;

{----------------------------------------------------------------------------}
function FDIIsCabinet(hfdi: HFDI; hf: integer; pfdici: PFDICabInfo): boolean;
begin
  LoadCabinetDLL;
  if Assigned(FDIIsCabinetProc) then
    Result := FDIIsCabinetProc(hfdi, hf, pfdici)
  else
    Result := False;
end;

{----------------------------------------------------------------------------}
function FDICopy(hfdi: HFDI; pszCabinet, pszCabPath: PChar;
  flags: integer; pfnfdin, pfnfdid: FARPROC; Archive: Pointer): boolean;
begin
  LoadCabinetDLL;
  if Assigned(FDICopyProc) then
    Result := FDICopyProc(hfdi, pszCabinet, pszCabPath, flags,
      pfnfdin, pfnfdid, Archive)
  else
    Result := False;
end;

{----------------------------------------------------------------------------}
function FDIDestroy(hfdi: HFDI): boolean;
begin
  LoadCabinetDLL;
  if Assigned(FDIDestroyProc) then
    Result := FDIDestroyProc(hfdi)
  else
    Result := False;
end;

{----------------------------------------------------------------------------}
function FCICreate(pError: PCabErrorRecord;
  pfnfcifp, pfnalloc, pfnfree, pfnopen, pfnread, pfnwrite, pfnclose,
  pfnseek, pfndelete, pfnfcigtf: FARPROC; pccab: PFCICabInfo;
  Archive: Pointer): HFCI;
begin
  LoadCabinetDLL;
  if Assigned(FCICreateProc) then
    Result := FCICreateProc(pError, pfnfcifp, pfnalloc, pfnfree,
      pfnopen, pfnread, pfnwrite, pfnclose, pfnseek, pfndelete,
      pfnfcigtf, pccab, Archive)
  else
    Result := nil;
end;

{----------------------------------------------------------------------------}
function FCIAddFile(hfci: HFCI; pszFilePath, pszFileName: PChar;
  fExecute: boolean; pfnfcignc, pfnfcis, pfnfcigoi: FARPROC;
  typeCompress: word): boolean;
begin
  LoadCabinetDLL;
  if Assigned(FCIAddFileProc) then
    Result := FCIAddFileProc(hfci, pszFilePath, pszFileName, fExecute,
      pfnfcignc, pfnfcis, pfnfcigoi, typeCompress)
  else
    Result := False;
end;

{----------------------------------------------------------------------------}
function FCIFlushCabinet(hfci: HFCI; fGetNextCab: boolean;
  pfnfcignc, pfnfcis: FARPROC): boolean;
begin
  LoadCabinetDLL;
  if Assigned(FCIFlushCabinetProc) then
    Result := FCIFlushCabinetProc(hfci, fGetNextCab, pfnfcignc, pfnfcis)
  else
    Result := False;
end;

{----------------------------------------------------------------------------}
function FCIFlushFolder(hfci: HFCI; pfnfcignc, pfnfcis: FARPROC): boolean;
begin
  LoadCabinetDLL;
  if Assigned(FCIFlushFolderProc) then
    Result := FCIFlushFolderProc(hfci, pfnfcignc, pfnfcis)
  else
    Result := False;
end;

{----------------------------------------------------------------------------}
function FCIDestroy(hfci: HFCI): boolean;
begin
  LoadCabinetDLL;
  if Assigned(FCIDestroyProc) then
    Result := FCIDestroyProc(hfci)
  else
    Result := False;
end;

{----------------------------------------------------------------------------}
initialization
  CabDllLoaded := False;

end.
