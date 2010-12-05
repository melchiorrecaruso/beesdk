{
  Copyright (c) 2005-2010 Andrew Filinsky and Melchiorre Caruso

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

    Types definitions

  Modifyed:

    v0.8.0 build 1110 - 2010.01.17 by Melchiorre Caruso.
}

unit Bee_Types;

{$I compiler.inc}

interface

type
  { Commands:                                             }
  {   ccNone     Nul command                              }
  {   ccHelp     Show help informations                   }
  {   ccAdd      Add files                                }
  {   ccExtract  Extract file                             }
  {   ceXextract Extract file with full path              }
  {   ccDelete   Delete files                             }
  {   ccRename   Rename files                             }
  {   ccTest     Test files                               }
  {   ccList     List files                               }
  {   ccOpen     Open archive                             }

  TCommand = (ccAdd, ccExtract, ccXextract, ccDelete,
    ccRename, ccTest, ccList, ccHelp, ccOpen, ccNone);

  { Recursive Mode Option:                                }
  {  rmNone      No resurse filename                      }
  {  rmWildcard  Recurse olny filename with wildcard      }
  {  rmFull      Recurse all filename                     }

  TRecursiveMode = (rmNone, rmWildCard, rmFull);

  { Update Mode Option:                                   }
  {  umAdd           Add only new files                   }
  {  umUpdate        Update only existing files           }
  {  umReplace       Replace only existing files          }
  {  umAddUpdate     Add and update existing files        }
  {  umAddReplace    Add and replace existing files       }
  {  umAddAutoRename Add and rename if already exists     }

  TUpdateMode = (umAdd, umUpdate, umReplace,
    umAddUpdate, umAddReplace, umAddAutoRename);

  { Compression Method Option:                            }
  {   moStore                                             }
  {   moFast                                              }
  {   moNormal                                            }
  {   moMaximum                                           }

  TmOption = (moStore, moFast, moNormal, moMaximum);

  { Compression Dictionary Level Option:                  }
  {   do2MB                                               }
  {   do5MB                                               }
  {   ..                                                  }
  {   do1280MB                                            }

  TdOption = (do2MB, do5MB, do10MB, do20MB, do40MB,
    do80MB, do160MB, do320MB, do640MB ,do1280MB);

  { Process Priority Option:                              }
  {   prioIdle                                            }
  {   prioNormal                                          }
  {   prioHigh                                            }
  {   prioRealTime                                        }

  TpriOption = (prioIdle, prioNormal, prioHigh,
    prioRealTime);

  { Header Version Option:                                }
  {   hv02                                                }
  {   hv03                                                }
  {   hv04                                                }

  ThvOption = (hv02, hv03, hv04);

  { PFileInfo record }

  PFileInfo = ^TFileInfo;

  TFileInfo = record
    Name:       PChar;
    Path:       PChar;
    Size:       int64;
    Time:       longint;
    Attr:       longint;
    PackedSize: int64;
    Ratio:      longint;
    Comm:       PChar;
    Crc:        longword;
    Method:     PChar;
    Version:    PChar;
    Password:   PChar;
    Position:   longint;
  end;

procedure FreePChar(P: PChar);
procedure FreePFileInfo(P: Pointer);

implementation

uses
  SysUtils;

procedure FreePChar(P: PChar);
begin
  if P <> nil then
  begin
    StrDispose(P);
  end;
end;

procedure FreePFileInfo(P: Pointer);
begin
  if P <> nil then
    with TFileInfo(P^) do
    begin
      if Name     <> nil then StrDispose(Name);
      if Path     <> nil then StrDispose(Path);
      if Comm     <> nil then StrDispose(Comm);
      if Method   <> nil then StrDispose(Method);
      if Version  <> nil then StrDispose(Version);
      if Password <> nil then StrDispose(Password);
    end;
end;

end.
