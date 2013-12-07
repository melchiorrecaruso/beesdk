{
    Copyright (c) 2003-2008 Andrew Filinsky and Melchiorre Caruso

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public LicenseMemo as published by
    the Free Software Foundation; either version 2 of the LicenseMemo, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public LicenseMemo for more details.

    You should have received a copy of the GNU General Public LicenseMemo
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{   Contains:

      BeeGui project compiler directives

    Modifyed:

      v1.0.5 build 0392 - 2008.06.14 by Melchiorre Caruso.
}

{$R-}
{$Q-}
{$S-}
{$I-}

{$INLINE OFF}
{*$DEFINE PLUGINS}
{*$DEFINE SAVELANGUAGE}

{$IFDEF MSWINDOWS}
  {*$DEFINE USEDLL}
  {*$DEFINE FILENAMECASESENSITIVE}
{$ELSE}
  {$IFDEF UNIX}
    {*$DEFINE USEDLL}
    {$DEFINE FILENAMECASESENSITIVE}
  {$ELSE}
    todo: define filenamecasesensitive
  {$ENDIF}
{$ENDIF}
