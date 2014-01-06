{
    Copyright (c) 2003-2009 Andrew Filinsky and Melchiorre Caruso

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

      BeeOpt project compiler directives

    Modifyed:

      v1.0.5 build 0392 - 2008.06.14 by Melchiorre Caruso.
}

{$R-}
{$Q-}
{$S-}
{$I-}

{$INLINE OFF}

{$DEFINE cppDLL}

{$IFDEF MSWINDOWS}
  {*$DEFINE FILENAMECASESENSITIVE}
{$ELSE}
  {$IFDEF UNIX}
    {$DEFINE FILENAMECASESENSITIVE}
  {$ELSE}
    todo: define filenamecasesensitive
  {$ENDIF}
{$ENDIF}