/*
  Copyright (c) 2013 Melchiorre Caruso.

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
*/

/* 
  Contains:

    Add form.

  Fist release:

    v1.0 build 0027 - 2013.11.23 by Melchiorre Caruso.

  Modifyed:

*/

using Gtk;

public class AddWindow : Dialog {
  
  public AddWindow () {
    this.title = "Add To Archive";
    this.set_default_size (600, 400);
    this.window_position = WindowPosition.CENTER;

    this.CreateWidgets ();          
  }

  private void CreateWidgets () {
  }
}
