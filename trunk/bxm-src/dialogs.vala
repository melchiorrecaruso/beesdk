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

    Dialog forms.

  Fist release:

    v1.0 build 0027 - 2013.11.23 by Melchiorre Caruso.

  Modifyed:

*/

using Gtk;
using Gdk;

string archiveopendialog () {
  var dialog = new Gtk.FileChooserDialog("Open Archive", null, Gtk.FileChooserAction.OPEN, 
  "Cancel", ResponseType.CANCEL, "Open", ResponseType.ACCEPT);

  dialog.select_multiple = false;

  var allfiles = new Gtk.FileFilter ();
  allfiles.set_filter_name ("All files");	 
  allfiles.add_pattern ("*");

  var bxmfiles = new Gtk.FileFilter ();
  bxmfiles.set_filter_name ("BX files");	
  bxmfiles.add_pattern ("*.bx");

  dialog.add_filter (allfiles);    
  dialog.add_filter (bxmfiles);
  dialog.set_filter (bxmfiles);
    
  string result = "";
  if (dialog.run () == ResponseType.ACCEPT) {
    result = dialog.get_filename ();      
  }
  dialog.close ();
  return result;
}

string archivesavedialog () {
  var dialog = new Gtk.FileChooserDialog("New Archive", null, Gtk.FileChooserAction.SAVE, 
  "Cancel", ResponseType.CANCEL, "Save", ResponseType.ACCEPT);

  dialog.select_multiple = false;

  var allfiles = new Gtk.FileFilter ();
  allfiles.set_filter_name ("All files");	
  allfiles.add_pattern ("*");

  var bxmfiles = new Gtk.FileFilter ();
  bxmfiles.set_filter_name ("BX files");	
  bxmfiles.add_pattern ("*.bx");

  dialog.add_filter (allfiles);    
  dialog.add_filter (bxmfiles);
  dialog.set_filter (bxmfiles);
    
  string result = "";
  if (dialog.run () == ResponseType.ACCEPT) {
    result = dialog.get_filename ();      
  }
  dialog.close ();
  return result;
}

void aboutdialog () {
  var dialog = new Gtk.AboutDialog ();	
  dialog.set_modal (true);

  const string[] authors = {"Melchiorre Caruso <melchiorrecaruso@gmail.com>" , null};
  dialog.authors = authors;
  const string[] artists = {"Melchiorre Caruso <melchiorrecaruso@gmail.com>", null};
  dialog.artists = artists;	
  //const string[] documenters = {"", null};
  //dialog.documenters = documenters;     
  //dialog.translator_credits = null; 
    
  //var logo = new Gdk.Pixbuf.from_file (Gda.get_application_exec_path ("bxm") + "logo.png");
  //dialog.logo = logo;
  dialog.logo_icon_name = "file-roller";  
  dialog.program_name = "BX Manager";
  dialog.comments = "The user interface for BX archiver";
  dialog.copyright = "Copyright © 2013 Melchiorre Caruso";
  dialog.version = "1.0";

  //dialog.license_type = Gtk.License.GPL_2_0;   
  dialog.license = 

"BX is free software; you can redistribute it and/or modify it under the 
terms of the GNU Lesser General Public License as published by the Free 
Software Foundation; either version 2.1 of the License, or (at your option) 
any later version.

BX is distributed in the hope that it will be useful, but WITHOUT 
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for 
more details.

You should have received a copy of the GNU Lesser General Public License 
along with BX; if not, write to the Free Software Foundation, Inc., 
51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA"; 

  dialog.wrap_license  = true;
  dialog.website       = "http://www.beegui.org";
  dialog.website_label = "Visit the BX web site";

  dialog.run ();
  dialog.close ();
  return;
}
