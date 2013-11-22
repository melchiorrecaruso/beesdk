using Gtk;

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
