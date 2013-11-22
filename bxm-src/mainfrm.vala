
using Gtk;
using Granite.Widgets;

int main (string[] args) {
    Gtk.init (ref args);
 
    // the window:
    var window = new Window (WindowType.TOPLEVEL);
    window.title = "BX Manager";
    window.set_default_size (600, 400);
    window.window_position = WindowPosition.CENTER;
    window.destroy.connect (Gtk.main_quit);
 
    // the toolbar:
    var toolbar = new Toolbar (); 
    toolbar.get_style_context ().add_class (STYLE_CLASS_PRIMARY_TOOLBAR);   	    
    
    var newbtn = new ToolButton.from_stock (Stock.NEW); 
		newbtn.clicked.connect (() => {	
          string archivename = archivesavedialog ();		
		});
	toolbar.add (newbtn);	

    var openbtn = new ToolButton.from_stock (Stock.OPEN);
		openbtn.clicked.connect (() => {
          string archivename = archiveopendialog ();			
		});
	toolbar.add (openbtn);

    var spacer = new ToolItem ();
    spacer.set_expand (true);
    toolbar.add (spacer);

    var sharemenu = new Gtk.Menu ();    	
	var sharemenu_item_1 = new Gtk.MenuItem.with_label ("Item-1");
	sharemenu.add (sharemenu_item_1);
    var sharemenu_separator_1 = new Gtk.SeparatorMenuItem ();
    sharemenu.add (sharemenu_separator_1);	
    var sharemenu_item_2 = new Gtk.MenuItem.with_label ("Item-2");
	sharemenu.add (sharemenu_item_2);		
    var sharebtn  = new Granite.Widgets.ToolButtonWithMenu (new Image.from_icon_name ("document-export", IconSize.MENU), "Share", sharemenu);
    toolbar.add (sharebtn);

    var mainmenu = new Gtk.Menu ();    	
	var mainmenu_item_1 = new Gtk.MenuItem.with_label ("Item-1");
	mainmenu.add (mainmenu_item_1);
    var mainmenu_separator_1 = new Gtk.SeparatorMenuItem ();
    mainmenu.add (mainmenu_separator_1);	
    var mainmenu_item_2 = new Gtk.MenuItem.with_label ("Item-2");
	mainmenu.add (mainmenu_item_2);		
    var menubtn  = new Granite.Widgets.ToolButtonWithMenu (new Image.from_icon_name ("document-properties", IconSize.MENU), "Menu", mainmenu);
    toolbar.add (menubtn);
    

    
    // the treeview:
    var treeview  = new TreeView ();
    var listmodel = new ListStore (4, typeof (string), typeof (string), typeof (string), typeof (string));
    treeview.set_model (listmodel);

    treeview.insert_column_with_attributes (-1, "Filename", new CellRendererText (), "text", 0);
    treeview.insert_column_with_attributes (-1, "Size",     new CellRendererText (), "text", 1);
    treeview.insert_column_with_attributes (-1, "Type",     new CellRendererText (), "text", 2);
    treeview.insert_column_with_attributes (-1, "Modified", new CellRendererText (), "text", 3);

    TreeIter iter;
    listmodel.append (out iter);
    listmodel.set (iter, 0, "My Visacard",   1, "card", 2, "102,10", 3, "red");

    listmodel.append (out iter);
    listmodel.set (iter, 0, "My Mastercard", 1, "card", 2, "10,20", 3, "red");
    
    // the box
    var vbox = new Box (Orientation.VERTICAL, 0);
    vbox.pack_start (toolbar, false, true, 0);
    vbox.pack_start (treeview, true, true, 0);
       
      
    window.add (vbox);              
    window.show_all ();
    Gtk.main ();
    return 0;
}
