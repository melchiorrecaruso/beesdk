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

    BX Manager main form.

  Fist release:

    v1.0 build 0027 - 2013.11.23 by Melchiorre Caruso.

  Modifyed:

*/

using Gtk;
using Granite.Widgets;

public class MainWindow : Window {
  
  public MainWindow () {
    this.title = "BX Manager";
    this.set_default_size (600, 400);
    this.window_position = WindowPosition.CENTER;          
  }

  private void CreateWidgets () {
    // the toolbar:
    var toolbar = new Toolbar (); 
    toolbar.get_style_context ().add_class (STYLE_CLASS_PRIMARY_TOOLBAR);  

    var newbtn = new ToolButton.from_stock (Stock.NEW); 
	newbtn.clicked.connect (() => {	
      stdout.printf(archivesavedialog () + "\n");		
	});
	toolbar.add (newbtn);

    var openbtn = new ToolButton.from_stock (Stock.OPEN);
	openbtn.clicked.connect (() => {
      stdout.printf(archiveopendialog () + "\n");			
    });
	toolbar.add (openbtn);

    var addbtn = new ToolButton.from_stock (Stock.GO_DOWN);
	addbtn.clicked.connect (() => {
      stdout.printf("add form show\n"); 	
    });
    toolbar.add (addbtn);

    var extractbtn = new ToolButton.from_stock (Stock.GO_UP);
	extractbtn.clicked.connect (() => {
      stdout.printf("extract form show\n"); 			
    });
    toolbar.add (extractbtn);

    var findbtn = new ToolButton.from_stock (Stock.FIND);
	findbtn.clicked.connect (() => {
      stdout.printf("find form show\n"); 			
    });
    toolbar.add (findbtn);


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
    var mainmenu_about = new Gtk.MenuItem.with_label ("About");
    mainmenu_about.activate.connect (() => {	
      aboutdialog (); 	
    });	    
    mainmenu.add (mainmenu_about);		
    var menubtn  = new Granite.Widgets.ToolButtonWithMenu (new Image.from_icon_name ("document-properties", IconSize.MENU), "Menu", mainmenu);
    toolbar.add (menubtn);
    
    // the treeview:
    var treeview  = new TreeView ();
    var listmodel = new ListStore (4, typeof (string), typeof (string), typeof (string), typeof (string));
    treeview.set_model (listmodel);

    treeview.insert_column_with_attributes (-1, "Filename", new CellRendererText (), "text", 0);
    treeview.get_column(0).clicked.connect (() => {	
    // ordina per nome		
	});    
    treeview.insert_column_with_attributes (-1, "Size",     new CellRendererText (), "text", 1);
    treeview.get_column(1).clicked.connect (() => {	
    // ordina per size		
	});
    treeview.insert_column_with_attributes (-1, "Type",     new CellRendererText (), "text", 2);
    treeview.get_column(2).clicked.connect (() => {	
    // ordina per type		
	});
    treeview.insert_column_with_attributes (-1, "Modified", new CellRendererText (), "text", 3);
    treeview.get_column(3).clicked.connect (() => {	
    // ordina per type		
	});
    treeview.headers_clickable = true;            

    var treeviewmenu = new Gtk.Menu ();  
    var treeviewmenu_item_1 = new Gtk.MenuItem.with_label ("Item-1");
    treeviewmenu_item_1.activate.connect (() => {	
      stdout.printf("treeview menu item 1 anctivated\n"); 	
    });
  	treeviewmenu.add (treeviewmenu_item_1);    
    
    var treeviewmenu_item_2 = new Gtk.MenuItem.with_label ("Item-2");
    treeviewmenu_item_2.activate.connect (() => {	
      stdout.printf("treeview menu item 2 anctivated\n"); 	
    });
	treeviewmenu.add (treeviewmenu_item_2);
    this.add (treeviewmenu);

    treeview.button_press_event.connect ((event) => {  
      if (event.button == 3) {
		treeviewmenu.show_all ();
        treeviewmenu.popup (null, null, null, event.button, event.time);			
	    return true;
	  }
      stdout.printf("button_press_event\n");
      return false;
    });
    
 

    treeview.row_activated.connect(() => {	
      stdout.printf("row_activated\n");
    });






    TreeIter iter;
    listmodel.append (out iter);
    listmodel.set (iter, 0, "My Visacard",   1, "card", 2, "102,10", 3, "red");

    listmodel.append (out iter);
    listmodel.set (iter, 0, "My Mastercard", 1, "card", 2, "10,20", 3, "red");
    
    // the box
    var vbox = new Box (Orientation.VERTICAL, 0);
    vbox.pack_start (toolbar, false, true, 0);
    vbox.pack_start (treeview, true, true, 0);
       
    this.add (vbox);
    this.show_all ();         
  }

  public static int main (string[] args) {
    Gtk.init (ref args);
    var window = new MainWindow ();
    window.destroy.connect (Gtk.main_quit);
    window.CreateWidgets ();
    window.show ();
    Gtk.main ();
    return 0;
  }
}