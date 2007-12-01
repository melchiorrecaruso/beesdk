unit ContextMenu_Main;

interface

uses
  Windows,
  Classes,
  SysUtils,
  ShellApi,
  Graphics,
  // ---
  ContextMenu_Utils,
  ContextMenu_Custom;

type
  TContextMenu = class(TOwnerDrawContextMenu)
  public
    // Basic methods
    function GetHelpText(IdCmdOffset: UINT): String; override;
    function GetVerb(IdCmdOffset: UINT): String; override;
    function AddMenuItems(Menu: HMENU; MenuIndex, IdCmdFirst, IdCmdLast: UINT): UINT; override;
    procedure ExecuteCommand(IdCmdOffset: UINT); override;
    // Owner-draw methods
    procedure MeasureMenuItem(IdCmd: UINT; var Width: UINT; var Height: UINT); override;
    procedure DrawMenuItem(Menu: HMENU; IdCmd: UINT; DC: HDC; Rect: TRect; State: TOwnerDrawState); override;
  end;

implementation

{$R ContextMenu.res} // Include resource file containg bitmap

const
  GUID_ContextMenuEntry: TGUID = '{105DEF32-1ABA-4672-B6E0-91591F5BEDAD}';
  VERB = 'BeeGuiContextMenuPlugin';
  // Menu items
  MENUITEM_PARENT      = 0;
  MENUITEM_OPEN        = 1;
  MENUITEM_EXTRACT     = 2;
  MENUITEM_EXTRACTHERE = 3;
  MENUITEM_TEST        = 4;
  MENUITEM_ADDTO       = 5;

{------------------- TContextMenu -------------------}

function TContextMenu.GetHelpText(IdCmdOffset: UINT): String;
begin
  case IdCmdOffset of
    MENUITEM_PARENT:      Result := 'BeeGui Context Menu.';
    MENUITEM_OPEN:        Result := 'Open archive.';
    MENUITEM_EXTRACT:     Result := 'Extract.';
    MENUITEM_EXTRACTHERE: Result := 'Extract here.';
    MENUITEM_TEST:        Result := 'Test';
    MENUITEM_ADDTO:       Result := 'Add to';
  end;
end;

function TContextMenu.GetVerb(IdCmdOffset: UINT): String;
begin
  Result := VERB;
end;

procedure TContextMenu.MeasureMenuItem(IdCmd: UINT; var Width: UINT; var Height: UINT);
begin
  Height := 16;
  { We need to calculate the width of our menu item. Since we just want to use the
    standard menu font we use the GetStandardTextWidth method. }
  // Get text width and add 32+4 px for the bitmap we're going to draw
  Width := GetStandardTextWidth('BeeGui') + 16 + 2;
end;

procedure TContextMenu.DrawMenuItem(Menu: HMENU; IdCmd: UINT; DC: HDC; Rect: TRect; State: TOwnerDrawState);
var
  Bmp: HBITMAP;
begin
  // Fill background and set colors depending on selection status
  if odSelected in State then
  begin
    SetBkColor(DC, GetSysColor(COLOR_HIGHLIGHT));
    SetTextColor(DC, GetSysColor(COLOR_HIGHLIGHTTEXT));
    FillRect(DC, Rect, HBRUSH(COLOR_HIGHLIGHT+1));
  end
  else
  begin
    SetBkColor(DC, GetSysColor(COLOR_MENU));
    SetTextColor(DC, GetSysColor(COLOR_MENUTEXT));
    FillRect(DC, Rect, HBRUSH(COLOR_MENU+1));
  end;
  Bmp := LoadBitmap(HInstance, 'ICON');   // Load 32x32 bitmap
  try
    { Drawing a bitmap transparently is complicated stuff. I tried using imagelists,
      but failed to make it work. The method below comes to the rescue.
      The bitmap we use has a white background. }
    // Draw bitmap (starting with a margin of 16)
    if Bmp <> 0 then
    begin
      DrawTransparentBitmap(DC, Bmp, Rect.Left, Rect.Top, clFuchsia);
    end;
    // Draw text, using a 16 px left margin, and 32+4 px reserved for the image
    TextOut(DC, Rect.Left + 17, Rect.Top, PChar('BeeGui'), Length('BeeGui'));
  finally
    // Clean up
    DeleteObject(Bmp);
  end;
end;

function TContextMenu.AddMenuItems(Menu: HMENU; MenuIndex, IdCmdFirst, IdCmdLast: UINT): UINT;
var
  IsDirectory: boolean;
  mii: TMenuItemInfo;
  SubMenu: HMENU;
  I: UINT;
begin
  IsDirectory := False;
  for I := 0 to FileNames.Count -1 do
  begin
    if DirectoryExists(FileNames.Strings [I]) then
    begin
      IsDirectory := True;
      Break;
    end;
  end;

  // Create Context Menu
  FillChar(mii, SizeOf(mii), 0);
  mii.cbSize := SizeOf(mii);
  mii.fType := MFT_STRING;
  mii.fState := MFS_ENABLED;

  // Submenu items
  SubMenu := CreatePopupMenu;

  if IsDirectory = False then
  begin
    mii.fMask := MIIM_ID or MIIM_TYPE;
    mii.dwTypeData := PChar('&Open');
    mii.wID := IdCmdFirst + MENUITEM_OPEN;
    InsertMenuItem(SubMenu, $FFFFFFFF, True, mii);

    mii.fMask := MIIM_ID or MIIM_TYPE;
    mii.dwTypeData := PChar('&Extract');
    mii.wID := IdCmdFirst + MENUITEM_EXTRACT;
    InsertMenuItem(SubMenu, $FFFFFFFF, True, mii);

    mii.fMask := MIIM_ID or MIIM_TYPE;
    mii.dwTypeData := PChar('E&xtract here');
    mii.wID := IdCmdFirst + MENUITEM_EXTRACTHERE;
    InsertMenuItem(SubMenu, $FFFFFFFF, True, mii);

    mii.fMask := MIIM_ID or MIIM_TYPE;
    mii.dwTypeData := PChar('&Test');
    mii.wID := IdCmdFirst + MENUITEM_TEST;
    InsertMenuItem(SubMenu, $FFFFFFFF, True, mii);
  end;

  mii.fMask := MIIM_ID or MIIM_TYPE;
  mii.dwTypeData := PChar('&Add to');
  mii.wID := IdCmdFirst + MENUITEM_ADDTO;
  InsertMenuItem(SubMenu, $FFFFFFFF, True, mii);

  // Create parent item containing the submenu

  mii.fType := MFT_OWNERDRAW;
  mii.fMask := MIIM_ID or MIIM_TYPE or MIIM_SUBMENU;
  mii.dwTypeData := PChar('&BeeGui');
  mii.wID := IdCmdFirst + MENUITEM_PARENT;
  mii.hSubMenu := SubMenu;
  InsertMenuItem(Menu, MenuIndex, True, mii);

  // Return max id of inserted menu items
  Result := IdCmdFirst + MENUITEM_ADDTO
end;


procedure TContextMenu.ExecuteCommand(IdCmdOffset: UINT);
var
  I: integer;
  Params: string;
begin
  case IdCmdOffset of
    MENUITEM_OPEN:
    begin
      for I := 0 to FileNames.Count - 1 do
      begin
        Params := 'l -r ''' + FileNames.Strings[I] + ''' *';
        ShellExecute(0, 'open', PChar('BeeGui'), PChar(Params), PChar(''), SW_SHOW);
      end;
    end;
    MENUITEM_EXTRACT:
    begin

    end;
    MENUITEM_EXTRACTHERE:
    begin

    end;
    MENUITEM_TEST:
    begin

    end;
    MENUITEM_ADDTO:
    begin
    
    end;
  end;
end;

initialization
  Initialize(TContextMenu, GUID_ContextMenuEntry, 'BeeGuiContextMenuPlugin');

end.

