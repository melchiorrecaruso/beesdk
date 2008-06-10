
  Folder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir('BeeGui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('mainfrm.ini'));
    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'Top', Top);
      Storage.WriteInteger('TForm', 'Left', Left);
      Storage.WriteInteger('TForm', 'Width', Width);
      Storage.WriteInteger('TForm', 'Height', Height);
    end;
    Storage.WriteInteger('TForm', 'WindowState', Ord(WindowState));


    // --- OK

    (*

    Storage.WriteBool('MMenu', 'View.AddressBar.Checked'         , MMenuViewAddressBar.Checked);
    Storage.WriteBool('MMenu', 'View.LargeIcons.Checked'         , MMenuViewLargeIcons.Checked);
    Storage.WriteBool('MMenu', 'View.SmallIcons.Checked'         , MMenuViewSmallIcons.Checked);
    Storage.WriteBool('MMenu', 'View.List.Checked'               , MMenuViewList.Checked);
    Storage.WriteBool('MMenu', 'View.Report.Checked'             , MMenuViewReport.Checked);
    Storage.WriteBool('MMenu', 'View.RowSelect.Checked'          , MMenuViewRowSelect.Checked);
    Storage.WriteBool('MMenu', 'View.GridLines.Checked'          , MMenuViewGridLines.Checked);
    Storage.WriteBool('MMenu', 'View.ListMode.Checked'           , MMenuViewListMode.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Name.Checked'       , MMenuViewOrderByName.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Size.Checked'       , MMenuViewOrderBySize.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Packed.Checked'     , MMenuViewOrderByPacked.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Ratio.Checked'      , MMenuViewOrderByRatio.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Type.Checked'       , MMenuViewOrderByType.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Modified.Checked'   , MMenuViewOrderByModified.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Attributes.Checked' , MMenuViewOrderByAttributes.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Method.Checked'     , MMenuViewOrderByMethod.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Password.Checked'   , MMenuViewOrderByPassword.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Crc.Checked'        , MMenuViewOrderByCrc.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Path.Checked'       , MMenuViewOrderByPath.Checked);
    Storage.WriteBool('MMenu', 'View.OrderBy.Position.Checked'   , MMenuViewOrderByPosition.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Name.Checked'       , MMenuViewDetailsName.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Size.Checked'       , MMenuViewDetailsSize.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Packed.Checked'     , MMenuViewDetailsPacked.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Ratio.Checked'      , MMenuViewDetailsRatio.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Type.Checked'       , MMenuViewDetailsType.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Modified.Checked'   , MMenuViewDetailsModified.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Attributes.Checked' , MMenuViewDetailsAttributes.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Method.Checked'     , MMenuViewDetailsMethod.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Password.Checked'   , MMenuViewDetailsPassword.Checked);
    Storage.WriteBool('MMenu', 'View.Details.CRC.Checked'        , MMenuViewDetailsCRC.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Path.Checked'       , MMenuViewDetailsPath.Checked);
    Storage.WriteBool('MMenu', 'View.Details.Position.Checked'   , MMenuViewDetailsPosition.Checked);

    Storage.WriteBool('MMenu', 'Options.SaveOnExit.Checked'      , MMenuOptionsSaveOnExit.Checked);
    Storage.WriteBool('MMenu', 'Options.LogReport.Checked'       , MMenuOptionsLogReport.Checked);

    // ---

    Storage.WriteInteger('ListView', 'Column[00].Width', ListView.Column[ 0].Width);
    Storage.WriteInteger('ListView', 'Column[01].Width', ListView.Column[ 1].Width);
    Storage.WriteInteger('ListView', 'Column[02].Width', ListView.Column[ 2].Width);
    Storage.WriteInteger('ListView', 'Column[03].Width', ListView.Column[ 3].Width);
    Storage.WriteInteger('ListView', 'Column[04].Width', ListView.Column[ 4].Width);
    Storage.WriteInteger('ListView', 'Column[05].Width', ListView.Column[ 5].Width);
    Storage.WriteInteger('ListView', 'Column[06].Width', ListView.Column[ 6].Width);
    Storage.WriteInteger('ListView', 'Column[07].Width', ListView.Column[ 7].Width);
    Storage.WriteInteger('ListView', 'Column[08].Width', ListView.Column[ 8].Width);
    Storage.WriteInteger('ListView', 'Column[09].Width', ListView.Column[ 9].Width);
    Storage.WriteInteger('ListView', 'Column[10].Width', ListView.Column[10].Width);
    Storage.WriteInteger('ListView', 'Column[11].Width', ListView.Column[11].Width);

    Storage.WriteInteger('ListView', 'Column', Ord(ListView.SortCol));

    // ---

    Storage.WriteBool('BtnsMenu', 'New.Checked'          , BMenuNew.Checked);
    Storage.WriteBool('BtnsMenu', 'Open.Checked'         , BMenuOpen.Checked);
    Storage.WriteBool('BtnsMenu', 'Add.Checked'          , BMenuAdd.Checked);
    Storage.WriteBool('BtnsMenu', 'Extract.Checked'      , BMenuExtract.Checked);
    Storage.WriteBool('BtnsMenu', 'View.Checked'         , BMenuView.Checked);
    Storage.WriteBool('BtnsMenu', 'View.Checked'         , BMenuView.Checked);
    Storage.WriteBool('BtnsMenu', 'Test.Checked'         , BMenuTest.Checked);
    Storage.WriteBool('BtnsMenu', 'CheckOut.Checked'     , BMenuCheckOut.Checked);
    Storage.WriteBool('BtnsMenu', 'Configuration.Checked', BMenuConfiguration.Checked);
    Storage.WriteBool('BtnsMenu', 'Help.Checked'         , BMenuHelp.Checked);
    Storage.WriteBool('BtnsMenu', 'Exit.Checked'         , BMenuExit.Checked);

    // ---
    *)
    Storage.Free;
  end;
