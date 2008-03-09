
  // ---

  CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir('BeeGui'));
  if ForceDirectories(CfgFolder) then
  begin
    Storage := TMemIniFile.Create(CfgFolder + ('mainfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Top := Storage.ReadInteger('MainFrm', 'Top', Top);
      Left := Storage.ReadInteger('MainFrm', 'Left', Left);
      Width := Storage.ReadInteger('MainFrm', 'Width', Width);
      Height := Storage.ReadInteger('MainFrm', 'Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('MainFrm', 'WindowState', Ord(WindowState)));

    // ---

    MMenuViewAddressBar.Checked        := Storage.ReadBool('MMenu', 'View.AddressBar.Checked'         , MMenuViewAddressBar.Checked);
    MMenuViewLargeIcons.Checked        := Storage.ReadBool('MMenu', 'View.LargeIcons.Checked'         , MMenuViewLargeIcons.Checked);
    MMenuViewSmallIcons.Checked        := Storage.ReadBool('MMenu', 'View.SmallIcons.Checked'         , MMenuViewSmallIcons.Checked);
    MMenuViewList.Checked              := Storage.ReadBool('MMenu', 'View.List.Checked'               , MMenuViewList.Checked);
    MMenuViewReport.Checked            := Storage.ReadBool('MMenu', 'View.Report.Checked'             , MMenuViewReport.Checked);
    MMenuViewRowSelect.Checked         := Storage.ReadBool('MMenu', 'View.RowSelect.Checked'          , MMenuViewRowSelect.Checked);
    MMenuViewGridLines.Checked         := Storage.ReadBool('MMenu', 'View.GridLines.Checked'          , MMenuViewGridLines.Checked);
    MMenuViewListMode.Checked          := Storage.ReadBool('MMenu', 'View.ListMode.Checked'           , MMenuViewListMode.Checked);
    MMenuViewOrderByName.Checked       := Storage.ReadBool('MMenu', 'View.OrderBy.Name.Checked'       , MMenuViewOrderByName.Checked);
    MMenuViewOrderBySize.Checked       := Storage.ReadBool('MMenu', 'View.OrderBy.Size.Checked'       , MMenuViewOrderBySize.Checked);
    MMenuViewOrderByPacked.Checked     := Storage.ReadBool('MMenu', 'View.OrderBy.Packed.Checked'     , MMenuViewOrderByPacked.Checked);
    MMenuViewOrderByRatio.Checked      := Storage.ReadBool('MMenu', 'View.OrderBy.Ratio.Checked'      , MMenuViewOrderByRatio.Checked);
    MMenuViewOrderByType.Checked       := Storage.ReadBool('MMenu', 'View.OrderBy.Type.Checked'       , MMenuViewOrderByType.Checked);
    MMenuViewOrderByModified.Checked   := Storage.ReadBool('MMenu', 'View.OrderBy.Modified.Checked'   , MMenuViewOrderByModified.Checked);
    MMenuViewOrderByAttributes.Checked := Storage.ReadBool('MMenu', 'View.OrderBy.Attributes.Checked' , MMenuViewOrderByAttributes.Checked);
    MMenuViewOrderByMethod.Checked     := Storage.ReadBool('MMenu', 'View.OrderBy.Method.Checked'     , MMenuViewOrderByMethod.Checked);
    MMenuViewOrderByPassword.Checked   := Storage.ReadBool('MMenu', 'View.OrderBy.Password.Checked'   , MMenuViewOrderByPassword.Checked);
    MMenuViewOrderByCrc.Checked        := Storage.ReadBool('MMenu', 'View.OrderBy.Crc.Checked'        , MMenuViewOrderByCrc.Checked);
    MMenuViewOrderByPath.Checked       := Storage.ReadBool('MMenu', 'View.OrderBy.Path.Checked'       , MMenuViewOrderByPath.Checked);
    MMenuViewOrderByPosition.Checked   := Storage.ReadBool('MMenu', 'View.OrderBy.Position.Checked'   , MMenuViewOrderByPosition.Checked);
    MMenuViewDetailsName.Checked       := Storage.ReadBool('MMenu', 'View.Details.Name.Checked'       , MMenuViewDetailsName.Checked);
    MMenuViewDetailsSize.Checked       := Storage.ReadBool('MMenu', 'View.Details.Size.Checked'       , MMenuViewDetailsSize.Checked);
    MMenuViewDetailsPacked.Checked     := Storage.ReadBool('MMenu', 'View.Details.Packed.Checked'     , MMenuViewDetailsPacked.Checked);
    MMenuViewDetailsRatio.Checked      := Storage.ReadBool('MMenu', 'View.Details.Ratio.Checked'      , MMenuViewDetailsRatio.Checked);
    MMenuViewDetailsType.Checked       := Storage.ReadBool('MMenu', 'View.Details.Type.Checked'       , MMenuViewDetailsType.Checked);
    MMenuViewDetailsModified.Checked   := Storage.ReadBool('MMenu', 'View.Details.Modified.Checked'   , MMenuViewDetailsModified.Checked);
    MMenuViewDetailsAttributes.Checked := Storage.ReadBool('MMenu', 'View.Details.Attributes.Checked' , MMenuViewDetailsAttributes.Checked);
    MMenuViewDetailsMethod.Checked     := Storage.ReadBool('MMenu', 'View.Details.Method.Checked'     , MMenuViewDetailsMethod.Checked);
    MMenuViewDetailsPassword.Checked   := Storage.ReadBool('MMenu', 'View.Details.Password.Checked'   , MMenuViewDetailsPassword.Checked);
    MMenuViewDetailsCRC.Checked        := Storage.ReadBool('MMenu', 'View.Details.CRC.Checked'        , MMenuViewDetailsCRC.Checked);
    MMenuViewDetailsPath.Checked       := Storage.ReadBool('MMenu', 'View.Details.Path.Checked'       , MMenuViewDetailsPath.Checked);
    MMenuViewDetailsPosition.Checked   := Storage.ReadBool('MMenu', 'View.Details.Position.Checked'   , MMenuViewDetailsPosition.Checked);

    MMenuOptionsSaveOnExit.Checked     := Storage.ReadBool('MMenu', 'Options.SaveOnExit.Checked'      , MMenuOptionsSaveOnExit.Checked);
    MMenuOptionsLogReport.Checked      := Storage.ReadBool('MMenu', 'Options.LogReport.Checked'       , MMenuOptionsLogReport.Checked);

    // ---

    ListView.Column[ 0].Width := Storage.ReadInteger('ListView', 'Column[00].Width', ListView.Column[ 0].Width);
    ListView.Column[ 1].Width := Storage.ReadInteger('ListView', 'Column[01].Width', ListView.Column[ 1].Width);
    ListView.Column[ 2].Width := Storage.ReadInteger('ListView', 'Column[02].Width', ListView.Column[ 2].Width);
    ListView.Column[ 3].Width := Storage.ReadInteger('ListView', 'Column[03].Width', ListView.Column[ 3].Width);
    ListView.Column[ 4].Width := Storage.ReadInteger('ListView', 'Column[04].Width', ListView.Column[ 4].Width);
    ListView.Column[ 5].Width := Storage.ReadInteger('ListView', 'Column[05].Width', ListView.Column[ 5].Width);
    ListView.Column[ 6].Width := Storage.ReadInteger('ListView', 'Column[06].Width', ListView.Column[ 6].Width);
    ListView.Column[ 7].Width := Storage.ReadInteger('ListView', 'Column[07].Width', ListView.Column[ 7].Width);
    ListView.Column[ 8].Width := Storage.ReadInteger('ListView', 'Column[08].Width', ListView.Column[ 8].Width);
    ListView.Column[ 9].Width := Storage.ReadInteger('ListView', 'Column[09].Width', ListView.Column[ 9].Width);
    ListView.Column[10].Width := Storage.ReadInteger('ListView', 'Column[10].Width', ListView.Column[10].Width);
    ListView.Column[11].Width := Storage.ReadInteger('ListView', 'Column[11].Width', ListView.Column[11].Width);

    ListView.SortCol := TArcListViewColumn(Storage.ReadInteger('ListView', 'Column', Ord(ListView.SortCol)));

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




    Storage.Free;
  end;




