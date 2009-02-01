
  procedure TMainFrm.LoadProperty;
  var
    I: integer;
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('mainfrm.ini'));
      if WindowState = wsNormal then
      begin
        Top := Storage.ReadInteger('TForm', 'Top', Top);
        Left := Storage.ReadInteger('TForm', 'Left', Left);
        Width := Storage.ReadInteger('TForm', 'Width', Width);
        Height := Storage.ReadInteger('TForm', 'Height', Height);
      end;
      WindowState := TWindowState(Storage.ReadInteger('TForm', 'WindowState', Ord(WindowState)));
      // ListView
      for I := 0 to ListView.Columns.Count -1 do
      begin
        ListView.Column[I].Width := Storage.ReadInteger('ListView', 'Column['+ IntToStr(I) +'].Width', ListView.Column[I].Width);
      end;
      ListView.SortColumn := Storage.ReadInteger('ListView', 'SortColumn', Ord(ListView.SortColumn));
      // Main Menu View
      MMenuViewAddressBar.Checked        := Storage.ReadBool('MMenu', 'View.AddressBar.Checked'         , MMenuViewAddressBar.Checked);
      MMenuViewstatusBar.Checked         := Storage.ReadBool('MMenu', 'View.StatusBar.Checked'          , MMenuViewStatusBar.Checked);
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
      // Main Menu Option
      MMenuOptionsSaveOnExit.Checked     := Storage.ReadBool('MMenu', 'Options.SaveOnExit.Checked'     , MMenuOptionsSaveOnExit.Checked);
      MMenuOptionsLogReport.Checked      := Storage.ReadBool('MMenu', 'Options.LogReport.Checked'      , MMenuOptionsLogReport.Checked);
      // Buttons popup menu
      BMenuNew.Checked           := Storage.ReadBool('BMenu', 'New.Checked'          , BMenuNew.Checked);
      BMenuOpen.Checked          := Storage.ReadBool('BMenu', 'Open.Checked'         , BMenuOpen.Checked);
      BMenuN1.Checked            := Storage.ReadBool('BMenu', 'Spacer-1'             , BMenuN1.Checked);
      BMenuAdd.Checked           := Storage.ReadBool('BMenu', 'Add.Checked'          , BMenuAdd.Checked);
      BMenuExtract.Checked       := Storage.ReadBool('BMenu', 'Extract.Checked'      , BMenuExtract.Checked);
      BMenuView.Checked          := Storage.ReadBool('BMenu', 'View.Checked'         , BMenuView.Checked);
      BMenuDelete.Checked        := Storage.ReadBool('BMenu', 'Delete.Checked'       , BMenuDelete.Checked);
      BMenuN2.Checked            := Storage.ReadBool('BMenu', 'Spacer-2'             , BMenuN2.Checked);
      BMenuTest.Checked          := Storage.ReadBool('BMenu', 'Test.Checked'         , BMenuTest.Checked);
      BMenuCheckOut.Checked      := Storage.ReadBool('BMenu', 'CheckOut.Checked'     , BMenuCheckOut.Checked);
      BMenuN3.Checked            := Storage.ReadBool('BMenu', 'Spacer-3'             , BMenuN3.Checked);
      BMenuConfiguration.Checked := Storage.ReadBool('BMenu', 'Configuration.Checked', BMenuConfiguration.Checked);
      BMenuHelp.Checked          := Storage.ReadBool('BMenu', 'Help.Checked'         , BMenuHelp.Checked);
      BMenuExit.Checked          := Storage.ReadBool('BMenu', 'Exit.Checked'         , BMenuExit.Checked);

      Storage.Free;
    end;
  end;



