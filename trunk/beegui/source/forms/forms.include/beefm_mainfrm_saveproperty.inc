
  procedure TMainFrm.SaveProperty;
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
        Storage.WriteInteger('TForm', 'Top', Top);
        Storage.WriteInteger('TForm', 'Left', Left);
        Storage.WriteInteger('TForm', 'Width', Width);
        Storage.WriteInteger('TForm', 'Height', Height);
      end;
      Storage.WriteInteger('TForm', 'WindowState', Ord(WindowState));
      // ListView
      if ListView.ViewStyle = vsReport then
      begin
        for I := 0 to ListView.Columns.Count -1 do
        begin
          Storage.WriteInteger('ListView', 'Column['+ IntToStr(I) +'].Width', ListView.Column[I].Width);
        end;
        Storage.WriteInteger('ListView', 'SortColumn', ListView.SortColumn);
      end;
      // Main Menu View
      Storage.WriteBool('MMenu', 'View.AddressBar.Checked'         , MMenuViewAddressBar.Checked);
      Storage.WriteBool('MMenu', 'View.StatusBar.Checked'          , MMenuViewStatusBar.Checked);
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
      // Main Menu Option
      Storage.WriteBool('MMenu', 'Options.SaveOnExit.Checked'      , MMenuOptionsSaveOnExit.Checked);
      Storage.WriteBool('MMenu', 'Options.LogReport.Checked'       , MMenuOptionsLogReport.Checked);
      // Buttons popup menu
      Storage.WriteBool('BMenu', 'New.Checked'          , BMenuNew.Checked);
      Storage.WriteBool('BMenu', 'Open.Checked'         , BMenuOpen.Checked);
      Storage.WriteBool('BMenu', 'Spacer-1'             , BMenuN1.Checked);
      Storage.WriteBool('BMenu', 'Add.Checked'          , BMenuAdd.Checked);
      Storage.WriteBool('BMenu', 'Extract.Checked'      , BMenuExtract.Checked);
      Storage.WriteBool('BMenu', 'View.Checked'         , BMenuView.Checked);
      Storage.WriteBool('BMenu', 'Delete.Checked'       , BMenuDelete.Checked);
      Storage.WriteBool('BMenu', 'Spacer-2'             , BMenuN2.Checked);
      Storage.WriteBool('BMenu', 'Test.Checked'         , BMenuTest.Checked);
      Storage.WriteBool('BMenu', 'CheckOut.Checked'     , BMenuCheckOut.Checked);
      Storage.WriteBool('BMenu', 'Spacer-3'             , BMenuN3.Checked);
      Storage.WriteBool('BMenu', 'Configuration.Checked', BMenuConfiguration.Checked);
      Storage.WriteBool('BMenu', 'Help.Checked'         , BMenuHelp.Checked);
      Storage.WriteBool('BMenu', 'Exit.Checked'         , BMenuExit.Checked);

      Storage.Free;
    end;
  end;
