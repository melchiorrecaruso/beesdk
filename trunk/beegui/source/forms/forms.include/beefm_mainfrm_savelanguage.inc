
  procedure TMainFrm.SaveLanguage;
  var
    I: integer;
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('mainfrm.ini'));

      // Buttons caption
      Storage.WriteString('TSpeedButton', 'BtnNew.Caption', BtnNew.Caption);
      Storage.WriteString('TSpeedButton', 'BtnOpen.Caption', BtnOpen.Caption);
      Storage.WriteString('TSpeedButton', 'BtnAdd.Caption', BtnAdd.Caption);
      Storage.WriteString('TSpeedButton', 'BtnExtract.Caption', BtnExtract.Caption);
      Storage.WriteString('TSpeedButton', 'BtnView.Caption', BtnView.Caption);
      Storage.WriteString('TSpeedButton', 'BtnDelete.Caption', BtnDelete.Caption);
      Storage.WriteString('TSpeedButton', 'BtnTest.Caption', BtnTest.Caption);
      Storage.WriteString('TSpeedButton', 'BtnCheckOut.Caption', BtnCheckOut.Caption);
      Storage.WriteString('TSpeedButton', 'BtnConfig.Caption', BtnConfig.Caption);
      Storage.WriteString('TSpeedButton', 'BtnHelp.Caption', BtnHelp.Caption);
      Storage.WriteString('TSpeedButton', 'BtnExit.Caption', BtnExit.Caption);
      Storage.WriteString('TSpeedButton', 'BtnUp.Caption', BtnUp.Caption);

      // Main menu caption
      for I := 0 to MMenu.Items.Count -1 do with MMenu.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenu.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuFile.Count -1 do with MMenuFile.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenuFile.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuView.Count -1 do with MMenuView.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenuView.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuViewToolBars.Count -1 do with MMenuViewToolBars.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenuViewToolBars.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuViewOrderBy.Count -1 do with MMenuViewOrderBy.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenuViewOrderBy.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuViewDetails.Count -1 do with MMenuViewDetails.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenuViewDetails.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuActions.Count -1 do with MMenuActions.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenuActions.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuOptions.Count -1 do with MMenuOptions.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenuOptions.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuHelp.Count -1 do with MMenuHelp.Items[I] do
        Storage.WriteString('TMenuItem', 'MMenuHelp.Items[' + IntToStr(I) + '].Caption', Caption);

      // Popup menu caption
      for I := 0 to PMenu.Items.Count -1 do with PMenu.Items[I] do
        Storage.WriteString('TMenuItem', 'PMenu.Items[' + IntToStr(I) + '].Caption', Caption);

      // Buttons menu caption
      for I := 0 to BMenu.Items.Count -1 do with BMenu.Items[I] do
        Storage.WriteString('TMenuItem', 'BMenu.Items[' + IntToStr(I) + '].Caption', Caption);

      // Archive listview
      for I := 0 to ListView.Columns.Count -1 do with ListView.Columns[I] do
        Storage.WriteString('TListView', 'ListView.Columns[' + IntToStr(I) + 'Caption', Caption);

      Storage.Free;
    end;
  end;
