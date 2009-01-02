
  procedure TMainFrm.LoadLanguage;
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
      BtnNew.Caption      := Storage.ReadString('TSpeedButton', 'BtnNew.Caption',      BtnNew.Caption);
      BtnOpen.Caption     := Storage.ReadString('TSpeedButton', 'BtnOpen.Caption',     BtnOpen.Caption);
      BtnAdd.Caption      := Storage.ReadString('TSpeedButton', 'BtnAdd.Caption',      BtnAdd.Caption);
      BtnExtract.Caption  := Storage.ReadString('TSpeedButton', 'BtnExtract.Caption',  BtnExtract.Caption);
      BtnView.Caption     := Storage.ReadString('TSpeedButton', 'BtnView.Caption',     BtnView.Caption);
      BtnDelete.Caption   := Storage.ReadString('TSpeedButton', 'BtnDelete.Caption',   BtnDelete.Caption);
      BtnTest.Caption     := Storage.ReadString('TSpeedButton', 'BtnTest.Caption',     BtnTest.Caption);
      BtnCheckOut.Caption := Storage.ReadString('TSpeedButton', 'BtnCheckOut.Caption', BtnCheckOut.Caption);
      BtnConfig.Caption   := Storage.ReadString('TSpeedButton', 'BtnConfig.Caption',   BtnConfig.Caption);
      BtnHelp.Caption     := Storage.ReadString('TSpeedButton', 'BtnHelp.Caption',     BtnHelp.Caption);
      BtnExit.Caption     := Storage.ReadString('TSpeedButton', 'BtnExit.Caption',     BtnExit.Caption);
      BtnUp.Caption       := Storage.ReadString('TSpeedButton', 'BtnUp.Caption',       BtnUp.Caption);

      // Main menu caption
      for I := 0 to MMenu.Items.Count -1 do with MMenu.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenu.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuFile.Count -1 do with MMenuFile.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenuFile.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuView.Count -1 do with MMenuView.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenuView.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuViewToolBars.Count -1 do with MMenuViewToolBars.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenuViewToolBars.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuViewOrderBy.Count -1 do with MMenuViewOrderBy.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenuViewOrderBy.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuViewDetails.Count -1 do with MMenuViewDetails.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenuViewDetails.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuActions.Count -1 do with MMenuActions.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenuActions.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuOptions.Count -1 do with MMenuOptions.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenuOptions.Items[' + IntToStr(I) + '].Caption', Caption);

      for I := 0 to MMenuHelp.Count -1 do with MMenuHelp.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'MMenuHelp.Items[' + IntToStr(I) + '].Caption', Caption);

      // Popup menu caption
      for I := 0 to PMenu.Items.Count -1 do with PMenu.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'PMenu.Items[' + IntToStr(I) + '].Caption', Caption);

      // Buttons menu caption
      for I := 0 to BMenu.Items.Count -1 do with BMenu.Items[I] do
        Caption := Storage.ReadString('TMenuItem', 'BMenu.Items[' + IntToStr(I) + '].Caption', Caption);

      // Archive listview
      for I := 0 to ListView.Columns.Count -1 do with ListView.Columns[I] do
        Caption := Storage.ReadString('TListView', 'ListView.Columns[' + IntToStr(I) + 'Caption', Caption);

      Storage.Free;
    end;
  end;
