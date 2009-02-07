
  procedure TConfigFrm.LoadProperty;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('configfrm.ini'));
      if WindowState = wsNormal then
      begin
        Top    := Storage.ReadInteger('TForm', 'Top', Top);
        Left   := Storage.ReadInteger('TForm', 'Left', Left);
        Height := Storage.ReadInteger('TForm', 'Height', Height);
        Width  := Storage.ReadInteger('TForm', 'Width', Width);
      end;
      WindowState := TWindowState(Storage.ReadInteger('TForm', 'WindowState', Ord(WindowState)));

      mOption.ItemIndex := Storage.ReadInteger('TComboBox', 'mOption.ItemIndex' ,mOption.ItemIndex);
      dOption.ItemIndex := Storage.ReadInteger('TComboBox', 'dOption.ItemIndex' ,dOption.ItemIndex);
      oOption.ItemIndex := Storage.ReadInteger('TComboBox', 'oOption.ItemIndex' ,oOption.ItemIndex);

      rOption.Checked := Storage.ReadBool('TCheckBox', 'rOption.Checked' ,rOption.Checked);
      sOption.Checked := Storage.ReadBool('TCheckBox', 'sOption.Checked' ,sOption.Checked);
      kOption.Checked := Storage.ReadBool('TCheckBox', 'kOption.Checked' ,kOption.Checked);
      tOption.Checked := Storage.ReadBool('TCheckBox', 'tOption.Checked' ,tOption.Checked);
      cdAOption.Checked := Storage.ReadBool('TCheckBox', 'cdAOption.Checked' ,cdAOption.Checked);

      xCommand.Checked := Storage.ReadBool('TCheckBox', 'xCommand.Checked' ,xCommand.Checked);
      cdEOption.Checked := Storage.ReadBool('TCheckBox', 'cdEOption.Checked' ,cdEOption.Checked);
    
      HideWithAddFrmOrExtractFrmOption.Checked := Storage.ReadBool('TCheckBox',
        'HideWithAddFrmOrExtractFrmOption.Checked' ,HideWithAddFrmOrExtractFrmOption.Checked);

      HideWithTickFrmOption.Checked := Storage.ReadBool('TCheckBox',
        'HideWithTickFrmOption.Checked' ,HideWithTickFrmOption.Checked);

      UpBtnCloseOption.Checked := Storage.ReadBool('TCheckBox', 'UpBtnCloseOption.Checked' ,UpBtnCloseOption.Checked);

      Storage.Free;
    end;
  end;
