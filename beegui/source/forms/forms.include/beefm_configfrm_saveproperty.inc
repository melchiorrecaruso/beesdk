
  procedure TConfigFrm.SaveProperty;
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
        Storage.WriteInteger('TForm', 'Top', Top);
        Storage.WriteInteger('TForm', 'Left', Left);
        Storage.WriteInteger('TForm', 'Height', Height);
        Storage.WriteInteger('TForm', 'Width', Width);
      end;
      Storage.WriteInteger('TForm', 'WindowState', Ord(WindowState));

      Storage.WriteInteger('TComboBox', 'mOption.ItemIndex' ,mOption.ItemIndex);
      Storage.WriteInteger('TComboBox', 'dOption.ItemIndex' ,dOption.ItemIndex);
      Storage.WriteInteger('TComboBox', 'oOption.ItemIndex' ,oOption.ItemIndex);
    
      Storage.WriteBool('TCheckBox', 'rOption.Checked' ,rOption.Checked);
      Storage.WriteBool('TCheckBox', 'sOption.Checked' ,sOption.Checked);
      Storage.WriteBool('TCheckBox', 'kOption.Checked' ,kOption.Checked);
      Storage.WriteBool('TCheckBox', 'tOption.Checked' ,tOption.Checked);
      Storage.WriteBool('TCheckBox', 'cdAOption.Checked' ,cdAOption.Checked);

      Storage.WriteBool('TCheckBox', 'xCommand.Checked' ,xCommand.Checked);
      Storage.WriteBool('TCheckBox', 'cdEOption.Checked' ,cdEOption.Checked);

      Storage.WriteBool('TCheckBox', 'HideAddFrmOption.Checked' ,HideAddFrmOption.Checked);
      Storage.WriteBool('TCheckBox', 'HideMainFrmOption.Checked' ,HideMainFrmOption.Checked);
      Storage.WriteBool('TCheckBox', 'UpBtnCloseOption.Checked' ,UpBtnCloseOption.Checked);

      Storage.Free;
    end;
  end;
