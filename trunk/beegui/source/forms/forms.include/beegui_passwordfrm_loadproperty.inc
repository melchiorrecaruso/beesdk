
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'PasswordFrm.Top', Top);
      Left   := Storage.ReadInteger('TForm', 'PasswordFrm.Left', Left);
      Width  := Storage.ReadInteger('TForm', 'PasswordFrm.Width', Width);
      Height := Storage.ReadInteger('TForm', 'PasswordFrm.Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('TForm', 'PasswordFrm.WindowState', Ord(WindowState)));
    // ---

    Storage.Free;
  end;

