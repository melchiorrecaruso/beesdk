
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('aboutfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'AboutFrm.Top', Top);
      Left   := Storage.ReadInteger('TForm', 'AboutFrm.Left', Left);
      Width  := Storage.ReadInteger('TForm', 'AboutFrm.Width', Width);
      Height := Storage.ReadInteger('TForm', 'AboutFrm.Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('TForm', 'AboutFrm.WindowState', Ord(WindowState)));
    // ---

    Storage.Free;
  end;

