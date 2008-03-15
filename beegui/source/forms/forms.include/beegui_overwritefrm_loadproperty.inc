
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('overwritefrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'OverwriteFrm.Top', Top);
      Left   := Storage.ReadInteger('TForm', 'OverwriteFrm.Left', Left);
      Width  := Storage.ReadInteger('TForm', 'OverwriteFrm.Width', Width);
      Height := Storage.ReadInteger('TForm', 'OverwriteFrm.Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('TForm', 'OverwriteFrm.WindowState', Ord(WindowState)));
    // ---

    Storage.Free;
  end;

