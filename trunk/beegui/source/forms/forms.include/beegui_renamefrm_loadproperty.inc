
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));
    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'RenameFrm.Top', Top);
      Left   := Storage.ReadInteger('TForm', 'RenameFrm.Left', Left);
      Width  := Storage.ReadInteger('TForm', 'RenameFrm.Width', Width);
      Height := Storage.ReadInteger('TForm', 'RenameFrm.Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('TForm', 'RenameFrm.WindowState', Ord(WindowState)));
    Storage.Free;
  end;

