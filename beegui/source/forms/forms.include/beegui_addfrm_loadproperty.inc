
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('addfrm.ini'));
    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'AddFrm.Top', Top);
      Left   := Storage.ReadInteger('TForm', 'AddFrm.Left', Left);
      Width  := Storage.ReadInteger('TForm', 'AddFrm.Width', Width);
      Height := Storage.ReadInteger('TForm', 'AddFrm.Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('TForm', 'AddFrm.WindowState', Ord(WindowState)));
    Storage.Free;
  end;

