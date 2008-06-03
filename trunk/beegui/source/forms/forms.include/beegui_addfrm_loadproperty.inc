
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('BeeGui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('addfrm.ini'));
    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'Top', Top);
      Left   := Storage.ReadInteger('TForm', 'Left', Left);
      Width  := Storage.ReadInteger('TForm', 'Width', Width);
      Height := Storage.ReadInteger('TForm', 'Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('TForm', 'WindowState', Ord(WindowState)));
    Storage.Free;
  end;

