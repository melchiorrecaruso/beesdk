
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('aboutfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'AboutFrm.Top', Top);
      Storage.WriteInteger('TForm', 'AboutFrm.Left', Left);
      Storage.WriteInteger('TForm', 'AboutFrm.Width', Width);
      Storage.WriteInteger('TForm', 'AboutFrm.Height', Height);
    end;
    Storage.WriteInteger('TForm', 'AboutFrm.WindowState', Ord(WindowState));
    // ---

    Storage.Free;
  end;
