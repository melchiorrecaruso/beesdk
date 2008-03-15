
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('addfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'AddFrm.Top', Top);
      Storage.WriteInteger('TForm', 'AddFrm.Left', Left);
      Storage.WriteInteger('TForm', 'AddFrm.Width', Width);
      Storage.WriteInteger('TForm', 'AddFrm.Height', Height);
    end;
    Storage.WriteInteger('TForm', 'AddFrm.WindowState', Ord(WindowState));

    // ---

    Storage.Free;
  end;
