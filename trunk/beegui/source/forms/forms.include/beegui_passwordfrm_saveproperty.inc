
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'PasswordFrm.Top', Top);
      Storage.WriteInteger('TForm', 'PasswordFrm.Left', Left);
      Storage.WriteInteger('TForm', 'PasswordFrm.Width', Width);
      Storage.WriteInteger('TForm', 'PasswordFrm.Height', Height);
    end;
    Storage.WriteInteger('TForm', 'PasswordFrm.WindowState', Ord(WindowState));
    // ---

    Storage.Free;
  end;
