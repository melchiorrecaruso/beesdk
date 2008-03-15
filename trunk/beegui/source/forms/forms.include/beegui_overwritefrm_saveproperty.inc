
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('overwritefrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'OverwriteFrm.Top', Top);
      Storage.WriteInteger('TForm', 'OverwriteFrm.Left', Left);
      Storage.WriteInteger('TForm', 'OverwriteFrm.Width', Width);
      Storage.WriteInteger('TForm', 'OverwriteFrm.Height', Height);
    end;
    Storage.WriteInteger('TForm', 'OverwriteFrm.WindowState', Ord(WindowState));
    // ---

    Storage.Free;
  end;
