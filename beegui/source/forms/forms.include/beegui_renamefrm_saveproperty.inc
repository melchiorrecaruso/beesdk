
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));
    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'RenameFrm.Top', Top);
      Storage.WriteInteger('TForm', 'RenameFrm.Left', Left);
      Storage.WriteInteger('TForm', 'RenameFrm.Width', Width);
      Storage.WriteInteger('TForm', 'RenameFrm.Height', Height);
    end;
    Storage.WriteInteger('TForm', 'RenameFrm.WindowState', Ord(WindowState));
    Storage.Free;
  end;
