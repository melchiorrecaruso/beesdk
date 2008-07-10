
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('addfrm.ini'));
    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'Top', Top);
      Storage.WriteInteger('TForm', 'Left', Left);
      Storage.WriteInteger('TForm', 'Width', Width);
      Storage.WriteInteger('TForm', 'Height', Height);
    end;
    Storage.WriteInteger('TForm', 'WindowState', Ord(WindowState));
    Storage.Free;
  end;
