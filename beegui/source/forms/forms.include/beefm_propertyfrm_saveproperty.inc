
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('propertyfrm.ini'));
    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'Top', Top);
      Storage.WriteInteger('TForm', 'Left', Left);
      Storage.WriteInteger('TForm', 'Height', Height);
      Storage.WriteInteger('TForm', 'Width', Width);
    end;
    Storage.WriteInteger('TForm', 'WindowState', Ord(WindowState));
    Storage.Free;
  end;
