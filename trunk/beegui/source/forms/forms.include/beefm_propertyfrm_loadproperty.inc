
  procedure TInfoFrm.LoadProperty;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('propertyfrm.ini'));
      if WindowState = wsNormal then
      begin
        Top    := Storage.ReadInteger('TForm', 'Top', Top);
        Left   := Storage.ReadInteger('TForm', 'Left', Left);
        Height := Storage.ReadInteger('TForm', 'Height', Height);
        Width  := Storage.ReadInteger('TForm', 'Width', Width);
      end;
      WindowState := TWindowState(Storage.ReadInteger('TForm', 'WindowState', Ord(WindowState)));
      Storage.Free;
    end;
  end;