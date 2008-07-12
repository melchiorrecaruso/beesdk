
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('aboutfrm.ini'));
    if WindowState = wsNormal then
    begin
      Top  := Storage.ReadInteger('TForm', 'Top', Top);
      Left := Storage.ReadInteger('TForm', 'Left', Left);
    end;
    Storage.Free;
  end;

