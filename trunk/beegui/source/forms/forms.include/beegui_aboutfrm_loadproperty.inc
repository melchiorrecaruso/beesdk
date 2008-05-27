
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('aboutfrm.ini'));
    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'AboutFrm.Top', Top);
      Left   := Storage.ReadInteger('TForm', 'AboutFrm.Left', Left);
    end;
    Storage.Free;
  end;
  
  // ---

