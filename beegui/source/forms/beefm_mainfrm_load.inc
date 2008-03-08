
  // ---

  CfgFolder := AnsiIncludeTrailingBackSlash(GetApplicationConfigDir('BeeGui'));
  if ForceDirectories(CfgFolder) then
  begin
    Storage := TIniFile.Create(CfgFolder + ('mainfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Top := Storage.ReadInteger('MainFrm', 'Top', Top);
      Left := Storage.ReadInteger('MainFrm', 'Left', Left);
      Width := Storage.ReadInteger('MainFrm', 'Width', Width);
      Height := Storage.ReadInteger('MainFrm', 'Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('MainFrm', 'WindowState', Ord(WindowState)));

    Storage.Free;
  end;




