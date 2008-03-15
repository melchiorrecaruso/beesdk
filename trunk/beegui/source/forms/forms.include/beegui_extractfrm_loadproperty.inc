
  // ---

  F := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(F) then
  begin
    Storage := TMemIniFile.Create(F + ('extractfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'ExtractFrm.Top', Top);
      Left   := Storage.ReadInteger('TForm', 'ExtractFrm.Left', Left);
      Width  := Storage.ReadInteger('TForm', 'ExtractFrm.Width', Width);
      Height := Storage.ReadInteger('TForm', 'ExtractFrm.Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('TForm', 'ExtractFrm.WindowState', Ord(WindowState)));

    // ---

    Storage.Free;
  end;

