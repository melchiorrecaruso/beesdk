
  F := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(F) then
  begin
    Storage := TMemIniFile.Create(F + ('extractfrm.ini'));
    begin
	  if WindowState = wsNormal then
      begin
        Top    := Storage.ReadInteger('TForm', 'Top', Top);
        Left   := Storage.ReadInteger('TForm', 'Left', Left);
        Width  := Storage.ReadInteger('TForm', 'Width', Width);
        Height := Storage.ReadInteger('TForm', 'Height', Height);
      end;
      WindowState := TWindowState(Storage.ReadInteger('TForm', 'WindowState', Ord(WindowState)));   
      Folder.Text := Storage.ReadString('TEdit', 'Folder.Text', Folder.Text);
    end;   
    Storage.Free;
  end;

