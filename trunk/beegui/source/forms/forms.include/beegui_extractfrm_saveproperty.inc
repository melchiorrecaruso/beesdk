
  // ---

  F := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(F) then
  begin
    Storage := TMemIniFile.Create(F + ('extractfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'ExtractFrm.Top', Top);
      Storage.WriteInteger('TForm', 'ExtractFrm.Left', Left);
      Storage.WriteInteger('TForm', 'ExtractFrm.Width', Width);
      Storage.WriteInteger('TForm', 'ExtractFrm.Height', Height);
    end;
    Storage.WriteInteger('TForm', 'ExtractFrm.WindowState', Ord(WindowState));

    // ---
    
    Storage.WriteString('TEdit', 'Folder.Text', Folder.Text);
    
    // ---

    Storage.Free;
  end;
