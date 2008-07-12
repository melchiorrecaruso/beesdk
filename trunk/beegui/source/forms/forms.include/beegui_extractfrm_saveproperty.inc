
  F := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
  if ForceDirectories(F) then
  begin
    Storage := TMemIniFile.Create(F + ('extractfrm.ini'));
    begin
      if WindowState = wsNormal then
      begin
        Storage.WriteInteger('TForm', 'Top', Top);
        Storage.WriteInteger('TForm', 'Left', Left);
        Storage.WriteInteger('TForm', 'Width', Width);
        Storage.WriteInteger('TForm', 'Height', Height);
      end;  
      Storage.WriteInteger('TForm', 'WindowState', Ord(WindowState));
      Storage.WriteString('TEdit', 'Folder.Text', Folder.Text);    
    end;
    Storage.Free;
  end;
