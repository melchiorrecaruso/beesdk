
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));    
    begin
      Storage.WriteInteger('TForm', 'Top', Top);
      Storage.WriteInteger('TForm', 'Left', Left);      
    end;      
    Storage.Free;
  end;
